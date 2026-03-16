# 07_model_train.R -- Train logistic regression + XGBoost ensemble
# =============================================================================
# Methodology based on:
#   - arXiv:2503.21790: Logistic regression on efficiency metrics placed in top
#     99.5% of ESPN Tournament Challenge
#   - arXiv:2603.10916: Combinatorial Fusion Analysis ensemble ~73% accuracy
#   - arXiv:2508.02725: XGBoost AUC 0.8473 on tournament outcomes
#
# Models:
#   1. Regularized logistic regression (elastic net via glmnet)
#   2. XGBoost (gradient boosted trees)
#   3. Ensemble: weighted average (weights calibrated by held-out AUC)
#
# Output:
#   output/models/{men,women}_logistic.rds
#   output/models/{men,women}_xgboost.rds
#   output/models/{men,women}_ensemble_weights.rds
#   output/models/{men,women}_feature_scaler.rds
# =============================================================================

source(here::here("R", "utils.R"))
source(here::here("R", "00_config.R"))

library(dplyr)
library(readr)
library(purrr)
library(xgboost)
library(glmnet)

# ---------------------------------------------------------------------------
# .prep_matrix() -- convert feature df to numeric matrix for modeling
# ---------------------------------------------------------------------------
.prep_matrix <- function(df, feature_cols = FEATURE_COLS) {
  available <- intersect(feature_cols, names(df))
  if (length(available) < length(feature_cols)) {
    missing_cols <- setdiff(feature_cols, names(df))
    warning("Missing feature columns, filling with 0: ", paste(missing_cols, collapse=", "))
    for (col in missing_cols) df[[col]] <- 0
  }
  mat <- df |>
    select(all_of(feature_cols)) |>
    mutate(across(everything(), ~replace_na(as.numeric(.), 0))) |>
    as.matrix()
  mat
}

# ---------------------------------------------------------------------------
# train_logistic() -- elastic net logistic regression
# ---------------------------------------------------------------------------
train_logistic <- function(X_train, y_train,
                           alpha = MODEL_PARAMS$logistic$alpha,
                           nfolds = MODEL_PARAMS$logistic$nfolds) {
  message("  Training logistic regression (elastic net alpha=", alpha, ")...")
  set.seed(RANDOM_SEED)

  cv_fit <- cv.glmnet(
    x        = X_train,
    y        = y_train,
    family   = "binomial",
    alpha    = alpha,
    nfolds   = nfolds,
    type.measure = "deviance",
    standardize  = TRUE
  )
  message(sprintf("  Best lambda: %.6f (CV deviance: %.4f)",
                  cv_fit$lambda.min, min(cv_fit$cvm)))
  cv_fit
}

# ---------------------------------------------------------------------------
# train_xgboost() -- gradient boosted trees
# ---------------------------------------------------------------------------
train_xgboost <- function(X_train, y_train,
                           X_val = NULL, y_val = NULL,
                           params = MODEL_PARAMS$xgboost) {
  message("  Training XGBoost (eta=", params$eta,
          ", max_depth=", params$max_depth, ")...")
  set.seed(RANDOM_SEED)

  dtrain <- xgb.DMatrix(data = X_train, label = y_train)

  watchlist <- list(train = dtrain)
  if (!is.null(X_val) && !is.null(y_val)) {
    dval <- xgb.DMatrix(data = X_val, label = y_val)
    watchlist <- list(train = dtrain, val = dval)
  }

  xgb_params <- list(
    objective        = params$objective,
    eval_metric      = params$eval_metric,
    eta              = params$eta,
    max_depth        = params$max_depth,
    subsample        = params$subsample,
    colsample_bytree = params$colsample_bytree,
    min_child_weight = params$min_child_weight
  )

  model <- xgb.train(
    params    = xgb_params,
    data      = dtrain,
    nrounds   = params$nrounds,
    watchlist = watchlist,
    early_stopping_rounds = params$early_stopping,
    verbose   = 1,
    print_every_n = 50
  )

  message(sprintf("  Best iteration: %d (best score: %.4f)",
                  model$best_iteration, model$best_score))
  model
}

# ---------------------------------------------------------------------------
# calibrate_ensemble_weights() -- find optimal weights on validation set
# ---------------------------------------------------------------------------
calibrate_ensemble_weights <- function(p_logistic, p_xgboost, y_true) {
  # Grid search over weight combinations
  weights <- seq(0.1, 0.9, by = 0.05)
  results <- map_dbl(weights, function(w) {
    p_ens <- w * p_logistic + (1 - w) * p_xgboost
    # Log-loss
    -mean(y_true * log(pmax(p_ens, 1e-7)) + (1 - y_true) * log(pmax(1 - p_ens, 1e-7)))
  })

  best_w_logistic <- weights[which.min(results)]
  best_w_xgboost  <- 1 - best_w_logistic
  message(sprintf("  Optimal weights: logistic=%.2f, xgboost=%.2f (log-loss=%.4f)",
                  best_w_logistic, best_w_xgboost, min(results)))
  list(w_logistic = best_w_logistic, w_xgboost = best_w_xgboost)
}

# ---------------------------------------------------------------------------
# train_ensemble() -- main exported function
# Uses leave-one-season-out CV for weight calibration,
# then trains final models on all seasons.
# ---------------------------------------------------------------------------
train_ensemble <- function(gender = c("men", "women"),
                           force = FALSE) {
  gender <- match.arg(gender)
  paths  <- gender_paths(gender)

  lr_path      <- file.path(OUTPUT_MODELS_DIR, paste0(gender, "_logistic.rds"))
  xgb_path     <- file.path(OUTPUT_MODELS_DIR, paste0(gender, "_xgboost.rds"))
  weights_path <- file.path(OUTPUT_MODELS_DIR, paste0(gender, "_ensemble_weights.rds"))
  scaler_path  <- file.path(OUTPUT_MODELS_DIR, paste0(gender, "_feature_scaler.rds"))

  if (all(file.exists(c(lr_path, xgb_path, weights_path))) && !force) {
    message(gender, "'s models already trained. Use force=TRUE to retrain.")
    return(invisible(list(
      logistic = readRDS(lr_path),
      xgboost  = readRDS(xgb_path),
      weights  = readRDS(weights_path)
    )))
  }

  message("Training ", gender, "'s ensemble model...")

  # Load training data
  matchups <- safe_read_csv(file.path(paths$processed, "training_matchups.csv"))
  if (is.null(matchups)) stop("Training matchups not found. Run 06_feature_engineering.R first.")

  # Remove rows with missing outcome
  matchups <- matchups |> filter(!is.na(outcome))
  message(sprintf("  Training data: %d matchup rows, %d seasons",
                  nrow(matchups), n_distinct(matchups$season)))

  # ---------------------------------------------------------------------------
  # Leave-one-season-out CV (last 3 seasons as validation pool for weight calibration)
  # ---------------------------------------------------------------------------
  val_seasons  <- tail(sort(unique(matchups$season)), 3)
  train_data   <- matchups |> filter(!season %in% val_seasons)
  val_data     <- matchups |> filter(season %in% val_seasons)

  message(sprintf("  Train: %d rows (%d seasons), Val: %d rows (%d seasons)",
                  nrow(train_data), n_distinct(train_data$season),
                  nrow(val_data),   n_distinct(val_data$season)))

  X_train <- .prep_matrix(train_data)
  y_train <- train_data$outcome
  X_val   <- .prep_matrix(val_data)
  y_val   <- val_data$outcome

  # Save feature scaling info (mean/sd from training set)
  scaler <- list(
    col_means = colMeans(X_train, na.rm = TRUE),
    col_sds   = apply(X_train, 2, sd, na.rm = TRUE)
  )
  saveRDS(scaler, scaler_path)

  # Train models on training split
  message("\n-- Logistic Regression (validation split) --")
  lr_val   <- train_logistic(X_train, y_train)
  p_lr_val <- as.vector(predict(lr_val, X_val, s = "lambda.min", type = "response"))

  message("\n-- XGBoost (validation split) --")
  xgb_val   <- train_xgboost(X_train, y_train, X_val, y_val)
  p_xgb_val <- predict(xgb_val, X_val)

  # Calibrate ensemble weights on validation set
  message("\n-- Calibrating ensemble weights --")
  weights <- calibrate_ensemble_weights(p_lr_val, p_xgb_val, y_val)

  # ---------------------------------------------------------------------------
  # Final models: retrain on ALL data (train + val)
  # ---------------------------------------------------------------------------
  message("\n-- Final models (all data) --")
  X_all <- .prep_matrix(matchups)
  y_all <- matchups$outcome

  message("\n-- Final Logistic Regression --")
  lr_final <- train_logistic(X_all, y_all)

  message("\n-- Final XGBoost --")
  xgb_final <- train_xgboost(X_all, y_all)

  # Save models
  saveRDS(lr_final,  lr_path)
  saveRDS(xgb_final, xgb_path)
  saveRDS(weights,   weights_path)

  message("\nModels saved:")
  message("  ", lr_path)
  message("  ", xgb_path)
  message("  ", weights_path)

  invisible(list(
    logistic = lr_final,
    xgboost  = xgb_final,
    weights  = weights
  ))
}

# ---------------------------------------------------------------------------
# predict_win_prob() -- score a matchup feature row with trained ensemble
# ---------------------------------------------------------------------------
predict_win_prob <- function(features_df, gender = c("men", "women"),
                             models = NULL) {
  gender <- match.arg(gender)

  if (is.null(models)) {
    models <- list(
      logistic = readRDS(file.path(OUTPUT_MODELS_DIR, paste0(gender, "_logistic.rds"))),
      xgboost  = readRDS(file.path(OUTPUT_MODELS_DIR, paste0(gender, "_xgboost.rds"))),
      weights  = readRDS(file.path(OUTPUT_MODELS_DIR, paste0(gender, "_ensemble_weights.rds")))
    )
  }

  X <- .prep_matrix(features_df)

  p_lr  <- as.vector(predict(models$logistic, X, s = "lambda.min", type = "response"))
  p_xgb <- predict(models$xgboost, X)

  w <- models$weights
  p_ensemble <- w$w_logistic * p_lr + w$w_xgboost * p_xgb

  features_df |>
    mutate(
      p_logistic = p_lr,
      p_xgboost  = p_xgb,
      p_ensemble = p_ensemble
    )
}
