# 08_model_evaluate.R -- Model diagnostics and evaluation
# =============================================================================
# Outputs:
#   output/{men,women}/calibration.png
#   output/{men,women}/feature_importance.png
#   output/{men,women}/auc_roc.png
#   output/{men,women}/accuracy_by_seed.png
#   output/{men,women}/model_comparison.csv
# =============================================================================

source(here::here("R", "utils.R"))
source(here::here("R", "00_config.R"))
source(here::here("R", "07_model_train.R"))

library(dplyr)
library(tidyr)
library(readr)
library(ggplot2)
library(purrr)

# ---------------------------------------------------------------------------
# .auc_roc() -- compute AUC-ROC from predictions and labels
# ---------------------------------------------------------------------------
.auc_roc <- function(y_true, y_pred) {
  # Trapezoidal AUC
  n1 <- sum(y_true == 1)
  n0 <- sum(y_true == 0)
  idx <- order(y_pred, decreasing = TRUE)
  y_sorted <- y_true[idx]
  tpr <- cumsum(y_sorted == 1) / n1
  fpr <- cumsum(y_sorted == 0) / n0
  # Trapezoidal rule
  sum(diff(c(0, fpr)) * (c(0, tpr) + c(tpr, 0)) / 2)
}

# ---------------------------------------------------------------------------
# .log_loss() -- compute log-loss
# ---------------------------------------------------------------------------
.log_loss <- function(y_true, y_pred) {
  y_pred <- pmax(pmin(y_pred, 1 - 1e-7), 1e-7)
  -mean(y_true * log(y_pred) + (1 - y_true) * log(1 - y_pred))
}

# ---------------------------------------------------------------------------
# evaluate_model() -- main exported function
# ---------------------------------------------------------------------------
evaluate_model <- function(gender = c("men", "women")) {
  gender <- match.arg(gender)
  paths  <- gender_paths(gender)

  message("Evaluating ", gender, "'s model...")

  matchups <- safe_read_csv(file.path(paths$processed, "training_matchups.csv"))
  if (is.null(matchups)) stop("Training matchups not found.")
  matchups <- matchups |> filter(!is.na(outcome))

  # Load models and generate predictions
  models <- list(
    logistic = readRDS(file.path(OUTPUT_MODELS_DIR, paste0(gender, "_logistic.rds"))),
    xgboost  = readRDS(file.path(OUTPUT_MODELS_DIR, paste0(gender, "_xgboost.rds"))),
    weights  = readRDS(file.path(OUTPUT_MODELS_DIR, paste0(gender, "_ensemble_weights.rds")))
  )

  preds <- predict_win_prob(matchups, gender, models)

  # Baseline: always pick better seed (lower seed number wins)
  preds <- preds |>
    mutate(
      p_chalk    = ifelse(seed_diff > 0, 0.95, ifelse(seed_diff < 0, 0.05, 0.5)),
      win_chalk  = as.integer(seed_diff > 0),
      win_lr     = as.integer(p_logistic > 0.5),
      win_xgb    = as.integer(p_xgboost > 0.5),
      win_ens    = as.integer(p_ensemble > 0.5)
    )

  # ---------------------------------------------------------------------------
  # Summary table
  # ---------------------------------------------------------------------------
  model_summary <- tibble(
    model    = c("Chalk (seed only)", "Logistic (LR)", "XGBoost", "Ensemble"),
    accuracy = c(
      mean(preds$win_chalk == preds$outcome),
      mean(preds$win_lr    == preds$outcome),
      mean(preds$win_xgb   == preds$outcome),
      mean(preds$win_ens   == preds$outcome)
    ),
    log_loss = c(
      .log_loss(preds$outcome, preds$p_chalk),
      .log_loss(preds$outcome, preds$p_logistic),
      .log_loss(preds$outcome, preds$p_xgboost),
      .log_loss(preds$outcome, preds$p_ensemble)
    ),
    auc = c(
      .auc_roc(preds$outcome, preds$p_chalk),
      .auc_roc(preds$outcome, preds$p_logistic),
      .auc_roc(preds$outcome, preds$p_xgboost),
      .auc_roc(preds$outcome, preds$p_ensemble)
    )
  )

  cat("\n--- Model Comparison (", gender, ") ---\n", sep = "")
  print(model_summary)

  comp_path <- file.path(paths$output, "model_comparison.csv")
  write_csv(model_summary, comp_path)

  # ---------------------------------------------------------------------------
  # Plot 1: Calibration curve
  # ---------------------------------------------------------------------------
  cal_data <- preds |>
    mutate(bin = cut(p_ensemble, breaks = seq(0, 1, 0.1), include.lowest = TRUE)) |>
    group_by(bin) |>
    summarise(
      mean_pred   = mean(p_ensemble),
      actual_rate = mean(outcome),
      n           = n(),
      .groups = "drop"
    )

  p_cal <- ggplot(cal_data, aes(mean_pred, actual_rate)) +
    geom_abline(slope = 1, intercept = 0, color = "gray50", linetype = "dashed") +
    geom_point(aes(size = n), color = "#003087", alpha = 0.8) +
    geom_line(color = "#003087") +
    scale_size_continuous(range = c(2, 8), name = "N games") +
    scale_x_continuous(labels = scales::percent_format()) +
    scale_y_continuous(labels = scales::percent_format()) +
    labs(
      title    = paste("Calibration Plot:", stringr::str_to_title(gender), "Tournament"),
      subtitle = "Diagonal line = perfect calibration",
      x = "Predicted Win Probability", y = "Actual Win Rate"
    ) +
    THEME_MM

  cal_path <- file.path(paths$output, "calibration.png")
  ggsave(cal_path, p_cal, width = 7, height = 6, dpi = 150)
  message("Saved: ", cal_path)

  # ---------------------------------------------------------------------------
  # Plot 2: XGBoost feature importance
  # ---------------------------------------------------------------------------
  xgb_imp <- xgboost::xgb.importance(
    feature_names = FEATURE_COLS,
    model         = models$xgboost
  )

  p_imp <- ggplot(xgb_imp[1:min(15, nrow(xgb_imp)), ],
                  aes(x = reorder(Feature, Gain), y = Gain)) +
    geom_col(fill = "#C8102E", alpha = 0.85) +
    coord_flip() +
    labs(
      title    = paste("XGBoost Feature Importance:", stringr::str_to_title(gender)),
      subtitle = "Gain = relative contribution to predictions",
      x = NULL, y = "Gain"
    ) +
    THEME_MM

  imp_path <- file.path(paths$output, "feature_importance.png")
  ggsave(imp_path, p_imp, width = 8, height = 6, dpi = 150)
  message("Saved: ", imp_path)

  # ---------------------------------------------------------------------------
  # Plot 3: Accuracy by seed differential bucket
  # ---------------------------------------------------------------------------
  acc_seed <- preds |>
    mutate(
      seed_bucket = case_when(
        seed_diff >= 8  ~ "8+ seeds apart",
        seed_diff >= 5  ~ "5-7 seeds apart",
        seed_diff >= 3  ~ "3-4 seeds apart",
        seed_diff >= 1  ~ "1-2 seeds apart",
        seed_diff == 0  ~ "Same seed",
        seed_diff <= -1 ~ "Underdog",
        TRUE            ~ "Other"
      )
    ) |>
    group_by(seed_bucket) |>
    summarise(accuracy = mean(win_ens == outcome), n = n(), .groups = "drop")

  p_acc <- ggplot(acc_seed, aes(x = reorder(seed_bucket, accuracy), y = accuracy)) +
    geom_col(fill = "#003087", alpha = 0.85) +
    geom_text(aes(label = paste0(round(accuracy * 100, 1), "%\n(n=", n, ")")),
              hjust = -0.1, size = 3.5) +
    coord_flip(ylim = c(0, 1.1)) +
    scale_y_continuous(labels = scales::percent_format()) +
    labs(
      title    = paste("Ensemble Accuracy by Seed Differential:", stringr::str_to_title(gender)),
      x = NULL, y = "Accuracy"
    ) +
    THEME_MM

  acc_path <- file.path(paths$output, "accuracy_by_seed.png")
  ggsave(acc_path, p_acc, width = 8, height = 5, dpi = 150)
  message("Saved: ", acc_path)

  invisible(model_summary)
}
