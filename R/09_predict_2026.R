# 09_predict_2026.R -- Apply trained ensemble to 2026 tournament matchups
# =============================================================================
# Generates:
#   1. Pairwise win probability matrix (all 68 teams vs all 68 teams)
#   2. First-round specific predictions with matchup context
#
# Output:
#   data/processed/{men,women}/win_prob_matrix_2026.csv
#   data/processed/{men,women}/first_round_predictions_2026.csv
# =============================================================================

source(here::here("R", "utils.R"))
source(here::here("R", "00_config.R"))
source(here::here("R", "07_model_train.R"))

library(dplyr)
library(tidyr)
library(readr)
library(purrr)

# ---------------------------------------------------------------------------
# predict_2026() -- main exported function
# ---------------------------------------------------------------------------
predict_2026 <- function(gender = c("men", "women")) {
  gender <- match.arg(gender)
  paths  <- gender_paths(gender)

  matrix_path <- file.path(paths$processed, "win_prob_matrix_2026.csv")
  r1_path     <- file.path(paths$processed, "first_round_predictions_2026.csv")

  message("Generating 2026 ", gender, "'s win probability matrix...")

  # Load pairwise features and models
  pairs_df <- safe_read_csv(file.path(paths$processed, "predict_matchups_2026.csv"))
  if (is.null(pairs_df)) {
    stop("2026 prediction features not found. Run 06_feature_engineering.R first.")
  }

  models <- list(
    logistic = readRDS(file.path(OUTPUT_MODELS_DIR, paste0(gender, "_logistic.rds"))),
    xgboost  = readRDS(file.path(OUTPUT_MODELS_DIR, paste0(gender, "_xgboost.rds"))),
    weights  = readRDS(file.path(OUTPUT_MODELS_DIR, paste0(gender, "_ensemble_weights.rds")))
  )

  # Score all pairs
  preds <- predict_win_prob(pairs_df, gender, models)

  # Save win probability matrix (long format)
  win_probs_long <- preds |>
    mutate(
      team_a_name = team_a, team_b_name = team_b,
      team_a_seed = seed_a, team_b_seed = seed_b,
      team_a_id   = team_a, team_b_id   = team_b
    ) |>
    select(team_a_id, team_a_name, team_a_seed,
           team_b_id, team_b_name, team_b_seed,
           p_logistic, p_xgboost, p_ensemble) |>
    rename(
      win_prob_lr  = p_logistic,
      win_prob_xgb = p_xgboost,
      win_prob_ens = p_ensemble
    )

  write_csv(win_probs_long, matrix_path)
  message(sprintf("Saved %d pairwise win probabilities to %s", nrow(win_probs_long), matrix_path))

  # ---------------------------------------------------------------------------
  # First-round predictions with bracket context
  # ---------------------------------------------------------------------------
  bracket <- safe_read_csv(file.path(paths$processed, "bracket_2026.csv"))

  if (!is.null(bracket)) {
    first_round <- bracket |>
      filter(round %in% c(0L, 1L)) |>
      left_join(
        win_probs_long |>
          select(team_a_name, team_b_name, win_prob_ens),
        by = c("team1_name" = "team_a_name", "team2_name" = "team_b_name")
      ) |>
      mutate(
        win_prob_ens = replace_na(win_prob_ens, 0.5),
        favorite = if_else(win_prob_ens >= 0.5, team1_name, team2_name),
        upset_prob = if_else(team1_seed > team2_seed, win_prob_ens, 1 - win_prob_ens),
        is_upset_likely = upset_prob > 0.4
      ) |>
      arrange(team1_seed)

    write_csv(first_round, r1_path)
    message(sprintf("Saved %d first-round predictions to %s", nrow(first_round), r1_path))

    # Print summary
    cat("\n--- 2026", toupper(gender), "FIRST ROUND PREDICTIONS ---\n")
    first_round |>
      select(team1_name, team1_seed, team2_name, team2_seed, win_prob_ens, favorite) |>
      print(n = Inf)
  }

  invisible(win_probs_long)
}

# ---------------------------------------------------------------------------
# get_win_prob() -- convenience function: get win probability for a specific matchup
# ---------------------------------------------------------------------------
get_win_prob <- function(team_a, team_b, gender = c("men", "women")) {
  gender <- match.arg(gender)
  paths  <- gender_paths(gender)

  matrix <- safe_read_csv(file.path(paths$processed, "win_prob_matrix_2026.csv"))
  if (is.null(matrix)) stop("Win probability matrix not found. Run predict_2026() first.")

  result <- matrix |>
    filter(
      (grepl(team_a, team_a_name, ignore.case = TRUE) &
       grepl(team_b, team_b_name, ignore.case = TRUE)) |
      (grepl(team_b, team_a_name, ignore.case = TRUE) &
       grepl(team_a, team_b_name, ignore.case = TRUE))
    )

  if (nrow(result) == 0) {
    message("No matchup found for: ", team_a, " vs ", team_b)
    return(invisible(NULL))
  }

  # Normalize so team_a is always in team_a position
  if (grepl(team_b, result$team_a_name[1], ignore.case = TRUE)) {
    result <- result |>
      mutate(
        win_prob_ens = 1 - win_prob_ens,
        tmp_name     = team_a_name,
        team_a_name  = team_b_name,
        team_b_name  = tmp_name
      ) |> select(-tmp_name)
  }

  cat(sprintf("%s vs %s:\n", result$team_a_name[1], result$team_b_name[1]))
  cat(sprintf("  %s win probability: %.1f%%\n",
              result$team_a_name[1], result$win_prob_ens[1] * 100))
  cat(sprintf("  %s win probability: %.1f%%\n",
              result$team_b_name[1], (1 - result$win_prob_ens[1]) * 100))

  invisible(result)
}
