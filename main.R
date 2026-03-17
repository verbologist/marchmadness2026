#!/usr/bin/env Rscript
# main.R -- NCAA March Madness 2026 Bracket Prediction Pipeline
# =============================================================================
# Steps:
#   0. Initialize directories
#   1. Download historical data (seeds, tournament games, team stats)
#   2. Download recent form (last 10 games, men)
#   3. Download 2026 bracket structure
#   4. Feature engineering (training matchups + 2026 prediction features)
#   5. Train ensemble model (Logistic Regression + XGBoost)
#   6. Evaluate model performance
#   7. Generate 2026 win probability matrix
#   8. Apply injury adjustments to win probability matrix
#   9. Run Monte Carlo bracket simulations (10,000 iterations)
#  10. Create visualizations + bracket plots
#
# Usage:
#   source("main.R")                        # Full pipeline, both genders
#   source("main.R")  # with args set below to skip download
#
# Prerequisites:
#   Run R/01_install_packages.R first.
#   Set CBBDATA_KEY in .Renviron (free at cbbdata.aweatherman.com).
# =============================================================================

setwd("C:/claudegit/marchmadness2026")
t_start <- proc.time()

# ---- Options ----
SKIP_DOWNLOAD <- FALSE   # Set TRUE to use cached data/processed files
RUN_MEN       <- TRUE
RUN_WOMEN     <- TRUE
FORCE_REBUILD <- FALSE   # Set TRUE to force re-download/rebuild all cached steps

gender_filter <- c(if (RUN_MEN) "men", if (RUN_WOMEN) "women")

# ---- Load all modules ----
library(here)
source(here::here("R", "utils.R"))
source(here::here("R", "00_config.R"))
source(here::here("R", "02_download_historical.R"))
source(here::here("R", "03_download_team_stats.R"))
source(here::here("R", "05_download_brackets.R"))
source(here::here("R", "06_feature_engineering.R"))
source(here::here("R", "07_model_train.R"))
source(here::here("R", "08_model_evaluate.R"))
source(here::here("R", "09_predict_2026.R"))
source(here::here("R", "11_visualize.R"))

cat("=============================================================\n")
cat("  NCAA March Madness 2026 Bracket Prediction Pipeline\n")
cat("  Date:", format(Sys.Date(), "%B %d, %Y"), "\n")
cat("  Genders:", paste(gender_filter, collapse = ", "), "\n")
cat("  Skip download:", SKIP_DOWNLOAD, "\n")
cat("=============================================================\n\n")

# ---------------------------------------------------------------------------
# STEP 0: Initialize directories
# ---------------------------------------------------------------------------
banner("STEP 0: Initializing directories")
ensure_dirs()

# ---------------------------------------------------------------------------
# STEP 1: Download historical data
# ---------------------------------------------------------------------------
if (!SKIP_DOWNLOAD) {
  banner("STEP 1: Downloading historical data (2013-2025)")

  if (RUN_MEN) {
    download_tournament_seeds(gender = "men",   seasons = HIST_SEASONS, force = FORCE_REBUILD)
    download_tournament_games_men(seasons = HIST_SEASONS,               force = FORCE_REBUILD)
    download_team_stats_men(seasons = c(HIST_SEASONS, CURRENT_SEASON),  force = FORCE_REBUILD)
  }
  if (RUN_WOMEN) {
    download_tournament_seeds(gender = "women", seasons = HIST_SEASONS, force = FORCE_REBUILD)
    download_team_stats_women(seasons = c(HIST_SEASONS, CURRENT_SEASON),force = FORCE_REBUILD)
  }
} else {
  banner("STEP 1: Skipping download (SKIP_DOWNLOAD = TRUE)")
}

# ---------------------------------------------------------------------------
# STEP 2: Download recent form (men's last-10-game rolling efficiency)
# ---------------------------------------------------------------------------
if (!SKIP_DOWNLOAD && RUN_MEN) {
  banner("STEP 2: Downloading recent form (last 10 games, men)")
  download_torvik_recent_form(seasons = c(HIST_SEASONS, CURRENT_SEASON), force = FORCE_REBUILD)
} else {
  banner("STEP 2: Skipping recent form download")
}

# ---------------------------------------------------------------------------
# STEP 3: Download 2026 bracket structure
# ---------------------------------------------------------------------------
if (!SKIP_DOWNLOAD) {
  banner("STEP 3: Downloading 2026 bracket structure")
  for (g in gender_filter) {
    download_historical_brackets(gender = g, force = FORCE_REBUILD)
    download_bracket_2026(gender = g,        force = FORCE_REBUILD)
  }
} else {
  banner("STEP 3: Skipping bracket download")
}

# ---------------------------------------------------------------------------
# STEP 4: Feature engineering
# ---------------------------------------------------------------------------
banner("STEP 4: Engineering matchup features")
for (g in gender_filter) {
  matchups <- build_matchup_features(gender = g, force = FORCE_REBUILD)
  cat(g, "training rows:", nrow(matchups), "\n")
  pred <- build_predict_features_2026(gender = g, force = FORCE_REBUILD)
  cat(g, "2026 prediction rows:", nrow(pred), "\n")
}

# ---------------------------------------------------------------------------
# STEP 5: Train ensemble model
# ---------------------------------------------------------------------------
banner("STEP 5: Training ensemble model (Logistic Regression + XGBoost)")
for (g in gender_filter) {
  train_ensemble(gender = g, force = FORCE_REBUILD)
}

# ---------------------------------------------------------------------------
# STEP 6: Evaluate model performance
# ---------------------------------------------------------------------------
banner("STEP 6: Evaluating model performance")
for (g in gender_filter) {
  tryCatch(
    evaluate_model(gender = g),
    error = function(e) message("  Model evaluation failed for ", g, ": ", e$message)
  )
}

# ---------------------------------------------------------------------------
# STEP 7: Generate 2026 win probability matrix
# ---------------------------------------------------------------------------
banner("STEP 7: Generating 2026 win probability matrices")
for (g in gender_filter) {
  predict_2026(gender = g)
}

# ---------------------------------------------------------------------------
# STEP 8: Apply injury adjustments
# ---------------------------------------------------------------------------
banner("STEP 8: Applying injury adjustments")

# Injury penalties (adj_EM reduction): ppg * 0.30 for OUT, * 0.15 for Questionable
# Sources: actionnetwork.com NCAA tournament injury report, March 17 2026
MEN_INJURY_PENALTIES <- c(
  "Miami Hurricanes"          = 8.27,  # JT Toppin OUT (ACL) -- star PF
  "Duke Blue Devils"          = 2.85,  # Caleb Foster OUT (foot) -- PG
  "Texas Tech Red Raiders"    = 2.55,
  "Wisconsin Badgers"         = 1.80,
  "UConn Huskies"             = 1.50,
  "Tennessee Volunteers"      = 1.20,
  "Gonzaga Bulldogs"          = 1.05,
  "Alabama Crimson Tide"      = 0.90,
  "Kentucky Wildcats"         = 0.75
)

WOMEN_INJURY_PENALTIES <- c(
  "Maryland Terrapins"        = 3.50,  # 2 season-ending injuries
  "Colorado State Rams"       = 2.10   # Bargesser OUT (knee)
)

logit  <- function(p) log(p / (1 - p))
ilogit <- function(x) 1 / (1 + exp(-x))

apply_injury_adj <- function(gender, penalties) {
  base_path <- file.path("data/processed", gender, "win_prob_matrix_2026.csv")
  adj_path  <- file.path("data/processed", gender, "win_prob_matrix_2026_injury_adj.csv")

  pm <- readr::read_csv(base_path, show_col_types = FALSE)
  pm_adj <- pm |>
    dplyr::mutate(
      pen_a = dplyr::coalesce(penalties[team_a_name], 0),
      pen_b = dplyr::coalesce(penalties[team_b_name], 0),
      shift = 0.10 * (pen_a - pen_b),
      win_prob_ens = dplyr::case_when(
        pen_a == 0 & pen_b == 0 ~ win_prob_ens,
        TRUE ~ ilogit(logit(pmin(pmax(win_prob_ens, 0.001), 0.999)) - shift)
      )
    ) |>
    dplyr::select(-pen_a, -pen_b, -shift)

  readr::write_csv(pm_adj, adj_path)
  message("  Injury-adjusted matrix saved: ", adj_path)
  invisible(pm_adj)
}

if (RUN_MEN)   apply_injury_adj("men",   MEN_INJURY_PENALTIES)
if (RUN_WOMEN) apply_injury_adj("women", WOMEN_INJURY_PENALTIES)

# ---------------------------------------------------------------------------
# STEP 9: Monte Carlo bracket simulation
# ---------------------------------------------------------------------------
banner(sprintf("STEP 9: Running Monte Carlo simulation (%s iterations)", format(N_SIMS, big.mark = ",")))

if (RUN_MEN) {
  source(here::here("R", "simulate_2026.R"))
  run_simulation(N = N_SIMS, seed = RANDOM_SEED, injury_adj = TRUE)
}
if (RUN_WOMEN) {
  source(here::here("R", "simulate_2026_women.R"))
  run_simulation_women(N = N_SIMS, seed = RANDOM_SEED)
}

# ---------------------------------------------------------------------------
# STEP 10: Visualizations + bracket plots
# ---------------------------------------------------------------------------
banner("STEP 10: Creating visualizations and bracket plots")
for (g in gender_filter) {
  tryCatch(
    create_visualizations(gender = g),
    error = function(e) message("  Visualization failed for ", g, ": ", e$message)
  )
}

tryCatch(
  source(here::here("R", "13_bracket_plots.R")),
  error = function(e) message("  Bracket plots failed: ", e$message)
)

# ---------------------------------------------------------------------------
# Done
# ---------------------------------------------------------------------------
t_elapsed <- (proc.time() - t_start)["elapsed"]
banner(sprintf("PIPELINE COMPLETE in %.1f minutes", t_elapsed / 60))
cat("Outputs:\n")
cat("  Men's:   output/men/\n")
cat("  Women's: output/women/\n")
cat("\nLaunch Shiny app:\n")
cat("  shiny::runApp('shiny/app.R')\n")
