#!/usr/bin/env Rscript
# main.R -- NCAA March Madness 2026 Bracket Prediction Pipeline
# =============================================================================
# Steps:
#   1. Download historical game results (2013-2025)
#   2. Download current 2026 team stats
#   3. Download NET rankings
#   4. Download 2026 bracket matchups
#   5. Engineer matchup features
#   6. Train ensemble model (Logistic Regression + XGBoost)
#   7. Evaluate model performance
#   8. Generate 2026 win probability predictions
#   9. Run Monte Carlo bracket simulation (10,000 iterations)
#  10. Create visualizations
#
# Usage:
#   Rscript main.R                   # Full pipeline, both genders
#   Rscript main.R --skip-download   # Skip data download (use cached)
#   Rscript main.R --men-only        # Men's tournament only
#   Rscript main.R --women-only      # Women's tournament only
#
# Prerequisites:
#   1. Run R/01_install_packages.R to install required packages
#   2. Set CBBDATA_KEY in .Renviron for BartTorvik data (free key at cbbdata.io)
# =============================================================================

library(here)
t_start <- proc.time()

# Parse command-line arguments
args           <- commandArgs(trailingOnly = TRUE)
skip_download  <- "--skip-download" %in% args
gender_filter  <- if ("--men-only"   %in% args) "men" else
                  if ("--women-only" %in% args) "women" else
                  c("men", "women")

cat("=============================================================\n")
cat("  NCAA March Madness 2026 Bracket Prediction Pipeline\n")
cat("  Date:", format(Sys.Date(), "%B %d, %Y"), "\n")
cat("  Genders:", paste(gender_filter, collapse = ", "), "\n")
cat("  Skip download:", skip_download, "\n")
cat("=============================================================\n\n")

# ---------------------------------------------------------------------------
# Source all modules
# ---------------------------------------------------------------------------
source(here::here("R", "utils.R"))
source(here::here("R", "00_config.R"))
source(here::here("R", "02_download_historical.R"))
source(here::here("R", "03_download_team_stats.R"))
source(here::here("R", "05_download_brackets.R"))
source(here::here("R", "06_feature_engineering.R"))
source(here::here("R", "07_model_train.R"))
source(here::here("R", "08_model_evaluate.R"))
source(here::here("R", "09_predict_2026.R"))
source(here::here("R", "10_monte_carlo.R"))
source(here::here("R", "11_visualize.R"))

# ---------------------------------------------------------------------------
# Step 0: Initialize directories
# ---------------------------------------------------------------------------
banner("STEP 0: Initializing directories")
ensure_dirs()

# ---------------------------------------------------------------------------
# Step 1: Download historical game results
# ---------------------------------------------------------------------------
if (!skip_download) {
  banner("STEP 1: Downloading historical game results (2013-2025)")
  if ("men"   %in% gender_filter) download_historical_men()
  if ("women" %in% gender_filter) download_historical_women()
} else {
  banner("STEP 1: Skipping download (--skip-download)")
}

# ---------------------------------------------------------------------------
# Step 2: Download team advanced stats
# ---------------------------------------------------------------------------
if (!skip_download) {
  banner("STEP 2: Downloading advanced team stats (KenPom/BartTorvik)")
  if ("men"   %in% gender_filter) download_team_stats_men()
  if ("women" %in% gender_filter) download_team_stats_women()
}

# ---------------------------------------------------------------------------
# Step 3: Download NET rankings
# ---------------------------------------------------------------------------
if (!skip_download) {
  banner("STEP 3: Downloading NET rankings")
  for (g in gender_filter) {
    tryCatch(
      download_net_rankings(gender = g, season = CURRENT_SEASON),
      error = function(e) message("NET rankings failed for ", g, ": ", e$message)
    )
  }
}

# ---------------------------------------------------------------------------
# Step 4: Download 2026 bracket structure + historical brackets
# ---------------------------------------------------------------------------
if (!skip_download) {
  banner("STEP 4: Downloading 2026 bracket structure")
  for (g in gender_filter) {
    download_historical_brackets(gender = g)
    download_bracket_2026(gender = g)
  }
}

# ---------------------------------------------------------------------------
# Step 5: Feature engineering
# ---------------------------------------------------------------------------
banner("STEP 5: Engineering matchup features")
for (g in gender_filter) {
  build_matchup_features(gender = g)
  build_predict_features_2026(gender = g)
}

# ---------------------------------------------------------------------------
# Step 6: Train ensemble model
# ---------------------------------------------------------------------------
banner("STEP 6: Training ensemble model (Logistic Regression + XGBoost)")
for (g in gender_filter) {
  train_ensemble(gender = g)
}

# ---------------------------------------------------------------------------
# Step 7: Evaluate models
# ---------------------------------------------------------------------------
banner("STEP 7: Evaluating model performance")
for (g in gender_filter) {
  tryCatch(
    evaluate_model(gender = g),
    error = function(e) message("Model evaluation failed: ", e$message)
  )
}

# ---------------------------------------------------------------------------
# Step 8: Generate 2026 predictions
# ---------------------------------------------------------------------------
banner("STEP 8: Generating 2026 win probability predictions")
for (g in gender_filter) {
  predict_2026(gender = g)
}

# ---------------------------------------------------------------------------
# Step 9: Monte Carlo simulation
# ---------------------------------------------------------------------------
banner(sprintf("STEP 9: Running Monte Carlo simulation (%s iterations)", format(N_SIMS, big.mark=",")))
for (g in gender_filter) {
  simulate_bracket(gender = g, n_sims = N_SIMS)
}

# ---------------------------------------------------------------------------
# Step 10: Visualizations
# ---------------------------------------------------------------------------
banner("STEP 10: Creating visualizations")
for (g in gender_filter) {
  tryCatch(
    create_visualizations(gender = g),
    error = function(e) message("Visualization failed: ", e$message)
  )
}

# ---------------------------------------------------------------------------
# Done
# ---------------------------------------------------------------------------
t_elapsed <- (proc.time() - t_start)["elapsed"]
banner(sprintf("PIPELINE COMPLETE in %.1f minutes", t_elapsed / 60))
message("Outputs:")
message("  Men's:   ", file.path(OUTPUT_DIR, "men"))
message("  Women's: ", file.path(OUTPUT_DIR, "women"))
message("")
message("Launch Shiny app:")
message("  shiny::runApp('shiny/app.R')")
