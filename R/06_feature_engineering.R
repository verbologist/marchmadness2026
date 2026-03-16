# 06_feature_engineering.R -- Build matchup-level training features
# =============================================================================
# Core analytics: transforms raw game results + team stats into the
# matchup-level features that drive all predictions.
#
# Key features per research literature:
#   - AdjEM differential (single most predictive: arXiv:2503.21790)
#   - Seed differential
#   - BartTorvik T-Rank differential
#   - Four Factors: eFG%, TO rate, OR rate, FT rate
#   - Tempo differential
#   - Upset indicator dummies (5v12, 6v11, 7v10, 8v9)
#
# Output:
#   data/processed/{men,women}/training_matchups.csv   (historical, 2013-2025)
#   data/processed/{men,women}/predict_matchups_2026.csv (for prediction)
# =============================================================================

source(here::here("R", "utils.R"))
source(here::here("R", "00_config.R"))

library(dplyr)
library(tidyr)
library(readr)
library(purrr)

# ---------------------------------------------------------------------------
# .build_team_lookup() -- join team stats for a given season
# ---------------------------------------------------------------------------
.build_team_lookup <- function(team_stats, season_val) {
  team_stats |>
    filter(season == season_val) |>
    select(team_id, team_name, adj_em, adj_oe, adj_de, adj_t,
           efg_o, efg_d, to_o, to_d, or_o, or_d, ft_o, ft_d,
           three_pct_o, three_pct_d, trank, barthag)
}

# ---------------------------------------------------------------------------
# .compute_matchup_features() -- generate all feature columns for one matchup
# team_a = favored/primary team, team_b = opponent
# outcome = 1 if team_a wins, 0 if team_b wins
# ---------------------------------------------------------------------------
.compute_matchup_features <- function(team_a, team_b, seed_a, seed_b,
                                      outcome, round_num, season) {
  # Differential: a - b (positive = team_a has advantage)
  tibble(
    season          = season,
    round           = round_num,
    team_a_id       = team_a$team_id,
    team_a_name     = team_a$team_name,
    team_a_seed     = seed_a,
    team_b_id       = team_b$team_id,
    team_b_name     = team_b$team_name,
    team_b_seed     = seed_b,
    outcome         = as.integer(outcome),

    # === Primary efficiency features (most predictive per literature) ===
    adj_em_diff     = team_a$adj_em    - team_b$adj_em,
    adj_oe_diff     = team_a$adj_oe    - team_b$adj_oe,
    adj_de_diff     = team_b$adj_de    - team_a$adj_de,  # flipped: lower adj_de is better
    trank_diff      = team_b$trank     - team_a$trank,   # flipped: lower rank is better

    # === Seed / ranking features ===
    seed_diff       = seed_b - seed_a,   # positive = team_a is higher seed (favored)
    log_seed_ratio  = log(pmax(seed_b, 1) / pmax(seed_a, 1)),
    net_rank_diff   = NA_real_,          # filled later from NET rankings join

    # === Four Factors (Dean Oliver's framework) ===
    efg_diff        = team_a$efg_o - team_b$efg_o,
    to_rate_diff    = team_b$to_o  - team_a$to_o,  # flipped: lower TO rate is better
    or_rate_diff    = team_a$or_o  - team_b$or_o,
    dr_rate_diff    = team_a$or_d  - team_b$or_d,  # defensive rebound rate
    ft_rate_diff    = team_a$ft_o  - team_b$ft_o,
    three_pt_rate_diff = team_a$three_pct_o - team_b$three_pct_o,

    # === Style features ===
    tempo_diff      = team_a$adj_t - team_b$adj_t,

    # === Upset indicators (historically high upset rates) ===
    is_5_12 = as.integer(sort(c(seed_a, seed_b))[1] == 5  & sort(c(seed_a, seed_b))[2] == 12),
    is_6_11 = as.integer(sort(c(seed_a, seed_b))[1] == 6  & sort(c(seed_a, seed_b))[2] == 11),
    is_7_10 = as.integer(sort(c(seed_a, seed_b))[1] == 7  & sort(c(seed_a, seed_b))[2] == 10),
    is_8_9  = as.integer(sort(c(seed_a, seed_b))[1] == 8  & sort(c(seed_a, seed_b))[2] == 9),
    seed_product = seed_a * seed_b  # captures "volatility zone"
  )
}

# ---------------------------------------------------------------------------
# build_matchup_features() -- main exported function
# ---------------------------------------------------------------------------
build_matchup_features <- function(gender = c("men", "women"),
                                   force = FALSE) {
  gender <- match.arg(gender)
  paths  <- gender_paths(gender)
  out_path <- file.path(paths$processed, "training_matchups.csv")

  if (file.exists(out_path) && !force) {
    message(gender, "'s training matchups already built. Use force=TRUE to rebuild.")
    return(invisible(safe_read_csv(out_path)))
  }

  message("Building ", gender, "'s matchup features...")

  # Load data
  team_stats      <- safe_read_csv(file.path(paths$processed, "team_stats_all.csv"))
  hist_brackets   <- safe_read_csv(file.path(paths$processed, "historical_brackets.csv"))

  if (is.null(team_stats) || is.null(hist_brackets)) {
    stop("Required data not found. Run 02, 03, and 05 scripts first.")
  }

  # Only use completed games with both teams identified
  games <- hist_brackets |>
    filter(is_completed, !is.na(team1_score), !is.na(team2_score)) |>
    mutate(
      winner_id = if_else(team1_score > team2_score, team1_id, team2_id),
      loser_id  = if_else(team1_score > team2_score, team2_id, team1_id)
    )

  # Build features for each historical game
  matchup_rows <- map_dfr(seq_len(nrow(games)), function(i) {
    g <- games[i, ]
    lookup <- .build_team_lookup(team_stats, g$season)

    ta <- lookup |> filter(team_id == g$team1_id)
    tb <- lookup |> filter(team_id == g$team2_id)

    if (nrow(ta) == 0 || nrow(tb) == 0) return(NULL)

    # Generate both perspectives (doubles training data size, improves calibration)
    row1 <- .compute_matchup_features(
      ta, tb, g$team1_seed, g$team2_seed,
      outcome   = as.integer(g$team1_score > g$team2_score),
      round_num = g$round,
      season    = g$season
    )
    row2 <- .compute_matchup_features(
      tb, ta, g$team2_seed, g$team1_seed,
      outcome   = as.integer(g$team2_score > g$team1_score),
      round_num = g$round,
      season    = g$season
    )
    bind_rows(row1, row2)
  })

  # Replace NA rounds with 1 (most tournament games are Round 1)
  matchup_rows <- matchup_rows |>
    mutate(round = replace_na(round, 1L))

  write_csv(matchup_rows, out_path)
  message(sprintf("Saved %d training matchup rows to %s", nrow(matchup_rows), out_path))
  invisible(matchup_rows)
}

# ---------------------------------------------------------------------------
# build_predict_features_2026() -- matchup features for 2026 prediction
# Generates all possible pairwise matchups within each region bracket path
# ---------------------------------------------------------------------------
build_predict_features_2026 <- function(gender = c("men", "women"),
                                         force = FALSE) {
  gender <- match.arg(gender)
  paths  <- gender_paths(gender)
  out_path <- file.path(paths$processed, "predict_matchups_2026.csv")

  if (file.exists(out_path) && !force) {
    message(gender, "'s 2026 prediction features already built.")
    return(invisible(safe_read_csv(out_path)))
  }

  message("Building 2026 ", gender, "'s all-pairs prediction features...")

  team_stats <- safe_read_csv(file.path(paths$processed, "team_stats_all.csv"))
  bracket    <- safe_read_csv(file.path(paths$processed, "bracket_2026.csv"))

  if (is.null(team_stats) || is.null(bracket)) {
    stop("Required data not found. Run 03 and 05 scripts first.")
  }

  # Get current-season stats
  stats_2026 <- .build_team_lookup(team_stats, CURRENT_SEASON)

  # All teams in the 2026 tournament (unique teams from bracket)
  tourn_teams <- bracket |>
    select(team_id = team1_id, team_name = team1_name, seed = team1_seed) |>
    bind_rows(
      bracket |> select(team_id = team2_id, team_name = team2_name, seed = team2_seed)
    ) |>
    distinct(team_id, .keep_all = TRUE) |>
    left_join(stats_2026, by = "team_id")

  # Generate all pairwise matchup features (for any possible matchup)
  pairs <- expand.grid(a = seq_len(nrow(tourn_teams)),
                       b = seq_len(nrow(tourn_teams)),
                       stringsAsFactors = FALSE) |>
    filter(a < b)  # avoid duplicates

  all_pairs <- map_dfr(seq_len(nrow(pairs)), function(i) {
    ta <- tourn_teams[pairs$a[i], ]
    tb <- tourn_teams[pairs$b[i], ]
    .compute_matchup_features(
      ta, tb, ta$seed, tb$seed,
      outcome   = NA_integer_,  # unknown -- we're predicting
      round_num = NA_integer_,
      season    = CURRENT_SEASON
    )
  })

  write_csv(all_pairs, out_path)
  message(sprintf("Saved %d pairwise prediction rows to %s", nrow(all_pairs), out_path))
  invisible(all_pairs)
}
