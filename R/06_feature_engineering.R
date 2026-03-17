# 06_feature_engineering.R -- Build matchup-level training features
# =============================================================================
# Strategy:
#   Training data: cbd_torvik_game_factors(type="post") already contains
#   per-game adj_o, adj_d, all four factors, and result for every tournament
#   game 2013-2025. Join game pairs on game_id to compute differentials.
#   Add seed differential from ESPN API seed data.
#
#   2026 prediction: For each first-round matchup, use season-average stats
#   from team_stats_all.csv (efficiency + four factors) and seeds from ESPN.
#
# Key features (per research literature):
#   adj_em_diff  -- single most predictive (arXiv:2503.21790)
#   seed_diff    -- tournament position
#   efg_diff     -- effective field goal % (Dean Oliver Four Factors #1)
#   to_diff      -- turnover rate
#   or_diff      -- offensive rebound rate
#   ftr_diff     -- free throw rate
#   tempo_diff   -- style-of-play
#   ppp_diff     -- points per possession differential
#
# Output:
#   data/processed/men/training_matchups.csv
#   data/processed/men/predict_matchups_2026.csv
# =============================================================================

source(here::here("R", "utils.R"))
source(here::here("R", "00_config.R"))

library(dplyr)
library(tidyr)
library(readr)
library(purrr)

# ---------------------------------------------------------------------------
# .upset_flags() -- compute upset indicator dummies from two seeds
# ---------------------------------------------------------------------------
.upset_flags <- function(seed_a, seed_b) {
  s <- sort(c(seed_a, seed_b))
  list(
    is_5_12      = as.integer(s[1] == 5  & s[2] == 12),
    is_6_11      = as.integer(s[1] == 6  & s[2] == 11),
    is_7_10      = as.integer(s[1] == 7  & s[2] == 10),
    is_8_9       = as.integer(s[1] == 8  & s[2] == 9),
    seed_product = as.integer(seed_a * seed_b)
  )
}

# ---------------------------------------------------------------------------
# build_matchup_features() -- training data from tournament game_factors
# Each tournament game produces two rows (one per team perspective).
# Both rows are joined to compute differentials in one shot.
# ---------------------------------------------------------------------------
build_matchup_features <- function(gender = c("men", "women"),
                                   force  = FALSE) {
  gender <- match.arg(gender)
  paths  <- gender_paths(gender)
  out_path <- file.path(paths$processed, "training_matchups.csv")

  if (file.exists(out_path) && !force) {
    message(gender, "'s training matchups already built. Use force=TRUE to rebuild.")
    return(invisible(safe_read_csv(out_path)))
  }

  message("Building ", gender, "'s training matchup features...")

  if (gender == "men") {
    .build_men_matchups(paths, out_path)
  } else {
    .build_women_matchups(paths, out_path)
  }
}

# ---------------------------------------------------------------------------
# .build_men_matchups() -- tournament results from game_factors + season-avg stats
#
# IMPORTANT: Use SEASON-AVERAGE efficiency/four-factors (from team_stats_all.csv)
# for all features, NOT game-specific adj_o/adj_d from game_factors.
# This ensures training features are on the same scale as 2026 predictions.
# Game_factors contributes: game_id, team, opp, result, date.
# team_stats_all contributes: all efficiency and four-factor features.
# ---------------------------------------------------------------------------
.build_men_matchups <- function(paths, out_path) {
  tourn_games  <- safe_read_csv(file.path(paths$processed, "tournament_games.csv"))
  team_stats   <- safe_read_csv(file.path(paths$processed, "team_stats_all.csv"))
  seeds_all    <- safe_read_csv(file.path(paths$processed, "seeds_all.csv"))
  recent_form  <- tryCatch(
    safe_read_csv(file.path(paths$processed, "torvik_recent_form_all.csv")),
    error = function(e) NULL
  )

  if (is.null(tourn_games)) stop("tournament_games.csv not found.")
  if (is.null(team_stats))  stop("team_stats_all.csv not found.")
  if (is.null(seeds_all))   stop("seeds_all.csv not found.")

  # Normalise Torvik team names (already canonical in team_stats)
  # Get unique (season, team, opp, result, game_id, date) from tournament games
  games <- tourn_games |>
    select(game_id, season, date, team, opp, result) |>
    distinct()

  # Join season-average stats for team_a and team_b
  stats_a <- team_stats |>
    select(season,
           team,
           a_adj_o = adj_o, a_adj_d = adj_d, a_adj_em = adj_em,
           a_adj_t = adj_t,
           a_off_efg = off_efg, a_off_to = off_to,
           a_off_or  = off_or,  a_off_ftr = off_ftr,
           a_def_efg = def_efg, a_def_to  = def_to,
           a_def_or  = def_or,  a_def_ftr = def_ftr,
           a_ppp     = off_ppp, a_def_ppp = def_ppp)

  stats_b <- team_stats |>
    select(season,
           opp   = team,
           b_adj_o = adj_o, b_adj_d = adj_d, b_adj_em = adj_em,
           b_adj_t = adj_t,
           b_off_efg = off_efg, b_off_to = off_to,
           b_off_or  = off_or,  b_off_ftr = off_ftr,
           b_def_efg = def_efg, b_def_to  = def_to,
           b_def_or  = def_or,  b_def_ftr = def_ftr,
           b_ppp     = off_ppp, b_def_ppp = def_ppp)

  # Seeds crosswalk: Torvik names need mapping from ESPN seed names
  name_xwalk <- tryCatch(cbbdata::cbd_match_teams(), error = function(e) character(0))

  seeds_torvik <- seeds_all |>
    mutate(torvik_name = coalesce(unname(name_xwalk[team_name]), team_name)) |>
    select(season = year, torvik_name, seed)

  # Recent form tables (if available)
  form_a_join <- if (!is.null(recent_form))
    recent_form |> select(season, team,
                          fa_form_win_pct = form_win_pct,
                          fa_form_adj_em  = form_adj_em,
                          fa_form_off_efg = form_off_efg)
  else NULL

  form_b_join <- if (!is.null(recent_form))
    recent_form |> select(season, opp = team,
                          fb_form_win_pct = form_win_pct,
                          fb_form_adj_em  = form_adj_em,
                          fb_form_off_efg = form_off_efg)
  else NULL

  # Conference -> region lookup for regional familiarity feature
  conf_region <- team_stats |>
    select(season, team, conf) |>
    mutate(region = coalesce(CONF_REGION_MAP[conf], "Unknown"))

  game_pairs <- games |>
    left_join(stats_a, by = c("season", "team")) |>
    left_join(stats_b, by = c("season", "opp"))  |>
    left_join(seeds_torvik |> rename(seed_a = seed), by = c("season", "team" = "torvik_name")) |>
    left_join(seeds_torvik |> rename(seed_b = seed), by = c("season", "opp"  = "torvik_name")) |>
    left_join(conf_region |> rename(region_a = region), by = c("season", "team")) |>
    left_join(conf_region |> rename(opp = team, region_b = region), by = c("season", "opp")) |>
    group_by(season) |>
    mutate(round = as.integer(dense_rank(date))) |>
    ungroup()

  if (!is.null(form_a_join)) {
    game_pairs <- game_pairs |>
      left_join(form_a_join, by = c("season", "team")) |>
      left_join(form_b_join, by = c("season", "opp"))
  } else {
    game_pairs <- game_pairs |>
      mutate(fa_form_win_pct=NA_real_, fa_form_adj_em=NA_real_, fa_form_off_efg=NA_real_,
             fb_form_win_pct=NA_real_, fb_form_adj_em=NA_real_, fb_form_off_efg=NA_real_)
  }

  # Drop rows where season-avg stats are missing for either team
  game_pairs <- game_pairs |>
    filter(!is.na(a_adj_em), !is.na(b_adj_em))

  matchups <- game_pairs |>
    mutate(
      outcome = as.integer(result == "W"),

      # === Efficiency (season averages -- same scale as 2026 predictions) ===
      adj_em_diff  = a_adj_em - b_adj_em,
      adj_oe_diff  = a_adj_o  - b_adj_o,
      adj_de_diff  = b_adj_d  - a_adj_d,

      # === Seed features ===
      seed_diff      = replace_na(seed_b - seed_a, 0),
      log_seed_ratio = log(pmax(replace_na(seed_b, 8), 1) / pmax(replace_na(seed_a, 8), 1)),

      # === Four Factors (season averages) ===
      efg_diff     = a_off_efg - b_off_efg,
      to_diff      = b_off_to  - a_off_to,
      or_diff      = a_off_or  - b_off_or,
      ftr_diff     = a_off_ftr - b_off_ftr,
      def_efg_diff = b_def_efg - a_def_efg,
      def_to_diff  = a_def_to  - b_def_to,
      def_or_diff  = a_def_or  - b_def_or,
      ppp_diff     = a_ppp     - b_ppp,
      tempo_diff   = replace_na(a_adj_t - b_adj_t, 0),

      # === Recent form (last 10 games) ===
      # form_adj_em_diff: captures momentum, late injuries not in season avg
      form_adj_em_diff  = replace_na(fa_form_adj_em  - fb_form_adj_em,  0),
      form_win_pct_diff = replace_na(fa_form_win_pct - fb_form_win_pct, 0),

      # === Tempo mismatch interactions (arXiv:2508.02725) ===
      # When fast team is also more efficient: extra possessions compound edge
      # When fast team shoots better: pace advantage is shooting advantage too
      tempo_em_interaction  = replace_na(tempo_diff * adj_em_diff, 0),
      tempo_efg_interaction = replace_na(tempo_diff * efg_diff,    0),

      # === Regional familiarity proxy ===
      # Conference geography alignment with tournament region => fan support / travel
      # 1 = team's conference is geographically aligned with its tournament region
      region_fam_a = as.integer(!is.na(region_a) & region_a != "Unknown"),
      region_fam_b = as.integer(!is.na(region_b) & region_b != "Unknown"),
      region_fam_diff = replace_na(region_fam_a - region_fam_b, 0),

      # === Upset indicators ===
      is_5_12  = as.integer(replace_na(pmin(seed_a,seed_b),0)==5  & replace_na(pmax(seed_a,seed_b),0)==12),
      is_6_11  = as.integer(replace_na(pmin(seed_a,seed_b),0)==6  & replace_na(pmax(seed_a,seed_b),0)==11),
      is_7_10  = as.integer(replace_na(pmin(seed_a,seed_b),0)==7  & replace_na(pmax(seed_a,seed_b),0)==10),
      is_8_9   = as.integer(replace_na(pmin(seed_a,seed_b),0)==8  & replace_na(pmax(seed_a,seed_b),0)==9),
      seed_product = replace_na(as.integer(seed_a * seed_b), 0)
    ) |>
    select(season, round, game_id,
           team_a = team, team_b = opp, seed_a, seed_b, outcome,
           adj_em_diff, adj_oe_diff, adj_de_diff,
           seed_diff, log_seed_ratio,
           form_adj_em_diff, form_win_pct_diff,
           efg_diff, to_diff, or_diff, ftr_diff,
           def_efg_diff, def_to_diff, def_or_diff,
           ppp_diff, tempo_diff,
           tempo_em_interaction, tempo_efg_interaction,
           region_fam_diff,
           is_5_12, is_6_11, is_7_10, is_8_9, seed_product)

  write_csv(matchups, out_path)
  message(sprintf("Saved %d training matchup rows (%d seasons, %d with stats) to %s",
                  nrow(matchups), n_distinct(matchups$season),
                  sum(!is.na(matchups$adj_em_diff)), out_path))
  invisible(matchups)
}

# ---------------------------------------------------------------------------
# .build_women_matchups() -- uses hoopR + wehoop derived stats
# ---------------------------------------------------------------------------
.build_women_matchups <- function(paths, out_path) {
  hist_games  <- safe_read_csv(file.path(paths$processed, "historical_brackets.csv"))
  team_stats  <- safe_read_csv(file.path(paths$processed, "team_stats_all.csv"))
  seeds_all   <- safe_read_csv(file.path(paths$processed, "seeds_all.csv"))

  if (is.null(hist_games) || is.null(team_stats)) {
    stop("Required files missing. Run 02 and 03 scripts first.")
  }

  games <- hist_games |>
    filter(is_completed, !is.na(team1_score), !is.na(team2_score)) |>
    mutate(outcome1 = as.integer(team1_score > team2_score))

  matchup_rows <- map_dfr(seq_len(nrow(games)), function(i) {
    g  <- games[i, ]
    s  <- g$season
    ta <- team_stats |> filter(season == s, team_display_name == g$team1_name)
    tb <- team_stats |> filter(season == s, team_display_name == g$team2_name)
    if (nrow(ta) == 0 || nrow(tb) == 0) return(NULL)

    seed_a <- seeds_all |> filter(year == s, team_name == g$team1_name) |> pull(seed)
    seed_b <- seeds_all |> filter(year == s, team_name == g$team2_name) |> pull(seed)
    seed_a <- if (length(seed_a) == 0) NA_integer_ else seed_a[1]
    seed_b <- if (length(seed_b) == 0) NA_integer_ else seed_b[1]

    bind_rows(
      tibble(
        season = s, round = NA_integer_, game_id = g$game_id,
        team_a = g$team1_name, team_b = g$team2_name,
        seed_a = seed_a, seed_b = seed_b,
        outcome = g$outcome1,
        adj_em_diff = ta$adj_em - tb$adj_em,
        adj_oe_diff = ta$adj_o  - tb$adj_o,
        adj_de_diff = tb$adj_d  - ta$adj_d,
        seed_diff   = replace_na(seed_b - seed_a, 0),
        log_seed_ratio = log(pmax(seed_b,1)/pmax(seed_a,1)),
        efg_diff    = ta$off_efg - tb$off_efg,
        to_diff     = tb$off_to  - ta$off_to,
        or_diff     = ta$off_or  - tb$off_or,
        ftr_diff    = ta$off_ftr - tb$off_ftr,
        def_efg_diff = 0, def_to_diff = 0, def_or_diff = 0,
        ppp_diff    = (ta$adj_o - ta$adj_d) - (tb$adj_o - tb$adj_d),
        tempo_diff  = 0,
        is_5_12 = 0L, is_6_11 = 0L, is_7_10 = 0L, is_8_9 = 0L,
        seed_product = replace_na(as.integer(seed_a * seed_b), 0)
      )
    )
  })

  write_csv(matchup_rows, out_path)
  message(sprintf("Saved %d women's training rows to %s", nrow(matchup_rows), out_path))
  invisible(matchup_rows)
}

# ---------------------------------------------------------------------------
# build_predict_features_2026() -- all-pairs features for 2026 prediction
# ---------------------------------------------------------------------------
build_predict_features_2026 <- function(gender = c("men", "women"),
                                         force  = FALSE) {
  gender <- match.arg(gender)
  paths  <- gender_paths(gender)
  out_path <- file.path(paths$processed, "predict_matchups_2026.csv")

  if (file.exists(out_path) && !force) {
    message(gender, "'s 2026 prediction features already built.")
    return(invisible(safe_read_csv(out_path)))
  }

  message("Building 2026 ", gender, "'s all-pairs prediction features...")

  team_stats <- safe_read_csv(file.path(paths$processed, "team_stats_all.csv"))
  seeds_2026 <- safe_read_csv(file.path(paths$processed, "seeds_all.csv")) |>
    filter(year == CURRENT_SEASON)
  bracket    <- safe_read_csv(file.path(paths$processed, "bracket_2026.csv"))

  if (is.null(team_stats) || is.null(bracket)) {
    stop("Required data not found. Run 03 and 05 scripts first.")
  }

  # Season stats: use most recent season available (2026 if present, else 2025)
  available_seasons <- sort(unique(team_stats$season), decreasing = TRUE)
  use_season <- available_seasons[1]
  message("  Using season ", use_season, " stats for 2026 predictions")

  stats <- team_stats |> filter(season == use_season)

  # Recent form (last 10 games) -- optional
  recent_form_2026 <- NULL
  if (gender == "men") {
    recent_form_2026 <- tryCatch(
      safe_read_csv(file.path(paths$processed, "torvik_recent_form_all.csv")) |>
        filter(season == use_season),
      error = function(e) NULL
    )
    if (!is.null(recent_form_2026))
      message("  Recent form loaded: ", nrow(recent_form_2026), " teams")
    else
      message("  Recent form not available; form features will be 0")
  }

  # All teams in the 2026 bracket
  tourn_teams_raw <- bind_rows(
    bracket |> select(espn_name = team1_name, seed = team1_seed),
    bracket |> select(espn_name = team2_name, seed = team2_seed)
  ) |> distinct(espn_name, .keep_all = TRUE)

  if (gender == "men") {
    # Men: normalise ESPN display names -> Torvik canonical names via cbbdata crosswalk
    name_xwalk <- cbbdata::cbd_match_teams()
    name_xwalk_patched <- name_xwalk
    name_xwalk_patched["NC State Wolfpack"]            <- "N.C. State"
    name_xwalk_patched["Long Island University Sharks"] <- "LIU"
    tourn_teams <- tourn_teams_raw |>
      mutate(team = coalesce(unname(name_xwalk_patched[espn_name]), espn_name)) |>
      left_join(stats, by = "team")
  } else {
    # Women: stats keyed by ESPN display name (team_name), no crosswalk needed
    tourn_teams <- tourn_teams_raw |>
      mutate(team = espn_name) |>
      left_join(stats, by = c("espn_name" = "team_name"))
  }

  tourn_teams <- tourn_teams |>
    left_join(seeds_2026 |> select(espn_name = team_name, espn_seed = seed), by = "espn_name") |>
    mutate(seed = coalesce(as.integer(seed), as.integer(espn_seed)))

  # Join recent form onto tourn_teams (men only)
  if (!is.null(recent_form_2026)) {
    tourn_teams <- tourn_teams |>
      left_join(
        recent_form_2026 |> select(team,
                                   form_win_pct = form_win_pct,
                                   form_adj_em  = form_adj_em),
        by = "team"
      )
  } else {
    tourn_teams <- tourn_teams |>
      mutate(form_win_pct = NA_real_, form_adj_em = NA_real_)
  }

  # Conference -> region familiarity (1 = conference geographically aligned with region)
  # Uses CONF_REGION_MAP from 00_config.R
  if ("conf" %in% names(tourn_teams)) {
    tourn_teams <- tourn_teams |>
      mutate(region_fam = as.integer(!is.na(conf) & !is.na(CONF_REGION_MAP[conf]) &
                                       CONF_REGION_MAP[conf] != "Unknown"))
  } else {
    tourn_teams <- tourn_teams |> mutate(region_fam = 0L)
  }

  # All pairwise combinations
  n <- nrow(tourn_teams)
  pairs <- expand.grid(a = seq_len(n), b = seq_len(n)) |>
    filter(a < b)

  all_pairs <- map_dfr(seq_len(nrow(pairs)), function(i) {
    ta <- tourn_teams[pairs$a[i], ]
    tb <- tourn_teams[pairs$b[i], ]

    seed_a <- ta$seed; seed_b <- tb$seed
    s <- c(sort(c(seed_a, seed_b)))

    tempo_diff_val <- replace_na(ta$tempo - tb$tempo, 0)
    adj_em_diff_val <- replace_na((ta$adj_o - ta$adj_d) - (tb$adj_o - tb$adj_d), 0)
    efg_diff_val    <- replace_na(ta$off_efg - tb$off_efg, 0)

    tibble(
      season = CURRENT_SEASON,
      round  = NA_integer_,
      game_id = paste0(ta$espn_name, "_vs_", tb$espn_name),
      team_a = ta$espn_name, team_b = tb$espn_name,
      seed_a = seed_a, seed_b = seed_b,
      outcome = NA_integer_,

      adj_em_diff  = adj_em_diff_val,
      adj_oe_diff  = replace_na(ta$adj_o - tb$adj_o, 0),
      adj_de_diff  = replace_na(tb$adj_d - ta$adj_d, 0),
      seed_diff    = replace_na(seed_b - seed_a, 0),
      log_seed_ratio = log(pmax(replace_na(seed_b,8), 1) / pmax(replace_na(seed_a,8), 1)),

      # === Recent form (last 10 games) ===
      form_adj_em_diff  = replace_na(ta$form_adj_em  - tb$form_adj_em,  0),
      form_win_pct_diff = replace_na(ta$form_win_pct - tb$form_win_pct, 0),

      efg_diff     = efg_diff_val,
      to_diff      = replace_na(tb$off_to  - ta$off_to,  0),
      or_diff      = replace_na(ta$off_or  - tb$off_or,  0),
      ftr_diff     = replace_na(ta$off_ftr - tb$off_ftr, 0),
      def_efg_diff = replace_na(tb$def_efg - ta$def_efg, 0),
      def_to_diff  = replace_na(ta$def_to  - tb$def_to,  0),
      def_or_diff  = replace_na(ta$def_or  - tb$def_or,  0),
      ppp_diff     = replace_na(ta$off_ppp - tb$off_ppp, 0),
      tempo_diff   = tempo_diff_val,

      # === Tempo mismatch interactions ===
      tempo_em_interaction  = tempo_diff_val * adj_em_diff_val,
      tempo_efg_interaction = tempo_diff_val * efg_diff_val,

      # === Regional familiarity proxy ===
      region_fam_diff = replace_na(ta$region_fam - tb$region_fam, 0L),

      is_5_12  = as.integer(!is.na(s[1]) & s[1]==5  & !is.na(s[2]) & s[2]==12),
      is_6_11  = as.integer(!is.na(s[1]) & s[1]==6  & !is.na(s[2]) & s[2]==11),
      is_7_10  = as.integer(!is.na(s[1]) & s[1]==7  & !is.na(s[2]) & s[2]==10),
      is_8_9   = as.integer(!is.na(s[1]) & s[1]==8  & !is.na(s[2]) & s[2]==9),
      seed_product = replace_na(as.integer(seed_a * seed_b), 64)
    )
  })

  write_csv(all_pairs, out_path)
  message(sprintf("Saved %d pairwise 2026 prediction rows to %s", nrow(all_pairs), out_path))
  invisible(all_pairs)
}
