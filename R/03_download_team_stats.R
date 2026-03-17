# 03_download_team_stats.R -- Download advanced team stats
# =============================================================================
# Sources (confirmed working):
#   Men's efficiency:  cbd_torvik_ratings_archive()  -> AdjOE, AdjDE, AdjEM, barthag
#   Men's four factors: cbd_torvik_game_factors()    -> eFG%, TO%, OR%, FT%, tempo
#   Seeds (all years): ESPN API curatedRank           -> tournament seeds per team/year
#   Women's:           wehoop::load_wbb_team_box()   -> box-score derived metrics
#
# Output:
#   data/processed/men/team_stats_all.csv      (season-level efficiency + four factors)
#   data/processed/men/tournament_games.csv    (per-game tournament results with all features)
#   data/processed/men/seeds_all.csv           (team seeds by year)
#   data/processed/women/team_stats_all.csv
#   data/processed/women/tournament_games.csv
# =============================================================================

source(here::here("R", "utils.R"))
source(here::here("R", "00_config.R"))

library(dplyr)
library(tidyr)
library(readr)
library(purrr)
library(httr2)
library(jsonlite)

# ---------------------------------------------------------------------------
# .fetch_tournament_seeds_espn() -- pull seeds for one tournament year
# Uses ESPN scoreboard API (groups=100 for NCAA tournament)
# ---------------------------------------------------------------------------
.fetch_tournament_seeds_espn <- function(year, gender = "men") {
  sport_path <- if (gender == "men") "mens-college-basketball" else "womens-college-basketball"
  group_id   <- 100L  # groups=100 works for both men's and women's NCAA tournament

  # Use date-range query (yyyymmdd-yyyymmdd) to fetch entire tournament in
  # ONE request rather than day-by-day -- dramatically fewer API calls
  date_range <- sprintf("%s-%s",
                        format(as.Date(paste0(year, "-03-15")), "%Y%m%d"),
                        format(as.Date(paste0(year, "-04-10")), "%Y%m%d"))

  url <- sprintf(
    "https://site.api.espn.com/apis/site/v2/sports/basketball/%s/scoreboard?dates=%s&groups=%d&limit=100",
    sport_path, date_range, group_id
  )

  result <- tryCatch(retry_download(url), error = function(e) {
    message("  ESPN API failed for ", year, ": ", e$message); NULL
  })
  if (is.null(result) || length(result$events) == 0) {
    message(sprintf("  %d: no events returned", year))
    return(NULL)
  }

  seeds_df <- map_dfr(result$events, function(ev) {
    comp <- ev$competitions[[1]]
    if (length(comp$competitors) < 2) return(NULL)
    map_dfr(comp$competitors, function(team) {
      seed <- tryCatch(as.integer(team$curatedRank$current), error = function(e) NA_integer_)
      if (is.na(seed) || seed == 99) return(NULL)
      tibble(
        year      = year,
        team_name = team$team$displayName,
        team_id   = team$team$id,
        seed      = seed
      )
    })
  })

  # 2020 tournament was cancelled (COVID-19) -- no seeds expected
  if (year == 2020) {
    message("  2020: tournament cancelled (COVID-19), skipping.")
    return(NULL)
  }

  out <- seeds_df |>
    filter(!is.na(seed)) |>
    distinct(year, team_name, .keep_all = TRUE)

  message(sprintf("  %d: %d seeded teams found", year, nrow(out)))
  out
}

# ---------------------------------------------------------------------------
# download_tournament_seeds() -- seeds for all years
# ---------------------------------------------------------------------------
download_tournament_seeds <- function(gender = "men",
                                      seasons = HIST_SEASONS,
                                      force   = FALSE) {
  paths    <- gender_paths(gender)
  out_path <- file.path(paths$processed, "seeds_all.csv")

  if (file.exists(out_path) && !force) {
    message(gender, "'s seeds already downloaded.")
    return(invisible(safe_read_csv(out_path)))
  }

  message("Fetching tournament seeds from ESPN API (", min(seasons), "-", max(seasons), ")...")
  seeds <- map_dfr(seasons, .fetch_tournament_seeds_espn, gender = gender)

  write_csv(seeds, out_path)
  message(sprintf("Saved %d seed records to %s", nrow(seeds), out_path))
  invisible(seeds)
}

# ---------------------------------------------------------------------------
# download_torvik_efficiency() -- pre-tournament AdjOE/AdjDE/AdjEM/barthag
# Uses cbd_torvik_ratings_archive(), filtered to last snapshot before Selection Sunday
# ---------------------------------------------------------------------------
download_torvik_efficiency <- function(seasons = c(HIST_SEASONS, CURRENT_SEASON),
                                       force   = FALSE) {
  out_path <- file.path(PROCESSED_MEN_DIR, "torvik_efficiency_all.csv")

  if (file.exists(out_path) && !force) {
    message("Torvik efficiency data already downloaded.")
    return(invisible(safe_read_csv(out_path)))
  }

  if (!requireNamespace("cbbdata", quietly = TRUE)) stop("cbbdata not installed.")

  message("Fetching Torvik efficiency via cbbdata (ratings archive)...")

  eff_list <- map(seasons, function(s) {
    message("  Season ", s, "...")

    # Selection Sunday is typically mid-March; use March 17 as cutoff
    cutoff <- as.Date(paste0(s, "-03-17"))

    tryCatch({
      cbbdata::cbd_torvik_ratings_archive(year = s) |>
        filter(date <= cutoff) |>
        group_by(team, conf) |>
        slice_max(date, n = 1) |>
        ungroup() |>
        transmute(
          season   = s,
          team     = team,
          conf     = conf,
          adj_o    = adj_o,
          adj_d    = adj_d,
          adj_em   = adj_o - adj_d,
          adj_t    = adj_tempo,
          barthag  = barthag,
          snap_date = date
        )
    }, error = function(e) {
      message("    Failed for ", s, ": ", e$message); NULL
    })
  })

  eff <- bind_rows(compact(eff_list))
  write_csv(eff, out_path)
  message(sprintf("Saved %d team-season efficiency rows to %s", nrow(eff), out_path))
  invisible(eff)
}

# ---------------------------------------------------------------------------
# download_torvik_four_factors() -- season-average four factors per team
# Aggregates cbd_torvik_game_factors() over all games (type = "reg" + "conf")
# ---------------------------------------------------------------------------
download_torvik_four_factors <- function(seasons = c(HIST_SEASONS, CURRENT_SEASON),
                                         force   = FALSE) {
  out_path <- file.path(PROCESSED_MEN_DIR, "torvik_four_factors_all.csv")

  if (file.exists(out_path) && !force) {
    message("Torvik four factors already downloaded.")
    return(invisible(safe_read_csv(out_path)))
  }

  message("Fetching four factors via cbbdata (game_factors)...")

  ff_list <- map(seasons, function(s) {
    message("  Season ", s, "...")
    tryCatch({
      cbbdata::cbd_torvik_game_factors(year = s) |>
        filter(type %in% c("reg", "conf")) |>     # regular season only for season averages
        group_by(season = s, team) |>
        summarise(
          games       = n(),
          off_efg     = mean(off_efg,  na.rm = TRUE),
          off_to      = mean(off_to,   na.rm = TRUE),
          off_or      = mean(off_or,   na.rm = TRUE),
          off_ftr     = mean(off_ftr,  na.rm = TRUE),
          def_efg     = mean(def_efg,  na.rm = TRUE),
          def_to      = mean(def_to,   na.rm = TRUE),
          def_or      = mean(def_or,   na.rm = TRUE),
          def_ftr     = mean(def_ftr,  na.rm = TRUE),
          tempo       = mean(tempo,    na.rm = TRUE),
          off_ppp     = mean(off_ppp,  na.rm = TRUE),
          def_ppp     = mean(def_ppp,  na.rm = TRUE),
          .groups = "drop"
        )
    }, error = function(e) {
      message("    Failed for ", s, ": ", e$message); NULL
    })
  })

  ff <- bind_rows(compact(ff_list))
  write_csv(ff, out_path)
  message(sprintf("Saved %d team-season four-factor rows to %s", nrow(ff), out_path))
  invisible(ff)
}

# ---------------------------------------------------------------------------
# download_tournament_games_men() -- all tournament games with full features
# Source: cbd_torvik_game_factors(year, type="post") -- already has adj_o/adj_d
# + four factors + result per game row
# ---------------------------------------------------------------------------
download_tournament_games_men <- function(seasons = HIST_SEASONS,
                                          force   = FALSE) {
  out_path <- file.path(PROCESSED_MEN_DIR, "tournament_games.csv")

  if (file.exists(out_path) && !force) {
    message("Men's tournament games already downloaded.")
    return(invisible(safe_read_csv(out_path)))
  }

  message("Fetching men's tournament game factors (", min(seasons), "-", max(seasons), ")...")

  games_list <- map(seasons, function(s) {
    message("  Season ", s, "...")
    tryCatch({
      cbbdata::cbd_torvik_game_factors(year = s) |>
        filter(type == "post") |>
        mutate(season = s)
    }, error = function(e) {
      message("    Failed for ", s, ": ", e$message); NULL
    })
  })

  games <- bind_rows(compact(games_list))
  write_csv(games, out_path)
  message(sprintf("Saved %d tournament game rows to %s", nrow(games), out_path))
  invisible(games)
}

# ---------------------------------------------------------------------------
# download_team_stats_women() -- women's box-score derived metrics via wehoop
# ---------------------------------------------------------------------------
download_team_stats_women <- function(seasons = c(HIST_SEASONS, CURRENT_SEASON),
                                      force   = FALSE) {
  out_path <- file.path(PROCESSED_WOMEN_DIR, "team_stats_all.csv")

  if (file.exists(out_path) && !force) {
    message("Women's team stats already downloaded.")
    return(invisible(safe_read_csv(out_path)))
  }

  if (!requireNamespace("wehoop", quietly = TRUE)) stop("wehoop not installed.")

  message("Fetching women's team stats via wehoop...")

  stats_list <- map(seasons, function(s) {
    message("  Season ", s, "...")
    tryCatch({
      wehoop::load_wbb_team_box(seasons = s) |>
        group_by(season, team_id, team_display_name) |>
        summarise(
          games     = n(),
          pts_pg    = mean(team_score,             na.rm = TRUE),
          pts_allow = mean(opponent_team_score,  na.rm = TRUE),
          fg_pct    = mean(field_goal_pct,         na.rm = TRUE),
          fg3_pct   = mean(three_point_field_goal_pct, na.rm = TRUE),
          ft_pct    = mean(free_throw_pct,         na.rm = TRUE),
          reb_off   = mean(offensive_rebounds,     na.rm = TRUE),
          reb_def   = mean(defensive_rebounds,     na.rm = TRUE),
          turnovers = mean(turnovers,              na.rm = TRUE),
          fga_pg    = mean(field_goals_attempted,  na.rm = TRUE),
          fta_pg    = mean(free_throws_attempted,  na.rm = TRUE),
          .groups   = "drop"
        ) |>
        mutate(
          adj_em   = pts_pg - pts_allow,
          adj_o    = pts_pg,
          adj_d    = pts_allow,
          off_efg  = fg_pct + 0.5 * fg3_pct * (fga_pg * 0.33) / pmax(fga_pg, 1),
          off_to   = turnovers / pmax(fga_pg + 0.44 * fta_pg + turnovers, 1) * 100,
          off_or   = reb_off / pmax(reb_off + reb_def, 1) * 100,
          off_ftr  = fta_pg / pmax(fga_pg, 1) * 100,
          team_name = team_display_name
        )
    }, error = function(e) {
      message("    Failed for ", s, ": ", e$message); NULL
    })
  })

  stats <- bind_rows(compact(stats_list))
  write_csv(stats, out_path)
  message(sprintf("Saved %d women's team-season rows to %s", nrow(stats), out_path))
  invisible(stats)
}

# ---------------------------------------------------------------------------
# download_torvik_recent_form() -- last-10-game rolling efficiency per team
# Captures momentum / injury impact / late-season form not in season averages.
# Feature rationale:
#   arXiv:2508.02725 -- rolling adj_EM ranked #3 of 22 features by SHAP value
#   arXiv:2503.21790 -- late-window efficiency outperforms season avg for
#                       teams with large recent deviation from their mean
# ---------------------------------------------------------------------------
download_torvik_recent_form <- function(seasons = c(HIST_SEASONS, CURRENT_SEASON),
                                        n_games = 10,
                                        force   = FALSE) {
  out_path <- file.path(PROCESSED_MEN_DIR, "torvik_recent_form_all.csv")

  if (file.exists(out_path) && !force) {
    message("Torvik recent form already downloaded.")
    return(invisible(safe_read_csv(out_path)))
  }

  if (!requireNamespace("cbbdata", quietly = TRUE)) stop("cbbdata not installed.")

  message("Fetching per-game Torvik data for recent form (last ", n_games, " games)...")

  form_list <- map(seasons, function(s) {
    message("  Season ", s, "...")
    cutoff <- as.Date(paste0(s, "-03-16"))  # day before First Four — no leakage
    tryCatch({
      cbbdata::cbd_torvik_game_factors(year = s) |>
        filter(type %in% c("reg", "conf"),
               as.Date(date) <= cutoff) |>
        group_by(team) |>
        slice_max(order_by = as.Date(date), n = n_games, with_ties = FALSE) |>
        summarise(
          season         = s,
          form_games     = n(),
          form_win_pct   = mean(result == "W",     na.rm = TRUE),
          form_adj_em    = mean(adj_o - adj_d,      na.rm = TRUE),
          form_off_efg   = mean(off_efg,            na.rm = TRUE),
          form_tempo     = mean(tempo,              na.rm = TRUE),
          .groups = "drop"
        )
    }, error = function(e) {
      message("    Failed for ", s, ": ", e$message); NULL
    })
  })

  form <- bind_rows(compact(form_list))
  write_csv(form, out_path)
  message(sprintf("Saved %d team-season recent form rows to %s", nrow(form), out_path))
  invisible(form)
}

# ---------------------------------------------------------------------------
# download_team_stats_men() -- merge efficiency + four factors into one table
# ---------------------------------------------------------------------------
download_team_stats_men <- function(seasons = c(HIST_SEASONS, CURRENT_SEASON),
                                    force   = FALSE) {
  out_path <- file.path(PROCESSED_MEN_DIR, "team_stats_all.csv")

  if (file.exists(out_path) && !force) {
    message("Men's combined team stats already built.")
    return(invisible(safe_read_csv(out_path)))
  }

  eff <- download_torvik_efficiency(seasons = seasons, force = force)
  ff  <- download_torvik_four_factors(seasons = seasons, force = force)

  combined <- eff |>
    left_join(ff, by = c("season", "team")) |>
    select(season, team, conf, adj_o, adj_d, adj_em, adj_t, barthag,
           off_efg, off_to, off_or, off_ftr,
           def_efg, def_to, def_or, def_ftr,
           tempo, off_ppp, def_ppp, games)

  write_csv(combined, out_path)
  message(sprintf("Saved %d combined men's team-season rows to %s", nrow(combined), out_path))
  invisible(combined)
}
