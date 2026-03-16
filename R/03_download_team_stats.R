# 03_download_team_stats.R -- Download advanced team stats
# =============================================================================
# Data sources:
#   Men's:   cbbdata (BartTorvik T-Rank: AdjOE, AdjDE, AdjEM, AdjT, Four Factors)
#   Women's: wehoop box scores -> computed efficiency metrics
#            + NCAA NET rankings via rvest
#
# Output:
#   data/processed/men/team_stats_all.csv
#   data/processed/women/team_stats_all.csv
# =============================================================================

source(here::here("R", "utils.R"))
source(here::here("R", "00_config.R"))

library(dplyr)
library(tidyr)
library(readr)
library(purrr)

# ---------------------------------------------------------------------------
# Men's: BartTorvik via cbbdata
# Requires free API key: Sys.setenv(CBBDATA_KEY = "...") or in .Renviron
# Register at https://cbbdata.aweatherman.com/
# ---------------------------------------------------------------------------
download_team_stats_men <- function(seasons = c(HIST_SEASONS, CURRENT_SEASON),
                                    out_path = file.path(PROCESSED_MEN_DIR, "team_stats_all.csv"),
                                    force = FALSE) {
  if (file.exists(out_path) && !force) {
    message("Men's team stats already downloaded. Use force=TRUE to re-download.")
    return(invisible(safe_read_csv(out_path)))
  }

  if (!requireNamespace("cbbdata", quietly = TRUE)) {
    stop("cbbdata not installed. Run R/01_install_packages.R first.")
  }

  message("Downloading men's advanced stats via cbbdata (BartTorvik)...")

  stats_list <- map(seasons, function(s) {
    message("  Season ", s, "...")
    tryCatch(
      cbbdata::cbd_torvik_ratings(year = s) |>
        mutate(season = s),
      error = function(e) {
        message("    cbbdata failed for ", s, ": ", e$message)
        NULL
      }
    )
  })

  stats_raw <- bind_rows(compact(stats_list))

  # Standardise column names to project schema
  stats <- stats_raw |>
    transmute(
      season,
      team_id   = as.character(team),
      team_name = team,
      conf      = conf,
      adj_oe    = adj_o,     # Adjusted Offensive Efficiency (points per 100 poss)
      adj_de    = adj_d,     # Adjusted Defensive Efficiency (lower is better)
      adj_em    = adj_o - adj_d,  # Adjusted Efficiency Margin
      adj_t     = coalesce(adj_t, tempo),  # Adjusted Tempo
      efg_o     = coalesce(efg_o, efg),    # Offensive eFG%
      efg_d     = efg_d,     # Defensive eFG% allowed
      to_o      = coalesce(to_o, tov),     # Offensive TO rate (lower = better)
      to_d      = to_d,      # Defensive TO rate forced
      or_o      = coalesce(or_o, reb_o),   # Offensive rebound rate
      or_d      = or_d,      # Defensive rebound rate (opp OR rate)
      ft_o      = coalesce(ft_o, ftr),     # FT rate (FTA/FGA)
      ft_d      = ft_d,
      three_pct_o = coalesce(three_pct, `3p_pct`),
      three_pct_d = coalesce(three_pct_d, `3p_pct_d`),
      trank     = coalesce(barthag_rk, rank),  # T-Rank (lower = better)
      barthag   = coalesce(barthag, wab)        # Probability of beating avg team
    ) |>
    # Fill any missing columns with NA
    mutate(across(where(is.numeric), ~replace_na(., NA_real_)))

  write_csv(stats, out_path)
  message(sprintf("Saved %d team-season rows to %s", nrow(stats), out_path))
  invisible(stats)
}

# ---------------------------------------------------------------------------
# Women's: Box scores via wehoop -> computed efficiency metrics
# The women's advanced analytics ecosystem is thinner than men's.
# We compute four-factors metrics from team box score aggregates.
# ---------------------------------------------------------------------------
download_team_stats_women <- function(seasons = c(HIST_SEASONS, CURRENT_SEASON),
                                      out_path = file.path(PROCESSED_WOMEN_DIR, "team_stats_all.csv"),
                                      force = FALSE) {
  if (file.exists(out_path) && !force) {
    message("Women's team stats already downloaded. Use force=TRUE to re-download.")
    return(invisible(safe_read_csv(out_path)))
  }

  if (!requireNamespace("wehoop", quietly = TRUE)) {
    stop("wehoop not installed. Run R/01_install_packages.R first.")
  }

  message("Downloading women's team stats via wehoop box scores...")

  stats_list <- map(seasons, function(s) {
    message("  Season ", s, "...")
    tryCatch({
      box <- wehoop::load_wbb_team_box(seasons = s)

      # Compute per-season team aggregates
      box |>
        group_by(season, team_id, team_display_name) |>
        summarise(
          games       = n(),
          pts_per_g   = mean(team_score, na.rm = TRUE),
          pts_allow   = mean(opponent_score, na.rm = TRUE),
          fg_pct      = mean(field_goal_pct, na.rm = TRUE),
          fg3_pct     = mean(three_point_field_goal_pct, na.rm = TRUE),
          ft_pct      = mean(free_throw_pct, na.rm = TRUE),
          reb_off     = mean(offensive_rebounds, na.rm = TRUE),
          reb_def     = mean(defensive_rebounds, na.rm = TRUE),
          turnovers   = mean(turnovers, na.rm = TRUE),
          assists     = mean(assists, na.rm = TRUE),
          steals      = mean(steals, na.rm = TRUE),
          blocks      = mean(blocks, na.rm = TRUE),
          fga_per_g   = mean(field_goals_attempted, na.rm = TRUE),
          fta_per_g   = mean(free_throws_attempted, na.rm = TRUE),
          .groups = "drop"
        ) |>
        mutate(
          # Derived efficiency metrics (approximate four factors)
          efg_o      = fg_pct + 0.5 * (fg3_pct * fga_per_g * 0.33) / pmax(fga_per_g, 1),
          to_rate_o  = turnovers / pmax(fga_per_g + 0.44 * fta_per_g + turnovers, 1),
          or_rate    = reb_off / pmax(reb_off + reb_def, 1),
          ft_rate    = fta_per_g / pmax(fga_per_g, 1),
          # Simple efficiency margin proxy
          adj_em     = pts_per_g - pts_allow,
          adj_oe     = pts_per_g,
          adj_de     = pts_allow
        ) |>
        rename(
          team_name  = team_display_name,
          efg_o_val  = efg_o,
          to_o       = to_rate_o,
          or_o       = or_rate,
          ft_o       = ft_rate
        )
    }, error = function(e) {
      message("    wehoop failed for ", s, ": ", e$message)
      NULL
    })
  })

  stats <- bind_rows(compact(stats_list)) |>
    mutate(
      team_id = as.character(team_id),
      trank   = NA_real_,
      barthag = NA_real_
    )

  write_csv(stats, out_path)
  message(sprintf("Saved %d team-season rows to %s", nrow(stats), out_path))
  invisible(stats)
}

# ---------------------------------------------------------------------------
# download_net_rankings() -- NCAA NET rankings via rvest
# ---------------------------------------------------------------------------
download_net_rankings <- function(gender = "men",
                                  season = CURRENT_SEASON,
                                  force = FALSE) {
  paths <- gender_paths(gender)
  out_path <- file.path(paths$processed, paste0("net_rankings_", season, ".csv"))

  if (file.exists(out_path) && !force) {
    return(invisible(safe_read_csv(out_path)))
  }

  sport_path <- if (gender == "men") "basketball-men" else "basketball-women"
  url <- sprintf("https://www.ncaa.com/rankings/%s/d1/ncaa-%s-basketball-net-rankings",
                 sport_path, if (gender == "men") "mens" else "womens")

  message("Scraping NET rankings from NCAA.com...")
  result <- tryCatch({
    page  <- rvest::read_html(url)
    table <- rvest::html_table(page)[[1]]
    table |>
      mutate(season = season) |>
      rename_with(tolower) |>
      rename_with(~gsub("[^a-z0-9_]", "_", .))
  }, error = function(e) {
    message("NET rankings scrape failed: ", e$message)
    NULL
  })

  if (!is.null(result)) {
    write_csv(result, out_path)
    message(sprintf("Saved %d NET rankings to %s", nrow(result), out_path))
  }
  invisible(result)
}
