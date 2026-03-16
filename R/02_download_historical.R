# 02_download_historical.R -- Download historical game results (2013-2025)
# =============================================================================
# Data sources:
#   Men's:   hoopR::load_mbb_schedule() + ESPN API fallback
#   Women's: wehoop::load_wbb_schedule() + ESPN API fallback
#
# Output:
#   data/raw/men/historical_games.csv
#   data/raw/women/historical_games.csv
# =============================================================================

source(here::here("R", "utils.R"))
source(here::here("R", "00_config.R"))

library(dplyr)
library(readr)
library(purrr)

# ---------------------------------------------------------------------------
# ESPN API fallback helpers
# ---------------------------------------------------------------------------
.espn_games_for_season <- function(gender, season) {
  sport_path <- if (gender == "men") "mens-college-basketball" else "womens-college-basketball"
  # NCAA tournament typically runs mid-March to early April
  # Regular season: Nov - early March
  # We fetch the full season calendar
  season_start <- as.Date(paste0(season - 1, "-11-01"))
  season_end   <- as.Date(paste0(season, "-04-15"))
  dates <- seq(season_start, season_end, by = "7 days")

  games_list <- map(dates, function(d) {
    url <- sprintf(
      "https://site.api.espn.com/apis/site/v2/sports/basketball/%s/scoreboard?dates=%s&limit=100",
      sport_path, format(d, "%Y%m%d")
    )
    result <- tryCatch(
      retry_download(url),
      error = function(e) { message("  ESPN API error: ", e$message); NULL }
    )
    if (is.null(result) || is.null(result$events)) return(NULL)

    map_dfr(result$events, function(ev) {
      comps <- ev$competitions[[1]]
      if (length(comps$competitors) < 2) return(NULL)
      home <- comps$competitors[[1]]
      away <- comps$competitors[[2]]
      tibble(
        season       = season,
        game_id      = ev$id,
        game_date    = as.Date(substr(ev$date, 1, 10)),
        home_id      = home$team$id,
        home_name    = home$team$displayName,
        home_score   = as.integer(home$score),
        away_id      = away$team$id,
        away_name    = away$team$displayName,
        away_score   = as.integer(away$score),
        is_neutral   = isTRUE(comps$neutralSite),
        is_completed = isTRUE(comps$status$type$completed)
      )
    })
  })
  bind_rows(games_list)
}

.normalize_games <- function(df) {
  # Expand each game into two rows (one per team perspective)
  bind_rows(
    df |> transmute(
      season, game_id, game_date,
      team_id = home_id, team_name = home_name, team_score = home_score,
      opp_id  = away_id, opp_name  = away_name, opp_score  = away_score,
      is_neutral, is_completed
    ),
    df |> transmute(
      season, game_id, game_date,
      team_id = away_id, team_name = away_name, team_score = away_score,
      opp_id  = home_id, opp_name  = home_name, opp_score  = home_score,
      is_neutral, is_completed
    )
  ) |>
    filter(is_completed) |>
    mutate(win = as.integer(team_score > opp_score)) |>
    arrange(season, game_date, game_id, team_id)
}

# ---------------------------------------------------------------------------
# download_historical_men()
# ---------------------------------------------------------------------------
download_historical_men <- function(seasons = HIST_SEASONS,
                                    out_path = file.path(RAW_MEN_DIR, "historical_games.csv"),
                                    force = FALSE) {
  if (file.exists(out_path) && !force) {
    message("Men's historical games already downloaded. Use force=TRUE to re-download.")
    return(invisible(safe_read_csv(out_path)))
  }

  message("Downloading men's historical games via hoopR...")

  games_raw <- tryCatch({
    if (!requireNamespace("hoopR", quietly = TRUE)) stop("hoopR not installed")
    hoopR::load_mbb_schedule(seasons = seasons) |>
      filter(!is.na(home_score), !is.na(away_score)) |>
      transmute(
        season, game_id,
        game_date    = as.Date(game_date),
        home_id      = as.character(home_id),
        home_name    = home_display_name,
        home_score   = as.integer(home_score),
        away_id      = as.character(away_id),
        away_name    = away_display_name,
        away_score   = as.integer(away_score),
        is_neutral   = neutral_site,
        is_completed = status_type_completed
      )
  }, error = function(e) {
    message("hoopR failed: ", e$message)
    message("Falling back to ESPN API (slower)...")
    map_dfr(seasons, function(s) {
      message("  Season ", s, "...")
      .espn_games_for_season("men", s)
    })
  })

  games_norm <- .normalize_games(games_raw)
  write_csv(games_norm, out_path)
  message(sprintf("Saved %d team-game rows to %s", nrow(games_norm), out_path))
  invisible(games_norm)
}

# ---------------------------------------------------------------------------
# download_historical_women()
# ---------------------------------------------------------------------------
download_historical_women <- function(seasons = HIST_SEASONS,
                                      out_path = file.path(RAW_WOMEN_DIR, "historical_games.csv"),
                                      force = FALSE) {
  if (file.exists(out_path) && !force) {
    message("Women's historical games already downloaded. Use force=TRUE to re-download.")
    return(invisible(safe_read_csv(out_path)))
  }

  message("Downloading women's historical games via wehoop...")

  games_raw <- tryCatch({
    if (!requireNamespace("wehoop", quietly = TRUE)) stop("wehoop not installed")
    wehoop::load_wbb_schedule(seasons = seasons) |>
      filter(!is.na(home_score), !is.na(away_score)) |>
      transmute(
        season, game_id,
        game_date    = as.Date(game_date),
        home_id      = as.character(home_id),
        home_name    = home_display_name,
        home_score   = as.integer(home_score),
        away_id      = as.character(away_id),
        away_name    = away_display_name,
        away_score   = as.integer(away_score),
        is_neutral   = neutral_site,
        is_completed = status_type_completed
      )
  }, error = function(e) {
    message("wehoop failed: ", e$message)
    message("Falling back to ESPN API (slower)...")
    map_dfr(seasons, function(s) {
      message("  Season ", s, "...")
      .espn_games_for_season("women", s)
    })
  })

  games_norm <- .normalize_games(games_raw)
  write_csv(games_norm, out_path)
  message(sprintf("Saved %d team-game rows to %s", nrow(games_norm), out_path))
  invisible(games_norm)
}
