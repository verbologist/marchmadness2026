# 05_download_brackets.R -- Download 2026 bracket structure + historical brackets
# =============================================================================
# The 2026 brackets have been announced (Selection Sunday: March 15, 2026).
#   Men's:   Duke #1 overall seed (South region)
#   Women's: UConn #1 overall seed
#
# Sources:
#   - ESPN API for current bracket matchups
#   - hoopR/wehoop historical schedule filtered to tournament games
#
# Output:
#   data/processed/men/bracket_2026.csv
#   data/processed/women/bracket_2026.csv
#   data/processed/men/historical_brackets.csv
#   data/processed/women/historical_brackets.csv
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
# .fetch_espn_bracket() -- parse ESPN scoreboard for tournament games
# ---------------------------------------------------------------------------
.fetch_espn_bracket_games <- function(gender, year) {
  sport_path <- if (gender == "men") "mens-college-basketball" else "womens-college-basketball"
  group_id   <- if (gender == "men") 100 else 150  # ESPN group IDs for NCAA tournament

  # Sweep tournament dates: typically March 17 - April 7
  start_date <- as.Date(paste0(year, "-03-15"))
  end_date   <- as.Date(paste0(year, "-04-10"))
  dates <- seq(start_date, end_date, by = 1)

  games <- map_dfr(dates, function(d) {
    url <- sprintf(
      "https://site.api.espn.com/apis/site/v2/sports/basketball/%s/scoreboard?dates=%s&groups=%d&limit=50",
      sport_path, format(d, "%Y%m%d"), group_id
    )
    result <- tryCatch(
      retry_download(url),
      error = function(e) NULL
    )
    if (is.null(result) || is.null(result$events) || length(result$events) == 0) return(NULL)

    map_dfr(result$events, function(ev) {
      comps <- ev$competitions[[1]]
      if (length(comps$competitors) < 2) return(NULL)

      # Identify home/away (in neutral-site tournaments, order varies)
      c1 <- comps$competitors[[1]]
      c2 <- comps$competitors[[2]]

      # Extract seed from linescores or notes
      seed1 <- tryCatch(as.integer(c1$curatedRank$current), error = function(e) NA_integer_)
      seed2 <- tryCatch(as.integer(c2$curatedRank$current), error = function(e) NA_integer_)

      # Extract round from notes
      notes <- tryCatch(comps$notes[[1]]$headline, error = function(e) "")
      round_num <- dplyr::case_when(
        grepl("First Four|Play-In",   notes, ignore.case = TRUE) ~ 0L,
        grepl("First Round|Round 64", notes, ignore.case = TRUE) ~ 1L,
        grepl("Second Round|Round 32",notes, ignore.case = TRUE) ~ 2L,
        grepl("Sweet 16|Sweet Sixteen",notes,ignore.case = TRUE) ~ 3L,
        grepl("Elite 8|Elite Eight",  notes, ignore.case = TRUE) ~ 4L,
        grepl("Final Four",           notes, ignore.case = TRUE) ~ 5L,
        grepl("Championship|National Championship", notes, ignore.case = TRUE) ~ 6L,
        TRUE ~ NA_integer_
      )

      tibble(
        season         = year,
        game_id        = ev$id,
        game_date      = as.Date(substr(ev$date, 1, 10)),
        round          = round_num,
        round_label    = notes,
        team1_id       = c1$team$id,
        team1_name     = c1$team$displayName,
        team1_seed     = seed1,
        team1_score    = tryCatch(as.integer(c1$score), error = function(e) NA_integer_),
        team2_id       = c2$team$id,
        team2_name     = c2$team$displayName,
        team2_seed     = seed2,
        team2_score    = tryCatch(as.integer(c2$score), error = function(e) NA_integer_),
        is_neutral     = isTRUE(comps$neutralSite),
        is_completed   = isTRUE(comps$status$type$completed)
      )
    })
  })
  games |> filter(!is.na(round))
}

# ---------------------------------------------------------------------------
# download_bracket_2026() -- current 2026 tournament bracket
# ---------------------------------------------------------------------------
download_bracket_2026 <- function(gender = c("men", "women"),
                                  force = FALSE) {
  gender <- match.arg(gender)
  paths  <- gender_paths(gender)
  out_path <- file.path(paths$processed, "bracket_2026.csv")

  if (file.exists(out_path) && !force) {
    message(gender, "'s 2026 bracket already downloaded.")
    return(invisible(safe_read_csv(out_path)))
  }

  message("Fetching 2026 ", gender, "'s tournament bracket from ESPN API...")
  bracket <- .fetch_espn_bracket_games(gender, 2026)

  if (nrow(bracket) == 0) {
    warning("No bracket data returned from ESPN API. Tournament may not have started yet.")
    return(invisible(NULL))
  }

  write_csv(bracket, out_path)
  message(sprintf("Saved %d bracket games to %s", nrow(bracket), out_path))
  invisible(bracket)
}

# ---------------------------------------------------------------------------
# download_historical_brackets() -- 2013-2025 tournament results for training
# ---------------------------------------------------------------------------
download_historical_brackets <- function(gender = c("men", "women"),
                                         seasons = HIST_SEASONS,
                                         force = FALSE) {
  gender <- match.arg(gender)
  paths  <- gender_paths(gender)
  out_path <- file.path(paths$processed, "historical_brackets.csv")

  if (file.exists(out_path) && !force) {
    message(gender, "'s historical brackets already downloaded.")
    return(invisible(safe_read_csv(out_path)))
  }

  message("Fetching historical ", gender, "'s tournament brackets (", min(seasons), "-", max(seasons), ")...")

  # Try hoopR/wehoop first (fastest)
  brackets <- tryCatch({
    load_fn <- if (gender == "men") hoopR::load_mbb_schedule else wehoop::load_wbb_schedule
    raw <- load_fn(seasons = seasons)

    # Filter to tournament games
    tourn_filter <- if (gender == "men") {
      raw |> filter(
        season_type == 3 |  # postseason
        grepl("NCAA|March Madness|Tournament", notes_headline, ignore.case = TRUE)
      )
    } else {
      raw |> filter(season_type == 3)
    }

    tourn_filter |>
      transmute(
        season,
        game_id   = as.character(game_id),
        game_date = as.Date(game_date),
        round     = NA_integer_,  # Not always available in schedule data
        team1_id  = as.character(home_id),
        team1_name= home_display_name,
        team1_seed= NA_integer_,
        team1_score = as.integer(home_score),
        team2_id  = as.character(away_id),
        team2_name= away_display_name,
        team2_seed= NA_integer_,
        team2_score = as.integer(away_score),
        is_neutral = neutral_site,
        is_completed = status_type_completed
      ) |>
      filter(is_completed)
  }, error = function(e) {
    message("hoopR/wehoop failed for historical brackets: ", e$message)
    message("Falling back to ESPN API (fetching year by year)...")
    map_dfr(seasons, function(s) {
      message("  Season ", s, "...")
      .fetch_espn_bracket_games(gender, s)
    })
  })

  write_csv(brackets, out_path)
  message(sprintf("Saved %d historical tournament games to %s", nrow(brackets), out_path))
  invisible(brackets)
}

# ---------------------------------------------------------------------------
# get_2026_first_round_matchups() -- structured first-round matchups for 2026
# Returns a tibble ready for feature engineering / prediction
# ---------------------------------------------------------------------------
get_2026_first_round_matchups <- function(gender = c("men", "women")) {
  gender <- match.arg(gender)
  paths  <- gender_paths(gender)
  bracket_path <- file.path(paths$processed, "bracket_2026.csv")

  if (!file.exists(bracket_path)) {
    stop("2026 bracket not downloaded. Run download_bracket_2026('", gender, "') first.")
  }

  bracket <- safe_read_csv(bracket_path)
  bracket |>
    filter(round == 1 | round == 0) |>
    arrange(round, team1_seed)
}
