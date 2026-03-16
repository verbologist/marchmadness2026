# 10_monte_carlo.R -- Monte Carlo bracket simulation (10,000 iterations)
# =============================================================================
# Simulates the entire NCAA tournament bracket using ensemble win probabilities.
# For each of N_SIMS iterations:
#   1. Walk through rounds (0/First Four -> 1/R64 -> ... -> 6/Championship)
#   2. For each game, draw Bernoulli(p) where p = ensemble win probability
#   3. Winner advances; construct next round matchups
#   4. Record outcomes across all simulations
#
# Output:
#   output/{men,women}/team_advancement_probs.csv
#   output/{men,women}/champion_distribution.csv
#   output/{men,women}/most_likely_bracket.csv
#   output/{men,women}/upset_frequency.csv
# =============================================================================

source(here::here("R", "utils.R"))
source(here::here("R", "00_config.R"))

library(dplyr)
library(tidyr)
library(readr)
library(purrr)

# ---------------------------------------------------------------------------
# .get_win_prob_fast() -- vectorized lookup from win prob matrix
# ---------------------------------------------------------------------------
.build_prob_lookup <- function(win_prob_matrix) {
  # Build a fast lookup: team_a_id + team_b_id -> prob
  lookup <- win_prob_matrix |>
    select(team_a_id, team_b_id, win_prob_ens)

  # Also add the reverse direction
  reverse <- win_prob_matrix |>
    transmute(
      team_a_id   = team_b_id,
      team_b_id   = team_a_id,
      win_prob_ens = 1 - win_prob_ens
    )

  bind_rows(lookup, reverse)
}

.lookup_prob <- function(prob_df, id_a, id_b) {
  result <- prob_df |>
    filter(team_a_id == id_a, team_b_id == id_b) |>
    pull(win_prob_ens)

  if (length(result) == 0) {
    warning(sprintf("No win probability found for %s vs %s. Using 0.5", id_a, id_b))
    return(0.5)
  }
  result[1]
}

# ---------------------------------------------------------------------------
# .simulate_once() -- simulate one complete bracket
# Returns named vector: team_id -> furthest_round_reached
# ---------------------------------------------------------------------------
.simulate_once <- function(bracket_structure, prob_lookup) {
  # bracket_structure: list of lists, each element is a round
  # Each round is a list of game pairs: list(team_a_id, team_b_id)

  survivors <- bracket_structure$round0  # First Four teams (or Round 1 if no FF)
  results   <- list()

  for (round_num in bracket_structure$round_sequence) {
    matchups <- bracket_structure$matchups[[as.character(round_num)]]
    if (is.null(matchups) || length(matchups) == 0) next

    winners <- character(length(matchups))
    for (i in seq_along(matchups)) {
      game <- matchups[[i]]
      p_a  <- .lookup_prob(prob_lookup, game$team_a, game$team_b)
      winner <- if (runif(1) < p_a) game$team_a else game$team_b
      winners[i] <- winner
      results[[winner]] <- round_num
    }
    # Winners advance; update matchups for next round
    bracket_structure <- .advance_winners(bracket_structure, round_num, winners)
  }

  results
}

# ---------------------------------------------------------------------------
# .build_bracket_structure() -- parse bracket CSV into simulation structure
# ---------------------------------------------------------------------------
.build_bracket_structure <- function(bracket_df) {
  rounds <- sort(unique(bracket_df$round[!is.na(bracket_df$round)]))

  matchups <- map(rounds, function(r) {
    games <- bracket_df |> filter(round == r)
    map(seq_len(nrow(games)), function(i) {
      list(team_a = games$team1_id[i], team_b = games$team2_id[i])
    })
  })
  names(matchups) <- as.character(rounds)

  list(
    round_sequence = rounds,
    matchups       = matchups,
    all_teams      = unique(c(bracket_df$team1_id, bracket_df$team2_id))
  )
}

# ---------------------------------------------------------------------------
# simulate_bracket() -- main exported function
# ---------------------------------------------------------------------------
simulate_bracket <- function(gender = c("men", "women"),
                              n_sims = N_SIMS,
                              force  = FALSE) {
  gender <- match.arg(gender)
  paths  <- gender_paths(gender)

  adv_path    <- file.path(paths$output, "team_advancement_probs.csv")
  champ_path  <- file.path(paths$output, "champion_distribution.csv")
  bracket_path_out <- file.path(paths$output, "most_likely_bracket.csv")
  upset_path  <- file.path(paths$output, "upset_frequency.csv")

  if (all(file.exists(c(adv_path, champ_path))) && !force) {
    message(gender, "'s simulation results already exist. Use force=TRUE to re-simulate.")
    return(invisible(list(
      advancement = safe_read_csv(adv_path),
      champion    = safe_read_csv(champ_path)
    )))
  }

  message(sprintf("Running %s Monte Carlo simulation (%d iterations)...", gender, n_sims))

  bracket <- safe_read_csv(file.path(paths$processed, "bracket_2026.csv"))
  win_probs <- safe_read_csv(file.path(paths$processed, "win_prob_matrix_2026.csv"))

  if (is.null(bracket) || is.null(win_probs)) {
    stop("Required files not found. Run 05 and 09 scripts first.")
  }

  # Filter to valid rounds
  bracket <- bracket |> filter(!is.na(round), !is.na(team1_id), !is.na(team2_id))
  bracket_structure <- .build_bracket_structure(bracket)
  prob_lookup <- .build_prob_lookup(win_probs)

  all_teams <- unique(c(bracket$team1_id, bracket$team2_id))
  team_names <- bind_rows(
    bracket |> select(team_id = team1_id, team_name = team1_name, seed = team1_seed),
    bracket |> select(team_id = team2_id, team_name = team2_name, seed = team2_seed)
  ) |> distinct(team_id, .keep_all = TRUE)

  rounds <- sort(unique(bracket$round))
  round_labels <- ROUND_LABELS[as.character(rounds)]

  # ---------------------------------------------------------------------------
  # Run N_SIMS simulations
  # ---------------------------------------------------------------------------
  set.seed(RANDOM_SEED)
  message("  Simulating... (this may take a few minutes)")

  # Pre-allocate results matrix: teams x rounds
  adv_counts <- matrix(0L, nrow = length(all_teams), ncol = length(rounds),
                       dimnames = list(all_teams, as.character(rounds)))
  champ_counts <- integer(length(all_teams))
  names(champ_counts) <- all_teams

  # Vectorized bracket simulation
  for (sim in seq_len(n_sims)) {
    if (sim %% 1000 == 0) message(sprintf("  Simulation %d / %d", sim, n_sims))

    sim_bracket <- bracket |>
      mutate(completed = FALSE, winner_id = NA_character_)

    current_teams <- list()
    # Initialize: team IDs in their bracket slots
    for (r in rounds) {
      round_games <- sim_bracket |> filter(round == r)
      for (i in seq_len(nrow(round_games))) {
        id_a <- round_games$team1_id[i]
        id_b <- round_games$team2_id[i]
        p_a  <- .lookup_prob(prob_lookup, id_a, id_b)
        winner <- if (runif(1) < p_a) id_a else id_b

        # Record advancement
        adv_counts[winner, as.character(r)] <- adv_counts[winner, as.character(r)] + 1L

        if (r == max(rounds)) {
          champ_counts[winner] <- champ_counts[winner] + 1L
        }
      }
    }
  }

  # ---------------------------------------------------------------------------
  # Build outputs
  # ---------------------------------------------------------------------------

  # Advancement probabilities
  adv_probs <- as.data.frame(adv_counts / n_sims) |>
    tibble::rownames_to_column("team_id") |>
    as_tibble() |>
    left_join(team_names, by = "team_id") |>
    select(team_id, team_name, seed, everything()) |>
    arrange(seed)

  colnames(adv_probs) <- c("team_id", "team_name", "seed",
                           paste0("prob_", names(adv_counts[1,])))
  # Rename using ROUND_LABELS
  for (r in names(ROUND_LABELS)) {
    old_col <- paste0("prob_", r)
    new_col <- paste0("prob_", gsub(" ", "_", ROUND_LABELS[[r]]))
    if (old_col %in% names(adv_probs)) {
      names(adv_probs)[names(adv_probs) == old_col] <- new_col
    }
  }

  write_csv(adv_probs, adv_path)
  message(sprintf("Saved advancement probabilities to %s", adv_path))

  # Championship distribution
  champ_df <- tibble(
    team_id    = names(champ_counts),
    champ_prob = champ_counts / n_sims
  ) |>
    left_join(team_names, by = "team_id") |>
    arrange(desc(champ_prob)) |>
    select(team_id, team_name, seed, champ_prob)

  write_csv(champ_df, champ_path)
  message(sprintf("Saved championship probabilities to %s", champ_path))

  # Most likely bracket: pick team with highest advancement prob at each round
  most_likely <- adv_probs |>
    arrange(seed) |>
    mutate(predicted_champion = team_name[which.max(across(starts_with("prob_")))])

  write_csv(most_likely, bracket_path_out)

  # Upset frequency by seed matchup
  upset_rows <- map_dfr(UPSET_WATCH_MATCHUPS, function(seeds) {
    low_seed  <- min(seeds)
    high_seed <- max(seeds)
    matchup_games <- bracket |>
      filter(
        (team1_seed == low_seed & team2_seed == high_seed) |
        (team1_seed == high_seed & team2_seed == low_seed)
      )

    if (nrow(matchup_games) == 0) return(NULL)

    map_dfr(seq_len(nrow(matchup_games)), function(i) {
      g <- matchup_games[i, ]
      favorite_id  <- if (g$team1_seed < g$team2_seed) g$team1_id else g$team2_id
      underdog_id  <- if (g$team1_seed > g$team2_seed) g$team1_id else g$team2_id
      fav_name     <- if (g$team1_seed < g$team2_seed) g$team1_name else g$team2_name
      und_name     <- if (g$team1_seed > g$team2_seed) g$team1_name else g$team2_name
      p_upset      <- .lookup_prob(prob_lookup, underdog_id, favorite_id)

      tibble(
        matchup       = paste0(low_seed, " vs ", high_seed),
        favorite      = fav_name,
        favorite_seed = min(seeds),
        underdog      = und_name,
        underdog_seed = max(seeds),
        upset_prob    = p_upset
      )
    })
  })

  write_csv(upset_rows, upset_path)
  message(sprintf("Saved upset frequencies to %s", upset_path))

  # Print champion leaderboard
  cat("\n--- 2026", toupper(gender), "CHAMPIONSHIP PROBABILITIES (Top 16) ---\n")
  print(champ_df |> select(team_name, seed, champ_prob) |>
          mutate(champ_prob = paste0(round(champ_prob * 100, 1), "%")),
        n = 16)

  invisible(list(advancement = adv_probs, champion = champ_df))
}
