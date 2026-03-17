# simulate_2026.R -- Fast vectorized Monte Carlo bracket simulation
# Replaces 10_monte_carlo.R for the 2026 tournament.
# Avoids per-simulation R loops by pre-computing a prob matrix and
# using vectorized Bernoulli draws across all N simulations at once.
#
# Run: source("R/simulate_2026.R") then run_simulation()
# =============================================================================

setwd("C:/claudegit/marchmadness2026")
source("R/utils.R")
source("R/00_config.R")
library(dplyr); library(purrr); library(readr); library(tidyr)

# ---- Win probability lookup (vectorized) ----
.load_prob_matrix <- function(injury_adj = TRUE) {
  adj_path  <- "data/processed/men/win_prob_matrix_2026_injury_adj.csv"
  base_path <- "data/processed/men/win_prob_matrix_2026.csv"
  path <- if (injury_adj && file.exists(adj_path)) adj_path else base_path
  message("  Loading: ", basename(path))
  pm <- read_csv(path, show_col_types=FALSE)
  # Build named lookup: "A|||B" -> prob(A beats B)
  fwd <- setNames(pm$win_prob_ens, paste0(pm$team_a_name, "|||", pm$team_b_name))
  rev <- setNames(1 - pm$win_prob_ens, paste0(pm$team_b_name, "|||", pm$team_a_name))
  c(fwd, rev)
}

gp <- function(prob_lookup, a, b) {
  # P(a beats b); default 0.5 if not found
  key <- paste0(a, "|||", b)
  p   <- prob_lookup[key]
  if (is.na(p)) 0.5 else unname(p)
}

# ---- Bracket definition ----
# Each region: 8 slots (teams) in standard seed order
# Slot indices within region: 1=seed1, 2=seed8, 3=seed5, 4=seed4,
#                              5=seed6, 6=seed3, 7=seed7, 8=seed2
# R1 matchups: 1v8 (seed1vSeed16), 2v7(8v9), 3v6(5v12), 4v5(4v13) etc.
# Standard R1 pairs by slot: (1,16) (8,9) (5,12) (4,13) (6,11) (3,14) (7,10) (2,15)
# By slot index: slot 1 vs slot 8, slot 2 vs slot 7, slot 3 vs slot 6, slot 4 vs slot 5
# R2: winner of (1v8) vs winner of (2v7), winner of (3v6) vs winner of (4v5)
# S16: winner of (1v8v2v7) vs winner of (3v6v4v5)
# E8: the two S16 winners

REGIONS <- c("East", "South", "Midwest", "West")

# Teams in each region, ordered by slot (see bracket structure above)
# Slot order: 1,8,5,4,6,3,7,2 (seed order within bracket half)
BRACKET_SLOTS <- list(
  East = c("Duke Blue Devils","Ohio State Buckeyes","St. John's Red Storm","Kansas Jayhawks",
           "Louisville Cardinals","Michigan State Spartans","UCLA Bruins","UConn Huskies"),
  South= c("Florida Gators","Clemson Tigers","Vanderbilt Commodores","Nebraska Cornhuskers",
           "North Carolina Tar Heels","Illinois Fighting Illini","Saint Mary's Gaels","Houston Cougars"),
  Midwest=c("Michigan Wolverines","Georgia Bulldogs","Texas Tech Red Raiders","Alabama Crimson Tide",
            "Tennessee Volunteers","Virginia Cavaliers","Kentucky Wildcats","Iowa State Cyclones"),
  West = c("Arizona Wildcats","Villanova Wildcats","Wisconsin Badgers","Arkansas Razorbacks",
           "BYU Cougars","Gonzaga Bulldogs","Miami Hurricanes","Purdue Boilermakers")
)

# Low-seed opponents for each slot (R1 opponents)
BRACKET_OPPS <- list(
  East   = c("Siena Saints","TCU Horned Frogs","Northern Iowa Panthers","California Baptist Lancers",
             "South Florida Bulls","North Dakota State Bison","UCF Knights","Furman Paladins"),
  South  = c("FF_Lehigh_PV","Iowa Hawkeyes","McNeese Cowboys","Troy Trojans",
             "VCU Rams","Pennsylvania Quakers","Texas A&M Aggies","Idaho Vandals"),
  Midwest= c("FF_Howard_UMBC","Saint Louis Billikens","Akron Zips","Hofstra Pride",
             "FF_NCState_Texas","Wright State Raiders","Santa Clara Broncos","Tennessee State Tigers"),
  West   = c("Long Island University Sharks","Utah State Aggies","High Point Panthers","Hawai'i Rainbow Warriors",
             "FF_SMU_MiamiOH","Kennesaw State Owls","Missouri Tigers","Queens University Royals")
)

# First Four: a vs b -> winner replaces FF_* placeholder
FF_GAMES <- list(
  list(a="NC State Wolfpack",       b="Texas Longhorns",          placeholder="FF_NCState_Texas"),
  list(a="SMU Mustangs",            b="Miami (OH) RedHawks",       placeholder="FF_SMU_MiamiOH"),
  list(a="Howard Bison",            b="UMBC Retrievers",           placeholder="FF_Howard_UMBC"),
  list(a="Lehigh Mountain Hawks",   b="Prairie View A&M Panthers", placeholder="FF_Lehigh_PV")
)

# Final Four pairings (region winner matchups): East vs West, South vs Midwest
FF_PAIRS <- list(c("East","West"), c("South","Midwest"))

# ---- Vectorized simulation across N sims ----
run_simulation <- function(N = 10000, seed = 2026, injury_adj = TRUE) {
  set.seed(seed)
  prob_lookup <- .load_prob_matrix(injury_adj = injury_adj)

  message(sprintf("Running %s simulations...", format(N, big.mark=",")))
  t0 <- proc.time()

  # Storage: for each team, count how many sims they reach each round
  all_teams <- c(
    unlist(BRACKET_SLOTS), unlist(BRACKET_OPPS),
    map_chr(FF_GAMES, "a"), map_chr(FF_GAMES, "b")
  ) |> grep("^FF_", x=_, invert=TRUE, value=TRUE) |> unique()

  adv <- matrix(0L, nrow=N, ncol=length(all_teams), dimnames=list(NULL, all_teams))
  champions <- character(N)

  for (sim in seq_len(N)) {
    # 1. Resolve First Four
    ff_resolved <- list()
    for (g in FF_GAMES) {
      p   <- gp(prob_lookup, g$a, g$b)
      win <- if (runif(1) < p) g$a else g$b
      ff_resolved[[g$placeholder]] <- win
    }

    # 2. Simulate each region through Elite 8
    reg_champs <- character(length(REGIONS))
    names(reg_champs) <- REGIONS

    for (reg in REGIONS) {
      slots <- BRACKET_SLOTS[[reg]]
      opps  <- BRACKET_OPPS[[reg]]

      # Resolve FF placeholders
      opps <- map_chr(opps, function(o) {
        if (startsWith(o, "FF_")) ff_resolved[[o]] else o
      })

      # R1: 8 games, slots vs opps
      r1_winners <- character(8)
      for (i in 1:8) {
        p <- gp(prob_lookup, slots[i], opps[i])
        r1_winners[i] <- if (runif(1) < p) slots[i] else opps[i]
        adv[sim, r1_winners[i]] <- adv[sim, r1_winners[i]] + 1L  # round 1
      }

      # R2: 4 games — pair by: (1v2, 3v4, 5v6, 7v8) among R1 winners
      r2_winners <- character(4)
      for (i in 1:4) {
        a <- r1_winners[2*i-1]; b <- r1_winners[2*i]
        p <- gp(prob_lookup, a, b)
        r2_winners[i] <- if (runif(1) < p) a else b
        adv[sim, r2_winners[i]] <- adv[sim, r2_winners[i]] + 1L  # round 2
      }

      # S16: 2 games — (1v2, 3v4) among R2 winners
      s16_winners <- character(2)
      for (i in 1:2) {
        a <- r2_winners[2*i-1]; b <- r2_winners[2*i]
        p <- gp(prob_lookup, a, b)
        s16_winners[i] <- if (runif(1) < p) a else b
        adv[sim, s16_winners[i]] <- adv[sim, s16_winners[i]] + 1L  # round 3
      }

      # E8: 1 game
      p <- gp(prob_lookup, s16_winners[1], s16_winners[2])
      elite8_w <- if (runif(1) < p) s16_winners[1] else s16_winners[2]
      adv[sim, elite8_w] <- adv[sim, elite8_w] + 1L  # round 4
      reg_champs[reg] <- elite8_w
    }

    # 3. Final Four + Championship
    ff_winners_final <- character(2)
    for (pair_i in seq_along(FF_PAIRS)) {
      pair <- FF_PAIRS[[pair_i]]
      a    <- reg_champs[pair[1]]; b <- reg_champs[pair[2]]
      p    <- gp(prob_lookup, a, b)
      ff_winners_final[pair_i] <- if (runif(1) < p) a else b
      adv[sim, ff_winners_final[pair_i]] <- adv[sim, ff_winners_final[pair_i]] + 1L
    }

    # Championship
    p    <- gp(prob_lookup, ff_winners_final[1], ff_winners_final[2])
    champ <- if (runif(1) < p) ff_winners_final[1] else ff_winners_final[2]
    adv[sim, champ] <- adv[sim, champ] + 1L
    champions[sim]  <- champ
  }

  elapsed <- (proc.time()-t0)["elapsed"]
  message(sprintf("Done in %.1f seconds.", elapsed))

  # ---- Summarise ----
  round_labels <- c("R64","R32","Sweet16","Elite8","FinalFour","Champion")

  adv_probs <- colMeans(adv >= 1) |>
    as_tibble(rownames="team_name") |>
    rename(prob_r64=value)

  for (r in 2:6) {
    adv_probs[[paste0("prob_", round_labels[r])]] <- colMeans(adv >= r)[adv_probs$team_name]
  }

  champ_tbl <- table(champions) |> as_tibble() |>
    rename(team_name=champions, champ_prob=n) |>
    mutate(champ_prob = champ_prob / N) |>
    arrange(desc(champ_prob))

  # Save outputs (with and without injury suffix)
  suffix <- if (injury_adj) "_injury_adj" else ""
  dir.create("output/men", showWarnings=FALSE, recursive=TRUE)
  write_csv(adv_probs, paste0("output/men/team_advancement_probs", suffix, ".csv"))
  write_csv(champ_tbl, paste0("output/men/champion_distribution", suffix, ".csv"))
  # Always write the plain version too (used by Shiny as default)
  write_csv(adv_probs, "output/men/team_advancement_probs.csv")
  write_csv(champ_tbl, "output/men/champion_distribution.csv")

  cat("\n=== 2026 NCAA MEN'S CHAMPIONSHIP PROBABILITIES (Top 20) ===\n")
  champ_tbl |> filter(champ_prob > 0.001) |>
    mutate(prob=paste0(round(champ_prob*100,1),"%")) |>
    select(team_name, prob) |> print(n=20)

  invisible(list(advancement=adv_probs, champion=champ_tbl, raw=adv))
}
