# simulate_2026_women.R -- Monte Carlo bracket simulation for 2026 Women's tournament
# =============================================================================
setwd("C:/claudegit/marchmadness2026")
source("R/utils.R")
source("R/00_config.R")
library(dplyr); library(purrr); library(readr)

.load_prob_matrix_women <- function() {
  pm <- read_csv("data/processed/women/win_prob_matrix_2026.csv", show_col_types=FALSE)
  fwd <- setNames(pm$win_prob_ens, paste0(pm$team_a_name, "|||", pm$team_b_name))
  rev <- setNames(1 - pm$win_prob_ens, paste0(pm$team_b_name, "|||", pm$team_a_name))
  c(fwd, rev)
}

gp_w <- function(prob_lookup, a, b) {
  key <- paste0(a, "|||", b)
  p   <- prob_lookup[key]
  if (is.na(p)) 0.5 else unname(p)
}

# ---- Bracket definition ----
# Slot order within each region: 1,8,5,4,6,3,7,2 (seed bracket position)
# R1 matchups: slot[i] vs opp[i]

REGIONS_W <- c("Reg1_FortWorth", "Reg2_Sacramento", "Reg3_FortWorth", "Reg4_Sacramento")

BRACKET_SLOTS_W <- list(
  Reg1_FortWorth  = c("UConn Huskies","Iowa State Cyclones","Maryland Terrapins","North Carolina Tar Heels",
                       "Notre Dame Fighting Irish","Ohio State Buckeyes","Illinois Fighting Illini","Vanderbilt Commodores"),
  Reg2_Sacramento = c("UCLA Bruins","Oklahoma State Cowgirls","Ole Miss Rebels","Minnesota Golden Gophers",
                       "Baylor Bears","Duke Blue Devils","Texas Tech Lady Raiders","LSU Tigers"),
  Reg3_FortWorth  = c("Texas Longhorns","Oregon Ducks","Kentucky Wildcats","West Virginia Mountaineers",
                       "Alabama Crimson Tide","Louisville Cardinals","NC State Wolfpack","Michigan Wolverines"),
  Reg4_Sacramento = c("South Carolina Gamecocks","Clemson Tigers","Michigan State Spartans","Oklahoma Sooners",
                       "Washington Huskies","TCU Horned Frogs","Georgia Lady Bulldogs","Iowa Hawkeyes")
)

BRACKET_OPPS_W <- list(
  Reg1_FortWorth  = c("UTSA Roadrunners","Syracuse Orange","Murray State Racers","Western Illinois Leathernecks",
                       "Fairfield Stags","Howard Bison","Colorado Buffaloes","High Point Panthers"),
  Reg2_Sacramento = c("California Baptist Lancers","Princeton Tigers","Gonzaga Bulldogs","Green Bay Phoenix",
                       "FF_Nebraska_Richmond","Charleston Cougars","Villanova Wildcats","Jacksonville Dolphins"),
  Reg3_FortWorth  = c("FF_MissouriSt_SFA","Virginia Tech Hokies","James Madison Dukes","Miami (OH) RedHawks",
                       "Rhode Island Rams","Vermont Catamounts","Tennessee Lady Volunteers","Holy Cross Crusaders"),
  Reg4_Sacramento = c("FF_Southern_Samford","USC Trojans","Colorado State Rams","Idaho Vandals",
                       "South Dakota State Jackrabbits","UC San Diego Tritons","FF_Virginia_AzSt","Fairleigh Dickinson Knights")
)

FF_GAMES_W <- list(
  list(a="Nebraska Cornhuskers",      b="Richmond Spiders",          placeholder="FF_Nebraska_Richmond"),
  list(a="Missouri State Lady Bears", b="Stephen F. Austin Ladyjacks",placeholder="FF_MissouriSt_SFA"),
  list(a="Southern Jaguars",          b="Samford Bulldogs",           placeholder="FF_Southern_Samford"),
  list(a="Virginia Cavaliers",        b="Arizona State Sun Devils",   placeholder="FF_Virginia_AzSt")
)

# Final Four: same-site regions meet
# Fort Worth: Reg1 vs Reg3 | Sacramento: Reg2 vs Reg4
FF_PAIRS_W <- list(c("Reg1_FortWorth","Reg3_FortWorth"), c("Reg2_Sacramento","Reg4_Sacramento"))

# ---- Simulation ----
run_simulation_women <- function(N = 10000, seed = 2026) {
  set.seed(seed)
  prob_lookup <- .load_prob_matrix_women()

  message(sprintf("Running %s women's simulations...", format(N, big.mark=",")))
  t0 <- proc.time()

  all_teams <- c(
    unlist(BRACKET_SLOTS_W), unlist(BRACKET_OPPS_W),
    map_chr(FF_GAMES_W, "a"), map_chr(FF_GAMES_W, "b")
  ) |> grep("^FF_", x=_, invert=TRUE, value=TRUE) |> unique()

  adv <- matrix(0L, nrow=N, ncol=length(all_teams), dimnames=list(NULL, all_teams))
  champions <- character(N)

  for (sim in seq_len(N)) {
    # 1. First Four
    ff_resolved <- list()
    for (g in FF_GAMES_W) {
      p   <- gp_w(prob_lookup, g$a, g$b)
      win <- if (runif(1) < p) g$a else g$b
      ff_resolved[[g$placeholder]] <- win
    }

    # 2. Regions through Elite 8
    reg_champs <- character(length(REGIONS_W))
    names(reg_champs) <- REGIONS_W

    for (reg in REGIONS_W) {
      slots <- BRACKET_SLOTS_W[[reg]]
      opps  <- BRACKET_OPPS_W[[reg]]
      opps  <- map_chr(opps, function(o) if (startsWith(o,"FF_")) ff_resolved[[o]] else o)

      # R1
      r1 <- character(8)
      for (i in 1:8) {
        p <- gp_w(prob_lookup, slots[i], opps[i])
        r1[i] <- if (runif(1) < p) slots[i] else opps[i]
        adv[sim, r1[i]] <- adv[sim, r1[i]] + 1L
      }
      # R2
      r2 <- character(4)
      for (i in 1:4) {
        p <- gp_w(prob_lookup, r1[2*i-1], r1[2*i])
        r2[i] <- if (runif(1) < p) r1[2*i-1] else r1[2*i]
        adv[sim, r2[i]] <- adv[sim, r2[i]] + 1L
      }
      # Sweet 16
      s16 <- character(2)
      for (i in 1:2) {
        p <- gp_w(prob_lookup, r2[2*i-1], r2[2*i])
        s16[i] <- if (runif(1) < p) r2[2*i-1] else r2[2*i]
        adv[sim, s16[i]] <- adv[sim, s16[i]] + 1L
      }
      # Elite 8
      p <- gp_w(prob_lookup, s16[1], s16[2])
      e8w <- if (runif(1) < p) s16[1] else s16[2]
      adv[sim, e8w] <- adv[sim, e8w] + 1L
      reg_champs[reg] <- e8w
    }

    # 3. Final Four + Championship
    ff_w <- character(2)
    for (pi in seq_along(FF_PAIRS_W)) {
      pair <- FF_PAIRS_W[[pi]]
      a <- reg_champs[pair[1]]; b <- reg_champs[pair[2]]
      p <- gp_w(prob_lookup, a, b)
      ff_w[pi] <- if (runif(1) < p) a else b
      adv[sim, ff_w[pi]] <- adv[sim, ff_w[pi]] + 1L
    }
    p     <- gp_w(prob_lookup, ff_w[1], ff_w[2])
    champ <- if (runif(1) < p) ff_w[1] else ff_w[2]
    adv[sim, champ] <- adv[sim, champ] + 1L
    champions[sim]  <- champ
  }

  elapsed <- (proc.time()-t0)["elapsed"]
  message(sprintf("Done in %.1f seconds.", elapsed))

  round_labels <- c("R64","R32","Sweet16","Elite8","FinalFour","Champion")
  adv_probs <- colMeans(adv >= 1) |>
    as_tibble(rownames="team_name") |>
    rename(prob_r64=value)
  for (r in 2:6)
    adv_probs[[paste0("prob_", round_labels[r])]] <- colMeans(adv >= r)[adv_probs$team_name]

  champ_tbl <- table(champions) |> as_tibble() |>
    rename(team_name=champions, champ_prob=n) |>
    mutate(champ_prob = champ_prob / N) |>
    arrange(desc(champ_prob))

  dir.create("output/women", showWarnings=FALSE, recursive=TRUE)
  write_csv(adv_probs, "output/women/team_advancement_probs.csv")
  write_csv(champ_tbl, "output/women/champion_distribution.csv")

  cat("\n=== 2026 NCAA WOMEN'S CHAMPIONSHIP PROBABILITIES (Top 20) ===\n")
  champ_tbl |> filter(champ_prob > 0.001) |>
    mutate(prob=paste0(round(champ_prob*100,1),"%")) |>
    select(team_name, prob) |> print(n=20)

  invisible(list(advancement=adv_probs, champion=champ_tbl, raw=adv))
}
