# 13_bracket_plots.R -- Visual bracket predictions (top 2 brackets per gender)
# =============================================================================
setwd("C:/claudegit/marchmadness2026")
source("R/utils.R"); source("R/00_config.R")
source("R/simulate_2026.R")         # men's bracket constants + gp()
source("R/simulate_2026_women.R")   # women's bracket constants + gp_w()
library(dplyr); library(ggplot2); library(readr); library(stringr); library(purrr)

# ---- Name shortener ----
shorten <- function(x, w=16) {
  x |>
    str_replace(" Fighting Illini$","") |>
    str_replace(" Cornhuskers$","") |>
    str_replace(" Gamecocks$","") |>
    str_replace(" Crimson Tide$","") |>
    str_replace(" Commodores$","") |>
    str_replace(" Razorbacks$","") |>
    str_replace(" Boilermakers$","") |>
    str_replace(" Wolverines$","") |>
    str_replace(" Bulldogs$","") |>
    str_replace(" Cardinals$","") |>
    str_replace(" Wildcats$","") |>
    str_replace(" Cavaliers$","") |>
    str_replace(" Huskies$","") |>
    str_replace(" Jayhawks$","") |>
    str_replace(" Spartans$","") |>
    str_replace(" Volunteers$","") |>
    str_replace(" Cyclones$","") |>
    str_replace(" Cougars$","") |>
    str_replace(" Longhorns$","") |>
    str_replace(" Gators$","") |>
    str_replace(" Tigers$","") |>
    str_replace(" Bruins$","") |>
    str_replace(" Blue Devils$","") |>
    str_replace(" Buckeyes$","") |>
    str_replace(" Hawkeyes$","") |>
    str_replace(" Badgers$","") |>
    str_replace(" Bears$","") |>
    str_replace(" Ducks$","") |>
    str_replace(" Rebels$","") |>
    str_replace(" Terrapins$","") |>
    str_replace(" Mountaineers$","") |>
    str_replace(" Red Raiders$","") |>
    str_replace(" Lady Raiders$","") |>
    str_replace(" Tar Heels$","") |>
    str_replace(" Fighting Irish$","") |>
    str_replace(" Horned Frogs$","") |>
    str_replace(" Sooners$","") |>
    str_replace(" Cowgirls$","") |>
    str_replace(" Lady Bulldogs$","") |>
    str_replace(" Wolfpack$","") |>
    str_trunc(w, "right", "")
}

# ---- Greedy bracket simulator ----
# Returns list: regions (r1, r2, s16, e8, opps), reg_champs, ff_winners, champion
simulate_greedy <- function(prob_lkp, slots, opps, ff_games, ff_pairs,
                             upset_overrides = character(0)) {
  # upset_overrides: named char vector "TeamA|||TeamB" = "forced_winner"
  pick <- function(a, b) {
    key_fwd <- paste0(a,"|||",b); key_rev <- paste0(b,"|||",a)
    if (key_fwd %in% names(upset_overrides)) return(upset_overrides[key_fwd])
    if (key_rev %in% names(upset_overrides)) return(upset_overrides[key_rev])
    p <- prob_lkp[key_fwd]; if (is.na(p)) p <- 0.5
    if (unname(p) >= 0.5) a else b
  }

  ff_res <- list()
  for (g in ff_games) { ff_res[[g$placeholder]] <- pick(g$a, g$b) }

  regions <- names(slots)
  reg_champs <- setNames(character(length(regions)), regions)
  reg_results <- list()

  for (reg in regions) {
    sl <- slots[[reg]]
    op <- map_chr(opps[[reg]], function(o) if (startsWith(o,"FF_")) ff_res[[o]] else o)
    r1  <- map_chr(1:8, function(i) pick(sl[i], op[i]))
    r2  <- map_chr(1:4, function(i) pick(r1[2*i-1], r1[2*i]))
    s16 <- map_chr(1:2, function(i) pick(r2[2*i-1], r2[2*i]))
    e8  <- pick(s16[1], s16[2])
    reg_champs[reg] <- e8
    reg_results[[reg]] <- list(sl=sl, op=op, r1=r1, r2=r2, s16=s16, e8=e8)
  }

  ff_w <- map_chr(seq_along(ff_pairs), function(pi) {
    pair <- ff_pairs[[pi]]; pick(reg_champs[pair[1]], reg_champs[pair[2]])
  })
  champ <- pick(ff_w[1], ff_w[2])

  list(reg=reg_results, reg_champs=reg_champs, ff_w=ff_w, champ=champ, ff_res=ff_res)
}

# ---- Build data frame for plotting ----
# Coordinate scheme (per region, 16 y-slots):
#   R64:  teams at y = 0,1,2,...,15  (game i: y=2i-2 top, 2i-1 bottom)
#   R32:  winner of game i at y = 2i - 1.5  (midpoint of the two R64 teams)
#   S16:  winner of R32 game i at y midpoint of R32 pair
#   E8:   winner at y midpoint of S16 pair
#
# y midpoints per region (0-based):
#   R64 game i center: y = 2i - 1.5
#   R32 game i center: 4i - 3     (midpoint of R64 games 2i-1 and 2i)
#   S16 game i center: 8i - 5.5  (midpoint of R32 games 2i-1 and 2i)
#   E8 center: 7.5                (midpoint of S16 games 1 and 2)
#
# Region y-offsets: top=0, bottom=17 (gap of 1)
# Left side x: R64=0, R32=1.5, S16=3, E8=4.5
# Right side x: R64=12, R32=10.5, S16=9, E8=7.5
# FF at x=5.5 (left teams) and x=6.5 (right teams); winner at x=6
# Champ: FF winners at x=6, y=7.5 and y=24.5; champ at x=6, y=16 (midpoint label only)

build_bracket_df <- function(br, slots, ff_pairs, layout) {
  # layout: named list, region -> list(side="left"/"right", half="top"/"bottom")
  rows <- list()

  r64_y  <- function(i) c(2*i-2, 2*i-1)   # returns c(top_y, bot_y)
  r32_y  <- function(i) 4*i - 3            # center of R32 game i
  s16_y  <- function(i) 8*i - 5.5          # center of S16 game i  (4.5*1 - ... hmm)
  # Let me compute these properly:
  # R32 game i: takes R64 games 2i-1 and 2i
  #   R64 game 2i-1 center: 2*(2i-1) - 1.5 = 4i - 3.5
  #   R64 game 2i   center: 2*(2i)   - 1.5 = 4i - 1.5
  #   R32 game i center: (4i-3.5 + 4i-1.5)/2 = 4i - 2.5
  r32_y  <- function(i) 4*i - 2.5
  # S16 game i: takes R32 games 2i-1 and 2i
  #   R32 game 2i-1 center: 4*(2i-1) - 2.5 = 8i - 6.5
  #   R32 game 2i   center: 4*(2i)   - 2.5 = 8i - 2.5
  #   S16 game i center: (8i-6.5 + 8i-2.5)/2 = 8i - 4.5
  s16_y  <- function(i) 8*i - 4.5
  # S16 game 1: 3.5, S16 game 2: 11.5
  # E8: (3.5 + 11.5)/2 = 7.5
  e8_y   <- 7.5

  x_left  <- c(R64=0, R32=1.5, S16=3, E8=4.5)
  x_right <- c(R64=12, R32=10.5, S16=9, E8=7.5)

  for (reg in names(layout)) {
    ly   <- layout[[reg]]
    yoff <- if (ly$half == "top") 0 else 17
    xmap <- if (ly$side == "left") x_left else x_right
    res  <- br$reg[[reg]]
    sl   <- res$sl; op <- res$op

    # R64
    for (i in 1:8) {
      for (j in 1:2) {
        team   <- if (j==1) sl[i] else op[i]
        winner <- res$r1[i]
        gy     <- r64_y(i)[j] + yoff
        rows[[length(rows)+1]] <- tibble(
          round="R64", region=reg, x=xmap["R64"], y=gy,
          team=team, short=shorten(team), is_winner=(team==winner), is_champ=FALSE)
      }
    }
    # R32 participants (R1 winners)
    for (i in 1:4) {
      for (j in 1:2) {
        team   <- res$r1[2*i-2+j]
        winner <- res$r2[i]
        gy     <- r64_y(2*i-2+j)[1] + 0.5 + yoff  # midpoint of R64 game
        rows[[length(rows)+1]] <- tibble(
          round="R32", region=reg, x=xmap["R32"], y=gy,
          team=team, short=shorten(team), is_winner=(team==winner), is_champ=FALSE)
      }
    }
    # S16 participants (R32 winners)
    for (i in 1:2) {
      for (j in 1:2) {
        team   <- res$r2[2*i-2+j]
        winner <- res$s16[i]
        gy     <- r32_y(2*i-2+j) + yoff
        rows[[length(rows)+1]] <- tibble(
          round="S16", region=reg, x=xmap["S16"], y=gy,
          team=team, short=shorten(team), is_winner=(team==winner), is_champ=FALSE)
      }
    }
    # E8 participants (S16 winners)
    for (i in 1:2) {
      team   <- res$s16[i]
      winner <- res$e8
      gy     <- s16_y(i) + yoff
      rows[[length(rows)+1]] <- tibble(
        round="E8", region=reg, x=xmap["E8"], y=gy,
        team=team, short=shorten(team), is_winner=(team==winner), is_champ=FALSE)
    }
    # Regional champ
    gy <- e8_y + yoff
    rows[[length(rows)+1]] <- tibble(
      round="RegChamp", region=reg, x=xmap["E8"]+if(ly$side=="left") 0.1 else -0.1, y=gy,
      team=res$e8, short=shorten(res$e8), is_winner=TRUE, is_champ=FALSE)
  }

  # Final Four
  for (pi in seq_along(ff_pairs)) {
    pair <- ff_pairs[[pi]]
    reg_a <- pair[1]; reg_b <- pair[2]
    yoff_a <- if (layout[[reg_a]]$half == "top") 0 else 17
    yoff_b <- if (layout[[reg_b]]$half == "top") 0 else 17
    ya <- e8_y + yoff_a; yb <- e8_y + yoff_b
    champ_a <- br$reg_champs[reg_a]; champ_b <- br$reg_champs[reg_b]
    ff_winner <- br$ff_w[pi]
    yff <- (ya + yb)/2

    rows[[length(rows)+1]] <- tibble(
      round="FF", region="FF", x=5.5, y=ya,
      team=champ_a, short=shorten(champ_a), is_winner=(ff_winner==champ_a), is_champ=FALSE)
    rows[[length(rows)+1]] <- tibble(
      round="FF", region="FF", x=6.5, y=yb,
      team=champ_b, short=shorten(champ_b), is_winner=(ff_winner==champ_b), is_champ=FALSE)
    rows[[length(rows)+1]] <- tibble(
      round="FFWinner", region="FF", x=6, y=yff,
      team=ff_winner, short=shorten(ff_winner), is_winner=TRUE, is_champ=FALSE)
  }

  # Championship
  y1 <- e8_y + 0; y2 <- e8_y + 17; yc <- (y1+y2)/2
  rows[[length(rows)+1]] <- tibble(
    round="ChampTeam1", region="Champ", x=5.5, y=y1,
    team=br$ff_w[1], short=shorten(br$ff_w[1]), is_winner=(br$champ==br$ff_w[1]), is_champ=(br$champ==br$ff_w[1]))
  rows[[length(rows)+1]] <- tibble(
    round="ChampTeam2", region="Champ", x=6.5, y=y2,
    team=br$ff_w[2], short=shorten(br$ff_w[2]), is_winner=(br$champ==br$ff_w[2]), is_champ=(br$champ==br$ff_w[2]))
  rows[[length(rows)+1]] <- tibble(
    round="Champion", region="Champ", x=6, y=yc,
    team=br$champ, short=shorten(br$champ), is_winner=TRUE, is_champ=TRUE)

  bind_rows(rows)
}

# ---- Build bracket segment lines ----
build_bracket_segments <- function(df) {
  segs <- list()
  # For each game: find the two teams, draw connecting bracket lines
  rounds_seq <- c("R64"="R32", "R32"="S16", "S16"="E8")

  for (r in names(rounds_seq)) {
    next_r <- rounds_seq[r]
    teams_r <- df |> filter(round == r)

    for (reg in unique(teams_r$region)) {
      sub <- teams_r |> filter(region == reg) |> arrange(y)
      x_cur  <- sub$x[1]
      x_next <- df |> filter(round == next_r, region == reg) |> pull(x) |> unique()
      if (length(x_next) == 0) next
      x_next <- x_next[1]
      x_mid  <- (x_cur + x_next) / 2

      n <- nrow(sub)
      for (i in seq(1, n, by=2)) {
        if (i+1 > n) break
        ya <- sub$y[i]; yb <- sub$y[i+1]
        ywin <- (ya+yb)/2
        is_left <- x_next > x_cur
        x_stub  <- if (is_left) x_cur + 0.1 else x_cur - 0.1

        segs[[length(segs)+1]] <- tibble(x=x_stub,    xend=x_mid, y=ya,   yend=ya)
        segs[[length(segs)+1]] <- tibble(x=x_stub,    xend=x_mid, y=yb,   yend=yb)
        segs[[length(segs)+1]] <- tibble(x=x_mid,     xend=x_mid, y=ya,   yend=yb)
        segs[[length(segs)+1]] <- tibble(x=x_mid,     xend=x_next-if(is_left)0.05 else -0.05, y=ywin, yend=ywin)
      }
    }
  }

  # FF lines: E8 winners → FF center
  e8_left  <- df |> filter(round=="E8", region %in% names(REGIONS_LEFT))  |> arrange(region, y)
  e8_right <- df |> filter(round=="E8", region %in% names(REGIONS_RIGHT)) |> arrange(region, y)

  bind_rows(segs)
}

# ---- Main plot function (B&W + probability bars) ----
# adv_probs: data frame with columns team_name + prob_r64/prob_R32/prob_Sweet16/
#            prob_Elite8/prob_FinalFour/prob_Champion (from team_advancement_probs.csv)
plot_bracket <- function(df, title, subtitle, adv_probs = NULL) {

  # Map bracket round label -> advancement prob column
  round_to_prob <- c(
    R64       = "prob_r64",
    R32       = "prob_R32",
    S16       = "prob_Sweet16",
    E8        = "prob_Elite8",
    RegChamp  = "prob_Elite8",
    FF        = "prob_FinalFour",
    FFWinner  = "prob_FinalFour",
    ChampTeam1= "prob_Champion",
    ChampTeam2= "prob_Champion",
    Champion  = "prob_Champion"
  )

  # Join advancement probability
  if (!is.null(adv_probs)) {
    prob_long <- adv_probs |>
      select(team_name, prob_r64, prob_R32, prob_Sweet16,
             prob_Elite8, prob_FinalFour, prob_Champion) |>
      tidyr::pivot_longer(-team_name, names_to = "prob_col", values_to = "win_prob")
    df <- df |>
      mutate(prob_col = round_to_prob[round]) |>
      left_join(prob_long, by = c("team" = "team_name", "prob_col" = "prob_col")) |>
      mutate(win_prob = replace_na(win_prob, 0))
  } else {
    df <- df |> mutate(win_prob = 0)
  }

  # Bar direction: left-side teams bar extends right; right-side teams bar extends left
  BAR_MAX <- 0.55   # max bar width in plot units (= 100% prob)
  df <- df |> mutate(
    is_left  = x <= 6,
    bar_x0   = if_else(is_left, x + 0.12, x - 0.12 - win_prob * BAR_MAX),
    bar_x1   = if_else(is_left, x + 0.12 + win_prob * BAR_MAX, x - 0.12),
    bar_col  = case_when(
      is_champ  ~ "#1a3a6b",
      is_winner ~ "#444444",
      TRUE      ~ "#cccccc"
    ),
    text_col = case_when(
      is_champ  ~ "#1a3a6b",
      is_winner ~ "black",
      TRUE      ~ "#888888"
    ),
    text_face = if_else(is_winner, "bold", "plain"),
    text_size = case_when(
      round %in% c("Champion") ~ 2.8,
      is_winner ~ 2.3,
      TRUE ~ 2.0
    ),
    text_hjust = if_else(is_left, 0, 1),
    text_x     = if_else(is_left, x - 0.62, x + 0.62)
  )

  # Slot underlines (thin gray line at each team position)
  slot_df <- df |> filter(!round %in% c("Champion", "FFWinner", "RegChamp",
                                          "ChampTeam1", "ChampTeam2"))
  line_half <- 0.55
  slot_segs <- slot_df |> mutate(
    x0 = if_else(is_left, x - line_half, x - line_half),
    x1 = if_else(is_left, x + line_half, x + line_half)
  )

  ggplot(df) +
    # Slot underlines
    geom_segment(data = slot_segs,
                 aes(x = x0, xend = x1, y = y - 0.42, yend = y - 0.42),
                 color = "#dddddd", linewidth = 0.3) +
    # Probability bars
    geom_rect(data = filter(df, win_prob > 0.01),
              aes(xmin = bar_x0, xmax = bar_x1, ymin = y - 0.28, ymax = y + 0.28,
                  fill = bar_col), alpha = 0.85) +
    scale_fill_identity() +
    # Team name text
    geom_text(aes(x = text_x, y = y, label = short,
                  color = text_col, hjust = text_hjust, size = text_size,
                  fontface = text_face)) +
    scale_color_identity() +
    scale_size_identity() +
    # Round labels at top
    annotate("text",
             x = c(0.25, 1.5, 3, 4.5, 5.5, 6, 6.5, 7.5, 9, 10.5, 12),
             y = rep(33.5, 11), size = 2.5, fontface = "bold", color = "#555555",
             label = c("R64","R32","S16","E8","FF","CHAMP","FF","E8","S16","R32","R64")) +
    # Separator between top and bottom halves
    geom_hline(yintercept = 16, linetype = "dotted", color = "#bbbbbb", linewidth = 0.4) +
    xlim(-0.5, 12.8) + ylim(-1, 34) +
    labs(title = title, subtitle = subtitle) +
    theme_void() +
    theme(
      plot.title      = element_text(size = 14, face = "bold", hjust = 0.5, margin = margin(b = 4)),
      plot.subtitle   = element_text(size = 10, hjust = 0.5, color = "#555555", margin = margin(b = 8)),
      plot.margin     = margin(12, 12, 12, 12),
      plot.background = element_rect(fill = "white", color = NA)
    )
}

# =============================================================================
# MEN'S BRACKETS
# =============================================================================
message("Building men's bracket plots...")
prob_m    <- .load_prob_matrix()
adv_m     <- tryCatch(readr::read_csv("output/men/team_advancement_probs_injury_adj.csv",
                                       show_col_types=FALSE),
                      error = function(e) readr::read_csv("output/men/team_advancement_probs.csv",
                                                           show_col_types=FALSE))

men_layout <- list(
  East    = list(side="left",  half="top"),
  Midwest = list(side="left",  half="bottom"),
  South   = list(side="right", half="top"),
  West    = list(side="right", half="bottom")
)

# Bracket 1: Pure greedy (all favorites)
br1_m <- simulate_greedy(prob_m, BRACKET_SLOTS, BRACKET_OPPS, FF_GAMES, FF_PAIRS)
df1_m <- build_bracket_df(br1_m, BRACKET_SLOTS, FF_PAIRS, men_layout)

p1_m <- plot_bracket(df1_m,
  title     = "2026 Men's NCAA Tournament — Predicted Bracket #1 (All Favorites)",
  subtitle  = paste0("Champion: ", shorten(br1_m$champ, 30),
                     " | Final Four: ", paste(shorten(br1_m$ff_w, 20), collapse=" vs ")),
  adv_probs = adv_m)

ggsave("output/men/predicted_bracket_1.png", p1_m, width=18, height=11, dpi=150)
message("Saved predicted_bracket_1.png (men)")

# Bracket 2: Force top 5 most likely R64 upsets
all_r64_m <- purrr::map_dfr(names(BRACKET_SLOTS), function(reg) {
  sl <- BRACKET_SLOTS[[reg]]; op <- BRACKET_OPPS[[reg]]
  purrr::map_dfr(1:8, function(i) {
    if (startsWith(op[i], "FF_")) return(NULL)
    p <- gp(prob_m, sl[i], op[i])
    tibble(fav=sl[i], dog=op[i], p_fav=p, p_dog=1-p, upset_prob=1-p)
  })
}) |> filter(!is.na(upset_prob)) |> arrange(desc(upset_prob))

cat("Top R64 upset candidates (men):\n")
print(select(all_r64_m, fav, dog, p_dog=upset_prob) |> head(8))

top5_upsets_m <- head(all_r64_m, 5)
overrides_m <- setNames(top5_upsets_m$dog,
                         paste0(top5_upsets_m$fav, "|||", top5_upsets_m$dog))

br2_m <- simulate_greedy(prob_m, BRACKET_SLOTS, BRACKET_OPPS, FF_GAMES, FF_PAIRS,
                          upset_overrides = overrides_m)
df2_m <- build_bracket_df(br2_m, BRACKET_SLOTS, FF_PAIRS, men_layout)

p2_m <- plot_bracket(df2_m,
  title     = "2026 Men's NCAA Tournament — Predicted Bracket #2 (Top 5 Upsets Included)",
  subtitle  = paste0("Champion: ", shorten(br2_m$champ, 30),
                     " | Final Four: ", paste(shorten(br2_m$ff_w, 20), collapse=" vs ")),
  adv_probs = adv_m)

ggsave("output/men/predicted_bracket_2.png", p2_m, width=18, height=11, dpi=150)
message("Saved predicted_bracket_2.png (men)")

# =============================================================================
# WOMEN'S BRACKETS
# =============================================================================
message("Building women's bracket plots...")
prob_w <- .load_prob_matrix_women()
adv_w  <- tryCatch(readr::read_csv("output/women/team_advancement_probs_injury_adj.csv",
                                    show_col_types=FALSE),
                   error = function(e) readr::read_csv("output/women/team_advancement_probs.csv",
                                                        show_col_types=FALSE))

women_layout <- list(
  Reg1_FortWorth  = list(side="left",  half="top"),
  Reg3_FortWorth  = list(side="left",  half="bottom"),
  Reg2_Sacramento = list(side="right", half="top"),
  Reg4_Sacramento = list(side="right", half="bottom")
)

br1_w <- simulate_greedy(prob_w, BRACKET_SLOTS_W, BRACKET_OPPS_W, FF_GAMES_W, FF_PAIRS_W)
df1_w <- build_bracket_df(br1_w, BRACKET_SLOTS_W, FF_PAIRS_W, women_layout)

p1_w <- plot_bracket(df1_w,
  title     = "2026 Women's NCAA Tournament — Predicted Bracket #1 (All Favorites)",
  subtitle  = paste0("Champion: ", shorten(br1_w$champ, 30),
                     " | Final Four: ", paste(shorten(br1_w$ff_w, 20), collapse=" vs ")),
  adv_probs = adv_w)

ggsave("output/women/predicted_bracket_1.png", p1_w, width=18, height=11, dpi=150)
message("Saved predicted_bracket_1.png (women)")

# Women's Bracket 2: Top 5 upsets
all_r64_w <- purrr::map_dfr(names(BRACKET_SLOTS_W), function(reg) {
  sl <- BRACKET_SLOTS_W[[reg]]; op <- BRACKET_OPPS_W[[reg]]
  purrr::map_dfr(1:8, function(i) {
    if (startsWith(op[i], "FF_")) return(NULL)
    p <- gp_w(prob_w, sl[i], op[i])
    tibble(fav=sl[i], dog=op[i], p_fav=p, upset_prob=1-p)
  })
}) |> filter(!is.na(upset_prob)) |> arrange(desc(upset_prob))

cat("Top R64 upset candidates (women):\n")
print(select(all_r64_w, fav, dog, upset_prob) |> head(8))

top5_upsets_w <- head(all_r64_w, 5)
overrides_w <- setNames(top5_upsets_w$dog,
                         paste0(top5_upsets_w$fav, "|||", top5_upsets_w$dog))

br2_w <- simulate_greedy(prob_w, BRACKET_SLOTS_W, BRACKET_OPPS_W, FF_GAMES_W, FF_PAIRS_W,
                          upset_overrides = overrides_w)
df2_w <- build_bracket_df(br2_w, BRACKET_SLOTS_W, FF_PAIRS_W, women_layout)

p2_w <- plot_bracket(df2_w,
  title     = "2026 Women's NCAA Tournament — Predicted Bracket #2 (Top 5 Upsets Included)",
  subtitle  = paste0("Champion: ", shorten(br2_w$champ, 30),
                     " | Final Four: ", paste(shorten(br2_w$ff_w, 20), collapse=" vs ")),
  adv_probs = adv_w)

ggsave("output/women/predicted_bracket_2.png", p2_w, width=18, height=11, dpi=150)
message("Saved predicted_bracket_2.png (women)")

message("All 4 bracket plots saved.")
