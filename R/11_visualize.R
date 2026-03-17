# 11_visualize.R -- Static visualizations
# =============================================================================
# Produces:
#   output/{men,women}/bracket_probs.png        -- bracket heatmap
#   output/{men,women}/champion_probs.png        -- championship bar chart
#   output/{men,women}/advancement_heatmap.png   -- teams x rounds heatmap
#   output/{men,women}/upset_watch.png           -- upset probability chart
# =============================================================================

source(here::here("R", "utils.R"))
source(here::here("R", "00_config.R"))

library(dplyr)
library(tidyr)
library(readr)
library(ggplot2)
library(scales)
library(RColorBrewer)
library(stringr)

# ---------------------------------------------------------------------------
# create_visualizations() -- main exported function
# ---------------------------------------------------------------------------
create_visualizations <- function(gender = c("men", "women")) {
  gender <- match.arg(gender)
  paths  <- gender_paths(gender)

  message("Creating visualizations for ", gender, "...")

  # Load simulation results
  adv_probs  <- safe_read_csv(file.path(paths$output, "team_advancement_probs.csv"))
  champ_df   <- safe_read_csv(file.path(paths$output, "champion_distribution.csv"))
  upset_df   <- safe_read_csv(file.path(paths$output, "upset_frequency.csv"))

  if (is.null(adv_probs) || is.null(champ_df)) {
    stop("Simulation results not found. Run 10_monte_carlo.R first.")
  }

  # Join seed from seeds_all.csv (needed for labels)
  seeds_df <- tryCatch(
    safe_read_csv(file.path(paths$processed, "seeds_all.csv")) |>
      filter(year == CURRENT_SEASON) |>
      select(team_name, seed),
    error = function(e) NULL
  )
  if (!is.null(seeds_df)) {
    champ_df  <- champ_df  |> left_join(seeds_df, by = "team_name")
    adv_probs <- adv_probs |> left_join(seeds_df, by = "team_name")
  }
  if (!"seed" %in% names(champ_df))  champ_df$seed  <- NA_integer_
  if (!"seed" %in% names(adv_probs)) adv_probs$seed <- NA_integer_

  title_suffix <- paste("2026", str_to_title(gender), "NCAA Tournament")

  # ---------------------------------------------------------------------------
  # Plot 1: Championship probability bar chart (top 20 teams)
  # ---------------------------------------------------------------------------
  p_champ <- champ_df |>
    arrange(desc(champ_prob)) |>
    slice_head(n = 20) |>
    mutate(
      team_label = paste0(team_name, " (", seed, ")"),
      team_label = reorder(team_label, champ_prob)
    ) |>
    ggplot(aes(x = team_label, y = champ_prob)) +
    geom_col(aes(fill = champ_prob), show.legend = FALSE) +
    geom_text(aes(label = paste0(round(champ_prob * 100, 1), "%")),
              hjust = -0.1, size = 3.5) +
    coord_flip(ylim = c(0, max(champ_df$champ_prob) * 1.25)) +
    scale_y_continuous(labels = percent_format()) +
    scale_fill_gradient(low = "#cce5ff", high = "#003087") +
    labs(
      title    = paste("Championship Win Probability:", title_suffix),
      subtitle = paste0("Based on ", format(N_SIMS, big.mark = ","), " Monte Carlo simulations"),
      x = NULL, y = "Probability"
    ) +
    THEME_MM

  champ_plot_path <- file.path(paths$output, "champion_probs.png")
  ggsave(champ_plot_path, p_champ, width = 10, height = 8, dpi = 150)
  message("Saved: ", champ_plot_path)

  # ---------------------------------------------------------------------------
  # Plot 2: Team advancement heatmap (teams x rounds)
  # ---------------------------------------------------------------------------
  prob_cols <- names(adv_probs)[startsWith(names(adv_probs), "prob_")]
  round_names <- str_replace_all(str_replace(prob_cols, "^prob_", ""), "_", " ")

  heat_data <- adv_probs |>
    arrange(seed) |>
    slice_head(n = 32) |>  # Top 32 most likely teams for readability
    select(team_name, seed, all_of(prob_cols)) |>
    pivot_longer(cols = all_of(prob_cols),
                 names_to = "round", values_to = "prob") |>
    mutate(
      round = str_replace_all(str_replace(round, "^prob_", ""), "_", " "),
      round = factor(round, levels = round_names),
      team_label = paste0(team_name, " (", seed, ")")
    )

  p_heat <- heat_data |>
    ggplot(aes(x = round, y = reorder(team_label, -seed), fill = prob)) +
    geom_tile(color = "white", linewidth = 0.3) +
    geom_text(aes(label = ifelse(prob > 0.05, paste0(round(prob * 100, 0), "%"), "")),
              size = 2.8, color = "white") +
    scale_fill_gradient2(
      low      = "#f7f7f7",
      mid      = "#4292c6",
      high     = "#003087",
      midpoint = 0.25,
      labels   = percent_format(),
      name     = "Prob"
    ) +
    labs(
      title    = paste("Tournament Advancement Probabilities:", title_suffix),
      subtitle = paste0(format(N_SIMS, big.mark = ","), " Monte Carlo simulations | Top 32 seeds shown"),
      x = "Round", y = NULL
    ) +
    THEME_MM +
    theme(
      axis.text.x = element_text(angle = 30, hjust = 1),
      axis.text.y = element_text(size = 9)
    )

  heat_path <- file.path(paths$output, "advancement_heatmap.png")
  ggsave(heat_path, p_heat, width = 14, height = 12, dpi = 150)
  message("Saved: ", heat_path)

  # ---------------------------------------------------------------------------
  # Plot 3: Upset watch chart
  # ---------------------------------------------------------------------------
  if (!is.null(upset_df) && nrow(upset_df) > 0) {
    p_upset <- upset_df |>
      mutate(
        matchup_label = paste0("#", favorite_seed, " ", favorite, "\nvs\n#",
                               underdog_seed, " ", underdog),
        upset_pct     = round(upset_prob * 100, 1)
      ) |>
      ggplot(aes(x = reorder(matchup_label, upset_prob), y = upset_prob)) +
      geom_col(aes(fill = upset_prob), show.legend = FALSE) +
      geom_text(aes(label = paste0(upset_pct, "%")),
                hjust = -0.1, size = 3.5) +
      coord_flip(ylim = c(0, 0.65)) +
      scale_y_continuous(labels = percent_format()) +
      scale_fill_gradient(low = "#fee5d9", high = "#C8102E") +
      geom_hline(yintercept = 0.5, linetype = "dashed", color = "gray50") +
      labs(
        title    = paste("Upset Watch:", title_suffix),
        subtitle = "Probability that the lower seed wins",
        x = NULL, y = "Upset Probability"
      ) +
      THEME_MM

    upset_path <- file.path(paths$output, "upset_watch.png")
    ggsave(upset_path, p_upset, width = 10, height = max(6, nrow(upset_df) * 0.8), dpi = 150)
    message("Saved: ", upset_path)
  }

  # ---------------------------------------------------------------------------
  # Plot 4: Final Four probabilities by region
  # ---------------------------------------------------------------------------
  ff_col <- names(adv_probs)[grepl("Final_Four|Final Four|5$", names(adv_probs))][1]

  if (!is.na(ff_col)) {
    p_ff <- adv_probs |>
      select(team_name, seed, ff_prob = all_of(ff_col)) |>
      arrange(desc(ff_prob)) |>
      slice_head(n = 16) |>
      mutate(team_label = paste0(team_name, " (#", seed, ")"),
             team_label = reorder(team_label, ff_prob)) |>
      ggplot(aes(x = team_label, y = ff_prob)) +
      geom_col(aes(fill = ff_prob), show.legend = FALSE) +
      geom_text(aes(label = paste0(round(ff_prob * 100, 1), "%")),
                hjust = -0.1, size = 3.5) +
      coord_flip(ylim = c(0, max(adv_probs[[ff_col]]) * 1.25)) +
      scale_y_continuous(labels = percent_format()) +
      scale_fill_gradient(low = "#cce5ff", high = "#003087") +
      labs(
        title    = paste("Final Four Probabilities:", title_suffix),
        x = NULL, y = "Probability"
      ) +
      THEME_MM

    ff_path <- file.path(paths$output, "final_four_probs.png")
    ggsave(ff_path, p_ff, width = 10, height = 7, dpi = 150)
    message("Saved: ", ff_path)
  }

  message("All visualizations saved to: ", paths$output)
  invisible(TRUE)
}
