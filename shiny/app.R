# shiny/app.R -- Interactive 2026 NCAA Bracket Explorer
# =============================================================================
# Run with: shiny::runApp("shiny/app.R")
# =============================================================================

library(shiny)
library(ggplot2)
library(dplyr)
library(tidyr)
library(readr)
library(scales)
library(stringr)
library(tibble)

# Resolve paths relative to shiny/ directory
project_root <- here::here()
OUTPUT_DIR   <- file.path(project_root, "output")

INJURY_TABLE <- tribble(
  ~team_name,                     ~player,              ~ppg,  ~status,
  "Texas Tech Red Raiders",       "JT Toppin",          21.8,  "OUT",
  "Texas Tech Red Raiders",       "LeJuan Watts",       11.5,  "Questionable",
  "North Carolina Tar Heels",     "Caleb Wilson",       19.8,  "OUT",
  "BYU Cougars",                  "Richie Saunders",    18.0,  "OUT",
  "Gonzaga Bulldogs",             "Braden Huff",        17.8,  "OUT",
  "UCLA Bruins",                  "Tyler Bilodeau",     17.6,  "Questionable",
  "UCLA Bruins",                  "Donovan Dent",       13.5,  "Questionable",
  "Duke Blue Devils",             "Caleb Foster",        8.5,  "OUT",
  "Duke Blue Devils",             "Patrick Ngongba II", 10.7,  "Questionable",
  "Clemson Tigers",               "Carter Welling",     10.2,  "OUT",
  "Vanderbilt Commodores",        "Frankie Collins",     8.5,  "OUT",
  "Michigan Wolverines",          "L.J. Cason",          7.5,  "OUT",
  "Kentucky Wildcats",            "Jaland Lowe",         9.2,  "OUT",
  "Maryland Terrapins",           "Kaylene Smikle",     15.0,  "OUT",
  "Maryland Terrapins",           "Bri McDaniel",        8.5,  "OUT",
  "Colorado State Rams",          "Lexus Bargesser",    15.6,  "OUT"
)

load_data <- function(gender, injury_adj = FALSE) {
  suffix <- if (injury_adj) "_injury_adj" else ""
  proc_dir <- file.path(project_root, "data", "processed", gender)
  list(
    adv_probs = tryCatch(
      read_csv(file.path(OUTPUT_DIR, gender, paste0("team_advancement_probs", suffix, ".csv")), show_col_types = FALSE),
      error = function(e) tryCatch(
        read_csv(file.path(OUTPUT_DIR, gender, "team_advancement_probs.csv"), show_col_types = FALSE),
        error = function(e2) NULL)
    ),
    champ_df = tryCatch(
      read_csv(file.path(OUTPUT_DIR, gender, paste0("champion_distribution", suffix, ".csv")), show_col_types = FALSE),
      error = function(e) tryCatch(
        read_csv(file.path(OUTPUT_DIR, gender, "champion_distribution.csv"), show_col_types = FALSE),
        error = function(e2) NULL)
    ),
    upset_df = tryCatch(
      read_csv(file.path(OUTPUT_DIR, gender, "upset_frequency.csv"), show_col_types = FALSE),
      error = function(e) NULL
    ),
    win_probs = tryCatch(
      read_csv(file.path(proc_dir, paste0("win_prob_matrix_2026", suffix, ".csv")), show_col_types = FALSE),
      error = function(e) tryCatch(
        read_csv(file.path(proc_dir, "win_prob_matrix_2026.csv"), show_col_types = FALSE),
        error = function(e2) NULL)
    )
  )
}

# ============================================================================
# UI
# ============================================================================
ui <- fluidPage(
  tags$head(
    tags$style(HTML("
      body { font-family: 'Helvetica Neue', Arial, sans-serif; }
      .sidebar { background: #f8f9fa; padding: 15px; border-radius: 8px; }
      h2 { color: #003087; font-weight: bold; }
      .nav-tabs > li > a { color: #003087; font-weight: 500; }
      .shiny-plot-output { border-radius: 6px; box-shadow: 0 2px 4px rgba(0,0,0,0.1); }
    "))
  ),

  titlePanel(
    div(
      h2("2026 NCAA March Madness Bracket Predictor"),
      p(style = "color: #666; font-size: 14px;",
        "Ensemble ML model (Logistic Regression + XGBoost) | ",
        "10,000 Monte Carlo simulations | ",
        "Based on KenPom/BartTorvik efficiency metrics")
    )
  ),

  sidebarLayout(
    sidebarPanel(
      class = "sidebar",
      selectInput("gender", "Tournament:",
                  choices = c("Men's" = "men", "Women's" = "women"),
                  selected = "men"),

      checkboxInput("injury_adj",
                    HTML("<b>Apply Injury Adjustments</b><br><small style='color:#c0392b'>Updated Mar 17, 2026</small>"),
                    value = TRUE),

      conditionalPanel(
        condition = "input.injury_adj == true",
        div(style = "background:#fff3cd; border-left:3px solid #f39c12; padding:8px; border-radius:4px; font-size:11px; margin-bottom:8px;",
          HTML("<b>Key injuries applied:</b><br>
               🔴 JT Toppin (TTU) OUT – ACL<br>
               🔴 Caleb Wilson (UNC) OUT – Thumb<br>
               🔴 Richie Saunders (BYU) OUT – Knee<br>
               🔴 Braden Huff (GON) OUT – Knee<br>
               🟡 Tyler Bilodeau (UCLA) Q – Knee<br>
               🟡 Donovan Dent (UCLA) Q – Calf<br>
               🔴 Caleb Foster (DUKE) OUT – Foot<br>
               🟡 P. Ngongba II (DUKE) Q – Foot"))
      ),

      hr(),
      h4("Matchup Explorer"),
      uiOutput("team_a_select"),
      uiOutput("team_b_select"),
      actionButton("calc_matchup", "Calculate Win Probability",
                   class = "btn btn-primary btn-block"),

      hr(),
      p(style = "font-size: 11px; color: #888;",
        "Model based on historical KenPom/BartTorvik data (2013-2025).",
        "Key papers: arXiv:2503.21790, arXiv:2603.10916, arXiv:2508.02725.")
    ),

    mainPanel(
      tabsetPanel(
        tabPanel("Championship Odds",
          br(),
          plotOutput("champ_plot", height = "600px")
        ),
        tabPanel("Advancement Heatmap",
          br(),
          plotOutput("heat_plot", height = "700px")
        ),
        tabPanel("Final Four",
          br(),
          plotOutput("ff_plot", height = "500px")
        ),
        tabPanel("Upset Watch",
          br(),
          plotOutput("upset_plot", height = "500px"),
          br(),
          tableOutput("upset_table")
        ),
        tabPanel("Matchup Explorer",
          br(),
          verbatimTextOutput("matchup_result"),
          br(),
          plotOutput("matchup_plot", height = "250px")
        ),
        tabPanel("Data Table",
          br(),
          DT::dataTableOutput("adv_table")
        ),
        tabPanel("Injury Report",
          br(),
          h4("Tournament Injury Adjustments", style="color:#c0392b; font-weight:bold;"),
          p(style="color:#666; font-size:12px;",
            "adj_EM penalty = PPG × 0.30 (OUT) or PPG × 0.15 (Questionable). ",
            "Applied via logistic shift: logit(p_new) = logit(p_orig) − 0.10 × (penaltyA − penaltyB)."),
          tableOutput("injury_table"),
          br(),
          h4("Championship Probability: Baseline vs Injury-Adjusted"),
          plotOutput("injury_compare_plot", height = "500px")
        )
      )
    )
  )
)

# ============================================================================
# Server
# ============================================================================
server <- function(input, output, session) {

  # Reactive: load data when gender or injury toggle changes
  data <- reactive({
    load_data(input$gender, isTRUE(input$injury_adj))
  })

  adj_label <- reactive({
    if (isTRUE(input$injury_adj)) " (Injury-Adjusted)" else " (Baseline)"
  })

  # Team selectors
  output$team_a_select <- renderUI({
    d <- data()
    if (is.null(d$win_probs)) return(NULL)
    teams <- sort(unique(d$win_probs$team_a_name))
    selectInput("team_a", "Team A:", choices = teams, selected = teams[1])
  })

  output$team_b_select <- renderUI({
    d <- data()
    if (is.null(d$win_probs)) return(NULL)
    teams <- sort(unique(d$win_probs$team_b_name))
    selectInput("team_b", "Team B:", choices = teams, selected = teams[min(5, length(teams))])
  })

  # Championship plot
  output$champ_plot <- renderPlot({
    d <- data()
    if (is.null(d$champ_df)) {
      return(ggplot() + annotate("text", x = 0.5, y = 0.5,
             label = "Run the pipeline first:\nmain.R", size = 8) + theme_void())
    }

    d$champ_df |>
      arrange(desc(champ_prob)) |>
      slice_head(n = 25) |>
      mutate(team_label = reorder(team_name, champ_prob)) |>
      ggplot(aes(x = team_label, y = champ_prob)) +
      geom_col(aes(fill = champ_prob), show.legend = FALSE) +
      geom_text(aes(label = paste0(round(champ_prob * 100, 1), "%")),
                hjust = -0.1, size = 3.5) +
      coord_flip(ylim = c(0, max(d$champ_df$champ_prob) * 1.3)) +
      scale_y_continuous(labels = percent_format()) +
      scale_fill_gradient(low = "#cce5ff", high = "#003087") +
      labs(title = paste0("Championship Win Probability: ", str_to_title(input$gender), adj_label()),
           x = NULL, y = "Probability") +
      theme_minimal(base_size = 13) +
      theme(plot.title = element_text(face = "bold"))
  })

  # Heatmap plot
  output$heat_plot <- renderPlot({
    d <- data()
    if (is.null(d$adv_probs)) return(ggplot() + theme_void())

    prob_cols <- names(d$adv_probs)[startsWith(names(d$adv_probs), "prob_")]

    d$adv_probs |>
      arrange(seed) |>
      slice_head(n = 32) |>
      select(team_name, seed, all_of(prob_cols)) |>
      pivot_longer(cols = all_of(prob_cols), names_to = "round", values_to = "prob") |>
      mutate(
        round = str_replace_all(str_replace(round, "^prob_", ""), "_", " "),
        team_label = paste0(team_name, " (#", seed, ")")
      ) |>
      ggplot(aes(x = round, y = reorder(team_label, -seed), fill = prob)) +
      geom_tile(color = "white", linewidth = 0.3) +
      geom_text(aes(label = ifelse(prob > 0.05, paste0(round(prob * 100, 0), "%"), "")),
                size = 2.8, color = "white") +
      scale_fill_gradient2(low = "#f7f7f7", mid = "#4292c6", high = "#003087",
                           midpoint = 0.25, labels = percent_format(), name = "Prob") +
      labs(title = paste0("Advancement Probabilities: ", str_to_title(input$gender), adj_label()),
           x = "Round", y = NULL) +
      theme_minimal(base_size = 11) +
      theme(axis.text.x = element_text(angle = 30, hjust = 1),
            plot.title = element_text(face = "bold"))
  })

  # Final Four plot
  output$ff_plot <- renderPlot({
    d <- data()
    if (is.null(d$adv_probs)) return(ggplot() + theme_void())

    ff_col <- names(d$adv_probs)[grepl("Final_Four|Final.Four|prob_5", names(d$adv_probs))][1]
    if (is.na(ff_col)) return(ggplot() + theme_void())

    d$adv_probs |>
      select(team_name, seed, ff_prob = all_of(ff_col)) |>
      arrange(desc(ff_prob)) |>
      slice_head(n = 20) |>
      mutate(team_label = paste0(team_name, " (#", seed, ")"),
             team_label = reorder(team_label, ff_prob)) |>
      ggplot(aes(x = team_label, y = ff_prob)) +
      geom_col(fill = "#003087", alpha = 0.85) +
      geom_text(aes(label = paste0(round(ff_prob * 100, 1), "%")),
                hjust = -0.1, size = 3.5) +
      coord_flip(ylim = c(0, max(d$adv_probs[[ff_col]], na.rm = TRUE) * 1.3)) +
      scale_y_continuous(labels = percent_format()) +
      labs(title = "Final Four Probabilities", x = NULL, y = "Probability") +
      theme_minimal(base_size = 13) +
      theme(plot.title = element_text(face = "bold"))
  })

  # Upset watch
  output$upset_plot <- renderPlot({
    d <- data()
    if (is.null(d$upset_df) || nrow(d$upset_df) == 0) return(ggplot() + theme_void())

    d$upset_df |>
      mutate(matchup_label = paste0("#", favorite_seed, " ", favorite,
                                    "\nvs #", underdog_seed, " ", underdog),
             matchup_label = reorder(matchup_label, upset_prob)) |>
      ggplot(aes(x = matchup_label, y = upset_prob)) +
      geom_col(aes(fill = upset_prob), show.legend = FALSE) +
      geom_text(aes(label = paste0(round(upset_prob * 100, 1), "%")),
                hjust = -0.1, size = 4) +
      coord_flip(ylim = c(0, 0.7)) +
      scale_y_continuous(labels = percent_format()) +
      scale_fill_gradient(low = "#fee5d9", high = "#C8102E") +
      geom_hline(yintercept = 0.5, linetype = "dashed", color = "gray50") +
      labs(title = "Upset Watch", subtitle = "Probability that the lower seed wins",
           x = NULL, y = "Upset Probability") +
      theme_minimal(base_size = 13) +
      theme(plot.title = element_text(face = "bold"))
  })

  output$upset_table <- renderTable({
    d <- data()
    if (is.null(d$upset_df)) return(NULL)
    d$upset_df |>
      mutate(upset_prob = paste0(round(upset_prob * 100, 1), "%")) |>
      select(Matchup = matchup, Favorite = favorite, Underdog = underdog,
             `Upset Prob` = upset_prob)
  })

  # Matchup explorer
  matchup_result <- eventReactive(input$calc_matchup, {
    d <- data()
    if (is.null(d$win_probs) || is.null(input$team_a) || is.null(input$team_b)) {
      return(list(text = "No data available.", df = NULL))
    }

    result <- d$win_probs |>
      filter(
        (team_a_name == input$team_a & team_b_name == input$team_b) |
        (team_a_name == input$team_b & team_b_name == input$team_a)
      )

    if (nrow(result) == 0) return(list(text = "Matchup not found in matrix.", df = NULL))

    # Normalize so team_a is always input$team_a
    if (result$team_a_name[1] != input$team_a) {
      result <- result |> mutate(win_prob_ens = 1 - win_prob_ens,
                                  tmp = team_a_name,
                                  team_a_name = team_b_name,
                                  team_b_name = tmp) |> select(-tmp)
    }

    p_a   <- result$win_prob_ens[1]
    p_b   <- 1 - p_a
    text  <- sprintf(
      "%s (#%s) vs %s (#%s)\n\n%s Win Probability: %.1f%%\n%s Win Probability: %.1f%%\n\n%s is the PREDICTED WINNER",
      result$team_a_name[1], result$team_a_seed[1],
      result$team_b_name[1], result$team_b_seed[1],
      result$team_a_name[1], p_a * 100,
      result$team_b_name[1], p_b * 100,
      if (p_a >= 0.5) result$team_a_name[1] else result$team_b_name[1]
    )
    list(text = text, df = result, p_a = p_a, p_b = p_b,
         name_a = result$team_a_name[1], name_b = result$team_b_name[1])
  })

  output$matchup_result <- renderText({ matchup_result()$text })

  output$matchup_plot <- renderPlot({
    r <- matchup_result()
    if (is.null(r$df)) return(ggplot() + theme_void())

    tibble(
      team = c(r$name_a, r$name_b),
      prob = c(r$p_a, r$p_b)
    ) |>
      ggplot(aes(x = team, y = prob, fill = team)) +
      geom_col(show.legend = FALSE, alpha = 0.9, width = 0.5) +
      geom_text(aes(label = paste0(round(prob * 100, 1), "%")),
                vjust = -0.5, size = 6, fontface = "bold") +
      scale_y_continuous(labels = percent_format(), limits = c(0, 1.1)) +
      scale_fill_manual(values = c("#003087", "#C8102E")) +
      geom_hline(yintercept = 0.5, linetype = "dashed", color = "gray50") +
      labs(x = NULL, y = "Win Probability") +
      theme_minimal(base_size = 14) +
      theme(plot.title = element_text(face = "bold"))
  })

  # Injury report table
  output$injury_table <- renderTable({
    gender <- input$gender
    tbl <- INJURY_TABLE
    if (gender == "women") {
      tbl <- tbl |> filter(team_name %in% c("Maryland Terrapins"))
    } else {
      tbl <- tbl |> filter(!team_name %in% c("Maryland Terrapins"))
    }
    tbl |>
      mutate(
        `adj_EM Penalty` = round(ppg * ifelse(status == "OUT", 0.30, 0.15), 2),
        ppg = round(ppg, 1)
      ) |>
      select(Team = team_name, Player = player, PPG = ppg,
             Status = status, `adj_EM Penalty`)
  }, striped = TRUE, hover = TRUE, bordered = TRUE)

  # Injury comparison plot
  output$injury_compare_plot <- renderPlot({
    gender <- input$gender
    baseline <- tryCatch(
      read_csv(file.path(OUTPUT_DIR, gender, "champion_distribution.csv"), show_col_types=FALSE),
      error = function(e) NULL)
    adjusted <- tryCatch(
      read_csv(file.path(OUTPUT_DIR, gender, "champion_distribution_injury_adj.csv"), show_col_types=FALSE),
      error = function(e) NULL)
    if (is.null(baseline) || is.null(adjusted)) return(ggplot() + theme_void())

    top_teams <- baseline |> arrange(desc(champ_prob)) |> slice_head(n=15) |> pull(team_name)

    bind_rows(
      baseline |> filter(team_name %in% top_teams) |> mutate(model = "Baseline"),
      adjusted |> filter(team_name %in% top_teams) |> mutate(model = "Injury-Adjusted")
    ) |>
      mutate(team_name = factor(team_name, levels=rev(top_teams))) |>
      ggplot(aes(x=team_name, y=champ_prob, fill=model)) +
      geom_col(position="dodge", alpha=0.9) +
      geom_text(aes(label=paste0(round(champ_prob*100,1),"%")),
                position=position_dodge(width=0.9), hjust=-0.1, size=3) +
      coord_flip(ylim=c(0, max(baseline$champ_prob)*1.4)) +
      scale_y_continuous(labels=percent_format()) +
      scale_fill_manual(values=c("Baseline"="#aec6e8","Injury-Adjusted"="#003087")) +
      labs(title=paste("Baseline vs Injury-Adjusted:", str_to_title(gender)),
           x=NULL, y="Championship Probability", fill=NULL) +
      theme_minimal(base_size=12) +
      theme(plot.title=element_text(face="bold"), legend.position="top")
  })

  # Data table
  output$adv_table <- DT::renderDataTable({
    d <- data()
    if (is.null(d$adv_probs)) return(NULL)
    d$adv_probs |>
      select(team_name, seed, everything(), -team_id) |>
      arrange(seed) |>
      DT::datatable(options = list(pageLength = 20, scrollX = TRUE)) |>
      DT::formatPercentage(names(d$adv_probs)[startsWith(names(d$adv_probs), "prob_")], 1)
  })
}

shinyApp(ui = ui, server = server)
