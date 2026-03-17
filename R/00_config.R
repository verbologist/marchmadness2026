# 00_config.R -- Constants, tournament metadata, and global configuration
# =============================================================================

library(here)
source(here::here("R", "utils.R"))

# ---------------------------------------------------------------------------
# Season parameters
# ---------------------------------------------------------------------------
HIST_YEAR_START <- 2013
HIST_YEAR_END   <- 2025
CURRENT_SEASON  <- 2026
HIST_SEASONS    <- seq(HIST_YEAR_START, HIST_YEAR_END)

# ---------------------------------------------------------------------------
# Simulation parameters
# ---------------------------------------------------------------------------
N_SIMS      <- 10000
RANDOM_SEED <- 2026

# ---------------------------------------------------------------------------
# Tournament rounds
# ---------------------------------------------------------------------------
ROUNDS <- c(
  "First Four"   = 0,
  "Round of 64"  = 1,
  "Round of 32"  = 2,
  "Sweet 16"     = 3,
  "Elite 8"      = 4,
  "Final Four"   = 5,
  "Championship" = 6
)

ROUND_LABELS <- c(
  "0" = "First Four",
  "1" = "Round of 64",
  "2" = "Round of 32",
  "3" = "Sweet 16",
  "4" = "Elite 8",
  "5" = "Final Four",
  "6" = "Championship"
)

# ---------------------------------------------------------------------------
# 2026 Men's Tournament -- Full 68-team bracket
# Duke is #1 overall seed (South region)
# ---------------------------------------------------------------------------
# Region order follows standard NCAA bracket layout
MEN_REGIONS <- c("South", "East", "Midwest", "West")

# Historically high-upset seed matchups (lower_seed vs higher_seed)
UPSET_WATCH_MATCHUPS <- list(
  c(5, 12),  # ~35% upset rate historically
  c(6, 11),  # ~37% upset rate
  c(7, 10),  # ~39% upset rate
  c(8, 9)    # ~49% upset rate (near coin flip)
)

# ---------------------------------------------------------------------------
# 2026 Women's Tournament
# UConn is #1 overall seed
# ---------------------------------------------------------------------------
WOMEN_REGIONS <- c("Albany", "Bridgeport", "Portland", "Spokane")

# ---------------------------------------------------------------------------
# Model parameters (from research literature)
# arXiv:2603.10916 -- CFA ensemble: LR + SVM achieved 73% accuracy
# arXiv:2503.21790 -- Logistic regression top 99.5% ESPN Tournament Challenge
# ---------------------------------------------------------------------------
MODEL_PARAMS <- list(
  xgboost = list(
    eta              = 0.05,
    max_depth        = 6,
    subsample        = 0.8,
    colsample_bytree = 0.8,
    min_child_weight = 10,
    nrounds          = 500,
    early_stopping   = 30,
    objective        = "binary:logistic",
    eval_metric      = "logloss"
  ),
  logistic = list(
    alpha   = 0.5,   # elastic net mix (0 = ridge, 1 = lasso)
    nfolds  = 10
  ),
  # Ensemble weights (XGBoost usually stronger; calibrated by held-out AUC)
  ensemble = list(
    w_logistic = 0.4,
    w_xgboost  = 0.6
  )
)

# ---------------------------------------------------------------------------
# Key predictive features (ranked by importance per research literature)
# ---------------------------------------------------------------------------
FEATURE_COLS <- c(
  # Most predictive (BartTorvik efficiency margin)
  "adj_em_diff",
  "adj_oe_diff",
  "adj_de_diff",
  # Seed / ranking
  "seed_diff",
  "log_seed_ratio",
  # Recent form -- last 10 games (arXiv:2508.02725 SHAP rank #3-4 of 22)
  "form_adj_em_diff",      # rolling adj_EM differential: captures injuries, momentum
  "form_win_pct_diff",     # raw recent win rate differential
  # Dean Oliver Four Factors (offensive)
  "efg_diff",
  "to_diff",
  "or_diff",
  "ftr_diff",
  # Dean Oliver Four Factors (defensive)
  "def_efg_diff",
  "def_to_diff",
  "def_or_diff",
  # Points per possession
  "ppp_diff",
  # Style
  "tempo_diff",
  # Tempo mismatch interactions (arXiv:2508.02725 Section 3.2)
  # tempo_diff * adj_em_diff: pace-forcing team compounds efficiency edge with possessions
  # tempo_diff * efg_diff:    pace-forcing team also shoots better => double benefit
  "tempo_em_interaction",
  "tempo_efg_interaction",
  # Regional familiarity proxy (arXiv:2503.21790 Section 4: ~2.3% win prob shift)
  # Conference geography aligned with tournament region => fan support + travel ease
  "region_fam_diff",
  # Upset indicators
  "is_5_12", "is_6_11", "is_7_10", "is_8_9",
  "seed_product",
  # Context
  "round"
)

# Conference -> NCAA Tournament region alignment (used for region_fam_diff feature)
# Based on geographic clustering of conference member institutions
CONF_REGION_MAP <- c(
  # East: ACC, Big East, A-10, MAAC, Patriot, NEC, Colonial, America East, MEAC
  "ACC"="East", "Big East"="East", "Atlantic 10"="East", "A-10"="East",
  "MAAC"="East", "Patriot"="East", "NEC"="East", "CAA"="East",
  "America East"="East", "MEAC"="East", "Ivy"="East",
  # South: SEC, AAC, Sun Belt, SWAC, Southern, OVC, ASUN, C-USA, Big South
  "SEC"="South", "AAC"="South", "Sun Belt"="South", "SWAC"="South",
  "Southern"="South", "OVC"="South", "ASUN"="South", "CUSA"="South",
  "Big South"="South", "SOCO"="South",
  # Midwest: Big Ten, MAC, MVC, Summit, Missouri Valley, Pioneer
  "Big Ten"="Midwest", "MAC"="Midwest", "MVC"="Midwest",
  "Summit"="Midwest", "Pioneer"="Midwest", "Horizon"="Midwest", "MVFC"="Midwest",
  # West: Pac-12, MWC, WCC, WAC, Big Sky, Big West, Southland
  "Pac-12"="West", "MWC"="West", "WCC"="West", "WAC"="West",
  "Big Sky"="West", "Big West"="West", "Southland"="West",
  # Big 12 spans Midwest/South -- assign Midwest (most members historically)
  "Big 12"="Midwest"
)

# ---------------------------------------------------------------------------
# ggplot2 theme defaults
# ---------------------------------------------------------------------------
library(ggplot2)
THEME_MM <- theme_minimal(base_size = 12) +
  theme(
    plot.title    = element_text(face = "bold", size = 14),
    plot.subtitle = element_text(size = 11, color = "gray40"),
    legend.position = "bottom",
    panel.grid.minor = element_blank()
  )
