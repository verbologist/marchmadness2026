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
  # Most predictive (KenPom/BartTorvik efficiency)
  "adj_em_diff",
  "adj_oe_diff",
  "adj_de_diff",
  "trank_diff",
  # Seed / ranking
  "seed_diff",
  "log_seed_ratio",
  "net_rank_diff",
  # Shooting / possession (Four Factors)
  "efg_diff",
  "to_rate_diff",
  "or_rate_diff",
  "dr_rate_diff",
  "ft_rate_diff",
  "three_pt_rate_diff",
  # Style
  "tempo_diff",
  # Upset indicators
  "is_5_12", "is_6_11", "is_7_10", "is_8_9",
  # Context
  "round"
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
