# marchmadness2026

**2026 NCAA Men's & Women's March Madness Bracket Prediction System**

Ensemble machine learning model combining regularized logistic regression and XGBoost, with 10,000-iteration Monte Carlo bracket simulation. Separate pipelines for men's and women's tournaments.

---

## Academic Basis

| Paper | Method | Result |
|-------|--------|--------|
| [arXiv:2503.21790](https://arxiv.org/abs/2503.21790) | Logistic regression on AdjOE/AdjDE/Power Rating | Top 99.5% ESPN Tournament Challenge |
| [arXiv:2603.10916](https://arxiv.org/html/2603.10916v1) | Combinatorial Fusion Analysis (LR + SVM + CNN) | ~73% accuracy on 2024 data |
| [arXiv:2508.02725](https://arxiv.org/html/2508.02725v1) | LSTM/Transformer with Elo + KenPom features | AUC 0.847 |

---

## Key Predictive Features

Based on the research literature, the most predictive features are:

1. **AdjEM differential** — Adjusted Efficiency Margin (AdjOE − AdjDE) from KenPom/BartTorvik. Single most predictive feature.
2. **Seed differential** — Captures tournament bracket position.
3. **BartTorvik T-Rank differential** — Composite team strength rating.
4. **Four Factors** (Dean Oliver): eFG%, turnover rate, offensive rebound rate, free throw rate.
5. **Adjusted Tempo** — Style-of-play interaction effects.
6. **Upset indicators** — Binary flags for historically volatile matchups (5v12, 6v11, 7v10, 8v9).

---

## Data Sources

| Source | Data | Package/API |
|--------|------|-------------|
| BartTorvik (T-Rank) | AdjOE, AdjDE, AdjEM, AdjT, Four Factors | `cbbdata` |
| ESPN / hoopR | Historical game results, schedules, tournament brackets | `hoopR` |
| wehoop | Women's game results and box scores | `wehoop` |
| NCAA.com | NET rankings | `rvest` scrape |

---

## Project Structure

```
marchmadness2026/
├── main.R                          # Pipeline orchestrator
├── R/
│   ├── 00_config.R                 # Constants, tournament metadata
│   ├── 01_install_packages.R       # One-time package installation
│   ├── 02_download_historical.R    # Historical games (2013-2025)
│   ├── 03_download_team_stats.R    # KenPom/BartTorvik advanced stats
│   ├── 05_download_brackets.R      # 2026 bracket + historical brackets
│   ├── 06_feature_engineering.R    # Matchup feature construction
│   ├── 07_model_train.R            # Logistic regression + XGBoost ensemble
│   ├── 08_model_evaluate.R         # Calibration, AUC, accuracy diagnostics
│   ├── 09_predict_2026.R           # Generate 2026 win probability matrix
│   ├── 10_monte_carlo.R            # 10,000-iteration bracket simulation
│   ├── 11_visualize.R              # Static plots (ggplot2)
│   └── utils.R                     # Shared helpers, directory constants
├── shiny/
│   └── app.R                       # Interactive bracket explorer (Shiny)
├── data/                           # Created at runtime (gitignored)
│   ├── raw/{men,women}/
│   └── processed/{men,women}/
└── output/                         # Created at runtime (gitignored)
    ├── men/
    ├── women/
    └── models/
```

---

## Quick Start

### 1. Install Required Packages

```r
source("R/01_install_packages.R")
```

Required CRAN packages: `tidyverse`, `xgboost`, `glmnet`, `tidymodels`, `httr2`, `rvest`, `furrr`, `shiny`, `RColorBrewer`

Required GitHub packages: `hoopR`, `wehoop`, `cbbdata`

### 2. Configure BartTorvik API Key (for Men's advanced stats)

```r
usethis::edit_r_environ()
# Add: CBBDATA_KEY=your_free_key
# Register at: https://cbbdata.aweatherman.com/
```

### 3. Run the Full Pipeline

```r
source("main.R")
# or from terminal:
# Rscript main.R
# Rscript main.R --men-only
# Rscript main.R --skip-download   # if data already cached
```

### 4. Launch Shiny App

```r
shiny::runApp("shiny/app.R")
```

---

## Model Architecture

```
Historical Data (2013-2025)
        │
        ▼
Feature Engineering (06_feature_engineering.R)
  ├── AdjEM diff, AdjOE diff, AdjDE diff
  ├── Seed diff, log(seed ratio)
  ├── Four Factors: eFG%, TO%, OR%, FT%
  ├── Tempo diff
  └── Upset indicator dummies
        │
        ├──────────────────────────────┐
        ▼                              ▼
Logistic Regression              XGBoost
(elastic net, α=0.5)       (η=0.05, depth=6)
        │                              │
        └──────────┬───────────────────┘
                   ▼
          Ensemble (weighted avg)
          w_LR=0.4, w_XGB=0.6
          (calibrated on held-out seasons)
                   │
                   ▼
        Win Probability Matrix
        (all 68×68 team pairs)
                   │
                   ▼
       Monte Carlo Simulation
         (10,000 iterations)
                   │
          ┌────────┴────────┐
          ▼                 ▼
  Championship         Advancement
  Probabilities       Probabilities
                      (by round)
```

---

## Outputs

After running the pipeline, find results in `output/`:

| File | Description |
|------|-------------|
| `{gender}/champion_probs.png` | Championship win probability bar chart |
| `{gender}/advancement_heatmap.png` | All teams × all rounds probability heatmap |
| `{gender}/final_four_probs.png` | Final Four probability chart |
| `{gender}/upset_watch.png` | Upset probability for volatile matchups |
| `{gender}/calibration.png` | Model calibration curve |
| `{gender}/feature_importance.png` | XGBoost feature importance |
| `{gender}/team_advancement_probs.csv` | Full advancement probability table |
| `{gender}/champion_distribution.csv` | Championship probability per team |

---

## 2026 Tournament Notes

- **Men's #1 overall seed**: Duke (South Region)
- **Women's #1 overall seed**: UConn
- Selection Sunday: March 15, 2026
- First/Second rounds: March 17-22, 2026
