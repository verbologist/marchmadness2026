# 01_install_packages.R -- One-time package installation
# Run this script manually once before the pipeline.
# =============================================================================

# CRAN packages not yet installed
cran_pkgs <- c(
  "tidyverse", "here", "httr2", "jsonlite", "rvest",
  "xgboost", "glmnet", "tidymodels", "ranger",
  "furrr", "future", "shiny", "RColorBrewer",
  "lubridate", "scales", "purrr"
)

to_install_cran <- cran_pkgs[!vapply(cran_pkgs, requireNamespace, logical(1), quietly = TRUE)]
if (length(to_install_cran) > 0) {
  message("Installing CRAN packages: ", paste(to_install_cran, collapse = ", "))
  install.packages(to_install_cran)
} else {
  message("All CRAN packages already installed.")
}

# GitHub / special packages
github_pkgs <- list(
  hoopR   = "sportsdataverse/hoopR",
  wehoop  = "sportsdataverse/wehoop",
  cbbdata = "andreweatherman/cbbdata"
)

for (pkg in names(github_pkgs)) {
  if (!requireNamespace(pkg, quietly = TRUE)) {
    message("Installing from GitHub: ", github_pkgs[[pkg]])
    if (!requireNamespace("remotes", quietly = TRUE)) install.packages("remotes")
    remotes::install_github(github_pkgs[[pkg]])
  } else {
    message(pkg, " already installed.")
  }
}

message("\nAll packages installed. Ready to run the pipeline.")
message("Next: set CBBDATA_KEY in .Renviron for BartTorvik data access.")
message("  1. Run: usethis::edit_r_environ()")
message("  2. Add: CBBDATA_KEY=your_free_key_from_cbbdata.io")
