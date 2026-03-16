# utils.R -- Shared helpers and constants
# =============================================================================

library(here)
library(httr2)
library(readr)

# ---------------------------------------------------------------------------
# Directory constants
# ---------------------------------------------------------------------------
DATA_DIR    <- here::here("data")
OUTPUT_DIR  <- here::here("output")

RAW_MEN_DIR         <- file.path(DATA_DIR, "raw", "men")
RAW_WOMEN_DIR       <- file.path(DATA_DIR, "raw", "women")
PROCESSED_MEN_DIR   <- file.path(DATA_DIR, "processed", "men")
PROCESSED_WOMEN_DIR <- file.path(DATA_DIR, "processed", "women")
OUTPUT_MEN_DIR      <- file.path(OUTPUT_DIR, "men")
OUTPUT_WOMEN_DIR    <- file.path(OUTPUT_DIR, "women")
OUTPUT_MODELS_DIR   <- file.path(OUTPUT_DIR, "models")

# ---------------------------------------------------------------------------
# ensure_dirs() -- create all required directories
# ---------------------------------------------------------------------------
ensure_dirs <- function() {
  dirs <- c(
    RAW_MEN_DIR, RAW_WOMEN_DIR,
    PROCESSED_MEN_DIR, PROCESSED_WOMEN_DIR,
    OUTPUT_MEN_DIR, OUTPUT_WOMEN_DIR,
    OUTPUT_MODELS_DIR
  )
  invisible(lapply(dirs, dir.create, recursive = TRUE, showWarnings = FALSE))
  message("Directories ready.")
}

# ---------------------------------------------------------------------------
# gender_paths(gender) -- return named list of paths for a gender
# ---------------------------------------------------------------------------
gender_paths <- function(gender) {
  stopifnot(gender %in% c("men", "women"))
  list(
    raw       = file.path(DATA_DIR,    "raw",       gender),
    processed = file.path(DATA_DIR,    "processed", gender),
    output    = file.path(OUTPUT_DIR,  gender)
  )
}

# ---------------------------------------------------------------------------
# retry_download() -- HTTP GET with exponential backoff
# ---------------------------------------------------------------------------
retry_download <- function(url, max_attempts = 4, wait_base = 2,
                           parse_fn = resp_body_json) {
  for (attempt in seq_len(max_attempts)) {
    result <- tryCatch({
      req <- request(url) |>
        req_headers(
          "User-Agent" = "marchmadness2026/1.0 (R; educational bracket prediction)"
        ) |>
        req_timeout(30)
      resp <- req_perform(req)
      parse_fn(resp)
    }, error = function(e) {
      if (attempt < max_attempts) {
        wait <- wait_base^attempt
        message(sprintf("  Attempt %d failed (%s). Retrying in %ds...",
                        attempt, conditionMessage(e), wait))
        Sys.sleep(wait)
        NULL
      } else {
        stop(sprintf("All %d attempts failed for: %s\n  Last error: %s",
                     max_attempts, url, conditionMessage(e)))
      }
    })
    if (!is.null(result)) return(result)
  }
}

# ---------------------------------------------------------------------------
# safe_read_csv() -- read CSV if it exists, else return NULL with warning
# ---------------------------------------------------------------------------
safe_read_csv <- function(path, ...) {
  if (!file.exists(path)) {
    warning(sprintf("File not found: %s", path))
    return(NULL)
  }
  readr::read_csv(path, show_col_types = FALSE, ...)
}

# ---------------------------------------------------------------------------
# banner() -- print a section header
# ---------------------------------------------------------------------------
banner <- function(msg) {
  message("\n", strrep("=", 70))
  message("  ", msg)
  message(strrep("=", 70))
}
