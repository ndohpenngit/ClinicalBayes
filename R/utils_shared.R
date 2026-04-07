# ======================================================
# R/utils_shared.R
# Shared safety utilities
# ======================================================

`%||%` <- function(x, y) if (!is.null(x)) x else y

get_current_hist_df <- function(app_rv) {
  if (!is.null(app_rv$current_dataset) &&
      nzchar(app_rv$current_dataset) &&
      !is.null(app_rv$datasets[[app_rv$current_dataset]])) {
    return(app_rv$datasets[[app_rv$current_dataset]])
  }
  app_rv$hist_df
}

# ------------------------------------------------------
# SAFE variance from SD and n
# ------------------------------------------------------
safe_var <- function(sd, n) {
  if (is.null(sd) || is.null(n)) return(NA_real_)
  if (is.na(sd) || is.na(n)) return(NA_real_)
  if (n < 2 || sd <= 0) return(NA_real_)

  ((n - 1) / n) * sd^2
}
