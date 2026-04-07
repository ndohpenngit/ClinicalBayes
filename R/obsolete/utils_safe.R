safe_var <- function(x) {
  v <- stats::var(x)
  if (is.na(v) || v <= 0) stop("Variance must be positive")
  v
}

validate_nig <- function(prior) {
  if (any(is.na(unlist(prior))))
    stop("NIG prior contains NA")

  if (prior$kappa <= 0 || prior$nu <= 0 || prior$s2 <= 0)
    stop("Invalid NIG parameters")

  TRUE
}

safe_nig <- function(prior) {
  validate_nig(prior)
  prior
}

`%||%` <- function(a, b) if (!is.null(a)) a else b
