# ======================================================
# R/utils_bayes_continuous.R
# Bayesian helper utilities — Continuous endpoints
# ======================================================

# ------------------------------------------------------
# Validate NIG parameters
# ------------------------------------------------------
validate_nig <- function(m0, k0, a0, b0) {
  if (!is.finite(m0)) stop("Invalid m0")
  if (!is.finite(k0) || k0 <= 0) stop("Invalid k0")
  if (!is.finite(a0) || a0 <= 1) stop("Invalid a0")
  if (!is.finite(b0) || b0 <= 0) stop("Invalid b0")
  TRUE
}

# ------------------------------------------------------
# Safe NIG constructor (returns NULL if invalid)
# ------------------------------------------------------
safe_nig <- function(m0, k0, a0, b0) {
  ok <- try(validate_nig(m0, k0, a0, b0), silent = TRUE)
  if (inherits(ok, "try-error")) return(NULL)

  list(m0 = m0, k0 = k0, a0 = a0, b0 = b0)
}

# ------------------------------------------------------
# Prior from historical summaries (ESS-calibrated)
# ------------------------------------------------------
cont_prior_from_hist <- function(ybar, s2, n,
                                 alpha,
                                 a0_base = 2) {

  stopifnot(length(ybar) == length(s2),
            length(ybar) == length(n))

  # pooled mean
  m0 <- weighted.mean(ybar, n)

  # pooled variance
  v_pool <- if (length(n) > 1) {
    sum((n - 1) * s2) / sum(n - 1)
  } else {
    s2
  }

  # effective sample size
  k0 <- alpha * sum(n)

  # --- HARD SAFETY GUARDS ---
  if (!is.finite(k0) || k0 <= 0) return(NULL)
  if (!is.finite(v_pool) || v_pool <= 0) return(NULL)

  a0 <- a0_base
  b0 <- v_pool * (a0 - 1)

  safe_nig(m0, k0, a0, b0)
}

# ------------------------------------------------------
# Posterior update
# ------------------------------------------------------
cont_posterior <- function(prior, ybar, s2, n) {
  stopifnot(!is.null(prior))

  k_n <- prior$k0 + n
  m_n <- (prior$k0 * prior$m0 + n * ybar) / k_n

  a_n <- prior$a0 + n / 2
  b_n <- prior$b0 +
    0.5 * (n - 1) * s2 +
    0.5 * (prior$k0 * n / k_n) * (ybar - prior$m0)^2

  safe_nig(m_n, k_n, a_n, b_n)
}

# ------------------------------------------------------
# ESS for NIG
# ------------------------------------------------------
ess_nig <- function(prior, post = NULL) {
  list(
    ess_prior = prior$k0,
    ess_post  = if (!is.null(post)) post$k0 else NA_real_
  )
}

# ------------------------------------------------------
# Sampling helpers
# ------------------------------------------------------
draw_mu_from_nig <- function(nig, nsim = 30000) {
  s2 <- 1 / rgamma(nsim, nig$a0, nig$b0)
  rnorm(nsim, nig$m0, sqrt(s2 / nig$k0))
}

draw_sigma2_from_nig <- function(nig, nsim = 30000) {
  1 / rgamma(nsim, nig$a0, nig$b0)
}
