# ======================================================
# R/utils_bayes_continuous.R
# Bayesian helper utilities — Continuous endpoints
# ======================================================

# Normal-Inverse-Gamma (NIG) conjugate prior utilities
# for mean and variance of a Normal distribution.

# ==============================================================================
# Bayesian Concept: The Normal-Inverse-Gamma (NIG) Distribution
# ==============================================================================

# NIG is the "Conjugate Prior" for a Normal distribution where
# BOTH the Mean (mu) and Variance (sigma^2) are unknown.

# -------------------------
# 1. Parameter Definitions
# -------------------------
# A NIG distribution is defined by four parameters: NIG(m, k, a, b)

# m (Location):
#   The prior mean. Your best guess for the center of the data.
# k (Scale/Virtual Sample Size):
#   The strength of your belief in 'm'. High k means 'm' is very stable.
# a (Shape):
#   Relates to the degrees of freedom of the variance information.
# b (Scale):
#   Relates to the "spread" or sum of squares of the variance.

# -------------------------
# 2. How it works (The Two-Step Logic)
# -------------------------
# The NIG is a joint distribution. To get a sample from it:
#   Step 1: Draw the variance (sigma^2) from an Inverse-Gamma(a, b).
#   Step 2: Draw the mean (mu) from a Normal(m, sigma^2/k).

# -------------------------
# 3. Simple Simulation Script
# -------------------------

# simulate_nig <- function(n_sim = 10000, m = 0, k = 10, a = 2, b = 1) {
#
#   # Step 1: Draw sigma^2 from Inverse-Gamma
#   # Note: rgamma draws from Gamma, so we take the reciprocal
#   sig2_draws <- 1 / rgamma(n_sim, shape = a, rate = b)
#
#   # Step 2: Draw mu from Normal, scaled by the precision k
#   mu_draws <- rnorm(n_sim, mean = m, sd = sqrt(sig2_draws / k))
#
#   return(data.frame(mu = mu_draws, sigma2 = sig2_draws))
# }

# # Run the simulation
# results <- simulate_nig(m = 10, k = 5, a = 3, b = 2)
#
# # Visualize
# par(mfrow = c(1, 2))
# hist(results$mu, main = "Posterior Mean (mu)", xlab = "Value", col = "skyblue")
# hist(results$sigma2, main = "Posterior Variance (sigma^2)", xlab = "Value", col = "salmon")

# -------------------------
# 4. Conjugate Update Rules
# -------------------------
# When you see new data (mean = y_bar, variance = s2, size = n),
# the parameters update instantly:
#
# k_new = k + n
# m_new = (k*m + n*y_bar) / k_new
# a_new = a + n/2
# b_new = b + 0.5*sum_sq + 0.5*(k*n/k_new)*(y_bar - m)^2



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

  # Safety: Ensure sum of squares is 0 if n=1 to avoid issues with s2
  ss_obs <- if (n > 1) 0.5 * (n - 1) * s2 else 0

  b_n <- prior$b0 +
    ss_obs +
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
  if (is.null(nig)) return(NULL)
  # Inverse-Gamma for variance, then Normal for mean
  s2 <- 1 / rgamma(nsim, nig$a0, nig$b0)
  rnorm(nsim, nig$m0, sqrt(s2 / nig$k0))
}

draw_sigma2_from_nig <- function(nig, nsim = 30000) {
  if (is.null(nig)) return(NULL)
  1 / rgamma(nsim, nig$a0, nig$b0)
}

# ------------------------------------------------------
# Delta Calculation
# ------------------------------------------------------
# Calculate Posterior Delta draws

cont_draw_delta <- function(post_ctrl, post_trt, S = 50000) {

  # Case A: Inputs are already numeric posterior draws (vectors)
  if (is.numeric(post_ctrl) && is.numeric(post_trt)) {
    return(post_trt - post_ctrl)
  }

  # Case B: Inputs are NIG parameter lists
  if (is.list(post_ctrl) && is.list(post_trt)) {
    mu_c <- draw_mu_from_nig(post_ctrl, nsim = S)
    mu_t <- draw_mu_from_nig(post_trt, nsim = S)
    return(mu_t - mu_c)
  }

  # Failure fallback
  stop("Invalid input types for cont_draw_delta. Expected NIG lists or numeric vectors.")
}
