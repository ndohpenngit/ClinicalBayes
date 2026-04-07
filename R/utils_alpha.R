# ======================================================
# R/utils_alpha.R
# ESS-calibrated alpha utilities
# ======================================================

auto_alpha <- function(alpha, mean, var, n, max_ess_frac = 0.5) {
  alpha <- max(min(alpha, 1), 0)
  ess_target <- alpha * sum(n)
  max_ess <- max_ess_frac * sum(n)

  if (ess_target > max_ess) {
    alpha <- max_ess / sum(n)
  }
  alpha
}

alpha_grid_from_ess <- function(n, ess_grid) {
  ess_grid / sum(n)
}

# ======================================================
# Alpha calibration — Continuous endpoints
# ======================================================

calibrate_alpha_grid_cont <- function(ybar, s2, n,
                                      target_ess,
                                      alpha_grid = seq(0, 1, by = 0.01)) {

  res <- lapply(alpha_grid, function(a) {
    prior <- cont_prior_from_hist(ybar, s2, n, a)

    if (is.null(prior)) {
      return(c(alpha = a, ess = NA, dist = NA))
    }

    ess <- prior$k0
    c(alpha = a, ess = ess, dist = abs(ess - target_ess))
  })

  out <- as.data.frame(do.call(rbind, res))
  out
}
