# ======================================================
# ESS-calibrated alpha grid (continuous NIG)
# ======================================================

ess_nig_prior <- function(prior) {
  2 * prior$k0
}

calibrate_alpha_grid <- function(
    mean_vec, sd_vec, n_vec,
    alpha_grid = seq(0, 1, by = 0.1),
    target_ess = NULL
) {
  out <- lapply(alpha_grid, function(a) {
    pri <- nig_from_summaries(
      mean_vec, sd_vec, n_vec,
      prior_strength = a
    )
    data.frame(
      alpha = a,
      ess   = ess_nig_prior(pri)
    )
  })
  do.call(rbind, out)
}
