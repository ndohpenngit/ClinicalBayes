draw_mu_from_nig <- function(prior, S = 50000) {
  sigma2 <- 1 / stats::rgamma(S, prior$nu / 2, prior$nu * prior$s2 / 2)
  stats::rnorm(S, prior$mu, sqrt(sigma2 / prior$kappa))
}

draw_sigma2_from_nig <- function(prior, S = 50000) {
  1 / stats::rgamma(S, prior$nu / 2, prior$nu * prior$s2 / 2)
}

cont_draw_delta <- function(post_c, post_t, S = 50000) {
  mu_c <- draw_mu_from_nig(post_c, S)
  mu_t <- draw_mu_from_nig(post_t, S)
  mu_t - mu_c
}
