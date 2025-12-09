# ======================================================
# Shared Bayesian utilities
# ======================================================

# ---------- Beta-mixture (binary) ----------
mixbeta_normalize <- function(a, b, w) {
  w <- pmax(w, 1e-12); w <- w / sum(w)
  list(a = as.numeric(a), b = as.numeric(b), w = as.numeric(w))
}

dmixbeta <- function(x, mix) {
  comp <- mapply(function(a,b) dbeta(x,a,b), mix$a, mix$b)
  as.numeric(comp %*% matrix(mix$w, ncol = 1))
}

rmixbeta <- function(n, mix) {
  k <- length(mix$w)
  idx <- sample.int(k, size = n, replace = TRUE, prob = mix$w)
  rbeta(n, mix$a[idx], mix$b[idx])
}

postmix_beta <- function(mix, y, n) {
  mixbeta_normalize(a = mix$a + y, b = mix$b + (n - y), w = mix$w)
}

summarize_mix <- function(mix, probs = c(0.025,0.5,0.975), nsim = 50000) {
  s <- rmixbeta(nsim, mix)
  data.frame(
    mean = mean(s),
    sd   = sd(s),
    q2.5 = unname(quantile(s, probs[1])),
    q50  = unname(quantile(s, probs[2])),
    q97.5= unname(quantile(s, probs[3]))
  )
}

ess_beta <- function(a,b) a + b
ess_mix  <- function(mix) sum(mix$w * (mix$a + mix$b))

make_hist_mix <- function(events, n, weight_by_n = TRUE) {
  stopifnot(length(events) == length(n))
  a <- 1 + events
  b <- 1 + (n - events)
  w <- if (weight_by_n) n / sum(n) else rep(1/length(n), length(n))
  mixbeta_normalize(a,b,w)
}

robustify_mix <- function(base_mix, robust_w = 0.1) {
  robust_w <- min(max(robust_w,0),0.5)
  if (robust_w == 0) return(base_mix)
  a <- c(base_mix$a, 1)
  b <- c(base_mix$b, 1)
  w <- c((1-robust_w)*base_mix$w, robust_w)
  mixbeta_normalize(a,b,w)
}

power_prior_beta <- function(events, n, alpha, a0 = 1, b0 = 1) {
  alpha <- min(max(alpha,0),1)
  yH <- sum(events); nH <- sum(n)
  list(a = a0 + alpha*yH, b = b0 + alpha*(nH - yH))
}

plot_density_mix <- function(mix, title="Density", xlab="Rate") {
  xs <- seq(0.0001, 0.9999, length.out = 400)
  ys <- sapply(xs, dmixbeta, mix = mix)
  ggplot2::ggplot(data.frame(x=xs, y=ys), ggplot2::aes(x,y)) +
    ggplot2::geom_line(linewidth = 0.9) +
    ggplot2::labs(title = title, x = xlab, y = "Density") +
    ggplot2::theme_minimal(base_size = 12)
}

plot_density_beta <- function(a,b, title="Density", xlab="Rate") {
  xs <- seq(0.0001, 0.9999, length.out = 400)
  ys <- dbeta(xs, a, b)
  ggplot2::ggplot(data.frame(x=xs, y=ys), ggplot2::aes(x,y)) +
    ggplot2::geom_line(linewidth = 0.9) +
    ggplot2::labs(title = title, x = xlab, y = "Density") +
    ggplot2::theme_minimal(base_size = 12)
}

# ---------- Continuous (Normal endpoint, N-IG) ----------
cont_prior_from_hist <- function(ybar_h, s2_h, n_h, alpha,
                                 m0 = 0, k0 = 0.001, a0 = 0.001, b0 = 0.001) {
  alpha <- min(max(alpha,0),1)
  n_eff <- alpha * n_h
  kN <- k0 + n_eff
  mN <- (k0*m0 + n_eff*ybar_h)/kN
  aN <- a0 + n_eff/2
  bN <- b0 + 0.5*((n_eff-1)*s2_h + (k0*n_eff)*(ybar_h - m0)^2 / kN)
  list(m = mN, k = kN, a = aN, b = bN)
}

cont_posterior <- function(pri, ybar, s2, n) {
  kN <- pri$k + n
  mN <- (pri$k*pri$m + n*ybar)/kN
  aN <- pri$a + n/2
  bN <- pri$b + 0.5*((n-1)*s2 + (pri$k*n)*(ybar - pri$m)^2 / kN)
  list(m = mN, k = kN, a = aN, b = bN)
}

cont_draw_mu_sigma2 <- function(post, S = 50000) {
  sigma2 <- 1 / rgamma(S, shape = post$a, rate = post$b)
  mu <- rnorm(S, mean = post$m, sd = sqrt(sigma2/post$k))
  list(mu = mu, sigma2 = sigma2)
}

cont_draw_diff <- function(post_ctrl, post_trt, S = 50000) {
  dc <- cont_draw_mu_sigma2(post_ctrl, S)
  dt <- cont_draw_mu_sigma2(post_trt, S)
  list(delta_mu = dt$mu - dc$mu,
       mu_c = dc$mu, mu_t = dt$mu,
       sig2_c = dc$sigma2, sig2_t = dt$sigma2)
}

`%||%` <- function(x, y) if (is.null(x)) y else x
