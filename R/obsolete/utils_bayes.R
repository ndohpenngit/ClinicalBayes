# ======================================================
# R/utils_bayes.R
# Bayesian helper utilities — Binary endpoints
# ======================================================

# ---- rMAP mixture construction ----
make_hist_mix <- function(events, n, weight_by_n = TRUE) {
  stopifnot(length(events) == length(n))

  a <- events + 1
  b <- n - events + 1

  w <- if (weight_by_n) n / sum(n) else rep(1 / length(n), length(n))

  list(w = w, a = a, b = b)
}

# ---- Robustification: add vague Beta(1,1) ----
robustify_mix <- function(base_mix, robust_w = 0.1) {
  stopifnot(robust_w >= 0, robust_w <= 1)

  w <- c(base_mix$w * (1 - robust_w), robust_w)
  a <- c(base_mix$a, 1)
  b <- c(base_mix$b, 1)

  w <- w / sum(w)
  list(w = w, a = a, b = b)
}

# ---- Posterior mixture update ----
postmix_beta <- function(mix, y, n) {
  list(
    w = mix$w,
    a = mix$a + y,
    b = mix$b + (n - y)
  )
}

# ---- Power prior (Beta) ----
power_prior_beta <- function(events, n, alpha, a0 = 1, b0 = 1) {
  stopifnot(length(events) == length(n))
  list(
    a = a0 + alpha * sum(events),
    b = b0 + alpha * sum(n - events)
  )
}

# ======================================================
# Diagnostics
# ======================================================

summarize_mix <- function(mix, nsim = 50000) {
  comp <- sample.int(length(mix$w), nsim, replace = TRUE, prob = mix$w)
  draws <- rbeta(nsim, mix$a[comp], mix$b[comp])

  qs <- quantile(draws, c(0.025, 0.5, 0.975))
  list(mean = mean(draws), q2.5 = qs[1], q50 = qs[2], q97.5 = qs[3])
}

ess_mix <- function(mix, nsim = 30000) {
  comp <- sample.int(length(mix$w), nsim, replace = TRUE, prob = mix$w)
  draws <- rbeta(nsim, mix$a[comp], mix$b[comp])

  m <- mean(draws)
  v <- var(draws)

  if (v <= 0 || is.na(v)) return(0)
  m * (1 - m) / v - 1
}

ess_beta <- function(a, b) {
  a + b
}

# ======================================================
# Plotting helpers (PDF-safe)
# ======================================================

plot_density_mix <- function(mix, main = "", xlab = "") {
  comp <- sample.int(length(mix$w), 40000, replace = TRUE, prob = mix$w)
  draws <- rbeta(40000, mix$a[comp], mix$b[comp])

  dens <- density(draws, from = 0, to = 1, n = 512)

  par(mar = c(4, 4, 2, 1))
  plot(dens, lwd = 2, col = "black",
       main = main, xlab = xlab, ylab = "Density")
}

plot_density_beta <- function(a, b, main = "", xlab = "") {
  x <- seq(0, 1, length.out = 500)
  y <- dbeta(x, a, b)

  par(mar = c(4, 4, 2, 1))
  plot(x, y, type = "l", lwd = 2, col = "black",
       main = main, xlab = xlab, ylab = "Density")
}
