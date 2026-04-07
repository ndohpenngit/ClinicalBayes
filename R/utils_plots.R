# ======================================================
# R/utils_plots.R
# PDF-safe plotting utilities
# ======================================================

pdf_safe_par <- function() {
  par(
    mar = c(4, 4, 2, 1),
    mgp = c(2.5, 0.8, 0),
    cex = 0.9,
    lend = "round"
  )
}

plot_density_mix <- function(mix, main = "", xlab = "") {
  pdf_safe_par()

  comp <- sample.int(length(mix$w), 40000, replace = TRUE, prob = mix$w)
  draws <- rbeta(40000, mix$a[comp], mix$b[comp])

  dens <- density(draws, from = 0, to = 1, n = 512)
  plot(dens, lwd = 2, col = "black",
       main = main, xlab = xlab, ylab = "Density")
}

plot_density_beta <- function(a, b, main = "", xlab = "") {
  pdf_safe_par()

  x <- seq(0, 1, length.out = 500)
  plot(x, dbeta(x, a, b), type = "l", lwd = 2, col = "black",
       main = main, xlab = xlab, ylab = "Density")
}

plot_mu_overlay <- function(prior, post, main = "μ prior vs posterior") {
  pdf_safe_par()

  mu_p <- draw_mu_from_nig(prior, 30000)
  mu_q <- draw_mu_from_nig(post, 30000)

  d1 <- density(mu_p)
  d2 <- density(mu_q)

  plot(d1, lwd = 2, col = "black", main = main, xlab = "μ")
  lines(d2, lwd = 2, lty = 2)
  legend("topright", c("Prior", "Posterior"),
         lwd = 2, lty = c(1, 2), bty = "n")
}

# Sigma^2 prior vs posterior overlay

plot_sigma2_overlay <- function(prior, post,
                                main = expression(sigma^2~"prior vs posterior")) {
  pdf_safe_par()

  s2_p <- draw_sigma2_from_nig(prior, 30000)
  s2_q <- draw_sigma2_from_nig(post, 30000)

  d1 <- density(s2_p)
  d2 <- density(s2_q)

  plot(d1, lwd = 2, col = "black",
       main = main,
       xlab = expression(sigma^2))
  lines(d2, lwd = 2, lty = 2)

  legend("topright",
         c("Prior", "Posterior"),
         lwd = 2,
         lty = c(1, 2),
         bty = "n")
}


# Mu prior only plot
plot_mu_prior_only <- function(prior) {
  pdf_safe_par()

  draws <- draw_mu_from_nig(prior, 30000)
  d <- density(draws)

  plot(d, lwd = 2, col = "black",
       main = expression(mu~"(prior only)"),
       xlab = expression(mu))
}

# Sigma^2 prior only plot
plot_sigma2_prior_only <- function(prior) {
  pdf_safe_par()

  draws <- draw_sigma2_from_nig(prior, 30000)
  d <- density(draws)

  plot(d, lwd = 2, col = "black",
       main = expression(sigma^2~"(prior only)"),
       xlab = expression(sigma^2))
}

