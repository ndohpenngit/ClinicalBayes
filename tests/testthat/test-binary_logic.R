library(testthat)

test_that("Binary rMAP mixture is constructed and updated correctly", {
  # 1. Test Construction: make_hist_mix
  events <- c(10, 20)
  n <- c(50, 100)
  base_mix <- make_hist_mix(events, n, weight_by_n = TRUE)

  expect_equal(length(base_mix$w), 2)
  expect_equal(base_mix$w[1], 50/150) # Sample size weighting

  # 2. Test Robustification: robustify_mix
  rob_mix <- robustify_mix(base_mix, robust_w = 0.1)
  expect_equal(length(rob_mix$w), 3) # Original 2 + 1 robust component
  expect_equal(tail(rob_mix$a, 1), 1) # Robust a=1
  expect_equal(tail(rob_mix$b, 1), 1) # Robust b=1

  # 3. Test Posterior Update: postmix_beta
  y_curr <- 5
  n_curr <- 20
  post_mix <- postmix_beta(rob_mix, y_curr, n_curr)

  # Check one component update: a_new = a_old + y
  expect_equal(post_mix$a[1], base_mix$a[1] + y_curr)
  expect_equal(post_mix$b[1], base_mix$b[1] + (n_curr - y_curr))
})

test_that("Binary Power Prior follows alpha discounting", {
  events <- c(10, 20)
  n <- c(50, 100)
  alpha <- 0.5

  # Power prior calculation
  pp <- power_prior_beta(events, n, alpha)

  # Expected a = a0 + alpha * sum(events) = 1 + 0.5 * 30 = 16
  expect_equal(pp$a, 16)
  # Expected b = b0 + alpha * sum(n - events) = 1 + 0.5 * 120 = 61
  expect_equal(pp$b, 61)
})
