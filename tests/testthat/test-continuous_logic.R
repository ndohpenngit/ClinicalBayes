library(testthat)

test_that("NIG Prior from history and Posterior update work correctly", {
  # 1. Test Prior Construction: cont_prior_from_hist
  ybar <- c(10, 12)
  s2 <- c(2, 2)
  n <- c(50, 50)
  alpha <- 0.5

  prior <- cont_prior_from_hist(ybar, s2, n, alpha)

  expect_equal(prior$m0, 11) # Weighted mean of 10 and 12
  expect_equal(prior$k0, 50) # alpha * sum(n) = 0.5 * 100

  # 2. Test Posterior Update: cont_posterior
  ybar_c <- 15
  s2_c <- 3
  n_c <- 20

  post <- cont_posterior(prior, ybar_c, s2_c, n_c)

  # k_n = k_0 + n = 50 + 20
  expect_equal(post$k0, 70)

  # a_n = a_0 + n/2 = 2 + 10 = 12
  expect_equal(post$a0, 12)

  # Mean should be pulled toward the new data (15) from the prior (11)
  expect_gt(post$m0, 11)
  expect_lt(post$m0, 15)
})

test_that("NIG Sampling and Delta calculation handle NIG lists", {
  # Create two NIG objects
  ctrl <- list(m0=10, k0=10, a0=2, b0=2)
  trt  <- list(m0=12, k0=10, a0=2, b0=2)

  # Test cont_draw_delta using the NIG list logic (Case B)
  delta_draws <- cont_draw_delta(ctrl, trt, S = 1000)

  expect_length(delta_draws, 1000)
  expect_gt(mean(delta_draws), 0) # trt mean (12) > ctrl mean (10)
})
