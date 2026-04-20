library(testthat)
library(rstan)

# Helper function to locate Stan files relative to the test file
get_stan_path <- function(filename) {
  # During dev, tests run from 'tests/testthat/',
  # so we go up two levels to reach the project root
  path <- file.path("../../stan", filename)

  if (!file.exists(path)) {
    # Fallback
    path <- file.path("stan", filename)
  }

  return(path)
}

# ---------------------------------------------------------
# Test A: Binary Commensurate (Logit Link)
# ---------------------------------------------------------
test_that("Binary Commensurate Stan samples logit_pc and tau", {
  skip_on_cran()

  stan_data <- list(
    y_t = 18, n_t = 40,
    y_c = 12, n_c = 40,
    y_h = 25, n_h = 100,
    tau_shape = 1.0, tau_rate = 1.0
  )

  # Compile and sample (using 1 chain/low iter for speed in CI)
  path <- get_stan_path("commensurate_binom.stan")
  expect_true(file.exists(path), info = paste("Stan file not found at:", path))

  fit <- rstan::stan(file = path, data = stan_data,
                     chains = 1, iter = 600, warmup = 300,
                     refresh = 0, cores = 1)

  draws <- rstan::extract(fit)

  # 1. Verify logit to probability transformation
  expect_equal(draws$pc[1], 1 / (1 + exp(-draws$logit_pc[1])), tolerance = 1e-5)

  # 2. Verify Generated Quantities (diff)
  expect_equal(draws$diff[1], draws$pt[1] - draws$pc[1], tolerance = 1e-5)

  # 3. Convergence Check
  fit_summary <- rstan::summary(fit)$summary

  # Ensure the row "tau" exists before checking Rhat
  expect_true("tau" %in% rownames(fit_summary))
  expect_true(fit_summary["tau", "Rhat"] < 1.1)
})

# ---------------------------------------------------------
# Test B: Continuous Commensurate (Precision Link)
# ---------------------------------------------------------
test_that("Continuous Commensurate Stan links mu_c to mu_h via tau", {
  skip_on_cran()

  stan_data_cont <- list(
    ybar_t = 15.0, s_t = 2.0, n_t = 30,
    ybar_c = 10.5, s_c = 1.8, n_c = 30,
    ybar_h = 10.0, s_h = 1.9, n_h = 80,
    tau_shape = 2.0, tau_rate = 0.1
  )

  path_cont <- get_stan_path("commensurate_cont.stan")
  expect_true(file.exists(path_cont))

  fit_cont <- rstan::stan(file = path_cont, data = stan_data_cont,
                          chains = 1, iter = 600, warmup = 300,
                          refresh = 0, cores = 1)

  draws_cont <- rstan::extract(fit_cont)

  # 1. Verify the link logic: mu_c should be influenced by mu_h
  # We expect mu_c and mu_h to be positively correlated in the posterior
  correlation <- cor(draws_cont$mu_c, draws_cont$mu_h)
  expect_gt(correlation, 0)

  # 2. Verify Generated Quantities (diff)
  expect_equal(draws_cont$diff[1], draws_cont$mu_t[1] - draws_cont$mu_c[1])

  # 3. Parameter Safety
  expect_true(all(draws_cont$tau > 0))
})
