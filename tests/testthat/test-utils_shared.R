test_that("%||% works correctly", {
  expect_equal(NULL %||% 1, 1)
  expect_equal(NA %||% 2, 2)
  expect_equal(3 %||% 2, 3)
})

test_that("get_current_hist_df resolves dataset", {
  app_rv <- reactiveValues(
    current_dataset = "A",
    datasets = list(A = data.frame(x = 1)),
    hist_df = data.frame(x = 2)
  )
  expect_equal(get_current_hist_df(app_rv)$x, 1)
})

test_that("nig_from_summaries returns valid NIG", {
  pri <- nig_from_summaries(
    mean_vec = c(5, 6),
    sd_vec   = c(2, 2),
    n_vec    = c(20, 30)
  )

  expect_true(pri$k0 > 0)
  expect_true(pri$a0 > 1)
  expect_true(pri$b0 > 0)
})
