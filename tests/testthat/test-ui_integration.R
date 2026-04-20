library(testthat)
library(shinytest2)

test_that("Full App Journey: From Upload to OC Results", {
  # Identify the app root
  app_path <- rprojroot::find_root(rprojroot::has_file("app.R"))

  # Build the path to the test data relative to the app root
  test_csv <- file.path(app_path, "data", "example_hist_binary.csv")

  app <- AppDriver$new(app_path, height = 900, width = 1400)

  # --- STEP 1: DATA UPLOAD ---
  # wait_ = FALSE prevents the 4-second warning on tab switches
  app$set_inputs(tabs = "data", wait_ = FALSE)
  app$wait_for_idle()

  app$upload_file(`data_1-hist_file` = test_csv)
  app$wait_for_idle()

  app$click("data_1-save_dataset")
  app$wait_for_idle()

  # --- STEP 2: BUILD PRIOR ---
  app$set_inputs(tabs = "binary", wait_ = FALSE)
  app$wait_for_idle()

  app$set_inputs(`binary_priors_1-curr_events` = 12, wait_ = FALSE)
  app$set_inputs(`binary_priors_1-curr_n` = 50, wait_ = FALSE)

  # Clicking the button triggers an output, so we don't need wait_ = FALSE here
  app$click("binary_priors_1-build_all")
  app$wait_for_idle()

  # --- STEP 3: SET DECISION RULE ---
  app$set_inputs(tabs = "decision", wait_ = FALSE)
  app$wait_for_idle()
  app$set_inputs(`decision_1-delta_star` = 0.05, wait_ = FALSE)
  app$set_inputs(`decision_1-p_cut` = 0.90, wait_ = FALSE)
  app$click("decision_1-run_decision")
  app$wait_for_idle()

  # --- STEP 4: RUN OC ANALYSIS ---
  set.seed(237)  # For reproducibility in testing

  app$set_inputs(tabs = "oc", wait_ = FALSE)
  app$wait_for_idle()
  app$set_inputs(`oc_1-n_sim` = 100, wait_ = FALSE)
  app$click("oc_1-run_oc")

  # --- STEP 5: VERIFICATION ---
  app$wait_for_value(output = "oc_1-oc_plot", timeout = 30000)

  app$expect_values(
    export = "oc_1-local_rv",
    screenshot_args = FALSE,
    transform = function(x) {
      # x is a character vector of JSON lines; collapse them into one string
      data <- jsonlite::fromJSON(paste(x, collapse = "\n"))

      # Reach into the nested list and scrub the random power value
      if (!is.null(data$export$`oc_1-local_rv`$power)) {
        data$export$`oc_1-local_rv`$power <- "RANDOM_VALIDATED"
      }

      # Convert back to JSON text for shinytest2 to save
      jsonlite::toJSON(data, auto_unbox = TRUE, pretty = TRUE)
    }
  )

  # Final snapshot
  # app$expect_values(output = "oc_1-oc_plot")

  app$stop()
})
