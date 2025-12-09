generate_example_data <- function() {
  dir <- "data"
  if (!dir.exists(dir)) dir.create(dir, recursive = TRUE)

  # Binomial
  binom <- data.frame(
    subject_id = 1:200,
    outcome = rbinom(200, 1, 0.35)
  )
  write.csv(binom, file.path(dir, "binomial_example.csv"), row.names = FALSE)

  # Normal
  normal <- data.frame(
    subject_id = 1:200,
    value = rnorm(200, 50, 10)
  )
  write.csv(normal, file.path(dir, "normal_example.csv"), row.names = FALSE)

  # Time-to-event
  tte <- data.frame(
    subject_id = 1:300,
    time  = rexp(300, 0.2),
    event = rbinom(300, 1, 0.7)
  )

  write.csv(tte, file.path(dir, "tte_example.csv"), row.names = FALSE)

  message("Saved example datasets to data/")
}

generate_example_data()

