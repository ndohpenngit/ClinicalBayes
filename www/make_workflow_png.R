# Create workflow-diagram.png robustly
out_png <- "www/workflow-diagram.png"
dir.create("www", showWarnings = FALSE)

try_gg_fallback <- function() {
  if (!requireNamespace("ggplot2", quietly = TRUE)) {
    png(out_png, width = 1200, height = 400, res = 150)
    plot.new()
    text(.5, .5, "Workflow diagram\n(ggplot2 not installed)", cex = 1.4)
    dev.off()
    return(TRUE)
  }

  library(ggplot2)

  steps <- c(
    "Upload historical data",
    "Build priors (rMAP / power)",
    "Add current data",
    "Posterior computation",
    "Decision (Δ)",
    "Operating characteristics",
    "Generate report"
  )

  df <- data.frame(
    x = seq(0.1, 0.9, length.out = length(steps)),
    y = 0.5,
    label = steps
  )

  png(out_png, width = 1500, height = 330, res = 150)
  ggplot(df, aes(x, y)) +
    geom_rect(aes(
      xmin = x - 0.07, xmax = x + 0.07,
      ymin = y - 0.12, ymax = y + 0.12
    ),
    fill = "#E6E6E6", color = "#333") +
    geom_text(aes(label = label), size = 4) +
    theme_void() +
    xlim(0, 1) + ylim(0, 1)
  dev.off()

  TRUE
}

# Call fallback immediately (most robust)
try_gg_fallback()
