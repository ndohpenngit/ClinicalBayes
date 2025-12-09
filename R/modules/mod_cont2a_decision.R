# ======================================================
# mod_cont2a_decision.R
# Continuous 2-arm: posterior decision on Δ
# ======================================================

cont2a_decision_ui <- function(id) {
  ns <- NS(id)
  tabItem(
    tabName = "cont2a_dec",
    fluidRow(
      box(
        width = 4,
        title = tagList(
          "Continuous Decision (Δ)",
          info_link("cont2a-decision")
        ),
        status = "primary",
        solidHeader = TRUE,
        numericInput(ns("delta_star"), "Decision margin Δ* (μ_t − μ_c)",
                     value = 3, step = 0.1),
        sliderInput(ns("p_cut"), "Posterior threshold P(Δ > Δ*)",
                    min = 0.5, max = 0.99, value = 0.95, step = 0.01),
        actionButton(ns("run_cont_dec"), "Compute Decision")
      ),
      box(
        width = 8,
        title = "Posterior for Δ = μ_t − μ_c",
        status = "primary",
        solidHeader = TRUE,
        plotOutput(ns("delta_plot"), height = 260) %>% withSpinner(),
        verbatimTextOutput(ns("delta_summary"))
      )
    )
  )
}

cont2a_decision_server <- function(id, app_rv) {
  moduleServer(id, function(input, output, session) {
    ns <- session$ns

    cont_decision_draws <- eventReactive(input$run_cont_dec, {
      validate(
        need(!is.null(app_rv$cont_ctrl_post) && !is.null(app_rv$cont_trt_post),
             "Please update continuous priors/posteriors in 'Continuous 2-arm: Data' first.")
      )
      draws <- cont_draw_diff(app_rv$cont_ctrl_post, app_rv$cont_trt_post, S = 4000)
      draws$delta_mu
    })

    output$delta_plot <- renderPlot({
      delta <- cont_decision_draws()
      hist(delta, breaks = 60, freq = FALSE,
           main = "Posterior of Δ = μ_t − μ_c",
           xlab = "Δ", ylab = "Density")
      abline(v = input$delta_star, col = "red", lwd = 2, lty = 2)
    })

    output$delta_summary <- renderText({
      delta <- cont_decision_draws()
      ds <- quantile(delta, c(0.025, 0.5, 0.975))
      mean_delta <- mean(delta)
      prob <- mean(delta > input$delta_star)
      decision <- ifelse(prob > input$p_cut, "Declare efficacy", "Do NOT declare efficacy")

      app_rv$cont_decision <- list(
        delta      = delta,
        mean       = mean_delta,
        q2.5       = ds[1],
        q50        = ds[2],
        q97.5      = ds[3],
        prob       = prob,
        delta_star = input$delta_star,
        p_cut      = input$p_cut,
        decision   = decision
      )

      paste0(
        "Posterior mean Δ: ", round(mean_delta, 4), "\n",
        "95% CrI for Δ: [", round(ds[1], 4), ", ", round(ds[3], 4), "]\n",
        "P(Δ > Δ* = ", input$delta_star, ") = ", round(prob, 4), "\n",
        "Decision: ", decision
      )
    })
  })
}
