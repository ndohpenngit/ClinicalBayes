# ======================================================
# Continuous 2-arm: Decision
# ======================================================

cont2a_decision_ui <- function(id) {
  ns <- NS(id)

  tabItem(
    tabName = "cont2a_dec",
    fluidRow(
      box(
        width = 4, title = "Decision rule",
        status = "primary", solidHeader = TRUE,
        numericInput(ns("delta_star"), "Δ*", value = 3),
        sliderInput(ns("p_cut"), "P(Δ > Δ*)", 0.5, 0.99, 0.95),
        actionButton(ns("run_dec"), "Run decision"),
        hr(),
        verbatimTextOutput(ns("decision_txt"))
      ),
      box(
        width = 8, title = "Δ diagnostics",
        status = "primary", solidHeader = TRUE,
        plotOutput(ns("delta_plot"), height = 260)
      )
    )
  )
}

cont2a_decision_server <- function(id, app_rv) {
  moduleServer(id, function(input, output, session) {

    delta_draws <- eventReactive(input$run_dec, {

      validate(
        need(!is.null(app_rv$cont_ctrl_post),
             "Run Continuous Data module first.")
      )

      cont_draw_delta(
        app_rv$cont_ctrl_post,
        app_rv$cont_trt_post,
        S = 50000
      )
    })

    output$delta_plot <- renderPlot({
      d <- delta_draws()

      hist(d, breaks = 60, freq = FALSE,
           main = "Posterior Δ = μ_t − μ_c", xlab = "Δ")
      abline(v = input$delta_star, col = "red", lwd = 2)
    })

    output$decision_txt <- renderText({

      d <- delta_draws()
      prob <- mean(d > input$delta_star)

      decision <- ifelse(prob > input$p_cut,
                         "Declare efficacy",
                         "No efficacy")

      app_rv$cont_decision <- list(
        prob = prob,
        delta_star = input$delta_star,
        p_cut = input$p_cut,
        decision = decision
      )

      paste0(
        "P(Δ > Δ*) = ", round(prob, 3), "\n",
        "Decision: ", decision
      )
    })
  })
}
