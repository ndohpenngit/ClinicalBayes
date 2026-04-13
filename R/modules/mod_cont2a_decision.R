# ======================================================
# Continuous Outcome: Decision Module
# ======================================================

cont2a_decision_ui <- function(id) {
  ns <- NS(id)
  tabItem(
    tabName = "cont2a_dec",
    fluidRow(
      # Left Column: Treatment Data & Decision Rules
      box(width = 4, title = "Treatment Data & Decision Rule",
          status = "primary", solidHeader = TRUE,

          h4("Current Trial: Treatment Arm"),
          numericInput(ns("mean_t"), "Observed Mean (y_t)", value = 0.5, step = 0.1),
          numericInput(ns("sd_t"), "Observed SD (s_t)", value = 1.0, min = 0.001),
          numericInput(ns("n_t"), "Treatment Total N (n_t)", value = 30, min = 2),

          hr(),
          h4("Decision Thresholds"),
          numericInput(ns("delta_star"), "Efficacy Margin (Δ*)", value = 0, step = 0.1),
          sliderInput(ns("p_cut"), "Posterior Threshold P(Δ > Δ*)",
                      min = 0.5, max = 0.99, value = 0.95, step = 0.01),

          actionButton(ns("run_decision"), "Compute Decision",
                       class = "btn-primary", width = "100%", icon = icon("bolt")),
          hr(),
          uiOutput(ns("decision_summary_ui"))
      ),

      # Right Column: Visual Analysis
      box(width = 8, title = "Posterior Analysis of Difference (Δ = μ_t - μ_c)",
          status = "primary", solidHeader = TRUE,
          plotOutput(ns("delta_plot"), height = "350px") %>% withSpinner(),
          hr(),
          tags$b("Interpretation:"),
          tags$p("The distribution shows the uncertainty in the treatment effect (difference in means) after incorporating historical control data and current trial results.")
      )
    )
  )
}

cont2a_decision_server <- function(id, app_rv) {
  moduleServer(id, function(input, output, session) {
    ns <- session$ns

    decision_draws <- eventReactive(input$run_decision, {
      # 1. VALIDATION
      validate(
        need(!is.null(app_rv$cont_ctrl_post), "Please build the Historical Prior in 'Data & Priors' first.")
      )

      # 2. SAMPLING
      n_samps <- 10000

      # Sample Control Posterior (assuming NIG distribution from previous module)
      # We use your helper function cont_draw_delta or manual sampling:
      post_c <- app_rv$cont_ctrl_post

      # Sample Treatment Posterior (Flat Prior: m0=0, k0=0.001, a0=2, b0=1)
      prior_trt_flat <- safe_nig(m0 = 0, k0 = 0.001, a0 = 2, b0 = 1)
      var_t <- (input$sd_t)^2
      post_t <- cont_posterior(prior_trt_flat, input$mean_t, var_t, input$n_t)

      # Generate Delta Samples (μ_t - μ_c)
      delta_samples <- cont_draw_delta(post_c, post_t, S = n_samps)

      # 3. GLOBAL STATE SYNC
      app_rv$cont_decision <- list(
        delta_star = input$delta_star,
        p_cut = input$p_cut
      )

      app_rv$cont_current_trt <- list(
        mean = input$mean_t,
        sd = input$sd_t,
        n = input$n_t
      )

      return(delta_samples)
    })

    # Color-coded Decision Summary UI
    output$decision_summary_ui <- renderUI({
      d <- req(decision_draws())
      prob <- mean(d > input$delta_star)
      success <- prob > input$p_cut

      color <- if(success) "#28a745" else "#dc3545"
      icon_name <- if(success) "check-circle" else "times-circle"

      tags$div(
        style = paste0("padding: 15px; border-left: 5px solid ", color, "; background: #f8f9fa;"),
        tags$h4("Final Decision", style = paste0("color: ", color, "; font-weight: bold;")),
        tags$p(tags$b("P(Δ > Δ*): "), round(prob, 4)),
        tags$h3(style = paste0("color: ", color, "; margin: 0;"),
                icon(icon_name),
                if(success) " Declare Efficacy" else " Fail to Declare")
      )
    })

    # ggplot2 Visual
    output$delta_plot <- renderPlot({
      d <- req(decision_draws())
      df <- data.frame(delta = d)

      ggplot(df, aes(x = delta)) +
        geom_density(fill = "#3c8dbc", alpha = 0.3, color = "#3c8dbc", lwd = 1) +
        geom_vline(xintercept = input$delta_star, color = "red", linetype = "dashed", lwd = 1.2) +
        annotate("text", x = input$delta_star, y = 0, label = " Margin (Δ*)",
                 hjust = -0.1, vjust = -1, color = "red", fontface = "bold") +
        theme_minimal() +
        labs(
          title = "Posterior Distribution of Mean Difference",
          subtitle = paste0("Decision Margin (Δ*) = ", input$delta_star),
          x = expression(paste("Effect Size (", Delta, " = ", mu[t], " - ", mu[c], ")")),
          y = "Density"
        )
    })
  })
}
