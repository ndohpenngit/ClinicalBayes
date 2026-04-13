# ======================================================
# Binary Outcome: Decision Module
# ======================================================

decision_ui <- function(id) {
  ns <- NS(id)
  tabItem(
    tabName = "decision",
    fluidRow(
      # Left Column: Treatment Data & Decision Rules
      box(width = 4, title = "Treatment Data & Decision Rule",
          status = "primary", solidHeader = TRUE,

          h4("Current Trial: Treatment Arm"),
          numericInput(ns("trt_events"), "Observed Events (y_t)", value = 30, min = 0),
          numericInput(ns("trt_n"), "Treatment Total N (n_t)", value = 100, min = 1),

          hr(),
          h4("Decision Thresholds"),
          numericInput(ns("delta_star"), "Efficacy Margin (Δ*)", value = 0, step = 0.01),
          sliderInput(ns("p_cut"), "Posterior Threshold P(Δ > Δ*)",
                      min = 0.5, max = 0.99, value = 0.95, step = 0.01),

          actionButton(ns("run_decision"), "Compute Decision",
                       class = "btn-primary", width = "100%", icon = icon("bolt")),
          hr(),
          uiOutput(ns("decision_summary_ui"))
      ),

      # Right Column: Visual Analysis
      box(width = 8, title = "Posterior Analysis of Difference (Δ = p_t - p_c)",
          status = "primary", solidHeader = TRUE,
          plotOutput(ns("delta_plot"), height = "350px") %>% withSpinner(),
          hr(),
          tags$b("Interpretation:"),
          tags$p("The distribution shows the uncertainty in the treatment effect after incorporating historical control data and current trial results.")
      )
    )
  )
}

decision_server <- function(id, app_rv) {
  moduleServer(id, function(input, output, session) {
    ns <- session$ns

    decision_draws <- eventReactive(input$run_decision, {
      # 1. VALIDATION
      validate(
        need(!is.null(app_rv$ctrl_post), "Please build the Historical Prior in 'Data & Priors' first.")
      )

      # 2. SAMPLING
      # Control Sampling (From saved template)
      cp <- app_rv$ctrl_post
      n_samps <- 10000

      p_c <- if(cp$type == "rmap") {
        comp <- sample.int(length(cp$obj$w), n_samps, replace = TRUE, prob = cp$obj$w)
        rbeta(n_samps, cp$obj$a[comp], cp$obj$b[comp])
      } else {
        rbeta(n_samps, cp$obj$a, cp$obj$b)
      }

      # Treatment Sampling (Flat Prior: Beta(1,1))
      p_t <- rbeta(n_samps, input$trt_events + 1, input$trt_n - input$trt_events + 1)

      # 3. GLOBAL STATE SYNC
      # Store results for OC module and Reporting
      app_rv$decision <- list(
        delta_star = input$delta_star,
        p_cut = input$p_cut
      )

      app_rv$current_trt <- list(
        y = input$trt_events,
        n = input$trt_n
      )

      list(p_c = p_c, p_t = p_t, delta = p_t - p_c)
    })

    # Color-coded Decision Summary UI
    output$decision_summary_ui <- renderUI({
      d <- req(decision_draws())
      prob <- mean(d$delta > input$delta_star)
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
      df <- data.frame(delta = d$delta)

      ggplot(df, aes(x = delta)) +
        geom_density(fill = "#3c8dbc", alpha = 0.3, color = "#3c8dbc", lwd = 1) +
        geom_vline(xintercept = input$delta_star, color = "red", linetype = "dashed", lwd = 1.2) +
        annotate("text", x = input$delta_star, y = 0, label = " Margin (Δ*)",
                 hjust = -0.1, vjust = -1, color = "red", fontface = "bold") +
        theme_minimal() +
        labs(
          title = "Posterior Distribution of Treatment Effect",
          subtitle = paste0("True Margin (Δ*) = ", input$delta_star),
          x = expression(paste("Effect Size (", Delta, " = ", p[t], " - ", p[c], ")")),
          y = "Density"
        ) +
        scale_x_continuous(labels = scales::percent)
    })
  })
}
