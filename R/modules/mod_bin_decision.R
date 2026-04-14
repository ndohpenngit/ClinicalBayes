# ======================================================
# Binary Outcome: Decision Module
# ======================================================

decision_ui <- function(id) {
  ns <- NS(id)
  tabItem(
    tabName = "decision",
    fluidRow(
      # Left Column: Input Settings
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

          # Only show summary box if ready
          conditionalPanel(
            condition = "output['decision_ready']",
            ns = ns,
            hr(),
            uiOutput(ns("decision_summary_ui"))
          )
      ),

      # Right Column: Visual Analysis (Conditional)
      box(width = 8, title = "Posterior Analysis of Difference (Δ = p_t - p_c)",
          status = "primary", solidHeader = TRUE,

          conditionalPanel(
            condition = "output['decision_ready']",
            ns = ns,
            plotOutput(ns("delta_plot"), height = "350px") %>% withSpinner(),
            hr(),
            tags$b("Interpretation:"),
            tags$p("The distribution shows the uncertainty in the treatment effect after incorporating historical control data and current trial results.")
          ),

          # Placeholder
          conditionalPanel(
            condition = "!output['decision_ready']",
            ns = ns,
            div(style = "height: 400px; display: flex; align-items: center; justify-content: center; color: #999; flex-direction: column;",
                icon("chart-area", class = "fa-4x"),
                h4("Ready to Analyze"),
                p("Ensure Control Data is built, then click 'Compute Decision'")
            )
          )
      )
    )
  )
}

decision_server <- function(id, app_rv) {
  moduleServer(id, function(input, output, session) {
    ns <- session$ns

    local_rv <- reactiveValues(
      ready = FALSE,
      results = NULL  # Store draws here instead of eventReactive
    )

    # Reset state if inputs change
    observeEvent(list(input$trt_events, input$trt_n, input$delta_star, input$p_cut), {
      local_rv$ready <- FALSE
      local_rv$results <- NULL
    })

    output$decision_ready <- reactive({ local_rv$ready })
    outputOptions(output, "decision_ready", suspendWhenHidden = FALSE)

    # --- THE TRIGGER ---
    observeEvent(input$run_decision, {

      # 1. GATEKEEPER VALIDATION
      if (is.null(app_rv$hist_df) || app_rv$hist_type != "binary") {
        showNotification("Invalid data type or missing dataset.", type = "error")
        return()
      }
      if (is.null(app_rv$ctrl_post)) {
        showNotification("Prior missing: Build the Historical Prior first.", type = "warning")
        return()
      }

      # 2. CALCULATION
      tryCatch({
        cp <- app_rv$ctrl_post
        n_samps <- 10000

        # Control Sampling
        p_c <- if(cp$type == "rmap") {
          comp <- sample.int(length(cp$obj$w), n_samps, replace = TRUE, prob = cp$obj$w)
          rbeta(n_samps, cp$obj$a[comp], cp$obj$b[comp])
        } else {
          rbeta(n_samps, cp$obj$a, cp$obj$b)
        }

        # Treatment Sampling
        p_t <- rbeta(n_samps, input$trt_events + 1, input$trt_n - input$trt_events + 1)

        # 3. SAVE TO LOCAL AND GLOBAL STATE
        local_rv$results <- list(p_c = p_c, p_t = p_t, delta = p_t - p_c)

        app_rv$decision <- list(delta_star = input$delta_star, p_cut = input$p_cut)
        app_rv$current_trt <- list(y = input$trt_events, n = input$trt_n)

        # 4. TRIGGER UI DISPLAY
        local_rv$ready <- TRUE

      }, error = function(e) {
        showNotification(paste("Decision Error:", e$message), type = "error")
        local_rv$ready <- FALSE
      })
    })

    # --- OUTPUTS ---
    # Now these depend on local_rv$results instead of an eventReactive
    output$decision_summary_ui <- renderUI({
      req(local_rv$results)
      d <- local_rv$results

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

    output$delta_plot <- renderPlot({
      req(local_rv$results)
      df <- data.frame(delta = local_rv$results$delta)

      prob_val <- mean(df$delta > input$delta_star) # probability for the subtitle

      # Create a subset for the "Success Area" shading
      dens <- density(df$delta)
      df_dens <- data.frame(x = dens$x, y = dens$y)
      df_success <- df_dens[df_dens$x > input$delta_star, ]

      ggplot(df, aes(x = delta)) +
        geom_density(fill = "grey90", color = "#3c8dbc", alpha = 0.5, lwd = 1) +
        geom_area(data = df_success, aes(x = x, y = y),
                  fill = "#28a745", alpha = 0.4) + # Success Shading
        geom_vline(xintercept = input$delta_star, color = "red", # Decision Line
                   linetype = "dashed", lwd = 1.2) +

        annotate("label", x = input$delta_star, y = max(df_dens$y) * 0.9,
                 label = paste0("Margin: ", input$delta_star),
                 color = "red", fontface = "bold", fill = "white", alpha = 0.8) +

        theme_minimal() +
        scale_x_continuous(labels = scales::percent) +
        labs(
          title = "Posterior Distribution of Rate Difference",
          subtitle = paste0("Posterior Probability of Efficacy: ", round(prob_val * 100, 2), "%"),
          x = expression(paste("Effect Size (", Delta, " = ", p[t], " - ", p[c], ")")),
          y = "Posterior Density"
        ) +
        theme(
          plot.title = element_text(face = "bold", size = 16),
          subtitle = element_text(size = 12, color = "#444"),
          axis.title = element_text(face = "italic"),
          panel.grid.minor = element_blank()
        )
    })
  })
}
