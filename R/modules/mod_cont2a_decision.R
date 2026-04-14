# ======================================================
# Continuous Outcome: Decision Module
# ======================================================

cont2a_decision_ui <- function(id) {
  ns <- NS(id)
  tabItem(
    tabName = "cont2a_dec",
    fluidRow(
      # Left Column: Input Settings
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

          # Only show summary box if ready
          conditionalPanel(
            condition = "output['decision_ready']",
            ns = ns,
            hr(),
            uiOutput(ns("decision_summary_ui"))
          )
      ),

      # Right Column: Visual Analysis (Conditional)
      box(width = 8, title = "Posterior Analysis of Difference (Δ = μ_t - μ_c)",
          status = "primary", solidHeader = TRUE,

          conditionalPanel(
            condition = "output['decision_ready']",
            ns = ns,
            plotOutput(ns("delta_plot"), height = "350px") %>% withSpinner(),
            hr(),
            tags$b("Interpretation:"),
            tags$p("The distribution shows the uncertainty in the treatment effect (difference in means) after incorporating historical control data and current trial results.")
          ),

          # Placeholder
          conditionalPanel(
            condition = "!output['decision_ready']",
            ns = ns,
            div(style = "height: 400px; display: flex; align-items: center; justify-content: center; color: #999; flex-direction: column;",
                icon("chart-line", class = "fa-4x"),
                h4("Analysis Ready"),
                p("Build the Continuous Prior first, then click 'Compute Decision' to view results.")
            )
          )
      )
    )
  )
}

cont2a_decision_server <- function(id, app_rv) {
  moduleServer(id, function(input, output, session) {
    ns <- session$ns

    # Local state for result tracking
    local_rv <- reactiveValues(
      ready = FALSE,
      delta_samples = NULL
    )

    # Reset result state if inputs change
    observeEvent(list(input$mean_t, input$sd_t, input$n_t, input$delta_star, input$p_cut), {
      local_rv$ready <- FALSE
      local_rv$delta_samples <- NULL
    })

    # Export state to UI
    output$decision_ready <- reactive({ local_rv$ready })
    outputOptions(output, "decision_ready", suspendWhenHidden = FALSE)

    # --- THE TRIGGER ---
    observeEvent(input$run_decision, {

      # 1. GATEKEEPER VALIDATION
      if (is.null(app_rv$hist_df) || app_rv$hist_type != "continuous") {
        showNotification("Data Mismatch: Please ensure a Continuous dataset is loaded.", type = "error")
        return()
      }
      if (is.null(app_rv$cont_ctrl_post)) {
        showNotification("Prior missing: Build the Historical Prior in 'Data & Priors' first.", type = "warning")
        return()
      }

      # 2. CALCULATION & SAMPLING
      tryCatch({
        n_samps <- 10000
        post_c <- app_rv$cont_ctrl_post

        # Sample Treatment Posterior (Flat Prior: m0=0, k0=0.001, a0=2, b0=1)
        prior_trt_flat <- safe_nig(m0 = 0, k0 = 0.001, a0 = 2, b0 = 1)
        var_t <- (input$sd_t)^2
        post_t <- cont_posterior(prior_trt_flat, input$mean_t, var_t, input$n_t)

        # Generate Delta Samples (μ_t - μ_c) using your helper
        samples_delta <- cont_draw_delta(post_c, post_t, S = n_samps)

        # 3. SAVE TO LOCAL AND GLOBAL STATE
        local_rv$delta_samples <- samples_delta

        app_rv$cont_decision <- list(delta_star = input$delta_star, p_cut = input$p_cut)
        app_rv$cont_current_trt <- list(mean = input$mean_t, sd = input$sd_t, n = input$n_t)

        # 4. TRIGGER UI DISPLAY
        local_rv$ready <- TRUE

      }, error = function(e) {
        showNotification(paste("Decision Error:", e$message), type = "error")
        local_rv$ready <- FALSE
      })
    })

    # --- OUTPUTS ---
    output$decision_summary_ui <- renderUI({
      req(local_rv$delta_samples)
      d <- local_rv$delta_samples

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

    output$delta_plot <- renderPlot({
      req(local_rv$delta_samples)
      df <- data.frame(delta = local_rv$delta_samples)

      # Calculate probability for the subtitle
      prob_val <- mean(df$delta > input$delta_star)

      # Generate density data for shading
      dens <- density(df$delta)
      df_dens <- data.frame(x = dens$x, y = dens$y)

      # Subset for shading the area greater than the margin
      df_success <- df_dens[df_dens$x > input$delta_star, ]

      ggplot(df, aes(x = delta)) +
        geom_density(fill = "grey90", color = "#3c8dbc", alpha = 0.5, lwd = 1) +
        geom_area(data = df_success, aes(x = x, y = y),
                  fill = "#28a745", alpha = 0.4) + # Success Area (Green)
        geom_vline(xintercept = input$delta_star, color = "red",
                   linetype = "dashed", lwd = 1.2) + # Decision Line

        annotate("label", x = input$delta_star, y = max(df_dens$y) * 0.9,
                 label = paste0("Margin (Δ*): ", input$delta_star),
                 color = "red", fontface = "bold", fill = "white", alpha = 0.8) +

        theme_minimal() +
        labs(
          title = "Posterior Distribution of Mean Difference",
          subtitle = paste0("Posterior Probability of Efficacy: ", round(prob_val * 100, 2), "%"),
          x = expression(paste("Effect Size (", Delta, " = ", mu[t], " - ", mu[c], ")")),
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
