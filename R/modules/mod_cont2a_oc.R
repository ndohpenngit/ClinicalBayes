# ======================================================
# Continuous 2-arm: Operating Characteristics (OC)
# ======================================================

cont2a_oc_ui <- function(id) {
  ns <- NS(id)
  tabItem(
    tabName = "cont2a_oc",
    fluidRow(
      # Left Column: Input Settings
      box(
        width = 4, title = "Simulation Scenario (Truth)",
        status = "primary", solidHeader = TRUE,
        tags$p("Define the 'True' state of the world to calculate power (Δ > 0) or Type I error (Δ = 0)."),

        numericInput(ns("mu_c_true"), "True Control Mean (μc)", value = 0),
        numericInput(ns("delta_true"), "True Effect Size (Δ)", value = 0.5),
        numericInput(ns("sd_true"), "True Common SD (σ)", value = 1, min = 0.01),

        hr(),
        h4("Simulation Control"),
        numericInput(ns("n_sim"), "Number of Simulations", value = 500, min = 10, max = 5000),
        actionButton(ns("run_oc"), "Run OC Analysis",
                     icon = icon("play"), class = "btn-success", width = "100%")
      ),

      # Right Column: Results
      box(
        width = 8, title = "Operating Characteristics Results",
        status = "primary", solidHeader = TRUE,
        conditionalPanel(
          condition = paste0("input['", ns("run_oc"), "'] == 0"),
          tags$div(style = "color: #777; padding: 50px; text-align: center;",
                   icon("calculator", class = "fa-4x"),
                   tags$h4("Ready to Simulate"),
                   tags$p("Ensure Priors and Decisions are set, then click 'Run OC Analysis'"))
        ),
        uiOutput(ns("oc_summary_ui")),
        tabsetPanel(
          tabPanel("Visual Summary", plotOutput(ns("oc_plot"), height = "350px") %>% withSpinner()),
          tabPanel("Detailed Statistics", verbatimTextOutput(ns("oc_txt")))
        )
      )
    )
  )
}

cont2a_oc_server <- function(id, app_rv) {
  moduleServer(id, function(input, output, session) {
    ns <- session$ns

    oc_data <- eventReactive(input$run_oc, {
      # 1. VALIDATION: Check global state for required trial parameters
      validate(
        need(!is.null(app_rv$cont_ctrl_prior), "Please build the Historical Prior in 'Data & Priors' first."),
        need(!is.null(app_rv$cont_decision), "Please compute a Decision rule in the 'Decision' tab first."),
        need(!is.null(app_rv$cont_current_ctrl$n), "Control sample size not found."),
        need(!is.null(app_rv$cont_current_trt$n), "Treatment sample size not found.")
      )

      # 2. CAPTURE INPUTS
      n_sim      <- req(input$n_sim)
      mu_c_true  <- input$mu_c_true
      delta_true <- input$delta_true
      sd_true    <- input$sd_true

      # Pull saved design from global app_rv
      pri_c      <- app_rv$cont_ctrl_prior
      delta_star <- app_rv$cont_decision$delta_star
      p_cut      <- app_rv$cont_decision$p_cut
      n_c        <- app_rv$cont_current_ctrl$n
      n_t        <- app_rv$cont_current_trt$n

      # Treatment Prior (Standard weak NIG prior)
      pri_t_flat <- safe_nig(m0 = 0, k0 = 0.001, a0 = 2, b0 = 1)

      # 3. SIMULATION LOOP
      success_vec <- logical(n_sim)

      withProgress(message = 'Running Monte Carlo Simulations...', value = 0, {
        for (i in 1:n_sim) {
          # Simulate "True" Trial Data based on inputs
          y_c_sim <- rnorm(n_c, mean = mu_c_true, sd = sd_true)
          y_t_sim <- rnorm(n_t, mean = mu_c_true + delta_true, sd = sd_true)

          # Calculate sample stats for the iteration
          v_c <- if(n_c > 1) var(y_c_sim) else sd_true^2
          v_t <- if(n_t > 1) var(y_t_sim) else sd_true^2

          # Update Posteriors for this specific trial iteration
          post_c <- cont_posterior(pri_c, mean(y_c_sim), v_c, n_c)
          post_t <- cont_posterior(pri_t_flat, mean(y_t_sim), v_t, n_t)

          # Calculate Probability of Efficacy
          d_draws <- cont_draw_delta(post_c, post_t, S = 1500)

          # Decision Check (Strict logical casting)
          success_vec[i] <- as.logical(mean(d_draws > delta_star) > p_cut)

          if (i %% 20 == 0) incProgress(20/n_sim)
        }
      })

      # 4. AGGREGATE RESULTS
      power_val <- mean(success_vec, na.rm = TRUE)
      list(
        power = power_val,
        n_sim = n_sim,
        delta = delta_true,
        se = sqrt((power_val * (1 - power_val)) / n_sim)
      )
    })

    # --- Outputs ---

    output$oc_summary_ui <- renderUI({
      res <- req(oc_data())
      color <- if(res$power >= 0.8) "#28a745" else "#ffc107"
      is_type_i <- abs(res$delta) < 0.0001

      tags$div(
        style = paste0("padding: 15px; border-left: 5px solid ", color, "; background: #f8f9fa; margin-bottom: 20px;"),
        tags$h4(if(is_type_i) "Type I Error Analysis" else "Power Analysis"),
        tags$h3(style = paste0("color: ", color, "; margin: 5px 0;"),
                paste0(round(res$power * 100, 1), "%"),
                tags$small(style="color: #666; font-size: 0.5em;", " Pr(Success)"))
      )
    })

    output$oc_plot <- renderPlot({
      res <- req(oc_data())
      df <- data.frame(category = "Success Rate", value = res$power)

      ggplot(df, aes(x = category, y = value)) +
        geom_bar(stat = "identity", fill = "#3c8dbc", width = 0.4) +
        geom_hline(yintercept = 0.8, linetype = "dashed", color = "red", size = 1) +
        scale_y_continuous(labels = scales::percent, limits = c(0, 1)) +
        theme_minimal() +
        labs(title = "Trial Success Probability",
             subtitle = paste("Based on", res$n_sim, "simulations"),
             x = "", y = "Probability")
    })

    output$oc_txt <- renderText({
      res <- req(oc_data())
      paste0(
        "Simulation Summary\n",
        "====================================\n",
        "Assumed True Delta:   ", res$delta, "\n",
        "Total Simulations:    ", res$n_sim, "\n",
        "Success Probability:  ", round(res$power * 100, 2), "%\n",
        "Monte Carlo SE:       ", round(res$se, 4), "\n",
        "===================================="
      )
    })
  })
}
