# ======================================================
# Continuous 2-arm: Operating Characteristics (OC)
# ======================================================

cont2a_oc_ui <- function(id) {
  ns <- NS(id)
  tabItem(
    tabName = "cont2a_oc",
    fluidRow(
      # Left Column: Simulation Parameters
      box(width = 4, title = "Simulation Scenario (Truth)",
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
      box(width = 8, title = "Operating Characteristics Results",
          status = "primary", solidHeader = TRUE,

          # Results Display
          conditionalPanel(
            condition = "output['oc_ready']",
            ns = ns,
            uiOutput(ns("oc_summary_ui")),
            tabsetPanel(
              tabPanel("Summary Plot", plotOutput(ns("oc_plot"), height = "350px") %>% withSpinner()),
              tabPanel("Detailed Stats", DTOutput(ns("oc_table")) %>% withSpinner())
            )
          ),

          # Placeholder
          conditionalPanel(
            condition = "!output['oc_ready']",
            ns = ns,
            div(style = "height: 450px; display: flex; align-items: center; justify-content: center; color: #999; flex-direction: column; text-align: center;",
                icon("flask", class = "fa-4x"),
                h4("Simulator Ready"),
                p("Adjust the truth scenario and click 'Run OC Analysis' to begin Monte Carlo simulation.")
            )
          )
      )
    )
  )
}

cont2a_oc_server <- function(id, app_rv) {
  moduleServer(id, function(input, output, session) {
    ns <- session$ns

    # Local state management
    local_rv <- reactiveValues(
      ready = FALSE,
      results = NULL
    )

    # Export state for UI conditionalPanel
    output$oc_ready <- reactive({ local_rv$ready })
    outputOptions(output, "oc_ready", suspendWhenHidden = FALSE)

    # Reset results if inputs change
    observeEvent(list(input$mu_c_true, input$delta_true, input$sd_true, input$n_sim), {
      local_rv$ready <- FALSE
      local_rv$results <- NULL
    })

    # --- SIMULATION ENGINE ---
    observeEvent(input$run_oc, {

      # 1. GATEKEEPER VALIDATION
      validate(
        need(!is.null(app_rv$hist_type) && app_rv$hist_type == "continuous",
             "Error: Continuous dataset required. Check 'Data' tab."),
        need(!is.null(app_rv$cont_ctrl_prior),
             "Prior missing: Build the Historical Prior first."),
        need(!is.null(app_rv$cont_decision),
             "Decision rule missing: Configure the 'Decision' tab first.")
      )

      # 2. CAPTURE INPUTS
      n_sim      <- input$n_sim
      mu_c_true  <- input$mu_c_true
      delta_true <- input$delta_true
      sd_true    <- input$sd_true

      pri_c      <- app_rv$cont_ctrl_prior
      delta_star <- app_rv$cont_decision$delta_star
      p_cut      <- app_rv$cont_decision$p_cut
      n_c        <- app_rv$cont_current_ctrl$n
      n_t        <- app_rv$cont_current_trt$n

      pri_t_flat <- safe_nig(m0 = 0, k0 = 0.001, a0 = 2, b0 = 1)
      success_vec <- logical(n_sim)

      # 3. RUN SIMULATION
      tryCatch({
        withProgress(message = 'Simulating Continuous Trials...', value = 0, {
          for (i in 1:n_sim) {
            y_c_sim <- rnorm(n_c, mean = mu_c_true, sd = sd_true)
            y_t_sim <- rnorm(n_t, mean = mu_c_true + delta_true, sd = sd_true)

            v_c <- if(n_c > 1) var(y_c_sim) else sd_true^2
            v_t <- if(n_t > 1) var(y_t_sim) else sd_true^2

            post_c <- cont_posterior(pri_c, mean(y_c_sim), v_c, n_c)
            post_t <- cont_posterior(pri_t_flat, mean(y_t_sim), v_t, n_t)

            d_draws <- cont_draw_delta(post_c, post_t, S = 1000)
            success_vec[i] <- mean(d_draws > delta_star) > p_cut

            if (i %% max(1, floor(n_sim/10)) == 0) incProgress(1/10)
          }
        })

        # 4. STORE RESULTS
        power_val <- mean(success_vec)
        local_rv$results <- list(
          power = power_val,
          n_sim = n_sim,
          delta = delta_true,
          mu_c = mu_c_true,
          sd = sd_true,
          se = sqrt((power_val * (1 - power_val)) / n_sim)
        )

        app_rv$oc_cont_results <- local_rv$results
        local_rv$ready <- TRUE

      }, error = function(e) {
        showNotification(paste("Simulation failed:", e$message), type = "error")
        local_rv$ready <- FALSE
      })
    })

    # --- OUTPUTS ---
    output$oc_summary_ui <- renderUI({
      res <- req(local_rv$results)
      is_type_i <- abs(res$delta) < 0.0001
      color <- if(is_type_i) {
        if(res$power <= 0.05) "#28a745" else "#dc3545"
      } else {
        if(res$power >= 0.8) "#28a745" else "#ffc107"
      }

      tags$div(
        style = paste0("padding: 15px; border-left: 5px solid ", color, "; background: #f8f9fa; margin-bottom: 20px;"),
        tags$h4(if(is_type_i) "Type I Error Analysis" else "Power Analysis", style = "font-weight: bold;"),
        tags$h3(style = paste0("color: ", color, "; margin: 5px 0;"),
                paste0(round(res$power * 100, 1), "%"),
                tags$small(style="color: #666; font-size: 0.5em;",
                           if(is_type_i) " Pr(False Positive)" else " Pr(Efficacy Success)"))
      )
    })

    output$oc_plot <- renderPlot({
      res <- req(local_rv$results)
      df <- data.frame(Result = c("Success", "Failure"),
                       Value = c(res$power, 1 - res$power))

      ggplot(df, aes(x = "", y = Value, fill = Result)) +
        geom_bar(stat = "identity", width = 1, color = "white") +
        coord_polar("y", start = 0) +
        scale_fill_manual(values = c("Success" = "#28a745", "Failure" = "#e9ecef")) +
        theme_void() +
        labs(title = "Simulated Trial Outcomes") +
        theme(plot.title = element_text(hjust = 0.5, face = "bold"), legend.position = "bottom")
    })

    output$oc_table <- renderDT({
      res <- req(local_rv$results)

      df <- data.frame(
        Metric = c("Assumed True Control Mean", "Assumed True Delta", "Assumed True SD",
                   "Total Simulations", "Probability of Success", "Monte Carlo Std Error"),
        Value = c(res$mu_c, res$delta, res$sd,
                  res$n_sim, paste0(round(res$power * 100, 2), "%"), round(res$se, 4))
      )

      datatable(df, rownames = FALSE, options = list(dom = 't', ordering = FALSE))
    })
  })
}
