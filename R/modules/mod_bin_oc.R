oc_ui <- function(id) {
  ns <- NS(id)
  tabItem(
    tabName = "oc",
    fluidRow(
      # Left Column: Input Settings
      box(width = 4, title = "Simulation Scenario (Truth)",
          status = "primary", solidHeader = TRUE,
          tags$p("Define underlying 'True' parameters to calculate Power (if Δ > 0) or Type I Error (if Δ = 0)."),

          numericInput(ns("n_ctrl"), "Control Sample Size (n_c)", value = 50, min = 1),
          numericInput(ns("n_trt"),  "Treatment Sample Size (n_t)", value = 100, min = 1),
          hr(),
          sliderInput(ns("p_c_true"), "True Control Rate (p_c)", min = 0, max = 1, value = 0.2, step = 0.05),
          sliderInput(ns("delta_true"), "True Effect Size (Δ = p_t - p_c)", min = -0.5, max = 0.5, value = 0.15, step = 0.05),
          hr(),
          h4("Simulation Control"),
          numericInput(ns("n_sim"), "Number of Trial Simulations", value = 500, min = 50, step = 50),

          actionButton(ns("run_oc"), "Run OC Analysis",
                       icon = icon("play"), class = "btn-success", width = "100%")
      ),

      # Right Column: Results
      box(width = 8, title = "Operating Characteristics Results",
          status = "primary", solidHeader = TRUE,

          # Result View
          conditionalPanel(
            condition = "output['oc_ready']",
            ns = ns,
            uiOutput(ns("oc_summary_ui")),
            tabsetPanel(
              tabPanel("Summary Plot", plotOutput(ns("oc_plot"), height = "350px") %>% withSpinner()),
              tabPanel("Detailed Stats", DTOutput(ns("oc_table")))
            )
          ),

          # Idle/Placeholder View
          conditionalPanel(
            condition = "!output['oc_ready']",
            ns = ns,
            div(style = "height: 450px; display: flex; align-items: center; justify-content: center; color: #999; flex-direction: column; text-align: center;",
                icon("vials", class = "fa-4x"),
                h4("Ready for Simulation"),
                p("Adjust the truth scenario and click 'Run OC Analysis'"),
                p(tags$small("Note: Simulations may take a few seconds depending on n_sim."))
            )
          )
      )
    )
  )
}

oc_server <- function(id, app_rv) {
  moduleServer(id, function(input, output, session) {
    ns <- session$ns

    # Local state management
    local_rv <- reactiveValues(
      ready = FALSE,
      results = NULL
    )

    # Export state to UI
    output$oc_ready <- reactive({ local_rv$ready })
    outputOptions(output, "oc_ready", suspendWhenHidden = FALSE)

    # Reset state if inputs change (Prevents showing stale data)
    observeEvent(list(input$n_ctrl, input$n_trt, input$p_c_true, input$delta_true, input$n_sim), {
      local_rv$ready <- FALSE
      local_rv$results <- NULL
    })

    # --- SIMULATION ENGINE ---
    observeEvent(input$run_oc, {

      # 1. GATEKEEPER VALIDATION
      if (is.null(app_rv$ctrl_post)) {
        showNotification("Prior missing: Build the Historical Prior first.", type = "warning")
        return()
      }
      if (is.null(app_rv$decision$delta_star)) {
        showNotification("Decision rules not set: Visit the 'Decision' tab first.", type = "warning")
        return()
      }

      # 2. SETUP
      n_c <- input$n_ctrl
      n_t <- input$n_trt
      p_c_true <- input$p_c_true
      p_t_true <- pmin(pmax(p_c_true + input$delta_true, 0), 1)
      n_sim <- input$n_sim

      delta_star <- app_rv$decision$delta_star
      p_cut <- app_rv$decision$p_cut
      ctrl_template <- app_rv$ctrl_post

      success_vec <- logical(n_sim)

      # 3. RUN SIMULATION
      tryCatch({
        withProgress(message = 'Simulating Trials...', value = 0, {
          for (i in 1:n_sim) {
            # Simulated data
            y_c_sim <- rbinom(1, n_c, p_c_true)
            y_t_sim <- rbinom(1, n_t, p_t_true)

            # Sample Posteriors (1000 samples per trial)
            # Use localized logic for speed
            p_c_draws <- if (ctrl_template$type == "rmap") {
              mix <- postmix_beta(ctrl_template$obj, y = y_c_sim, n = n_c)
              comp <- sample.int(length(mix$w), size = 1000, replace = TRUE, prob = mix$w)
              rbeta(1000, mix$a[comp], mix$b[comp])
            } else {
              rbeta(1000, ctrl_template$obj$a + y_c_sim, ctrl_template$obj$b + (n_c - y_c_sim))
            }

            p_t_draws <- rbeta(1000, y_t_sim + 1, n_t - y_t_sim + 1)

            # Rule: Posterior Prob of difference > threshold
            success_vec[i] <- mean((p_t_draws - p_c_draws) > delta_star) > p_cut

            if (i %% max(1, floor(n_sim/10)) == 0) incProgress(1/10)
          }
        })

        # 4. FINALIZING
        power_val <- mean(success_vec)

        local_rv$results <- list(
          power = power_val,
          n_sim = n_sim,
          p_c_true = p_c_true,
          p_t_true = p_t_true,
          delta_true = input$delta_true,
          se = sqrt((power_val * (1 - power_val)) / n_sim)
        )
        local_rv$ready <- TRUE

      }, error = function(e) {
        showNotification(paste("Simulation failed:", e$message), type = "error")
        local_rv$ready <- FALSE
      })
    })

    # --- OUTPUTS ---
    output$oc_summary_ui <- renderUI({
      res <- req(local_rv$results)
      is_type_i <- abs(res$delta_true) < 0.0001

      # Visual logic
      # For Power (delta > 0), green is high. For Type I (delta = 0), red is high.
      if (is_type_i) {
        color <- if(res$power <= 0.05) "#28a745" else "#dc3545"
        title <- "Type I Error Analysis (Null Scenario)"
        subtitle <- "Pr(False Positive)"
      } else {
        color <- if(res$power >= 0.8) "#28a745" else "#ffc107"
        title <- "Power Analysis (Alternative Scenario)"
        subtitle <- "Pr(Efficacy)"
      }

      tags$div(
        style = paste0("padding: 15px; border-left: 5px solid ", color, "; background: #f8f9fa; margin-bottom: 20px;"),
        tags$h4(title, style = "font-weight: bold;"),
        tags$p("Scenario: ", tags$b("p_c =", res$p_c_true * 100, "%"), " vs ", tags$b("p_t =", round(res$p_t_true * 100, 1), "%")),
        tags$h3(style = paste0("color: ", color, "; margin: 5px 0;"),
                paste0(round(res$power * 100, 1), "%"),
                tags$small(style="color: #666; font-size: 0.5em;", subtitle))
      )
    })

    output$oc_plot <- renderPlot({
      res <- req(local_rv$results)
      df <- data.frame(Result = c("Declare Efficacy", "Fail to Declare"),
                       Value = c(res$power, 1 - res$power))

      ggplot(df, aes(x = "", y = Value, fill = Result)) +
        geom_bar(stat = "identity", width = 1, color = "white") +
        coord_polar("y", start = 0) +
        theme_void() +
        scale_fill_manual(values = c("Declare Efficacy" = "#28a745", "Fail to Declare" = "#e9ecef")) +
        labs(title = paste0("Decision Results (n=", res$n_sim, " trials)")) +
        theme(legend.position = "bottom", plot.title = element_text(hjust = 0.5, face = "bold"))
    })

    output$oc_table <- renderDT({
      res <- req(local_rv$results)
      df <- data.frame(
        Parameter = c("Simulations Run", "True Control Rate", "True Treatment Rate", "True Delta", "Resulting Probability", "MC Std Error"),
        Value = c(res$n_sim, res$p_c_true, round(res$p_t_true, 4), res$delta_true,
                  paste0(round(res$power*100, 2), "%"), round(res$se, 4))
      )
      datatable(df, options = list(dom = 't'), rownames = FALSE)
    })
  })
}
