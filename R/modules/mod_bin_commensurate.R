# ======================================================
# Module: Binary Commensurate Prior (RStan)
# ======================================================

comm_ui <- function(id) {
  ns <- NS(id)
  tabItem(
    tabName = "comm",
    fluidRow(
      # Configuration Box
      box(width = 12, status = "primary", solidHeader = TRUE,
          title = "Binary Commensurate Prior Management",
          column(5,
                 wellPanel(
                   h4("Current Trial Data"),
                   fluidRow(
                     column(6,
                            h5(tags$b("Control Arm")),
                            numericInput(ns("y_c"), "Events (y_c)", value = 20, min = 0),
                            numericInput(ns("n_c"), "Total N (n_c)", value = 100, min = 1)
                     ),
                     column(6,
                            h5(tags$b("Treatment Arm")),
                            numericInput(ns("y_t"), "Events (y_t)", value = 30, min = 0),
                            numericInput(ns("n_t"), "Total N (n_t)", value = 100, min = 1)
                     )
                   )
                 )),
          column(7,
                 h4("Historical Data & Commensurability"),
                 fluidRow(
                   column(6,
                          h5(tags$b("Historical Control")),
                          numericInput(ns("y_h"), "Events (y_h)", value = 45, min = 0),
                          numericInput(ns("n_h"), "Total N (n_h)", value = 220, min = 1)
                   ),
                   column(6,
                          h5(tags$b("Hyperprior: τ ~ Gamma")),
                          numericInput(ns("tau_shape"), "Shape", value = 2, min = 0.1, step = 0.1),
                          numericInput(ns("tau_rate"),  "Rate",  value = 0.5, min = 0.1, step = 0.1)
                   )
                 ),
                 hr(),
                 actionButton(ns("fit"), "Fit Commensurate Model (MCMC)",
                              class = "btn-success", icon = icon("gears"), width = "100%")
          )
      )
    ),

    # --- CONDITIONAL RESULTS AREA ---
    conditionalPanel(
      condition = "output['comm_ready']",
      ns = ns,

      fluidRow(
        valueBoxOutput(ns("mean_delta_box"), width = 6),
        valueBoxOutput(ns("mean_tau_box"), width = 6)
      ),

      fluidRow(
        box(width = 6, title = "Treatment Effect (Δ)", status = "info", solidHeader = TRUE,
            tabsetPanel(
              tabPanel("Density", plotOutput(ns("delta_plot"), height = "280px") %>% withSpinner()),
              tabPanel("Diagnostics", plotOutput(ns("delta_trace"), height = "280px"))
            ),
            hr(),
            tags$b("Summary Statistics:"),
            verbatimTextOutput(ns("delta_txt"))
        ),
        box(width = 6, title = "Commensurability (τ)", status = "success", solidHeader = TRUE,
            tabsetPanel(
              tabPanel("Density", plotOutput(ns("tau_plot"), height = "280px") %>% withSpinner()),
              tabPanel("Diagnostics", plotOutput(ns("tau_trace"), height = "280px"))
            ),
            hr(),
            tags$b("Summary Statistics:"),
            verbatimTextOutput(ns("tau_txt"))
        )
      )
    ),

    # Placeholder
    conditionalPanel(
      condition = "!output['comm_ready']",
      ns = ns,
      column(12, align = "center",
             div(style = "padding: 50px; color: #999;",
                 icon("microchip", class = "fa-3x"),
                 h4("MCMC Model Idle"),
                 p("Configure trial data and hyperpriors, then click 'Fit Model' to start sampling.")
             )
      )
    )
  )
}

comm_server <- function(id, app_rv) {
  moduleServer(id, function(input, output, session) {
    ns <- session$ns

    # Local state for MCMC
    local_rv <- reactiveValues(
      draws = NULL,
      ready = FALSE
    )

    # Clean UI on input change
    observeEvent(list(input$y_c, input$y_t, input$y_h, input$n_c, input$n_t, input$n_h, input$tau_shape, input$tau_rate), {
      local_rv$draws <- NULL
      local_rv$ready <- FALSE
    })

    # Export state to UI
    output$comm_ready <- reactive({ local_rv$ready })
    outputOptions(output, "comm_ready", suspendWhenHidden = FALSE)

    # Fit MCMC Logic
    observeEvent(input$fit, {

      # 1. GATEKEEPER VALIDATION
      # Check if binary data was actually the intent/source
      if (!is.null(app_rv$hist_type) && app_rv$hist_type != "binary") {
        showNotification("Data Type Mismatch: Commensurate model is currently configured for Binary data.", type = "error")
        return()
      }

      library(rstan)
      library(posterior)
      library(bayesplot)

      stan_path <- "stan/commensurate_binom.stan"
      if (!file.exists(stan_path)) {
        showNotification("Stan model file not found in 'stan/' directory.", type = "error")
        return()
      }

      # 2. PROCEED WITH SAMPLING
      tryCatch({
        data_list <- list(
          y_t = input$y_t, n_t = input$n_t,
          y_c = input$y_c, n_c = input$n_c,
          y_h = input$y_h, n_h = input$n_h,
          tau_shape = input$tau_shape,
          tau_rate  = input$tau_rate
        )

        withProgress(message = 'Running MCMC Sampling...', value = 0.5, {
          fit <- stan(file = stan_path, data = data_list,
                      chains = 4, iter = 2000, warmup = 1000,
                      cores = parallel::detectCores(), seed = 123)

          # Convert and Store
          draws <- as_draws_df(fit)
          local_rv$draws <- draws
          local_rv$ready <- TRUE

          # Store for global use
          app_rv$comm <- list(
            draws = draws,
            summary = list(
              delta_mean = mean(draws$diff),
              delta_q = quantile(draws$diff, c(0.025, 0.975)),
              tau_mean = mean(draws$tau),
              tau_q = quantile(draws$tau, c(0.025, 0.975))
            )
          )
        })
        showNotification("MCMC sampling complete.", type = "message")

      }, error = function(e) {
        showNotification(paste("Stan Error:", e$message), type = "error")
        local_rv$ready <- FALSE
      })
    })

    # --- Value Boxes ---
    output$mean_delta_box <- renderValueBox({
      req(local_rv$draws)
      val <- mean(local_rv$draws$diff)
      valueBox(round(val, 4), "Mean Treatment Effect (Δ)", icon = icon("balance-scale"), color = "blue")
    })

    output$mean_tau_box <- renderValueBox({
      req(local_rv$draws)
      val <- mean(local_rv$draws$tau)
      valueBox(round(val, 2), "Mean Commensurability (τ)", icon = icon("link"), color = "green")
    })

    # --- Plots & Summaries ---
    # Density Plots ---
    output$delta_plot <- renderPlot({
      req(local_rv$draws)
      mcmc_dens(local_rv$draws, pars = "diff") +
        theme_minimal() +
        labs(title = "Posterior Density of Δ", subtitle = "Difference in proportions (p_t - p_c)")
    })

    output$tau_plot <- renderPlot({
      req(local_rv$draws)
      mcmc_dens(local_rv$draws, pars = "tau") +
        theme_minimal() +
        labs(title = "Posterior Density of τ", subtitle = "Precision of historical borrow")
    })

    # Trace Plots (Diagnostics) ---
    output$delta_trace <- renderPlot({
      req(local_rv$draws)
      mcmc_trace(local_rv$draws, pars = "diff") +
        theme_minimal() +
        labs(title = "Trace Plot: Δ", subtitle = "Check for chain convergence and mixing")
    })

    output$tau_trace <- renderPlot({
      req(local_rv$draws)
      mcmc_trace(local_rv$draws, pars = "tau") +
        theme_minimal() +
        labs(title = "Trace Plot: τ", subtitle = "Check for chain convergence and mixing")
    })

    output$delta_txt <- renderText({
      req(app_rv$comm)
      s <- app_rv$comm$summary
      paste0("Δ Mean: ", round(s$delta_mean, 4),
             " | 95% CrI: [", round(s$delta_q[1], 4), ", ", round(s$delta_q[2], 4), "]")
    })

    output$tau_txt <- renderText({
      req(app_rv$comm)
      s <- app_rv$comm$summary
      paste0("τ Mean: ", round(s$tau_mean, 4),
             " | 95% CrI: [", round(s$tau_q[1], 4), ", ", round(s$tau_q[2], 4), "]")
    })
  })
}
