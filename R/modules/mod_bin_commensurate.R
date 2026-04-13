# ======================================================
# Module: Binary Commensurate Prior (RStan Version)
# ======================================================

comm_ui <- function(id) {
  ns <- NS(id)
  tabItem(
    tabName = "comm",
    fluidRow(
      # Configuration Box - Matching Binary/Continuous UX
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

    # Executive Summary - Matching UX
    fluidRow(
      valueBoxOutput(ns("mean_delta_box"), width = 6),
      valueBoxOutput(ns("mean_tau_box"), width = 6)
    ),

    fluidRow(
      # Diagnostics
      box(width = 6, title = "Posterior of Δ (p_t − p_c)", status = "info", solidHeader = TRUE,
          plotOutput(ns("delta_plot"), height = "280px") %>% withSpinner(),
          tags$b("Summary:"),
          verbatimTextOutput(ns("delta_txt"))
      ),
      box(width = 6, title = "Posterior of τ (Commensurability)", status = "success", solidHeader = TRUE,
          plotOutput(ns("tau_plot"), height = "280px") %>% withSpinner(),
          tags$b("Summary:"),
          verbatimTextOutput(ns("tau_txt"))
      )
    )
  )
}

comm_server <- function(id, app_rv) {
  moduleServer(id, function(input, output, session) {
    ns <- session$ns

    # Reactive state for MCMC draws
    draws_rv <- reactiveVal(NULL)

    # Clean UI on input change
    observeEvent(list(input$y_c, input$y_t, input$y_h, input$tau_shape), {
      draws_rv(NULL)
    })

    # Fit MCMC Logic
    observeEvent(input$fit, {
      # Use rstan for easier ShinyApps deployment
      library(rstan)
      library(posterior)
      library(bayesplot)

      stan_path <- "stan/commensurate_binom.stan"
      validate(need(file.exists(stan_path), "Stan model file not found."))

      data_list <- list(
        y_t = input$y_t, n_t = input$n_t,
        y_c = input$y_c, n_c = input$n_c,
        y_h = input$y_h, n_h = input$n_h,
        tau_shape = input$tau_shape,
        tau_rate  = input$tau_rate
      )

      # Compile and sample using RStan
      withProgress(message = 'Running MCMC Sampling...', value = 0, {
        fit <- stan(file = stan_path, data = data_list,
                    chains = 4, iter = 2000, warmup = 1000,
                    cores = parallel::detectCores(), seed = 123)

        # Convert to draws_df for easy processing
        draws <- as_draws_df(fit)
        draws_rv(draws)

        # Store for global decision modules
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
    })

    # --- Value Boxes ---
    output$mean_delta_box <- renderValueBox({
      req(draws_rv())
      val <- mean(draws_rv()$diff)
      valueBox(round(val, 4), "Mean Treatment Effect (Δ)", icon = icon("balance-scale"), color = "blue")
    })

    output$mean_tau_box <- renderValueBox({
      req(draws_rv())
      val <- mean(draws_rv()$tau)
      valueBox(round(val, 2), "Mean Commensurability (τ)", icon = icon("link"), color = "green")
    })

    # --- Plots & Summaries ---
    output$delta_plot <- renderPlot({
      req(draws_rv())
      mcmc_dens(draws_rv(), pars = "diff") +
        theme_minimal() + labs(title = "Treatment Effect Posterior")
    })

    output$tau_plot <- renderPlot({
      req(draws_rv())
      mcmc_dens(draws_rv(), pars = "tau") +
        theme_minimal() + labs(title = "Commensurability Parameter Posterior")
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
