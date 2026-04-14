# ======================================================
# Module: Continuous Commensurate Prior (RStan)
# ======================================================

comm_cont_ui <- function(id) {
  ns <- NS(id)
  tabItem(
    tabName = "comm_cont",
    fluidRow(
      box(width = 12, status = "primary", solidHeader = TRUE,
          title = "Continuous Commensurate Prior Management",
          column(5,
                 wellPanel(
                   h4("Current Trial Data"),
                   fluidRow(
                     column(6,
                            h5(tags$b("Control Arm")),
                            numericInput(ns("ybar_c"), "Mean (ȳc)", value = 10),
                            numericInput(ns("sd_c"), "Std Dev (sc)", value = 2, min = 0.1),
                            numericInput(ns("n_c"), "Total N (nc)", value = 50, min = 1)
                     ),
                     column(6,
                            h5(tags$b("Treatment Arm")),
                            numericInput(ns("ybar_t"), "Mean (ȳt)", value = 12),
                            numericInput(ns("sd_t"), "Std Dev (st)", value = 2, min = 0.1),
                            numericInput(ns("n_t"), "Total N (nt)", value = 50, min = 1)
                     )
                   )
                 )),
          column(7,
                 h4("Historical Data & Commensurability"),
                 fluidRow(
                   column(6,
                          h5(tags$b("Historical Control")),
                          numericInput(ns("ybar_h"), "Mean (ȳh)", value = 9.5),
                          numericInput(ns("sd_h"), "Std Dev (sh)", value = 2.1),
                          numericInput(ns("n_h"), "Total N (nh)", value = 150)
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

    # --- RESULTS AREA ---
    conditionalPanel(
      condition = "output['comm_cont_ready']",
      ns = ns,
      fluidRow(
        valueBoxOutput(ns("mean_delta_box"), width = 6),
        valueBoxOutput(ns("mean_tau_box"), width = 6)
      ),
      fluidRow(
        box(width = 6, title = "Treatment Effect (Δ = μt - μc)", status = "info", solidHeader = TRUE,
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
    )
  )
}

comm_cont_server <- function(id, app_rv) {
  moduleServer(id, function(input, output, session) {
    ns <- session$ns
    local_rv <- reactiveValues(draws = NULL, ready = FALSE)

    # Reset UI on input change
    observeEvent(list(input$ybar_c, input$sd_c, input$n_c, input$ybar_t, input$sd_t, input$n_t,
                      input$ybar_h, input$sd_h, input$n_h, input$tau_shape, input$tau_rate), {
                        local_rv$ready <- FALSE
                      })

    output$comm_cont_ready <- reactive({ local_rv$ready })
    outputOptions(output, "comm_cont_ready", suspendWhenHidden = FALSE)

    observeEvent(input$fit, {
      library(rstan)
      library(posterior)
      library(bayesplot)

      # 1. VALIDATION
      if (!is.null(app_rv$hist_type) && app_rv$hist_type != "continuous") {
        showNotification("Data Type Mismatch: Please load continuous data.", type = "error")
        return()
      }

      # 2. SAMPLING
      tryCatch({
        data_list <- list(
          ybar_t = input$ybar_t, s_t = input$sd_t, n_t = input$n_t,
          ybar_c = input$ybar_c, s_c = input$sd_c, n_c = input$n_c,
          ybar_h = input$ybar_h, s_h = input$sd_h, n_h = input$n_h,
          tau_shape = input$tau_shape, tau_rate = input$tau_rate
        )

        withProgress(message = 'Running Continuous MCMC...', value = 0.5, {
          # Path to your continuous Stan model
          fit <- stan(file = "stan/commensurate_cont.stan", data = data_list,
                      chains = 4, iter = 2000, warmup = 1000, seed = 123)

          draws <- as_draws_df(fit)
          local_rv$draws <- draws
          local_rv$ready <- TRUE

          # Compute summary for global storage
          app_rv$comm_cont <- list(
            draws = draws,
            summary = list(
              delta_mean = mean(draws$diff),
              delta_q = quantile(draws$diff, c(0.025, 0.975)),
              tau_mean = mean(draws$tau)
            )
          )
        })
      }, error = function(e) {
        showNotification(paste("MCMC Error:", e$message), type = "error")
      })
    })

    # --- Value Boxes & Plots ---
    output$mean_delta_box <- renderValueBox({
      req(local_rv$draws)
      valueBox(round(mean(local_rv$draws$diff), 3), "Mean Delta (μt - μc)", icon = icon("arrows-left-right"), color = "blue")
    })

    output$mean_tau_box <- renderValueBox({
      req(local_rv$draws)
      valueBox(round(mean(local_rv$draws$tau), 2), "Mean Tau (Precision)", icon = icon("link"), color = "green")
    })

    output$delta_plot  <- renderPlot({ req(local_rv$draws); mcmc_dens(local_rv$draws, pars = "diff") + theme_minimal() })
    output$delta_trace <- renderPlot({ req(local_rv$draws); mcmc_trace(local_rv$draws, pars = "diff") + theme_minimal() })
    output$tau_plot    <- renderPlot({ req(local_rv$draws); mcmc_dens(local_rv$draws, pars = "tau") + theme_minimal() })
    output$tau_trace   <- renderPlot({ req(local_rv$draws); mcmc_trace(local_rv$draws, pars = "tau") + theme_minimal() })

    output$delta_txt <- renderText({
      req(local_rv$draws)
      d <- local_rv$draws$diff
      paste0("Mean: ", round(mean(d), 3), " | 95% CrI: [", round(quantile(d, 0.025), 3), ", ", round(quantile(d, 0.975), 3), "]")
    })
  })
}
