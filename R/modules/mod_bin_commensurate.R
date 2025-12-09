# ======================================================
# Module: Binary Commensurate Prior (Stan / cmdstanr)
# ======================================================

comm_ui <- function(id) {
  ns <- NS(id)
  tabItem(
    tabName = "comm",
    fluidRow(
      box(width = 4, title = "Counts & Hyperprior", status = "primary", solidHeader = TRUE,
          h4("Current trial (binary)"),
          numericInput(ns("y_t"), "Treatment events (y_t)", value = 30, min = 0, step = 1),
          numericInput(ns("n_t"), "Treatment N (n_t)", value = 100, min = 1, step = 1),
          numericInput(ns("y_c"), "Control events (y_c)", value = 20, min = 0, step = 1),
          numericInput(ns("n_c"), "Control N (n_c)", value = 100, min = 1, step = 1),
          hr(),
          h4("Historical control"),
          numericInput(ns("y_h"), "Hist control events (y_h)", value = 45, min = 0, step = 1),
          numericInput(ns("n_h"), "Hist control N (n_h)", value = 220, min = 1, step = 1),
          hr(),
          h4("τ ~ Gamma(shape, rate)"),
          numericInput(ns("tau_shape"), "tau_shape", value = 2, min = 0.1, step = 0.1),
          numericInput(ns("tau_rate"),  "tau_rate",  value = 0.5, min = 0.1, step = 0.1),
          hr(),
          actionButton(ns("fit"), "Fit commensurate model")
      ),
      box(width = 8, title = "Posterior (τ and Δ)", status = "primary", solidHeader = TRUE,
          plotOutput(ns("delta_plot"), height = 260) %>% withSpinner(),
          plotOutput(ns("tau_plot"), height = 260) %>% withSpinner(),
          verbatimTextOutput(ns("comm_txt"))
      )
    )
  )
}

comm_server <- function(id, app_rv) {
  moduleServer(id, function(input, output, session) {
    # require cmdstanr, posterior, bayesplot
    library(cmdstanr)
    library(posterior)
    library(bayesplot)

    mod <- reactiveVal(NULL)

    observeEvent(input$fit, {
      stan_path <- file.path("stan", "commensurate_binom.stan")
      validate(need(file.exists(stan_path),
                    "Stan model stan/commensurate_binom.stan not found."))
      if (is.null(mod())) {
        mod(cmdstan_model(stan_path))
      }
    }, ignoreInit = TRUE)

    fit_res <- eventReactive(input$fit, {
      req(mod())
      data_list <- list(
        y_t = input$y_t,
        n_t = input$n_t,
        y_c = input$y_c,
        n_c = input$n_c,
        y_h = input$y_h,
        n_h = input$n_h,
        tau_shape = input$tau_shape,
        tau_rate  = input$tau_rate
      )

      fit <- mod()$sample(
        data = data_list,
        seed = 20251117,
        chains = 4,
        parallel_chains = 4,
        iter_warmup = 1000,
        iter_sampling = 1000
      )

      draws <- as_draws_df(fit$draws())

      app_rv$comm <- list(
        draws = draws,
        summary = list(
          delta_mean = mean(draws$diff),
          delta_q = quantile(draws$diff, c(0.025,0.5,0.975)),
          tau_mean = mean(draws$tau),
          tau_q = quantile(draws$tau, c(0.025,0.5,0.975))
        ),
        settings = data_list
      )

      list(draws = draws)
    })

    output$delta_plot <- renderPlot({
      req(fit_res())

      bayesplot::mcmc_dens(fit_res()$draws, pars = "diff") +
        ggplot2::labs(title = "Posterior of Δ = p_t − p_c")
    })

    output$tau_plot <- renderPlot({
      req(fit_res())

      bayesplot::mcmc_dens(fit_res()$draws, pars = "tau") +
        ggplot2::labs(title = "Posterior of τ")
    })

    output$comm_txt <- renderText({
      req(app_rv$comm)
      s <- app_rv$comm$summary
      paste0(
        "Δ mean: ", round(s$delta_mean,4),
        " | 95% CrI: [", round(s$delta_q[1],4), ", ", round(s$delta_q[3],4), "]",
        "\nτ mean: ", round(s$tau_mean,4),
        " | 95% CrI: [", round(s$tau_q[1],4), ", ", round(s$tau_q[3],4), "]"
      )
    })
  })
}
