# ======================================================
# Continuous 2-arm: data & priors (FINAL)
# ======================================================

cont2a_data_ui <- function(id) {
  ns <- NS(id)

  tabItem(
    tabName = "cont2a",
    fluidRow(
      box(width = 6, title = "Control arm",
          status = "primary", solidHeader = TRUE,
          numericInput(ns("mean_c"), "Mean", 0),
          numericInput(ns("sd_c"),   "SD",   1, min = 1e-6),
          numericInput(ns("n_c"),    "N",    30, min = 2)
      ),
      box(width = 6, title = "Treatment arm",
          status = "primary", solidHeader = TRUE,
          numericInput(ns("mean_t"), "Mean", 0.5),
          numericInput(ns("sd_t"),   "SD",   1, min = 1e-6),
          numericInput(ns("n_t"),    "N",    30, min = 2)
      )
    ),

    fluidRow(
      box(width = 4, title = "Historical borrowing",
          status = "primary", solidHeader = TRUE,

          sliderInput(ns("alpha"),
                      "Borrowing strength α",
                      min = 0, max = 1, value = 0.3, step = 0.01),

          numericInput(ns("target_ess"),
                       "Target ESS (prior)",
                       value = 20, min = 1),

          actionButton(ns("calibrate"),
                       "Calibrate α grid",
                       icon = icon("bullseye")),

          hr(),
          verbatimTextOutput(ns("ess_txt")),
          actionButton(ns("build"),
                       "Build prior / posterior",
                       class = "btn-success")
      ),

      box(width = 8, title = "Control prior diagnostics",
          status = "primary", solidHeader = TRUE,
          plotOutput(ns("mu_plot"), height = 250),
          plotOutput(ns("s2_plot"), height = 250),
          verbatimTextOutput(ns("build_status"))
      )
    )
  )
}


cont2a_data_server <- function(id, app_rv) {
  moduleServer(id, function(input, output, session) {

    built <- reactiveVal(FALSE)

    observe({
      app_rv$cont_current <- list(
        mean_c = input$mean_c,
        sd_c   = input$sd_c,
        n_c    = input$n_c,
        mean_t = input$mean_t,
        sd_t   = input$sd_t,
        n_t    = input$n_t
      )
    })

    # --------------------------------------------------
    # Alpha calibration
    # --------------------------------------------------
    observeEvent(input$calibrate, {

      df <- get_current_hist_df(app_rv)
      req(df)

      grid <- calibrate_alpha_grid_cont(
        ybar = df$mean,
        s2   = df$sd^2,
        n    = df$n,
        target_ess = input$target_ess
      )

      best <- grid$alpha[which.min(grid$dist)]

      updateSliderInput(session, "alpha", value = best)

      app_rv$cont_alpha_grid <- grid
      app_rv$cont_alpha_best <- best
    })


    # --------------------------------------------------
    # Build prior / posterior
    # --------------------------------------------------
    observeEvent(input$build, {

      # ---- HARD GUARDS (no silent failure) ----
      req(app_rv$cont_current)
      req(app_rv$cont_current$mean_c,
          app_rv$cont_current$sd_c,
          app_rv$cont_current$n_c,
          app_rv$cont_current$mean_t,
          app_rv$cont_current$sd_t,
          app_rv$cont_current$n_t)

      df <- get_current_hist_df(app_rv)
      req(df)

      # ---- Historical summaries ----
      ybar <- df$mean
      s2   <- df$sd^2
      n    <- df$n

      alpha_safe <- auto_alpha(input$alpha, ybar, s2, n)

      prior <- cont_prior_from_hist(
        ybar = ybar,
        s2   = s2,
        n    = n,
        alpha = alpha_safe
      )

      # ---- Current data (defensive variance) ----
      var_c <- safe_var(app_rv$cont_current$sd_c,
                        app_rv$cont_current$n_c)
      var_t <- safe_var(app_rv$cont_current$sd_t,
                        app_rv$cont_current$n_t)

      req(!is.na(var_c), !is.na(var_t))

      # ---- Posteriors ----
      post_c <- cont_posterior(
        prior,
        app_rv$cont_current$mean_c,
        var_c,
        app_rv$cont_current$n_c
      )

      post_t <- cont_posterior(
        prior,
        app_rv$cont_current$mean_t,
        var_t,
        app_rv$cont_current$n_t
      )

      # ---- Store ----
      app_rv$cont_ctrl_prior <- prior
      app_rv$cont_ctrl_post  <- post_c
      app_rv$cont_trt_post   <- post_t
      app_rv$cont_ess        <- ess_nig(prior, post_c)

      built(TRUE)
    })


    # --------------------------------------------------
    # Outputs (defined ONCE)
    # --------------------------------------------------
    output$ess_txt <- renderText({
      req(built())
      paste0(
        "ESS prior ≈ ", round(app_rv$cont_ess$ess_prior, 1),
        " | ESS posterior ≈ ", round(app_rv$cont_ess$ess_post, 1)
      )
    })

    output$mu_plot <- renderPlot({

      req(app_rv$cont_ctrl_prior)

      if (!built()) {
        plot_mu_prior_only(app_rv$cont_ctrl_prior)
      } else {
        plot_mu_overlay(
          app_rv$cont_ctrl_prior,
          app_rv$cont_ctrl_post
        )
      }
    })

    output$s2_plot <- renderPlot({

      req(app_rv$cont_ctrl_prior)

      if (!built()) {
        plot_sigma2_prior_only(app_rv$cont_ctrl_prior)
      } else {
        plot_sigma2_overlay(
          app_rv$cont_ctrl_prior,
          app_rv$cont_ctrl_post
        )
      }
    })

    output$build_status <- renderText({
      if (!built()) {
        "Waiting for current trial data and Build."
      } else {
        "Prior and posterior successfully built."
      }
    })

  })
}
