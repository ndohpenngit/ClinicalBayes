# ======================================================
# Continuous 2-arm: Data & Priors
# ======================================================

cont2a_data_ui <- function(id) {
  ns <- NS(id)
  tabItem(
    tabName = "cont2a",
    fluidRow(
      # Configuration Box - Focused on Control & Borrowing
      box(width = 12, status = "primary", solidHeader = TRUE,
          title = "Continuous Trial & Prior Management",
          column(5,
                 wellPanel(
                   h4("Current Trial: Control Arm"),
                   numericInput(ns("mean_c"), "Mean", 0),
                   numericInput(ns("sd_c"), "SD", 1, min = 0.001),
                   numericInput(ns("n_c"), "Control Sample Size (n_c)", 30, min = 2)
                 )),
          column(7,
                 h4("Methodology Selection (Control Borrowing)"),
                 p("Borrowing is applied to the Control arm using a Power Prior approach."),
                 fluidRow(
                   column(8,
                          sliderInput(ns("alpha"), "Borrowing Strength (α)",
                                      min = 0, max = 1, value = 0.3, step = 0.01)
                   ),
                   column(4,
                          numericInput(ns("target_ess"), "Target ESS", value = 20, min = 1),
                          actionButton(ns("calibrate"), "Calibrate α",
                                       icon = icon("bullseye"), width = "100%")
                   )
                 ),
                 hr(),
                 actionButton(ns("build_all"), "Build Prior & Sync Control Data",
                              class = "btn-success", icon = icon("gears"), width = "100%")
          )
      )
    ),

    # Executive Summary Boxes
    fluidRow(
      valueBoxOutput(ns("ess_prior_box"), width = 4),
      valueBoxOutput(ns("ess_current_box"), width = 4),
      valueBoxOutput(ns("ess_total_box"), width = 4)
    ),

    # Diagnostics Layout
    fluidRow(
      box(width = 6, title = "Control Mean (μ) Density", status = "info", solidHeader = TRUE,
          plotOutput(ns("mu_plot"), height = "280px") %>% withSpinner(),
          tags$b("Summary:"),
          verbatimTextOutput(ns("mu_txt"))
      ),
      box(width = 6, title = "Control Variance (σ²) Density", status = "success", solidHeader = TRUE,
          plotOutput(ns("s2_plot"), height = "280px") %>% withSpinner(),
          tags$b("Summary:"),
          verbatimTextOutput(ns("s2_txt"))
      )
    )
  )
}

cont2a_data_server <- function(id, app_rv) {
  moduleServer(id, function(input, output, session) {
    ns <- session$ns

    # Reactive state for calculated objects
    local_rv <- reactiveValues(active_prior = NULL, active_post = NULL, ess = NULL)

    # Defensive Reset
    observeEvent(list(input$alpha, input$mean_c, input$sd_c, input$n_c), {
      local_rv$active_prior <- NULL
      local_rv$active_post  <- NULL
      local_rv$ess          <- NULL
    })

    # Alpha Calibration Logic
    observeEvent(input$calibrate, {
      df <- get_current_hist_df(app_rv)
      validate(need(nrow(df) > 0, "No historical data found. Please load data first."))

      grid <- calibrate_alpha_grid_cont(
        ybar = df$mean, s2 = df$sd^2, n = df$n,
        target_ess = input$target_ess
      )

      best_alpha <- grid$alpha[which.min(grid$dist)]
      updateSliderInput(session, "alpha", value = best_alpha)
      showNotification(paste("Alpha calibrated to", round(best_alpha, 3)), type = "message")
    })

    # Main Build Logic
    observeEvent(input$build_all, {
      df <- get_current_hist_df(app_rv)
      validate(
        need(nrow(df) > 0, "Please load historical data in the Data tab first."),
        need(input$sd_c > 0, "Standard deviation must be positive.")
      )

      # 1. Generate Control Prior (Borrowed)
      alpha_safe <- auto_alpha(input$alpha, df$mean, df$sd^2, df$n)
      prior_ctrl <- cont_prior_from_hist(df$mean, df$sd^2, df$n, alpha_safe)

      # 2. Generate Control Posterior
      var_c <- (input$sd_c)^2
      post_c <- cont_posterior(prior_ctrl, input$mean_c, var_c, input$n_c)

      # 3. Store Results locally and globally
      local_rv$active_prior <- prior_ctrl
      local_rv$active_post  <- post_c
      local_rv$ess          <- ess_nig(prior_ctrl, post_c)

      # Global Sync: matching binary pattern
      app_rv$cont_ctrl_prior <- prior_ctrl
      app_rv$cont_ctrl_post  <- post_c

      # We store the current control data specifically
      app_rv$cont_current_ctrl <- list(
        mean = input$mean_c,
        sd = input$sd_c,
        n = input$n_c
      )

      showNotification("Control prior and data synchronized.", type = "message")
    })

    # --- Value Box Outputs ---
    output$ess_prior_box <- renderValueBox({
      req(local_rv$ess)
      valueBox(round(local_rv$ess$ess_prior, 1), "Borrowed ESS", icon = icon("history"), color = "blue")
    })

    output$ess_current_box <- renderValueBox({
      valueBox(input$n_c, "Current Control N", icon = icon("user-check"), color = "purple")
    })

    output$ess_total_box <- renderValueBox({
      req(local_rv$ess)
      valueBox(round(local_rv$ess$ess_post, 1), "Control Total ESS", icon = icon("plus-circle"), color = "green")
    })

    # --- Summary Text Outputs ---
    output$mu_txt <- renderText({
      req(local_rv$active_post)
      paste0("Mean Posterior | Mean (m): ", round(local_rv$active_post$m, 4),
             " | Precision (k): ", round(local_rv$active_post$k, 2))
    })

    output$s2_txt <- renderText({
      req(local_rv$active_post)
      paste0("Variance Posterior | Shape (a): ", round(local_rv$active_post$a, 2),
             " | Rate (b): ", round(local_rv$active_post$b, 2))
    })

    # --- Plot Outputs ---
    output$mu_plot <- renderPlot({
      req(local_rv$active_prior)
      if (is.null(local_rv$active_post)) {
        plot_mu_prior_only(local_rv$active_prior)
      } else {
        plot_mu_overlay(local_rv$active_prior, local_rv$active_post)
      }
    })

    output$s2_plot <- renderPlot({
      req(local_rv$active_prior)
      if (is.null(local_rv$active_post)) {
        plot_sigma2_prior_only(local_rv$active_prior)
      } else {
        plot_sigma2_overlay(local_rv$active_prior, local_rv$active_post)
      }
    })
  })
}
