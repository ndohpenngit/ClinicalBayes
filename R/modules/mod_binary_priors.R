# ======================================================
# mod_binary_priors.R
# ======================================================

binary_priors_ui <- function(id) {
  ns <- NS(id)
  tabItem(
    tabName = "binary",
    fluidRow(
      # Configuration Box
      box(width = 12, status = "primary", solidHeader = TRUE,
          title = "Binary Control Prior Management",
          column(5,
                 wellPanel(
                   h4("Current Trial: Control Arm"),
                   p(tags$small("Enter current control data to build the borrowed posterior.")),
                   numericInput(ns("curr_events"), "Control Events (y_c)", value = 12, min = 0),
                   numericInput(ns("curr_n"), "Control Total N (n_c)", value = 50, min = 1)
                 )),
          column(7,
                 h4("Methodology Selection (Control Borrowing)"),
                 radioButtons(ns("prior_method"), "Choose Method:",
                              choices = c("rMAP (Robust Mixture)" = "rmap",
                                          "Power Prior (Alpha-weighted)" = "pp"),
                              inline = TRUE),
                 uiOutput(ns("method_controls_ui")),
                 hr(),
                 actionButton(ns("build_all"), "Build Prior & Sync Control Data",
                              class = "btn-success", icon = icon("gears"), width = "100%")
          )
      )
    ),

    fluidRow(
      # Executive Summary Boxes - Focused on Control Arm Information
      valueBoxOutput(ns("ess_prior_box"), width = 4),
      valueBoxOutput(ns("ess_current_box"), width = 4),
      valueBoxOutput(ns("ess_total_box"), width = 4)
    ),

    fluidRow(
      # Diagnostics
      box(width = 6, title = "Control Prior (Historical)", status = "info", solidHeader = TRUE,
          plotOutput(ns("prior_plot"), height = "280px") %>% withSpinner(),
          tags$b("Summary:"),
          verbatimTextOutput(ns("prior_txt"))
      ),
      box(width = 6, title = "Control Posterior (Borrowed)", status = "success", solidHeader = TRUE,
          plotOutput(ns("post_plot"), height = "280px") %>% withSpinner(),
          tags$b("Summary:"),
          verbatimTextOutput(ns("post_txt"))
      )
    )
  )
}

binary_priors_server <- function(id, app_rv) {
  moduleServer(id, function(input, output, session) {
    ns <- session$ns

    local_rv <- reactiveValues(active_prior = NULL, active_post = NULL)

    observeEvent(input$prior_method, {
      local_rv$active_prior <- NULL
      local_rv$active_post  <- NULL
    })

    output$method_controls_ui <- renderUI({
      if (input$prior_method == "rmap") {
        sliderInput(ns("robust_w"), "Robustness weight", min = 0, max = 0.5, value = 0.1, step = 0.05)
      } else {
        sliderInput(ns("alpha"), "Borrowing Strength (α)", min = 0, max = 1, value = 0.5)
      }
    })

    observeEvent(input$build_all, {
      df <- app_rv$hist_summary
      validate(need(!is.null(df), "Please load historical data in the Data tab first."))

      # 1. Handle Control Prior/Posterior logic
      if (input$prior_method == "rmap") {
        base_mix <- make_hist_mix(events = df$events, n = df$n, weight_by_n = TRUE)
        prior <- robustify_mix(base_mix, robust_w = input$robust_w)
        post  <- postmix_beta(prior, y = input$curr_events, n = input$curr_n)

        local_rv$active_prior <- prior
        local_rv$active_post  <- post
        app_rv$ctrl_post      <- list(type = "rmap", obj = post)

      } else {
        pp <- power_prior_beta(events = df$events, n = df$n, alpha = input$alpha, a0 = 1, b0 = 1)
        prior <- list(a = as.numeric(pp$a)[1], b = as.numeric(pp$b)[1])
        post  <- list(a = prior$a + input$curr_events,
                      b = prior$b + (input$curr_n - input$curr_events))

        local_rv$active_prior <- prior
        local_rv$active_post  <- post
        app_rv$ctrl_post      <- list(type = "pp", obj = post)
      }

      # 2. Sync Control Data (Treatment data is handled in mod_bin_decision.R)
      app_rv$current_trial_control <- list(y = input$curr_events, n = input$curr_n)

      showNotification("Control prior built and synchronized for analysis.", type = "message")
    })

    # --- Calibrated ESS Boxes ---
    output$ess_prior_box <- renderValueBox({
      req(local_rv$active_prior)
      val <- if(input$prior_method == "rmap") ess_mix(local_rv$active_prior) else (local_rv$active_prior$a + local_rv$active_prior$b)
      valueBox(round(as.numeric(val)[1], 1), "Borrowed ESS (Historical)", icon = icon("history"), color = "blue")
    })

    output$ess_current_box <- renderValueBox({
      valueBox(input$curr_n, "Current Control N", icon = icon("user-check"), color = "purple")
    })

    output$ess_total_box <- renderValueBox({
      req(local_rv$active_post)
      val <- if(input$prior_method == "rmap") ess_mix(local_rv$active_post) else (local_rv$active_post$a + local_rv$active_post$b)
      valueBox(round(as.numeric(val)[1], 1), "Total Control ESS (Combined)", icon = icon("plus-circle"), color = "green")
    })

    # --- Summaries & Plots ---
    output$prior_txt <- renderText({
      req(local_rv$active_prior)
      if(input$prior_method == "rmap") {
        s <- summarize_mix(local_rv$active_prior)
        paste0("Mixture Model | Mean: ", round(s$mean[1], 4), " | 95% CrI: [", round(s$q2.5[1], 4), ", ", round(s$q97.5[1], 4), "]")
      } else {
        p <- local_rv$active_prior
        paste0("Beta(", round(p$a, 2), ", ", round(p$b, 2), ") | Mean: ", round(p$a / (p$a + p$b), 4))
      }
    })

    output$post_txt <- renderText({
      req(local_rv$active_post)
      if(input$prior_method == "rmap") {
        s <- summarize_mix(local_rv$active_post)
        paste0("Updated Mixture | Mean: ", round(s$mean[1], 4), " | 95% CrI: [", round(s$q2.5[1], 4), ", ", round(s$q97.5[1], 4), "]")
      } else {
        p <- local_rv$active_post
        paste0("Posterior Beta(", round(p$a, 2), ", ", round(p$b, 2), ") | Mean: ", round(p$a / (p$a + p$b), 4))
      }
    })

    output$prior_plot <- renderPlot({
      req(local_rv$active_prior)
      if (input$prior_method == "rmap") plot_density_mix(local_rv$active_prior, "Control Prior Density")
      else plot_density_beta(local_rv$active_prior$a, local_rv$active_prior$b, "Control Prior Density")
    })

    output$post_plot <- renderPlot({
      req(local_rv$active_post)
      if (input$prior_method == "rmap") plot_density_mix(local_rv$active_post, "Control Posterior Density")
      else plot_density_beta(local_rv$active_post$a, local_rv$active_post$b, "Control Posterior Density")
    })
  })
}
