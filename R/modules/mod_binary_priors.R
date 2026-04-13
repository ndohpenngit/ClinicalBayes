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
          title = "Binary Trial & Prior Management",
          column(5,
                 wellPanel(
                   h4("Current Trial Data"),
                   fluidRow(
                     column(6,
                            h5(tags$b("Control Arm")),
                            numericInput(ns("curr_events"), "Events (y_c)", value = 12, min = 0),
                            numericInput(ns("curr_n"), "Total N (n_c)", value = 50, min = 1)
                     ),
                     column(6,
                            h5(tags$b("Treatment Arm")),
                            numericInput(ns("trt_events"), "Events (y_t)", value = 15, min = 0),
                            numericInput(ns("trt_n"), "Total N (n_t)", value = 50, min = 1)
                     )
                   )
                 )),
          column(7,
                 h4("Methodology Selection (Control Borrowing)"),
                 radioButtons(ns("prior_method"), "Choose Method:",
                              choices = c("rMAP (Robust Mixture)" = "rmap",
                                          "Power Prior (Alpha-weighted)" = "pp"),
                              inline = TRUE),
                 uiOutput(ns("method_controls_ui")),
                 hr(),
                 actionButton(ns("build_all"), "Build Prior & Save Trial Data",
                              class = "btn-success", icon = icon("gears"), width = "100%")
          )
      )
    ),

    fluidRow(
      # Executive Summary Boxes
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

    # Reactive state for objects
    local_rv <- reactiveValues(active_prior = NULL, active_post = NULL)

    # Clean UI on method switch
    observeEvent(input$prior_method, {
      local_rv$active_prior <- NULL
      local_rv$active_post  <- NULL
    })

    # Method-specific inputs
    output$method_controls_ui <- renderUI({
      if (input$prior_method == "rmap") {
        sliderInput(ns("robust_w"), "Robustness weight", min = 0, max = 0.5, value = 0.1, step = 0.05)
      } else {
        sliderInput(ns("alpha"), "Borrowing Strength (α)", min = 0, max = 1, value = 0.5)
      }
    })

    # Main Build Logic
    observeEvent(input$build_all, {
      df <- app_rv$hist_summary
      validate(need(!is.null(df), "Please load historical data in the Data tab first."))

      # 1. Handle Control Prior/Posterior
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

      # 2. Store Current Trial Data (Both Arms) for Decision/OC Tabs
      app_rv$current_trial <- list(
        ctrl = list(y = input$curr_events, n = input$curr_n),
        trt  = list(y = input$trt_events,  n = input$trt_n)
      )

      showNotification("Prior built and trial data synchronized.", type = "message")
    })

    # --- ESS Boxes ---
    output$ess_prior_box <- renderValueBox({
      req(local_rv$active_prior)
      val <- if(input$prior_method == "rmap") ess_mix(local_rv$active_prior) else (local_rv$active_prior$a + local_rv$active_prior$b)
      valueBox(round(as.numeric(val)[1], 1), "Control Prior ESS", icon = icon("history"), color = "blue")
    })

    output$ess_current_box <- renderValueBox({
      valueBox(input$curr_n + input$trt_n, "Total Trial N (C+T)", icon = icon("vial"), color = "purple")
    })

    output$ess_total_box <- renderValueBox({
      req(local_rv$active_post)
      val <- if(input$prior_method == "rmap") ess_mix(local_rv$active_post) else (local_rv$active_post$a + local_rv$active_post$b)
      valueBox(round(as.numeric(val)[1], 1), "Control Total ESS", icon = icon("plus-circle"), color = "green")
    })

    # --- Summaries ---
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

    # --- Plots ---
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
