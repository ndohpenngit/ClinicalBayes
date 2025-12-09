# R/modules/mod_binary_priors.R
# Binary priors UI + server (rMAP + Power prior)
# ------------------------------------------------

# ---- UI ----
binary_priors_ui <- function(id) {
  ns <- NS(id)
  tabItem(
    tabName = "binary",
    fluidRow(
      column(
        width = 12,
        h3("Binary endpoint — Priors & Current control"),
        p("This tab uses the active historical dataset from the Data tab (app_rv$hist_df).",
          style = "color: #666;")
      )
    ),

    fluidRow(
      box(
        width = 4, status = "primary", solidHeader = TRUE,
        title = "Current control (for posterior)",
        numericInput(ns("curr_events"), "Current control events", value = 12, min = 0, step = 1),
        numericInput(ns("curr_n"), "Current control N", value = 50, min = 1, step = 1),
        hr(),
        actionButton(ns("reset_ctrl"), "Reset current control", icon = icon("undo"))
      ),

      box(
        width = 8, status = NULL, solidHeader = TRUE,
        title = "Historical dataset info",
        uiOutput(ns("hist_summary")),
        hr(),
        helpText("If no dataset is active, go to Data → Upload / Load a dataset.")
      )
    ),

    fluidRow(
      box(
        width = 6,
        title = tagList("rMAP-style Prior", tags$a(href = "#binary-rmap", icon("info-circle"))),
        status = "primary", solidHeader = TRUE,
        sliderInput(ns("robust_w"), "Robust weight vs Beta(1,1)", min = 0, max = 0.5, value = 0.10, step = 0.02),
        actionButton(ns("build_rmap"), "Build / Update rMAP"),
        hr(),
        strong("Prior"),
        plotOutput(ns("rmap_prior_plot"), height = 220) %>% withSpinner(),
        verbatimTextOutput(ns("rmap_prior_txt")),
        strong("Posterior (Control)"),
        plotOutput(ns("rmap_post_plot"), height = 220) %>% withSpinner(),
        verbatimTextOutput(ns("rmap_post_txt")),
        strong("Borrowing (ESS)"),
        verbatimTextOutput(ns("rmap_ess_txt")),
        br(),
        actionButton(ns("clear_rmap"), "Clear rMAP results", icon = icon("trash"))
      ),

      box(
        width = 6,
        title = tagList("Power Prior", tags$a(href = "#binary-powerprior", icon("info-circle"))),
        status = "primary", solidHeader = TRUE,
        sliderInput(ns("alpha"), "Power prior α", min = 0, max = 1, value = 0.5, step = 0.05),
        numericInput(ns("a0"), "Baseline a0", value = 1, min = 0.001, step = 0.5),
        numericInput(ns("b0"), "Baseline b0", value = 1, min = 0.001, step = 0.5),
        actionButton(ns("build_pp"), "Build / Update Power Prior"),
        hr(),
        strong("Prior"),
        plotOutput(ns("pp_prior_plot"), height = 220) %>% withSpinner(),
        verbatimTextOutput(ns("pp_prior_txt")),
        strong("Posterior (Control)"),
        plotOutput(ns("pp_post_plot"), height = 220) %>% withSpinner(),
        verbatimTextOutput(ns("pp_post_txt")),
        strong("Borrowing (ESS)"),
        verbatimTextOutput(ns("pp_ess_txt")),
        br(),
        actionButton(ns("clear_pp"), "Clear Power prior results", icon = icon("trash"))
      )
    )
  )
}

# ---- Server ----
binary_priors_server <- function(id, app_rv) {
  moduleServer(id, function(input, output, session) {
    ns <- session$ns

    # Helper: simple schema detection using app_rv$hist_df
    detect_schema_from_app <- reactive({
      df <- app_rv$hist_df
      if (is.null(df)) return("none")
      nm <- tolower(names(df))
      if (all(c("study","events","n") %in% nm)) return("binary")
      if (all(c("study","mean","sd","n") %in% nm) || all(c("study","mean","var","n") %in% nm)) return("continuous")
      return("unknown")
    })

    # Historical dataset summary UI
    output$hist_summary <- renderUI({
      df <- app_rv$hist_df
      schema <- detect_schema_from_app()
      if (is.null(df)) {
        tagList(
          tags$strong("No active historical dataset"),
          tags$p("Go to Data → Upload / Load a dataset and then return here.")
        )
      } else {
        if (schema == "binary") {
          nstud <- nrow(df)
          tot_n <- sum(df$n, na.rm = TRUE)
          tot_events <- sum(df$events, na.rm = TRUE)
          tagList(
            tags$strong("Active dataset:"),
            tags$p(paste0("Studies: ", nstud, " | Total N: ", tot_n, " | Total events: ", tot_events)),
            DT::renderDataTable({ df }, options = list(pageLength = 5, scrollX = TRUE))
          )
        } else {
          tagList(
            tags$strong("Active dataset (non-binary)"),
            tags$p("Active dataset is not in binary schema. Binary priors require columns: study, events, n."),
            DT::renderDataTable({ df }, options = list(pageLength = 5, scrollX = TRUE))
          )
        }
      }
    })

    # Reset current-control inputs
    observeEvent(input$reset_ctrl, {
      updateNumericInput(session, "curr_events", value = 0)
      updateNumericInput(session, "curr_n", value = 1)
      showNotification("Current control reset", type = "message")
    })

    # ---- rMAP prior ----
    rmap_prior <- eventReactive(input$build_rmap, {
      df <- app_rv$hist_df
      validate(need(!is.null(df), "No historical dataset available. Upload or load a dataset from the Data tab."))
      schema <- detect_schema_from_app()
      validate(need(schema == "binary", "rMAP requires binary-format historical data with columns: study, events, n"))
      # compute base mixture; helper functions in R/utils_bayes.R
      base_mix <- make_hist_mix(df$events, df$n, weight_by_n = TRUE)
      robustify_mix(base_mix, robust_w = input$robust_w)
    })

    rmap_post <- reactive({
      req(rmap_prior())
      postmix_beta(rmap_prior(), y = input$curr_events, n = input$curr_n)
    })

    observeEvent(input$build_rmap, {
      app_rv$rmap <- rmap_prior()
      app_rv$ctrl_post <- list(type = "rmap", obj = rmap_post())
    })

    # render rMAP outputs
    output$rmap_prior_plot <- renderPlot({
      req(rmap_prior())
      plot_density_mix(rmap_prior(), "rMAP-style Prior", "Control rate")
    })
    output$rmap_prior_txt <- renderText({
      req(rmap_prior())
      s <- summarize_mix(rmap_prior(), nsim = 50000)
      paste0("Components: ", length(rmap_prior()$w), " | Mean: ", round(s$mean, 4),
             " | 95% CrI: [", round(s$q2.5, 4), ", ", round(s$q97.5, 4), "]")
    })
    output$rmap_post_plot <- renderPlot({
      req(rmap_post())
      plot_density_mix(rmap_post(), "Posterior (Control, rMAP)", "Control rate")
    })
    output$rmap_post_txt <- renderText({
      req(rmap_post())
      s <- summarize_mix(rmap_post(), nsim = 50000)
      paste0("Posterior mean: ", round(s$mean, 4),
             " | 95% CrI: [", round(s$q2.5, 4), ", ", round(s$q97.5, 4), "]")
    })
    output$rmap_ess_txt <- renderText({
      req(rmap_prior(), rmap_post())
      paste0("Prior ESS ~", round(ess_mix(rmap_prior()), 1),
             " | Posterior ESS ~", round(ess_mix(rmap_post()), 1),
             " | Incremental ESS ~", round(ess_mix(rmap_post()) - ess_mix(rmap_prior()), 1))
    })

    # Clear rMAP results
    observeEvent(input$clear_rmap, {
      app_rv$rmap <- NULL
      app_rv$ctrl_post <- NULL
      showNotification("rMAP results cleared", type = "warning")
    })

    # ---- Power prior ----
    pp_prior <- eventReactive(input$build_pp, {
      df <- app_rv$hist_df
      validate(need(!is.null(df), "No historical dataset available. Upload or load a dataset from the Data tab."))
      schema <- detect_schema_from_app()
      validate(need(schema == "binary", "Power prior (beta) requires binary-format historical data with columns: study, events, n"))
      # power_prior_beta should accept named args: events, n, alpha, a0, b0
      power_prior_beta(events = df$events, n = df$n, alpha = input$alpha, a0 = input$a0, b0 = input$b0)
    })

    pp_post <- reactive({
      req(pp_prior())
      list(
        a = pp_prior()$a + input$curr_events,
        b = pp_prior()$b + (input$curr_n - input$curr_events)
      )
    })

    observeEvent(input$build_pp, {
      app_rv$pp <- pp_prior()
      app_rv$ctrl_post <- list(type = "pp", obj = pp_post())
    })

    # render power prior outputs
    output$pp_prior_plot <- renderPlot({
      req(pp_prior())
      plot_density_beta(pp_prior()$a, pp_prior()$b, "Power Prior", "Control rate")
    })
    output$pp_prior_txt <- renderText({
      req(pp_prior())
      m <- pp_prior()
      mean_val <- m$a / (m$a + m$b)
      q <- qbeta(c(0.025, 0.975), m$a, m$b)
      paste0("Beta(", round(m$a, 2), ", ", round(m$b, 2), ")",
             " | Mean: ", round(mean_val, 4),
             " | 95% CrI: [", round(q[1], 4), ", ", round(q[2], 4), "]")
    })
    output$pp_post_plot <- renderPlot({
      req(pp_post())
      plot_density_beta(pp_post()$a, pp_post()$b, "Posterior (Control, Power Prior)", "Control rate")
    })
    output$pp_post_txt <- renderText({
      req(pp_post())
      a <- pp_post()$a; b <- pp_post()$b
      mean_val <- a / (a + b); q <- qbeta(c(0.025, 0.975), a, b)
      paste0("Posterior Beta(", round(a, 2), ", ", round(b, 2), ")",
             " | Mean: ", round(mean_val, 4),
             " | 95% CrI: [", round(q[1], 4), ", ", round(q[2], 4), "]")
    })
    output$pp_ess_txt <- renderText({
      req(pp_prior(), pp_post())
      pri_ess <- ess_beta(pp_prior()$a, pp_prior()$b)
      post_ess <- ess_beta(pp_post()$a, pp_post()$b)
      paste0("Prior ESS ~", round(pri_ess, 1),
             " | Posterior ESS ~", round(post_ess, 1),
             " | Incremental ESS ~", round(post_ess - pri_ess, 1))
    })

    # Clear power prior results
    observeEvent(input$clear_pp, {
      app_rv$pp <- NULL
      app_rv$ctrl_post <- NULL
      showNotification("Power prior results cleared", type = "warning")
    })

  })
}
