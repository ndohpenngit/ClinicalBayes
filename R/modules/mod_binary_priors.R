# R/modules/mod_binary_priors.R
# Binary priors module (rMAP & Power prior) - reads canonical app_rv$hist_summary
# Uses helper functions in R/utils_bayes.R:
# - make_hist_mix(events, n, weight_by_n)
# - robustify_mix(base_mix, robust_w)
# - postmix_beta(mix, y, n)
# - power_prior_beta(events, n, alpha, a0, b0)
# - plot_density_mix(), plot_density_beta(), summarize_mix(), ess_mix(), ess_beta()
# ------------------------------------------------------------


binary_priors_ui <- function(id) {
  ns <- NS(id)
  tabItem(
    tabName = "binary",
    fluidRow(
      box(
        width = 4,
        title = "Historical Data (binary summary)",
        status = "primary",
        solidHeader = TRUE,
        p("This module uses the canonical historical summary (study, events, n) produced by the Data tab."),
        verbatimTextOutput(ns("hist_summary_note"))
      ),

      box(
        width = 8,
        title = "Current control + Management",
        status = "primary",
        solidHeader = TRUE,
        numericInput(ns("curr_events"), "Current control events", value = 12, min = 0, step = 1),
        numericInput(ns("curr_n"), "Current control N", value = 50, min = 1, step = 1),
        actionButton(ns("clear_priors"), "Reset / Clear priors", icon = icon("trash"), class = "btn-warning")
      )
    ),

    fluidRow(
      box(
        width = 6,
        title = tagList("rMAP-style Prior", a(href = "#binary-rmap", icon("info-circle"))),
        status = "warning", solidHeader = TRUE,
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
        verbatimTextOutput(ns("rmap_ess_txt"))
      ),

      box(
        width = 6,
        title = tagList("Power Prior", a(href = "#binary-powerprior", icon("info-circle"))),
        status = "warning", solidHeader = TRUE,
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
        verbatimTextOutput(ns("pp_ess_txt"))
      )
    )
  )
}

binary_priors_server <- function(id, app_rv) {
  moduleServer(id, function(input, output, session) {
    ns <- session$ns

    # Helper to get canonical summary (study, events, n) from app_rv
    hist_summary_df <- reactive({
      # prefer explicit canonical hist_summary produced by data manager
      if (!is.null(app_rv$hist_summary)) return(app_rv$hist_summary)
      # fallback: try app_rv$hist_df and try to aggregate if raw uploaded
      if (!is.null(app_rv$hist_df)) {
        df <- app_rv$hist_df
        # tolerant attempt to find summary columns
        nm <- tolower(trimws(names(df)))
        if (all(c("study","events","n") %in% nm)) {
          return(data.frame(study = df[[which(nm=="study")]], events = as.integer(df[[which(nm=="events")]]), n = as.integer(df[[which(nm=="n")]]), stringsAsFactors = FALSE))
        } else {
          # try to aggregate using the same helper used in data module if available
          if (exists("aggregate_raw_to_summary", mode = "function")) {
            try({
              agg <- aggregate_raw_to_summary(df)
              if (!is.null(agg)) return(agg)
            }, silent = TRUE)
          }
        }
      }
      NULL
    })

    output$hist_summary_note <- renderText({
      df <- hist_summary_df()
      if (is.null(df)) {
        "No historical summary available. Upload or load a dataset in the Data tab."
      } else {
        paste0("Using historical summary: ", nrow(df), " studies (columns: study, events, n).")
      }
    })

    # ---- rMAP prior ----
    rmap_prior <- eventReactive(input$build_rmap, {
      df <- hist_summary_df()
      validate(need(!is.null(df), "No historical summary available. Upload or load a dataset in the Data tab."))
      # require schema
      if (!all(c("study","events","n") %in% tolower(names(df)))) {
        # try to coerce column names
        nm <- tolower(names(df))
        if (!all(c("study","events","n") %in% nm)) {
          stop("rMAP requires historical summary with columns: study, events, n")
        }
      }
      # call to utils_bayes functions: make_hist_mix + robustify_mix
      base_mix <- make_hist_mix(events = df$events, n = df$n, weight_by_n = TRUE)
      robustify_mix(base_mix, robust_w = input$robust_w)
    })

    rmap_post <- reactive({
      req(rmap_prior())
      postmix_beta(rmap_prior(), y = input$curr_events, n = input$curr_n)
    })

    observeEvent(input$build_rmap, {
      app_rv$rmap <- rmap_prior()
      app_rv$ctrl_post <- list(type = "rmap", obj = rmap_post())
      showNotification("rMAP prior built", type = "message")
    })

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

    # ---- Power prior ----
    pp_prior <- eventReactive(input$build_pp, {
      df <- hist_summary_df()
      validate(need(!is.null(df), "No historical summary available. Upload or load a dataset in the Data tab."))
      if (!all(c("study","events","n") %in% tolower(names(df)))) {
        stop("Power prior (beta) requires binary-format historical summary with columns: study, events, n")
      }
      # NOTE: power_prior_beta expects (events, n, alpha, a0, b0) — adapt as available
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
      showNotification("Power prior built", type = "message")
    })

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
      mean_val <- a / (a + b)
      q <- qbeta(c(0.025, 0.975), a, b)
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

    # ---- Reset / Clear priors button ----
    observeEvent(input$clear_priors, {
      app_rv$rmap <- NULL
      app_rv$pp <- NULL
      app_rv$ctrl_post <- NULL
      showNotification("Cleared priors and control posterior", type = "warning")
    })

  })
}
