# ======================================================
# Continuous 2-arm: Operating Characteristics (OC)
# ======================================================

cont2a_oc_ui <- function(id) {
  ns <- NS(id)

  tabItem(
    tabName = "cont2a_oc",
    fluidRow(
      # Left Column: Input Settings
      box(
        width = 4, title = "Simulation Scenario",
        status = "primary", solidHeader = TRUE,

        tags$p("Define the 'True' state of the world to calculate power or Type I error."),

        numericInput(ns("mu_c_true"), "True Control Mean (μc)", value = 0),
        numericInput(ns("delta_true"), "True Effect Size (Δ)", value = 0.5),
        numericInput(ns("sd_true"), "True Common SD (σ)", value = 1, min = 0.01),

        hr(),
        h4("Simulation Control"),
        numericInput(ns("n_sim"), "Number of Simulations", value = 500, min = 10, max = 5000),
        actionButton(ns("run_oc"), "Run OC Analysis",
                     icon = icon("play"), class = "btn-success", width = "100%")
      ),

      # Right Column: Results and Interpretation
      box(
        width = 8, title = "Operating Characteristics Results",
        status = "primary", solidHeader = TRUE,

        conditionalPanel(
          condition = paste0("input['", ns("run_oc"), "'] == 0"),
          tags$div(style = "color: #777; padding: 20px; text-align: center;",
                   icon("calculator", class = "fa-3x"),
                   tags$h4("Ready to Simulate"),
                   tags$p("Set your truth scenario and click 'Run OC Analysis'"))
        ),

        verbatimTextOutput(ns("oc_txt")),
        uiOutput(ns("oc_summary_ui")),
        plotOutput(ns("oc_plot"), height = "300px")
      )
    )
  )
}

cont2a_oc_server <- function(id, app_rv) {
  moduleServer(id, function(input, output, session) {
    ns <- session$ns

    # Reactive to store results after simulation finishes
    oc_data <- eventReactive(input$run_oc, {

      # VALIDATION: Ensure global design parameters exist
      validate(
        need(!is.null(app_rv$cont_ctrl_prior), "Please build the Historical Prior in 'Priors & Posterior' first."),
        need(!is.null(app_rv$cont_decision), "Please set the Decision Rules (Delta*) first."),
        need(!is.null(app_rv$cont_current$n_c), "Sample sizes (N) not found. Please visit the Priors tab.")
      )

      # 1. Capture Scenario from Local UI
      sim_mu_c   <- input$mu_c_true
      sim_delta  <- input$delta_true
      sim_sd     <- input$sd_true
      n_sim      <- input$n_sim

      # 2. Capture Design from Shared app_rv
      pri_c      <- app_rv$cont_ctrl_prior
      delta_star <- app_rv$cont_decision$delta_star
      p_cut      <- app_rv$cont_decision$p_cut
      n_c        <- app_rv$cont_current$n_c
      n_t        <- app_rv$cont_current$n_t

      # 3. Non-informative Prior for Treatment (Standard Practice)
      pri_t_flat <- safe_nig(0, 0.0001, 2, 1)

      # 4. Monte Carlo Loop
      withProgress(message = 'Running Monte Carlo Simulations...', value = 0, {

        success_vec <- replicate(n_sim, {
          # Simulate "True" Trial Data
          y_c <- rnorm(n_c, mean = sim_mu_c, sd = sim_sd)
          y_t <- rnorm(n_t, mean = sim_mu_c + sim_delta, sd = sim_sd)

          # Fallback variance if n=1
          v_c <- if(n_c > 1) var(y_c) else sim_sd^2
          v_t <- if(n_t > 1) var(y_t) else sim_sd^2

          # Update Posteriors
          post_c <- cont_posterior(pri_c, mean(y_c), v_c, n_c)
          post_t <- cont_posterior(pri_t_flat, mean(y_t), v_t, n_t)

          # Decision Rule Check
          # We use 2000 draws per sim for performance in loops
          d_draws <- cont_draw_delta(post_c, post_t, S = 2000)

          incProgress(1/n_sim)
          mean(d_draws > delta_star) > p_cut
        })
      })

      list(
        power = mean(success_vec),
        n_sim = n_sim,
        delta = sim_delta,
        se = sqrt((mean(success_vec) * (1 - mean(success_vec))) / n_sim)
      )
    })

    # --- Outputs ---

    output$oc_txt <- renderText({
      res <- oc_data()
      paste0(
        "Simulations Performed: ", res$n_sim, "\n",
        "Assumed True Delta:   ", res$delta, "\n",
        "----------------------------------------\n",
        "Efficacy Probability: ", round(res$power * 100, 2), "%\n",
        "M.C. Standard Error:  ", round(res$se, 4)
      )
    })

    output$oc_summary_ui <- renderUI({
      res <- oc_data()
      # Determine if we are looking at Power or Type I Error
      is_power <- res$delta > 0

      tags$div(
        style = "margin-top: 10px; padding: 15px; border-radius: 5px; background: #f4f4f4;",
        tags$b("Interpretation:"),
        if(is_power) {
          tags$p("With a true treatment effect of ", tags$b(res$delta),
                 ", the trial has a ", tags$b(round(res$power * 100, 1), "%"),
                 " probability of success (Power).")
        } else {
          tags$p("With no treatment effect (Delta = 0), the probability of falsely declaring efficacy is ",
                 tags$b(round(res$power * 100, 1), "%"), " (Type I Error).")
        }
      )
    })

    output$oc_plot <- renderPlot({
      res <- oc_data()
      # Simple gauge-style bar plot
      df <- data.frame(category = "Efficacy", value = res$power)

      ggplot(df, aes(x = category, y = value)) +
        geom_bar(stat = "identity", fill = "#3c8dbc", width = 0.5) +
        geom_hline(yintercept = 0.8, linetype = "dashed", color = "red") +
        annotate("text", x = 1, y = 0.85, label = "80% Power Target", color = "red") +
        scale_y_continuous(labels = scales::percent, limits = c(0, 1)) +
        labs(title = "Probability of Declaring Efficacy", x = "", y = "") +
        theme_minimal()
    })
  })
}
