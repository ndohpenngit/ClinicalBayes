# ======================================================
# Binary Operating Characteristics (OC) Module
# Updated to match Continuous UX
# ======================================================

oc_ui <- function(id) {
  ns <- NS(id)

  tabItem(
    tabName = "oc",
    fluidRow(
      # Left Column: Input Settings
      box(
        width = 4, title = "Simulation Scenario (Truth)",
        status = "primary", solidHeader = TRUE,

        tags$p("Define the underlying 'True' parameters to calculate Power (if Δ > 0) or Type I Error (if Δ = 0)."),

        numericInput(ns("n_ctrl"), "Control Sample Size (n_c)", value = 50, min = 1),
        numericInput(ns("n_trt"),  "Treatment Sample Size (n_t)", value = 100, min = 1),

        hr(),
        sliderInput(ns("p_c_true"), "True Control Rate (p_c)",
                    min = 0, max = 1, value = 0.2, step = 0.05),
        sliderInput(ns("delta_true"), "True Effect Size (Δ = p_t - p_c)",
                    min = -0.5, max = 0.5, value = 0.15, step = 0.05),

        hr(),
        h4("Simulation Control"),
        numericInput(ns("n_sim"), "Number of Trial Simulations", value = 500, min = 50, step = 50),
        actionButton(ns("run_oc"), "Run OC Analysis",
                     icon = icon("play"), class = "btn-success", width = "100%")
      ),

      # Right Column: Results and Visuals
      box(
        width = 8, title = "Operating Characteristics Results",
        status = "primary", solidHeader = TRUE,

        # Placeholder before running
        conditionalPanel(
          condition = paste0("input['", ns("run_oc"), "'] == 0"),
          tags$div(style = "color: #777; padding: 50px; text-align: center;",
                   icon("vials", class = "fa-4x"),
                   tags$h4("Ready for Simulation"),
                   tags$p("Set your truth scenario and click 'Run OC Analysis'"))
        ),

        # Results Display
        uiOutput(ns("oc_summary_ui")),

        tabsetPanel(
          tabPanel("Power Plot", plotOutput(ns("oc_plot"), height = "350px") %>% withSpinner()),
          tabPanel("Detailed Results", DTOutput(ns("oc_table")) %>% withSpinner())
        )
      )
    )
  )
}

oc_server <- function(id, app_rv) {
  moduleServer(id, function(input, output, session) {
    ns <- session$ns

    # --- Internal Simulation Logic ---

    # Helper to update control posterior for a specific simulated iteration
    sample_p_c_once <- function(y_c, n_c, ctrl_post_template, n_samp = 2000) {
      type <- ctrl_post_template$type
      obj  <- ctrl_post_template$obj

      if (type == "rmap") {
        # Using Robust MAP Mixture
        mix <- postmix_beta(obj, y = y_c, n = n_c)
        # Sample from the mixture components
        comp <- sample.int(length(mix$w), size = n_samp, replace = TRUE, prob = mix$w)
        rbeta(n_samp, mix$a[comp], mix$b[comp])
      } else {
        # Standard Power Prior / Beta-Binomial
        a_post <- obj$a + y_c
        b_post <- obj$b + (n_c - y_c)
        rbeta(n_samp, a_post, b_post)
      }
    }

    oc_res <- eventReactive(input$run_oc, {
      # 1. VALIDATION
      validate(
        need(!is.null(app_rv$ctrl_post), "Please build the Historical Prior in 'Data & Priors' first."),
        need(!is.null(app_rv$decision), "Please define Decision Rules (Delta*) first.")
      )

      # 2. LOCAL ASSIGNMENT
      n_c <- input$n_ctrl
      n_t <- input$n_trt
      p_c_true <- input$p_c_true
      delta_true <- input$delta_true
      p_t_true <- pmin(pmax(p_c_true + delta_true, 0), 1)

      n_sim <- input$n_sim
      delta_star <- app_rv$decision$delta_star
      p_cut <- app_rv$decision$p_cut
      ctrl_post_template <- app_rv$ctrl_post

      # 3. SIMULATION LOOP
      withProgress(message = 'Running Binary Simulations...', value = 0, {

        success_vec <- replicate(n_sim, {
          # Simulate Data
          y_c <- rbinom(1, n_c, p_c_true)
          y_t <- rbinom(1, n_t, p_t_true)

          # Sample Posteriors
          p_c_draw <- sample_p_c_once(y_c, n_c, ctrl_post_template, n_samp = 2000)
          p_t_draw <- rbeta(2000, y_t + 1, n_t - y_t + 1) # Uniform prior for Trt

          incProgress(1/n_sim)

          # Check Decision Rule: P(p_t - p_c > delta_star) > p_cut
          mean((p_t_draw - p_c_draw) > delta_star) > p_cut
        })
      })

      # 4. STORE & RETURN
      res <- list(
        power = mean(success_vec),
        n_sim = n_sim,
        p_c_true = p_c_true,
        p_t_true = p_t_true,
        delta_true = delta_true,
        se = sqrt((mean(success_vec) * (1 - mean(success_vec))) / n_sim)
      )

      # Also update global app_rv for reports
      app_rv$oc_bin_results <- res
      res
    })

    # --- Outputs ---

    output$oc_summary_ui <- renderUI({
      res <- oc_res()
      color <- if(res$power >= 0.8) "#28a745" else "#ffc107"
      is_type_i <- res$delta_true == 0

      tags$div(
        style = paste0("padding: 15px; border-left: 5px solid ", color, "; background: #f8f9fa; margin-bottom: 20px;"),
        tags$h4(if(is_type_i) "Type I Error Analysis" else "Power Analysis"),
        tags$p("Scenario: ", tags$b("p_c =", res$p_c_true), " vs ", tags$b("p_t =", round(res$p_t_true, 3))),
        tags$h3(style = paste0("color: ", color, "; margin: 5px 0;"),
                paste0(round(res$power * 100, 1), "%"),
                tags$small(style="color: #666; font-size: 0.5em;", " Pr(Success)"))
      )
    })

    output$oc_plot <- renderPlot({
      res <- oc_res()
      df <- data.frame(Result = c("Success", "Failure"),
                       Value = c(res$power, 1 - res$power))

      ggplot(df, aes(x = "", y = Value, fill = Result)) +
        geom_bar(stat = "identity", width = 1) +
        coord_polar("y", start = 0) +
        theme_void() +
        scale_fill_manual(values = c("Success" = "#28a745", "Failure" = "#e9ecef")) +
        labs(title = paste0("Trial Success Probability (n=", res$n_sim, ")")) +
        theme(legend.position = "right")
    })

    output$oc_table <- renderDT({
      res <- oc_res()
      df <- data.frame(
        Parameter = c("True Control Rate", "True Treatment Rate", "True Delta",
                      "Simulated Power", "M.C. Error (SE)", "Simulations"),
        Value = c(res$p_c_true, round(res$p_t_true, 3), res$delta_true,
                  paste0(round(res$power*100, 2), "%"), round(res$se, 4), res$n_sim)
      )
      datatable(df, rownames = FALSE, options = list(dom = 't'))
    })
  })
}
