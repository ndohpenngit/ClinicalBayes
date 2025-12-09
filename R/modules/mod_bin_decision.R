# ======================================================
# mod_bin_decision.R
# Binary posterior decision: P(Δ > Δ*)
# ======================================================

decision_ui <- function(id) {
  ns <- NS(id)
  tabItem(
    tabName = "decision",
    fluidRow(
      box(
        width = 4,
        title = tagList(
          "Decision Inputs (Binary)",
          info_link("binary-decision")
        ),
        status = "primary",
        solidHeader = TRUE,
        numericInput(ns("trt_events"), "Treatment events", value = 30, min = 0, step = 1),
        numericInput(ns("trt_n"),      "Treatment N",      value = 100, min = 1, step = 1),
        numericInput(ns("delta_star"), "Decision margin Δ*", value = 0, step = 0.01),
        sliderInput(ns("p_cut"), "Posterior threshold P(Δ > Δ*)",
                    min = 0.5, max = 0.99, value = 0.95, step = 0.01),
        actionButton(ns("run_decision"), "Compute Decision")
      ),
      box(
        width = 8,
        title = "Posterior for Δ = p_t − p_c",
        status = "primary",
        solidHeader = TRUE,
        plotOutput(ns("delta_plot"), height = 260) %>% withSpinner(),
        verbatimTextOutput(ns("delta_summary"))
      )
    )
  )
}

decision_server <- function(id, app_rv) {
  moduleServer(id, function(input, output, session) {
    ns <- session$ns

    # Sample from control posterior stored in app_rv$ctrl_post
    sample_p_c <- function(ctrl_post, n_samp = 4000) {
      validate(need(!is.null(ctrl_post), "Please build a prior in 'Data & Priors' first."))
      type <- ctrl_post$type
      obj  <- ctrl_post$obj

      if (type == "rmap") {
        # mixture: obj is list(w, a, b)
        w <- obj$w
        a <- obj$a
        b <- obj$b
        k <- length(w)
        comp <- sample.int(k, size = n_samp, replace = TRUE, prob = w)
        rbeta(n_samp, shape1 = a[comp], shape2 = b[comp])
      } else if (type == "pp") {
        # Beta(a,b)
        rbeta(n_samp, shape1 = obj$a, shape2 = obj$b)
      } else {
        stop("Unknown ctrl_post$type: ", type)
      }
    }

    decision_draws <- eventReactive(input$run_decision, {
      validate(
        need(!is.null(app_rv$ctrl_post),
             "No control posterior found. First build at least one prior in 'Data & Priors'.")
      )

      p_c <- sample_p_c(app_rv$ctrl_post, n_samp = 4000)
      # treatment: simple Beta(1,1) prior
      p_t <- rbeta(4000, input$trt_events + 1, input$trt_n - input$trt_events + 1)
      delta <- p_t - p_c
      list(p_c = p_c, p_t = p_t, delta = delta)
    })

    output$delta_plot <- renderPlot({
      d <- decision_draws()
      hist(
        d$delta, breaks = 60, freq = FALSE,
        main = "Posterior of Δ = p_t − p_c",
        xlab = "Δ", ylab = "Density"
      )
      abline(v = input$delta_star, col = "red", lwd = 2, lty = 2)
    })

    output$delta_summary <- renderText({
      d <- decision_draws()
      delta <- d$delta
      ds <- quantile(delta, c(0.025, 0.5, 0.975))
      mean_delta <- mean(delta)
      prob <- mean(delta > input$delta_star)
      decision <- ifelse(prob > input$p_cut, "Declare efficacy", "Do NOT declare efficacy")

      app_rv$decision <- list(
        delta    = delta,
        mean     = mean_delta,
        q2.5     = ds[1],
        q50      = ds[2],
        q97.5    = ds[3],
        prob     = prob,
        delta_star = input$delta_star,
        p_cut      = input$p_cut,
        decision   = decision
      )

      paste0(
        "Posterior mean Δ: ", round(mean_delta, 4), "\n",
        "95% CrI for Δ: [", round(ds[1], 4), ", ", round(ds[3], 4), "]\n",
        "P(Δ > Δ* = ", input$delta_star, ") = ", round(prob, 4), "\n",
        "Decision: ", decision
      )
    })
  })
}
