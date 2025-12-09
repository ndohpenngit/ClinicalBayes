# ======================================================
# mod_cont2a_oc.R
# Continuous 2-arm: Operating Characteristics
# ======================================================

cont2a_oc_ui <- function(id) {
  ns <- NS(id)
  tabItem(
    tabName = "cont2a_oc",
    fluidRow(
      box(
        width = 4,
        title = "Simulation Settings (Continuous)",
        status = "primary",
        solidHeader = TRUE,
        numericInput(ns("n_ctrl"), "Control N",   value = 50,  min = 1, step = 1),
        numericInput(ns("n_trt"),  "Treatment N", value = 100, min = 1, step = 1),
        numericInput(ns("mu_c_true"), "True μ_c",    value = 9,  step = 0.1),
        numericInput(ns("sd_c_true"), "True SD_c",   value = 10, step = 0.1),
        numericInput(ns("delta_true"), "True Δ = μ_t − μ_c", value = 4, step = 0.1),
        numericInput(ns("sd_t_true"), "True SD_t",   value = 10, step = 0.1),
        numericInput(ns("n_sim"), "Simulations", value = 500, min = 50, step = 50),
        actionButton(ns("run_oc"), "Run OC (Continuous)")
      ),
      box(
        width = 8,
        title = tagList(
          "Operating Characteristics (Continuous)",
          info_link("cont2a-oc")
        ),
        status = "primary",
        solidHeader = TRUE,
        verbatimTextOutput(ns("oc_summary"))
      )
    )
  )
}

cont2a_oc_server <- function(id, app_rv) {
  moduleServer(id, function(input, output, session) {
    ns <- session$ns

    oc_res <- eventReactive(input$run_oc, {
      validate(
        need(!is.null(app_rv$cont_ctrl_prior) && !is.null(app_rv$cont_trt_prior),
             "Please update continuous priors/posteriors in 'Continuous 2-arm: Data' first.")
      )

      pri_ctrl <- app_rv$cont_ctrl_prior
      pri_trt  <- app_rv$cont_trt_prior

      n_sim <- input$n_sim
      n_c   <- input$n_ctrl
      n_t   <- input$n_trt

      mu_c_true <- input$mu_c_true
      sd_c_true <- input$sd_c_true
      delta_true <- input$delta_true
      mu_t_true  <- mu_c_true + delta_true
      sd_t_true  <- input$sd_t_true

      delta_star <- app_rv$cont_decision$delta_star %||% 3
      p_cut      <- app_rv$cont_decision$p_cut      %||% 0.95

      decide <- logical(n_sim)

      for (i in seq_len(n_sim)) {
        # simulate raw data
        y_c <- rnorm(n_c, mean = mu_c_true, sd = sd_c_true)
        y_t <- rnorm(n_t, mean = mu_t_true, sd = sd_t_true)

        ybar_c <- mean(y_c); s2_c <- stats::var(y_c)
        ybar_t <- mean(y_t); s2_t <- stats::var(y_t)

        post_c_i <- cont_posterior(pri_ctrl, ybar_c, s2_c, n_c)
        post_t_i <- cont_posterior(pri_trt,  ybar_t, s2_t, n_t)

        draws_i <- cont_draw_diff(post_c_i, post_t_i, S = 2000)
        prob <- mean(draws_i$delta_mu > delta_star)
        decide[i] <- (prob > p_cut)
      }

      list(
        power      = mean(decide),
        mu_c_true  = mu_c_true,
        mu_t_true  = mu_t_true,
        delta_true = delta_true,
        n_c        = n_c,
        n_t        = n_t,
        delta_star = delta_star,
        p_cut      = p_cut
      )
    })

    output$oc_summary <- renderText({
      r <- oc_res()
      paste0(
        "OC results (Continuous):\n",
        "True μ_c = ", r$mu_c_true, ", True μ_t = ", r$mu_t_true,
        " (Δ_true = ", r$delta_true, ")\n",
        "n_c = ", r$n_c, ", n_t = ", r$n_t, "\n",
        "Decision rule: P(Δ > Δ*) > ", r$p_cut,
        " with Δ* = ", r$delta_star, "\n",
        "Estimated Pr(declare efficacy) ~ ", round(r$power, 3)
      )
    })
  })
}
