# ======================================================
# mod_cont2a_data.R
# Continuous 2-arm: data & priors (N-IG model with power prior)
# ======================================================

cont2a_data_ui <- function(id) {
  ns <- NS(id)
  tabItem(
    tabName = "cont2a_data",
    fluidRow(
      box(
        width = 4,
        title = "Continuous Data (2-arm)",
        status = "primary",
        solidHeader = TRUE,
        h4("Historical Control Summary"),
        numericInput(ns("hist_mean_c"), "Hist. control mean", value = 8, step = 0.1),
        numericInput(ns("hist_sd_c"),   "Hist. control SD",   value = 10, step = 0.1),
        numericInput(ns("hist_n_c"),    "Hist. control N",    value = 300, min = 1, step = 1),
        sliderInput(ns("alpha_c"), "Historical borrowing α (0 = none, 1 = full)",
                    min = 0, max = 1, value = 0.5, step = 0.05),
        hr(),
        h4("Current Trial Summary"),
        numericInput(ns("cur_mean_c"), "Current control mean", value = 9, step = 0.1),
        numericInput(ns("cur_sd_c"),   "Current control SD",   value = 10, step = 0.1),
        numericInput(ns("cur_n_c"),    "Current control N",    value = 50, min = 1, step = 1),
        numericInput(ns("cur_mean_t"), "Current treatment mean", value = 14, step = 0.1),
        numericInput(ns("cur_sd_t"),   "Current treatment SD",   value = 10, step = 0.1),
        numericInput(ns("cur_n_t"),    "Current treatment N",    value = 100, min = 1, step = 1),
        hr(),
        actionButton(ns("update_cont"), "Update Priors / Posteriors")
      ),
      box(
        width = 8,
        title = tagList(
          "Priors & Posterior (Continuous 2-arm)",
          info_link("cont2a-data")   # anchor in manual
        ),
        status = "primary",
        solidHeader = TRUE,
        splitLayout(
          cellWidths = c("50%", "50%"),
          plotOutput(ns("ctrl_post_plot"), height = 250) %>% withSpinner(),
          plotOutput(ns("trt_post_plot"),  height = 250) %>% withSpinner()
        ),
        verbatimTextOutput(ns("cont_summary"))
      )
    )
  )
}

cont2a_data_server <- function(id, app_rv) {
  moduleServer(id, function(input, output, session) {
    ns <- session$ns

    # ---- Reactive priors & posteriors ----
    pri_ctrl <- eventReactive(input$update_cont, {
      # historical variance
      s2_h <- input$hist_sd_c^2
      cont_prior_from_hist(
        ybar_h = input$hist_mean_c,
        s2_h   = s2_h,
        n_h    = input$hist_n_c,
        alpha  = input$alpha_c,
        m0 = 0, k0 = 0.001, a0 = 0.001, b0 = 0.001
      )
    })

    pri_trt <- eventReactive(input$update_cont, {
      # vague baseline prior for treatment
      list(m = 0, k = 0.001, a = 0.001, b = 0.001)
    })

    post_ctrl <- eventReactive(input$update_cont, {
      pri <- pri_ctrl()
      s2_c <- input$cur_sd_c^2
      cont_posterior(
        pri   = pri,
        ybar  = input$cur_mean_c,
        s2    = s2_c,
        n     = input$cur_n_c
      )
    })

    post_trt <- eventReactive(input$update_cont, {
      pri <- pri_trt()
      s2_t <- input$cur_sd_t^2
      cont_posterior(
        pri   = pri,
        ybar  = input$cur_mean_t,
        s2    = s2_t,
        n     = input$cur_n_t
      )
    })

    observeEvent(input$update_cont, {
      app_rv$cont_ctrl_prior <- pri_ctrl()
      app_rv$cont_trt_prior  <- pri_trt()
      app_rv$cont_ctrl_post  <- post_ctrl()
      app_rv$cont_trt_post   <- post_trt()
    })

    # ---- Plots ----
    output$ctrl_post_plot <- renderPlot({
      post <- post_ctrl()
      draws <- cont_draw_mu_sigma2(post, S = 4000)
      dens  <- density(draws$mu)
      plot(dens,
           main = "Control mean posterior",
           xlab = expression(mu[c]),
           ylab = "Density",
           lwd  = 2)
    })

    output$trt_post_plot <- renderPlot({
      post <- post_trt()
      draws <- cont_draw_mu_sigma2(post, S = 4000)
      dens  <- density(draws$mu)
      plot(dens,
           main = "Treatment mean posterior",
           xlab = expression(mu[t]),
           ylab = "Density",
           lwd  = 2)
    })

    output$cont_summary <- renderText({
      pc <- post_ctrl()
      pt <- post_trt()
      paste0(
        "Control posterior (N-IG):\n",
        "  m = ", round(pc$m, 3),
        ", k = ", round(pc$k, 3),
        ", a = ", round(pc$a, 3),
        ", b = ", round(pc$b, 3), "\n",
        "Treatment posterior (N-IG):\n",
        "  m = ", round(pt$m, 3),
        ", k = ", round(pt$k, 3),
        ", a = ", round(pt$a, 3),
        ", b = ", round(pt$b, 3)
      )
    })
  })
}
