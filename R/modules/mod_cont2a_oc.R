# ======================================================
# Continuous 2-arm: Operating Characteristics
# ======================================================

cont2a_oc_ui <- function(id) {
  ns <- NS(id)

  tabItem(
    tabName = "cont2a_oc",
    fluidRow(
      box(
        width = 4, title = "OC settings",
        status = "primary", solidHeader = TRUE,
        numericInput(ns("n_sim"), "Simulations", 500),
        actionButton(ns("run_oc"), "Run OC")
      ),
      box(
        width = 8, title = "OC results",
        status = "primary", solidHeader = TRUE,
        verbatimTextOutput(ns("oc_txt"))
      )
    )
  )
}

cont2a_oc_server <- function(id, app_rv) {
  moduleServer(id, function(input, output, session) {

    oc <- eventReactive(input$run_oc, {

      validate(
        need(!is.null(app_rv$cont_ctrl_prior),
             "Build continuous priors first."),
        need(!is.null(app_rv$cont_decision),
             "Run decision first.")
      )

      pri <- app_rv$cont_ctrl_prior
      delta_star <- app_rv$cont_decision$delta_star
      p_cut <- app_rv$cont_decision$p_cut

      mean(replicate(input$n_sim, {

         # Simulate data

        y_c <- rnorm(app_rv$cont_current$n_c,
                     app_rv$cont_current$mu_c_true,
                     app_rv$cont_current$sd_c_true)

        y_t <- rnorm(app_rv$cont_current$n_t,
                     app_rv$cont_current$mu_c_true +
                       app_rv$cont_current$delta_true,
                     app_rv$cont_current$sd_t_true)

        post_c <- cont_posterior(pri, mean(y_c), var(y_c), length(y_c))
        post_t <- cont_posterior(pri, mean(y_t), var(y_t), length(y_t))

        mean(cont_draw_delta(post_c, post_t, 3000) > delta_star) > p_cut
      }))
    })

    output$oc_txt <- renderText({
      paste0("Estimated power ≈ ", round(oc(), 3))
    })
  })
}
