# ======================================================
# mod_bin_oc.R
# Binary Operating Characteristics (power / type I)
# ======================================================

oc_ui <- function(id) {
  ns <- NS(id)
  tabItem(
    tabName = "oc",
    fluidRow(
      box(
        width = 4,
        title = "Simulation Settings (Binary)",
        status = "primary",
        solidHeader = TRUE,
        numericInput(ns("n_ctrl"), "Control N", value = 50, min = 1, step = 1),
        numericInput(ns("n_trt"),  "Treatment N", value = 100, min = 1, step = 1),
        sliderInput(ns("p_c_grid"), "True control rate (p_c) grid",
                    min = 0.05, max = 0.8, value = c(0.1, 0.3), step = 0.05),
        sliderInput(ns("p_t_delta"), "Effect size Δ = p_t − p_c (for power curves)",
                    min = 0, max = 0.6, value = 0.2, step = 0.05),
        numericInput(ns("n_sim"), "Simulations per grid point", value = 500, min = 50, step = 50),
        actionButton(ns("run_oc"), "Run OC Simulation")
      ),
      box(
        width = 8,
        title = tagList(
          "Operating Characteristics (Binary)",
          info_link("binary-oc")
        ),
        status = "primary",
        solidHeader = TRUE,
        plotOutput(ns("oc_plot"), height = 300) %>% withSpinner(),
        DTOutput(ns("oc_table")) %>% withSpinner()
      )
    )
  )
}

oc_server <- function(id, app_rv) {
  moduleServer(id, function(input, output, session) {
    ns <- session$ns

    # Helper to sample p_c posterior for a given simulated dataset
    sample_p_c_once <- function(y_c, n_c, ctrl_post_template, n_samp = 2000) {
      # Rebuild a control posterior given simulated (y_c, n_c)
      type <- ctrl_post_template$type
      obj  <- ctrl_post_template$obj
      if (type == "rmap") {
        mix <- postmix_beta(obj, y = y_c, n = n_c)
        w <- mix$w; a <- mix$a; b <- mix$b
        k <- length(w)
        comp <- sample.int(k, size = n_samp, replace = TRUE, prob = w)
        rbeta(n_samp, a[comp], b[comp])
      } else if (type == "pp") {
        a <- obj$a + y_c
        b <- obj$b + (n_c - y_c)
        rbeta(n_samp, a, b)
      } else {
        stop("Unknown ctrl_post$type: ", type)
      }
    }

    simulate_decision <- function(p_c_true, delta_true, n_c, n_t, ctrl_post_template,
                                  delta_star, p_cut, n_sim = 500) {
      # p_t_true = p_c_true + delta_true (bounded to [0,1])
      p_t_true <- pmin(pmax(p_c_true + delta_true, 0), 1)
      decide <- logical(n_sim)
      for (i in seq_len(n_sim)) {
        y_c <- rbinom(1, n_c, p_c_true)
        y_t <- rbinom(1, n_t, p_t_true)
        p_c_draw <- sample_p_c_once(y_c, n_c, ctrl_post_template, n_samp = 2000)
        p_t_draw <- rbeta(2000, y_t + 1, n_t - y_t + 1)
        delta_draw <- p_t_draw - p_c_draw
        prob <- mean(delta_draw > delta_star)
        decide[i] <- (prob > p_cut)
      }
      mean(decide)
    }

    oc_res <- eventReactive(input$run_oc, {
      validate(
        need(!is.null(app_rv$ctrl_post),
             "No control posterior template found. Please build a prior in 'Data & Priors' first.")
      )

      p_c_vals <- seq(input$p_c_grid[1], input$p_c_grid[2], by = 0.05)
      delta_true <- input$p_t_delta
      n_c <- input$n_ctrl
      n_t <- input$n_trt
      n_sim <- input$n_sim
      delta_star <- if (!is.null(app_rv$decision$delta_star)) app_rv$decision$delta_star else 0
      p_cut      <- if (!is.null(app_rv$decision$p_cut))      app_rv$decision$p_cut      else 0.95

      probs <- sapply(p_c_vals, function(pc) {
        simulate_decision(
          p_c_true = pc,
          delta_true = delta_true,
          n_c = n_c,
          n_t = n_t,
          ctrl_post_template = app_rv$ctrl_post,
          delta_star = delta_star,
          p_cut = p_cut,
          n_sim = n_sim
        )
      })

      df <- data.frame(
        p_c_true   = p_c_vals,
        p_t_true   = p_c_vals + delta_true,
        power      = probs
      )

      app_rv$oc <- df
      df
    })

    output$oc_plot <- renderPlot({
      df <- oc_res()
      plot(df$p_c_true, df$power, type = "b", pch = 19,
           ylim = c(0,1),
           xlab = "True control rate p_c",
           ylab = "Pr(declare efficacy)",
           main = "OC curve (fixed Δ = p_t − p_c)")
      abline(h = 0.025, col = "red", lty = 2)  # example type I threshold
    })

    output$oc_table <- renderDT({
      df <- oc_res()
      datatable(df, rownames = FALSE,
                options = list(pageLength = 10, dom = "tp"))
    })
  })
}
