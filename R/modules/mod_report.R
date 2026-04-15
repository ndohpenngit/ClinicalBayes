# ======================================================
# Module: Report (HTML via rmarkdown)
# ======================================================

report_ui <- function(id) {
  ns <- NS(id)
  tabItem(
    tabName = "report",
    fluidRow(
      box(width = 4, title = "Report Options", status = "primary", solidHeader = TRUE,
          textInput(ns("title"), "Report title", "ClinicalBayes Analysis Report"),
          helpText("This will generate a comprehensive HTML summary of all analysis performed in this session."),
          hr(),
          actionButton(ns("gen"), "Generate Report",
                       class = "btn-success", icon = icon("file-code"), width = "100%")
      ),
      box(width = 8, title = "Status & Download", status = "primary", solidHeader = TRUE,
          column(12, align = "center",
                 uiOutput(ns("dl_ui"))
          )
      )
    )
  )
}

report_server <- function(id, app_rv) {
  moduleServer(id, function(input, output, session) {
    ns <- session$ns
    library(rmarkdown)

    out_file <- reactiveVal(NULL)

    observeEvent(input$gen, {
      tpl <- file.path("reports", "report_template.Rmd")

      # Validation
      if (!file.exists(tpl)) {
        showNotification("Report template not found at reports/report_template.Rmd", type = "error")
        return()
      }

      # Updated Params to include Continuous Data
      params <- list(
        title        = input$title,
        # Binary results
        priors       = list(rmap = app_rv$rmap, pp = app_rv$pp),
        ctrl_post    = app_rv$ctrl_post,
        decision     = app_rv$decision,
        oc           = app_rv$oc,
        comm         = app_rv$comm,
        # Continuous results (Added these)
        cont_ctrl    = app_rv$cont_ctrl_post,
        cont_decision = app_rv$cont_decision,
        cont_oc      = app_rv$cont2a_oc,
        comm_cont    = app_rv$comm_cont
      )

      withProgress(message = 'Rendering Report...', value = 0.5, {
        tryCatch({
          of <- tempfile(fileext = ".html")
          rmarkdown::render(
            input = tpl,
            output_file = of,
            params = params,
            quiet = TRUE,
            envir = new.env(parent = globalenv())
          )
          out_file(of)
          showNotification("Report successfully generated.", type = "message")
        }, error = function(e) {
          showNotification(paste("Rendering Error:", e$message), type = "error")
        })
      })
    })

    output$dl_ui <- renderUI({
      if (is.null(out_file())) {
        div(style = "padding: 40px; color: #999;",
            icon("file-circle-question", class = "fa-4x"),
            h4("No report generated yet."),
            p("Click 'Generate Report' to compile your results.")
        )
      } else {
        div(style = "padding: 40px;",
            icon("file-check", class = "fa-4x", style = "color: #28a745;"),
            h4("Report Ready"),
            downloadButton(ns("dl"), "Download HTML Report", class = "btn-lg btn-primary")
        )
      }
    })

    output$dl <- downloadHandler(
      filename = function() {
        paste0("ClinicalBayes_Report_", format(Sys.time(), "%Y%m%d_%H%M"), ".html")
      },
      content = function(file) {
        file.copy(out_file(), file)
      }
    )
  })
}
