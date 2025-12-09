# ======================================================
# Module: Report (HTML via rmarkdown)
# ======================================================

report_ui <- function(id) {
  ns <- NS(id)
  tabItem(
    tabName = "report",
    fluidRow(
      box(width = 4, title = "Report Options", status = "primary", solidHeader = TRUE,
          textInput(ns("title"), "Report title", "ClinicalBayes Report"),
          actionButton(ns("gen"), "Generate HTML")
      ),
      box(width = 8, title = "Download", status = "primary", solidHeader = TRUE,
          uiOutput(ns("dl_ui"))
      )
    )
  )
}

report_server <- function(id, app_rv) {
  moduleServer(id, function(input, output, session) {
    library(rmarkdown)

    out_file <- reactiveVal(NULL)

    observeEvent(input$gen, {
      tpl <- file.path("reports", "report_template.Rmd")
      validate(need(file.exists(tpl),
                    "Report template not found at reports/report_template.Rmd"))

      params <- list(
        title      = input$title,
        priors     = list(rmap = app_rv$rmap, pp = app_rv$pp),
        ctrl_post  = app_rv$ctrl_post,
        decision   = app_rv$decision,
        oc         = app_rv$oc,
        comm       = app_rv$comm,
        cont2a_oc  = app_rv$cont2a_oc
      )

      of <- tempfile(fileext = ".html")
      rmarkdown::render(
        input = tpl,
        output_file = of,
        params = params,
        quiet = TRUE,
        envir = new.env()
      )
      out_file(of)
      showNotification("Report generated.", type = "message")
    })

    output$dl_ui <- renderUI({
      req(out_file())
      ns <- session$ns
      downloadLink(ns("dl"), "Download report")
    })

    output$dl <- downloadHandler(
      filename = function() "clinicalbayes_report.html",
      content = function(file) file.copy(out_file(), file)
    )
  })
}
