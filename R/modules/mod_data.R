# R/modules/mod_data_prior.R
# Data manager (upload / examples / saved datasets)
# ------------------------------------------------

# Example datasets generator
make_example_binary <- function() {
  data.frame(
    study = paste0("hist", 1:4),
    events = c(8, 15, 5, 12),
    n = c(40, 75, 30, 50),
    stringsAsFactors = FALSE
  )
}
make_example_continuous <- function() {
  data.frame(
    study = paste0("hist", 1:4),
    mean = c(1.2, 1.4, 0.9, 1.1),
    sd = c(1.5, 1.6, 1.1, 1.4),
    n = c(60, 80, 40, 50),
    stringsAsFactors = FALSE
  )
}

# ---- UI ----
data_ui <- function(id) {
  ns <- NS(id)
  tabItem(
    tabName = "data",
    fluidRow(
      box(
        width = 4, status = "primary", solidHeader = TRUE,
        title = "Upload dataset(s)",
        fileInput(ns("hist_file"), "Upload CSV (auto-detect schema)", accept = ".csv"),
        textInput(ns("save_name"), "Save as (dataset name)", value = "my_hist"),
        actionButton(ns("save_dataset"), "Save dataset", icon = icon("save"), class = "btn-success"),
        actionButton(ns("clear_all"), "Reset / Clear all", icon = icon("trash"), class = "btn-warning"),
        hr(),
        h5("Quick examples:"),
        downloadButton(ns("dl_example_bin"), "Download binary example", class = "btn-default btn-sm"),
        " ",
        downloadButton(ns("dl_example_cont"), "Download continuous example", class = "btn-default btn-sm"),
        hr(),
        tags$div(style = "margin-top:8px; color:#666;", uiOutput(ns("detected_schema")))
      ),

      box(
        width = 8, status = "primary", solidHeader = TRUE,
        title = "Saved datasets",
        uiOutput(ns("dataset_manager_ui")),
        hr(),
        h5("Preview / Active dataset:"),
        DTOutput(ns("hist_tbl")) %>% withSpinner()
      )
    )
  )
}

# ---- Server ----
data_server <- function(id, app_rv) {
  moduleServer(id, function(input, output, session) {
    ns <- session$ns

    # Example dataset downloads
    output$dl_example_bin <- downloadHandler(
      filename = function() "example_hist_binary.csv",
      content = function(file) write.csv(make_example_binary(), file, row.names = FALSE)
    )
    output$dl_example_cont <- downloadHandler(
      filename = function() "example_hist_continuous.csv",
      content = function(file) write.csv(make_example_continuous(), file, row.names = FALSE)
    )

    # Read uploaded CSV (reactive)
    uploaded_df <- reactive({
      req(input$hist_file)
      tryCatch({
        df <- read.csv(input$hist_file$datapath, stringsAsFactors = FALSE)
        # trim names & lower-case for detection convenience
        names(df) <- trimws(names(df))
        df
      }, error = function(e) {
        showNotification("Failed to read uploaded CSV", type = "error")
        NULL
      })
    })

    # Simple schema detection
    detect_schema <- reactive({
      df <- uploaded_df()
      if (is.null(df)) return("none")
      nm <- tolower(names(df))
      if (all(c("study","events","n") %in% nm)) return("binary")
      if (all(c("study","mean","sd","n") %in% nm) || all(c("study","mean","var","n") %in% nm)) return("continuous")
      return("unknown")
    })

    output$detected_schema <- renderUI({
      schema <- detect_schema()
      if (schema == "binary") {
        tags$p(tags$b("Detected schema:"), "binary (columns: study, events, n)", style = "color: #1a7fb8;")
      } else if (schema == "continuous") {
        tags$p(tags$b("Detected schema:"), "continuous (columns: study, mean, sd, n)", style = "color: #1a7fb8;")
      } else if (schema == "unknown") {
        tags$p(tags$b("Detected schema:"), "unknown — check column names.", style = "color: #a33;")
      } else {
        tags$p("No uploaded file.", style = "color:#666;")
      }
    })

    # Save dataset into app_rv$datasets
    observeEvent(input$save_dataset, {
      df <- uploaded_df()
      if (is.null(df)) {
        showNotification("No uploaded file to save", type = "error")
        return()
      }
      name <- trimws(input$save_name)
      if (name == "") {
        showNotification("Please provide a dataset name", type = "error")
        return()
      }
      # save (overwrite allowed)
      app_rv$datasets[[name]] <- df
      app_rv$current_dataset <- name
      app_rv$hist_df <- df
      showNotification(paste0("Saved dataset '", name, "'"), type = "message")
    })

    # Dataset manager UI
    output$dataset_manager_ui <- renderUI({
      ds_names <- names(app_rv$datasets)
      if (length(ds_names) == 0) {
        tagList(tags$p("No saved datasets."), tags$p("Use Upload + Save dataset, or download an example."))
      } else {
        rows <- lapply(ds_names, function(nm) {
          fluidRow(
            column(6, strong(nm)),
            column(6, align = "right",
                   actionButton(ns(paste0("load_", nm)), "Load", class = "btn-primary btn-xs"),
                   " ",
                   actionButton(ns(paste0("del_", nm)), "Delete", class = "btn-danger btn-xs")
            )
          )
        })
        do.call(tagList, rows)
      }
    })

    # register observers for load/delete buttons
    observe({
      ds_names <- names(app_rv$datasets)
      lapply(ds_names, function(nm) {
        # load
        observeEvent(input[[paste0("load_", nm)]], {
          app_rv$current_dataset <- nm
          app_rv$hist_df <- app_rv$datasets[[nm]]
          showNotification(paste0("Loaded dataset '", nm, "'"), type = "message")
        }, ignoreInit = TRUE)
        # delete
        observeEvent(input[[paste0("del_", nm)]], {
          app_rv$datasets[[nm]] <- NULL
          if (identical(app_rv$current_dataset, nm)) {
            app_rv$current_dataset <- NULL
            app_rv$hist_df <- NULL
          }
          showNotification(paste0("Deleted dataset '", nm, "'"), type = "warning")
        }, ignoreInit = TRUE)
      })
    })

    # Reset / Clear all saved datasets
    observeEvent(input$clear_all, {
      app_rv$datasets <- list()
      app_rv$current_dataset <- NULL
      app_rv$hist_df <- NULL
      showNotification("All saved datasets cleared", type = "warning")
    })

    # when user uploads a file (not saved), set preview for immediate use
    observeEvent(uploaded_df(), {
      app_rv$hist_df <- uploaded_df()
      # set suggested save name
      if (is.null(input$save_name) || input$save_name == "" || input$save_name == "my_hist") {
        updateTextInput(session, "save_name", value = paste0("hist_", format(Sys.time(), "%Y%m%d_%H%M%S")))
      }
    })

    # Render preview table for current dataset (saved or uploaded)
    output$hist_tbl <- renderDT({
      df <- app_rv$hist_df
      if (is.null(df)) {
        datatable(data.frame(Note = "No dataset selected or uploaded"), options = list(dom = "t"))
      } else {
        datatable(df, rownames = FALSE, options = list(pageLength = 8, scrollX = TRUE))
      }
    })

  })
}
