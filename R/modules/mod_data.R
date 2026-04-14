# ======================================================
# R/modules/mod_data.R
# (Historical) Data module (binary + continuous support)
# ======================================================

# Data manager module: upload, example datasets, saved datasets, aggregation -> hist_summary
# Provides canonical summary: app_rv$hist_summary (data.frame with study, events, n)
# and preview raw: app_rv$hist_df
# ------------------------------------------------------------

# -------------------------
# Example dataset generators & helpers
# -------------------------
make_example_binary_summary <- function() {
  data.frame(
    study = paste0("hist", 1:4),
    events = c(8, 15, 5, 12),
    n = c(40, 75, 30, 50),
    stringsAsFactors = FALSE
  )
}

make_example_binary_raw <- function() {
  set.seed(1)
  studies <- c("hist1", "hist2", "hist3")
  rows <- lapply(studies, function(s) {
    n <- sample(c(30,50,80), 1)
    values <- rbinom(n, 1, runif(1, 0.1, 0.4))
    data.frame(study = s, arm = "control", value = values, stringsAsFactors = FALSE)
  })
  do.call(rbind, rows)
}

make_example_continuous_summary <- function() {
  data.frame(
    study = paste0("hist", 1:4),
    mean = c(1.2, 1.4, 0.9, 1.1),
    sd = c(1.5, 1.6, 1.1, 1.4),
    n = c(60, 80, 40, 50),
    stringsAsFactors = FALSE
  )
}

validate_continuous_summary <- function(df) {

  bad <- which(
    is.na(df$mean) |
      is.na(df$sd)   |
      is.na(df$n)    |
      df$n < 2       |
      df$sd <= 0
  )

  list(
    ok = length(bad) == 0,
    bad_rows = bad
  )
}


# Convert raw per-subject df -> aggregated summary (study, events, n)
aggregate_raw_to_summary <- function(df) {
  if (is.null(df) || nrow(df) == 0) return(NULL)
  # tolerant column name matching
  nm <- tolower(trimws(names(df)))
  # find study column
  study_idx <- which(nm %in% c("study", "trial", "study_id", "studyname"))[1]
  if (is.na(study_idx)) stop("No study-like column found in raw data.")
  study_col <- names(df)[study_idx]

  # find value column candidate
  value_candidates <- c("value","response","outcome","y","event","events","obs")
  value_idx <- which(nm %in% value_candidates)[1]
  if (is.na(value_idx)) stop("No 'value'-like column found in raw data for aggregation.")
  value_col <- names(df)[value_idx]

  # coerce to numeric 0/1
  vals_raw <- df[[value_col]]
  if (is.logical(vals_raw)) {
    vals <- as.numeric(vals_raw)
  } else if (is.numeric(vals_raw)) {
    vals <- as.numeric(vals_raw)
  } else {
    tmp <- tolower(trimws(as.character(vals_raw)))
    tmp[tmp %in% c("true","t")] <- "1"
    tmp[tmp %in% c("false","f")] <- "0"
    vals <- suppressWarnings(as.numeric(tmp))
  }

  df2 <- df
  df2$..value_num_internal <- vals

  # if arm-like column present, attempt to filter to control-like labels
  arm_candidates <- c("arm","group","treatment","trt")
  arm_idx <- which(nm %in% arm_candidates)[1]
  if (!is.na(arm_idx)) {
    arm_col <- names(df)[arm_idx]
    arm_vals <- tolower(as.character(df[[arm_col]]))
    is_control <- grepl("control|placebo|ctl|c", arm_vals)
    if (any(is_control, na.rm = TRUE)) {
      df2 <- df2[which(is_control), , drop = FALSE]
    }
  }

  # aggregate by study
  agg_list <- by(df2$..value_num_internal, df2[[study_col]], function(v) {
    v <- v[!is.na(v)]
    events <- sum(v == 1)
    n <- length(v)
    c(events = events, n = n)
  }, simplify = FALSE)

  res <- do.call(rbind, lapply(names(agg_list), function(st) {
    vals <- agg_list[[st]]
    data.frame(study = st, events = as.integer(vals["events"]), n = as.integer(vals["n"]), stringsAsFactors = FALSE)
  }))

  rownames(res) <- NULL
  res
}

# ---- UI ----
data_ui <- function(id) {
  ns <- NS(id)
  tabItem(
    tabName = "data", # Matches sidebar tabItem name in app.R
    fluidRow(
      box(
        width = 4, status = "primary", solidHeader = TRUE,
        title = "Upload / Example datasets",
        fileInput(ns("hist_file"), "Upload CSV (auto-detect schema)", accept = c(".csv", "text/csv")),
        textInput(ns("save_name"), "Save as (dataset name)", value = "my_hist"),
        actionButton(ns("save_dataset"), "Save dataset", icon = icon("save"), class = "btn-success"),
        actionButton(ns("clear_all"), "Reset / Clear all", icon = icon("trash"), class = "btn-warning"),
        hr(),
        h5("Quick examples:"),
        downloadButton(ns("dl_example_bin_summary"), "Download binary summary CSV", class = "btn-default btn-sm"),
        " ",
        downloadButton(ns("dl_example_bin_raw"), "Download binary raw CSV", class = "btn-default btn-sm"),
        br(), br(),
        downloadButton(ns("dl_example_cont_summary"), "Download continuous summary CSV", class = "btn-default btn-sm"),
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
      # ,
      #
      # hr(),
      # box(
      #   width = 12, status = NULL, solidHeader = TRUE,
      #   title = "Notes:",
      #   tags$ul(
      #     tags$li("Ensure binary data is loaded before building binary priors or running OC analysis."),
      #     tags$li("Ensure continuous data is loaded before building continuous priors or running OC analysis."),
      #     tags$li("Allow user to specify which columns to use for study/events/n if auto-detection fails? (advanced)"),
      #   )
      # )
    )
  )
}

# ---- Server ----
data_server <- function(id, app_rv) {
  moduleServer(id, function(input, output, session) {
    ns <- session$ns

    # --- Example dataset downloads ---
    output$dl_example_bin_summary <- downloadHandler(
      filename = function() "example_hist_binary_summary.csv",
      content = function(file) write.csv(make_example_binary_summary(), file, row.names = FALSE)
    )
    output$dl_example_bin_raw <- downloadHandler(
      filename = function() "example_hist_binary_raw.csv",
      content = function(file) write.csv(make_example_binary_raw(), file, row.names = FALSE)
    )
    output$dl_example_cont_summary <- downloadHandler(
      filename = function() "example_hist_continuous_summary.csv",
      content = function(file) write.csv(make_example_continuous_summary(), file, row.names = FALSE)
    )

    # --- Read uploaded CSV (reactive) ---
    uploaded_df <- reactive({
      if (is.null(input$hist_file)) return(NULL)

      tryCatch({
        df <- read.csv(input$hist_file$datapath, stringsAsFactors = FALSE)
        names(df) <- trimws(names(df))
        df
      }, error = function(e) {
        showNotification("Failed to read uploaded CSV", type = "error")
        NULL
      })
    })

    # --- Schema detection: simple heuristic ---
    detect_schema <- reactive({
      df <- uploaded_df()
      if (is.null(df)) return("none")
      nm <- tolower(trimws(names(df)))
      # summary binary
      if (all(c("study","events","n") %in% nm)) return("binary_summary")
      # raw binary (per-subject) e.g. study, arm, value / response
      if (any(c("value","response","outcome","y","event","events") %in% nm) && any(c("study","subject","id") %in% nm)) return("binary_raw")
      # continuous summary
      if (all(c("study","mean","sd","n") %in% nm) || all(c("study","mean","var","n") %in% nm)) return("continuous_summary")
      # continuous raw (rare)
      if (any(c("value","measurement","obs") %in% nm) && any(c("study","subject","id") %in% nm)) return("continuous_raw")
      return("unknown")
    })

    output$detected_schema <- renderUI({
      schema <- detect_schema()
      switch(schema,
             "binary_summary" = tags$p(tags$b("Detected schema:"), "binary summary (study, events, n)", style = "color: #1a7fb8;"),
             "binary_raw"     = tags$p(tags$b("Detected schema:"), "binary raw (per-subject rows)", style = "color: #1a7fb8;"),
             "continuous_summary" = tags$p(tags$b("Detected schema:"), "continuous summary (study, mean, sd, n)", style = "color: #1a7fb8;"),
             "continuous_raw" = tags$p(tags$b("Detected schema:"), "continuous raw (per-subject rows)", style = "color: #1a7fb8;"),
             "unknown"        = tags$p(tags$b("Detected schema:"), "unknown — check column names.", style = "color: #a33;"),
             tags$p("No uploaded file.", style = "color:#666;")
      )
    })

    # --- Save dataset into app_rv$datasets (named store) ---
    observeEvent(input$save_dataset, {
      df <- uploaded_df()

      if (is.null(df)) {
        showModal(modalDialog(
          title = "Missing File",
          "You must upload a CSV file before you can save a dataset.",
          easyClose = TRUE, footer = modalButton("Close")
        ))
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

      # produce canonical hist_summary if possible
      nm <- tolower(trimws(names(df)))
      if (all(c("study","events","n") %in% nm)) {
        app_rv$hist_summary <- data.frame(study = df[[which(nm=="study")]], events = as.integer(df[[which(nm=="events")]]), n = as.integer(df[[which(nm=="n")]]), stringsAsFactors = FALSE)
      } else {
        # try raw aggregation
        try({
          agg <- aggregate_raw_to_summary(df)
          app_rv$hist_summary <- agg
        }, silent = TRUE)
      }

      # keep a preview of raw upload too
      app_rv$hist_df <- df
      showNotification(paste0("Saved dataset '", name, "'"), type = "message")
    })

    # --- Dataset manager UI (list saved datasets + load/delete buttons) ---
    output$dataset_manager_ui <- renderUI({
      ds_names <- names(app_rv$datasets)
      if (length(ds_names) == 0) {
        tagList(tags$p("No saved datasets."), tags$p("Use Upload + Save dataset, or download an example."))
      } else {
        # create buttons per dataset
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

    # Dynamically register observers for load/delete buttons
    observe({
      ds_names <- names(app_rv$datasets)
      lapply(ds_names, function(nm) {
        # load
        observeEvent(input[[paste0("load_", nm)]], {
          app_rv$current_dataset <- nm
          df <- app_rv$datasets[[nm]]
          app_rv$hist_df <- df
          # compute hist_summary if possible
          nm_low <- tolower(trimws(names(df)))
          if (all(c("study","events","n") %in% nm_low)) {
            app_rv$hist_summary <- data.frame(study = df[[which(nm_low=="study")]], events = as.integer(df[[which(nm_low=="events")]]), n = as.integer(df[[which(nm_low=="n")]]), stringsAsFactors = FALSE)
          } else {
            try({ app_rv$hist_summary <- aggregate_raw_to_summary(df) }, silent = TRUE)
          }
          showNotification(paste0("Loaded dataset '", nm, "'"), type = "message")
        }, ignoreInit = TRUE)
        # delete
        observeEvent(input[[paste0("del_", nm)]], {
          app_rv$datasets[[nm]] <- NULL
          if (identical(app_rv$current_dataset, nm)) {
            app_rv$current_dataset <- NULL
            app_rv$hist_df <- NULL
            app_rv$hist_summary <- NULL
          }
          showNotification(paste0("Deleted dataset '", nm, "'"), type = "warning")
        }, ignoreInit = TRUE)
      })
    })

    # Reset / Clear all datasets
    observeEvent(input$clear_all, {
      app_rv$datasets <- list()
      app_rv$current_dataset <- NULL
      app_rv$hist_df <- NULL
      app_rv$hist_summary <- NULL
      showNotification("All saved datasets cleared", type = "warning")
    })

    # If user uploads a file and does not save it, still allow preview and single-run operations.
    observeEvent(uploaded_df(), {
      app_rv$hist_df <- uploaded_df()
      # set suggested save name
      if (is.null(input$save_name) || input$save_name == "" || input$save_name == "my_hist") {
        updateTextInput(session, "save_name", value = paste0("hist_", format(Sys.time(), "%Y%m%d_%H%M%S")))
      }
      # try to produce hist_summary automatically
      df <- uploaded_df()
      nm_low <- tolower(trimws(names(df)))
      if (all(c("study","events","n") %in% nm_low)) {
        app_rv$hist_summary <- data.frame(study = df[[which(nm_low=="study")]], events = as.integer(df[[which(nm_low=="events")]]), n = as.integer(df[[which(nm_low=="n")]]), stringsAsFactors = FALSE)
      } else {
        try({ app_rv$hist_summary <- aggregate_raw_to_summary(df) }, silent = TRUE)
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
