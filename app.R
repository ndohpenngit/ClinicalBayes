# ======================================================
# ClinicalBayes: Bayesian Borrowing for Clinical Trials
# “Clinical trial decisions powered by Bayesian statistics”
# ======================================================

library(shiny)
library(shinydashboard)
library(shinycssloaders)
library(shinyjs)
library(DT)
library(ggplot2)
library(cmdstanr)
tryCatch({
  # CmdStan path
  cmdstanr::set_cmdstan_path(path = cmdstanr::cmdstan_path())
}, error = function(e) {
  warning("Failed to set cmdstanr path: ", conditionMessage(e))
})
library(bayesplot)
library(DiagrammeR)
library(pagedown)
# install.packages("rsvg")
library(tinytex)
# tinytex::install_tinytex()    # installs TinyTeX (pdflatex)

# -----------------------------
# Ensure www/ exists and copy logo if present in project root
# -----------------------------
dir.create("www", showWarnings = FALSE)

# If user put a logo.png in project root, copy it to www/ for packaging
if (!file.exists(file.path("www", "logo.png")) && file.exists("logo.png")) {
  tryCatch({
    file.copy("logo.png", file.path("www", "logo.png"), overwrite = TRUE)
    message("Copied logo.png -> www/logo.png")
  }, error = function(e) {
    message("Failed to copy logo: ", conditionMessage(e))
  })
}

# --- source utility and module files (only if present) ---
src_if_exists <- function(path) {
  if (file.exists(path)) {
    tryCatch({
      source(path)
    }, error = function(e) {
      warning("Error sourcing ", path, " : ", conditionMessage(e))
    })
  } else {
    warning("File not found (skipping): ", path)
  }
}

# Core helpers / modules
source("R/utils_bayes.R")
source("R/utils_ui.R")
source("R/modules/mod_data.R")
source("R/modules/mod_binary_priors.R")
source("R/modules/mod_bin_decision.R")
source("R/modules/mod_bin_oc.R")
source("R/modules/mod_bin_commensurate.R")
source("R/modules/mod_report.R")
source("R/modules/mod_cont2a_data.R")
source("R/modules/mod_cont2a_decision.R")
source("R/modules/mod_cont2a_oc.R")
source("R/modules/mod_workflow.R")
# source("R/workflow_diag.R")
source("www/build_manual.R")

# -------------------------------
# UI
# -------------------------------
ui <- dashboardPage(
  skin = "blue",

  dashboardHeader(
    title = tags$div(
      class = "cb-header-title",
      tags$img(
        src   = "logo.png",
        height = "35px",
        style  = "margin-right: 10px;"
      ),
      tags$span("ClinicalBayes")
    )
  ),

  dashboardSidebar(
    sidebarMenu(
      id = "tabs",

      # -------- Data --------
      menuItem(
        "Data", tabName = "data", icon = icon("database")
      ),

      # -------- Binary group --------
      menuItem(
        "Binary endpoints", icon = icon("vial"), startExpanded = FALSE,
        menuSubItem("1. Priors", tabName = "binary", icon = icon("sliders")),
        menuSubItem("2. Decision (Δ)", tabName = "decision", icon = icon("balance-scale-right")),
        menuSubItem("3. Operating characteristics", tabName = "oc", icon = icon("chart-area")),
        menuSubItem("4. Commensurate (Stan)", tabName = "comm", icon = icon("project-diagram"))
      ),

      # -------- Continuous group --------
      menuItem(
        "Continuous endpoints", icon = icon("chart-line"), startExpanded = FALSE,
        menuSubItem("1. Priors & Posterior", tabName = "cont2a_data", icon = icon("sliders")),
        menuSubItem("2. Decision (Δ)", tabName = "cont2a_dec", icon = icon("balance-scale")),
        menuSubItem("3. Operating characteristics", tabName = "cont2a_oc",  icon = icon("chart-line"))
      ),

      # -------- Other --------
      menuItem("Workflow",       tabName = "workflow", icon = icon("project-diagram")),
      menuItem("Report",         tabName = "report",   icon = icon("file-download")),
      menuItem("Documentation",  tabName = "docs",     icon = icon("book-open")),

      hr(),
      div(
        style = "padding:10px;",
        actionButton(
          "show_help", "About & Methods", icon = icon("info-circle"),
          width = "80%", class = "btn-primary"
        )
      ),

      # Moon icon + label (Theme)
      div(
        style = "padding:10px; padding-top:0;",
        tags$div(
          style = "display:flex; align-items:center; justify-content:center; gap:10px;",
          tags$button(
            id    = "dark_toggle_btn",
            type  = "button",
            class = "cb-dark-toggle",
            title = "Toggle dark / light theme",
            tags$i(class = "fa fa-moon")
          ),
          tags$span(
            id = "theme_label",
            "Theme"
          )
        )
      )
    )
  ),

  dashboardBody(
    useShinyjs(),
    tags$head(
      # external CSS
      tags$link(rel = "stylesheet", type = "text/css", href = "theme.css"),

      # JS: handle dark toggle + icon swap
      tags$script(HTML("
        Shiny.addCustomMessageHandler('toggle-dark', function(state){
          var body = document.body;
          var btn  = document.getElementById('dark_toggle_btn');
          if (state) {
            body.classList.add('dark');
            if (btn) btn.innerHTML = '<i class=\"fa fa-sun\"></i>';
          } else {
            body.classList.remove('dark');
            if (btn) btn.innerHTML = '<i class=\"fa fa-moon\"></i>';
          }
        });

        document.addEventListener('DOMContentLoaded', function() {
          var btn = document.getElementById('dark_toggle_btn');
          if (!btn) return;
          btn.addEventListener('click', function() {
            // send an event to server to flip state
            Shiny.setInputValue('dark_toggle_click', new Date().getTime(), {priority: 'event'});
          });
        });
      "))
    ),

    tabItems(
      # module UIs (these will warn in console if module files not found)
      if (exists("data_ui")) data_ui("data_1"),
      if (exists("binary_priors_ui")) binary_priors_ui("binary_priors_1"),
      if (exists("decision_ui")) decision_ui("decision_1"),
      if (exists("oc_ui")) oc_ui("oc_1"),
      if (exists("comm_ui")) comm_ui("comm_1"),
      if (exists("cont2a_data_ui")) cont2a_data_ui("cont2a_data_1"),
      if (exists("cont2a_decision_ui")) cont2a_decision_ui("cont2a_dec_1"),
      if (exists("cont2a_oc_ui")) cont2a_oc_ui("cont2a_oc_1"),
      if (exists("workflow_ui")) workflow_ui("workflow_1"),
      if (exists("report_ui")) report_ui("report_1"),

      # --- Documentation tab ---
      tabItem(
        tabName = "docs",
        fluidRow(
          box(
            width = 12,
            title = tagList(
              "ClinicalBayes – Manual",
              tags$div(
                style = "float:right; font-size: 0.85em;",
                tags$a(
                  href   = "clinicalbayes_manual.html",
                  target = "_blank",
                  class  = "btn btn-default btn-xs",
                  icon("external-link-alt"), " Open in new tab"
                ),
                " ",
                tags$a(
                  href   = "ClinicalBayes_Manual.pdf",
                  target = "_blank",
                  class  = "btn btn-default btn-xs",
                  icon("file-pdf"), " Download PDF"
                )
              )
            ),
            solidHeader = TRUE,
            status = "primary",
            collapsible = TRUE,

            # Embed manual via iframe
            uiOutput("docs_iframe")
          )
        )
      )
    )
  )
)

# -------------------------------
# Server
# -------------------------------
server <- function(input, output, session) {

  # Session-specific shared state
  app_rv <- reactiveValues(
    datasets  = list(),
    current_dataset = NULL,

    # binary
    hist_df   = NULL,
    rmap      = NULL,
    pp        = NULL,
    ctrl_post = NULL,
    decision  = NULL,
    oc        = NULL,
    comm      = NULL,

    # continuous two-arm
    cont_ctrl_post = NULL,
    cont_trt_post  = NULL,
    cont2a_oc      = NULL
  )

  # Modules (call only if server functions exist)
  if (exists("data_server")) data_server("data_1", app_rv)
  if (exists("binary_priors_server")) binary_priors_server("binary_priors_1", app_rv)
  if (exists("decision_server")) decision_server("decision_1", app_rv)
  if (exists("oc_server")) oc_server("oc_1", app_rv)
  if (exists("comm_server")) comm_server("comm_1", app_rv)
  if (exists("cont2a_data_server")) cont2a_data_server("cont2a_data_1", app_rv)
  if (exists("cont2a_decision_server")) cont2a_decision_server("cont2a_dec_1", app_rv)
  if (exists("cont2a_oc_server")) cont2a_oc_server("cont2a_oc_1", app_rv)
  if (exists("report_server")) report_server("report_1", app_rv)
  if (exists("workflow_server")) workflow_server("workflow_1", dark_reactive = dark_state)

  # Documentation iframe UI
  output$docs_iframe <- renderUI({
    manual_html <- "clinicalbayes_manual.html"   # expected inside www/
    manual_pdf  <- "ClinicalBayes_Manual.pdf"

    if (file.exists(file.path("www", manual_html))) {
      tags$iframe(
        src = manual_html,
        style = "width:100%; height: calc(100vh - 230px); border:none; overflow:auto;"
      )
    } else {
      # show helpful instructions if manual is missing
      tagList(
        div(style = "padding:18px;",
            h4("Manual not found"),
            p("The HTML manual was not found in the app's www/ folder."),
            p("To add the manual:"),
            tags$ol(
              tags$li("Build the manual locally with:"),
              tags$pre("source('www/build_manual.R')"),
              tags$li("Or place 'clinicalbayes_manual.html' and 'ClinicalBayes_Manual.pdf' inside the www/ folder."),
              tags$li("Then restart the app or refresh the Documentation tab.")
            ),
            p("Note: building the manual requires Pandoc and (for PDF) LaTeX / Chrome + pagedown.")
        )
      )
    }
  })

  # per-session dark-mode state
  dark_state <- reactiveVal(FALSE)

  observeEvent(input$dark_toggle_click, {
    new_state <- !isTRUE(dark_state())
    dark_state(new_state)
    session$sendCustomMessage("toggle-dark", new_state)
  })

  # About & Methods modal
  observeEvent(input$show_help, {
    showModal(modalDialog(
      title = "ClinicalBayes – About & Methods",
      size = "l",
      easyClose = TRUE,
      footer = modalButton("Close"),
      tagList(
        tags$h4("About"),
        tags$p(
          "ClinicalBayes is a Shiny dashboard for Bayesian clinical trial design ",
          "and analysis with dynamic borrowing and operating characteristics ",
          "for binary and continuous endpoints."
        ),

        tags$h4("Binary endpoint (response / event)"),
        tags$ul(
          tags$li(tags$b("rMAP-style robust mixture prior:"), " mixture of Beta posteriors from historical studies, ",
                  "weighted by sample size, with an added weak Beta(1,1) component to guard against conflict."),
          tags$li(tags$b("Power Prior:"), " Beta(a0,b0) updated using \u03b1-weighted historical counts, ",
                  "controlling the effective borrowing."),
          tags$li(tags$b("Decision rule:"), " posterior probability ",
                  HTML("P(Δ = p<sub>t</sub> − p<sub>c</sub> &gt; Δ*)")),
          tags$li(tags$b("Operating characteristics:"), " Monte Carlo simulation over grids of true (p_c, p_t), ",
                  "reporting Pr(declare efficacy).")
        ),

        tags$h4("Binary commensurate prior (Stan)"),
        tags$ul(
          tags$li("Logit-scale commensurate link between current and historical control "),
          tags$li(HTML("Commensurability parameter τ with Gamma(shape, rate) prior controls shrinkage;")),
          tags$li("Posterior of τ and Δ gives data-driven borrowing.")
        ),

        tags$h4("Continuous endpoint (two-arm)"),
        tags$ul(
          tags$li(tags$b("Normal–Inverse-Gamma (N-IG) model:"), " unknown mean and variance per arm."),
          tags$li(tags$b("Control power prior:"), " historical control summary (Ŷ_H, S²_H, N_H) ",
                  "enter a power prior on the control mean and variance."),
          tags$li(tags$b("Treatment prior:"), " weak N-IG baseline updated by current treatment data."),
          tags$li(tags$b("Decision rule:"), HTML("posterior P(μ<sub>t</sub> − μ<sub>c</sub> &gt; Δ*).")),
          tags$li(tags$b("Continuous OC:"), " simulation over grids of true (μ_c, μ_t) with specified SDs, ",
                  "reporting Pr(declare efficacy).")
        ),

        tags$h4("Report"),
        tags$p(
          "The Report tab generates an HTML document summarizing priors, ",
          "posteriors, decisions, and operating characteristics for audit and communication."
        )
      )
    ))
  })
}

# -------------------------------
# Run App
# -------------------------------
shinyApp(ui, server)
