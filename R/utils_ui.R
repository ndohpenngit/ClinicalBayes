# ============================================================
# utils_ui.R
# Helper functions for UI components (info icon, linking, etc.)
# ============================================================

#' Info icon linking to manual section
#'
#' @param anchor String. Anchor ID in the manual Rmd/HTML, e.g. "binary-rmap".
#'
#' @return HTML tag for small clickable icon.
#'
info_link <- function(anchor) {
  href <- paste0("clinicalbayes_manual.html#", anchor)

  tags$a(
    href   = href,
    target = "_blank",
    class  = "cb-info-icon",
    title  = "Open relevant section in the ClinicalBayes Manual",
    tags$i(class = "fa fa-info-circle")
  )
}

