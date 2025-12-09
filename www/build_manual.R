# =====================================================================
# www/build_manual.R  -- robust/manual builder for ClinicalBayes
# Creates:
#   - www/logo.png              (copied from project root if available)
#   - www/workflow-diagram.png  (if www/make_workflow_png.R exists)
#   - www/clinicalbayes_manual.html
#   - www/ClinicalBayes_Manual.pdf  (via pagedown::chrome_print or fallback to rmarkdown -> pdf)
# =====================================================================

message("---- ClinicalBayes manual builder: start ----")

# ---- config ----
rmd_rel     <- file.path("www", "clinicalbayes_manual.Rmd")   # expected Rmd location
html_out    <- file.path("www", "clinicalbayes_manual.html")  # HTML target
pdf_out     <- file.path("www", "ClinicalBayes_Manual.pdf")   # PDF target
logo_src_candidates <- c("logo.png", file.path("www","logo.png")) # prefer project root logo.png, then www/logo.png
logo_dst    <- file.path("www", "logo.png")
workflow_script <- file.path("www", "make_workflow_png.R")    # optional
workflow_png    <- file.path("www", "workflow-diagram.png")

# ensure www exists
if (!dir.exists("www")) dir.create("www", recursive = TRUE)

# ---- 0) sanity checks ----
if (!file.exists(rmd_rel)) {
  stop("ERROR: Rmd not found at: ", rmd_rel, "\nPlease save your manual as 'www/clinicalbayes_manual.Rmd' and try again.")
}
message("Found Rmd: ", rmd_rel)

# ---- 1) copy logo if available in project root (safe packaging) ----
logo_found <- FALSE
for (cand in logo_src_candidates) {
  if (file.exists(cand) && !identical(normalizePath(cand), normalizePath(logo_dst))) {
    tryCatch({
      file.copy(cand, logo_dst, overwrite = TRUE)
      message("Copied logo: ", cand, " -> ", logo_dst)
      logo_found <- TRUE
      break
    }, error = function(e) {
      message("Warning: failed to copy logo from ", cand, ": ", conditionMessage(e))
    })
  } else if (file.exists(cand) && identical(normalizePath(cand), normalizePath(logo_dst))) {
    logo_found <- TRUE
    message("Logo already in place: ", logo_dst)
    break
  }
}
if (!logo_found) {
  message("No logo found in project root or www/. If you want a logo, place 'logo.png' in project root or 'www/'.")
}

# ---- 2) optional: generate workflow diagram ----
if (file.exists(workflow_script)) {
  message("Found workflow script: ", workflow_script, " — running it to create diagram(s)...")
  tryCatch({
    source(workflow_script, local = new.env())
    if (file.exists(workflow_png)) {
      message("Workflow diagram created: ", workflow_png)
    } else {
      message("Workflow script ran but did not produce ", workflow_png, " (check script).")
    }
  }, error = function(e) {
    message("Error running ", workflow_script, ": ", conditionMessage(e))
  })
} else {
  message("No workflow script at ", workflow_script, " — skipping workflow PNG generation.")
}

# ---- 3) render HTML ----
message("Rendering HTML manual to: ", html_out)
render_ok <- FALSE
tryCatch({
  rmarkdown::render(input = rmd_rel,
                    output_format = "html_document",
                    output_file = basename(html_out),
                    output_dir = dirname(html_out),
                    clean = TRUE,
                    quiet = FALSE)
  render_ok <- file.exists(html_out)
  if (render_ok) message("✓ HTML manual generated at: ", html_out)
  else message("HTML render finished but output not found at: ", html_out)
}, error = function(e) {
  message("ERROR: HTML render failed: ", conditionMessage(e))
})

# ---- 4) render/convert to PDF ----
if (!render_ok) {
  message("Skipping PDF build because HTML render did not complete successfully.")
} else {
  # prefer pagedown::chrome_print to convert the generated HTML to PDF (preserves widgets)
  can_pagedown <- requireNamespace("pagedown", quietly = TRUE)
  chrome_done <- FALSE

  if (can_pagedown) {
    message("Attempting pagedown::chrome_print() to convert HTML -> PDF (recommended).")
    tryCatch({
      # chrome_print takes input html file path and output (pdf). It requires
      # Chrome/Chromium to be installed and visible to pagedown.
      pagedown::chrome_print(input = html_out, output = pdf_out)
      if (file.exists(pdf_out)) {
        message("✓ PDF created via pagedown::chrome_print(): ", pdf_out)
        chrome_done <- TRUE
      } else {
        message("pagedown::chrome_print() completed but PDF not found at ", pdf_out)
      }
    }, error = function(e) {
      message("pagedown::chrome_print() failed: ", conditionMessage(e))
    })
  } else {
    message("pagedown package not available — skipping chrome_print step.")
  }

  # fallback: rmarkdown -> pdf_document (requires LaTeX)
  if (!chrome_done) {
    message("Falling back to rmarkdown::render(..., pdf_document). This requires LaTeX (TinyTeX or full TeX).")
    tex_available <- FALSE
    # check tinytex / pdflatex availability; try tinytex::is_tinytex_installed or rmarkdown::pandoc_available for latex
    if (requireNamespace("tinytex", quietly = TRUE) && tinytex::is_tinytex()) {
      tex_available <- TRUE
    } else {
      # try to detect pdflatex or xelatex in PATH
      pdflatex_ok <- Sys.which("pdflatex") != ""
      xelatex_ok   <- Sys.which("xelatex") != ""
      tex_available <- pdflatex_ok || xelatex_ok
    }

    if (!tex_available) {
      message("No LaTeX installation detected. PDF fallback cannot run. To enable PDF build, install TinyTeX: tinytex::install_tinytex()")
    } else {
      # render PDF using xelatex engine (safer with unicode like Δ)
      tryCatch({
        rmarkdown::render(input = rmd_rel,
                          output_format = rmarkdown::pdf_document(latex_engine = "xelatex"),
                          output_file = basename(pdf_out),
                          output_dir = dirname(pdf_out),
                          clean = TRUE,
                          quiet = FALSE)
        if (file.exists(pdf_out)) {
          message("✓ PDF generated via rmarkdown -> pdf_document at: ", pdf_out)
        } else {
          message("rmarkdown::render completed but PDF not found at: ", pdf_out)
        }
      }, error = function(e) {
        message("PDF build (rmarkdown -> pdf_document) failed: ", conditionMessage(e))
      })
    }
  }
}

message("---- ClinicalBayes manual builder: finished ----")
