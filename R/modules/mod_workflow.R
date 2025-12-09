library(DiagrammeR)

# -----------------------------------------------------------------------------
# clinicalbayes_workflow()
# - returns a DiagrammeR grViz object (SVG widget) with tuned styling for dashboards
# - arg `dark`: use darker palette (TRUE) or light/neutral palette (FALSE)
# -----------------------------------------------------------------------------
clinicalbayes_workflow <- function(dark = FALSE) {

  # neutral palette that works in light & dark; overridden if dark=TRUE
  pal <- if (isTRUE(dark)) {
    list(
      bg       = "#0d1f2d",  # page background hint
      node_bg  = "#122834",
      node_fill = "#173244",
      node_text = "#E6F0F6",
      cluster_border = "#1f3a44",
      arrow_col = "#8fbfe6"
    )
  } else {
    list(
      bg       = "transparent",
      node_bg  = "white",
      node_fill = "#ffffff",
      node_text = "#222222",
      cluster_border = "#bfcbd6",
      arrow_col = "#2b7fb2"
    )
  }

  # Graph - tuned: fontsize, penwidth, arrowsize, margins, rounded boxes
  grViz(sprintf("
    digraph clinicalbayes {
      graph [rankdir = LR, fontsize = 11, pad='0.2', bgcolor = '%s']

      # Default node style
      node [shape = rectangle,
            style = 'rounded,filled',
            margin = 0.18,
            fontname = 'Helvetica',
            fontsize = 12,
            color = '%s',       /* border color */
            fillcolor = '%s',
            fontcolor = '%s',
            penwidth = 1.2]

      edge [color = '%s',
            arrowsize = 1.0,
            penwidth = 1.1]

      # --- historical data cluster
      subgraph cluster_hist {
        label = 'Historical data';
        labelloc = t;
        style = rounded;
        color = '%s';
        node [fillcolor = '%s'];
        hist_bin  [label='Historical (binary)\\n(y_i, n_i)'];
        hist_cont [label='Historical (continuous)\\n(mean, var, N)'];
      }

      # --- binary endpoint cluster
      subgraph cluster_binary {
        label = 'Binary endpoint';
        labelloc = t;
        style = rounded;
        color = '%s';
        node [fillcolor = '%s'];
        rmap   [label='rMAP prior'];
        pprior [label='Power prior'];
        comm   [label='Commensurate\\n(Stan)'];
        b_dec  [label='Decision: P(Δ > Δ*)'];
        b_oc   [label='Operating characteristics'];
      }

      # --- continuous endpoint cluster
      subgraph cluster_cont {
        label = 'Continuous endpoint';
        labelloc = t;
        style = rounded;
        color = '%s';
        node [fillcolor = '%s'];
        cont_prior [label='Control N-IG\\n(power prior)'];
        cont_dec   [label='Decision: P(μ_t - μ_c > Δ*)'];
        cont_oc    [label='Operating characteristics'];
      }

      # --- reporting cluster
      subgraph cluster_report {
        label = 'Reporting';
        labelloc = t;
        style = rounded;
        color = '%s';
        node [fillcolor = '%s'];
        rep    [label='HTML / PDF report'];
      }

      # current data nodes (outside clusters)
      curr_bin  [label='Current binary data\\n(y_c, n_c, y_t, n_t)'];
      curr_cont [label='Current continuous data\\n(means, variances, N)'];

      # edges
      hist_bin  -> rmap;
      hist_bin  -> pprior;
      hist_bin  -> comm;
      hist_cont -> cont_prior;

      curr_bin  -> rmap;
      curr_bin  -> pprior;
      curr_bin  -> comm;

      curr_bin  -> b_dec;
      b_dec     -> b_oc;

      curr_cont -> cont_prior;
      cont_prior -> cont_dec;
      cont_dec   -> cont_oc;

      rmap   -> b_dec;
      pprior -> b_dec;
      comm   -> b_dec;

      b_oc    -> rep;
      cont_oc -> rep;
      b_dec   -> rep;
      cont_dec-> rep;
    }
  ",
                # sprintf args - palette values
                pal$bg,
                pal$cluster_border, pal$node_fill, pal$node_text,
                pal$arrow_col,
                pal$cluster_border, pal$node_fill,           # cluster_hist color, node fill
                pal$cluster_border, pal$node_fill,           # cluster_binary
                pal$cluster_border, pal$node_fill,           # cluster_cont
                pal$cluster_border, pal$node_fill            # cluster_report
  ))
}

# -----------------------------------------------------------------------------
# Minimal module UI + server (ready-to-copy)
# - Keeps same API as your previous module (ns-based)
# -----------------------------------------------------------------------------
workflow_ui <- function(id) {
  ns <- NS(id)
  tabItem(
    tabName = "workflow",
    fluidRow(
      box(width = 12,
          title = "ClinicalBayes Workflow",
          status = "primary",
          solidHeader = TRUE,
          DiagrammeR::grVizOutput(ns("wf_plot"), height = "540px")
      )
    )
  )
}

workflow_server <- function(id, dark_reactive = NULL) {
  moduleServer(id, function(input, output, session) {

    output$wf_plot <- DiagrammeR::renderGrViz({
      # If you pass a reactive for dark mode, use it; else default FALSE
      dark_flag <- FALSE
      if (!is.null(dark_reactive)) {
        # accept either reactiveVal or reactive expression
        dark_flag <- isTRUE(dark_reactive())
      }
      clinicalbayes_workflow(dark = dark_flag)
    })
  })
}
