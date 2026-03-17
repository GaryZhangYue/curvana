
# ---- dependency checks --------------------------------------------------------
if (!requireNamespace("shiny", quietly = TRUE)) {
  stop("Package 'shiny' is required. Please install it with install.packages('shiny').")
}
if (!requireNamespace("bs4Dash", quietly = TRUE)) {
  stop("Package 'bs4Dash' is required. Please install it with install.packages('bs4Dash').")
}
if (!requireNamespace("rhandsontable", quietly = TRUE)) {
  stop("Package 'rhandsontable' is required. Please install it with install.packages('rhandsontable').")
}

required_pkgs <- c("ggplot2", "dplyr")
missing_pkgs <- required_pkgs[!vapply(required_pkgs, requireNamespace, logical(1), quietly = TRUE)]
if (length(missing_pkgs) > 0) {
  stop("Please install required packages first: ", paste(missing_pkgs, collapse = ", "))
}

tooltip_label <- function(label, tip) {
  shiny::tagList(
    label,
    shiny::tags$span(
      "?",
      class = "cv-tooltip-badge",
      `data-tip` = tip,
      `aria-label` = tip
    )
  )
}

tooltip_plot <- function(output_id, height, tip) {
  shiny::tags$div(
    class = "cv-tooltip-plot",
    `data-tip` = tip,
    style = "cursor:help;",
    shiny::plotOutput(output_id, height = height)
  )
}

# ---- UI ----------------------------------------------------------------------
ui <- bs4Dash::dashboardPage(
  title = "curvana",
  dark  = FALSE,

  header = bs4Dash::dashboardHeader(
    title = bs4Dash::dashboardBrand(title = "curvana", color = "primary")
  ),

  sidebar = bs4Dash::dashboardSidebar(
    bs4Dash::sidebarMenu(
      id = "sidebar_tabs",
      bs4Dash::menuItem("Introduction",       tabName = "intro",     icon = shiny::icon("home"),        selected = TRUE),
      bs4Dash::menuItem("Load Data",          tabName = "load",      icon = shiny::icon("folder-open")),
      bs4Dash::menuItem("Transform Curves",   tabName = "transform", icon = shiny::icon("cogs")),
      bs4Dash::menuItem("Analytical Metrics", tabName = "metrics",   icon = shiny::icon("chart-bar")),
      bs4Dash::menuItem("Summary",            tabName = "summary",   icon = shiny::icon("chart-line"))
    )
  ),

  body = bs4Dash::dashboardBody(
    shiny::tags$head(
      shiny::tags$style(shiny::HTML(
        paste0(
          ".cv-tooltip-badge {\n",
          "  display: inline-block;\n",
          "  margin-left: 6px;\n",
          "  width: 16px;\n",
          "  height: 16px;\n",
          "  line-height: 16px;\n",
          "  text-align: center;\n",
          "  border-radius: 50%;\n",
          "  background: #17a2b8;\n",
          "  color: #fff;\n",
          "  font-size: 11px;\n",
          "  font-weight: 700;\n",
          "  cursor: help;\n",
          "  position: relative;\n",
          "}\n",
          ".cv-tooltip-badge:hover::after, .cv-tooltip-plot:hover::after {\n",
          "  content: attr(data-tip);\n",
          "  position: absolute;\n",
          "  left: 0;\n",
          "  top: 100%;\n",
          "  margin-top: 8px;\n",
          "  z-index: 9999;\n",
          "  background: rgba(35, 35, 35, 0.95);\n",
          "  color: #fff;\n",
          "  padding: 8px 10px;\n",
          "  border-radius: 6px;\n",
          "  font-size: 12px;\n",
          "  line-height: 1.35;\n",
          "  min-width: 220px;\n",
          "  max-width: 360px;\n",
          "  white-space: normal;\n",
          "  box-shadow: 0 6px 18px rgba(0,0,0,0.25);\n",
          "}\n",
          ".cv-tooltip-plot {\n",
          "  position: relative;\n",
          "}\n",
          ".cv-tooltip-badge:hover::before, .cv-tooltip-plot:hover::before {\n",
          "  content: '';\n",
          "  position: absolute;\n",
          "  left: 12px;\n",
          "  top: 100%;\n",
          "  margin-top: 2px;\n",
          "  border-width: 6px;\n",
          "  border-style: solid;\n",
          "  border-color: transparent transparent rgba(35, 35, 35, 0.95) transparent;\n",
          "  z-index: 10000;\n",
          "}\n"
        )
      ))
    ),
    bs4Dash::tabItems(

      # ===========================================================
      # Page 1 -- Introduction
      # ===========================================================
      bs4Dash::tabItem(
        tabName = "intro",
        shiny::fluidRow(
          bs4Dash::bs4Card(
            title = shiny::tags$span(
              shiny::tags$i(class = "fas fa-atom", style = "margin-right:8px;"),
              shiny::tags$strong("curvana"), " \u2014 AFM Force Curve Analysis"
            ),
            width       = 12,
            status      = "primary",
            solidHeader = TRUE,
            shiny::tags$div(
              style = "font-size:15px; line-height:1.9;",
              shiny::tags$p(
                shiny::tags$strong("curvana"), " is an R package for the analysis and visualization of",
                " atomic force microscopy (AFM) force-distance curve data.",
                " This workflow app guides you through each step of the analysis pipeline,",
                " from raw data import to publication-ready summary plots."
              ),
              shiny::tags$hr(),
              shiny::tags$h4("Workflow"),
              shiny::tags$ol(
                shiny::tags$li(
                  shiny::tags$strong("Load Data"), " \u2014 Import raw AFM curves from a folder by providing its path.",
                  " The metadata generated by createFdObjFromFolder will appear automatically after import.",
                  " You can then review and edit that metadata table interactively, or optionally replace it with a CSV, TXT, or Excel file."
                ),
                shiny::tags$li(
                  shiny::tags$strong("Transform Curves"), " \u2014 Denoise raw deflection data,",
                  " detect and subtract baselines, compute tip sensitivity,",
                  " and convert deflection to calibrated force-distance (FD) curves."
                ),
                shiny::tags$li(
                  shiny::tags$strong("Analytical Metrics"), " \u2014 Compute per-curve metrics:",
                  " adhesive force, adhesion/repulsion energy, rupture distance, and repulsive distance.",
                  " Inspect individual curves with full annotation overlays."
                ),
                shiny::tags$li(
                  shiny::tags$strong("Summary"), " \u2014 Compare results across samples and conditions",
                  " using PCA biplots and violin plots."
                )
              ),
              shiny::tags$hr(),
              shiny::tags$p(
                "Navigate between steps using the sidebar on the left.",
                " Start by clicking ", shiny::tags$strong("Load Data"), "."
              )
            )
          )
        )
      ),

      # ===========================================================
      # Page 2 -- Load Data
      # ===========================================================
      bs4Dash::tabItem(
        tabName = "load",

        shiny::fluidRow(
          bs4Dash::bs4Card(
            title       = "Load Raw Curves",
            width       = 8,
            status      = "primary",
            solidHeader = TRUE,
            shiny::radioButtons(
              "data_source", tooltip_label("Data source", "Choose demo extdata bundled with the package or provide your own folder path."),
              choices  = c("Built-in demo data" = "demo", "Custom folder path" = "custom"),
              selected = "demo"
            ),
            shiny::conditionalPanel(
              condition = "input.data_source == 'custom'",
              shiny::textInput(
                "custom_folder", tooltip_label("Folder path", "Folder contains the raw AFM curves."), value = "",
                placeholder = "e.g. C:/data/my_experiment"
              ),
              shiny::textInput("load_suffix", tooltip_label("suffix", "File suffix of the raw AFM curves, e.g. .txt."), value = ".txt"),
              shiny::textInput("load_pattern", tooltip_label("pattern", "Optional filename pattern used to filter files before loading."), value = ""),
              shiny::fluidRow(
                shiny::column(6,
                  shiny::textInput("load_calc_ramp_ex_nm", tooltip_label("Calc_Ramp_Ex_nm", "Column name for approach distance in raw files."), value = "Calc_Ramp_Ex_nm")
                ),
                shiny::column(6,
                  shiny::textInput("load_calc_ramp_rt_nm", tooltip_label("Calc_Ramp_Rt_nm", "Column name for retract distance in raw files."), value = "Calc_Ramp_Rt_nm")
                )
              ),
              shiny::fluidRow(
                shiny::column(6,
                  shiny::textInput("load_defl_v_ex", tooltip_label("Defl_V_Ex", "Column name for approach deflection in raw files."), value = "Defl_V_Ex")
                ),
                shiny::column(6,
                  shiny::textInput("load_defl_v_rt", tooltip_label("Defl_V_Rt", "Column name for retract deflection in raw files."), value = "Defl_V_Rt")
                )
              ),
              shiny::checkboxInput(
                "use_metadata_for_load",
                tooltip_label("Use uploaded metadata", "When enabled, used the metadata uploaded here to select raw curve files in the folder"),
                value = FALSE
              ),
              shiny::conditionalPanel(
                condition = "input.use_metadata_for_load",
                shiny::hr(),
                shiny::h4("Import Metadata File (optional)"),
                shiny::helpText("Upload a CSV, TXT (tab-separated), or XLSX file if you want to replace or enrich the metadata generated when the curves are loaded."),
                shiny::fileInput(
                  "meta_file", tooltip_label("Choose file", "Upload metadata table (CSV/TXT/XLSX) for matching or replacement."),
                  accept = c("text/csv", "text/plain",
                             "application/vnd.openxmlformats-officedocument.spreadsheetml.sheet",
                             ".csv", ".txt", ".xlsx")
                ),
                shiny::textInput("meta_key_col", tooltip_label("Key column name in uploaded file", "Column in uploaded metadata that identifies each sample (e.g., sample or filename)."),
                                 value = "sample", placeholder = "e.g. sample or filename"),
                shiny::tags$div(
                  style = "margin-top:12px;",
                  shiny::actionButton("import_meta_btn", "Import Metadata", class = "btn-info btn-block",
                                      icon = shiny::icon("file-import"))
                )
              ),
              shiny::numericInput("threads_load", tooltip_label("Threads", "Number of parallel workers used for loading files."), value = 1, min = 1, step = 1)
            ),
            shiny::tags$div(
              style = "margin-top:12px;",
              shiny::actionButton("load_btn", "Load Data", class = "btn-primary btn-block",
                                  icon = shiny::icon("upload"))
            )
          ),
          bs4Dash::bs4Card(
            title = "Status", width = 4, collapsible = TRUE,
            shiny::verbatimTextOutput("status_text")
          )
        ),

        shiny::fluidRow(
          bs4Dash::bs4Card(
            title  = "Metadata",
            width  = 12,
            status = "secondary",
            shiny::fluidRow(
              shiny::column(3, shiny::textInput("new_meta_col_name", tooltip_label("New column name", "Name of the new metadata column to add to all samples."), value = "")),
              shiny::column(3, shiny::selectInput("new_meta_col_type", tooltip_label("Column type", "Data type used for the new metadata column values."),
                choices = c("character", "numeric", "integer", "logical"), selected = "character")),
              shiny::column(3, shiny::textInput("new_meta_col_default", tooltip_label("Default value", "Initial value filled for all rows in the new metadata column."), value = "")),
              shiny::column(3, shiny::tags$br(),
                shiny::actionButton("add_meta_col_btn", "Add column", class = "btn-secondary"))
            ),
            shiny::fluidRow(
              shiny::column(
                width = 12,
                shiny::downloadButton("download_metadata_csv", "Download metadata (CSV)", class = "btn-info")
              )
            ),
            shiny::helpText("Edit cells directly, drag the fill handle to autofill like Excel, or right-click for a context menu."),
            rhandsontable::rHandsontableOutput("metadata_hot")
          )
        )
      ),

      # ===========================================================
      # Page 3 -- Transform Curves
      # ===========================================================
      bs4Dash::tabItem(
        tabName = "transform",

        shiny::fluidRow(
          class = "align-items-stretch",
          style = "min-height: 1020px; margin-bottom: 12px;",
          shiny::column(
            width = 4,
            shiny::tags$div(
              style = "height: 980px;",
              bs4Dash::bs4Card(
                title       = "Curve Transform Settings",
                width       = NULL,
                status      = "warning",
                solidHeader = TRUE,
                style       = "height: 980px; overflow-y: auto;",
                shiny::hr(),

                shiny::numericInput("spring_constant", tooltip_label("Spring constant (N/m)", "Cantilever spring constant used to convert deflection to force during transform_curves."), value = 0.08, min = 0),
                shiny::hr(),

                shiny::checkboxInput("denoise_first", tooltip_label("Denoise with Savitzky-Golay filter", "Apply Savitzky-Golay smoothing before baseline and sensitivity calculations."), value = TRUE),
                shiny::fluidRow(
                  shiny::column(6, shiny::numericInput("denoise_p", tooltip_label("Polynomial Degree", "Savitzky-Golay polynomial order p."), value = 1, min = 0, step = 1)),
                  shiny::column(6, shiny::numericInput("denoise_n", tooltip_label("Window Size", "Savitzky-Golay window size n (odd integer)."), value = 3, min = 3, step = 2)),
                  shiny::column(6, shiny::numericInput("denoise_m", tooltip_label("Derivative Order", "Savitzky-Golay derivative order m; 0 keeps smoothed signal."), value = 0, min = 0, step = 1)),
                  shiny::column(6, shiny::numericInput("denoise_ts", tooltip_label("Sample Spacing", "Sampling interval ts used in Savitzky-Golay filter."), value = 1, min = 0.0001, step = 0.1))
                ),

                shiny::hr(),
                shiny::h5(shiny::tags$b("Baseline span"),style = "font-size: 18px; font-weight: bold;"),
                shiny::h5("Approach",style = "font-size: 16px"),
                shiny::fluidRow(
                  shiny::column(6, shiny::selectInput("least_mode_approach", tooltip_label("Mode", "fixed: use least_length value; automatic: use baseline_span_approach in metadata."),
                    choices = c("fixed", "automatic"), selected = "fixed")),
                  shiny::column(6, shiny::numericInput("least_length_approach", tooltip_label("least_length", "Minimum baseline span length for approach curve."),
                    value = 400, min = 1, step = 1))
                ),
                shiny::h5("Retract",style = "font-size: 16px"),
                shiny::fluidRow(
                  shiny::column(6, shiny::selectInput("least_mode_retract", tooltip_label("Mode", "fixed: use least_length value; automatic: use baseline_span_retract in metadata."),
                    choices = c("fixed", "automatic"), selected = "fixed")),
                  shiny::column(6, shiny::numericInput("least_length_retract", tooltip_label("least_length", "Minimum baseline span length for retract curve."),
                    value = 400, min = 1, step = 1))
                ),

                shiny::hr(),
                shiny::h5("Baseline threshold",style = "font-size: 18px; font-weight: bold;"),
                shiny::numericInput("slp_threshold", tooltip_label("Maximum slope", "Discard curves whose baseline slope exceeds this threshold."),
                    value = 0.01, min = 0, step = 0.001),
                  shiny::numericInput("std_threshold", tooltip_label("Maximum standard deviation of deflection", "Discard curves whose baseline deflection SD exceeds this threshold."),
                    value = 0.01, min = 0, step = 0.001),

                shiny::hr(),
                shiny::h5("Sensitivity Calculation",style = "font-size: 18px; font-weight: bold;"),
                shiny::numericInput("sens_end", tooltip_label("End of Contact region", "Maximum index used for sensitivity calibration near initial contact region."), value = 100, min = 1, step = 1),

                shiny::hr(),
                shiny::numericInput("threads_transform", tooltip_label("Threads", "Number of workers used by transform_curves."), value = 1, min = 1, step = 1),

                shiny::tags$div(
                  style = "margin-top:12px;",
                  shiny::actionButton("transform_btn", "Run Transform", class = "btn-warning btn-block",
                                      icon = shiny::icon("play"))
                )

              )
            )
          ),

          shiny::column(
            width = 8,
            shiny::tags$div(
              style = "height: 980px;",
              bs4Dash::bs4Card(
                title = "Raw Deflection Heatmap",
                width = NULL,
                style = "height: 980px; overflow: hidden;",
                shiny::fluidRow(
                  shiny::column(4, shiny::selectInput("raw_anno_col1", tooltip_label("Annotate col 1", "First metadata column used for top annotation on raw heatmap."),
                    choices = c("None" = ""), selected = "")),
                  shiny::column(4, shiny::selectInput("raw_anno_col2", tooltip_label("Annotate col 2", "Second metadata column used for top annotation on raw heatmap."),
                    choices = c("None" = ""), selected = "")),
                  shiny::column(4, shiny::numericInput("heatmap_tick_interval", tooltip_label("Row tick interval", "Index interval used for row tick marks on the heatmap."),
                    value = 100, min = 1, step = 1))
                ),
                tooltip_plot("raw_heatmap", "740px", "Min-max scaled raw deflection heatmap. Columns are sample segments (approach/retract), rows are measurement index."),
                shiny::downloadButton("download_raw_heatmap", "Download heatmap", class = "btn-info")
              )
            )
          )
        ),

        shiny::fluidRow(
          bs4Dash::bs4Card(
            title = "Raw Deflection Curves",
            width = 6,
            shiny::fluidRow(
              shiny::column(6, shiny::selectInput("raw_group_by", tooltip_label("Group by", "Metadata column used for grouping/color in raw deflection curves."),
                choices = c("None" = ""), selected = "")),
              shiny::column(6, shiny::selectInput("raw_split_by", tooltip_label("Split by", "Metadata column used for panel split in raw deflection curves."),
                choices = c("None" = ""), selected = ""))
            ),
            tooltip_plot("raw_curves_plot", "640px", "Raw deflection curves across samples. Use Group by / Split by to compare metadata-defined groups."),
            shiny::downloadButton("download_raw_curves", "Download raw curves plot", class = "btn-info")
          ),
          bs4Dash::bs4Card(
            title = "FD Curves (Transformed)",
            width = 6,
            shiny::fluidRow(
              shiny::column(6, shiny::selectInput("fd_group_by", tooltip_label("Group by", "Metadata column used for grouping/color in transformed FD curves."),
                choices = c("None" = ""), selected = "")),
              shiny::column(6, shiny::selectInput("fd_split_by", tooltip_label("Split by", "Metadata column used for panel split in transformed FD curves."),
                choices = c("None" = ""), selected = ""))
            ),
            tooltip_plot("fd_curves_plot", "640px", "Transformed force-distance curves generated by transform_curves for available curve segments."),
            shiny::downloadButton("download_fd_curves", "Download transformed curves plot", class = "btn-info")
          )
        )
      ),

      # ===========================================================
      # Page 4 -- Analytical Metrics
      # ===========================================================
      bs4Dash::tabItem(
        tabName = "metrics",
        shiny::fluidRow(
          bs4Dash::bs4Card(
            title       = "Metrics Settings",
            width       = 4,
            status      = "success",
            solidHeader = TRUE,
            shiny::selectInput("noise_threshold_method", tooltip_label("Noise threshold method", "Method used to estimate baseline noise band for downstream metrics."),
              choices = c("quantile", "sd", "mad", "fixed"), selected = "quantile"),
            shiny::numericInput("noise_quantile_low",  tooltip_label("Noise quantile low", "Lower quantile bound used when threshold method is quantile."),
              value = 0, min = 0, max = 1, step = 0.01),
            shiny::numericInput("noise_quantile_high", tooltip_label("Noise quantile high", "Upper quantile bound used when threshold method is quantile."),
              value = 1, min = 0, max = 1, step = 0.01),
            shiny::numericInput("noise_multiplier", tooltip_label("Noise multiplier", "Multiplier applied to noise band for metric detection thresholds."),
              value = 1, min = 0, step = 0.1),
            shiny::hr(),
            shiny::checkboxInput("do_adhesive_force", tooltip_label("Adhesive force", "Calculate minimum-force based adhesion metric."), value = TRUE),
            shiny::checkboxInput("do_energy", tooltip_label("Energies", "Calculate adhesive and repulsive interaction energies."), value = TRUE),
            shiny::checkboxInput("do_rupture", tooltip_label("Rupture distance", "Calculate adhesive/rupture distance metric."), value = TRUE),
            shiny::checkboxInput("do_repulsive", tooltip_label("Repulsive distance", "Calculate repulsive distance metric."), value = TRUE),
            shiny::numericInput("threads_metrics", tooltip_label("Threads", "Number of workers used by analyze_curves_all_analytical_metrics."), value = 1, min = 1, step = 1),
            shiny::tags$div(
              style = "margin-top:12px;",
              shiny::actionButton("metrics_btn", "Run Metrics", class = "btn-success btn-block",
                                  icon = shiny::icon("calculator"))
            )
          ),
          bs4Dash::bs4Card(
            title = "Single Curve Inspector",
            width = 8,
            shiny::fluidRow(
              shiny::column(8, shiny::selectInput("metric_curve_name", tooltip_label("Curve name", "Sample/curve identifier to visualize in the single-curve inspector."),
                choices = character(0))),
              shiny::column(4, shiny::selectInput("metric_use_curve", tooltip_label("Segment", "Choose approach or retract segment for single-curve metrics display."),
                choices = c("retract", "approach"), selected = "retract"))
            ),
            tooltip_plot("single_curve_plot", "480px", "Single selected curve with analytical metric annotations. Toggle segment to inspect approach or retract."),
            shiny::downloadButton("download_single_curve", "Download single curve plot", class = "btn-info")
          )
        )
      ),

      # ===========================================================
      # Page 5 -- Summary
      # ===========================================================
      bs4Dash::tabItem(
        tabName = "summary",
        shiny::fluidRow(
          bs4Dash::bs4Card(
            title = "PCA Biplot",
            width = 7,
            tooltip_plot("pca_plot", "460px", "PCA biplot built from selected analytical metrics; points are samples and arrows are feature loadings."),
            shiny::downloadButton("download_pca", "Download PCA plot", class = "btn-info")
          ),
          bs4Dash::bs4Card(
            title = "Violin Plot",
            width = 5,
            shiny::fluidRow(
              shiny::column(6, shiny::selectInput("violin_metric", tooltip_label("Metric", "Analytical metric column to display in violin plot."),
                choices = character(0))),
              shiny::column(6, shiny::selectInput("summary_group_by", tooltip_label("Group by", "Metadata column used to group samples in violin and PCA coloring."),
                choices = c("None" = ""), selected = ""))
            ),
            tooltip_plot("violin_plot", "400px", "Distribution of selected metric across groups from the selected metadata column."),
            shiny::downloadButton("download_violin", "Download violin plot", class = "btn-info")
          )
        )
      )

    ) # tabItems
  )   # dashboardBody
)     # dashboardPage


# ---- Server ------------------------------------------------------------------
server <- function(input, output, session) {

  rv <- shiny::reactiveValues(
    fdobj = NULL,
    status = "No data loaded yet."
  )

  read_metadata_file <- function(file_info) {
    if (is.null(file_info)) {
      return(NULL)
    }

    file_path <- file_info$datapath
    file_name <- file_info$name
    ext <- tolower(tools::file_ext(file_name))

    tryCatch({
      if (ext == "xlsx") {
        if (!requireNamespace("readxl", quietly = TRUE)) {
          shiny::showNotification(
            "Package 'readxl' is required for Excel files. Install with install.packages('readxl').",
            type = "error",
            duration = NULL
          )
          return(NULL)
        }
        as.data.frame(readxl::read_excel(file_path), stringsAsFactors = FALSE, check.names = FALSE)
      } else if (ext == "csv") {
        read.csv(file_path, stringsAsFactors = FALSE, check.names = FALSE)
      } else if (ext == "txt") {
        read.table(file_path, header = TRUE, sep = "\t", stringsAsFactors = FALSE, check.names = FALSE)
      } else {
        shiny::showNotification("Unsupported file type. Use .csv, .txt, or .xlsx.", type = "error")
        NULL
      }
    }, error = function(e) {
      shiny::showNotification(paste("Failed to read file:", e$message), type = "error", duration = NULL)
      NULL
    })
  }

  find_demo_metadata_file <- function() {
    demo_meta_path <- system.file("extdata_demo_metadata.csv", package = "curvana")
    if (nzchar(demo_meta_path) && file.exists(demo_meta_path)) {
      return(demo_meta_path)
    }

    local_demo_meta_path <- file.path(getwd(), "inst", "extdata_demo_metadata.csv")
    if (file.exists(local_demo_meta_path)) {
      return(local_demo_meta_path)
    }

    NULL
  }

  prepare_metadata_for_loading <- function(meta_df, key_col) {
    if (is.null(meta_df)) {
      return(NULL)
    }

    key_col <- trimws(as.character(key_col))
    if (!nzchar(key_col) || !(key_col %in% colnames(meta_df))) {
      shiny::showNotification(
        sprintf("Metadata key column '%s' not found in uploaded file.", key_col),
        type = "error",
        duration = NULL
      )
      return(NULL)
    }

    key_vals <- trimws(as.character(meta_df[[key_col]]))
    if (any(!nzchar(key_vals)) || any(is.na(key_vals))) {
      shiny::showNotification(
        "Metadata key column contains empty/NA values; cannot use it as row names.",
        type = "error",
        duration = NULL
      )
      return(NULL)
    }
    if (anyDuplicated(key_vals) > 0) {
      shiny::showNotification(
        "Metadata key column contains duplicate values; row names must be unique.",
        type = "error",
        duration = NULL
      )
      return(NULL)
    }

    rownames(meta_df) <- key_vals
    meta_df[[key_col]] <- NULL
    meta_df
  }

  cast_value <- function(value, target_type) {
    if (identical(target_type, "logical")) {
      v <- tolower(trimws(as.character(value)))
      if (v %in% c("true",  "t", "1", "yes", "y")) return(TRUE)
      if (v %in% c("false", "f", "0", "no",  "n")) return(FALSE)
      return(NA)
    }
    if (identical(target_type, "integer")) return(suppressWarnings(as.integer(value)))
    if (identical(target_type, "numeric")) return(suppressWarnings(as.numeric(value)))
    as.character(value)
  }

  make_discrete_color_map <- function(values) {
    vals <- sort(unique(trimws(as.character(values))))
    vals <- vals[nzchar(vals) & !is.na(vals)]
    if (length(vals) == 0) {
      return(NULL)
    }
    cols <- grDevices::hcl.colors(length(vals), palette = "Dark 3")
    stats::setNames(cols, vals)
  }

  update_choices <- function(fdobj) {
    md_cols <- colnames(fdobj@metadata)
    if (is.null(md_cols)) md_cols <- character(0)
    md_cols <- trimws(as.character(md_cols))
    md_cols <- unique(md_cols[nzchar(md_cols) & !is.na(md_cols)])

    curve_names <- names(fdobj@rawCurves)
    if (is.null(curve_names)) curve_names <- character(0)
    curve_names <- trimws(as.character(curve_names))
    curve_names <- unique(curve_names[nzchar(curve_names) & !is.na(curve_names)])

    chooser <- c("None" = "", stats::setNames(md_cols, md_cols))

    default_choice <- function(preferred = character(0), fallback_index = 1L) {
      hit <- intersect(preferred, md_cols)
      if (length(hit) > 0) {
        return(hit[[1]])
      }
      if (length(md_cols) >= fallback_index) {
        return(md_cols[[fallback_index]])
      }
      ""
    }

    shiny::updateSelectInput(session, "raw_anno_col1",
      choices = chooser, selected = default_choice(c("surface"), 1L))
    shiny::updateSelectInput(session, "raw_anno_col2",
      choices = chooser, selected = default_choice(c("region"), 2L))
    shiny::updateSelectInput(session, "raw_group_by",
      choices = chooser, selected = default_choice(c("region"), 1L))
    shiny::updateSelectInput(session, "raw_split_by",
      choices = chooser, selected = default_choice(c("surface"), 2L))
    shiny::updateSelectInput(session, "fd_group_by",
      choices = chooser, selected = default_choice(c("region"), 1L))
    shiny::updateSelectInput(session, "fd_split_by",
      choices = chooser, selected = default_choice(c("surface"), 2L))
    shiny::updateSelectInput(session, "summary_group_by",
      choices = chooser, selected = default_choice(c("surface"), 1L))

    curve_choices <- stats::setNames(curve_names, curve_names)
    shiny::updateSelectInput(session, "metric_curve_name",
      choices  = curve_choices,
      selected = if (length(curve_names) > 0) curve_names[[1]] else NULL)
    metric_candidates <- c(
      "adhesive_force_nN_retract",   "adhesive_energy_aJ_retract",
      "repulsive_energy_aJ_retract", "rupture_distance_nm_retract",
      "repulsive_distance_nm_retract",
      "adhesive_force_nN_approach",  "adhesive_energy_aJ_approach",
      "repulsive_energy_aJ_approach","rupture_distance_nm_approach",
      "repulsive_distance_nm_approach"
    )
    metric_choices <- intersect(metric_candidates, md_cols)
    metric_choices <- stats::setNames(metric_choices, metric_choices)
    shiny::updateSelectInput(session, "violin_metric",
      choices  = metric_choices,
      selected = if (length(metric_choices) > 0) unname(metric_choices[[1]]) else NULL)
  }

  # Load Data
  shiny::observeEvent(input$load_btn, {
    shiny::withProgress(message = "Loading data...", value = 0.1, {
      folder <- if (identical(input$data_source, "demo")) {
        system.file("extdata", package = "curvana")
      } else {
        input$custom_folder
      }
      if (!nzchar(folder) || !dir.exists(folder)) {
        shiny::showNotification("Folder path does not exist.", type = "error")
        return(NULL)
      }

      suffix_in <- trimws(as.character(input$load_suffix))
      if (!nzchar(suffix_in)) {
        shiny::showNotification("suffix cannot be empty.", type = "error")
        return(NULL)
      }
      suffix <- if (startsWith(suffix_in, ".")) suffix_in else paste0(".", suffix_in)
      pattern <- as.character(input$load_pattern)

      col_calc_ramp_ex <- trimws(as.character(input$load_calc_ramp_ex_nm))
      col_calc_ramp_rt <- trimws(as.character(input$load_calc_ramp_rt_nm))
      col_defl_v_ex <- trimws(as.character(input$load_defl_v_ex))
      col_defl_v_rt <- trimws(as.character(input$load_defl_v_rt))
      req_cols <- c(col_calc_ramp_ex, col_calc_ramp_rt, col_defl_v_ex, col_defl_v_rt)
      if (any(!nzchar(req_cols))) {
        shiny::showNotification("Column-name arguments cannot be empty.", type = "error")
        return(NULL)
      }

      metadata_for_load <- NULL
      metadata_source_note <- ""
      if (identical(input$data_source, "demo")) {
        demo_meta_path <- find_demo_metadata_file()
        if (is.null(demo_meta_path)) {
          shiny::showNotification("Demo metadata file 'extdata_demo_metadata.csv' could not be found.", type = "error", duration = NULL)
          return(NULL)
        }

        metadata_for_load <- read_metadata_file(list(
          datapath = demo_meta_path,
          name = basename(demo_meta_path)
        ))
        if (is.null(metadata_for_load)) return(NULL)

        metadata_for_load <- prepare_metadata_for_loading(metadata_for_load, "filename")
        if (is.null(metadata_for_load)) return(NULL)

        metadata_source_note <- sprintf("\nDemo metadata source: %s", basename(demo_meta_path))
      } else if (isTRUE(input$use_metadata_for_load)) {
        if (is.null(input$meta_file)) {
          shiny::showNotification("Upload a metadata file first, or uncheck metadata loading option.", type = "error")
          return(NULL)
        }
        metadata_for_load <- read_metadata_file(input$meta_file)
        if (is.null(metadata_for_load)) return(NULL)

        metadata_for_load <- prepare_metadata_for_loading(metadata_for_load, input$meta_key_col)
        if (is.null(metadata_for_load)) return(NULL)
      }

      shiny::incProgress(0.3)
      fdobj <- tryCatch(
        curvana::createFdObjFromFolder(
          folder  = folder,
          suffix = suffix,
          pattern = pattern,
          Calc_Ramp_Ex_nm = col_calc_ramp_ex,
          Calc_Ramp_Rt_nm = col_calc_ramp_rt,
          Defl_V_Ex = col_defl_v_ex,
          Defl_V_Rt = col_defl_v_rt,
          metadata = metadata_for_load,
          threads = max(1L, as.integer(input$threads_load))
        ),
        error = function(e) {
          shiny::showNotification(paste("Load failed:", e$message), type = "error", duration = NULL)
          NULL
        }
      )
      if (is.null(fdobj)) return(NULL)
      shiny::incProgress(0.3)
      rv$fdobj  <- fdobj
      rv$status <- sprintf(
        "Loaded %d raw curves from:\n%s\nGenerated metadata: %d rows x %d columns.\ncreateFdObjFromFolder args: suffix='%s', pattern='%s'.%s",
        length(fdobj@rawCurves),
        folder,
        nrow(fdobj@metadata),
        ncol(fdobj@metadata),
        suffix,
        pattern,
        metadata_source_note
      )
      update_choices(fdobj)
      shiny::incProgress(0.3)
      shiny::showNotification(sprintf("Loaded %d curves.", length(fdobj@rawCurves)), type = "message")
    })
  })

  # Import Metadata
  shiny::observeEvent(input$import_meta_btn, {
    shiny::req(rv$fdobj)
    shiny::req(input$meta_file)

    file_name <- input$meta_file$name
    imported_df <- read_metadata_file(input$meta_file)
    if (is.null(imported_df)) return(NULL)
    md <- rv$fdobj@metadata
    key_col <- trimws(input$meta_key_col)
    if (!nzchar(key_col) || !(key_col %in% colnames(imported_df))) {
      shiny::showNotification(
        sprintf("Key column '%s' not found. Available: %s",
                key_col, paste(colnames(imported_df), collapse = ", ")),
        type = "error", duration = NULL)
      return(NULL)
    }
    matched_idx <- match(rownames(md), as.character(imported_df[[key_col]]))
    if (all(is.na(matched_idx))) {
      shiny::showNotification(
        "No rows matched. Ensure the key column contains the sample names.",
        type = "error", duration = NULL)
      return(NULL)
    }
    imported_df            <- imported_df[matched_idx, , drop = FALSE]
    imported_df[[key_col]] <- NULL
    rownames(imported_df)  <- rownames(md)
    rv$fdobj@metadata <- imported_df
    rv$status <- sprintf("Metadata imported from '%s' (%d rows x %d columns).",
                         file_name, nrow(imported_df), ncol(imported_df))
    update_choices(rv$fdobj)
    shiny::showNotification("Metadata imported successfully.", type = "message")
  })

  # Transform Curves
  shiny::observeEvent(input$transform_btn, {
    shiny::req(rv$fdobj)
    shiny::withProgress(message = "Running transform_curves...", value = 0.1, {
      fdobj     <- rv$fdobj
      least_app <- if (identical(input$least_mode_approach, "automatic")) "automatic" else as.integer(input$least_length_approach)
      least_ret <- if (identical(input$least_mode_retract,  "automatic")) "automatic" else as.integer(input$least_length_retract)
      fdobj <- tryCatch({
        shiny::incProgress(0.35, detail = "Approach")
        fdobj <- curvana::transform_curves(
          fdObj = fdobj, spring_constant = input$spring_constant, useCurve = "approach",
          threads = max(1L, as.integer(input$threads_transform)),
          denoise_first = isTRUE(input$denoise_first),
          p = as.integer(input$denoise_p), n = as.integer(input$denoise_n),
          m = as.integer(input$denoise_m), ts = as.numeric(input$denoise_ts),
          least_length  = least_app,
          slp_threshold = as.numeric(input$slp_threshold),
          std_threshold = as.numeric(input$std_threshold),
          end           = as.integer(input$sens_end)
        )
        shiny::incProgress(0.45, detail = "Retract")
        curvana::transform_curves(
          fdObj = fdobj, spring_constant = input$spring_constant, useCurve = "retract",
          threads = max(1L, as.integer(input$threads_transform)),
          denoise_first = isTRUE(input$denoise_first),
          p = as.integer(input$denoise_p), n = as.integer(input$denoise_n),
          m = as.integer(input$denoise_m), ts = as.numeric(input$denoise_ts),
          least_length  = least_ret,
          slp_threshold = as.numeric(input$slp_threshold),
          std_threshold = as.numeric(input$std_threshold),
          end           = as.integer(input$sens_end)
        )
      }, error = function(e) {
        shiny::showNotification(paste("Transform failed:", e$message), type = "error", duration = NULL)
        NULL
      })
      if (is.null(fdobj)) return(NULL)
      rv$fdobj  <- fdobj
      rv$status <- "Transformation completed for approach and retract curves."
      update_choices(fdobj)
      shiny::incProgress(0.1)
      shiny::showNotification("Transform complete.", type = "message")
    })
  })

  # Analytical Metrics
  shiny::observeEvent(input$metrics_btn, {
    shiny::req(rv$fdobj)
    shiny::withProgress(message = "Computing analytical metrics...", value = 0.2, {
      fdobj <- tryCatch(
        curvana::analyze_curves_all_analytical_metrics(
          fdObj                                    = rv$fdobj,
          useCurve                                 = "both",
          threads                                  = max(1L, as.integer(input$threads_metrics)),
          noise_baseline_span                      = "automatic",
          noise_threshold_method                   = input$noise_threshold_method,
          noise_multiplier                         = as.numeric(input$noise_multiplier),
          noise_quantile_low                       = as.numeric(input$noise_quantile_low),
          noise_quantile_high                      = as.numeric(input$noise_quantile_high),
          analyze_adhesive_force                   = isTRUE(input$do_adhesive_force),
          analyze_energy                           = isTRUE(input$do_energy),
          analyze_rupture_distance                 = isTRUE(input$do_rupture),
          analyze_rupture_distance_baseline_span   = "automatic",
          analyze_rupture_distance_x_direction     = "left",
          analyze_repulsive_distance               = isTRUE(input$do_repulsive),
          analyze_repulsive_distance_baseline_span = "automatic",
          analyze_repulsive_distance_x_direction   = "right"
        ),
        error = function(e) {
          shiny::showNotification(paste("Metrics failed:", e$message), type = "error", duration = NULL)
          NULL
        }
      )
      if (is.null(fdobj)) return(NULL)
      rv$fdobj  <- fdobj
      rv$status <- "Analytical metrics completed."
      update_choices(fdobj)
      shiny::incProgress(0.8)
      shiny::showNotification("Metrics complete.", type = "message")
    })
  })

  # Add metadata column
  shiny::observeEvent(input$add_meta_col_btn, {
    shiny::req(rv$fdobj)
    new_col <- trimws(input$new_meta_col_name)
    if (!nzchar(new_col)) {
      shiny::showNotification("Please provide a non-empty column name.", type = "error")
      return(NULL)
    }
    md <- rv$fdobj@metadata
    if (new_col %in% colnames(md)) {
      shiny::showNotification("Column already exists in metadata.", type = "error")
      return(NULL)
    }
    col_type    <- input$new_meta_col_type
    default_val <- cast_value(input$new_meta_col_default, col_type)
    if ((identical(col_type, "numeric") || identical(col_type, "integer")) &&
        nzchar(trimws(input$new_meta_col_default)) && is.na(default_val)) {
      shiny::showNotification("Default value is not valid for the selected type.", type = "error")
      return(NULL)
    }
    n <- nrow(md)
    md[[new_col]] <- switch(col_type,
      "numeric"  = rep(as.numeric(default_val),  n),
      "integer"  = rep(as.integer(default_val),  n),
      "logical"  = rep(as.logical(default_val),  n),
                   rep(as.character(default_val), n)
    )
    rv$fdobj@metadata <- md
    rv$status <- sprintf("Added metadata column '%s'.", new_col)
    update_choices(rv$fdobj)
  })

  # Handsontable edits
  shiny::observeEvent(input$metadata_hot, {
    shiny::req(rv$fdobj)
    hot_df <- rhandsontable::hot_to_r(input$metadata_hot)
    if (is.null(hot_df) || nrow(hot_df) == 0 || !("sample" %in% colnames(hot_df))) return(NULL)
    md_old     <- rv$fdobj@metadata
    sample_ids <- as.character(hot_df$sample)
    hot_df$sample <- NULL
    if (length(sample_ids) != nrow(hot_df)) return(NULL)
    rownames(hot_df) <- sample_ids
    if (!all(rownames(md_old) %in% rownames(hot_df))) return(NULL)
    hot_df <- hot_df[rownames(md_old), , drop = FALSE]
    for (cn in intersect(colnames(md_old), colnames(hot_df))) {
      old_col      <- md_old[[cn]]
      new_col      <- hot_df[[cn]]
      hot_df[[cn]] <- if (is.factor(old_col)) {
        as.factor(as.character(new_col))
      } else if (is.logical(old_col)) {
        vapply(new_col, function(z) cast_value(z, "logical"), logical(1))
      } else if (is.integer(old_col)) {
        suppressWarnings(as.integer(new_col))
      } else if (is.numeric(old_col)) {
        suppressWarnings(as.numeric(new_col))
      } else {
        as.character(new_col)
      }
    }
    rv$fdobj@metadata <- hot_df
    rv$status <- "Metadata updated from spreadsheet editor."
    update_choices(rv$fdobj)
  }, ignoreInit = TRUE)

  save_plot_png <- function(file, plot_fun, width = 10, height = 7, res = 300) {
    grDevices::png(filename = file, width = width, height = height, units = "in", res = res)
    on.exit(grDevices::dev.off(), add = TRUE)
    plot_fun()
  }

  draw_raw_heatmap <- function() {
    shiny::req(rv$fdobj)
    anno_cols <- c(input$raw_anno_col1, input$raw_anno_col2)
    anno_cols <- intersect(anno_cols[nzchar(anno_cols)], colnames(rv$fdobj@metadata))

    anno_color_list <- list()
    if (length(anno_cols) > 0) {
      for (cn in anno_cols) {
        cmap <- make_discrete_color_map(rv$fdobj@metadata[[cn]])
        if (!is.null(cmap)) {
          anno_color_list[[cn]] <- cmap
        }
      }
    }

    curvana::plot_raw_deflection_heatmap(
      fdobj               = rv$fdobj,
      annotate_columns    = if (length(anno_cols) > 0) anno_cols else NULL,
      annotation_colors   = anno_color_list,
      index_tick_interval = as.integer(input$heatmap_tick_interval),
      show_column_names   = FALSE,
      draw                = TRUE
    )
  }

  draw_raw_curves <- function() {
    shiny::req(rv$fdobj)
    curvana::plot_deflection_curves(
      fdobj           = rv$fdobj,
      curve           = "both",
      group_curves_by = if (nzchar(input$raw_group_by)) input$raw_group_by else NULL,
      split_curves_by = if (nzchar(input$raw_split_by)) input$raw_split_by else NULL,
      alpha = 0.5, point_size = 0.5, line_alpha = 0.3
    )
  }

  draw_fd_curves <- function() {
    shiny::req(rv$fdobj)
    curvana::plot_fd_curves(
      fdobj           = rv$fdobj,
      curve           = "both",
      group_curves_by = if (nzchar(input$fd_group_by)) input$fd_group_by else NULL,
      split_curves_by = if (nzchar(input$fd_split_by)) input$fd_split_by else NULL,
      point_alpha = 0.5, line_alpha = 0.3, point_size = 0.5
    )
  }

  draw_single_curve <- function() {
    shiny::req(rv$fdobj)
    shiny::req(nzchar(input$metric_curve_name))
    curvana::plot_a_curve_metrics(
      fdobj                = rv$fdobj,
      curve_name           = input$metric_curve_name,
      useCurve             = input$metric_use_curve,
      plot_raw             = TRUE,
      base_size            = 13,
      annotation_text_size = 3.2,
      point_alpha          = 0.6
    )
  }

  draw_pca <- function() {
    shiny::req(rv$fdobj)
    md <- rv$fdobj@metadata
    pca_features <- intersect(
      c("adhesive_force_nN_retract",   "adhesive_energy_aJ_retract",
        "repulsive_energy_aJ_retract", "rupture_distance_nm_retract",
        "repulsive_distance_nm_retract"),
      colnames(md)
    )
    if (length(pca_features) < 2) {
      stop("Run analytical metrics first (need at least 2 feature columns).")
    }
    color_by <- if (nzchar(input$summary_group_by) && input$summary_group_by %in% colnames(md)) {
      input$summary_group_by
    } else if ("surface" %in% colnames(md)) {
      "surface"
    } else {
      colnames(md)[1]
    }
    color_map <- make_discrete_color_map(md[[color_by]])
    curvana::plot_pca_biplot(
      df                  = md,
      include_columns     = pca_features,
      color_by            = color_by,
      color_map           = color_map,
      arrow_scale         = 1.1,
      show_feature_labels = TRUE
    )
  }

  draw_violin <- function() {
    shiny::req(rv$fdobj)
    shiny::req(nzchar(input$violin_metric))
    if (!nzchar(input$summary_group_by)) {
      stop("Select a grouping column.")
    }
    md <- rv$fdobj@metadata
    if (!(input$summary_group_by %in% colnames(md))) {
      stop("Grouping column not found.")
    }
    if (!(input$violin_metric %in% colnames(md))) {
      stop("Metric column not found.")
    }
    curvana::plot_metric_violin(
      df          = md,
      metric_name = input$violin_metric,
      group_by    = input$summary_group_by,
      color_by    = input$summary_group_by,
      color_map   = make_discrete_color_map(md[[input$summary_group_by]]),
      base_size   = 11
    )
  }

  # Status text
  output$status_text <- shiny::renderText({
    if (is.null(rv$fdobj)) return(rv$status)
    fdobj <- rv$fdobj
    n_raw <- length(fdobj@rawCurves)
    n_app <- if (length(fdobj@approachCurves) > 0)
      sum(vapply(fdobj@approachCurves, nrow, integer(1)) > 0) else 0L
    n_ret <- if (length(fdobj@retractCurves) > 0)
      sum(vapply(fdobj@retractCurves,  nrow, integer(1)) > 0) else 0L
    paste(rv$status,
          sprintf("Raw curves                  : %d", n_raw),
          sprintf("Transformed approach curves : %d", n_app),
          sprintf("Transformed retract curves  : %d", n_ret),
          sep = "\n")
  })

  # Metadata handsontable
  output$metadata_hot <- rhandsontable::renderRHandsontable({
    shiny::validate(shiny::need(!is.null(rv$fdobj), "Load curve data to preview the metadata generated by createFdObjFromFolder."))
    md   <- rv$fdobj@metadata
    disp <- data.frame(
      sample = rownames(md),
      md,
      check.names = FALSE,
      stringsAsFactors = FALSE
    )
    rhandsontable::rhandsontable(disp, rowHeaders = NULL, stretchH = "all", height = 520) |>
      rhandsontable::hot_col("sample", readOnly = TRUE) |>
      rhandsontable::hot_table(
        contextMenu = TRUE,
        manualColumnResize = TRUE,
        fillHandle = TRUE,
        columnSorting = TRUE
      )
  })

  output$download_metadata_csv <- shiny::downloadHandler(
    filename = function() {
      paste0("curvana_metadata_", Sys.Date(), ".csv")
    },
    content = function(file) {
      shiny::req(rv$fdobj)
      md <- rv$fdobj@metadata
      out <- data.frame(sample = rownames(md), md, check.names = FALSE, stringsAsFactors = FALSE)
      utils::write.csv(out, file = file, row.names = FALSE, na = "")
    }
  )

  # Raw heatmap
  output$raw_heatmap <- shiny::renderPlot({
    draw_raw_heatmap()
  })

  output$download_raw_heatmap <- shiny::downloadHandler(
    filename = function() {
      paste0("curvana_raw_deflection_heatmap_", Sys.Date(), ".png")
    },
    content = function(file) {
      save_plot_png(file, draw_raw_heatmap, width = 12, height = 9, res = 300)
    }
  )

  # Raw deflection curves
  output$raw_curves_plot <- shiny::renderPlot({
    draw_raw_curves()
  })

  output$download_raw_curves <- shiny::downloadHandler(
    filename = function() {
      paste0("curvana_raw_deflection_curves_", Sys.Date(), ".png")
    },
    content = function(file) {
      save_plot_png(file, draw_raw_curves, width = 12, height = 8, res = 300)
    }
  )

  # FD curves
  output$fd_curves_plot <- shiny::renderPlot({
    draw_fd_curves()
  })

  output$download_fd_curves <- shiny::downloadHandler(
    filename = function() {
      paste0("curvana_transformed_fd_curves_", Sys.Date(), ".png")
    },
    content = function(file) {
      save_plot_png(file, draw_fd_curves, width = 12, height = 8, res = 300)
    }
  )

  # Single curve inspector
  output$single_curve_plot <- shiny::renderPlot({
    draw_single_curve()
  })

  output$download_single_curve <- shiny::downloadHandler(
    filename = function() {
      paste0("curvana_single_curve_", Sys.Date(), ".png")
    },
    content = function(file) {
      save_plot_png(file, draw_single_curve, width = 14, height = 7, res = 300)
    }
  )

  # PCA biplot
  output$pca_plot <- shiny::renderPlot({
    draw_pca()
  })

  output$download_pca <- shiny::downloadHandler(
    filename = function() {
      paste0("curvana_pca_biplot_", Sys.Date(), ".png")
    },
    content = function(file) {
      save_plot_png(file, draw_pca, width = 10, height = 7, res = 300)
    }
  )

  # Violin plot
  output$violin_plot <- shiny::renderPlot({
    draw_violin()
  })

  output$download_violin <- shiny::downloadHandler(
    filename = function() {
      paste0("curvana_violin_plot_", Sys.Date(), ".png")
    },
    content = function(file) {
      save_plot_png(file, draw_violin, width = 9, height = 7, res = 300)
    }
  )
}

shiny::shinyApp(ui = ui, server = server)

