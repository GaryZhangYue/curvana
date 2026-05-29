
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
      bs4Dash::menuItem("Results",            tabName = "results",   icon = shiny::icon("table")),
      bs4Dash::menuItem("Figures",            tabName = "figures",   icon = shiny::icon("chart-line")),
      bs4Dash::menuItem("Download",           tabName = "download",  icon = shiny::icon("download"))
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
          "}\n",
          ".pca-checkbox-grid .shiny-options-group {\n",
          "  display: grid;\n",
          "  grid-template-columns: repeat(3, minmax(0, 1fr));\n",
          "  gap: 6px 14px;\n",
          "}\n",
          ".pca-checkbox-grid .checkbox {\n",
          "  margin-top: 0;\n",
          "  margin-bottom: 0;\n",
          "}\n",
          "@media (max-width: 900px) {\n",
          "  .pca-checkbox-grid .shiny-options-group {\n",
          "    grid-template-columns: repeat(2, minmax(0, 1fr));\n",
          "  }\n",
          "}\n",
          "@media (max-width: 600px) {\n",
          "  .pca-checkbox-grid .shiny-options-group {\n",
          "    grid-template-columns: 1fr;\n",
          "  }\n",
          "}\n",
          ".shiny-notification {\n",
          "  max-width: 500px;\n",
          "  width: auto;\n",
          "  white-space: pre-wrap;\n",
          "  word-wrap: break-word;\n",
          "  overflow-wrap: break-word;\n",
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
                  " Choose the appropriate file format (Bruker NanoScope/Veeco, JPK, or Generic column-separated).",
                  " The metadata will be generated automatically after import.",
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
                  shiny::tags$strong("Results"), " \u2014 Review the current metadata table and export it as CSV or XLSX."
                ),
                shiny::tags$li(
                  shiny::tags$strong("Figures"), " \u2014 Compare results across samples and conditions",
                  " using PCA biplots and violin plots."
                ),
                shiny::tags$li(
                  shiny::tags$strong("Download"), " \u2014 Download all raw curves, transformed curves, or the full fdobj object."
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
            shiny::div(
              tooltip_label(
                shiny::tags$span(style = "font-size: 1.15em; font-weight: 600;", "Data source"),
                "Use the package's built-in demo data or provide a folder path to your own raw curves."
              )
            ),
            shiny::div(style = "margin-top: 8px;",
              shiny::radioButtons(
                "data_source",
                label = NULL,
                choices  = c("Built-in demo data" = "demo", "Custom folder path" = "custom"),
                selected = "demo"
              )
            ),
            shiny::conditionalPanel(
              condition = "input.data_source == 'custom'",
              shiny::textInput(
                "custom_folder", tooltip_label("Folder path", "Path to the folder containing the raw AFM curve files."), value = "",
                placeholder = "e.g. C:/data/my_experiment"
              ),
              shiny::textInput("load_suffix", tooltip_label("File suffix", "File extension used for the raw AFM curve files, for example, .txt."), value = ".txt"),
              shiny::textInput("load_pattern", tooltip_label("File name pattern", "Optional filename pattern used to filter files before loading them."), value = ""),
              shiny::div(
                tooltip_label(
                  shiny::tags$span(style = "font-size: 1.15em; font-weight: 600;", "File format"),
                  "Select the format of your AFM curve files."
                )
              ),
              shiny::div(style = "margin-top: 8px;",
                shiny::radioButtons(
                  "file_format_tabs",
                  label = NULL,
                  choiceNames = list(
                  shiny::HTML("<strong>Bruker NanoScope/Veeco ASCII</strong> <a href='examples/input.example.nanoscopeASCII.txt' target='_blank' style='font-size: 0.85em; margin-left: 6px;'>(View example file)</a><br><span style='font-size: 0.9em; color: #6c757d;'>Column-separated format where each displacement-deflection pair represents one curve segment. Approach: displacement column Calc_Ramp_Ex_nm, deflection column Defl_V_Ex (reversed). Retract: displacement column Calc_Ramp_Rt_nm (reversed), deflection column Defl_V_Rt. Files may contain approach only, retract only, or both segments. Defl_V_Ex and Calc_Ramp_Rt_nm will be reversed so that both curves begin with the contact region. Any rows starting with '#' or '\"' will be omitted. </span>"),
                  shiny::HTML("<strong>JPK ASCII Format</strong> <a href='examples/input.example.JPKASCII.txt' target='_blank' style='font-size: 0.85em; margin-left: 6px;'>(View example file)</a><br><span style='font-size: 0.9em; color: #6c757d;'>JPK text export files contain approach (extend) and retract segments marked by segment headers. The function automatically detects segments and extracts sensitivity and spring constant values separately for each. Users are encouraged to import deflection data in Voltage. If force data is provided (indicated by 'N' unit), it will be converted back to Voltage using the associated sensitivity and spring constant values. By default, the displacement and deflection columns of approach segment will be reversed so that both curves begin with the contact region. </span>"),
                  shiny::HTML("<strong>Generic Column-Separated Format</strong> <a href='examples/input.example.generic.txt' target='_blank' style='font-size: 0.85em; margin-left: 6px;'>(View example file)</a><br><span style='font-size: 0.9em; color: #6c757d;'>Generic format for column-separated AFM data files. Piezo displacement and cantilever deflection voltage are stored in separate columns. Files can contain approach segment only, retract segment only, or both segments. Please correctly specify the column names for displacement and deflection of each segment in the input files. If a segment is missing, leave the corresponding column name blank. Any rows starting with '#' or '\"' will be omitted. </span>")
                ),
                choiceValues = list(
                  "Bruker NanoScope/Veeco ASCII",
                  "JPK ASCII Format",
                  "Generic Column-Separated Format"
                ),
                selected = "Bruker NanoScope/Veeco ASCII"
              )
              ),
              
              # Bruker NanoScope/Veeco ASCII Format
              shiny::conditionalPanel(
                condition = "input.file_format_tabs == 'Bruker NanoScope/Veeco ASCII'",
                shiny::tags$div(style = "font-size: 1.15em; font-weight: 600; margin-top: 0px; margin-bottom: 8px;", "Data Column Mapping"),
                shiny::fluidRow(
                shiny::column(6,
                  shiny::fluidRow(
                    shiny::column(8,
                      shiny::textInput("load_calc_ramp_ex_nm", tooltip_label("Displacement (approach, nm)", "Column name for piezo displacement in the approach segment of the raw files."), value = "Calc_Ramp_Ex_nm")
                    ),
                    shiny::column(4,
                      shiny::tags$br(),
                      shiny::checkboxInput("reverse_calc_ramp_ex_nm", tooltip_label("Reverse", "If checked, reverse the order of values before constructing raw curves."), value = FALSE)
                    )
                  )
                ),
                shiny::column(6,
                  shiny::fluidRow(
                    shiny::column(8,
                      shiny::textInput("load_calc_ramp_rt_nm", tooltip_label("Displacement (retract, nm)", "Column name for piezo displacement in the retract segment of the raw files."), value = "Calc_Ramp_Rt_nm")
                    ),
                    shiny::column(4,
                      shiny::tags$br(),
                      shiny::checkboxInput("reverse_calc_ramp_rt_nm", tooltip_label("Reverse", "If checked, reverse the order of values before constructing raw curves."), value = TRUE)
                    )
                  )
                )
              ),
              shiny::fluidRow(
                shiny::column(6,
                  shiny::fluidRow(
                    shiny::column(8,
                      shiny::textInput("load_defl_v_ex", tooltip_label("Deflection (approach, V)", "Column name for cantilever deflection in the approach segment of the raw files."), value = "Defl_V_Ex")
                    ),
                    shiny::column(4,
                      shiny::tags$br(),
                      shiny::checkboxInput("reverse_defl_v_ex", tooltip_label("Reverse", "If checked, reverse the order of values before constructing raw curves."), value = TRUE)
                    )
                  )
                ),
                shiny::column(6,
                  shiny::fluidRow(
                    shiny::column(8,
                      shiny::textInput("load_defl_v_rt", tooltip_label("Deflection (retract, V)", "Column name for cantilever deflection in the retract segment of the raw files."), value = "Defl_V_Rt")
                    ),
                    shiny::column(4,
                      shiny::tags$br(),
                      shiny::checkboxInput("reverse_defl_v_rt", tooltip_label("Reverse", "If checked, reverse the order of values before constructing raw curves."), value = FALSE)
                    )
                  )
                )
              )
              ),
              
              # JPK ASCII Format
              shiny::conditionalPanel(
                condition = "input.file_format_tabs == 'JPK ASCII Format'",
                shiny::tags$div(style = "font-size: 1.15em; font-weight: 600; margin-top: 0px; margin-bottom: 8px;", "Data Column Mapping"),
                shiny::fluidRow(
                  shiny::column(6,
                    shiny::textInput("jpk_height_col", tooltip_label("Height/Displacement Column", "Column name for height/distance data in JPK files."), value = "height")
                  ),
                  shiny::column(6,
                    shiny::textInput("jpk_deflection_col", tooltip_label("Deflection Column", "Column name for deflection data in JPK files."), value = "vDeflection")
                  )
                ),
                shiny::fluidRow(
                  shiny::column(6,
                    shiny::fluidRow(
                      shiny::column(8,
                        shiny::tags$label("Displacement (approach)")
                      ),
                      shiny::column(4,
                        shiny::tags$br(),
                        shiny::checkboxInput("jpk_reverse_calc_ramp_ex_nm", tooltip_label("Reverse", "Reverse approach displacement."), value = TRUE)
                      )
                    )
                  ),
                  shiny::column(6,
                    shiny::fluidRow(
                      shiny::column(8,
                        shiny::tags$label("Displacement (retract)")
                      ),
                      shiny::column(4,
                        shiny::tags$br(),
                        shiny::checkboxInput("jpk_reverse_calc_ramp_rt_nm", tooltip_label("Reverse", "Reverse retract displacement."), value = FALSE)
                      )
                    )
                  )
                ),
                shiny::fluidRow(
                  shiny::column(6,
                    shiny::fluidRow(
                      shiny::column(8,
                        shiny::tags$label("Deflection (approach)")
                      ),
                      shiny::column(4,
                        shiny::tags$br(),
                        shiny::checkboxInput("jpk_reverse_defl_v_ex", tooltip_label("Reverse", "Reverse approach deflection."), value = TRUE)
                      )
                    )
                  ),
                  shiny::column(6,
                    shiny::fluidRow(
                      shiny::column(8,
                        shiny::tags$label("Deflection (retract)")
                      ),
                      shiny::column(4,
                        shiny::tags$br(),
                        shiny::checkboxInput("jpk_reverse_defl_v_rt", tooltip_label("Reverse", "Reverse retract deflection."), value = FALSE)
                      )
                    )
                  )
                )
              ),
              
              # Generic Column-Separated Format
              shiny::conditionalPanel(
                condition = "input.file_format_tabs == 'Generic Column-Separated Format'",
                shiny::tags$div(style = "font-size: 1.15em; font-weight: 600; margin-top: 0px; margin-bottom: 8px;", "Data Column Mapping"),
                shiny::fluidRow(
                  shiny::column(6,
                    shiny::fluidRow(
                      shiny::column(8,
                        shiny::textInput("generic_calc_ramp_ex_nm", tooltip_label("Displacement (approach, nm)", "Column name for piezo displacement in the approach segment."), value = "")
                      ),
                      shiny::column(4,
                        shiny::tags$br(),
                        shiny::checkboxInput("generic_reverse_calc_ramp_ex_nm", tooltip_label("Reverse", "Reverse the order of values."), value = FALSE)
                      )
                    )
                  ),
                  shiny::column(6,
                    shiny::fluidRow(
                      shiny::column(8,
                        shiny::textInput("generic_calc_ramp_rt_nm", tooltip_label("Displacement (retract, nm)", "Column name for piezo displacement in the retract segment."), value = "")
                      ),
                      shiny::column(4,
                        shiny::tags$br(),
                        shiny::checkboxInput("generic_reverse_calc_ramp_rt_nm", tooltip_label("Reverse", "Reverse the order of values."), value = FALSE)
                      )
                    )
                  )
                ),
                shiny::fluidRow(
                  shiny::column(6,
                    shiny::fluidRow(
                      shiny::column(8,
                        shiny::textInput("generic_defl_v_ex", tooltip_label("Deflection (approach, V)", "Column name for cantilever deflection in the approach segment."), value = "")
                      ),
                      shiny::column(4,
                        shiny::tags$br(),
                        shiny::checkboxInput("generic_reverse_defl_v_ex", tooltip_label("Reverse", "Reverse the order of values."), value = FALSE)
                      )
                    )
                  ),
                  shiny::column(6,
                    shiny::fluidRow(
                      shiny::column(8,
                        shiny::textInput("generic_defl_v_rt", tooltip_label("Deflection (retract, V)", "Column name for cantilever deflection in the retract segment."), value = "")
                      ),
                      shiny::column(4,
                        shiny::tags$br(),
                        shiny::checkboxInput("generic_reverse_defl_v_rt", tooltip_label("Reverse", "Reverse the order of values."), value = FALSE)
                      )
                    )
                  )
                )
              ),
              shiny::tags$br(),
              shiny::checkboxInput(
                "use_metadata_for_load",
                tooltip_label("Use uploaded metadata", "When enabled, use the uploaded metadata file to select which raw curve files to load from the folder."),
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
              shiny::numericInput("threads_load", tooltip_label("Threads", "Number of parallel workers to use when loading files."), value = 1, min = 1, step = 1)
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
              shiny::column(3, shiny::textInput("new_meta_col_name", tooltip_label("New column name", "Name of the metadata column to add."), value = "")),
              shiny::column(3, shiny::selectInput("new_meta_col_type", tooltip_label("Column type", "Data type to use for the new metadata column."),
                choices = c("character", "numeric", "integer", "logical"), selected = "character")),
              shiny::column(3, shiny::textInput("new_meta_col_default", tooltip_label("Default value", "Initial value to fill into all rows of the new metadata column."), value = "")),
              shiny::column(3, shiny::tags$br(),
                shiny::actionButton("add_meta_col_btn", "Add column", class = "btn-secondary"))
            ),
            shiny::fluidRow(
              shiny::column(6,
                shiny::downloadButton("download_metadata_csv", "Download metadata (.csv)", class = "btn-info btn-block")
              ),
              shiny::column(6,
                shiny::downloadButton("download_metadata_xlsx", "Download metadata (.xlsx)", class = "btn-info btn-block")
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
          bs4Dash::bs4Card(
            title       = "Raw Deflection Curves by Data Point Index",
            width       = 12,
            status      = "warning",
            solidHeader = TRUE,
            shiny::helpText(
              "This plot shows raw deflection values against the row index (data point number) for each curve. This helps verify data orientation (contact region at beginning, non-interaction region at end), visualize the number of data points in each curve, and help select appropriate regions for baseline calibration and sensitivity calculation."
            ),
            shiny::fluidRow(
              shiny::column(4, shiny::selectInput("raw_index_group_by", tooltip_label("Color by", "Metadata column to use for coloring curves."),
                choices = c("None" = ""), selected = "")),
              shiny::column(4, shiny::selectInput("raw_index_split_by", tooltip_label("Facet by", "Metadata column to use for splitting curves into subpanels."),
                choices = c("None" = ""), selected = "")),
              shiny::column(4,
                shiny::tags$br(),
                shiny::actionButton("plot_raw_index_btn", "Plot", class = "btn-warning btn-block", icon = shiny::icon("chart-line"))
              )
            ),
            tooltip_plot("raw_index_plot", "370px", "Raw deflection vs data point index. Each curve shows deflection values plotted against row number."),
            shiny::fluidRow(
              shiny::column(3, shiny::numericInput("raw_index_download_width", tooltip_label("Download width (in)", "Width in inches for exported plot."), value = 12, min = 1, step = 0.5)),
              shiny::column(3, shiny::numericInput("raw_index_download_height", tooltip_label("Download height (in)", "Height in inches for exported plot."), value = 9, min = 1, step = 0.5)),
              shiny::column(6,
                shiny::tags$br(),
                shiny::downloadButton("download_raw_index_plot", "Download plot", class = "btn-info btn-block")
              )
            )
          )
        ),

        shiny::fluidRow(
          bs4Dash::bs4Card(
            title       = "Curve Transform Settings",
            width       = 12,
            status      = "warning",
            solidHeader = TRUE,
            shiny::fluidRow(
              shiny::column(6,
                shiny::checkboxInput(
                  "transform_approach",
                  tooltip_label("Transform approach curves", "Uncheck to skip transforming approach curves."),
                  value = TRUE
                )
              ),
              shiny::column(6,
                shiny::checkboxInput(
                  "transform_retract",
                  tooltip_label("Transform retract curves", "Uncheck to skip transforming retract curves."),
                  value = TRUE
                )
              )
            ),
            shiny::fluidRow(
              shiny::column(
                width = 6,
                bs4Dash::bs4Card(
                  title = "Approach Curve Settings",
                  width = NULL,
                  collapsible = TRUE,
                  shiny::radioButtons("spring_constant_mode_approach", tooltip_label("Spring constant source", "Choose whether to use one fixed spring constant for all curves or read per-curve values from a metadata column."),
                    choices = c("Fixed value" = "fixed", "Metadata column" = "column"), selected = "fixed", inline = TRUE),
                  shiny::conditionalPanel(
                    condition = "input.spring_constant_mode_approach == 'fixed'",
                    shiny::numericInput("spring_constant_value_approach", tooltip_label("Spring constant (nN/nm)", "Cantilever spring constant applied to all approach curves."), value = 0.08, min = 0, step = 0.001)
                  ),
                  shiny::conditionalPanel(
                    condition = "input.spring_constant_mode_approach == 'column'",
                    shiny::selectInput("spring_constant_col_approach", tooltip_label("Spring constant column", "Metadata column containing per-curve spring constant values in nN/nm."), choices = character(0))
                  ),
                  shiny::checkboxInput("denoise_first_approach", tooltip_label("Denoise with Savitzky-Golay filter", "Apply Savitzky-Golay smoothing before baseline and sensitivity calculations for approach curves."), value = TRUE),
                  shiny::fluidRow(
                    shiny::column(6, shiny::numericInput("denoise_p_approach", tooltip_label("Polynomial degree", "Polynomial order p used by the Savitzky-Golay filter."), value = 1, min = 0, step = 1)),
                    shiny::column(6, shiny::numericInput("denoise_n_approach", tooltip_label("Window size", "Window size n used by the Savitzky-Golay filter. This must be an odd integer."), value = 3, min = 3, step = 2)),
                    shiny::column(6, shiny::numericInput("denoise_m_approach", tooltip_label("Derivative order", "Derivative order m used by the Savitzky-Golay filter. Use 0 for standard denoising."), value = 0, min = 0, step = 1)),
                    shiny::column(6, shiny::numericInput("denoise_ts_approach", tooltip_label("Sample spacing", "Sampling interval ts used by the Savitzky-Golay filter."), value = 1, min = 0.0001, step = 0.1))
                  ),
                  shiny::hr(),
                  shiny::h5("Baseline span", style = "font-size: 18px; font-weight: bold;"),
                  shiny::fluidRow(
                    shiny::column(6, shiny::selectInput("least_mode_approach", tooltip_label("Mode", "Choose 'fixed' to use the value in least_length, or 'automatic' to use the column 'baseline_span_approach' from the metadata."),
                      choices = c("fixed", "automatic"), selected = "fixed")),
                    shiny::column(6, shiny::numericInput("least_length_approach", tooltip_label("Least length", "Minimum baseline span from the right side of the curve in terms of the number of data points for the approach curve. This value is used only when baseline mode is set to 'fixed'."),
                      value = 100, min = 1, step = 1))
                  ),
                  shiny::hr(),
                  shiny::h5("Baseline threshold", style = "font-size: 18px; font-weight: bold;"),
                  shiny::numericInput("slp_threshold_approach", tooltip_label("Maximum slope (absolute value)", "Discard approach curves whose baseline slope (absolute value) exceeds this threshold."),
                    value = 0.01, min = 0, step = 0.001),
                  shiny::numericInput("std_threshold_approach", tooltip_label("Maximum standard deviation of deflection", "Discard approach curves whose baseline deflection SD exceeds this threshold."),
                    value = 0.01, min = 0, step = 0.001),
                  shiny::hr(),
                  shiny::h5("Sensitivity Calculation", style = "font-size: 18px; font-weight: bold;"),
                  shiny::numericInput("sens_end_approach", tooltip_label("End of contact region", "Maximum number of data points from the left side of the curve to use for sensitivity calibration."), value = 100, min = 1, step = 1),
                  shiny::numericInput("intv_approach", tooltip_label("Chunk size (intv)", "Number of data points added per iteration when building the linear sensitivity segment."), value = 4, min = 1, step = 1),
                  shiny::numericInput("R_squared_min_approach", tooltip_label("R² minimum", "Minimum R² threshold for accepting a segment as sufficiently linear during sensitivity calculation."), value = 0.99, min = 0, max = 1, step = 0.001),
                  shiny::numericInput("minimum_length_approach", tooltip_label("Minimum segment length", "Minimum number of accumulated points required for a valid sensitivity result. If the segment is shorter, sensitivity is reported as NA."), value = 4, min = 1, step = 1),
                  shiny::checkboxInput("soft_approach", tooltip_label("Soft substrate", "Enable this if the curves were measured on a soft substrate. You will need to provide the probe sensitivity measured on a hard reference surface."), value = FALSE),
                  shiny::conditionalPanel(
                    condition = "input.soft_approach == true",
                    shiny::radioButtons("soft_sens_mode_approach", tooltip_label("Probe sensitivity source", "Choose whether to use one fixed probe sensitivity for all curves or read per-curve values from a metadata column."),
                      choices = c("Fixed value" = "fixed", "Metadata column" = "column"), selected = "fixed", inline = TRUE),
                    shiny::conditionalPanel(
                      condition = "input.soft_sens_mode_approach == 'fixed'",
                      shiny::numericInput("soft_sens_value_approach", tooltip_label("Probe sensitivity (V/nm)", "Probe sensitivity measured on a hard reference surface and applied to all curves."), value = NA, step = 0.0001)
                    ),
                    shiny::conditionalPanel(
                      condition = "input.soft_sens_mode_approach == 'column'",
                      shiny::selectInput("soft_sens_col_approach", tooltip_label("Sensitivity column", "Metadata column containing per-curve probe sensitivity values measured on a hard reference surface."), choices = character(0))
                    )
                  ),
                    shiny::hr(),
                  shiny::numericInput("threads_transform_approach", tooltip_label("Threads", "Number of workers used for transforming approach curves."), value = 1, min = 1, step = 1)
                )
              ),
              shiny::column(
                width = 6,
                bs4Dash::bs4Card(
                  title = "Retract Curve Settings",
                  width = NULL,
                  collapsible = TRUE,
                  shiny::radioButtons("spring_constant_mode_retract", tooltip_label("Spring constant source", "Choose whether to use one fixed spring constant for all curves or read per-curve values from a metadata column."),
                    choices = c("Fixed value" = "fixed", "Metadata column" = "column"), selected = "fixed", inline = TRUE),
                  shiny::conditionalPanel(
                    condition = "input.spring_constant_mode_retract == 'fixed'",
                    shiny::numericInput("spring_constant_value_retract", tooltip_label("Spring constant (nN/nm)", "Cantilever spring constant applied to all retract curves."), value = 0.08, min = 0, step = 0.001)
                  ),
                  shiny::conditionalPanel(
                    condition = "input.spring_constant_mode_retract == 'column'",
                    shiny::selectInput("spring_constant_col_retract", tooltip_label("Spring constant column", "Metadata column containing per-curve spring constant values in nN/nm."), choices = character(0))
                  ),
                  shiny::checkboxInput("denoise_first_retract", tooltip_label("Denoise with Savitzky-Golay filter", "Apply Savitzky-Golay smoothing before baseline and sensitivity calculations for retract curves."), value = TRUE),
                  shiny::fluidRow(
                    shiny::column(6, shiny::numericInput("denoise_p_retract", tooltip_label("Polynomial degree", "Polynomial order p used by the Savitzky-Golay filter."), value = 1, min = 0, step = 1)),
                    shiny::column(6, shiny::numericInput("denoise_n_retract", tooltip_label("Window size", "Window size n used by the Savitzky-Golay filter. This must be an odd integer."), value = 3, min = 3, step = 2)),
                    shiny::column(6, shiny::numericInput("denoise_m_retract", tooltip_label("Derivative order", "Derivative order m used by the Savitzky-Golay filter. Use 0 for standard denoising."), value = 0, min = 0, step = 1)),
                    shiny::column(6, shiny::numericInput("denoise_ts_retract", tooltip_label("Sample spacing", "Sampling interval ts used by the Savitzky-Golay filter."), value = 1, min = 0.0001, step = 0.1))
                  ),
                  shiny::hr(),
                  shiny::h5("Baseline span", style = "font-size: 18px; font-weight: bold;"),
                  shiny::fluidRow(
                    shiny::column(6, shiny::selectInput("least_mode_retract", tooltip_label("Mode", "Choose 'fixed' to use the value in least_length, or 'automatic' to use baseline_span_retract from the metadata."),
                      choices = c("fixed", "automatic"), selected = "fixed")),
                    shiny::column(6, shiny::numericInput("least_length_retract", tooltip_label("Least length", "Minimum baseline span from the right side of the curve in terms of the number of data points for the retract curve. This value is used only when baseline mode is set to 'fixed'."),
                      value = 100, min = 1, step = 1))
                  ),
                  shiny::hr(),
                  shiny::h5("Baseline threshold", style = "font-size: 18px; font-weight: bold;"),
                  shiny::numericInput("slp_threshold_retract", tooltip_label("Maximum slope (absolute value)", "Discard retract curves whose baseline slope (absolute value) exceeds this threshold."),
                    value = 0.01, min = 0, step = 0.001),
                  shiny::numericInput("std_threshold_retract", tooltip_label("Maximum standard deviation of deflection", "Discard retract curves whose baseline deflection SD exceeds this threshold."),
                    value = 0.01, min = 0, step = 0.001),
                  shiny::hr(),
                  shiny::h5("Sensitivity Calculation", style = "font-size: 18px; font-weight: bold;"),
                  shiny::numericInput("sens_end_retract", tooltip_label("End of contact region", "Maximum number of data points from the left side of the curve to use for sensitivity calibration."), value = 100, min = 1, step = 1),
                  shiny::numericInput("intv_retract", tooltip_label("Chunk size (intv)", "Number of data points added per iteration when building the linear sensitivity segment."), value = 4, min = 1, step = 1),
                  shiny::numericInput("R_squared_min_retract", tooltip_label("R² minimum", "Minimum R² threshold for accepting a segment as sufficiently linear during sensitivity calculation."), value = 0.99, min = 0, max = 1, step = 0.001),
                  shiny::numericInput("minimum_length_retract", tooltip_label("Minimum segment length", "Minimum number of accumulated points required for a valid sensitivity result. If the segment is shorter, sensitivity is reported as NA."), value = 4, min = 1, step = 1),
                  shiny::checkboxInput("soft_retract", tooltip_label("Soft substrate", "Enable this if the curves were measured on a soft substrate. You will need to provide the probe sensitivity measured on a hard reference surface."), value = FALSE),
                  shiny::conditionalPanel(
                    condition = "input.soft_retract == true",
                    shiny::radioButtons("soft_sens_mode_retract", tooltip_label("Probe sensitivity source", "Choose whether to use one fixed probe sensitivity for all curves or read per-curve values from a metadata column."),
                      choices = c("Fixed value" = "fixed", "Metadata column" = "column"), selected = "fixed", inline = TRUE),
                    shiny::conditionalPanel(
                      condition = "input.soft_sens_mode_retract == 'fixed'",
                      shiny::numericInput("soft_sens_value_retract", tooltip_label("Probe sensitivity (V/nm)", "Probe sensitivity measured on a hard reference surface and applied to all curves."), value = NA, step = 0.0001)
                    ),
                    shiny::conditionalPanel(
                      condition = "input.soft_sens_mode_retract == 'column'",
                      shiny::selectInput("soft_sens_col_retract", tooltip_label("Sensitivity column", "Metadata column containing per-curve probe sensitivity values measured on a hard reference surface."), choices = character(0))
                    )
                  ),
                    shiny::hr(),
                  shiny::numericInput("threads_transform_retract", tooltip_label("Threads", "Number of workers used for transforming retract curves."), value = 1, min = 1, step = 1)
                )
              )
            ),
            shiny::tags$div(
              style = "margin-top:12px;",
              shiny::actionButton("transform_btn", "Run Transform", class = "btn-warning btn-block",
                                  icon = shiny::icon("play"))
            )
          )
        ),

        shiny::fluidRow(
          bs4Dash::bs4Card(
            title = "Raw Deflection Curves",
            width = 6,
            status = "warning",
            solidHeader = TRUE,
            shiny::fluidRow(
              shiny::column(4, shiny::selectInput("raw_group_by", tooltip_label("Color by", "Metadata column used to color the raw deflection curves."),
                choices = c("None" = ""), selected = "")),
              shiny::column(4, shiny::selectInput("raw_split_by", tooltip_label("Split by", "Metadata column used to split the raw deflection curves into panels."),
                choices = c("None" = ""), selected = "")),
              shiny::column(4,
                shiny::tags$br(),
                shiny::actionButton("plot_raw_curves_btn", "Plot", class = "btn-warning btn-block", icon = shiny::icon("chart-line"))
              )
            ),
            tooltip_plot("raw_curves_plot", "640px", "Raw deflection curves across samples. Use Color by and Split by to compare groups defined in the metadata."),
            shiny::fluidRow(
              shiny::column(3, shiny::numericInput("raw_curves_download_width", tooltip_label("Download width (in)", "Width in inches for exported raw deflection curves plot."), value = 12, min = 1, step = 0.5)),
              shiny::column(3, shiny::numericInput("raw_curves_download_height", tooltip_label("Download height (in)", "Height in inches for exported raw deflection curves plot."), value = 8, min = 1, step = 0.5)),
              shiny::column(6,
                shiny::tags$br(),
                shiny::downloadButton("download_raw_curves", "Download raw curves plot", class = "btn-info btn-block")
              )
            )
          ),
          bs4Dash::bs4Card(
            title = "FD Curves (Transformed)",
            width = 6,
            status = "warning",
            solidHeader = TRUE,
            shiny::fluidRow(
              shiny::column(4, shiny::selectInput("fd_group_by", tooltip_label("Color by", "Metadata column used to color the transformed force-distance curves."),
                choices = c("None" = ""), selected = "")),
              shiny::column(4, shiny::selectInput("fd_split_by", tooltip_label("Split by", "Metadata column used to split the transformed force-distance curves into panels."),
                choices = c("None" = ""), selected = "")),
              shiny::column(4,
                shiny::tags$br(),
                shiny::actionButton("plot_fd_curves_btn", "Plot", class = "btn-warning btn-block", icon = shiny::icon("chart-line"))
              )
            ),
            shiny::fluidRow(
              shiny::column(3, shiny::numericInput("fd_xmin", tooltip_label("X min", "Lower x-axis limit (separation distance, nm). Leave blank for auto."), value = NA, step = 1)),
              shiny::column(3, shiny::numericInput("fd_xmax", tooltip_label("X max", "Upper x-axis limit (separation distance, nm). Leave blank for auto."), value = NA, step = 1)),
              shiny::column(3, shiny::numericInput("fd_ymin", tooltip_label("Y min", "Lower y-axis limit (force, nN). Leave blank for auto."), value = NA, step = 0.1)),
              shiny::column(3, shiny::numericInput("fd_ymax", tooltip_label("Y max", "Upper y-axis limit (force, nN). Leave blank for auto."), value = NA, step = 0.1))
            ),
            tooltip_plot("fd_curves_plot", "640px", "Transformed force-distance curves."),
            shiny::fluidRow(
              shiny::column(3, shiny::numericInput("fd_curves_download_width", tooltip_label("Download width (in)", "Width in inches for exported transformed FD curves plot."), value = 12, min = 1, step = 0.5)),
              shiny::column(3, shiny::numericInput("fd_curves_download_height", tooltip_label("Download height (in)", "Height in inches for exported transformed FD curves plot."), value = 8, min = 1, step = 0.5)),
              shiny::column(6,
                shiny::tags$br(),
                shiny::downloadButton("download_fd_curves", "Download transformed curves plot", class = "btn-info btn-block")
              )
            )
          )
        )
      ),

      # ===========================================================
      # Page 4 -- Analytical Metrics
      # ===========================================================
      bs4Dash::tabItem(
        tabName = "metrics",

        # --- Settings row: Approach (left) | Retract (right) ---
        shiny::fluidRow(

          # ---- Approach ----
          bs4Dash::bs4Card(
            title       = "Approach Curve Analysis Settings",
            width       = 6,
            status      = "primary",
            solidHeader = TRUE,
            collapsible = TRUE,

            shiny::checkboxInput("analyze_approach",
              tooltip_label("Analyze approach curves", "Uncheck this to skip analysis of approach curves, for example if they are absent or not needed."),
              value = TRUE),
            shiny::conditionalPanel(
              condition = "input.analyze_approach == true",
            shiny::h6(shiny::strong("Noise Band Estimation")),
            shiny::textInput("noise_baseline_span_approach",
              tooltip_label("Window size", "Number of end-of-curve points used for noise-band estimation. For example, in a curve with 512 points, setting this to 100 uses points 413-512. Set this to 'automatic' to use the predefined baseline segment."),
              value = "automatic"),
            shiny::selectInput("noise_threshold_method_approach",
              tooltip_label("Noise band estimation method", "Method used to estimate the noise band. sd uses the standard deviation of force values in the baseline segment; mad uses the median absolute deviation; quantile uses a quantile range defined by the low and high quantiles; fixed uses user-defined threshold values."),
              choices = c("sd", "mad", "quantile", "fixed"), selected = "sd"),
            shiny::conditionalPanel(
              condition = "input.noise_threshold_method_approach == 'mad'",
              shiny::numericInput("noise_mad_constant_approach",
                tooltip_label("MAD constant", "Scaling constant for the median absolute deviation."),
                value = 1.4826, step = 0.0001)
            ),
            shiny::conditionalPanel(
              condition = "input.noise_threshold_method_approach == 'quantile'",
              shiny::fluidRow(
                shiny::column(6, shiny::numericInput("noise_quantile_low_approach",
                  tooltip_label("Quantile low", "Lower quantile bound for noise estimation (e.g., setting this to 0.05 returns the value at the 5th percentile of the force within the defined region)."),
                  value = 0.05, min = 0, max = 1, step = 0.01)),
                shiny::column(6, shiny::numericInput("noise_quantile_high_approach",
                  tooltip_label("Quantile high", "Upper quantile bound for noise estimation (e.g., setting this to 0.95 returns the value at the 95th percentile of the force within the defined region)."),
                  value = 0.95, min = 0, max = 1, step = 0.01))
              )
            ),
            shiny::conditionalPanel(
              condition = "input.noise_threshold_method_approach == 'fixed'",
              shiny::fluidRow(
                shiny::column(6, shiny::numericInput("noise_fixed_low_approach",
                  tooltip_label("Fixed low", "Use this value as the lower bound of the noise band."),
                  value = NA, step = 1)),
                shiny::column(6, shiny::numericInput("noise_fixed_high_approach",
                  tooltip_label("Fixed high", "Use this value as the upper bound of the noise band."),
                  value = NA, step = 1))
              )
            ),
            shiny::numericInput("noise_multiplier_approach",
              tooltip_label("Noise band multiplier", "Multiplier applied to the noise band estimated by the selected method to scale the width of the noise band. For example, setting this to 3 means the noise band extends 3 times above and below the estimated noise level."),
              value = 3, min = 0, step = 0.1),

            shiny::hr(),
            shiny::h6(shiny::strong("Metrics to Compute")),
            shiny::checkboxInput("do_adhesive_force_approach",
              tooltip_label("Adhesive force", "Calculate the maximum adhesive force in the curve, defined as the absolute value of the most negative force."), value = TRUE),
            shiny::checkboxInput("do_energy_approach",
              tooltip_label("Energies", "Calculate adhesive and repulsive interaction energies. Adhesive energy is the area above the curve and below the lower noise-band bound in the IV quadrant. Repulsive energy is the area below the curve and above the upper noise-band bound in the first quadrant."), value = TRUE),
            shiny::checkboxInput("do_rupture_approach",
              tooltip_label("Rupture distance", "Calculate the adhesive or rupture distance. The curve is scanned from right to left to find the first point where force falls below the lower noise-band bound, indicating entry into the adhesive region."), value = TRUE),
            shiny::conditionalPanel(
              condition = "input.do_rupture_approach == true",
              shiny::fluidRow(
                shiny::column(8, shiny::textInput("rupture_baseline_span_approach",
                  tooltip_label("Exclusion cutoff", "Points after this cutoff are excluded from scanning. For example, if a curve has 512 points and this is set to 100, points 413-512 are excluded from scanning. Set this to 'automatic' to exclude the predefined baseline segment."),
                  value = "automatic")),
                shiny::column(4, shiny::numericInput("rupture_min_consecutive_approach",
                  tooltip_label("Min consecutive", "Minimum number of consecutive points below the noise band required to classify that the curve has entered the adhesive region."),
                  value = 3, min = 1, step = 1))
              )
            ),
            shiny::checkboxInput("do_repulsive_approach",
              tooltip_label("Repulsive distance", "Calculate the repulsive distance. The curve is scanned from left to right to find the last point where force remains above the upper noise-band bound, indicating the end of the repulsive region."), value = TRUE),
            shiny::conditionalPanel(
              condition = "input.do_repulsive_approach == true",
              shiny::fluidRow(
                shiny::column(8, shiny::textInput("repulsive_baseline_span_approach",
                  tooltip_label("Exclusion cutoff", "Points after this cutoff are excluded from scanning. For example, if a curve has 512 points and this is set to 100, points 413-512 are excluded from scanning. Set this to 'automatic' to exclude the predefined baseline segment."),
                  value = "automatic")),
                shiny::column(4, shiny::numericInput("repulsive_min_consecutive_approach",
                  tooltip_label("Min consecutive", "Minimum number of consecutive points above the noise band required to call a repulsive event.  Because the curve is expected to begin in the repulsive region, this is generally set to 1."),
                  value = 1, min = 1, step = 1))
              )
            )
            ) # end conditionalPanel analyze_approach
          ),

          # ---- Retract ----
          bs4Dash::bs4Card(
            title       = "Retract Curve Analysis Settings",
            width       = 6,
            status      = "danger",
            solidHeader = TRUE,
            collapsible = TRUE,

            shiny::checkboxInput("analyze_retract",
              tooltip_label("Analyze retract curves", "Uncheck this to skip analysis of retract curves, for example if they are absent or not needed."),
              value = TRUE),
            shiny::conditionalPanel(
              condition = "input.analyze_retract == true",
            shiny::h6(shiny::strong("Noise Band Estimation")),
            shiny::textInput("noise_baseline_span_retract",
              tooltip_label("Window size", "Number of end-of-curve points used for noise-band estimation. For example, in a curve with 512 points, setting this to 100 uses points 413-512. Set this to 'automatic' to use the predefined baseline segment."),
              value = "automatic"),
            shiny::selectInput("noise_threshold_method_retract",
              tooltip_label("Noise band estimation method", "Method used to estimate the noise band. sd uses the standard deviation of force values in the baseline segment; mad uses the median absolute deviation; quantile uses a quantile range defined by the low and high quantiles; fixed uses user-defined threshold values."),
              choices = c("sd", "mad", "quantile", "fixed"), selected = "sd"),
            shiny::conditionalPanel(
              condition = "input.noise_threshold_method_retract == 'mad'",
              shiny::numericInput("noise_mad_constant_retract",
                tooltip_label("MAD constant", "Scaling constant for the median absolute deviation."),
                value = 1.4826, step = 0.0001)
            ),
            shiny::conditionalPanel(
              condition = "input.noise_threshold_method_retract == 'quantile'",
              shiny::fluidRow(
                shiny::column(6, shiny::numericInput("noise_quantile_low_retract",
                  tooltip_label("Quantile low", "Lower quantile bound for noise estimation (e.g., setting this to 0.05 returns the value at the 5th percentile of the force within the defined region)."),
                  value = 0.05, min = 0, max = 1, step = 0.01)),
                shiny::column(6, shiny::numericInput("noise_quantile_high_retract",
                  tooltip_label("Quantile high", "Upper quantile bound for noise estimation (e.g., setting this to 0.95 returns the value at the 95th percentile of the force within the defined region)."),
                  value = 0.95, min = 0, max = 1, step = 0.01))
              )
            ),
            shiny::conditionalPanel(
              condition = "input.noise_threshold_method_retract == 'fixed'",
              shiny::fluidRow(
                shiny::column(6, shiny::numericInput("noise_fixed_low_retract",
                  tooltip_label("Fixed low", "Use this value as the lower bound of the noise band."),
                  value = NA, step = 1)),
                shiny::column(6, shiny::numericInput("noise_fixed_high_retract",
                  tooltip_label("Fixed high", "Use this value as the upper bound of the noise band."),
                  value = NA, step = 1))
              )
            ),
            shiny::numericInput("noise_multiplier_retract",
              tooltip_label("Noise band multiplier", "Multiplier applied to the noise band estimated by the selected method to scale the width of the noise band. For example, setting this to 3 means the noise band extends 3 times above and below the estimated noise level."),
              value = 3, min = 0, step = 0.1),

            shiny::hr(),
            shiny::h6(shiny::strong("Metrics to Compute")),
            shiny::checkboxInput("do_adhesive_force_retract",
              tooltip_label("Adhesive force", "Calculate the maximum adhesive force in the curve, defined as the absolute value of the most negative force."), value = TRUE),
            shiny::checkboxInput("do_energy_retract",
              tooltip_label("Energies", "Calculate adhesive and repulsive interaction energies. Adhesive energy is the area above the curve and below the lower noise-band bound in the IV quadrant. Repulsive energy is the area below the curve and above the upper noise-band bound in the first quadrant."), value = TRUE),
            shiny::checkboxInput("do_rupture_retract",
              tooltip_label("Rupture distance", "Calculate the adhesive or rupture distance. The curve is scanned from right to left to find the first point where force falls below the lower noise-band bound, indicating entry into the adhesive region."), value = TRUE),
            shiny::conditionalPanel(
              condition = "input.do_rupture_retract == true",
              shiny::fluidRow(
                shiny::column(8, shiny::textInput("rupture_baseline_span_retract",
                  tooltip_label("Exclusion cutoff", "Points after this cutoff are excluded from scanning. For example, if a curve has 512 points and this is set to 100, points 413-512 are excluded from scanning. Set this to 'automatic' to exclude the predefined baseline segment."),
                  value = "automatic")),
                shiny::column(4, shiny::numericInput("rupture_min_consecutive_retract",
                  tooltip_label("Min consecutive", "Minimum number of consecutive points below the noise band required to classify that the curve has entered the adhesive region."),
                  value = 3, min = 1, step = 1))
              )
            ),
            shiny::checkboxInput("do_repulsive_retract",
              tooltip_label("Repulsive distance", "Calculate the repulsive distance. The curve is scanned from left to right to find the last point where force remains above the upper noise-band bound, indicating the end of the repulsive region."), value = TRUE),
            shiny::conditionalPanel(
              condition = "input.do_repulsive_retract == true",
              shiny::fluidRow(
                shiny::column(8, shiny::textInput("repulsive_baseline_span_retract",
                  tooltip_label("Exclusion cutoff", "Points after this cutoff are excluded from scanning. For example, if a curve has 512 points and this is set to 100, points 413-512 are excluded from scanning. Set this to 'automatic' to exclude the predefined baseline segment."),
                  value = "automatic")),
                shiny::column(4, shiny::numericInput("repulsive_min_consecutive_retract",
                  tooltip_label("Min consecutive", "Minimum number of consecutive points above the noise band required to call a repulsive event.  Because the curve is expected to begin in the repulsive region, this is generally set to 1."),
                  value = 1, min = 1, step = 1))
              )
            )
            ) # end conditionalPanel analyze_retract
          )
        ),

        # --- Threads + Run button ---
        shiny::fluidRow(
          bs4Dash::bs4Card(
            width  = 12,
            status = "success",
            shiny::fluidRow(
              shiny::column(3,
                shiny::numericInput("threads_metrics",
                  tooltip_label("Threads", "Number of parallel workers to use for metric calculations."),
                  value = 1, min = 1, step = 1)
              ),
              shiny::column(9,
                shiny::tags$div(style = "margin-top:24px;",
                  shiny::actionButton("metrics_btn", "Analyze",
                    class = "btn-success btn-block", icon = shiny::icon("calculator"))
                )
              )
            )
          )
        ),

        # --- Single Curve Inspector ---
        shiny::fluidRow(
          bs4Dash::bs4Card(
            title = "Single Curve Inspector",
            solidHeader = TRUE,
            status = 'info',
            width = 12,
            shiny::fluidRow(
              shiny::column(8, shiny::selectInput("metric_curve_name",
                tooltip_label("Curve name", "Sample or curve identifier to display in the single-curve inspector."),
                choices = character(0))),
              shiny::column(4, shiny::selectInput("metric_use_curve",
                tooltip_label("Segment", "Choose whether to display the approach or retract segment for the selected curve."),
                choices = c("retract", "approach"), selected = "retract"))
            ),
            tooltip_plot("single_curve_plot", "480px", "Selected curve with analytical metric annotations."),
            shiny::fluidRow(
              shiny::column(3, shiny::numericInput("single_curve_download_width", tooltip_label("Download width (in)", "Width in inches for exported single-curve plot."), value = 14, min = 1, step = 0.5)),
              shiny::column(3, shiny::numericInput("single_curve_download_height", tooltip_label("Download height (in)", "Height in inches for exported single-curve plot."), value = 7, min = 1, step = 0.5)),
              shiny::column(6)
            ),
            shiny::fluidRow(
              shiny::column(6, shiny::downloadButton("download_single_curve", "Download plot", class = "btn-info btn-block")),
              shiny::column(6, shiny::downloadButton("download_single_curve_data", "Download data", class = "btn-info btn-block"))
            )
          )
        )
      ),

      # ===========================================================
      # Page 5 -- Results
      # ===========================================================
      bs4Dash::tabItem(
        tabName = "results",
        shiny::fluidRow(
          bs4Dash::bs4Card(
            title       = "Results",
            width       = 12,
            status      = "info",
            solidHeader = TRUE,
            shiny::fluidRow(
              shiny::column(3, shiny::textInput("results_new_meta_col_name", tooltip_label("New column name", "Name of the metadata column to add for all samples."), value = "")),
              shiny::column(3, shiny::selectInput("results_new_meta_col_type", tooltip_label("Column type", "Data type to use for the new metadata column."),
                choices = c("character", "numeric", "integer", "logical"), selected = "character")),
              shiny::column(3, shiny::textInput("results_new_meta_col_default", tooltip_label("Default value", "Initial value to fill into all rows of the new metadata column."), value = "")),
              shiny::column(3, shiny::tags$br(),
                shiny::actionButton("add_meta_col_results_btn", "Add column", class = "btn-secondary"))
            ),
            shiny::helpText("Edit cells directly, drag the fill handle to autofill like Excel, or right-click for a context menu."),
            rhandsontable::rHandsontableOutput("results_metadata_hot"),
            shiny::fluidRow(
              shiny::column(6, shiny::downloadButton("download_results_metadata_csv", "Download metadata (.csv)", class = "btn-info btn-block")),
              shiny::column(6, shiny::downloadButton("download_results_metadata_xlsx", "Download metadata (.xlsx)", class = "btn-info btn-block"))
            )
          )
        )
      ),

      # ===========================================================
      # Page 6 -- Download
      # ===========================================================
      bs4Dash::tabItem(
        tabName = "download",
        shiny::fluidRow(
          bs4Dash::bs4Card(
            title       = "Download Data",
            width       = 12,
            status      = "info",
            solidHeader = TRUE,
            shiny::fluidRow(
              shiny::column(12,
                shiny::helpText("Download raw curves: download all untransformed curves. If smoothing step is included, the smoothed curves are included."),
                shiny::downloadButton("download_all_raw_curves_zip", "Download raw curves (.zip)", class = "btn-info btn-block")
              )
            ),
            shiny::tags$br(),
            shiny::fluidRow(
              shiny::column(12,
                shiny::helpText("Download all force-distance curves."),
                shiny::downloadButton("download_all_transformed_curves_zip", "Download transformed curves (.zip)", class = "btn-info btn-block")
              )
            ),
            shiny::tags$br(),
            shiny::fluidRow(
              shiny::column(12,
                shiny::helpText("Download the fdObj, a S4 container including all raw and transformed curves, metadata, and sensitivity and baseline segment."),
                shiny::downloadButton("download_fdobj_rds", "Download fdObj (.rds)", class = "btn-info btn-block")
              )
            )
          )
        )
      ),

      # ===========================================================
      # Page 7 -- Figures
      # ===========================================================
      bs4Dash::tabItem(
        tabName = "figures",
        shiny::fluidRow(
          bs4Dash::bs4Card(
            title       = "PCA Biplot",
            width       = 12,
            status      = "info",
            solidHeader = TRUE,
            
            shiny::fluidRow(
              shiny::column(12, shiny::selectInput("summary_group_by", tooltip_label("Group by", "Metadata column used to group samples and color points in the PCA and violin plots."),
                choices = c("None" = ""), selected = ""))
            ),
            shiny::fluidRow(
              shiny::column(12, shiny::div(
                class = "pca-checkbox-grid",
                shiny::checkboxGroupInput("pca_include_columns", tooltip_label("PCA include columns", "Choose the metadata columns to include in the PCA. Only columns containing terms such as force, energy, or distance are offered."),
                  choices = character(0), selected = character(0), inline = FALSE)
              ))
            ),
            shiny::fluidRow(
              shiny::column(2, shiny::numericInput("pca_point_size", tooltip_label("Point size", "Point size for PCA sample markers."), value = 2.2, min = 0.1, step = 0.1)),
              shiny::column(2, shiny::numericInput("pca_point_alpha", tooltip_label("Point alpha", "Transparency for PCA sample markers."), value = 0.9, min = 0, max = 1, step = 0.05)),
              shiny::column(2, shiny::textInput("pca_arrow_color", tooltip_label("Arrow color", "Color used for PCA loading arrows and feature labels."), value = "grey30")),
              shiny::column(2, shiny::numericInput("pca_arrow_alpha", tooltip_label("Arrow alpha", "Transparency for PCA loading arrows."), value = 0.85, min = 0, max = 1, step = 0.05)),
              shiny::column(2, shiny::numericInput("pca_arrow_scale", tooltip_label("Arrow scale", "Multiplier controlling PCA loading arrow length."), value = 1.1, min = 0.1, step = 0.1)),
              shiny::column(2, shiny::checkboxInput("pca_show_feature_labels", tooltip_label("Show feature labels", "Show or hide labels for PCA loading arrows."), value = TRUE))
            ),
            shiny::fluidRow(
              shiny::column(6, shiny::numericInput("pca_feature_label_size", tooltip_label("Feature label size", "Text size for PCA feature labels."), value = 3.5, min = 0.1, step = 0.1)),
              shiny::column(6, shiny::numericInput("pca_base_size", tooltip_label("Base size", "Base font size for the PCA plot."), value = 12, min = 1, step = 1))
            ),
            tooltip_plot("pca_plot", "460px", "PCA biplot built from the selected analytical metrics. Points represent samples, and arrows represent feature loadings."),
            shiny::fluidRow(
              shiny::column(3, shiny::numericInput("pca_download_width", tooltip_label("Download width (in)", "Width in inches for exported PCA plot."), value = 10, min = 1, step = 0.5)),
              shiny::column(3, shiny::numericInput("pca_download_height", tooltip_label("Download height (in)", "Height in inches for exported PCA plot."), value = 7, min = 1, step = 0.5)),
              shiny::column(6, shiny::tags$br(), shiny::downloadButton("download_pca", "Download PCA plot", class = "btn-info btn-block"))
            )
          )
        ),
        shiny::fluidRow(
          bs4Dash::bs4Card(
            title = "Violin Plot",
            width = 12,
            status      = "info",
            solidHeader = TRUE,
            shiny::fluidRow(
              shiny::column(4, shiny::selectInput("violin_metric", tooltip_label("Metric", "Analytical metric column to display in the violin plot."),
                choices = character(0))),
              shiny::column(4, shiny::selectInput("violin_group_by", tooltip_label("Group by", "Metadata column used to group samples."),
                choices = c("None" = ""), selected = "")),
              shiny::column(4, shiny::selectInput("violin_color_by", tooltip_label("Color by", "Metadata column used to color the samples. If set to None, the Group by column is used."),
                choices = c("None" = ""), selected = ""))
            ),
            shiny::fluidRow(
              shiny::column(2, shiny::checkboxInput("violin_add_points", tooltip_label("Add points", "Overlay jittered points on top of violin distributions."), value = FALSE)),
              shiny::column(2, shiny::checkboxInput("violin_show_whisker_box", tooltip_label("Show whisker box", "Overlay a whisker boxplot on top of each violin."), value = TRUE)),
              shiny::column(2, shiny::checkboxInput("violin_log10", tooltip_label("Log10", "Apply log10 transform to positive metric values before plotting and testing."), value = FALSE)),
              shiny::column(3, shiny::selectInput("violin_global_test", tooltip_label("Global test", "Global significance test across groups."),
                choices = c("none", "anova", "kruskal"), selected = "anova")),
              shiny::column(3, shiny::selectInput("violin_pairwise_test", tooltip_label("Pairwise test", "Statistical test used for pairwise group comparisons."),
                choices = c("none", "t.test", "wilcox"), selected = "t.test"))
            ),
            shiny::fluidRow(
              shiny::column(6, shiny::selectInput("violin_p_adjust_method", tooltip_label("P adjust method", "Multiple-testing correction method used for pairwise comparisons."),
                choices = stats::p.adjust.methods, selected = "BH")),
              shiny::column(6)
            ),
            tooltip_plot("violin_plot", "400px", "Distribution of the selected metric across groups defined by the chosen metadata column."),
            shiny::fluidRow(
              shiny::column(3, shiny::numericInput("violin_download_width", tooltip_label("Download width (in)", "Width in inches for exported violin plot."), value = 9, min = 1, step = 0.5)),
              shiny::column(3, shiny::numericInput("violin_download_height", tooltip_label("Download height (in)", "Height in inches for exported violin plot."), value = 7, min = 1, step = 0.5)),
              shiny::column(6, shiny::tags$br(), shiny::downloadButton("download_violin", "Download violin plot", class = "btn-info btn-block"))
            )
          )
        ),
        shiny::fluidRow(
          bs4Dash::bs4Card(
            title = "Complex Heatmap",
            width = 12,
            status = "info",
            solidHeader = TRUE,
            shiny::fluidRow(
              shiny::column(12, shiny::div(
                class = "pca-checkbox-grid",
                shiny::checkboxGroupInput("heatmap_include_columns", tooltip_label("Heatmap include columns", "Choose the metadata feature columns to display as heatmap rows."),
                  choices = character(0), selected = character(0), inline = FALSE)
              ))
            ),
            shiny::fluidRow(
              shiny::column(3, shiny::selectInput("heatmap_anno_col1", tooltip_label("Annotate col 1", "First metadata column used for heatmap column annotation."),
                choices = c("None" = ""), selected = "")),
              shiny::column(3, shiny::selectInput("heatmap_anno_col2", tooltip_label("Annotate col 2", "Second metadata column used for heatmap column annotation."),
                choices = c("None" = ""), selected = "")),
              shiny::column(3),
              shiny::column(3)
            ),
            shiny::fluidRow(
              shiny::column(3, shiny::checkboxInput("heatmap_show_row_names", tooltip_label("Show row names", "Show feature names on the heatmap y-axis."), value = TRUE)),
              shiny::column(3, shiny::checkboxInput("heatmap_show_column_names", tooltip_label("Show column names", "Show sample names on the heatmap x-axis."), value = FALSE)),
              shiny::column(3, shiny::checkboxInput("heatmap_cluster_rows", tooltip_label("Cluster rows", "Cluster feature rows in the heatmap."), value = TRUE)),
              shiny::column(3, shiny::checkboxInput("heatmap_cluster_columns", tooltip_label("Cluster columns", "Cluster sample columns in the heatmap."), value = TRUE))
            ),
            tooltip_plot("complex_heatmap_plot", "680px", "Scaled feature heatmap with optional metadata annotations."),
            shiny::fluidRow(
              shiny::column(3, shiny::numericInput("heatmap_download_width", tooltip_label("Download width (in)", "Width in inches for exported complex heatmap."), value = 11, min = 1, step = 0.5)),
              shiny::column(3, shiny::numericInput("heatmap_download_height", tooltip_label("Download height (in)", "Height in inches for exported complex heatmap."), value = 8, min = 1, step = 0.5)),
              shiny::column(6, shiny::tags$br(), shiny::downloadButton("download_complex_heatmap", "Download complex heatmap", class = "btn-info btn-block"))
            )
          )
        )
      )

    ) # tabItems
  ),  # dashboardBody
  
  controlbar = bs4Dash::dashboardControlbar(
    id = "controlbar",
    skin = "light",
    shiny::div(
      style = "padding: 15px;",
      shiny::h4("About curvana", style = "margin-top: 0;"),
      shiny::p(
        shiny::strong("Version:"), " 0.1.0", shiny::br(),
        shiny::strong("Purpose:"), " Interactive AFM force curve analysis"
      ),
      shiny::tags$hr(),
      
      shiny::h5("Links"),
      shiny::tags$ul(
        shiny::tags$li(shiny::tags$a("Tutorial", href = "https://github.com/GaryZhangYue/fdafmR", target = "_blank")),
        shiny::tags$li(shiny::tags$a("GitHub Repository", href = "https://github.com/GaryZhangYue/fdafmR", target = "_blank")),
        shiny::tags$li(shiny::tags$a("Report Issues", href = "https://github.com/GaryZhangYue/fdafmR/issues", target = "_blank"))
      )
    )
  )
)     # dashboardPage


# ---- Server ------------------------------------------------------------------
server <- function(input, output, session) {

  # Make example files accessible to the web server (works for both local and shinyapps.io)
  shiny::addResourcePath("examples", getwd())

  rv <- shiny::reactiveValues(
    fdobj             = NULL,
    fdobj_clean       = NULL,
    fdobj_transformed = NULL,
    fdobj_final       = NULL,
    clean_metadata_cols = NULL,
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

  metadata_keep_clean_only <- function(md) {
    keep_cols <- rv$clean_metadata_cols
    if (is.null(keep_cols) || !is.data.frame(md)) {
      return(md)
    }
    keep_cols <- keep_cols[keep_cols %in% colnames(md)]
    if (length(keep_cols) == 0) {
      return(md[, 0, drop = FALSE])
    }
    md[, keep_cols, drop = FALSE]
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

    keep_or_none <- function(current_value) {
      if (!is.null(current_value) && nzchar(current_value) && (current_value %in% md_cols)) {
        return(current_value)
      }
      ""
    }

    sens_col_chooser <- stats::setNames(md_cols, md_cols)
    shiny::updateSelectInput(session, "soft_sens_col_approach",
      choices = sens_col_chooser, selected = keep_or_none(input$soft_sens_col_approach))
    shiny::updateSelectInput(session, "soft_sens_col_retract",
      choices = sens_col_chooser, selected = keep_or_none(input$soft_sens_col_retract))
    shiny::updateSelectInput(session, "spring_constant_col_approach",
      choices = sens_col_chooser, selected = keep_or_none(input$spring_constant_col_approach))
    shiny::updateSelectInput(session, "spring_constant_col_retract",
      choices = sens_col_chooser, selected = keep_or_none(input$spring_constant_col_retract))

    shiny::updateSelectInput(session, "raw_index_group_by",
      choices = chooser, selected = keep_or_none(input$raw_index_group_by))
    shiny::updateSelectInput(session, "raw_index_split_by",
      choices = chooser, selected = keep_or_none(input$raw_index_split_by))
    shiny::updateSelectInput(session, "raw_group_by",
      choices = chooser, selected = keep_or_none(input$raw_group_by))
    shiny::updateSelectInput(session, "raw_split_by",
      choices = chooser, selected = keep_or_none(input$raw_split_by))
    shiny::updateSelectInput(session, "fd_group_by",
      choices = chooser, selected = keep_or_none(input$fd_group_by))
    shiny::updateSelectInput(session, "fd_split_by",
      choices = chooser, selected = keep_or_none(input$fd_split_by))
    shiny::updateSelectInput(session, "summary_group_by",
      choices = chooser, selected = default_choice(c("surface"), 1L))
    shiny::updateSelectInput(session, "violin_group_by",
      choices = chooser, selected = default_choice(c("surface"), 1L))
    shiny::updateSelectInput(session, "violin_color_by",
      choices = chooser, selected = keep_or_none(input$violin_color_by))
    shiny::updateSelectInput(session, "heatmap_anno_col1",
      choices = chooser, selected = keep_or_none(input$heatmap_anno_col1))
    shiny::updateSelectInput(session, "heatmap_anno_col2",
      choices = chooser, selected = keep_or_none(input$heatmap_anno_col2))

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

    pca_candidates <- md_cols[grepl("energy|force|distance", md_cols, ignore.case = TRUE)]
    pca_candidates <- unique(pca_candidates)
    current_pca <- input$pca_include_columns
    default_pca <- intersect(c(
      "adhesive_force_nN_retract",
      "adhesive_energy_aJ_retract",
      "repulsive_energy_aJ_retract",
      "rupture_distance_nm_retract",
      "repulsive_distance_nm_retract"
    ), pca_candidates)
    selected_pca <- if (!is.null(current_pca) && length(current_pca) > 0) {
      intersect(current_pca, pca_candidates)
    } else {
      default_pca
    }
    if (length(selected_pca) == 0 && length(pca_candidates) > 0) {
      selected_pca <- pca_candidates
    }
    shiny::updateCheckboxGroupInput(session, "pca_include_columns",
      choices = stats::setNames(pca_candidates, pca_candidates),
      selected = selected_pca)

    current_heatmap <- input$heatmap_include_columns
    selected_heatmap <- if (!is.null(current_heatmap) && length(current_heatmap) > 0) {
      intersect(current_heatmap, pca_candidates)
    } else {
      default_pca
    }
    if (length(selected_heatmap) == 0 && length(pca_candidates) > 0) {
      selected_heatmap <- pca_candidates
    }
    shiny::updateCheckboxGroupInput(session, "heatmap_include_columns",
      choices = stats::setNames(pca_candidates, pca_candidates),
      selected = selected_heatmap)
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

      # Get selected file format
      file_format <- as.character(input$file_format_tabs)
      if (!nzchar(file_format)) {
        file_format <- "Bruker NanoScope/Veeco ASCII"  # default
      }

      # Get column names based on selected format
      if (file_format == "Bruker NanoScope/Veeco ASCII") {
        col_calc_ramp_ex <- trimws(as.character(input$load_calc_ramp_ex_nm))
        col_calc_ramp_rt <- trimws(as.character(input$load_calc_ramp_rt_nm))
        col_defl_v_ex <- trimws(as.character(input$load_defl_v_ex))
        col_defl_v_rt <- trimws(as.character(input$load_defl_v_rt))
      } else if (file_format == "Generic Column-Separated Format") {
        col_calc_ramp_ex <- trimws(as.character(input$generic_calc_ramp_ex_nm))
        col_calc_ramp_rt <- trimws(as.character(input$generic_calc_ramp_rt_nm))
        col_defl_v_ex <- trimws(as.character(input$generic_defl_v_ex))
        col_defl_v_rt <- trimws(as.character(input$generic_defl_v_rt))
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
      
      # Call appropriate function based on file format
      if (file_format == "JPK ASCII Format") {
        # JPK format: use createFdObjFromJPKFolder
        jpk_height <- trimws(as.character(input$jpk_height_col))
        jpk_deflection <- trimws(as.character(input$jpk_deflection_col))
        fdobj <- tryCatch(
          curvana::createFdObjFromJPKFolder(
            folder = folder,
            suffix = suffix,
            pattern = pattern,
            metadata = metadata_for_load,
            threads = max(1L, as.integer(input$threads_load)),
            height_col = jpk_height,
            deflection_col = jpk_deflection,
            reverse_Displacement_Approach = isTRUE(input$jpk_reverse_calc_ramp_ex_nm),
            reverse_Displacement_Retract = isTRUE(input$jpk_reverse_calc_ramp_rt_nm),
            reverse_Deflection_Approach = isTRUE(input$jpk_reverse_defl_v_ex),
            reverse_Deflection_Retract = isTRUE(input$jpk_reverse_defl_v_rt)
          ),
          error = function(e) {
            shiny::showNotification(
              paste0("JPK load failed with height_col='", jpk_height, "', deflection_col='", jpk_deflection, "'. Error: ", e$message),
              type = "error", duration = NULL
            )
            NULL
          }
        )
      } else if (file_format == "Bruker NanoScope/Veeco ASCII") {
        # Bruker format: use createFdObjFromFolder with Bruker inputs
        fdobj <- tryCatch(
          curvana::createFdObjFromFolder(
            folder  = folder,
            suffix = suffix,
            pattern = pattern,
            Displacement_Approach = col_calc_ramp_ex,
            Displacement_Retract = col_calc_ramp_rt,
            Deflection_Approach = col_defl_v_ex,
            Deflection_Retract = col_defl_v_rt,
            reverse_Displacement_Approach = isTRUE(input$reverse_calc_ramp_ex_nm),
            reverse_Displacement_Retract = isTRUE(input$reverse_calc_ramp_rt_nm),
            reverse_Deflection_Approach = isTRUE(input$reverse_defl_v_ex),
            reverse_Deflection_Retract = isTRUE(input$reverse_defl_v_rt),
            metadata = metadata_for_load,
            threads = max(1L, as.integer(input$threads_load))
          ),
          error = function(e) {
            shiny::showNotification(
              paste0("Bruker load failed with columns: Displacement_Approach='", col_calc_ramp_ex,
                     "', Displacement_Retract='", col_calc_ramp_rt,
                     "', Deflection_Approach='", col_defl_v_ex,
                     "', Deflection_Retract='", col_defl_v_rt,
                     "'. Error: ", e$message),
              type = "error", duration = NULL
            )
            NULL
          }
        )
      } else {
        # Generic format: use createFdObjFromFolder with Generic inputs
        fdobj <- tryCatch(
          curvana::createFdObjFromFolder(
            folder  = folder,
            suffix = suffix,
            pattern = pattern,
            Displacement_Approach = col_calc_ramp_ex,
            Displacement_Retract = col_calc_ramp_rt,
            Deflection_Approach = col_defl_v_ex,
            Deflection_Retract = col_defl_v_rt,
            reverse_Displacement_Approach = isTRUE(input$generic_reverse_calc_ramp_ex_nm),
            reverse_Displacement_Retract = isTRUE(input$generic_reverse_calc_ramp_rt_nm),
            reverse_Deflection_Approach = isTRUE(input$generic_reverse_defl_v_ex),
            reverse_Deflection_Retract = isTRUE(input$generic_reverse_defl_v_rt),
            metadata = metadata_for_load,
            threads = max(1L, as.integer(input$threads_load))
          ),
          error = function(e) {
            shiny::showNotification(
              paste0("Generic load failed with columns: Displacement_Approach='", col_calc_ramp_ex,
                     "', Displacement_Retract='", col_calc_ramp_rt,
                     "', Deflection_Approach='", col_defl_v_ex,
                     "', Deflection_Retract='", col_defl_v_rt,
                     "'. Error: ", e$message),
              type = "error", duration = NULL
            )
            NULL
          }
        )
      }
      
      if (is.null(fdobj)) return(NULL)
      shiny::incProgress(0.3)
      rv$fdobj             <- fdobj
      rv$fdobj_clean       <- fdobj
      rv$fdobj_transformed <- NULL
      rv$fdobj_final       <- NULL
      rv$clean_metadata_cols <- colnames(fdobj@metadata)
      
      # Update status message based on file format
      function_used <- if (file_format == "JPK ASCII Format") {
        "createFdObjFromJPKFolder"
      } else {
        "createFdObjFromFolder"
      }
      
      rv$status <- sprintf(
        "Loaded %d raw curves from:\n%s\nFile format: %s\nGenerated metadata: %d rows x %d columns.\n%s args: suffix='%s', pattern='%s'.%s",
        length(fdobj@rawCurves),
        folder,
        file_format,
        nrow(fdobj@metadata),
        ncol(fdobj@metadata),
        function_used,
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
    rv$clean_metadata_cols <- colnames(imported_df)
    rv$fdobj@metadata <- imported_df
    if (!is.null(rv$fdobj_clean)) {
      rv$fdobj_clean@metadata <- imported_df
    }
    if (!is.null(rv$fdobj_transformed)) {
      rv$fdobj_transformed@metadata <- imported_df
    }
    rv$status <- sprintf("Metadata imported from '%s' (%d rows x %d columns).",
                         file_name, nrow(imported_df), ncol(imported_df))
    update_choices(rv$fdobj)
    shiny::showNotification("Metadata imported successfully.", type = "message")
  })

  # Transform Curves
  shiny::observeEvent(input$transform_btn, {
    shiny::req(rv$fdobj_clean)
    shiny::withProgress(message = "Running transform_curves...", value = 0.1, {
      if (!isTRUE(input$transform_approach) && !isTRUE(input$transform_retract)) {
        shiny::showNotification("Please enable transformation for at least one curve segment (approach or retract).", type = "warning")
        return(NULL)
      }

      fdobj_current <- rv$fdobj_clean
      fdobj_current@metadata <- metadata_keep_clean_only(fdobj_current@metadata)
      least_app <- if (identical(input$least_mode_approach, "automatic")) "automatic" else as.integer(input$least_length_approach)
      least_ret <- if (identical(input$least_mode_retract,  "automatic")) "automatic" else as.integer(input$least_length_retract)
      
      transform_messages <- c()
      
      if (isTRUE(input$transform_approach)) {
        result <- tryCatch({
          shiny::incProgress(0.35, detail = "Approach")
          soft_app <- isTRUE(input$soft_approach)
          probe_sens_app <- if (soft_app) {
            if (identical(input$soft_sens_mode_approach, "column")) input$soft_sens_col_approach else as.numeric(input$soft_sens_value_approach)
          } else { NULL }
          sc_app <- if (identical(input$spring_constant_mode_approach, "column")) input$spring_constant_col_approach else as.numeric(input$spring_constant_value_approach)
          
          fdobj_result <- curvana::transform_curves(
            fdObj = fdobj_current, spring_constant = sc_app, useCurve = "approach",
            threads = max(1L, as.integer(input$threads_transform_approach)),
            denoise_first = isTRUE(input$denoise_first_approach),
            p = as.integer(input$denoise_p_approach), n = as.integer(input$denoise_n_approach),
            m = as.integer(input$denoise_m_approach), ts = as.numeric(input$denoise_ts_approach),
            least_length  = least_app,
            slp_threshold = as.numeric(input$slp_threshold_approach),
            std_threshold = as.numeric(input$std_threshold_approach),
            end           = as.integer(input$sens_end_approach),
            intv          = as.integer(input$intv_approach),
            R_squared_min = as.numeric(input$R_squared_min_approach),
            minimum_length = as.integer(input$minimum_length_approach),
            soft = soft_app,
            probe_sensitivity_external = probe_sens_app
          )
          
          fdobj_result
        }, error = function(e) {
          shiny::showNotification(paste("Approach transform failed:", e$message), type = "error", duration = NULL)
          NULL
        })
        fdobj_current <- result
        if (is.null(fdobj_current)) return(NULL)
      }

      if (isTRUE(input$transform_retract)) {
        result <- tryCatch({
          shiny::incProgress(0.45, detail = "Retract")
          soft_ret <- isTRUE(input$soft_retract)
          probe_sens_ret <- if (soft_ret) {
            if (identical(input$soft_sens_mode_retract, "column")) input$soft_sens_col_retract else as.numeric(input$soft_sens_value_retract)
          } else { NULL }
          sc_ret <- if (identical(input$spring_constant_mode_retract, "column")) input$spring_constant_col_retract else as.numeric(input$spring_constant_value_retract)
          
          fdobj_result <- curvana::transform_curves(
            fdObj = fdobj_current, spring_constant = sc_ret, useCurve = "retract",
            threads = max(1L, as.integer(input$threads_transform_retract)),
            denoise_first = isTRUE(input$denoise_first_retract),
            p = as.integer(input$denoise_p_retract), n = as.integer(input$denoise_n_retract),
            m = as.integer(input$denoise_m_retract), ts = as.numeric(input$denoise_ts_retract),
            least_length  = least_ret,
            slp_threshold = as.numeric(input$slp_threshold_retract),
            std_threshold = as.numeric(input$std_threshold_retract),
            end           = as.integer(input$sens_end_retract),
            intv          = as.integer(input$intv_retract),
            R_squared_min = as.numeric(input$R_squared_min_retract),
            minimum_length = as.integer(input$minimum_length_retract),
            soft = soft_ret,
            probe_sensitivity_external = probe_sens_ret
          )
          
          fdobj_result
        }, error = function(e) {
          shiny::showNotification(paste("Retract transform failed:", e$message), type = "error", duration = NULL)
          NULL
        })
        fdobj_current <- result
        if (is.null(fdobj_current)) return(NULL)
      }

      rv$fdobj             <- fdobj_current
      rv$fdobj_transformed <- fdobj_current
      rv$fdobj_final       <- NULL
      
      # Build status message with transform output
      transformed_segments <- c(
        if (isTRUE(input$transform_approach)) "approach" else NULL,
        if (isTRUE(input$transform_retract)) "retract" else NULL
      )
      
      # Generate detailed summary from final fdObj
      summary_lines <- character(0)
      
      if (isTRUE(input$transform_approach)) {
        n_total <- length(fdobj_current@approachCurves)
        n_success <- sum(sapply(fdobj_current@approachCurves, function(x) !is.null(x) && nrow(x) > 0))
        n_fail <- n_total - n_success
        
        sens_col <- "sensitivity_V_nm_approach"
        base_col <- "baseline_V_approach"
        n_sens_fail <- if (sens_col %in% names(fdobj_current@metadata)) {
          sum(is.na(fdobj_current@metadata[[sens_col]]))
        } else { NA }
        n_base_fail <- if (base_col %in% names(fdobj_current@metadata) && sens_col %in% names(fdobj_current@metadata)) {
          # Only count baseline failures where sensitivity exists
          sum(!is.na(fdobj_current@metadata[[sens_col]]) & is.na(fdobj_current@metadata[[base_col]]))
        } else { NA }
        
        approach_msg <- sprintf("Approach: %d processed, %d successful, %d failed transformation", 
                              n_total, n_success, n_fail)
        if (!is.na(n_sens_fail) && n_sens_fail > 0) {
          approach_msg <- paste0(approach_msg, sprintf("\n  - %d failed linear contact region detection", n_sens_fail))
        }
        if (!is.na(n_base_fail) && n_base_fail > 0) {
          approach_msg <- paste0(approach_msg, sprintf("\n  - %d has undulating baseline", n_base_fail))
        }
        summary_lines <- c(summary_lines, approach_msg)
      }
      
      if (isTRUE(input$transform_retract)) {
        n_total <- length(fdobj_current@retractCurves)
        n_success <- sum(sapply(fdobj_current@retractCurves, function(x) !is.null(x) && nrow(x) > 0))
        n_fail <- n_total - n_success
        
        sens_col <- "sensitivity_V_nm_retract"
        base_col <- "baseline_V_retract"
        n_sens_fail <- if (sens_col %in% names(fdobj_current@metadata)) {
          sum(is.na(fdobj_current@metadata[[sens_col]]))
        } else { NA }
        n_base_fail <- if (base_col %in% names(fdobj_current@metadata) && sens_col %in% names(fdobj_current@metadata)) {
          # Only count baseline failures where sensitivity exists
          sum(!is.na(fdobj_current@metadata[[sens_col]]) & is.na(fdobj_current@metadata[[base_col]]))
        } else { NA }
        
        retract_msg <- sprintf("Retract: %d processed, %d successful, %d failed transformation", 
                              n_total, n_success, n_fail)
        if (!is.na(n_sens_fail) && n_sens_fail > 0) {
          retract_msg <- paste0(retract_msg, sprintf("\n  - %d failed linear contact region detection", n_sens_fail))
        }
        if (!is.na(n_base_fail) && n_base_fail > 0) {
          retract_msg <- paste0(retract_msg, sprintf("\n  - %d has undulating baseline", n_base_fail))
        }
        summary_lines <- c(summary_lines, retract_msg)
      }
      
      status_msg <- sprintf("Transformation completed for: %s.", paste(transformed_segments, collapse = ", "))
      if (length(summary_lines) > 0) {
        status_msg <- paste(status_msg, "\n\n", paste(summary_lines, collapse = "\n"), sep = "")
      }
      rv$status <- status_msg
      
      update_choices(fdobj_current)
      shiny::incProgress(0.1)
      
      # Show notification with summary
      shiny::showNotification(
        paste(summary_lines, collapse = "\n"),
        type = "message",
        duration = 10
      )
    })
  })

  # Analytical Metrics
  shiny::observeEvent(input$metrics_btn, {
    if (is.null(rv$fdobj_transformed)) {
      shiny::showNotification("Run Transform Curves on Page 3 before analyzing metrics.", type = "warning")
      return(NULL)
    }
    shiny::withProgress(message = "Computing analytical metrics...", value = 0.1, {

      # Helper: parse baseline span — "automatic" or positive integer
      parse_span <- function(x) {
        x <- trimws(as.character(x))
        if (tolower(x) == "automatic") return("automatic")
        v <- suppressWarnings(as.integer(x))
        if (!is.na(v) && v >= 1L) v else "automatic"
      }

      # Helper: NA numeric input → NULL (for noise_fixed_low/high)
      get_fixed <- function(val) {
        v <- suppressWarnings(as.numeric(val))
        if (is.na(v)) NULL else v
      }

      # validate at least one segment is selected
      if (!isTRUE(input$analyze_approach) && !isTRUE(input$analyze_retract)) {
        shiny::showNotification("Please enable analysis for at least one curve segment (approach or retract).", type = "warning")
        return(NULL)
      }

      fdobj_current <- rv$fdobj_transformed

      # ---- Approach ----
      if (isTRUE(input$analyze_approach)) {
      approach_args <- list(
        fdObj                                    = fdobj_current,
        useCurve                                 = "approach",
        threads                                  = max(1L, as.integer(input$threads_metrics)),
        noise_baseline_span                      = parse_span(input$noise_baseline_span_approach),
        noise_threshold_method                   = input$noise_threshold_method_approach,
        noise_multiplier                         = as.numeric(input$noise_multiplier_approach),
        noise_mad_constant                       = as.numeric(input$noise_mad_constant_approach),
        noise_quantile_low                       = as.numeric(input$noise_quantile_low_approach),
        noise_quantile_high                      = as.numeric(input$noise_quantile_high_approach),
        noise_fixed_low                          = get_fixed(input$noise_fixed_low_approach),
        noise_fixed_high                         = get_fixed(input$noise_fixed_high_approach),
        analyze_adhesive_force                   = isTRUE(input$do_adhesive_force_approach),
        analyze_energy                           = isTRUE(input$do_energy_approach),
        analyze_rupture_distance                 = isTRUE(input$do_rupture_approach),
        analyze_rupture_distance_baseline_span   = parse_span(input$rupture_baseline_span_approach),
        analyze_rupture_distance_min_consecutive = as.integer(input$rupture_min_consecutive_approach),
        analyze_rupture_distance_x_direction     = "left",
        analyze_repulsive_distance               = isTRUE(input$do_repulsive_approach),
        analyze_repulsive_distance_baseline_span = parse_span(input$repulsive_baseline_span_approach),
        analyze_repulsive_distance_min_consecutive = as.integer(input$repulsive_min_consecutive_approach),
        analyze_repulsive_distance_x_direction   = "right"
      )

      fdobj_current <- tryCatch(
        do.call(curvana::analyze_curves_all_analytical_metrics, approach_args),
        error = function(e) {
          shiny::showNotification(paste("Approach metrics failed:", e$message), type = "error", duration = NULL)
          NULL
        }
      )
      if (is.null(fdobj_current)) return(NULL)
      } # end if analyze_approach
      shiny::incProgress(0.45)

      # ---- Retract ----
      if (isTRUE(input$analyze_retract)) {
      retract_args <- list(
        fdObj                                    = fdobj_current,
        useCurve                                 = "retract",
        threads                                  = max(1L, as.integer(input$threads_metrics)),
        noise_baseline_span                      = parse_span(input$noise_baseline_span_retract),
        noise_threshold_method                   = input$noise_threshold_method_retract,
        noise_multiplier                         = as.numeric(input$noise_multiplier_retract),
        noise_mad_constant                       = as.numeric(input$noise_mad_constant_retract),
        noise_quantile_low                       = as.numeric(input$noise_quantile_low_retract),
        noise_quantile_high                      = as.numeric(input$noise_quantile_high_retract),
        noise_fixed_low                          = get_fixed(input$noise_fixed_low_retract),
        noise_fixed_high                         = get_fixed(input$noise_fixed_high_retract),
        analyze_adhesive_force                   = isTRUE(input$do_adhesive_force_retract),
        analyze_energy                           = isTRUE(input$do_energy_retract),
        analyze_rupture_distance                 = isTRUE(input$do_rupture_retract),
        analyze_rupture_distance_baseline_span   = parse_span(input$rupture_baseline_span_retract),
        analyze_rupture_distance_min_consecutive = as.integer(input$rupture_min_consecutive_retract),
        analyze_rupture_distance_x_direction     = "left",
        analyze_repulsive_distance               = isTRUE(input$do_repulsive_retract),
        analyze_repulsive_distance_baseline_span = parse_span(input$repulsive_baseline_span_retract),
        analyze_repulsive_distance_min_consecutive = as.integer(input$repulsive_min_consecutive_retract),
        analyze_repulsive_distance_x_direction   = "right"
      )

      fdobj_current <- tryCatch(
        do.call(curvana::analyze_curves_all_analytical_metrics, retract_args),
        error = function(e) {
          shiny::showNotification(paste("Retract metrics failed:", e$message), type = "error", duration = NULL)
          NULL
        }
      )
      if (is.null(fdobj_current)) return(NULL)
      } # end if analyze_retract

      rv$fdobj  <- fdobj_current
      rv$fdobj_final <- fdobj_current
      rv$status <- "Analytical metrics completed."
      update_choices(fdobj_current)
      shiny::incProgress(0.9)
      shiny::showNotification("Metrics complete.", type = "message")
    })
  })

  # Add metadata column
  build_metadata_with_new_column <- function(md, new_col, col_type, default_text) {
    new_col <- trimws(as.character(new_col))
    if (!nzchar(new_col)) {
      shiny::showNotification("Please provide a non-empty column name.", type = "error")
      return(NULL)
    }
    if (new_col %in% colnames(md)) {
      shiny::showNotification("Column already exists in metadata.", type = "error")
      return(NULL)
    }
    col_type <- trimws(as.character(col_type))
    if (!(col_type %in% c("character", "numeric", "integer", "logical"))) {
      shiny::showNotification("Invalid column type.", type = "error")
      return(NULL)
    }
    default_text <- as.character(default_text)
    default_val <- cast_value(default_text, col_type)
    if ((identical(col_type, "numeric") || identical(col_type, "integer")) &&
        nzchar(trimws(default_text)) && is.na(default_val)) {
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
    md
  }

  add_metadata_column <- function(new_col, col_type, default_text) {
    md_source <- if (!is.null(rv$fdobj_clean)) rv$fdobj_clean else rv$fdobj
    shiny::req(md_source)
    md <- build_metadata_with_new_column(md_source@metadata, new_col, col_type, default_text)
    if (is.null(md)) {
      return(NULL)
    }
    rv$fdobj@metadata <- md
    rv$clean_metadata_cols <- unique(c(rv$clean_metadata_cols, new_col))
    if (!is.null(rv$fdobj_clean)) {
      md_clean <- rv$fdobj_clean@metadata
      md_clean[[new_col]] <- md[[new_col]]
      rv$fdobj_clean@metadata <- md_clean
    }
    if (!is.null(rv$fdobj_transformed)) {
      rv$fdobj_transformed@metadata <- md
    }
    rv$status <- sprintf("Added metadata column '%s'.", new_col)
    update_choices(rv$fdobj)
  }

  shiny::observeEvent(input$add_meta_col_btn, {
    add_metadata_column(
      new_col = input$new_meta_col_name,
      col_type = input$new_meta_col_type,
      default_text = input$new_meta_col_default
    )
  })

  shiny::observeEvent(input$add_meta_col_results_btn, {
    shiny::req(rv$fdobj_final)
    md_final <- build_metadata_with_new_column(
      rv$fdobj_final@metadata,
      input$results_new_meta_col_name,
      input$results_new_meta_col_type,
      input$results_new_meta_col_default
    )
    if (is.null(md_final)) {
      return(NULL)
    }

    rv$fdobj_final@metadata <- md_final
    rv$fdobj@metadata <- md_final
    rv$status <- sprintf("Added final metadata column '%s'.", trimws(as.character(input$results_new_meta_col_name)))
    update_choices(rv$fdobj)
  })

  # Handsontable edits
  apply_metadata_hot_edit <- function(hot_df, md_old) {
    tryCatch({
      if (is.null(md_old)) return(NULL)
      if (is.null(hot_df) || nrow(hot_df) == 0 || !("sample" %in% colnames(hot_df))) return(NULL)
      sample_ids <- as.character(hot_df$sample)
      hot_df$sample <- NULL
      if (length(sample_ids) != nrow(hot_df)) return(NULL)

      bad_sample_ids <- is.na(sample_ids) | !nzchar(trimws(sample_ids))
      if (any(bad_sample_ids) || anyDuplicated(sample_ids) > 0) {
        shiny::showNotification(
          "Invalid metadata row key detected. Keep sample names non-empty and unique.",
          type = "warning"
        )
        return(NULL)
      }

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
      hot_df
    }, error = function(e) {
      shiny::showNotification(
        paste("Metadata edit was ignored:", e$message),
        type = "error"
      )
      NULL
    })
  }

  shiny::observeEvent(input$metadata_hot, {
    md_source <- if (!is.null(rv$fdobj_clean)) rv$fdobj_clean else rv$fdobj
    shiny::req(md_source)
    edited_md <- apply_metadata_hot_edit(
      rhandsontable::hot_to_r(input$metadata_hot),
      md_source@metadata
    )
    if (is.null(edited_md)) return(NULL)

    rv$fdobj@metadata <- edited_md

    if (!is.null(rv$fdobj_clean)) {
      rv$fdobj_clean@metadata <- metadata_keep_clean_only(edited_md)
    }

    rv$status <- "Metadata updated from Load Data table."
    update_choices(rv$fdobj)
  }, ignoreInit = TRUE)

  shiny::observeEvent(input$results_metadata_hot, {
    shiny::req(rv$fdobj_final)
    edited_md <- apply_metadata_hot_edit(
      rhandsontable::hot_to_r(input$results_metadata_hot),
      rv$fdobj_final@metadata
    )
    if (is.null(edited_md)) return(NULL)

    rv$fdobj_final@metadata <- edited_md
    rv$fdobj@metadata <- edited_md
    rv$status <- "Metadata updated from Results table."
    update_choices(rv$fdobj)
  }, ignoreInit = TRUE)

  save_plot_png <- function(file, plot_fun, width = 10, height = 7, res = 300) {
    grDevices::png(filename = file, width = width, height = height, units = "in", res = res)
    on.exit(grDevices::dev.off(), add = TRUE)
    plot_obj <- plot_fun()
    if (inherits(plot_obj, "ggplot")) {
      print(plot_obj)
    }
  }

  resolve_download_dim <- function(value, default_value) {
    dim_val <- suppressWarnings(as.numeric(value))
    if (!is.finite(dim_val) || dim_val <= 0) {
      return(default_value)
    }
    dim_val
  }

  metadata_export_df <- function(fdobj) {
    shiny::req(fdobj)
    md <- fdobj@metadata
    data.frame(sample = rownames(md), md, check.names = FALSE, stringsAsFactors = FALSE)
  }

  write_metadata_xlsx <- function(out, file) {
    writexl::write_xlsx(out, path = file)
  }

  sanitize_file_name <- function(x) {
    x <- trimws(as.character(x))
    if (is.na(x) || !nzchar(x)) {
      x <- "curve"
    }
    gsub("[^A-Za-z0-9._-]+", "_", x)
  }

  write_curve_slot_csvs <- function(curve_list, out_dir, slot_prefix) {
    if (is.null(curve_list) || length(curve_list) == 0) {
      return(character(0))
    }
    curve_names <- names(curve_list)
    if (is.null(curve_names) || length(curve_names) != length(curve_list)) {
      curve_names <- rep("", length(curve_list))
    }

    out_files <- character(0)
    for (i in seq_along(curve_list)) {
      curve_df <- curve_list[[i]]
      if (is.null(curve_df)) {
        next
      }
      if (!is.data.frame(curve_df)) {
        curve_df <- tryCatch(as.data.frame(curve_df), error = function(e) NULL)
      }
      if (is.null(curve_df)) {
        next
      }

      safe_curve_name <- sanitize_file_name(curve_names[[i]])
      out_file <- file.path(out_dir, sprintf("%s_%04d_%s.csv", slot_prefix, i, safe_curve_name))
      utils::write.csv(curve_df, file = out_file, row.names = FALSE, na = "")
      out_files <- c(out_files, out_file)
    }

    out_files
  }

  zip_files_for_download <- function(zipfile, files) {
    files <- files[file.exists(files)]
    if (length(files) == 0) {
      stop("No files available for download.")
    }
    zip::zipr(zipfile = zipfile, files = files, include_directories = FALSE)
    invisible(TRUE)
  }

  draw_raw_index_plot <- function() {
    shiny::req(rv$fdobj)

    curvana::plot_deflection_curves_by_index(
      fdobj           = rv$fdobj,
      curve           = "both",
      group_curves_by = if (nzchar(input$raw_index_group_by)) input$raw_index_group_by else NULL,
      split_curves_by = if (nzchar(input$raw_index_split_by)) input$raw_index_split_by else NULL,
      alpha = 0.5, point_size = 0.5, line_alpha = 0.3
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
    fd_xlim <- if (is.finite(input$fd_xmin) && is.finite(input$fd_xmax)) c(input$fd_xmin, input$fd_xmax) else NULL
    fd_ylim <- if (is.finite(input$fd_ymin) && is.finite(input$fd_ymax)) c(input$fd_ymin, input$fd_ymax) else NULL
    curvana::plot_fd_curves(
      fdobj           = rv$fdobj,
      curve           = "both",
      group_curves_by = if (nzchar(input$fd_group_by)) input$fd_group_by else NULL,
      split_curves_by = if (nzchar(input$fd_split_by)) input$fd_split_by else NULL,
      point_alpha = 0.5, line_alpha = 0.3, point_size = 0.5,
      xlim = fd_xlim,
      ylim = fd_ylim
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
    shiny::req(rv$fdobj_final)
    md <- rv$fdobj_final@metadata
    pca_features <- intersect(input$pca_include_columns, colnames(md))
    if (length(pca_features) < 2) {
      stop("Select at least 2 PCA feature columns.")
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
      point_size          = as.numeric(input$pca_point_size),
      point_alpha         = as.numeric(input$pca_point_alpha),
      arrow_color         = if (nzchar(trimws(input$pca_arrow_color))) trimws(input$pca_arrow_color) else "grey30",
      arrow_alpha         = as.numeric(input$pca_arrow_alpha),
      arrow_scale         = as.numeric(input$pca_arrow_scale),
      show_feature_labels = isTRUE(input$pca_show_feature_labels),
      feature_label_size  = as.numeric(input$pca_feature_label_size),
      base_size           = as.numeric(input$pca_base_size)
    )
  }

  draw_violin <- function() {
    shiny::req(rv$fdobj_final)
    shiny::req(nzchar(input$violin_metric))
    if (!nzchar(input$violin_group_by)) {
      stop("Select a grouping column.")
    }
    md <- rv$fdobj_final@metadata
    if (!(input$violin_group_by %in% colnames(md))) {
      stop("Grouping column not found.")
    }
    if (!(input$violin_metric %in% colnames(md))) {
      stop("Metric column not found.")
    }

    violin_color_by <- if (nzchar(input$violin_color_by) && input$violin_color_by %in% colnames(md)) {
      input$violin_color_by
    } else {
      input$violin_group_by
    }

    curvana::plot_metric_violin(
      df          = md,
      metric_name = input$violin_metric,
      group_by    = input$violin_group_by,
      color_by    = violin_color_by,
      color_map   = make_discrete_color_map(md[[violin_color_by]]),
      log10       = isTRUE(input$violin_log10),
      add_points  = isTRUE(input$violin_add_points),
      show_whisker_box = isTRUE(input$violin_show_whisker_box),
      global_test = input$violin_global_test,
      pairwise_test = input$violin_pairwise_test,
      p_adjust_method = input$violin_p_adjust_method,
      base_size   = 11
    )
  }

  draw_complex_heatmap <- function() {
    shiny::req(rv$fdobj_final)
    md <- rv$fdobj_final@metadata

    include_cols <- intersect(input$heatmap_include_columns, colnames(md))
    if (length(include_cols) < 1) {
      stop("Select at least 1 heatmap feature column.")
    }

    anno_cols <- c(input$heatmap_anno_col1, input$heatmap_anno_col2)
    anno_cols <- intersect(anno_cols[nzchar(anno_cols)], colnames(md))

    anno_color_list <- list()
    if (length(anno_cols) > 0) {
      for (cn in anno_cols) {
        cmap <- make_discrete_color_map(md[[cn]])
        if (!is.null(cmap)) {
          anno_color_list[[cn]] <- cmap
        }
      }
    }

    curvana::plot_complex_heatmap(
      df = md,
      include_columns = include_cols,
      annotate_columns = if (length(anno_cols) > 0) anno_cols else NULL,
      annotation_colors = if (length(anno_color_list) > 0) anno_color_list else NULL,
      cluster_rows = isTRUE(input$heatmap_cluster_rows),
      cluster_columns = isTRUE(input$heatmap_cluster_columns),
      show_row_names = isTRUE(input$heatmap_show_row_names),
      show_column_names = isTRUE(input$heatmap_show_column_names),
      heatmap_name = "z-score",
      draw = TRUE
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
  render_metadata_hot <- function(fdobj, validation_message, editable = TRUE) {
    shiny::validate(shiny::need(!is.null(fdobj), validation_message))
    md   <- fdobj@metadata
    disp <- data.frame(
      sample = rownames(md),
      md,
      check.names = FALSE,
      stringsAsFactors = FALSE
    )
    hot <- rhandsontable::rhandsontable(disp, rowHeaders = NULL, stretchH = "all", height = 520)
    if (isTRUE(editable)) {
      hot |>
        rhandsontable::hot_col("sample", readOnly = TRUE) |>
        rhandsontable::hot_table(
          contextMenu = TRUE,
          allowInsertRow = FALSE,
          manualColumnResize = TRUE,
          fillHandle = TRUE,
          columnSorting = TRUE
        )
    } else {
      for (cn in colnames(disp)) {
        hot <- rhandsontable::hot_col(hot, cn, readOnly = TRUE)
      }
      hot |>
        rhandsontable::hot_table(
          contextMenu = FALSE,
          allowInsertRow = FALSE,
          manualColumnResize = TRUE,
          fillHandle = FALSE,
          columnSorting = TRUE
        )
    }
  }

  output$metadata_hot <- rhandsontable::renderRHandsontable({
    render_metadata_hot(
      if (!is.null(rv$fdobj_clean)) rv$fdobj_clean else rv$fdobj,
      "Load curve data to preview the metadata generated by createFdObjFromFolder."
    )
  })

  output$results_metadata_hot <- rhandsontable::renderRHandsontable({
    render_metadata_hot(rv$fdobj_final, "Run analytical metrics to view final metadata results.")
  })

  output$download_metadata_csv <- shiny::downloadHandler(
    filename = function() {
      paste0("curvana_metadata_", Sys.Date(), ".csv")
    },
    content = function(file) {
      out <- metadata_export_df(rv$fdobj_clean)
      utils::write.csv(out, file = file, row.names = FALSE, na = "")
    }
  )

  output$download_metadata_xlsx <- shiny::downloadHandler(
    filename = function() {
      paste0("curvana_metadata_", Sys.Date(), ".xlsx")
    },
    content = function(file) {
      out <- metadata_export_df(rv$fdobj_clean)
      write_metadata_xlsx(out, file)
    }
  )

  output$download_results_metadata_csv <- shiny::downloadHandler(
    filename = function() {
      paste0("curvana_results_metadata_", Sys.Date(), ".csv")
    },
    content = function(file) {
      out <- metadata_export_df(rv$fdobj_final)
      utils::write.csv(out, file = file, row.names = FALSE, na = "")
    }
  )

  output$download_results_metadata_xlsx <- shiny::downloadHandler(
    filename = function() {
      paste0("curvana_results_metadata_", Sys.Date(), ".xlsx")
    },
    content = function(file) {
      out <- metadata_export_df(rv$fdobj_final)
      write_metadata_xlsx(out, file)
    }
  )

  output$download_all_raw_curves_zip <- shiny::downloadHandler(
    filename = function() {
      paste0("curvana_raw_curves_", Sys.Date(), ".zip")
    },
    content = function(file) {
      shiny::req(rv$fdobj_final)
      raw_curves <- rv$fdobj_final@rawCurves
      if (is.null(raw_curves) || length(raw_curves) == 0) {
        stop("No raw curves available in rawCurves slot.")
      }

      tmp_dir <- tempfile("raw_curves_zip_")
      dir.create(tmp_dir, recursive = TRUE, showWarnings = FALSE)
      on.exit(unlink(tmp_dir, recursive = TRUE, force = TRUE), add = TRUE)

      files <- write_curve_slot_csvs(raw_curves, tmp_dir, "raw")
      if (length(files) == 0) {
        stop("No raw curves could be exported.")
      }
      zip_files_for_download(file, files)
    }
  )

  output$download_all_transformed_curves_zip <- shiny::downloadHandler(
    filename = function() {
      paste0("curvana_transformed_curves_", Sys.Date(), ".zip")
    },
    content = function(file) {
      shiny::req(rv$fdobj_final)
      approach_curves <- rv$fdobj_final@approachCurves
      retract_curves <- rv$fdobj_final@retractCurves

      tmp_dir <- tempfile("transformed_curves_zip_")
      dir.create(tmp_dir, recursive = TRUE, showWarnings = FALSE)
      on.exit(unlink(tmp_dir, recursive = TRUE, force = TRUE), add = TRUE)

      approach_files <- write_curve_slot_csvs(approach_curves, tmp_dir, "approach")
      retract_files <- write_curve_slot_csvs(retract_curves, tmp_dir, "retract")
      files <- c(approach_files, retract_files)

      if (length(files) == 0) {
        stop("No transformed curves found in approachCurves or retractCurves slots.")
      }
      zip_files_for_download(file, files)
    }
  )

  output$download_fdobj_rds <- shiny::downloadHandler(
    filename = function() {
      paste0("curvana_fdobj_", Sys.Date(), ".rds")
    },
    content = function(file) {
      shiny::req(rv$fdobj_final)
      saveRDS(rv$fdobj_final, file = file)
    }
  )

  # Raw deflection index plot
  output$raw_index_plot <- shiny::renderPlot({
    shiny::req(input$plot_raw_index_btn > 0)
    isolate(draw_raw_index_plot())
  })

  output$download_raw_index_plot <- shiny::downloadHandler(
    filename = function() {
      paste0("curvana_raw_deflection_by_index_", Sys.Date(), ".png")
    },
    content = function(file) {
      save_plot_png(
        file,
        draw_raw_index_plot,
        width = resolve_download_dim(input$raw_index_download_width, 12),
        height = resolve_download_dim(input$raw_index_download_height, 9),
        res = 300
      )
    }
  )

  # Raw deflection curves
  output$raw_curves_plot <- shiny::renderPlot({
    shiny::req(input$plot_raw_curves_btn > 0)
    isolate(draw_raw_curves())
  })

  output$download_raw_curves <- shiny::downloadHandler(
    filename = function() {
      paste0("curvana_raw_deflection_curves_", Sys.Date(), ".png")
    },
    content = function(file) {
      save_plot_png(
        file,
        draw_raw_curves,
        width = resolve_download_dim(input$raw_curves_download_width, 12),
        height = resolve_download_dim(input$raw_curves_download_height, 8),
        res = 300
      )
    }
  )

  # FD curves
  output$fd_curves_plot <- shiny::renderPlot({
    shiny::req(input$plot_fd_curves_btn > 0)
    isolate(draw_fd_curves())
  })

  output$download_fd_curves <- shiny::downloadHandler(
    filename = function() {
      paste0("curvana_transformed_fd_curves_", Sys.Date(), ".png")
    },
    content = function(file) {
      save_plot_png(
        file,
        draw_fd_curves,
        width = resolve_download_dim(input$fd_curves_download_width, 12),
        height = resolve_download_dim(input$fd_curves_download_height, 8),
        res = 300
      )
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
      save_plot_png(
        file,
        draw_single_curve,
        width = resolve_download_dim(input$single_curve_download_width, 14),
        height = resolve_download_dim(input$single_curve_download_height, 7),
        res = 300
      )
    }
  )

  output$download_single_curve_data <- shiny::downloadHandler(
    filename = function() {
      shiny::req(rv$fdobj)
      shiny::req(nzchar(input$metric_curve_name))
      curve_name <- input$metric_curve_name
      use_curve <- input$metric_use_curve
      safe_curve_name <- gsub("[^A-Za-z0-9._-]+", "_", curve_name)
      paste0(safe_curve_name, "_", use_curve, ".data.zip")
    },
    content = function(file) {
      shiny::req(rv$fdobj)
      shiny::req(nzchar(input$metric_curve_name))
      fdobj <- rv$fdobj
      curve_name <- input$metric_curve_name
      use_curve <- input$metric_use_curve
      safe_curve_name <- gsub("[^A-Za-z0-9._-]+", "_", curve_name)

      if (!(curve_name %in% names(fdobj@rawCurves))) {
        stop("Selected curve not found in raw curves.")
      }

      transformed_list <- if (identical(use_curve, "approach")) {
        fdobj@approachCurves
      } else {
        fdobj@retractCurves
      }

      if (!(curve_name %in% names(transformed_list))) {
        stop("Selected curve not found in transformed curves.")
      }

      raw_df <- fdobj@rawCurves[[curve_name]]
      transformed_df <- transformed_list[[curve_name]]

      if (!is.data.frame(raw_df)) {
        raw_df <- data.frame()
      }
      if (!is.data.frame(transformed_df)) {
        transformed_df <- data.frame()
      }

      tmp_dir <- tempfile("single_curve_data_")
      dir.create(tmp_dir, recursive = TRUE, showWarnings = FALSE)
      on.exit(unlink(tmp_dir, recursive = TRUE, force = TRUE), add = TRUE)

      raw_csv <- file.path(tmp_dir, paste0(safe_curve_name, "_", use_curve, "_raw.csv"))
      transformed_csv <- file.path(tmp_dir, paste0(safe_curve_name, "_", use_curve, "_transformed.csv"))

      utils::write.csv(raw_df, file = raw_csv, row.names = FALSE, na = "")
      utils::write.csv(transformed_df, file = transformed_csv, row.names = FALSE, na = "")

      zip::zipr(zipfile = file, files = c(raw_csv, transformed_csv), include_directories = FALSE)
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
      save_plot_png(
        file,
        draw_pca,
        width = resolve_download_dim(input$pca_download_width, 10),
        height = resolve_download_dim(input$pca_download_height, 7),
        res = 300
      )
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
      save_plot_png(
        file,
        draw_violin,
        width = resolve_download_dim(input$violin_download_width, 9),
        height = resolve_download_dim(input$violin_download_height, 7),
        res = 300
      )
    }
  )

  # Complex heatmap
  output$complex_heatmap_plot <- shiny::renderPlot({
    draw_complex_heatmap()
  })

  output$download_complex_heatmap <- shiny::downloadHandler(
    filename = function() {
      paste0("curvana_complex_heatmap_", Sys.Date(), ".png")
    },
    content = function(file) {
      save_plot_png(
        file,
        draw_complex_heatmap,
        width = resolve_download_dim(input$heatmap_download_width, 11),
        height = resolve_download_dim(input$heatmap_download_height, 8),
        res = 300
      )
    }
  )
}

shiny::shinyApp(ui = ui, server = server)

