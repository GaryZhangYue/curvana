#' Constructor for fdObj from a folder of AFM force–distance curve files
#'
#' @param folder Path to a folder containing the raw AFM curve files
#' @param suffix File extension to look for (e.g., ".txt")
#' @param pattern Optional string pattern to filter files (e.g., "experiment"); default is "" (no filtering)
#' @param Calc_Ramp_Ex_nm Column name for approach distance (default: "Calc_Ramp_Ex_nm")
#' @param Calc_Ramp_Rt_nm Column name for retract distance (default: "Calc_Ramp_Rt_nm")
#' @param Defl_V_Ex Column name for approach deflection (default: "Defl_V_Ex")
#' @param Defl_V_Rt Column name for retract deflection (default: "Defl_V_Rt")
#' @param metadata Optional data.frame. If provided, file names (excluding the suffix) matched to rownames(metadata) will be read in. Returns error if any files not found.
#' @param threads Number of parallel threads to use for file reading. Default = 1 (sequential).
#'
#' @return An object of class \code{fdObj}
#' @export
createFdObjFromFolder <- function(folder,
                                  suffix = ".txt",
                                  pattern = "",
                                  Calc_Ramp_Ex_nm = "Calc_Ramp_Ex_nm",
                                  Calc_Ramp_Rt_nm = "Calc_Ramp_Rt_nm",
                                  Defl_V_Ex       = "Defl_V_Ex",
                                  Defl_V_Rt       = "Defl_V_Rt",
                                  metadata = NULL,
                                  threads = 1) {
  if (!dir.exists(folder)) {
    stop("Folder does not exist: ", folder)
  }

  # Determine files to load
  all_files <- list.files(folder, pattern = paste0(pattern, ".*\\", suffix, "$"), full.names = TRUE)

  if (!is.null(metadata)) {
    expected_files <- file.path(folder, paste0(rownames(metadata), suffix))
    missing <- expected_files[!file.exists(expected_files)]
    if (length(missing) > 0) {
      stop("The following metadata-linked files are missing:\n", paste(missing, collapse = "\n"))
    }
    files_to_read <- expected_files
    curve_names <- rownames(metadata)
  } else {
    if (length(all_files) == 0) {
      stop("No matching files found with given pattern and suffix")
    }
    files_to_read <- all_files
    curve_names <- tools::file_path_sans_ext(basename(files_to_read))
    metadata <- data.frame(row.names = curve_names, filename = curve_names)
  }

  # Read files: parallel if threads > 1
  if (threads > 1) {
    if (!requireNamespace("future.apply", quietly = TRUE)) {
      stop("Package 'future.apply' is required for multithreading. Please install it.")
    }
    future::plan(future::multisession, workers = threads)
    read_func <- future.apply::future_lapply
  } else {
    read_func <- lapply
  }

  required_cols <- c(Calc_Ramp_Ex_nm, Calc_Ramp_Rt_nm, Defl_V_Ex, Defl_V_Rt)
  rawCurves <- setNames(
    read_func(files_to_read, function(f) {
      df <- read.table(f, header = TRUE, sep = "\t", stringsAsFactors = FALSE)
      if (!all(required_cols %in% colnames(df))) {
        warning(sprintf("File %s is missing required columns", f))
        return(NULL)
      }
      df %>%
        transmute(
          Calc_Ramp_Ex_nm = .data[[Calc_Ramp_Ex_nm]],
          Calc_Ramp_Rt_nm = rev(.data[[Calc_Ramp_Rt_nm]]),
          Defl_V_Ex       = rev(.data[[Defl_V_Ex]]),
          Defl_V_Rt       = .data[[Defl_V_Rt]]
        )
    }),
    curve_names
  )

  # Initialize empty lists for approach/retract
  empty_appr_df <- data.frame(distance = numeric(0), force = numeric(0))
  empty_retr_df <- data.frame(distance = numeric(0), force = numeric(0))
  approachCurves <- setNames(rep(list(empty_appr_df), length(curve_names)), curve_names)
  retractCurves  <- setNames(rep(list(empty_retr_df), length(curve_names)), curve_names)

  # Initialize senscal_segment and baseline_segment
  empty_sens_appr <- data.frame(Calc_Ramp_Ex_nm = numeric(0), Defl_V_Ex = numeric(0))
  empty_sens_retr <- data.frame(Calc_Ramp_Rt_nm = numeric(0), Defl_V_Rt = numeric(0))

  senscal_segment <- list(
    approach = setNames(rep(list(empty_sens_appr), length(curve_names)), curve_names),
    retract  = setNames(rep(list(empty_sens_retr), length(curve_names)), curve_names)
  )
  baseline_segment <- list(
    approach = setNames(rep(list(empty_sens_appr), length(curve_names)), curve_names),
    retract  = setNames(rep(list(empty_sens_retr), length(curve_names)), curve_names)
  )

  # Construct and return fdObj
  new("fdObj",
      rawCurves = rawCurves,
      approachCurves = approachCurves,
      retractCurves = retractCurves,
      metadata = metadata,
      senscal_segment = senscal_segment,
      baseline_segment = baseline_segment)
}

