#' Constructor for fdObj from a folder of AFM force–distance curve files
#' Each curve should have the contact region at the beginning of the curve and the non-interaction region 
#' at the end of the curve, so that the approach and retract segments can be correctly identified and processed.
#' However, different instruments generate curves with different column names, orientations, and conventions,
#' so the user must specify which columns correspond to approach and retract distances and deflections,
#' and whether any of these columns need to be reversed to ensure the contact and non-interaction regions
#' are correctly positioned at the beginning and end of the curve, respectively.
#' @param folder Path to a folder containing the raw AFM curve files
#' @param suffix File extension to look for (e.g., ".txt")
#' @param pattern Optional string pattern to filter files (e.g., "experiment"); default is "" (no filtering)
#' @param Calc_Ramp_Ex_nm Column name for approach distance (default: "Calc_Ramp_Ex_nm")
#' @param Calc_Ramp_Rt_nm Column name for retract distance (default: "Calc_Ramp_Rt_nm")
#' @param Defl_V_Ex Column name for approach deflection (default: "Defl_V_Ex")
#' @param Defl_V_Rt Column name for retract deflection (default: "Defl_V_Rt")
#' @param reverse_Calc_Ramp_Ex_nm Logical; reverse approach distance column if TRUE. Default = FALSE.
#' @param reverse_Calc_Ramp_Rt_nm Logical; reverse retract distance column if TRUE. Default = TRUE.
#' @param reverse_Defl_V_Ex Logical; reverse approach deflection column if TRUE. Default = TRUE.
#' @param reverse_Defl_V_Rt Logical; reverse retract deflection column if TRUE. Default = FALSE.
#' @param metadata Optional data.frame. If provided, file names (excluding the suffix) matched to rownames(metadata) will be read in. Returns error if any files not found.
#' @param threads Number of parallel threads to use for file reading. Default = 1 (sequential).
#'
#' @return An object of class \code{fdObj}
#' @export
createFdObjFromFolder <- function(folder,
                                  suffix = ".txt",
                                  pattern = "",
                                  Displacement_Approach = "Calc_Ramp_Ex_nm",
                                  Displacement_Retract = "Calc_Ramp_Rt_nm",
                                  Deflection_Approach = "Defl_V_Ex",
                                  Deflection_Retract = "Defl_V_Rt",
                                  metadata = NULL,
                                  threads = 1,
                                  reverse_Displacement_Approach = FALSE,
                                  reverse_Displacement_Retract = TRUE,
                                  reverse_Deflection_Approach = TRUE,
                                  reverse_Deflection_Retract = FALSE) {
                                    
  Calc_Ramp_Ex_nm <- Displacement_Approach
  Calc_Ramp_Rt_nm <- Displacement_Retract
  Defl_V_Ex <- Deflection_Approach
  Defl_V_Rt <- Deflection_Retract

  reverse_Calc_Ramp_Ex_nm <- reverse_Displacement_Approach
  reverse_Calc_Ramp_Rt_nm <- reverse_Displacement_Retract
  reverse_Defl_V_Ex <- reverse_Deflection_Approach
  reverse_Defl_V_Rt <- reverse_Deflection_Retract

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

  rawCurves <- setNames(
    read_func(files_to_read, function(f) {
      df <- read.table(f, header = TRUE, sep = "\t", stringsAsFactors = FALSE)

      has_approach <- all(c(Calc_Ramp_Ex_nm, Defl_V_Ex) %in% colnames(df))
      has_retract <- all(c(Calc_Ramp_Rt_nm, Defl_V_Rt) %in% colnames(df))

      if (!has_approach && !has_retract) {
        stop(sprintf(
          "File %s is missing both approach columns (%s, %s) and retract columns (%s, %s).",
          f,
          Calc_Ramp_Ex_nm,
          Defl_V_Ex,
          Calc_Ramp_Rt_nm,
          Defl_V_Rt
        ))
      }

      cols <- list()
      if (has_approach) {
        cols[["Calc_Ramp_Ex_nm"]] <- if (reverse_Calc_Ramp_Ex_nm) rev(df[[Calc_Ramp_Ex_nm]]) else df[[Calc_Ramp_Ex_nm]]
        cols[["Defl_V_Ex"]] <- if (reverse_Defl_V_Ex) rev(df[[Defl_V_Ex]]) else df[[Defl_V_Ex]]
      }
      if (has_retract) {
        cols[["Calc_Ramp_Rt_nm"]] <- if (reverse_Calc_Ramp_Rt_nm) rev(df[[Calc_Ramp_Rt_nm]]) else df[[Calc_Ramp_Rt_nm]]
        cols[["Defl_V_Rt"]] <- if (reverse_Defl_V_Rt) rev(df[[Defl_V_Rt]]) else df[[Defl_V_Rt]]
      }

      as.data.frame(cols, stringsAsFactors = FALSE)
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

