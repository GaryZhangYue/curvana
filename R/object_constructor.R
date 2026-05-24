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


#' Constructor for fdObj from a folder of JPK format AFM force–distance curve files
#'
#' @description
#' This function reads JPK format force-distance curve files exported from JPK AFM software.
#' The function automatically detects and separates approach and retract segments, extracts
#' sensitivity and spring constant, and handles unit conversion from Force to Voltage if needed.
#'
#' @param folder Path to a folder containing JPK format AFM curve files (.txt)
#' @param suffix File extension to look for (default: ".txt")
#' @param pattern Optional string pattern to filter files (e.g., "experiment"); default is "" (no filtering)
#' @param metadata Optional data.frame. If provided, file names (excluding the suffix) matched to rownames(metadata) will be read in. Returns error if any files not found.
#' @param threads Number of parallel threads to use for file reading. Default = 1 (sequential).
#' @param height_col Column name for height/distance data in JPK files. Default is "height".
#' @param deflection_col Column name for deflection data in JPK files. Default is "vDeflection".
#'
#' @return An object of class \code{fdObj}
#' @export
#' 
#' @details
#' The function reads JPK text export files and automatically:
#' \itemize{
#'   \item Detects approach (extend) and retract segments
#'   \item Extracts sensitivity and spring constant separately for each segment
#'   \item Determines if deflection data is in Voltage (V) or Force (N)
#'   \item If Force (N), converts to Voltage using: V = F / (sensitivity * springConstant)
#'   \item Converts distance from meters to nanometers
#'   \item Adds segment-specific columns to metadata: \code{approach_sensitivity_imported}, 
#'     \code{approach_springConstant_imported}, \code{retract_sensitivity_imported}, 
#'     \code{retract_springConstant_imported}, \code{Number_of_datapoints_approach},
#'     \code{Number_of_datapoints_retract}
#' }
#' 
#' The returned \code{fdObj} contains:
#' \itemize{
#'   \item rawCurves: Raw data with approach and retract columns (distance in nm, deflection in V).
#'     If segments have different lengths, shorter ones are padded with NA. Analysis functions
#'     use \code{Number_of_datapoints_*} columns to exclude padded NA values.
#'   \item approachCurves: Empty initially (to be filled by transform_curves)
#'   \item retractCurves: Empty initially (to be filled by transform_curves)
#'   \item metadata: Sample information including segment-specific imported sensitivity and spring constant,
#'     plus data point counts
#' }
#'
#' @examples
#' \dontrun{
#' # Read all JPK files from a folder
#' fd_obj <- createFdObjFromJPKFolder("path/to/jpk/files")
#' 
#' # Read files matching a pattern with metadata
#' metadata <- data.frame(
#'   sample_type = c("control", "treated"),
#'   row.names = c("file1", "file2")
#' )
#' fd_obj <- createFdObjFromJPKFolder("path/to/jpk/files", metadata = metadata)
#' 
#' # Check imported parameters (segment-specific)
#' head(fd_obj@metadata[, c("approach_sensitivity_imported", 
#'                          "approach_springConstant_imported",
#'                          "retract_sensitivity_imported",
#'                          "retract_springConstant_imported",
#'                          "Number_of_datapoints_approach",
#'                          "Number_of_datapoints_retract")])
#' }
createFdObjFromJPKFolder <- function(folder,
                                     suffix = ".txt",
                                     pattern = "",
                                     metadata = NULL,
                                     threads = 1,
                                     height_col = "height",
                                     deflection_col = "vDeflection") {
  
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
    future::plan(future::multisession, workers = threads)
    read_func <- future.apply::future_lapply
  } else {
    read_func <- lapply
  }
  
  # Read JPK files using custom JPK reader
  message("Reading JPK files...")
  jpk_data_list <- setNames(
    read_func(files_to_read, function(f) {
      tryCatch({
        read_jpk_file(f, height_col = height_col, deflection_col = deflection_col)
      }, error = function(e) {
        warning(sprintf("Failed to read file %s: %s", f, e$message))
        return(NULL)
      })
    }),
    curve_names
  )
  
  # Remove NULL entries (failed reads)
  failed_reads <- sapply(jpk_data_list, is.null)
  if (any(failed_reads)) {
    warning(sprintf("%d files failed to read and were excluded", sum(failed_reads)))
    jpk_data_list <- jpk_data_list[!failed_reads]
    curve_names <- names(jpk_data_list)
    metadata <- metadata[curve_names, , drop = FALSE]
  }
  
  if (length(jpk_data_list) == 0) {
    stop("No files were successfully read")
  }
  
  # Extract raw curves and add parameters to metadata
  rawCurves <- setNames(
    lapply(names(jpk_data_list), function(name) {
      jpk_data_list[[name]]$raw_data
    }),
    names(jpk_data_list)
  )
  
  # Add segment-specific sensitivity and spring constant to metadata
  for (name in names(jpk_data_list)) {
    params <- jpk_data_list[[name]]$parameters
    metadata[name, "approach_sensitivity_imported"] <- params["approach_sensitivity_imported"]
    metadata[name, "approach_springConstant_imported"] <- params["approach_springConstant_imported"]
    metadata[name, "retract_sensitivity_imported"] <- params["retract_sensitivity_imported"]
    metadata[name, "retract_springConstant_imported"] <- params["retract_springConstant_imported"]
    metadata[name, "Number_of_datapoints_approach"] <- params["Number_of_datapoints_approach"]
    metadata[name, "Number_of_datapoints_retract"] <- params["Number_of_datapoints_retract"]
  }
  
  # Initialize empty lists for approach/retract (will be filled by transform_curves)
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
  
  message(sprintf("Successfully created fdObj with %d curves", length(curve_names)))
  
  # Construct and return fdObj
  new("fdObj",
      rawCurves = rawCurves,
      approachCurves = approachCurves,
      retractCurves = retractCurves,
      metadata = metadata,
      senscal_segment = senscal_segment,
      baseline_segment = baseline_segment)
}

