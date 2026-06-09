#' @importFrom dplyr %>%
NULL

#' Calculate Sensitivity from a Segment of AFM Data
#'
#' This function identifies a linear segment from AFM deflection vs. piezo extension data and calculates
#' the sensitivity (slope) using linear regression. It iteratively adds chunks of data to build a segment
#' with high linearity (\eqn{R^2 >} \code{R_squared_min}) and optionally removes outliers to maintain this criterion.
#'
#' @param x Numeric vector. The piezo extension values (e.g., distance in nm).
#' @param y Numeric vector. The deflection values (e.g., voltage).
#' @param R_squared_min Numeric. Minimum \eqn{R^2} threshold for accepting a
#'   segment as sufficiently linear during sensitivity calculation. Default is
#'   0.99.
#' @param end Integer. The maximum index to consider in the `x` and `y` vectors (i.e., up to which point to search).
#' @param intv Integer. Number of data points added per iteration when
#'   building the linear sensitivity segment.
#' @param minimum_length Integer. Minimum number of accumulated points required
#'   for a valid sensitivity result. If the segment is shorter, sensitivity is
#'   reported as \code{NA}. Default is 4.
#'
#' @return A list of three elements:
#' \describe{
#'   \item{senscal_segment}{a dataframe of a subset of points in the original raw curve which are used for sensitivity calibration}
#'   \item{sensitivity}{The calculated sensitivity (slope) as a numeric value, or \code{NULL} if not computable.}
#' }
#'
#' @examples
#' x <- c(
#'   0.000, 0.977, 1.953, 2.930, 3.906, 4.883, 5.859, 6.836, 7.812, 8.789,
#'   9.766, 10.742, 11.719, 12.695, 13.672, 14.648, 15.625, 16.602, 17.578, 18.555,
#'   19.531, 20.508, 21.484, 22.461, 23.438, 24.414, 25.391, 26.367, 27.344, 28.320,
#'   29.297, 30.273, 31.250, 32.227, 33.203, 34.180, 35.156, 36.133, 37.109, 38.086,
#'   39.062, 40.039, 41.016, 41.992, 42.969, 43.945, 44.922, 45.898, 46.875, 47.852,
#'   48.828, 49.805, 50.781, 51.758, 52.734, 53.711, 54.688, 55.664, 56.641, 57.617,
#'   58.594, 59.570, 60.547, 61.523, 62.500, 63.477, 64.453, 65.430, 66.406, 67.383,
#'   68.359, 69.336, 70.312, 71.289, 72.266, 73.242, 74.219, 75.195, 76.172, 77.148,
#'   78.125, 79.102, 80.078, 81.055, 82.031, 83.008, 83.984, 84.961, 85.938, 86.914,
#'   87.891, 88.867, 89.844, 90.820, 91.797, 92.773, 93.750, 94.727, 95.703, 96.680
#' )
#'
#' y <- c(
#'   0.255, 0.243, 0.231, 0.220, 0.208, 0.196, 0.184, 0.172, 0.160, 0.149,
#'   0.137, 0.125, 0.114, 0.101, 0.088, 0.074, 0.061, 0.048, 0.035, 0.024,
#'   0.012, 0.001, -0.012, -0.025, -0.037, -0.050, -0.061, -0.070, -0.078, -0.087,
#'   -0.097, -0.105, -0.109, -0.098, -0.085, -0.072, -0.071, -0.070, -0.066, -0.061,
#'   -0.057, -0.055, -0.054, -0.055, -0.054, -0.054, -0.054, -0.055, -0.055, -0.055,
#'   -0.056, -0.056, -0.056, -0.056, -0.055, -0.056, -0.056, -0.056, -0.056, -0.056,
#'   -0.056, -0.056, -0.056, -0.056, -0.057, -0.057, -0.056, -0.056, -0.055, -0.055,
#'   -0.055, -0.055, -0.055, -0.055, -0.056, -0.056, -0.056, -0.055, -0.056, -0.056,
#'   -0.056, -0.055, -0.056, -0.055, -0.056, -0.056, -0.056, -0.055, -0.055, -0.056,
#'   -0.056, -0.055, -0.055, -0.053, -0.054, -0.054, -0.055, -0.056, -0.056, -0.057
#' )
#' calc_sensitivity(x = x, y = y, end = 80, intv = 10)
#'
#' @export
calc_sensitivity <- function(x, y, R_squared_min = 0.99, end, intv = 4, minimum_length = 4) {
  sens_x <- numeric()
  sens_y <- numeric()

  i <- 1
  while ((i + intv - 1) <= end) {
    idx <- i:(i + intv - 1)
    x_seg <- x[idx]
    y_seg <- y[idx]

    combined_x <- c(sens_x, x_seg)
    combined_y <- c(sens_y, y_seg)

    r <- suppressWarnings(cor(combined_x, combined_y, use = "complete.obs"))

    if (!is.na(r) && r^2 > R_squared_min) {
      sens_x <- combined_x
      sens_y <- combined_y
    } else {
      rescued <- FALSE
      for (j in intv:1) {
        temp_x <- x_seg[-j]
        temp_y <- y_seg[-j]

        r_try <- suppressWarnings(cor(c(sens_x, temp_x), c(sens_y, temp_y), use = "complete.obs"))

        if (!is.na(r_try) && r_try^2 > R_squared_min) {
          sens_x <- c(sens_x, temp_x)
          sens_y <- c(sens_y, temp_y)
          rescued <- TRUE
          break
        }
      }
      if (!rescued) break
    }

    i <- i + intv
  }

  if (length(sens_x) < minimum_length || length(sens_y) < minimum_length) {
    return(list(NULL, NULL))
  } else {
    fit <- lm(sens_y ~ sens_x)
    sensitivity <- -coef(fit)[2][[1]]
    senscal_segment = data.frame(x = sens_x, y = sens_y)
    return(list(senscal_segment = senscal_segment, sensitivity = sensitivity))
  }
}

#' Add Sensitivity Estimates to an fdObj
#'
#' Applies sensitivity estimation to each rawCurve in an fdObj and stores the sensitivity values
#' in the metadata and the segment used in the senscal_segment slot.
#'
#' @param fdObj An object of class \code{fdObj}.
#' @param end Integer. The maximum index in raw curves to consider (e.g., 200) in \code{calc_sensitivity()}.
#' @param intv Integer. Number of data points added per iteration when
#'   building the linear sensitivity segment in \code{calc_sensitivity()}.
#' @param R_squared_min Numeric. Minimum \eqn{R^2} threshold for accepting a
#'   segment as sufficiently linear during sensitivity calculation in
#'   \code{calc_sensitivity()}. Default is 0.99.
#' @param minimum_length Integer. Minimum number of accumulated points required
#'   for a valid sensitivity result. If the segment is shorter, sensitivity is
#'   reported as \code{NA}. Passed to \code{calc_sensitivity()}. Default is 4.
#' @param useCurve Character. Either "approach" or "retract" to determine which curve to use.
#' @param threads Number of parallel threads to use (default = 1).
#'
#' @return An updated \code{fdObj} with sensitivity values in metadata and segments in senscal_segment.
#' @examples
#' folder <- system.file("extdata", package = "curvana")
#' fd_obj <- createFdObjFromFolder(folder)
#' fd_obj <- analyze_sensitivity(fd_obj, end = 80, intv = 10, useCurve = "retract")
#' head(fd_obj@metadata$sensitivity_V_nm_retract)
#' @export
analyze_sensitivity <- function(fdObj, end = 200, intv = 4, R_squared_min = 0.99, minimum_length = 4, useCurve = "approach", threads = 1) {
  if (!inherits(fdObj, "fdObj")) stop("fdObj must be of class 'fdObj'")
  if (!useCurve %in% c("approach", "retract")) stop("useCurve must be either 'approach' or 'retract'")

  # Set column names based on approach/retract
  if (useCurve == "approach") {
    x_col <- "Calc_Ramp_Ex_nm"
    y_col <- "Defl_V_Ex"
    empty_seg <- data.frame(Calc_Ramp_Ex_nm = numeric(0), Defl_V_Ex = numeric(0))
  } else {
    x_col <- "Calc_Ramp_Rt_nm"
    y_col <- "Defl_V_Rt"
    empty_seg <- data.frame(Calc_Ramp_Rt_nm = numeric(0), Defl_V_Rt = numeric(0))
  }

  raw_list <- fdObj@rawCurves
  curve_names <- names(raw_list)

  # Determine data point count column name
  n_datapoints_col <- if (useCurve == "approach") "Number_of_datapoints_approach" else "Number_of_datapoints_retract"
  has_datapoint_info <- n_datapoints_col %in% names(fdObj@metadata)

  # Choose parallel or sequential
  if (threads > 1) {
    future::plan(future::multisession, workers = threads)
    results <- future.apply::future_lapply(curve_names, function(name) {
      df <- raw_list[[name]]
      if (!(x_col %in% colnames(df)) || !(y_col %in% colnames(df))) {
        return(list(senscal_segment = NULL, sensitivity = NULL))
      }
      x <- df[[x_col]]
      y <- df[[y_col]]
      
      # Subset to valid data if metadata is available
      if (has_datapoint_info) {
        n_valid <- fdObj@metadata[name, n_datapoints_col]
        if (!is.na(n_valid) && n_valid > 0 && n_valid <= length(x)) {
          x <- x[seq_len(n_valid)]
          y <- y[seq_len(n_valid)]
        }
      }
      
      calc_sensitivity(x = x, y = y, R_squared_min = R_squared_min, end = end, intv = intv, minimum_length = minimum_length)
    },future.packages = "curvana")
  } else {
    results <- lapply(curve_names, function(name) {
      df <- raw_list[[name]]
      if (!(x_col %in% colnames(df)) || !(y_col %in% colnames(df))) {
        return(list(senscal_segment = NULL, sensitivity = NULL))
      }
      x <- df[[x_col]]
      y <- df[[y_col]]
      
      # Subset to valid data if metadata is available
      if (has_datapoint_info) {
        n_valid <- fdObj@metadata[name, n_datapoints_col]
        if (!is.na(n_valid) && n_valid > 0 && n_valid <= length(x)) {
          x <- x[seq_len(n_valid)]
          y <- y[seq_len(n_valid)]
        }
      }
      
      calc_sensitivity(x = x, y = y, R_squared_min = R_squared_min, end = end, intv = intv, minimum_length = minimum_length)
    })
  }

  # Handle failed calculations
  sensitivity_values <- sapply(results, function(r) {
    if (is.null(r$sensitivity)) NA_real_ else r$sensitivity
  })

  segments <- lapply(results, function(r) {
    if (is.null(r$senscal_segment)) empty_seg else {
      seg <- r$senscal_segment
      colnames(seg) <- colnames(empty_seg)
      seg
    }
  })
  names(segments) <- curve_names

  # Update metadata
  if (useCurve == 'approach'){
    fdObj@metadata$sensitivity_V_nm_approach <- sensitivity_values
  } else {
    fdObj@metadata$sensitivity_V_nm_retract <- sensitivity_values
    }


  # Update senscal_segment slot
  fdObj@senscal_segment[[useCurve]] <- segments

  # Print summary
  n_total <- length(curve_names)
  n_fail  <- sum(is.na(sensitivity_values))
  message(sprintf("Processed %d curves; %d failed sensitivity calculation.", n_total, n_fail))

  return(fdObj)
}

#' Identify Baseline Segment in AFM Raw Force-Distance Curve
#'
#' This function tests the trailing part of an AFM (Atomic Force Microscopy)
#' raw force-distance curve for a baseline segment. It takes the last
#' \code{least_length} points after scaling \code{y} by \code{sensitivity}, fits a
#' linear model of scaled deflection against \code{x}, and accepts that trailing
#' window as baseline only when two criteria are met: the absolute fitted slope
#' is smaller than \code{slp_threshold} and the standard error of that slope is
#' smaller than \code{std_threshold}. If both criteria are satisfied, the
#' function returns the mean baseline value in the original \code{y} units and
#' the accepted segment. If either criterion fails, or if the inputs are too
#' short or mismatched, no baseline is reported and \code{NULL} values are
#' returned.
#'
#' @param x Numeric vector. Distance values (e.g., piezo positions or z-sensor values).
#' @param y Numeric vector. Deflection values (e.g., in volts).
#' @param least_length Integer. Minimum number of points in the baseline segment. The function will
#' take the last \code{least_length} points of the curve for testing.
#' @param sensitivity Numeric. Scaling factor for the deflection signal (e.g., probe sensitivity)).
#' @param slp_threshold Numeric. Maximum absolute slope for the segment to be considered flat (default: 0.01).
#' @param std_threshold Numeric. Maximum standard error of the slope (default: 0.05).
#'
#' @return A list with two elements:
#' \describe{
#'   \item{baseline}{Numeric. The estimated baseline deflection value (in original units), or \code{NULL} if no valid segment is found.}
#'   \item{segment}{Data frame with \code{x} and \code{y} columns representing the detected baseline segment, or \code{NULL} if not found.}
#' }
#'
#' @details
#' The function assumes the baseline occurs toward the tail end of the curve
#' It does not search the full curve for
#' candidate windows; it evaluates only the final trailing window defined by
#' \code{least_length}. 
#'
#' @examples
#' # Simulated flat tail data
#' x <- c(
#'   0.000, 0.977, 1.953, 2.930, 3.906, 4.883, 5.859, 6.836, 7.812, 8.789,
#'   9.766, 10.742, 11.719, 12.695, 13.672, 14.648, 15.625, 16.602, 17.578, 18.555,
#'   19.531, 20.508, 21.484, 22.461, 23.438, 24.414, 25.391, 26.367, 27.344, 28.320,
#'   29.297, 30.273, 31.250, 32.227, 33.203, 34.180, 35.156, 36.133, 37.109, 38.086,
#'   39.062, 40.039, 41.016, 41.992, 42.969, 43.945, 44.922, 45.898, 46.875, 47.852,
#'   48.828, 49.805, 50.781, 51.758, 52.734, 53.711, 54.688, 55.664, 56.641, 57.617,
#'   58.594, 59.570, 60.547, 61.523, 62.500, 63.477, 64.453, 65.430, 66.406, 67.383,
#'   68.359, 69.336, 70.312, 71.289, 72.266, 73.242, 74.219, 75.195, 76.172, 77.148,
#'   78.125, 79.102, 80.078, 81.055, 82.031, 83.008, 83.984, 84.961, 85.938, 86.914,
#'   87.891, 88.867, 89.844, 90.820, 91.797, 92.773, 93.750, 94.727, 95.703, 96.680
#' )
#'
#' y <- c(
#'   0.255, 0.243, 0.231, 0.220, 0.208, 0.196, 0.184, 0.172, 0.160, 0.149,
#'   0.137, 0.125, 0.114, 0.101, 0.088, 0.074, 0.061, 0.048, 0.035, 0.024,
#'   0.012, 0.001, -0.012, -0.025, -0.037, -0.050, -0.061, -0.070, -0.078, -0.087,
#'   -0.097, -0.105, -0.109, -0.098, -0.085, -0.072, -0.071, -0.070, -0.066, -0.061,
#'   -0.057, -0.055, -0.054, -0.055, -0.054, -0.054, -0.054, -0.055, -0.055, -0.055,
#'   -0.056, -0.056, -0.056, -0.056, -0.055, -0.056, -0.056, -0.056, -0.056, -0.056,
#'   -0.056, -0.056, -0.056, -0.056, -0.057, -0.057, -0.056, -0.056, -0.055, -0.055,
#'   -0.055, -0.055, -0.055, -0.055, -0.056, -0.056, -0.056, -0.055, -0.056, -0.056,
#'   -0.056, -0.055, -0.056, -0.055, -0.056, -0.056, -0.056, -0.055, -0.055, -0.056,
#'   -0.056, -0.055, -0.055, -0.053, -0.054, -0.054, -0.055, -0.056, -0.056, -0.057
#' )
#' result <- find_baseline(x,y, least_length = 50,sensitivity = 0.012,slp_threshold = 0.02, std_threshold = 0.02)
#' print(result$baseline)
#' plot(x, y, type = "l"); lines(result$segment, col = "red", lwd = 2)
#'
#' @export
find_baseline <- function(x, y, least_length, sensitivity, slp_threshold = 0.01, std_threshold = 0.05) {
  if (length(x) < least_length + 1 || length(y) != length(x)) {
    warning("Insufficient data or mismatched x and y lengths.")
    return(list(baseline = NULL, segment = NULL))
  }

  # Scale y by sensitivity
  y_sens_crct <- y / sensitivity

  # Select the tail segment
  window_x <- tail(x, least_length + 1)[-1]
  window_y <- tail(y_sens_crct, least_length + 1)[-1]

  # Linear regression
  model <- lm(window_y ~ window_x)
  slope <- coef(model)[["window_x"]]
  std_err <- summary(model)$coefficients["window_x", "Std. Error"]

  if (abs(slope) < slp_threshold && std_err < std_threshold) {
    base_y <- window_y * sensitivity
    baseline <- mean(base_y)
    segment <- data.frame(x = window_x, y = base_y)
    return(list(baseline = baseline, segment = segment))
  } else {
    return(list(baseline = NULL, segment = NULL))
  }
}

#' Analyze Baseline from AFM Raw Curves in an fdObj
#'
#' Applies baseline detection to each raw curve stored in an \code{fdObj} and
#' writes the resulting baseline values and accepted baseline segments back into
#' the object. For each curve, the function, finds the associated sensitivity value in metadata, and then calls
#' \code{find_baseline()} on the trailing part of that curve.
#'
#' Baseline detection is therefore based on the same criteria implemented in
#' \code{find_baseline()}: the last \code{least_length} points are tested after
#' scaling the deflection signal by sensitivity, and the window is accepted only
#' if the fitted slope magnitude is below \code{slp_threshold} and the slope
#' standard error is below \code{std_threshold}. When \code{least_length} is a
#' single number, that same trailing-window size is used for all curves; when it
#' is \code{"automatic"}, per-curve window sizes are taken from the
#' corresponding \code{baseline_span_<dir>} metadata column.
#'
#' Curves with missing required columns, missing sensitivity values, invalid
#' per-curve baseline spans, or failed baseline detection are not dropped; they
#' are recorded with \code{NA} baseline values and empty baseline-segment data
#' frames. Successful results are stored in
#' \code{fdObj@metadata$baseline_V_approach} or
#' \code{fdObj@metadata$baseline_V_retract}, and in
#' \code{fdObj@baseline_segment[[useCurve]]}. If \code{least_length} is fixed,
#' the matching \code{baseline_span_<dir>} metadata column is updated to that
#' value for all curves.
#'
#' @param fdObj An object of class \code{fdObj}.
#' @param least_length Either a single integer (minimum number of points in
#'   the baseline segment) or \code{"automatic"}. When \code{"automatic"},
#'   per-curve span values are read from
#'   \code{fdObj@metadata$baseline_span_approach} or
#'   \code{fdObj@metadata$baseline_span_retract} depending on \code{useCurve}.
#' @param useCurve Character. Either "approach" or "retract" to specify which raw curve to use.
#' @param slp_threshold Numeric. Maximum absolute slope allowed for baseline detection (default = 0.01).
#' @param std_threshold Numeric. Maximum standard error of the slope (default = 0.05).
#' @param threads Integer. Number of parallel threads to use (default = 1).
#' @param soft Logical. If \code{TRUE}, use the sensitivity column specified by
#'   \code{external_sensitivity_column} instead of the default
#'   \code{sensitivity_V_nm_approach} / \code{sensitivity_V_nm_retract}.
#'   Default \code{FALSE}.
#' @param external_sensitivity_column Character. Name of a column in
#'   \code{fdObj@metadata} containing sensitivity values. Required when
#'   \code{soft = TRUE}.
#'
#' @return An updated \code{fdObj} with baseline values in metadata and baseline
#'   segments in \code{baseline_segment}. If \code{least_length} is numeric,
#'   the corresponding \code{baseline_span_<dir>} column is updated; if
#'   \code{least_length = "automatic"}, existing span columns are preserved.
#' @examples
#' folder <- system.file("extdata", package = "curvana")
#' fd_obj <- createFdObjFromFolder(folder)
#' fd_obj <- analyze_sensitivity(fd_obj, end = 80, intv = 10, useCurve = "retract")
#' fd_obj <- analyze_baseline(fd_obj, least_length = 100, useCurve = "retract", slp_threshold = 0.02, std_threshold = 0.02)
#' head(fd_obj@metadata$baseline_V_retract)
#' @export
analyze_baseline <- function(fdObj, least_length = 150, useCurve = NULL,
                             slp_threshold = 0.01, std_threshold = 0.05,
                             threads = 1, soft = FALSE,
                             external_sensitivity_column = NULL) {
  if (!inherits(fdObj, "fdObj")) stop("fdObj must be of class 'fdObj'")
  if (!useCurve %in% c("approach", "retract")) stop("useCurve must be either 'approach' or 'retract'")

  least_length_mode <- if (is.character(least_length) && length(least_length) == 1 && least_length == "automatic") {
    "automatic"
  } else if (is.numeric(least_length) && length(least_length) == 1 && is.finite(least_length) && least_length >= 1) {
    "fixed"
  } else {
    stop("least_length must be either a single numeric value >= 1 or 'automatic'.")
  }

  fixed_least_length <- if (least_length_mode == "fixed") as.integer(least_length) else NA_integer_
  span_col <- paste0("baseline_span_", useCurve)
  span_vec <- NULL

  if (least_length_mode == "automatic") {
    if (!(span_col %in% colnames(fdObj@metadata))) {
      stop(sprintf(
        "least_length = 'automatic' requires metadata column '%s'.",
        span_col
      ))
    }
    span_vec <- suppressWarnings(as.numeric(fdObj@metadata[[span_col]]))
    names(span_vec) <- rownames(fdObj@metadata)
  }

  # Set column names based on approach/retract; select the corresponding sensitivity value
  if (useCurve == "approach") {
    x_col <- "Calc_Ramp_Ex_nm"
    y_col <- "Defl_V_Ex"
    empty_seg <- data.frame(Calc_Ramp_Ex_nm = numeric(0), Defl_V_Ex = numeric(0))
  } else {
    x_col <- "Calc_Ramp_Rt_nm"
    y_col <- "Defl_V_Rt"
    empty_seg <- data.frame(Calc_Ramp_Rt_nm = numeric(0), Defl_V_Rt = numeric(0))
  }

  if (soft) {
    if (is.null(external_sensitivity_column) ||
        !is.character(external_sensitivity_column) ||
        length(external_sensitivity_column) != 1) {
      stop("external_sensitivity_column must be a single column name when soft = TRUE.")
    }
    if (!external_sensitivity_column %in% colnames(fdObj@metadata)) {
      stop(sprintf("Column '%s' not found in fdObj@metadata.", external_sensitivity_column))
    }
    sensitivity_vec <- fdObj@metadata[[external_sensitivity_column]]
  } else {
    if (useCurve == "approach") {
      sensitivity_vec <- fdObj@metadata$sensitivity_V_nm_approach
    } else {
      sensitivity_vec <- fdObj@metadata$sensitivity_V_nm_retract
    }
  }

  raw_list <- fdObj@rawCurves
  curve_names <- names(raw_list)
  names(sensitivity_vec) <- rownames(fdObj@metadata)

  if (is.null(sensitivity_vec)) stop("No sensitivity values found in metadata.")

  # Determine data point count column name
  n_datapoints_col <- if (useCurve == "approach") "Number_of_datapoints_approach" else "Number_of_datapoints_retract"
  has_datapoint_info <- n_datapoints_col %in% names(fdObj@metadata)

  find_result_for_curve <- function(name) {
    df <- raw_list[[name]]
    sensitivity <- sensitivity_vec[name]
    curve_least_length <- if (least_length_mode == "automatic") span_vec[name] else fixed_least_length

    if (is.na(sensitivity) ||
        is.na(curve_least_length) || !is.finite(curve_least_length) || curve_least_length < 1 ||
        !(x_col %in% names(df)) || !(y_col %in% names(df))) {
      return(list(baseline = NA_real_, segment = empty_seg))
    }

    x <- df[[x_col]]
    y <- df[[y_col]]
    
    # Subset to valid data if metadata is available
    if (has_datapoint_info) {
      n_valid <- fdObj@metadata[name, n_datapoints_col]
      if (!is.na(n_valid) && n_valid > 0 && n_valid <= length(x)) {
        x <- x[seq_len(n_valid)]
        y <- y[seq_len(n_valid)]
      }
    }
    
    res <- find_baseline(
      x = x,
      y = y,
      least_length = as.integer(curve_least_length),
      sensitivity = sensitivity,
      slp_threshold = slp_threshold,
      std_threshold = std_threshold
    )
    baseline <- res$baseline
    seg <- if (!is.null(res$segment)) {
      colnames(res$segment) <- c(x_col, y_col)
      res$segment
    } else {
      empty_seg
    }

    list(baseline = baseline, segment = seg)
  }

  # Apply in parallel or sequentially
  if (threads > 1) {
    future::plan(future::multisession, workers = threads)
    results <- future.apply::future_lapply(
      curve_names,
      find_result_for_curve,
      future.globals = list(
        find_result_for_curve = find_result_for_curve,
        find_baseline = find_baseline,
        raw_list = raw_list,
        sensitivity_vec = sensitivity_vec,
        span_vec = span_vec,
        least_length_mode = least_length_mode,
        fixed_least_length = fixed_least_length,
        x_col = x_col,
        y_col = y_col,
        empty_seg = empty_seg,
        slp_threshold = slp_threshold,
        std_threshold = std_threshold
      )
    )
  } else {
    results <- lapply(curve_names, find_result_for_curve)
  }

  # Extract results
  if (!all(names(results) == curve_names)) stop("the input and output order does not match")
  baseline_values <- sapply(results, function(r) if (is.null(r$baseline)) NA_real_ else r$baseline)
  baseline_segments <- lapply(results, function(r) r$segment)
  names(baseline_segments) <- curve_names

  # Update fdObj
  if(useCurve == 'approach') {
    fdObj@metadata$baseline_V_approach <- baseline_values
    if (least_length_mode == "fixed") {
      fdObj@metadata$baseline_span_approach <- fixed_least_length
    }
    } else {
    fdObj@metadata$baseline_V_retract <- baseline_values
    if (least_length_mode == "fixed") {
      fdObj@metadata$baseline_span_retract <- fixed_least_length
    }
  }
  fdObj@baseline_segment[[useCurve]] <- baseline_segments

  # Summary
  n_fail <- sum(is.na(baseline_values))
  message(sprintf("Processed %d curves; %d failed baseline detection.", length(curve_names), n_fail))

  return(fdObj)
}

#' Denoise Deflection Columns in a Raw Curve Data Frame
#'
#' Applies Savitzky-Golay smoothing to deflection columns in a raw curve data frame.
#' The original values are first copied to new columns with suffix
#' \code{_original}, then the target columns are overwritten with smoothed values.
#' If segment length information is provided via \code{n_approach} and \code{n_retract},
#' only the valid (non-NA) portion of each segment is denoised, and the result is
#' padded back to the original length with NA.
#'
#' @param raw_curve A raw curve data frame. 
#' @param p Filter polynomial order.
#' @param n Filter length (must be odd).
#' @param m Order of the derivative to compute.
#' @param ts Sampling interval.
#' @param useCurve Character; one of \code{c("retract", "approach", "both")}.
#' @param n_approach Optional. Number of valid data points in approach segment.
#'   If provided, only the first \code{n_approach} rows of approach columns are denoised.
#' @param n_retract Optional. Number of valid data points in retract segment.
#'   If provided, only the first \code{n_retract} rows of retract columns are denoised.
#'
#' @return A data frame with original columns showing for the smoothed deflection values 
#' and additional \code{*_original} as backup column(s) for the raw deflection values (before denoising).
#' 
#' @examples
#' x <- c(
#'   0.000, 0.977, 1.953, 2.930, 3.906, 4.883, 5.859, 6.836, 7.812, 8.789,
#'   9.766, 10.742, 11.719, 12.695, 13.672, 14.648, 15.625, 16.602, 17.578, 18.555,
#'   19.531, 20.508, 21.484, 22.461, 23.438, 24.414, 25.391, 26.367, 27.344, 28.320,
#'   29.297, 30.273, 31.250, 32.227, 33.203, 34.180, 35.156, 36.133, 37.109, 38.086,
#'   39.062, 40.039, 41.016, 41.992, 42.969, 43.945, 44.922, 45.898, 46.875, 47.852,
#'   48.828, 49.805, 50.781, 51.758, 52.734, 53.711, 54.688, 55.664, 56.641, 57.617,
#'   58.594, 59.570, 60.547, 61.523, 62.500, 63.477, 64.453, 65.430, 66.406, 67.383,
#'   68.359, 69.336, 70.312, 71.289, 72.266, 73.242, 74.219, 75.195, 76.172, 77.148,
#'   78.125, 79.102, 80.078, 81.055, 82.031, 83.008, 83.984, 84.961, 85.938, 86.914,
#'   87.891, 88.867, 89.844, 90.820, 91.797, 92.773, 93.750, 94.727, 95.703, 96.680
#' )
#'
#' y <- c(
#'   0.255, 0.243, 0.231, 0.220, 0.208, 0.196, 0.184, 0.172, 0.160, 0.149,
#'   0.137, 0.125, 0.114, 0.101, 0.088, 0.074, 0.061, 0.048, 0.035, 0.024,
#'   0.012, 0.001, -0.012, -0.025, -0.037, -0.050, -0.061, -0.070, -0.078, -0.087,
#'   -0.097, -0.105, -0.109, -0.098, -0.085, -0.072, -0.071, -0.070, -0.066, -0.061,
#'   -0.057, -0.055, -0.054, -0.055, -0.054, -0.054, -0.054, -0.055, -0.055, -0.055,
#'   -0.056, -0.056, -0.056, -0.056, -0.055, -0.056, -0.056, -0.056, -0.056, -0.056,
#'   -0.056, -0.056, -0.056, -0.056, -0.057, -0.057, -0.056, -0.056, -0.055, -0.055,
#'   -0.055, -0.055, -0.055, -0.055, -0.056, -0.056, -0.056, -0.055, -0.056, -0.056,
#'   -0.056, -0.055, -0.056, -0.055, -0.056, -0.056, -0.056, -0.055, -0.055, -0.056,
#'   -0.056, -0.055, -0.055, -0.053, -0.054, -0.054, -0.055, -0.056, -0.056, -0.057
#' )
#' denoised_df <- denoise_a_curve(data.frame(Calc_Ramp_Ex_nm = x, Defl_V_Ex = y), p = 1, n = 5, m = 0, ts = 1, useCurve = "approach", n_approach = length(x))
#' head(denoised_df)
#' @export
denoise_a_curve <- function(raw_curve,
                    p = 1,
                    n = 3,
                    m = 0,
                    ts = 1,
                    useCurve = c("retract", "approach", "both"),
                    n_approach = NULL,
                    n_retract = NULL) {
  if (!is.data.frame(raw_curve)) {
    stop("raw_curve must be a data.frame")
  }
  if (!requireNamespace("signal", quietly = TRUE)) {
    stop("Package 'signal' is required. Please install it with install.packages('signal').")
  }

  useCurve <- match.arg(useCurve)

  target_cols <- switch(
    useCurve,
    approach = "Defl_V_Ex",
    retract = "Defl_V_Rt",
    both = c("Defl_V_Ex", "Defl_V_Rt")
  )

  missing_cols <- setdiff(target_cols, names(raw_curve))
  if (length(missing_cols) > 0) {
    stop(sprintf(
      "Missing required column(s) for useCurve = '%s': %s",
      useCurve,
      paste(missing_cols, collapse = ", ")
    ))
  }

  for (col_name in target_cols) {
    if (!is.numeric(raw_curve[[col_name]])) {
      stop(sprintf("Column '%s' must be numeric.", col_name))
    }
  }

  backup_cols <- paste0(target_cols, "_original")

  # Create backup columns
  raw_curve <- raw_curve %>%
    dplyr::mutate(dplyr::across(dplyr::all_of(target_cols), ~ .x, .names = "{.col}_original"))
  
  # Process each target column with appropriate subsetting
  for (col_name in target_cols) {
    # Determine valid data range for this column
    if (col_name == "Defl_V_Ex" && !is.null(n_approach)) {
      # Approach segment - use first n_approach rows
      n_valid <- min(n_approach, nrow(raw_curve))
      if (n_valid > 0) {
        valid_idx <- seq_len(n_valid)
        valid_data <- raw_curve[[col_name]][valid_idx]
        
        # Denoise only valid data
        denoised <- signal::sgolayfilt(valid_data, p = p, n = n, m = m, ts = ts)
        
        # Pad back to original length with NA
        padded <- rep(NA_real_, nrow(raw_curve))
        padded[valid_idx] <- denoised
        raw_curve[[col_name]] <- padded
      }
      
    } else if (col_name == "Defl_V_Rt" && !is.null(n_retract)) {
      # Retract segment - use first n_retract rows
      n_valid <- min(n_retract, nrow(raw_curve))
      if (n_valid > 0) {
        valid_idx <- seq_len(n_valid)
        valid_data <- raw_curve[[col_name]][valid_idx]
        
        # Denoise only valid data
        denoised <- signal::sgolayfilt(valid_data, p = p, n = n, m = m, ts = ts)
        
        # Pad back to original length with NA
        padded <- rep(NA_real_, nrow(raw_curve))
        padded[valid_idx] <- denoised
        raw_curve[[col_name]] <- padded
      }
      
    } else {
      # No subsetting info provided - denoise entire column (may contain NA)
      raw_curve[[col_name]] <- signal::sgolayfilt(raw_curve[[col_name]], p = p, n = n, m = m, ts = ts)
    }
  }

  core_cols <- c("Calc_Ramp_Ex_nm", "Calc_Ramp_Rt_nm", "Defl_V_Ex", "Defl_V_Rt")
  raw_curve <- raw_curve %>%
    dplyr::relocate(dplyr::any_of(core_cols), .before = 1) %>%
    dplyr::relocate(dplyr::any_of(backup_cols), .after = dplyr::last_col())

  return(raw_curve)
}

#' Denoise All Raw Curves in an fdObj
#'
#' Applies \code{denoise_a_curve()} to every data frame in \code{fdObj@rawCurves}
#' and writes the denoised results back to the \code{rawCurves} slot.
#'
#' @param fdObj An object of class \code{fdObj}.
#' @param p Filter polynomial order.
#' @param n Filter length (must be odd).
#' @param m Order of the derivative to compute.
#' @param ts Sampling interval.
#' @param useCurve Character; one of \code{c("retract", "approach", "both")}.
#' @param threads Integer. Number of parallel workers (default \code{1}).
#'
#' @return Updated \code{fdObj} with denoised curves in \code{rawCurves}.
#' @examples
#' folder <- system.file("extdata", package = "curvana")
#' fd_obj <- createFdObjFromFolder(folder)
#' fd_obj <- denoise_curves(fd_obj, p = 1, n = 5, m = 0, ts = 1, useCurve = "approach", threads = 1)
#' head(fd_obj@rawCurves[[1]])
#' @export
denoise_curves <- function(fdObj,
                           p = 1,
                           n = 3,
                           m = 0,
                           ts = 1,
                           useCurve = c("retract", "approach", "both"),
                           threads = 1) {
  if (!inherits(fdObj, "fdObj")) {
    stop("fdObj must be of class 'fdObj'")
  }

  useCurve <- match.arg(useCurve)
  raw_list <- fdObj@rawCurves
  curve_names <- names(raw_list)
  
  # Extract metadata for data point counts (if available)
  metadata <- fdObj@metadata
  has_counts <- all(c("Number_of_datapoints_approach", "Number_of_datapoints_retract") %in% colnames(metadata))

  run_one <- function(name) {
    # Get data point counts for this curve if available
    n_approach <- if (has_counts) metadata[name, "Number_of_datapoints_approach"] else NULL
    n_retract <- if (has_counts) metadata[name, "Number_of_datapoints_retract"] else NULL
    
    denoise_a_curve(
      raw_curve = raw_list[[name]],
      p = p,
      n = n,
      m = m,
      ts = ts,
      useCurve = useCurve,
      n_approach = n_approach,
      n_retract = n_retract
    )
  }

  results <- if (threads > 1) {
    old_plan <- future::plan()
    on.exit(future::plan(old_plan), add = TRUE)
    future::plan(future::multisession, workers = threads)
    future.apply::future_lapply(curve_names, run_one)
  } else {
    lapply(curve_names, run_one)
  }

  names(results) <- curve_names
  fdObj@rawCurves <- results

  message(sprintf("Denoised %d raw curves (useCurve = '%s').", length(curve_names), useCurve))
  return(fdObj)
}

#' Transform a Single AFM Curve into Separation Distance and Force
#'
#' Converts one raw AFM force-distance trace from piezo position and deflection
#' signal into a transformed curve containing separation distance and force.
#' The function first subtracts the supplied baseline from the raw deflection
#' signal, converts the corrected deflection from volts to deflection length,
#' multiplies by the spring constant to obtain force in nN, and then estimates a
#' contact-position offset from the supplied sensitivity-calibration segment.
#' That offset is used to shift the piezo axis and compute separation distance.
#'
#' In the default hard-reference workflow, deflection is converted to length
#' using \code{sensitivity}. When \code{soft = TRUE}, force conversion instead
#' uses \code{probe_sensitivity}, which should come from a hard reference
#' measurement of the same probe. The contact-point estimate and separation-axis
#' calculation still use \code{sensitivity} together with the supplied
#' sensitivity-calibration segment.
#'
#' The contact position is estimated by fitting a linear model to
#' \code{senscal_seg_x} against baseline-corrected, sensitivity-normalized
#' calibration deflection, and using the fitted intercept as the piezo position
#' corresponding to zero deflection. If baseline, sensitivity, or the
#' calibration segment is missing, the function returns an empty result rather
#' than a partially transformed curve.
#'
#' @param x Numeric vector. Piezo position (distance) in nm.
#' @param y Numeric vector. Deflection signal in volts.
#' @param baseline Numeric. The deflection baseline (in volts).
#' @param sensitivity Numeric. Sensitivity in V/nm.
#' @param spring_constant Numeric. Spring constant in N/m.
#' @param senscal_seg_x Numeric vector. X-values of the sensitivity calibration segment.
#' @param senscal_seg_y Numeric vector. Y-values of the sensitivity calibration segment (in volts).
#' @param soft Logical. If \code{TRUE}, use \code{probe_sensitivity} for
#'   force conversion while retaining the current transformation equations.
#' @param probe_sensitivity Numeric. Probe sensitivity measured on a hard
#'   reference surface, required when \code{soft = TRUE}.
#'
#' @return A data.frame with:
#' \describe{
#'   \item{separation_distance_nm}{Tip-sample separation (nm).}
#'   \item{force_nN}{Force (nN).}
#' }
#' If required inputs are missing, an empty data frame is returned.
#' @examples
#' #' @examples
#' x <- c(
#'   0.000, 0.977, 1.953, 2.930, 3.906, 4.883, 5.859, 6.836, 7.812, 8.789,
#'   9.766, 10.742, 11.719, 12.695, 13.672, 14.648, 15.625, 16.602, 17.578, 18.555,
#'   19.531, 20.508, 21.484, 22.461, 23.438, 24.414, 25.391, 26.367, 27.344, 28.320,
#'   29.297, 30.273, 31.250, 32.227, 33.203, 34.180, 35.156, 36.133, 37.109, 38.086,
#'   39.062, 40.039, 41.016, 41.992, 42.969, 43.945, 44.922, 45.898, 46.875, 47.852,
#'   48.828, 49.805, 50.781, 51.758, 52.734, 53.711, 54.688, 55.664, 56.641, 57.617,
#'   58.594, 59.570, 60.547, 61.523, 62.500, 63.477, 64.453, 65.430, 66.406, 67.383,
#'   68.359, 69.336, 70.312, 71.289, 72.266, 73.242, 74.219, 75.195, 76.172, 77.148,
#'   78.125, 79.102, 80.078, 81.055, 82.031, 83.008, 83.984, 84.961, 85.938, 86.914,
#'   87.891, 88.867, 89.844, 90.820, 91.797, 92.773, 93.750, 94.727, 95.703, 96.680
#' )
#'
#' y <- c(
#'   0.255, 0.243, 0.231, 0.220, 0.208, 0.196, 0.184, 0.172, 0.160, 0.149,
#'   0.137, 0.125, 0.114, 0.101, 0.088, 0.074, 0.061, 0.048, 0.035, 0.024,
#'   0.012, 0.001, -0.012, -0.025, -0.037, -0.050, -0.061, -0.070, -0.078, -0.087,
#'   -0.097, -0.105, -0.109, -0.098, -0.085, -0.072, -0.071, -0.070, -0.066, -0.061,
#'   -0.057, -0.055, -0.054, -0.055, -0.054, -0.054, -0.054, -0.055, -0.055, -0.055,
#'   -0.056, -0.056, -0.056, -0.056, -0.055, -0.056, -0.056, -0.056, -0.056, -0.056,
#'   -0.056, -0.056, -0.056, -0.056, -0.057, -0.057, -0.056, -0.056, -0.055, -0.055,
#'   -0.055, -0.055, -0.055, -0.055, -0.056, -0.056, -0.056, -0.055, -0.056, -0.056,
#'   -0.056, -0.055, -0.056, -0.055, -0.056, -0.056, -0.056, -0.055, -0.055, -0.056,
#'   -0.056, -0.055, -0.055, -0.053, -0.054, -0.054, -0.055, -0.056, -0.056, -0.057
#' )
#' transformed_df <- transform_a_curve(x, y,
#'   baseline = -0.05,
#'   sensitivity = 0.012,
#'   spring_constant = 0.1,
#'   senscal_seg_x = x[1:10],
#'   senscal_seg_y = y[1:10]
#' )
#' @export
transform_a_curve <- function(x, y,
                              baseline, sensitivity, spring_constant,
                              senscal_seg_x, senscal_seg_y,soft = FALSE, probe_sensitivity = NULL) {
  if (length(x) != length(y)) stop("x and y must be the same length.")
  if (length(senscal_seg_x) != length(senscal_seg_y)) stop("senscal_seg_x and senscal_seg_y must be the same length.")

  if(is.na(baseline) || is.na(sensitivity) || is.null(senscal_seg_x) || is.null(senscal_seg_y)){
    return(data.frame(
      separation_distance_nm = numeric(0),
      force_nN = numeric(0)
    ))
  }
  if(soft && (!is.numeric(probe_sensitivity) || length(probe_sensitivity) != 1 || is.na(probe_sensitivity) || !is.finite(probe_sensitivity) || probe_sensitivity == 0)){
    stop("probe_sensitivity must be provided as one finite non-zero numeric value when soft = TRUE.")
  }

  # Correct deflection
  new_defl_v <- y - baseline
  if(soft){
    new_defl_length_nm <- new_defl_v / probe_sensitivity
  } else {
    new_defl_length_nm <- new_defl_v / sensitivity
  }
  force_nN <- new_defl_length_nm * spring_constant

  # Estimate contact point using sensitivity calibration segment
  y_sens_norm <- (senscal_seg_y - baseline) / sensitivity
  model <- lm(senscal_seg_x ~ y_sens_norm)
  x_zero_position <- coef(model)["(Intercept)"]

  # Shift piezo to new z-position
  new_z_nm <- x - x_zero_position

  # Calculate separation distance
  separation_distance_nm <- new_z_nm + new_defl_v / sensitivity

  return(data.frame(
    separation_distance_nm = separation_distance_nm,
    force_nN = force_nN
  ))
}


#' Transform All Curves in an fdObj into Separation Distance and Force
#'
#' Applies \code{transform_a_curve()} to every raw curve stored in an
#' \code{fdObj} and writes the transformed force-separation curves back into the
#' object. The function operates on either the approach or retract branch,
#' selects the corresponding raw piezo and deflection columns, optionally
#' denoises those raw curves first, and then resolves all quantities required
#' for transformation on a per-curve basis.
#'
#' Before transforming curves, the function ensures that spring constants,
#' sensitivity values, baseline values, and sensitivity-calibration segments are
#' available. A single numeric \code{spring_constant} is copied to all curves,
#' whereas a character value is interpreted as the name of a metadata column
#' containing per-curve spring constants. If the required
#' \code{sensitivity_V_nm_<useCurve>} or \code{baseline_V_<useCurve>} metadata
#' columns are missing, the function automatically calls
#' \code{analyze_sensitivity()} and \code{analyze_baseline()} using the supplied
#' transformation arguments.
#'
#' When \code{soft = TRUE}, the function also resolves probe sensitivity values
#' from \code{probe_sensitivity_external}, stores them in
#' \code{probe_true_sensitivity_V_nm_<useCurve>}, and passes that information to
#' both baseline analysis and per-curve transformation. If metadata includes the
#' imported number of valid approach or retract points, each raw curve is first
#' truncated to that valid length before transformation so that trailing padding
#' values are ignored.
#'
#' Curves are transformed independently. For each curve with complete required
#' inputs, \code{transform_a_curve()} returns separation distance and force. If a
#' curve is missing the required raw columns, has missing baseline,
#' sensitivity, spring constant, or calibration segment, it is not dropped but
#' stored as an empty transformed data frame. Successful results are written to
#' \code{fdObj@approachCurves} or \code{fdObj@retractCurves}, depending on
#' \code{useCurve}, and a summary message reports how many curves failed to
#' transform.
#'
#' @param fdObj An object of class \code{fdObj}.
#' @param spring_constant Numeric or character. If numeric, a constant spring
#'   constant (nN/nm) applied to all curves and written to
#'   \code{fdObj@metadata$spring_constant}. If character, the name of a column
#'   in \code{fdObj@metadata} that contains per-curve spring constants.
#' @param useCurve Character; one of \code{c("approach", "retract")}.
#' @param threads Integer. Number of parallel threads to use (default = 1).
#' @param denoise_first Logical. If \code{TRUE}, denoise raw curves for the
#'   selected \code{useCurve} before transformation.
#' @param p Integer. Savitzky-Golay polynomial order used by
#'   \code{denoise_curves()} when \code{denoise_first = TRUE}.
#' @param n Integer. Savitzky-Golay window size (odd integer) used by
#'   \code{denoise_curves()} when \code{denoise_first = TRUE}.
#' @param m Integer. Savitzky-Golay derivative order used by
#'   \code{denoise_curves()} when \code{denoise_first = TRUE}.
#' @param ts Numeric. Sampling interval used by \code{denoise_curves()} when
#'   \code{denoise_first = TRUE}.
#' @param end Integer. Maximum index considered during sensitivity estimation
#'   in \code{analyze_sensitivity()}.
#' @param intv Integer. Number of data points added per iteration when
#'   building the linear sensitivity segment in \code{calc_sensitivity()} via
#'   \code{analyze_sensitivity()}.
#' @param R_squared_min Numeric. Minimum \eqn{R^2} threshold for accepting a
#'   segment as sufficiently linear during sensitivity calculation in
#'   \code{calc_sensitivity()}. Default is 0.9.
#' @param minimum_length Integer. Minimum number of accumulated points required
#'   for a valid sensitivity result. If the segment is shorter, sensitivity is
#'   reported as \code{NA} in \code{analyze_sensitivity()}.
#' @param least_length Either a single integer (minimum baseline span) or
#'   \code{"automatic"}. Passed to \code{analyze_baseline()} when baseline
#'   metadata is missing. when \code{"automatic"}, per-curve span values are read from
#'   \code{fdObj@metadata$baseline_span_approach} or
#'  \code{fdObj@metadata$baseline_span_retract} depending on \code{useCurve}.
#' @param slp_threshold Numeric. Maximum absolute baseline slope allowed in
#'   \code{analyze_baseline()}.
#' @param std_threshold Numeric. Maximum baseline slope standard error allowed
#'   in \code{analyze_baseline()}.
#' @param soft Logical. If \code{TRUE}, use \code{probe_sensitivity_external}
#'   for force conversion in \code{transform_a_curve()} and pass the
#'   resulting sensitivity column to \code{analyze_baseline()}.
#'   Default \code{FALSE}.
#' @param probe_sensitivity_external Numeric or character. Required when
#'   \code{soft = TRUE}. If numeric, a single probe sensitivity value applied
#'   to all curves. If character, the name of an existing column in
#'   \code{fdObj@metadata} containing per-curve probe sensitivity values.
#'   The values are stored in \code{probe_true_sensitivity_V_nm_<useCurve>}.
#'
#' @return An updated \code{fdObj} with transformed curves stored in the corresponding slot.
#' @examples
#' folder <- system.file("extdata", package = "curvana")
#' fd_obj <- createFdObjFromFolder(folder)
#' fd_obj <- transform_curves(fd_obj,spring_constant = 0.1, useCurve = "approach", threads = 1,denoise_first = TRUE, least_length= 300)
#' @export
transform_curves <- function(fdObj,
                             spring_constant,
                             useCurve = c("approach", "retract"),
                             threads = 1,
                             denoise_first = FALSE,
                             p = 1,
                             n = 3,
                             m = 0,
                             ts = 1,
                             end = 200,
                             intv = 4,
                             R_squared_min = 0.9,
                             minimum_length = 4,
                             least_length = 150,
                             slp_threshold = 0.01,
                             std_threshold = 0.05,
                             soft = FALSE,
                             probe_sensitivity_external = NULL) {
  # ---- Validation ----
  if (!inherits(fdObj, "fdObj"))
    stop("fdObj must be of class 'fdObj'")

  useCurve <- match.arg(useCurve)

  if (!is.logical(denoise_first) || length(denoise_first) != 1 || is.na(denoise_first)) {
    stop("denoise_first must be a single TRUE/FALSE value.")
  }
  if (!is.logical(soft) || length(soft) != 1 || is.na(soft)) {
    stop("soft must be a single TRUE/FALSE value.")
  }

  # ---- Handle probe sensitivity for soft mode ----
  probe_sens_col <- paste0("probe_true_sensitivity_V_nm_", useCurve)
  if (soft) {
    if (is.null(probe_sensitivity_external)) {
      stop("probe_sensitivity_external must be provided when soft = TRUE.")
    }
    if (is.numeric(probe_sensitivity_external) && length(probe_sensitivity_external) == 1) {
      fdObj@metadata[[probe_sens_col]] <- rep(probe_sensitivity_external, nrow(fdObj@metadata))
      message(sprintf("Using fixed probe sensitivity: %g (stored in metadata$%s).", probe_sensitivity_external, probe_sens_col))
    } else if (is.character(probe_sensitivity_external) && length(probe_sensitivity_external) == 1) {
      if (!probe_sensitivity_external %in% names(fdObj@metadata)) {
        stop(sprintf("Column '%s' not found in fdObj@metadata for probe sensitivity.", probe_sensitivity_external))
      }
      fdObj@metadata[[probe_sens_col]] <- fdObj@metadata[[probe_sensitivity_external]]
      message(sprintf("Copied probe sensitivity from column '%s' to metadata$%s.", probe_sensitivity_external, probe_sens_col))
    } else {
      stop("probe_sensitivity_external must be a single numeric value or a column name (string) in metadata.")
    }
  }

  # ---- Optional denoising ----
  if (denoise_first) {
    fdObj <- denoise_curves(
      fdObj = fdObj,
      p = p,
      n = n,
      m = m,
      ts = ts,
      useCurve = useCurve,
      threads = threads
    )
  }

  # ---- Determine column names ----
  if (useCurve == "approach") {
    x_col <- "Calc_Ramp_Ex_nm"
    y_col <- "Defl_V_Ex"
  } else {
    x_col <- "Calc_Ramp_Rt_nm"
    y_col <- "Defl_V_Rt"
  }

  raw_list <- fdObj@rawCurves
  curve_names <- names(raw_list)

  # ---- Handle spring constant ----
  if (is.numeric(spring_constant) && length(spring_constant) == 1) {
    # Use constant value and record it in metadata
    fdObj@metadata$spring_constant <- rep(spring_constant, nrow(fdObj@metadata))
    spring_constant_vec <- fdObj@metadata$spring_constant
    message(sprintf("Using fixed spring constant: %.4f nN/nm (stored in metadata$spring_constant).", spring_constant))
  } else if (is.character(spring_constant) && length(spring_constant) == 1) {
    if (!spring_constant %in% names(fdObj@metadata)) {
      stop(sprintf("Column '%s' not found in fdObj@metadata for spring constants.", spring_constant))
    }
    spring_constant_vec <- fdObj@metadata[[spring_constant]]
    message(sprintf("Using spring constant column '%s' from metadata.", spring_constant))
  } else {
    stop("spring_constant must be either a single numeric value or a column name (string) in metadata.")
  }
  names(spring_constant_vec) <- rownames(fdObj@metadata)

  # ---- Ensure sensitivity information ----
  sens_col <- paste0("sensitivity_V_nm_", useCurve)
  if (!sens_col %in% names(fdObj@metadata)) {
    message(sprintf("Column '%s' not found in metadata. Running analyze_sensitivity()...", sens_col))
    fdObj <- analyze_sensitivity(
      fdObj = fdObj,
      end = end,
      intv = intv,
      R_squared_min = R_squared_min,
      minimum_length = minimum_length,
      useCurve = useCurve,
      threads = threads
    )
  }

  sensitivity_vec <- fdObj@metadata[[sens_col]]
  names(sensitivity_vec) <- rownames(fdObj@metadata)
  senscal_segment <- fdObj@senscal_segment[[useCurve]]

  # ---- Ensure baseline information ----
  base_col <- paste0("baseline_V_", useCurve)
  if (!base_col %in% names(fdObj@metadata)) {
    message(sprintf("Column '%s' not found in metadata. Running analyze_baseline()...", base_col))
    fdObj <- analyze_baseline(
      fdObj = fdObj,
      least_length = least_length,
      useCurve = useCurve,
      slp_threshold = slp_threshold,
      std_threshold = std_threshold,
      threads = threads,
      soft = soft,
      external_sensitivity_column = if (soft) probe_sens_col else NULL
    )
  }

  baseline_vec <- fdObj@metadata[[base_col]]
  names(baseline_vec) <- rownames(fdObj@metadata)

  # ---- Probe sensitivity vector for soft mode ----
  probe_sensitivity_vec <- if (soft) {
    pv <- fdObj@metadata[[probe_sens_col]]
    names(pv) <- rownames(fdObj@metadata)
    pv
  } else {
    NULL
  }

  # ---- Determine data point count column ----
  n_datapoints_col <- if (useCurve == "approach") "Number_of_datapoints_approach" else "Number_of_datapoints_retract"
  has_datapoint_info <- n_datapoints_col %in% names(fdObj@metadata)

  # ---- Transform a single curve ----
  transform_one <- function(name) {
    df <- raw_list[[name]]
    if (!(x_col %in% colnames(df)) || !(y_col %in% colnames(df))) {
      return(data.frame(separation_distance_nm = numeric(0), force_nN = numeric(0)))
    }

    x <- df[[x_col]]
    y <- df[[y_col]]
    
    # Subset to valid data if metadata is available
    if (has_datapoint_info) {
      n_valid <- fdObj@metadata[name, n_datapoints_col]
      if (!is.na(n_valid) && n_valid > 0 && n_valid <= length(x)) {
        x <- x[seq_len(n_valid)]
        y <- y[seq_len(n_valid)]
      }
    }
    
    baseline <- baseline_vec[name]
    sensitivity <- sensitivity_vec[name]
    spring_const <- spring_constant_vec[name]
    senscal_seg <- senscal_segment[[name]]

    if (is.na(baseline) || is.na(sensitivity) || is.null(senscal_seg) || is.na(spring_const)) {
      return(data.frame(separation_distance_nm = numeric(0), force_nN = numeric(0)))
    } else {
      transform_a_curve(
        x = x,
        y = y,
        baseline = baseline,
        sensitivity = sensitivity,
        spring_constant = spring_const,
        senscal_seg_x = senscal_seg[[1]],
        senscal_seg_y = senscal_seg[[2]],
        soft = soft,
        probe_sensitivity = if (soft) probe_sensitivity_vec[name] else NULL
      )
    }
  }

  # ---- Parallel or sequential execution ----
  if (threads > 1) {
    old_plan <- future::plan()
    on.exit(future::plan(old_plan), add = TRUE)
    future::plan(future::multisession, workers = threads)
    results <- future.apply::future_lapply(curve_names, transform_one)
  } else {
    results <- lapply(curve_names, transform_one)
  }

  names(results) <- curve_names

  n_fail <- sum(sapply(results, nrow) == 0)
  message(sprintf("Processed %d curves (%s); %d failed transformation.", length(curve_names), useCurve, n_fail))

  # ---- Store results in fdObj ----
  if (useCurve == "approach") {
    fdObj@approachCurves <- results
  } else {
    fdObj@retractCurves <- results
  }

  return(fdObj)
}



#' Adhesive Force for a Single Transformed AFM Curve
#'
#' Extracts the adhesion minimum from one transformed force-separation curve by
#' locating the most negative value in \\code{force_nN} and reporting its
#' magnitude together with the corresponding
#' \\code{separation_distance_nm}. The function first validates that the input is
#' a data frame containing the required transformed-curve columns, coerces both
#' columns to numeric, and discards rows with non-finite values before
#' searching for the minimum force.
#'
#' For valid transformed curves, adhesion is defined here as a negative force
#' excursion. If the minimum force is below zero, its sign is flipped so the
#' reported \\code{adhesive_force_nN} is a positive adhesion magnitude. If the
#' curve contains no negative force values, the function records this as "no
#' detectable adhesion" and returns \\code{adhesive_force_nN = 0} with
#' \\code{separation_distance_nm = NA_real_}. If the input cannot be analyzed at
#' all because required columns are missing or no finite x-y pairs remain after
#' cleaning, both returned values are \\code{NA}.
#' @param curve_df A data.frame with columns:
#'   \itemize{
#'     \item{\code{separation_distance_nm}}{Numeric tip-sample separation (nm).}
#'     \item{\code{force_nN}}{Numeric force (nN).}
#'   }
#'
#' @return A named numeric vector of length 2:
#' \describe{
#'   \item{adhesive_force_nN}{The negated most negative force
#'   value in the curve. Returns \\code{0} when the curve is valid but contains
#'   no negative force values, and \\code{NA_real_} when the curve cannot be
#'   analyzed.}
#'   \item{separation_distance_nm}{The separation distance (nm) at which the
#'   adhesive-force minimum occurs. Returns \\code{NA_real_} when no adhesion is
#'   detected or when the input is invalid.}
#' }
#' This makes the output distinguish between a valid "no adhesion" case
#' (\\code{0}/\\code{NA}) and a failed analysis case (\\code{NA}/\\code{NA}).
#'
#' @examples
#' transformed_df <- data.frame(
#'   separation_distance_nm = c(
#'     0.22196440, 0.19896440, 0.17496440, 0.23529773, 0.21129773,
#'     0.18829773, 0.16429773, 0.14129773, 0.11729773, 0.17763107,
#'     0.15463107, 0.13063107, 0.19096440, 0.08363107, -0.02270227,
#'     -0.21336893, -0.31970227, -0.42603560, -0.53336893, -0.47303560,
#'     -0.49703560, -0.43670227, -0.54403560, -0.65036893, -0.67336893,
#'     -0.78070227, -0.72036893, -0.49436893, -0.18403560, 0.04196440,
#'     0.18563107, 0.49496440, 1.13863107, 3.03229773, 5.09163107,
#'     7.15196440, 8.21129773, 9.27163107, 10.58096440, 11.97463107,
#'     13.28396440, 14.42763107, 15.48796440, 16.38063107, 17.44096440,
#'     18.41696440, 19.39396440, 20.28663107, 21.26363107, 22.24063107,
#'     23.13329773, 24.11029773, 25.08629773, 26.06329773, 27.12263107,
#'     28.01629773, 28.99329773, 29.96929773, 30.94629773, 31.92229773,
#'     32.89929773, 33.87529773, 34.85229773, 35.82829773, 36.72196440,
#'     37.69896440, 38.75829773, 39.73529773, 40.79463107, 41.77163107,
#'     42.74763107, 43.72463107, 44.70063107, 45.67763107, 46.57129773,
#'     47.54729773, 48.52429773, 49.58363107, 50.47729773, 51.45329773,
#'     52.43029773, 53.49063107, 54.38329773, 55.44363107, 56.33629773,
#'     57.31329773, 58.28929773, 59.34963107, 60.32663107, 61.21929773,
#'     62.19629773, 63.25563107, 64.23263107, 65.37529773, 66.26896440,
#'     67.24496440, 68.13863107, 69.03229773, 70.00829773, 70.90196440
#'   ),
#'   force_nN = c(
#'     2.588500000, 2.488500000, 2.388500000, 2.296833333, 2.196833333,
#'     2.096833333, 1.996833333, 1.896833333, 1.796833333, 1.705166667,
#'     1.605166667, 1.505166667, 1.413500000, 1.305166667, 1.196833333,
#'     1.080166667, 0.971833333, 0.863500000, 0.755166667, 0.663500000,
#'     0.563500000, 0.471833333, 0.363500000, 0.255166667, 0.155166667,
#'     0.046833333, -0.044833333, -0.119833333, -0.186500000, -0.261500000,
#'     -0.344833333, -0.411500000, -0.444833333, -0.353166667, -0.244833333,
#'     -0.136500000, -0.128166667, -0.119833333, -0.086500000, -0.044833333,
#'     -0.011500000, 0.005166667, 0.013500000, 0.005166667, 0.013500000,
#'     0.013500000, 0.013500000, 0.005166667, 0.005166667, 0.005166667,
#'     -0.003166667, -0.003166667, -0.003166667, -0.003166667, 0.005166667,
#'     -0.003166667, -0.003166667, -0.003166667, -0.003166667, -0.003166667,
#'     -0.003166667, -0.003166667, -0.003166667, -0.003166667, -0.011500000,
#'     -0.011500000, -0.003166667, -0.003166667, 0.005166667, 0.005166667,
#'     0.005166667, 0.005166667, 0.005166667, 0.005166667, -0.003166667,
#'     -0.003166667, -0.003166667, 0.005166667, -0.003166667, -0.003166667,
#'     -0.003166667, 0.005166667, -0.003166667, 0.005166667, -0.003166667,
#'     -0.003166667, -0.003166667, 0.005166667, 0.005166667, -0.003166667,
#'     -0.003166667, 0.005166667, 0.005166667, 0.021833333, 0.013500000,
#'     0.013500000, 0.005166667, -0.003166667, -0.003166667, -0.011500000
#'   )
#' )
#' analyze_a_curve_adhesive_force(transformed_df)
#'
#' @export
analyze_a_curve_adhesive_force <- function(curve_df) {
  # Basic validation
  if (!is.data.frame(curve_df) ||
      !all(c("separation_distance_nm", "force_nN") %in% names(curve_df))) {
    return(c(adhesive_force_nN = NA_real_, separation_distance_nm = NA_real_))
  }

  # Coerce to numeric and drop NAs
  sep <- suppressWarnings(as.numeric(curve_df$separation_distance_nm))
  frc <- suppressWarnings(as.numeric(curve_df$force_nN))
  ok  <- is.finite(sep) & is.finite(frc)

  if (!any(ok)) {
    return(c(adhesive_force_nN = NA_real_, separation_distance_nm = NA_real_))
  }

  sep <- sep[ok]; frc <- frc[ok]

  # Identify the most negative force (adhesion dip)
  idx_min <- which.min(frc)
  min_force <- frc[idx_min]

  # Require negativity to report adhesion (common convention)
  if (!(is.finite(min_force) && min_force < 0)) {
    return(c(adhesive_force_nN = 0, separation_distance_nm = NA_real_))
  }

  c(adhesive_force_nN = unname(min_force)*(-1),
    separation_distance_nm = unname(sep[idx_min]))
}


#' Adhesive Force for All Transformed Curves in an fdObj
#'
#' Applies \code{analyze_a_curve_adhesive_force()} to each transformed curve in an \code{fdObj}
#' and stores results in metadata:
#' \itemize{
#'   \item \code{adhesive_force_nN_<dir>}
#'   \item \code{adhesive_sep_nm_<dir>}
#' }
#'
#' Where \code{<dir>} is "approach" or "retract".
#'
#' Behavior:
#' \itemize{
#'   \item If a curve is present and valid but has no negative force, force = 0 and sep = NA.
#'   \item If a curve cannot be analyzed at all (missing/invalid transformed data), both are NA.
#' }
#'
#' @param fdObj An object of class \code{fdObj}.
#' @param useCurve Character. Either "approach" or "retract" (default "retract").
#' @param threads Integer. Number of parallel workers (default 1).
#'
#' @return The updated \code{fdObj}.
#' @examples
#' folder <- system.file("extdata", package = "curvana")
#' fd_obj <- createFdObjFromFolder(folder)
#' fd_obj <- transform_curves(fd_obj, spring_constant = 0.1, useCurve = "retract", threads = 1, least_length= 300)
#' fd_obj <- analyze_curves_adhesive_force(fd_obj, useCurve = "retract", threads = 1)
#' print(fd_obj@metadata[, c("adhesive_force_nN_retract", "adhesive_sep_nm_retract")])
#' @export
analyze_curves_adhesive_force <- function(fdObj, useCurve = "retract", threads = 1) {
  if (!inherits(fdObj, "fdObj")) stop("fdObj must be of class 'fdObj'")
  if (!useCurve %in% c("approach", "retract")) stop("useCurve must be either 'approach' or 'retract'")

  curve_list <- if (useCurve == "approach") fdObj@approachCurves else fdObj@retractCurves
  dir_tag <- if (useCurve == "approach") "approach" else "retract"

  # If no transformed curves are present, create NA columns and exit
  if (is.null(curve_list) || length(curve_list) == 0) {
    warning("No transformed curves found for the selected direction. Did you run transform_curves()?")

    fdObj@metadata[[paste0("adhesive_force_nN_", dir_tag)]] <- NA_real_
    fdObj@metadata[[paste0("adhesive_sep_nm_",  dir_tag)]] <- NA_real_
    return(fdObj)
  }

  curve_names <- names(curve_list)

  run_one <- function(id) {
    df <- curve_list[[id]]
    # If curve not analyzable at all, return NA/NA to distinguish from "no adhesion" (0/NA)
    if (!is.data.frame(df) ||
        !all(c("separation_distance_nm", "force_nN") %in% names(df)) ||
        nrow(df) == 0) {
      return(c(adhesive_force_nN = NA_real_, separation_distance_nm = NA_real_))
    }
    analyze_a_curve_adhesive_force(df)
  }

  res_list <- if (threads > 1) {
    old_plan <- future::plan()
    on.exit(future::plan(old_plan), add = TRUE)
    future::plan(future::multisession, workers = threads)
    future.apply::future_lapply(curve_names, run_one)
  } else {
    lapply(curve_names, run_one)
  }

  # Bind results and align to metadata order
  res_mat <- do.call(rbind, res_list) %>% as.data.frame(., stringsAsFactors = FALSE)
  rownames(res_mat) <- curve_names

  # Match metadata rows to result rows
  fdObj@metadata[[paste0("adhesive_force_nN_", dir_tag)]] <-
    res_mat$adhesive_force_nN[match(rownames(fdObj@metadata), rownames(res_mat))]

  fdObj@metadata[[paste0("adhesive_sep_nm_", dir_tag)]] <-
    res_mat$separation_distance_nm[match(rownames(fdObj@metadata), rownames(res_mat))]

  # Reporting
  forces <- fdObj@metadata[[paste0("adhesive_force_nN_", dir_tag)]]
  n_total   <- length(curve_list)
  n_na      <- sum(is.na(forces))
  n_zero    <- sum(!is.na(forces) & forces == 0)
  n_detect  <- sum(!is.na(forces) & forces > 0)

  message(sprintf(
    "Adhesive force analyzed for %d curves: %d with adhesion, %d with no adhesion (0), %d not analyzed (NA).",
    n_total, n_detect, n_zero, n_na
  ))

  fdObj
}

#' Determine single-curve noise cutoff from baseline force noise
#'
#' Estimates lower and upper force-noise bands from the baseline region of one
#' transformed AFM force-separation curve. The function uses the last
#' \code{baseline_span} rows of \code{force_nN} as the baseline window.
#'  It then converts that baseline
#' window into a negative and positive threshold pair,
#' \code{noiseBand_low}/\code{noiseBand_high}, that can be used for downstream
#' rupture, repulsion, or area analyses.
#'
#' Four thresholding strategies are supported. \code{"sd"} builds symmetric
#' bands from the baseline standard deviation, \code{"mad"} builds symmetric
#' bands from the median absolute deviation scaled by
#' \code{mad_constant}, \code{"quantile"} uses empirical lower and upper
#' quantiles of the baseline force distribution, and \code{"fixed"} uses
#' user-supplied lower and upper force cutoffs directly. In all cases the final
#' bands are multiplied by \code{multiplier}. For the symmetric methods, if the
#' estimated spread is missing or non-positive, the function substitutes a very
#' small positive value (\code{.Machine$double.eps}) so that downstream methods
#' still receive non-zero thresholds instead of a degenerate zero-width band.
#'
#' Invalid inputs such as a non-data-frame \code{curve_df}, missing required
#' columns, or an invalid \code{baseline_span} do not stop execution; they
#' return \code{NA} noise bands with a warning. By contrast, invalid method-
#' specific arguments such as an out-of-range quantile or missing
#' \code{fixed_low}/\code{fixed_high} for \code{"fixed"} cause an error,
#' because the requested thresholding rule itself cannot be evaluated.
#'
#' @param curve_df data.frame with columns:
#'   - separation_distance_nm (numeric): x values (distance, nm)
#'   - force_nN (numeric): y values (force, nN)
#' @param baseline_span Integer >= 1. Number of last points used as the baseline window.
#' @param threshold_method Character scalar controlling how baseline noise bands are estimated.
#'   One of
#'   \\code{c("sd", "mad", "quantile", "fixed")} (default \\code{"sd"}):
#'   \\itemize{
#'     \\item \\code{"sd"}: symmetric bands from baseline SD, i.e. \\eqn{\\pm\\,sd(y_{base})\\times multiplier}.
#'     \\item \\code{"mad"}: symmetric bands from MAD, i.e. \\eqn{\\pm\\,mad(y_{base})\\times mad\\_constant\\times multiplier}.
#'     \\item \\code{"quantile"}: asymmetric empirical bands using \\code{quantile\\_low} and \\code{quantile\\_high}, each scaled by \\code{multiplier}.
#'     \\item \\code{"fixed"}: user-specified \\code{fixed\\_low} and \\code{fixed\\_high}, each scaled by \\code{multiplier}.
#'   }
#' @param multiplier Numeric >= 0 scaling multiplier for all methods. Default 3.
#' @param mad_constant Scaling for MAD to be SD-equivalent. Default 1.4826.
#' @param quantile_low Lower quantile probability for "quantile" method. Default 0.25.
#' @param quantile_high Upper quantile probability for "quantile" method. Default 0.75.
#' @param fixed_low Numeric lower band (nN) for "fixed" method. Default NULL.
#' @param fixed_high Numeric upper band (nN) for "fixed" method. Default NULL.
#'
#' @return A named numeric vector of length 2:
#' \describe{
#'   \item{noiseBand_low}{Lower force threshold in nN, typically negative for
#'   symmetric methods. Returns \code{NA_real_} when the curve cannot be
#'   analyzed.}
#'   \item{noiseBand_high}{Upper force threshold in nN, typically positive for
#'   symmetric methods. Returns \code{NA_real_} when the curve cannot be
#'   analyzed.}
#' }
#' @examples
#' transformed_df <- data.frame(
#'   separation_distance_nm = c(
#'     0.22196440, 0.19896440, 0.17496440, 0.23529773, 0.21129773,
#'     0.18829773, 0.16429773, 0.14129773, 0.11729773, 0.17763107,
#'     0.15463107, 0.13063107, 0.19096440, 0.08363107, -0.02270227,
#'     -0.21336893, -0.31970227, -0.42603560, -0.53336893, -0.47303560,
#'     -0.49703560, -0.43670227, -0.54403560, -0.65036893, -0.67336893,
#'     -0.78070227, -0.72036893, -0.49436893, -0.18403560, 0.04196440,
#'     0.18563107, 0.49496440, 1.13863107, 3.03229773, 5.09163107,
#'     7.15196440, 8.21129773, 9.27163107, 10.58096440, 11.97463107,
#'     13.28396440, 14.42763107, 15.48796440, 16.38063107, 17.44096440,
#'     18.41696440, 19.39396440, 20.28663107, 21.26363107, 22.24063107,
#'     23.13329773, 24.11029773, 25.08629773, 26.06329773, 27.12263107,
#'     28.01629773, 28.99329773, 29.96929773, 30.94629773, 31.92229773,
#'     32.89929773, 33.87529773, 34.85229773, 35.82829773, 36.72196440,
#'     37.69896440, 38.75829773, 39.73529773, 40.79463107, 41.77163107,
#'     42.74763107, 43.72463107, 44.70063107, 45.67763107, 46.57129773,
#'     47.54729773, 48.52429773, 49.58363107, 50.47729773, 51.45329773,
#'     52.43029773, 53.49063107, 54.38329773, 55.44363107, 56.33629773,
#'     57.31329773, 58.28929773, 59.34963107, 60.32663107, 61.21929773,
#'     62.19629773, 63.25563107, 64.23263107, 65.37529773, 66.26896440,
#'     67.24496440, 68.13863107, 69.03229773, 70.00829773, 70.90196440
#'   ),
#'   force_nN = c(
#'     2.588500000, 2.488500000, 2.388500000, 2.296833333, 2.196833333,
#'     2.096833333, 1.996833333, 1.896833333, 1.796833333, 1.705166667,
#'     1.605166667, 1.505166667, 1.413500000, 1.305166667, 1.196833333,
#'     1.080166667, 0.971833333, 0.863500000, 0.755166667, 0.663500000,
#'     0.563500000, 0.471833333, 0.363500000, 0.255166667, 0.155166667,
#'     0.046833333, -0.044833333, -0.119833333, -0.186500000, -0.261500000,
#'     -0.344833333, -0.411500000, -0.444833333, -0.353166667, -0.244833333,
#'     -0.136500000, -0.128166667, -0.119833333, -0.086500000, -0.044833333,
#'     -0.011500000, 0.005166667, 0.013500000, 0.005166667, 0.013500000,
#'     0.013500000, 0.013500000, 0.005166667, 0.005166667, 0.005166667,
#'     -0.003166667, -0.003166667, -0.003166667, -0.003166667, 0.005166667,
#'     -0.003166667, -0.003166667, -0.003166667, -0.003166667, -0.003166667,
#'     -0.003166667, -0.003166667, -0.003166667, -0.003166667, -0.011500000,
#'     -0.011500000, -0.003166667, -0.003166667, 0.005166667, 0.005166667,
#'     0.005166667, 0.005166667, 0.005166667, 0.005166667, -0.003166667,
#'     -0.003166667, -0.003166667, 0.005166667, -0.003166667, -0.003166667,
#'     -0.003166667, 0.005166667, -0.003166667, 0.005166667, -0.003166667,
#'     -0.003166667, -0.003166667, 0.005166667, 0.005166667, -0.003166667,
#'     -0.003166667, 0.005166667, 0.005166667, 0.021833333, 0.013500000,
#'     0.013500000, 0.005166667, -0.003166667, -0.003166667, -0.011500000
#'   )
#' )
#' analyze_a_curve_noise(transformed_df, baseline_span = 50)
#' @export
analyze_a_curve_noise <- function(
    curve_df,
    baseline_span,
  threshold_method = c("sd","mad","quantile","fixed"),
    multiplier = 3,
    mad_constant = 1.4826,
    quantile_low = 0.25,
    quantile_high = 0.75,
    fixed_low = NULL,
    fixed_high = NULL
) {
  threshold_method <- match.arg(threshold_method)

  if (!is.data.frame(curve_df)) {
    warning("curve_df must be a data.frame. Returning NA as results.")
    return(c(noiseBand_low = NA_real_, noiseBand_high = NA_real_))
  }
  if (!all(c("separation_distance_nm", "force_nN") %in% names(curve_df))) {
    warning("curve_df must contain 'separation_distance_nm' and 'force_nN'. Returning NA as results.")
    return(c(noiseBand_low = NA_real_, noiseBand_high = NA_real_))
  }
  if (!is.numeric(baseline_span) || length(baseline_span) != 1 || baseline_span < 1) {
    warning("baseline_span must be a single integer >= 1. Returning NA as results.")
    return(c(noiseBand_low = NA_real_, noiseBand_high = NA_real_))
  }

  n <- nrow(curve_df)
  baseline_span <- min(as.integer(baseline_span), n)
  b_start <- n - baseline_span + 1L
  y_base  <- curve_df$force_nN[b_start:n]

  guard <- function(val) if (is.na(val) || val <= 0) .Machine$double.eps else val

  if (!is.numeric(multiplier) || length(multiplier) != 1 || is.na(multiplier) || multiplier < 0) {
    stop("multiplier must be a single non-negative numeric value.")
  }

  if (threshold_method == "sd") {
    value <- guard(stats::sd(y_base, na.rm = TRUE)) * multiplier
    noiseBand_low <- -value
    noiseBand_high <- value
  } else if (threshold_method == "mad") {
    md <- stats::mad(y_base, constant = 1, na.rm = TRUE)
    value <- guard(md) * mad_constant * multiplier
    noiseBand_low <- -value
    noiseBand_high <- value
  } else if (threshold_method == "quantile") {
    if (!is.numeric(quantile_low) || length(quantile_low) != 1 || is.na(quantile_low) || quantile_low < 0 || quantile_low > 1) {
      stop("quantile_low must be a single numeric value in [0, 1].")
    }
    if (!is.numeric(quantile_high) || length(quantile_high) != 1 || is.na(quantile_high) || quantile_high < 0 || quantile_high > 1) {
      stop("quantile_high must be a single numeric value in [0, 1].")
    }
    noiseBand_low <- stats::quantile(y_base, probs = quantile_low, na.rm = TRUE, names = FALSE) * multiplier
    noiseBand_high <- stats::quantile(y_base, probs = quantile_high, na.rm = TRUE, names = FALSE) * multiplier
    if (!is.finite(noiseBand_low)) noiseBand_low <- -.Machine$double.eps
    if (!is.finite(noiseBand_high)) noiseBand_high <- .Machine$double.eps
  } else if (threshold_method == "fixed") {
    if (is.null(fixed_low) || !is.numeric(fixed_low) || length(fixed_low) != 1 || is.na(fixed_low)) {
      stop("For threshold_method = 'fixed', provide numeric fixed_low (nN).")
    }
    if (is.null(fixed_high) || !is.numeric(fixed_high) || length(fixed_high) != 1 || is.na(fixed_high)) {
      stop("For threshold_method = 'fixed', provide numeric fixed_high (nN).")
    }
    noiseBand_low <- fixed_low * multiplier
    noiseBand_high <- fixed_high * multiplier
    if (!is.finite(noiseBand_low)) noiseBand_low <- -.Machine$double.eps
    if (!is.finite(noiseBand_high)) noiseBand_high <- .Machine$double.eps
  } else {
    stop("Unknown threshold_method.")
  }

  c(noiseBand_low = noiseBand_low, noiseBand_high = noiseBand_high)
}

#' Noise-band Analysis (nN) for All Transformed Curves in an fdObj
#'
#' Applies \code{analyze_a_curve_noise()} to each transformed curve in an
#' \code{fdObj} and stores per-curve lower/upper noise bands in metadata:
#' \itemize{
#'   \item \code{noiseBand_low_nN_<dir>}
#'   \item \code{noiseBand_high_nN_<dir>}
#' }
#' where \code{<dir>} is \code{"approach"} or \code{"retract"}.
#'
#' @param fdObj An object of class \code{fdObj}.
#' @param useCurve Character; must be one of \code{c("retract", "approach")}.
#' @param threads Integer. Number of parallel workers (default \code{1}).
#' @param baseline_span Either a single integer >= 1, or the string \code{"automatic"}.
#'   When \code{"automatic"}, per-curve baseline spans are read from
#'   \code{fdObj@metadata$baseline_span_<useCurve>}.
#' @param threshold_method Character scalar controlling how baseline noise bands are estimated.
#'   One of \code{c("sd", "mad", "quantile", "fixed")} (default \code{"sd"}).
#' @param multiplier Numeric >= 0 scaling multiplier for all methods. Default 3.
#' @param mad_constant Scaling for MAD to be SD-equivalent. Default 1.4826.
#' @param quantile_low Lower quantile probability for \code{"quantile"} method. Default 0.25.
#' @param quantile_high Upper quantile probability for \code{"quantile"} method. Default 0.75.
#' @param fixed_low Numeric lower band (nN) for \code{"fixed"} method. Default NULL.
#' @param fixed_high Numeric upper band (nN) for \code{"fixed"} method. Default NULL.
#'
#' @return The updated \code{fdObj} with two new metadata columns:
#' \code{noiseBand_low_nN_<dir>} and \code{noiseBand_high_nN_<dir>}.
#' @seealso analyze_a_curve_noise
#' @examples
#' folder <- system.file("extdata", package = "curvana")
#' fd_obj <- createFdObjFromFolder(folder)
#' fd_obj <- transform_curves(fd_obj, spring_constant = 0.1, useCurve = "retract", threads = 1, least_length= 300)
#' fd_obj <- analyze_curves_noise(fd_obj, useCurve = "retract", threads = 1)
#' @export
analyze_curves_noise <- function(
    fdObj,
    useCurve = c("retract", "approach"),
    threads = 1,
    baseline_span = "automatic",
    threshold_method = c("sd", "mad", "quantile", "fixed"),
    multiplier = 3,
    mad_constant = 1.4826,
    quantile_low = 0.25,
    quantile_high = 0.75,
    fixed_low = NULL,
    fixed_high = NULL
) {
  if (!inherits(fdObj, "fdObj")) {
    stop("fdObj must be of class 'fdObj'")
  }

  useCurve <- match.arg(useCurve)
  threshold_method <- match.arg(threshold_method)
  curve_list <- if (useCurve == "approach") fdObj@approachCurves else fdObj@retractCurves
  dir_tag <- useCurve

  if (is.character(baseline_span) && baseline_span == "automatic") {
    meta_col <- paste0("baseline_span_", dir_tag)
    if (!meta_col %in% names(fdObj@metadata)) {
      stop(sprintf(
        "baseline_span is set to 'automatic', but column '%s' is missing in fdObj@metadata.",
        meta_col
      ))
    }
    use_baseline_span <- "from_metadata"
  } else if (is.numeric(baseline_span) && length(baseline_span) == 1 && baseline_span >= 1) {
    use_baseline_span <- "fixed"
  } else {
    stop("baseline_span must be either 'automatic' or a single integer >= 1.")
  }

  if (is.null(curve_list) || length(curve_list) == 0) {
    warning("No transformed curves found for the selected direction. Did you run transform_curves()?")
    fdObj@metadata[[paste0("noiseBand_low_nN_", dir_tag)]] <- NA_real_
    fdObj@metadata[[paste0("noiseBand_high_nN_", dir_tag)]] <- NA_real_
    return(fdObj)
  }

  curve_names <- names(curve_list)

  run_one <- function(id) {
    df <- curve_list[[id]]
    if (!is.data.frame(df) ||
        !all(c("separation_distance_nm", "force_nN") %in% names(df)) ||
        nrow(df) == 0) {
      return(c(noiseBand_low = NA_real_, noiseBand_high = NA_real_))
    }

    bs_value <- if (use_baseline_span == "from_metadata") {
      bs <- fdObj@metadata[[paste0("baseline_span_", dir_tag)]][
        match(id, rownames(fdObj@metadata))
      ]
      if (is.na(bs) || !is.numeric(bs) || bs < 1) {
        return(c(noiseBand_low = NA_real_, noiseBand_high = NA_real_))
      }
      bs
    } else {
      baseline_span
    }

    analyze_a_curve_noise(
      curve_df = df,
      baseline_span = bs_value,
      threshold_method = threshold_method,
      multiplier = multiplier,
      mad_constant = mad_constant,
      quantile_low = quantile_low,
      quantile_high = quantile_high,
      fixed_low = fixed_low,
      fixed_high = fixed_high
    )
  }

  res_list <- if (threads > 1) {
    old_plan <- future::plan()
    on.exit(future::plan(old_plan), add = TRUE)
    future::plan(future::multisession, workers = threads)
    future.apply::future_lapply(curve_names, run_one)
  } else {
    lapply(curve_names, run_one)
  }

  res_mat <- do.call(rbind, res_list)
  res_df <- as.data.frame(res_mat, stringsAsFactors = FALSE)
  rownames(res_df) <- curve_names

  fdObj@metadata[[paste0("noiseBand_low_nN_", dir_tag)]] <-
    res_df$noiseBand_low[match(rownames(fdObj@metadata), rownames(res_df))]
  fdObj@metadata[[paste0("noiseBand_high_nN_", dir_tag)]] <-
    res_df$noiseBand_high[match(rownames(fdObj@metadata), rownames(res_df))]

  n_total <- length(curve_list)
  n_na <- sum(is.na(res_df$noiseBand_low) | is.na(res_df$noiseBand_high))
  n_valid <- n_total - n_na

  message(sprintf(
    "Noise bands analyzed for %d curves (%s): %d valid results.",
    n_total, dir_tag, n_valid
  ))

  fdObj
}

#' Analyze interaction distance from a single AFM curve using noise-band thresholds
#'
#' @description
#' Detects a single interaction distance from one transformed AFM curve by
#' searching for the first sustained excursion of \code{force_nN} outside a
#' user-supplied noise band. The function never scans the final
#' \code{baseline_span} points, treating that tail region as baseline-only, and
#' instead searches only the portion of the curve before that baseline window.
#' This lets the same function be used for either rupture-type events
#' (negative-force excursions) or repulsive-contact events (positive-force
#' excursions) once appropriate threshold values have already been estimated.
#'
#' Scan direction is controlled by \code{x_direction}. With
#' \code{x_direction = "left"}, the search proceeds right-to-left, starting just
#' before the excluded baseline region and moving toward the origin. With
#' \code{x_direction = "right"}, the search proceeds left-to-right, starting at
#' the origin-side of the curve and stopping just before the baseline region.
#' After thresholding the scanned force values, the function uses run-length
#' encoding to identify contiguous stretches of points that satisfy the event
#' condition and accepts only runs of length at least
#' \code{min_consecutive}.
#'
#' The reported interaction distance depends on the event type. For
#' \code{y_direction = "negative"}, the function looks for the first accepted
#' run where \code{force_nN < noiseBand_low} and reports the first point of that
#' run, corresponding to entry into the adhesive or rupture region. For
#' \code{y_direction = "positive"}, it looks for the first accepted run where
#' \code{force_nN > noiseBand_high} and reports the last point of that run,
#' corresponding to exit from the repulsive excursion. If no qualifying run is
#' found, or if there is no scannable region before baseline, the function
#' returns \code{NA} for distance and echoes back the threshold used.
#'
#' @param curve_df data.frame with columns:
#'   - separation_distance_nm (numeric): x values (distance, nm)
#'   - force_nN (numeric): y values (force, nN)
#' @param baseline_span Integer >= 1. Number of last points used as the baseline window. The baseline region will not be scanned.
#' @param y_direction "negative" or "positive".
#'   - "negative": find first y < threshold (rupture-like; threshold is typically negative)
#'   - "positive": find the last point of the first run of y > threshold
#'     (repulsion-like; threshold is typically positive)
#' @param x_direction "left" or "right".
#'   - "left": scan from right to left (from just before baseline toward the origin)
#'   - "right": scan from left to right (from origin up to just before baseline)
#' @param min_consecutive Integer >= 1. Minimum number of consecutive points that
#'   must satisfy the threshold criterion for an interaction event to be called.
#' @param noiseBand_low Numeric scalar threshold for negative-direction detection.
#'   Required when \code{y_direction = "negative"}.
#' @param noiseBand_high Numeric scalar threshold for positive-direction detection.
#'   Required when \code{y_direction = "positive"}.
#'
#' @return A named numeric vector of length 2:
#' \describe{
#'   \item{distance}{Interaction distance in nm for the detected event. Returns
#'   \code{NA_real_} when no qualifying excursion is found or when the input is
#'   invalid. Negative distances are clamped to \code{0}.}
#'   \item{threshold}{The numeric threshold actually used for detection:
#'   \code{noiseBand_low} for negative-direction searches or
#'   \code{noiseBand_high} for positive-direction searches. Returns
#'   \code{NA_real_} only when validation fails before threshold selection.}
#' }
#' @examples
#' transformed_df <- data.frame(
#'   separation_distance_nm = c(
#'     0.22196440, 0.19896440, 0.17496440, 0.23529773, 0.21129773,
#'     0.18829773, 0.16429773, 0.14129773, 0.11729773, 0.17763107,
#'     0.15463107, 0.13063107, 0.19096440, 0.08363107, -0.02270227,
#'     -0.21336893, -0.31970227, -0.42603560, -0.53336893, -0.47303560,
#'     -0.49703560, -0.43670227, -0.54403560, -0.65036893, -0.67336893,
#'     -0.78070227, -0.72036893, -0.49436893, -0.18403560, 0.04196440,
#'     0.18563107, 0.49496440, 1.13863107, 3.03229773, 5.09163107,
#'     7.15196440, 8.21129773, 9.27163107, 10.58096440, 11.97463107,
#'     13.28396440, 14.42763107, 15.48796440, 16.38063107, 17.44096440,
#'     18.41696440, 19.39396440, 20.28663107, 21.26363107, 22.24063107,
#'     23.13329773, 24.11029773, 25.08629773, 26.06329773, 27.12263107,
#'     28.01629773, 28.99329773, 29.96929773, 30.94629773, 31.92229773,
#'     32.89929773, 33.87529773, 34.85229773, 35.82829773, 36.72196440,
#'     37.69896440, 38.75829773, 39.73529773, 40.79463107, 41.77163107,
#'     42.74763107, 43.72463107, 44.70063107, 45.67763107, 46.57129773,
#'     47.54729773, 48.52429773, 49.58363107, 50.47729773, 51.45329773,
#'     52.43029773, 53.49063107, 54.38329773, 55.44363107, 56.33629773,
#'     57.31329773, 58.28929773, 59.34963107, 60.32663107, 61.21929773,
#'     62.19629773, 63.25563107, 64.23263107, 65.37529773, 66.26896440,
#'     67.24496440, 68.13863107, 69.03229773, 70.00829773, 70.90196440
#'   ),
#'   force_nN = c(
#'     2.588500000, 2.488500000, 2.388500000, 2.296833333, 2.196833333,
#'     2.096833333, 1.996833333, 1.896833333, 1.796833333, 1.705166667,
#'     1.605166667, 1.505166667, 1.413500000, 1.305166667, 1.196833333,
#'     1.080166667, 0.971833333, 0.863500000, 0.755166667, 0.663500000,
#'     0.563500000, 0.471833333, 0.363500000, 0.255166667, 0.155166667,
#'     0.046833333, -0.044833333, -0.119833333, -0.186500000, -0.261500000,
#'     -0.344833333, -0.411500000, -0.444833333, -0.353166667, -0.244833333,
#'     -0.136500000, -0.128166667, -0.119833333, -0.086500000, -0.044833333,
#'     -0.011500000, 0.005166667, 0.013500000, 0.005166667, 0.013500000,
#'     0.013500000, 0.013500000, 0.005166667, 0.005166667, 0.005166667,
#'     -0.003166667, -0.003166667, -0.003166667, -0.003166667, 0.005166667,
#'     -0.003166667, -0.003166667, -0.003166667, -0.003166667, -0.003166667,
#'     -0.003166667, -0.003166667, -0.003166667, -0.003166667, -0.011500000,
#'     -0.011500000, -0.003166667, -0.003166667, 0.005166667, 0.005166667,
#'     0.005166667, 0.005166667, 0.005166667, 0.005166667, -0.003166667,
#'     -0.003166667, -0.003166667, 0.005166667, -0.003166667, -0.003166667,
#'     -0.003166667, 0.005166667, -0.003166667, 0.005166667, -0.003166667,
#'     -0.003166667, -0.003166667, 0.005166667, 0.005166667, -0.003166667,
#'     -0.003166667, 0.005166667, 0.005166667, 0.021833333, 0.013500000,
#'     0.013500000, 0.005166667, -0.003166667, -0.003166667, -0.011500000
#'   )
#' )
#' rupdis = analyze_a_curve_interaction_distance(transformed_df,
#'                                                       y_direction = "negative", 
#'                                                       x_direction = "left",
#'                                                       baseline_span = 50,
#'                                                       noiseBand_low = -0.1,
#'                                                       noiseBand_high = 0.1,
#'                                                       min_consecutive = 3
#'                                                       )
#' plot(transformed_df$separation_distance_nm, transformed_df$force_nN, type = "l")
#' abline(v = rupdis['distance'], col= "red", lwd = 2)
#'
#' repdis = analyze_a_curve_interaction_distance(transformed_df,
#'                                                       y_direction = "positive",
#'                                                       x_direction = "right",
#'                                                       baseline_span = 50,
#'                                                       noiseBand_low = -0.1,
#'                                                       noiseBand_high = 0.1,
#'                                                       min_consecutive = 3
#'                                                       )
#' plot(transformed_df$separation_distance_nm, transformed_df$force_nN, type = "l")
#' abline(v = repdis['distance'], col= "blue", lwd = 2)
#' @export
analyze_a_curve_interaction_distance <- function(
    curve_df,
    baseline_span,
    y_direction = c("negative", "positive"),
    x_direction = c("left", "right"),
  min_consecutive = 1,
    noiseBand_low = NULL,
    noiseBand_high = NULL
) {
  y_direction <- match.arg(y_direction)
  x_direction <- match.arg(x_direction)

  # ---- validation ----
  if (!is.data.frame(curve_df)) {
    warning("curve_df must be a data.frame. Returning NA as results.")
    return(c(distance = NA_real_, threshold = NA_real_))
  }
  if (!all(c("separation_distance_nm", "force_nN") %in% names(curve_df))) {
    warning("curve_df must contain 'separation_distance_nm' and 'force_nN'. Returning NA as results.")
    return(c(distance = NA_real_, threshold = NA_real_))
  }
  if (!is.numeric(baseline_span) || length(baseline_span) != 1 || baseline_span < 1) {
    warning("baseline_span must be a single integer >= 1. Returning NA as results.")
    return(c(distance = NA_real_, threshold = NA_real_))
  }
  if (!is.numeric(min_consecutive) || length(min_consecutive) != 1 || !is.finite(min_consecutive) || min_consecutive < 1) {
    warning("min_consecutive must be a single integer >= 1. Returning NA as results.")
    return(c(distance = NA_real_, threshold = NA_real_))
  }

  n <- nrow(curve_df)
  baseline_span <- min(as.integer(baseline_span), n)
  min_consecutive <- as.integer(min_consecutive)

  x <- curve_df$separation_distance_nm
  y <- curve_df$force_nN
  b_start <- n - baseline_span

  threshold <- if (y_direction == "negative") {
    if (is.null(noiseBand_low) || !is.numeric(noiseBand_low) || length(noiseBand_low) != 1 || is.na(noiseBand_low)) {
      stop("For y_direction = 'negative', provide noiseBand_low as a single numeric value.")
    }
    as.numeric(noiseBand_low)
  } else {
    if (is.null(noiseBand_high) || !is.numeric(noiseBand_high) || length(noiseBand_high) != 1 || is.na(noiseBand_high)) {
      stop("For y_direction = 'positive', provide noiseBand_high as a single numeric value.")
    }
    as.numeric(noiseBand_high)
  }

  # ---- choose scan indices based on x_direction ----
  if (x_direction == "left") {
    # right -> left (from just before baseline toward origin)
    scan_idx <- seq.int(from = b_start, to = 1L, by = -1L)
  } else {
    # left -> right (from origin up to just before baseline)
    scan_idx <- seq.int(from = 1L, to = max(b_start, 1L), by = 1L)
    if (b_start <= 1L) scan_idx <- integer(0)  # no room before baseline
  }

  if (length(scan_idx) == 0L) {
    return(c(distance = NA_real_, threshold = threshold))
  }

  # ---- apply y-direction rule ----
  y_scan <- y[scan_idx]
  hit_mask <- if (y_direction == "negative") {
    y_scan < threshold
  } else {
    y_scan > threshold
  }

  runs <- rle(hit_mask)
  run_ends <- cumsum(runs$lengths)
  run_starts <- run_ends - runs$lengths + 1L
  qualifying_runs <- which(runs$values & runs$lengths >= min_consecutive)

  if (length(qualifying_runs) == 0L) {
    return(c(distance = NA_real_, threshold = threshold))
  }

  run_id <- qualifying_runs[1L]
  hit <- if (y_direction == "negative") run_starts[run_id] else run_ends[run_id]

  c(distance = max(0, x[scan_idx[hit]]), threshold = threshold)
}

#' Interaction Distance (nm) for All Transformed Curves in an fdObj
#'
#' Applies \code{analyze_a_curve_interaction_distance()} across all transformed
#' curves stored in an \code{fdObj} and writes the detected interaction
#' distances and thresholds back into the metadata. Depending on
#' \code{y_direction}, the function measures either rupture-like interaction
#' distances from negative force excursions or repulsive interaction distances
#' from positive force excursions. The selected transformed-curve branch is
#' controlled by \code{useCurve}, so the analysis operates on either
#' \code{fdObj@approachCurves} or \code{fdObj@retractCurves}.
#'
#' This wrapper does not estimate thresholds itself. Instead, it expects the
#' relevant per-curve noise-band metadata column to already exist and then uses
#' those stored values as direct inputs to
#' \code{analyze_a_curve_interaction_distance()}. For negative-direction
#' searches it reads \code{noiseBand_low_nN_<useCurve>}; for positive-direction
#' searches it reads \code{noiseBand_high_nN_<useCurve>}. Baseline spans can be
#' supplied as one fixed value for all curves or read per curve from
#' \code{baseline_span_<useCurve>} when \code{baseline_span = "automatic"}.
#'
#' Curves are processed independently. For each curve, the function verifies
#' that transformed force-separation data are present, resolves the appropriate
#' baseline span and threshold value, and then forwards the curve together with
#' \code{x_direction}, \code{y_direction}, and \code{min_consecutive} to the
#' single-curve detector. Curves with missing transformed data, missing
#' threshold metadata, or invalid per-curve baseline spans are not dropped but
#' recorded as \code{NA} results. After processing, the per-curve outputs are
#' aligned to metadata row order and stored in direction-specific
#' columns.
#'
#' @param fdObj An object of class \code{fdObj}.
#' @param useCurve Character; must be one of \code{c("retract", "approach")}.
#' @param threads Integer. Number of parallel workers (default \code{1}).
#' @param baseline_span Either a single integer >= 1, or the string "automatic".
#'   When "automatic", per-curve baseline spans are read from
#'   \code{fdObj@metadata$baseline_span_<useCurve>}.
#' @param y_direction Detection mode:
#'   \itemize{
#'     \item \code{"negative"}: detects first point with \code{force_nN < noiseBand_low}
#'       (curve enters adhesive region; threshold is typically negative).
#'     \item \code{"positive"}: detects last point with \code{force_nN > noiseBand_high}
#'       (curve exits repulsive excursion; threshold is typically positive).
#'   }
#' @param x_direction Scan direction for each curve:
#'   \itemize{
#'     \item \code{"left"}: right-to-left, from just before baseline toward origin.
#'     \item \code{"right"}: left-to-right, from origin toward just before baseline.
#'   }
#' @param min_consecutive Integer >= 1. Minimum number of consecutive points that
#'   must satisfy the excursion criterion to call an interaction event.
#'
#' @details
#' This function assumes precomputed noise-band columns already exist in
#' \code{fdObj@metadata} and uses them directly:
#' \itemize{
#'   \item if \code{y_direction = "negative"}: \code{noiseBand_low_nN_<useCurve>}
#'   \item if \code{y_direction = "positive"}: \code{noiseBand_high_nN_<useCurve>}
#' }
#' Example for \code{useCurve = "retract"}:
#' \code{noiseBand_low_nN_retract} or \code{noiseBand_high_nN_retract}.
#'
#' @return The updated \code{fdObj} with two metadata columns added or updated:
#' \code{<type>_distance_nm_<dir>} and \code{<type>_threshold_nN_<dir>},
#' where \code{<type>} is \code{"repulsive"} or \code{"rupture"}, depending on
#' \code{y_direction}, and \code{<dir>} is \code{"approach"} or
#' \code{"retract"}. Curves that cannot be analyzed are retained with
#' \code{NA} values in those output columns.
#' @seealso analyze_a_curve_interaction_distance
#' @examples
#' folder <- system.file("extdata", package = "curvana")
#' fd_obj <- createFdObjFromFolder(folder)
#' fd_obj <- transform_curves(fd_obj, spring_constant = 0.1, useCurve = "retract", threads = 1, least_length= 300)
#' fd_obj <- analyze_curves_noise(fd_obj, useCurve = "retract", threads = 1)
#' fd_obj <- analyze_curves_interaction_distance(fd_obj, useCurve = "retract", baseline_span = 'automatic', x_direction = "left")
#'
#' @export
analyze_curves_interaction_distance <- function(
    fdObj,
    useCurve = c("retract", "approach"),
    threads = 1,
    baseline_span,
    y_direction = c("negative", "positive"),
  x_direction = c("left", "right"),
  min_consecutive = 1
) {
  # ---- Validation ----
  if (!inherits(fdObj, "fdObj"))
    stop("fdObj must be of class 'fdObj'")

  useCurve <- match.arg(useCurve)
  y_direction <- match.arg(y_direction)
  x_direction <- match.arg(x_direction)
  if (!is.numeric(min_consecutive) || length(min_consecutive) != 1 || !is.finite(min_consecutive) || min_consecutive < 1) {
    stop("min_consecutive must be a single integer >= 1.")
  }
  min_consecutive <- as.integer(min_consecutive)

  curve_list <- if (useCurve == "approach") fdObj@approachCurves else fdObj@retractCurves
  dir_tag <- useCurve

  noise_col <- if (y_direction == "negative") {
    paste0("noiseBand_low_nN_", dir_tag)
  } else {
    paste0("noiseBand_high_nN_", dir_tag)
  }

  if (!noise_col %in% names(fdObj@metadata)) {
    stop(sprintf("Required metadata column '%s' is missing.", noise_col))
  }

  # ---- baseline_span handling ----
  if (is.character(baseline_span) && baseline_span == "automatic") {
    meta_col <- paste0("baseline_span_", dir_tag)
    if (!meta_col %in% names(fdObj@metadata)) {
      stop(sprintf(
        "baseline_span is set to 'automatic', but column '%s' is missing in fdObj@metadata.",
        meta_col
      ))
    }
    use_baseline_span <- "from_metadata"
  } else if (is.numeric(baseline_span) && length(baseline_span) == 1 && baseline_span >= 1) {
    use_baseline_span <- "fixed"
  } else {
    stop("baseline_span must be either 'automatic' or a single integer >= 1.")
  }

  # ---- Handle missing curves ----
  if (is.null(curve_list) || length(curve_list) == 0) {
    warning("No transformed curves found for the selected direction. Did you run transform_curves()?")

    prefix <- if (y_direction == "positive") "repulsive" else "rupture"
    fdObj@metadata[[paste0(prefix, "_distance_nm_", dir_tag)]]  <- NA_real_
    fdObj@metadata[[paste0(prefix, "_threshold_nN_", dir_tag)]] <- NA_real_
    return(fdObj)
  }

  curve_names <- names(curve_list)
  prefix <- if (y_direction == "positive") "repulsive" else "rupture"

  # ---- Inner function for one curve ----
  run_one <- function(id) {
    df <- curve_list[[id]]
    if (!is.data.frame(df) ||
        !all(c("separation_distance_nm", "force_nN") %in% names(df)) ||
        nrow(df) == 0) {
      return(c(distance = NA_real_, threshold = NA_real_))
    }

    # Determine baseline_span for this curve
    bs_value <- if (use_baseline_span == "from_metadata") {
      bs <- fdObj@metadata[[paste0("baseline_span_", dir_tag)]][
        match(id, rownames(fdObj@metadata))
      ]
      if (is.na(bs) || !is.numeric(bs) || bs < 1)
        return(c(distance = NA_real_, threshold = NA_real_))
      bs
    } else {
      baseline_span
    }

    curve_threshold <- fdObj@metadata[[noise_col]][match(id, rownames(fdObj@metadata))]
    if (is.na(curve_threshold) || !is.numeric(curve_threshold) || length(curve_threshold) != 1) {
      return(c(distance = NA_real_, threshold = NA_real_))
    }

    if (y_direction == "negative") {
      analyze_a_curve_interaction_distance(
        curve_df = df,
        baseline_span = bs_value,
        y_direction = y_direction,
        x_direction = x_direction,
        min_consecutive = min_consecutive,
        noiseBand_low = as.numeric(curve_threshold),
        noiseBand_high = NULL
      )
    } else {
      analyze_a_curve_interaction_distance(
        curve_df = df,
        baseline_span = bs_value,
        y_direction = y_direction,
        x_direction = x_direction,
        min_consecutive = min_consecutive,
        noiseBand_low = NULL,
        noiseBand_high = as.numeric(curve_threshold)
      )
    }
  }

  # ---- Parallel or sequential execution ----
  res_list <- if (threads > 1) {
    old_plan <- future::plan()
    on.exit(future::plan(old_plan), add = TRUE)
    future::plan(future::multisession, workers = threads)
    future.apply::future_lapply(curve_names, run_one)
  } else {
    lapply(curve_names, run_one)
  }

  # ---- Combine and align with metadata ----
  res_mat <- do.call(rbind, res_list)
  res_df  <- as.data.frame(res_mat, stringsAsFactors = FALSE)
  rownames(res_df) <- curve_names

  fdObj@metadata[[paste0(prefix, "_distance_nm_", dir_tag)]] <-
    res_df$distance[match(rownames(fdObj@metadata), rownames(res_df))]
  fdObj@metadata[[paste0(prefix, "_threshold_nN_", dir_tag)]] <-
    res_df$threshold[match(rownames(fdObj@metadata), rownames(res_df))]

  # ---- Summary ----
  n_fdObj  <- nrow(fdObj@metadata)
  n_total  <- length(curve_list)
  n_na     <- sum(is.na(res_df$distance))
  n_valid  <- n_total - n_na

  message(sprintf(
    "Interaction distance analyzed for %d/%d curves (%s, %s): %d valid results.",
    n_total, n_fdObj, dir_tag, prefix, n_valid
  ))

  fdObj
}

#' Analyze the adhesive and repulsive areas of a single AFM curve relative to noise-band thresholds
#'
#' Calculates the total area of a transformed force-separation curve that lies
#' outside a user-defined noise band. Positive force contributions above
#' \code{noiseBand_high} are reported as \emph{repulsive_area}, whereas
#' negative force contributions below \code{noiseBand_low} are reported as
#' \emph{adhesive_area}. The function expects one curve in tabular form with
#' \code{separation_distance_nm} as the x-axis and \code{force_nN} as the
#' y-axis, and it returns the two areas separately rather than combining them
#' into a signed net integral.
#'
#' Integration is performed in the original row order, so the function respects
#' the incoming trace geometry and does not sort or otherwise reorder points.
#' For each adjacent pair of points, the function checks
#' whether the segment lies fully above the upper threshold, fully below the
#' lower threshold, entirely inside the noise band, or crosses one or both
#' thresholds. Fully above-threshold and fully below-threshold segments are
#' integrated as trapezoids after subtracting the relevant threshold, while
#' partial excursions are split at linearly interpolated crossing points and the
#' out-of-band portion is integrated as one or two triangles. Signal within the
#' interval \code{[noiseBand_low, noiseBand_high]} is treated as baseline-like
#' noise and contributes zero area.
#'
#' Before integration, both required columns are coerced to numeric and only
#' finite x-y pairs are retained. If the input is not a data frame with the
#' required columns, or if no finite pairs remain after cleaning, the function
#' returns \code{NA} for both outputs to indicate that the curve could not be
#' analyzed. If fewer than two valid points remain, the function returns
#' zero area for both outputs because no segment exists to integrate.
#'
#' As a practical correction for small transformation offsets near contact, any
#' negative \code{separation_distance_nm} values are clamped to 0 before
#' integration. This avoids assigning artificial area to small negative-distance
#' tails.
#'
#' @param curve_df A data frame with at least two columns:
#'   \describe{
#'     \item{\code{separation_distance_nm}}{Numeric x-coordinates (nm). Values < 0 are clamped to 0.}
#'     \item{\code{force_nN}}{Numeric y-coordinates (nN).}
#'   }
#' @param noiseBand_low Numeric lower noise-band threshold
#'   (default: \code{-.Machine$double.eps}).
#'   Adhesive area is counted where \code{force_nN < noiseBand_low}.
#' @param noiseBand_high Numeric upper noise-band threshold
#'   (default: \code{.Machine$double.eps}).
#'   Repulsive area is counted where \code{force_nN > noiseBand_high}.
#'
#' @return A named numeric vector of length 2:
#' \describe{
#'   \item{adhesive_area}{Total positive area where \code{y < noiseBand_low}.}
#'   \item{repulsive_area}{Total positive area where \code{y > noiseBand_high}.}
#' }
#'
#' @examples
#' transformed_df <- data.frame(
#'   separation_distance_nm = c(
#'     0.22196440, 0.19896440, 0.17496440, 0.23529773, 0.21129773,
#'     0.18829773, 0.16429773, 0.14129773, 0.11729773, 0.17763107,
#'     0.15463107, 0.13063107, 0.19096440, 0.08363107, -0.02270227,
#'     -0.21336893, -0.31970227, -0.42603560, -0.53336893, -0.47303560,
#'     -0.49703560, -0.43670227, -0.54403560, -0.65036893, -0.67336893,
#'     -0.78070227, -0.72036893, -0.49436893, -0.18403560, 0.04196440,
#'     0.18563107, 0.49496440, 1.13863107, 3.03229773, 5.09163107,
#'     7.15196440, 8.21129773, 9.27163107, 10.58096440, 11.97463107,
#'     13.28396440, 14.42763107, 15.48796440, 16.38063107, 17.44096440,
#'     18.41696440, 19.39396440, 20.28663107, 21.26363107, 22.24063107,
#'     23.13329773, 24.11029773, 25.08629773, 26.06329773, 27.12263107,
#'     28.01629773, 28.99329773, 29.96929773, 30.94629773, 31.92229773,
#'     32.89929773, 33.87529773, 34.85229773, 35.82829773, 36.72196440,
#'     37.69896440, 38.75829773, 39.73529773, 40.79463107, 41.77163107,
#'     42.74763107, 43.72463107, 44.70063107, 45.67763107, 46.57129773,
#'     47.54729773, 48.52429773, 49.58363107, 50.47729773, 51.45329773,
#'     52.43029773, 53.49063107, 54.38329773, 55.44363107, 56.33629773,
#'     57.31329773, 58.28929773, 59.34963107, 60.32663107, 61.21929773,
#'     62.19629773, 63.25563107, 64.23263107, 65.37529773, 66.26896440,
#'     67.24496440, 68.13863107, 69.03229773, 70.00829773, 70.90196440
#'   ),
#'   force_nN = c(
#'     2.588500000, 2.488500000, 2.388500000, 2.296833333, 2.196833333,
#'     2.096833333, 1.996833333, 1.896833333, 1.796833333, 1.705166667,
#'     1.605166667, 1.505166667, 1.413500000, 1.305166667, 1.196833333,
#'     1.080166667, 0.971833333, 0.863500000, 0.755166667, 0.663500000,
#'     0.563500000, 0.471833333, 0.363500000, 0.255166667, 0.155166667,
#'     0.046833333, -0.044833333, -0.119833333, -0.186500000, -0.261500000,
#'     -0.344833333, -0.411500000, -0.444833333, -0.353166667, -0.244833333,
#'     -0.136500000, -0.128166667, -0.119833333, -0.086500000, -0.044833333,
#'     -0.011500000, 0.005166667, 0.013500000, 0.005166667, 0.013500000,
#'     0.013500000, 0.013500000, 0.005166667, 0.005166667, 0.005166667,
#'     -0.003166667, -0.003166667, -0.003166667, -0.003166667, 0.005166667,
#'     -0.003166667, -0.003166667, -0.003166667, -0.003166667, -0.003166667,
#'     -0.003166667, -0.003166667, -0.003166667, -0.003166667, -0.011500000,
#'     -0.011500000, -0.003166667, -0.003166667, 0.005166667, 0.005166667,
#'     0.005166667, 0.005166667, 0.005166667, 0.005166667, -0.003166667,
#'     -0.003166667, -0.003166667, 0.005166667, -0.003166667, -0.003166667,
#'     -0.003166667, 0.005166667, -0.003166667, 0.005166667, -0.003166667,
#'     -0.003166667, -0.003166667, 0.005166667, 0.005166667, -0.003166667,
#'     -0.003166667, 0.005166667, 0.005166667, 0.021833333, 0.013500000,
#'     0.013500000, 0.005166667, -0.003166667, -0.003166667, -0.011500000
#'   )
#' )
#' analyze_a_curve_area(transformed_df, noiseBand_low = -0.1, noiseBand_high = 0.1)
#' @seealso area_trapezoid, area_triangle, crossing_x0
#' @export
analyze_a_curve_area <- function(curve_df, noiseBand_low = -.Machine$double.eps, noiseBand_high = .Machine$double.eps) {
  if (!is.numeric(noiseBand_low) || length(noiseBand_low) != 1 || is.na(noiseBand_low)) {
    stop("noiseBand_low must be a single numeric value.")
  }
  if (!is.numeric(noiseBand_high) || length(noiseBand_high) != 1 || is.na(noiseBand_high)) {
    stop("noiseBand_high must be a single numeric value.")
  }
  if (!(noiseBand_low <= 0)) {
    stop("noiseBand_low must be <= 0.")
  }
  if (!(noiseBand_high >= 0)) {
    stop("noiseBand_high must be >= 0.")
  }

  # ---- Universal input check (adapted to this function's return type) ----
  if (!is.data.frame(curve_df) ||
      !all(c("separation_distance_nm", "force_nN") %in% names(curve_df))) {
    return(c(adhesive_area = NA_real_, repulsive_area = NA_real_))
  }

  # Coerce to numeric and drop non-finite pairs
  sep <- suppressWarnings(as.numeric(curve_df$separation_distance_nm))
  frc <- suppressWarnings(as.numeric(curve_df$force_nN))
  ok  <- is.finite(sep) & is.finite(frc)

  if (!any(ok)) {
    return(c(adhesive_area = NA_real_, repulsive_area = NA_real_))
  }

  # Use only valid pairs, preserve original order
  x <- sep[ok]
  y <- frc[ok]

  # Clamp tiny negative x to zero (do not reorder)
  if (any(x < 0)) x[x < 0] <- 0

  adhesive_area  <- 0
  repulsive_area <- 0
  n <- length(x)

  if (n < 2) return(c(adhesive_area = 0, repulsive_area = 0))

  for (i in seq_len(n - 1)) {
    x1 <- x[i];   y1 <- y[i]
    x2 <- x[i+1]; y2 <- y[i+1]

    # Skip zero-width segments (possible after clamping/backtracking)
    if (x1 == x2) next

    # Both above upper noise band → repulsive trapezoid
    if (y1 > noiseBand_high && y2 > noiseBand_high) {
      repulsive_area <- repulsive_area + area_trapezoid(x1, y1 - noiseBand_high, x2, y2 - noiseBand_high)
      next
    }

    # Both below lower noise band → adhesive trapezoid
    if (y1 < noiseBand_low && y2 < noiseBand_low) {
      adhesive_area  <- adhesive_area  + area_trapezoid(x1, y1 - noiseBand_low, x2, y2 - noiseBand_low)
      next
    }

    # Both within noise band [noiseBand_low, noiseBand_high] → skip entirely
    if (y1 >= noiseBand_low && y1 <= noiseBand_high && y2 >= noiseBand_low && y2 <= noiseBand_high) {
      next
    }

    # One endpoint on upper noise band
    if (y1 == noiseBand_high && y2 > noiseBand_high) {
      repulsive_area <- repulsive_area + area_triangle(x2, y2 - noiseBand_high, x1)
      next
    }
    if (y2 == noiseBand_high && y1 > noiseBand_high) {
      repulsive_area <- repulsive_area + area_triangle(x1, y1 - noiseBand_high, x2)
      next
    }

    # One endpoint on lower noise band
    if (y1 == noiseBand_low && y2 < noiseBand_low) {
      adhesive_area <- adhesive_area + area_triangle(x2, y2 - noiseBand_low, x1)
      next
    }
    if (y2 == noiseBand_low && y1 < noiseBand_low) {
      adhesive_area <- adhesive_area + area_triangle(x1, y1 - noiseBand_low, x2)
      next
    }

    # Handle crossings through both noise-band boundaries
    # NOTE: Area inside [noiseBand_low, noiseBand_high] is never counted
    if ((y1 > noiseBand_high && y2 < noiseBand_low) || (y1 < noiseBand_low && y2 > noiseBand_high)) {
      x_pos <- x1 + (x2 - x1) * (noiseBand_high - y1) / (y2 - y1)
      x_neg <- x1 + (x2 - x1) * (noiseBand_low - y1) / (y2 - y1)

      if (y1 > noiseBand_high) {
        repulsive_area <- repulsive_area + area_triangle(x1, y1 - noiseBand_high, x_pos)
        adhesive_area <- adhesive_area + area_triangle(x2, y2 - noiseBand_low, x_neg)
      } else {
        adhesive_area <- adhesive_area + area_triangle(x1, y1 - noiseBand_low, x_neg)
        repulsive_area <- repulsive_area + area_triangle(x2, y2 - noiseBand_high, x_pos)
      }
      next
    }

    # Crossing from above upper band to within band
    if (y1 > noiseBand_high && y2 >= noiseBand_low && y2 <= noiseBand_high) {
      x_cross <- x1 + (x2 - x1) * (noiseBand_high - y1) / (y2 - y1)
      repulsive_area <- repulsive_area + area_triangle(x1, y1 - noiseBand_high, x_cross)
      next
    }
    if (y2 > noiseBand_high && y1 >= noiseBand_low && y1 <= noiseBand_high) {
      x_cross <- x1 + (x2 - x1) * (noiseBand_high - y1) / (y2 - y1)
      repulsive_area <- repulsive_area + area_triangle(x2, y2 - noiseBand_high, x_cross)
      next
    }

    # Crossing from below lower band to within band
    if (y1 < noiseBand_low && y2 >= noiseBand_low && y2 <= noiseBand_high) {
      x_cross <- x1 + (x2 - x1) * (noiseBand_low - y1) / (y2 - y1)
      adhesive_area <- adhesive_area + area_triangle(x1, y1 - noiseBand_low, x_cross)
      next
    }
    if (y2 < noiseBand_low && y1 >= noiseBand_low && y1 <= noiseBand_high) {
      x_cross <- x1 + (x2 - x1) * (noiseBand_low - y1) / (y2 - y1)
      adhesive_area <- adhesive_area + area_triangle(x2, y2 - noiseBand_low, x_cross)
      next
    }
  }

  c(adhesive_area = adhesive_area, repulsive_area = repulsive_area)
}

#' Adhesive and Repulsive Energy (aJ) for All Transformed Curves in an fdObj
#'
#' Applies \code{analyze_a_curve_area()} to every transformed curve stored in
#' an \code{fdObj} and writes the resulting out-of-band force-distance areas
#' back into metadata as per-curve energy metrics. For each selected curve, the
#' function reads the corresponding lower and upper noise-band thresholds from
#' metadata, forwards those thresholds together with the transformed
#' force-separation data to \code{analyze_a_curve_area()}, and stores the two
#' returned areas separately rather than collapsing them into a single net
#' value. Negative-force area below the lower threshold is recorded as adhesive
#' energy, while positive-force area above the upper threshold is recorded as
#' repulsive energy.
#'
#' Results are written to the metadata as:
#' \itemize{
#'   \item \code{adhesive_energy_aJ_<dir>} — total area where \code{y < noiseBand_low}
#'   \item \code{repulsive_energy_aJ_<dir>} — total area where \code{y > noiseBand_high}
#' }
#' where \code{<dir>} is \code{"approach"} or \code{"retract"}.
#'
#' Energies are reported in \strong{attojoules (aJ)}, since
#' \eqn{1~\mathrm{nN·nm} = 10^{-18}~\mathrm{J} = 1~\mathrm{aJ}}.
#'
#' The wrapper does not recompute thresholds or modify curve geometry itself.
#' It relies on the transformed curves already stored in either
#' \code{fdObj@approachCurves} or \code{fdObj@retractCurves} and on existing
#' metadata columns \code{noiseBand_low_nN_<useCurve>} and
#' \code{noiseBand_high_nN_<useCurve>}. The underlying single-curve routine
#' preserves point order, and clamps
#' negative separation distances to zero before integration, so those same
#' rules apply here on a curve-by-curve basis.
#'
#' Behavior:
#' \itemize{
#'   \item If transformed curves are missing entirely for the selected direction, the function creates the output metadata columns and fills them with \code{NA}.
#'   \item If an individual curve is missing required transformed columns, is empty, or has missing noise-band thresholds, that curve is retained but both output energies are recorded as \code{NA}.
#'   \item If an individual curve is valid and simply contains no area outside the noise band, the returned adhesive and/or repulsive energy is \code{0} rather than \code{NA}.
#' }
#'
#' @param fdObj An object of class \code{fdObj}.
#' @param useCurve Character; must be one of \code{c("retract", "approach")}.
#' @param threads Integer. Number of parallel workers (default \code{1}). Uses
#'   \pkg{future}+\pkg{future.apply} when \code{threads > 1}.
#'
#' @details
#' This function assumes precomputed noise-band columns already exist in
#' \code{fdObj@metadata} and uses them directly:
#' \itemize{
#'   \item \code{noiseBand_low_nN_<useCurve>}
#'   \item \code{noiseBand_high_nN_<useCurve>}
#' }
#' Example for \code{useCurve = "retract"}:
#' \code{noiseBand_low_nN_retract} and \code{noiseBand_high_nN_retract}.
#'
#' @return The updated \code{fdObj} with two new metadata columns:
#' \code{adhesive_energy_aJ_<dir>} and \code{repulsive_energy_aJ_<dir>}.
#' @seealso analyze_a_curve_area, analyze_curves_adhesive_force
#' @examples
#' folder <- system.file("extdata", package = "curvana")
#' fd_obj <- createFdObjFromFolder(folder)
#' fd_obj <- transform_curves(fd_obj, spring_constant = 0.1, useCurve = "retract", threads = 1, least_length= 300)
#' fd_obj <- analyze_curves_noise(fd_obj, useCurve = "retract", threads = 1)
#' fd_obj = analyze_curves_energy(fd_obj, useCurve = "retract")
#' @export
analyze_curves_energy <- function(fdObj, useCurve = c("retract", "approach"), threads = 1) {
  if (!inherits(fdObj, "fdObj"))
    stop("fdObj must be of class 'fdObj'")

  useCurve <- match.arg(useCurve)
  curve_list <- if (useCurve == "approach") fdObj@approachCurves else fdObj@retractCurves
  dir_tag <- useCurve

  low_col <- paste0("noiseBand_low_nN_", dir_tag)
  high_col <- paste0("noiseBand_high_nN_", dir_tag)

  if (!low_col %in% names(fdObj@metadata)) {
    stop(sprintf("Required metadata column '%s' is missing.", low_col))
  }
  if (!high_col %in% names(fdObj@metadata)) {
    stop(sprintf("Required metadata column '%s' is missing.", high_col))
  }

  noise_low_vec <- fdObj@metadata[[low_col]]
  noise_high_vec <- fdObj@metadata[[high_col]]
  names(noise_low_vec) <- rownames(fdObj@metadata)
  names(noise_high_vec) <- rownames(fdObj@metadata)

  # If no transformed curves are present, create NA columns and exit
  if (is.null(curve_list) || length(curve_list) == 0) {
    warning("No transformed curves found for the selected direction. Did you run transform_curves()?")

    fdObj@metadata[[paste0("adhesive_energy_aJ_",  dir_tag)]] <- NA_real_
    fdObj@metadata[[paste0("repulsive_energy_aJ_", dir_tag)]] <- NA_real_
    return(fdObj)
  }

  curve_names <- names(curve_list)

  run_one <- function(id) {
    df <- curve_list[[id]]
    # If curve not analyzable at all, return NA/NA to distinguish from "no adhesion" (0/NA)
    if (!is.data.frame(df) ||
        !all(c("separation_distance_nm", "force_nN") %in% names(df)) ||
        nrow(df) == 0) {
      return(c(adhesive_force_nN = NA_real_, separation_distance_nm = NA_real_))
    }

    noise_low <- noise_low_vec[id]
    noise_high <- noise_high_vec[id]
    if (is.na(noise_low) || is.na(noise_high)) {
      return(c(adhesive_area = NA_real_, repulsive_area = NA_real_))
    }

    analyze_a_curve_area(df, noiseBand_low = as.numeric(noise_low), noiseBand_high = as.numeric(noise_high))
  }

  res_list <- if (threads > 1) {
    old_plan <- future::plan()
    on.exit(future::plan(old_plan), add = TRUE)
    future::plan(future::multisession, workers = threads)
    future.apply::future_lapply(curve_names, run_one,
                                 future.globals = list(
                                   run_one = run_one,
                                   curve_list = curve_list,
                                   noise_low_vec = noise_low_vec,
                                   noise_high_vec = noise_high_vec
                                 ),
                                 future.packages = "curvana")
  } else {
    lapply(curve_names, run_one)
  }

  res_mat <- do.call(rbind, res_list)
  res_df  <- as.data.frame(res_mat, stringsAsFactors = FALSE)
  rownames(res_df) <- curve_names

  fdObj@metadata[[paste0("adhesive_energy_aJ_",  dir_tag)]] <-
    res_df$adhesive_area[match(rownames(fdObj@metadata), rownames(res_df))]

  fdObj@metadata[[paste0("repulsive_energy_aJ_", dir_tag)]] <-
    res_df$repulsive_area[match(rownames(fdObj@metadata), rownames(res_df))]

  ad  <- fdObj@metadata[[paste0("adhesive_energy_aJ_",  dir_tag)]]
  rep <- fdObj@metadata[[paste0("repulsive_energy_aJ_", dir_tag)]]
  n_total <- length(curve_list)
  n_na_ad  <- sum(is.na(ad))
  n_na_rep <- sum(is.na(rep))

  message(sprintf(
    "Energy analyzed for %d curves (%s): %d NA adhesive, %d NA repulsive (units = aJ).",
    n_total, dir_tag, n_na_ad, n_na_rep
  ))

  fdObj
}


#' Run full analytical metrics pipeline on transformed curves in an fdObj
#'
#' Wrapper around:
#' \itemize{
#'   \item \code{analyze_curves_noise()}
#'   \item \code{analyze_curves_adhesive_force()}
#'   \item \code{analyze_curves_energy()}
#'   \item \code{analyze_curves_interaction_distance()} (optional negative and/or positive runs)
#' }
#'
#' This function runs the analytical-metrics stage of the workflow as one
#' coordinated wrapper. For each requested curve direction, it first estimates
#' per-curve noise bands, then optionally computes adhesive-force metrics,
#' energy metrics, rupture distances, and repulsive distances using those
#' stored thresholds and any required baseline-span settings. The goal is to
#' let users execute the standard downstream analysis sequence with a single
#' call instead of invoking each metric-specific helper manually.
#'
#' Processing is performed direction by direction. If \code{useCurve = "both"},
#' the function runs the full selected pipeline for approach curves and then
#' repeats it for retract curves, updating \code{fdObj@metadata} after each
#' stage. Each wrapped function is still responsible for its own validation and
#' for creating or updating its corresponding metadata columns, so this wrapper
#' mainly coordinates argument routing, optional execution, and call order.
#'
#' This means the returned object may contain a combination of noise-band,
#' adhesive-force, adhesive and repulsive energy, rupture-distance, and repulsive-distance columns,
#' depending on which optional analyses were enabled. Any curves that cannot be
#' analyzed by a downstream step are retained in the object, with that step's
#' output columns typically populated with \code{NA} for the affected rows.
#'
#' @param fdObj An object of class \code{fdObj}.
#' @param useCurve Character; one of \code{c("retract", "approach", "both")}. Default \code{"both"}.
#' @param threads Integer. Number of parallel workers passed to wrapped functions (default \code{1}).
#'
#' @param noise_baseline_span Either a single integer >= 1, or \code{"automatic"}.
#'   Passed to \code{analyze_curves_noise()}.
#' @param noise_threshold_method Character scalar; one of
#'   \code{c("sd", "mad", "quantile", "fixed")}. Passed to \code{analyze_curves_noise()}.
#' @param noise_multiplier Numeric >= 0. Passed to \code{analyze_curves_noise()}.
#' @param noise_mad_constant Numeric. Passed to \code{analyze_curves_noise()}.
#' @param noise_quantile_low Numeric in [0,1]. Passed to \code{analyze_curves_noise()}.
#' @param noise_quantile_high Numeric in [0,1]. Passed to \code{analyze_curves_noise()}.
#' @param noise_fixed_low Numeric or \code{NULL}. Passed to \code{analyze_curves_noise()} when method is \code{"fixed"}.
#' @param noise_fixed_high Numeric or \code{NULL}. Passed to \code{analyze_curves_noise()} when method is \code{"fixed"}.
#' @param analyze_adhesive_force Logical. If \code{TRUE}, runs \code{analyze_curves_adhesive_force()}.
#'   Default \code{TRUE}.
#' @param analyze_energy Logical. If \code{TRUE}, runs \code{analyze_curves_energy()}.
#'   Default \code{TRUE}.
#'
#' @param analyze_rupture_distance Logical. If \code{TRUE}, runs interaction-distance analysis with
#'   \code{y_direction = "negative"}. Default \code{TRUE}.
#' @param analyze_rupture_distance_baseline_span Either a single integer >= 1, or \code{"automatic"}.
#'   Passed to negative-direction \code{analyze_curves_interaction_distance()}.
#' @param analyze_rupture_distance_x_direction Character; one of \code{c("left", "right")}.
#'   Default \code{"left"}. Passed to negative-direction
#'   \code{analyze_curves_interaction_distance()}.
#' @param analyze_rupture_distance_min_consecutive Integer >= 1. Minimum
#'   consecutive points below threshold required to call a rupture event.
#'
#' @param analyze_repulsive_distance Logical. If \code{TRUE}, runs interaction-distance analysis with
#'   \code{y_direction = "positive"}. Default \code{TRUE}.
#' @param analyze_repulsive_distance_baseline_span Either a single integer >= 1, or \code{"automatic"}.
#'   Passed to positive-direction \code{analyze_curves_interaction_distance()}.
#' @param analyze_repulsive_distance_x_direction Character; one of \code{c("right", "left")}.
#'   Default \code{"right"}. Passed to positive-direction
#'   \code{analyze_curves_interaction_distance()}.
#' @param analyze_repulsive_distance_min_consecutive Integer >= 1. Minimum
#'   consecutive points above threshold required to call a repulsive event.
#'
#' @return Updated \code{fdObj} with analytical metrics written to metadata.
#' @seealso analyze_curves_noise, analyze_curves_adhesive_force, analyze_curves_energy, analyze_curves_interaction_distance
#' @examples
#' folder <- system.file("extdata", package = "curvana")
#' fd_obj <- createFdObjFromFolder(folder)
#' fd_obj <- transform_curves(fd_obj, spring_constant = 0.1, useCurve = "retract", threads = 1, least_length= 300)
#' fd_obj = analyze_curves_all_analytical_metrics(
#'   fd_obj, # the object
#'   useCurve = "retract", # run analysis for both approach and retract 
#'   threads = 1, 
#'
#'   # noise band parameters
#'   noise_baseline_span = "automatic", # use quantile method to define the noise band
#'   noise_threshold_method = "quantile", # use quantile method to define the noise band
#'   noise_quantile_low = 0.00, # define the lower end of noise band as minimum y value of the baseline section
#'   noise_quantile_high = 1, # define the upper end of noise band as maximum y value of the baseline section
#'   noise_multiplier = 1, # multiply the noise band by 1 to directly use the min-max range of the force in baseline region without further scaling.
#'
#'   # adhesive force
#'   analyze_adhesive_force = TRUE, # whether to analyze adhesive force
#'
#'   # adhesive energy and repulsive energy
#'   analyze_energy = TRUE, # whether to analyze interaction energy (repulsive and adhesive)
#'
#'   # rupture (adhesive) distance
#'   analyze_rupture_distance = TRUE, # whether to analyze adhesive/rupture distance
#'   analyze_rupture_distance_baseline_span = "automatic", # use the baseline section previously defined in curve transformation for rupture distance analysis
#'   analyze_rupture_distance_x_direction = "left", # scan from right to left to find the first data point at which a curve enters the adhesive region from the noise band region
#'   analyze_rupture_distance_min_consecutive = 3, # to reduce false positives from random fluctuations, require at least 3 consecutive points below the lower noise-band threshold for classifying the curve as entering the adhesive region.
#'
#'   # repulsive distance
#'   analyze_repulsive_distance = TRUE, # whether to analyze repulsive distance
#'   analyze_repulsive_distance_baseline_span = "automatic", # use the baseline section previously defined in curve transformation for repulsive distance analysis
#'   analyze_repulsive_distance_x_direction = "right", # scan from left to right to find the last data point before a curve first enters the noise band region from the repulsive region
#'   analyze_repulsive_distance_min_consecutive = 1 # because the curve is expected to start from the repulsive region, we set min_consecutive to 1, meaning that a single point above the positive noise threshold is sufficient to mark the start of the repulsive region.
#' )
#' @export
analyze_curves_all_analytical_metrics <- function(
    fdObj,
    useCurve = c("retract", "approach", "both"),
    threads = 1,
    noise_baseline_span = "automatic",
    noise_threshold_method = c("sd", "mad", "quantile", "fixed"),
    noise_multiplier = 3,
    noise_mad_constant = 1.4826,
    noise_quantile_low = 0.25,
    noise_quantile_high = 0.75,
    noise_fixed_low = NULL,
    noise_fixed_high = NULL,
    analyze_adhesive_force = TRUE,
    analyze_energy = TRUE,
    analyze_rupture_distance = TRUE,
    analyze_rupture_distance_baseline_span = "automatic",
    analyze_rupture_distance_x_direction = c("left", "right"),
    analyze_rupture_distance_min_consecutive = 3,
    analyze_repulsive_distance = TRUE,
    analyze_repulsive_distance_baseline_span = "automatic",
    analyze_repulsive_distance_x_direction = c("right", "left"),
    analyze_repulsive_distance_min_consecutive = 1
) {
  if (!inherits(fdObj, "fdObj")) {
    stop("fdObj must be of class 'fdObj'")
  }

  useCurve <- match.arg(useCurve)
  noise_threshold_method <- match.arg(noise_threshold_method)
  analyze_rupture_distance_x_direction <- match.arg(analyze_rupture_distance_x_direction)
  analyze_repulsive_distance_x_direction <- match.arg(analyze_repulsive_distance_x_direction)

  if (!is.logical(analyze_rupture_distance) || length(analyze_rupture_distance) != 1 || is.na(analyze_rupture_distance)) {
    stop("analyze_rupture_distance must be a single TRUE/FALSE value.")
  }
  if (!is.logical(analyze_repulsive_distance) || length(analyze_repulsive_distance) != 1 || is.na(analyze_repulsive_distance)) {
    stop("analyze_repulsive_distance must be a single TRUE/FALSE value.")
  }
  if (!is.logical(analyze_adhesive_force) || length(analyze_adhesive_force) != 1 || is.na(analyze_adhesive_force)) {
    stop("analyze_adhesive_force must be a single TRUE/FALSE value.")
  }
  if (!is.logical(analyze_energy) || length(analyze_energy) != 1 || is.na(analyze_energy)) {
    stop("analyze_energy must be a single TRUE/FALSE value.")
  }
  if (!is.numeric(analyze_rupture_distance_min_consecutive) ||
      length(analyze_rupture_distance_min_consecutive) != 1 ||
      !is.finite(analyze_rupture_distance_min_consecutive) ||
      analyze_rupture_distance_min_consecutive < 1) {
    stop("analyze_rupture_distance_min_consecutive must be a single integer >= 1.")
  }
  if (!is.numeric(analyze_repulsive_distance_min_consecutive) ||
      length(analyze_repulsive_distance_min_consecutive) != 1 ||
      !is.finite(analyze_repulsive_distance_min_consecutive) ||
      analyze_repulsive_distance_min_consecutive < 1) {
    stop("analyze_repulsive_distance_min_consecutive must be a single integer >= 1.")
  }

  analyze_rupture_distance_min_consecutive <- as.integer(analyze_rupture_distance_min_consecutive)
  analyze_repulsive_distance_min_consecutive <- as.integer(analyze_repulsive_distance_min_consecutive)

  directions <- if (useCurve == "both") c("approach", "retract") else useCurve

  for (direction in directions) {
    message(sprintf("Analyzing %s curves ...", direction))
    fdObj <- analyze_curves_noise(
      fdObj = fdObj,
      useCurve = direction,
      threads = threads,
      baseline_span = noise_baseline_span,
      threshold_method = noise_threshold_method,
      multiplier = noise_multiplier,
      mad_constant = noise_mad_constant,
      quantile_low = noise_quantile_low,
      quantile_high = noise_quantile_high,
      fixed_low = noise_fixed_low,
      fixed_high = noise_fixed_high
    )

    if (analyze_adhesive_force) {
      fdObj <- analyze_curves_adhesive_force(
        fdObj = fdObj,
        useCurve = direction,
        threads = threads
      )
    }

    if (analyze_energy) {
      fdObj <- analyze_curves_energy(
        fdObj = fdObj,
        useCurve = direction,
        threads = threads
      )
    }

    if (analyze_rupture_distance) {
      fdObj <- analyze_curves_interaction_distance(
        fdObj = fdObj,
        useCurve = direction,
        threads = threads,
        baseline_span = analyze_rupture_distance_baseline_span,
        y_direction = "negative",
        x_direction = analyze_rupture_distance_x_direction,
        min_consecutive = analyze_rupture_distance_min_consecutive
      )
    }

    if (analyze_repulsive_distance) {
      fdObj <- analyze_curves_interaction_distance(
        fdObj = fdObj,
        useCurve = direction,
        threads = threads,
        baseline_span = analyze_repulsive_distance_baseline_span,
        y_direction = "positive",
        x_direction = analyze_repulsive_distance_x_direction,
        min_consecutive = analyze_repulsive_distance_min_consecutive
      )
    }
  }

  fdObj
}


