#' Calculate Sensitivity from a Segment of AFM Data
#'
#' This function identifies a linear segment from AFM deflection vs. piezo extension data and calculates
#' the sensitivity (slope) using linear regression. It iteratively adds chunks of data to build a segment
#' with high linearity (\eqn{R^2 > 0.999}) and optionally removes outliers to maintain this criterion.
#'
#' @param end Integer. The maximum index to consider in the `x` and `y` vectors (i.e., up to which point to search).
#' @param intv Integer. The chunk size to use when iteratively selecting data segments.
#' @param x Numeric vector. The piezo extension values (e.g., distance in nm).
#' @param y Numeric vector. The deflection values (e.g., voltage).
#'
#' @return A list of three elements:
#' \describe{
#'   \item{senscal_segment}{a dataframe of a subset of points in the original raw curve which are used for sensitivity calibration}
#'   \item{sensitivity}{The calculated sensitivity (slope) as a numeric value, or \code{NULL} if not computable.}
#' }
#'
#' @examples
#' x <- seq(0, 100, by = 1)
#' y <- 0.02 * x + rnorm(length(x), sd = 0.01)
#' calc_sensitivity(end = 80, intv = 10, x = x, y = y)
#'
#' @export
calc_sensitivity <- function(end, intv, x, y) {
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

    if (!is.na(r) && r^2 > 0.999) {
      sens_x <- combined_x
      sens_y <- combined_y
    } else {
      for (j in intv:1) {
        temp_x <- x_seg
        temp_y <- y_seg

        temp_x <- temp_x[-j]
        temp_y <- temp_y[-j]

        r_try <- suppressWarnings(cor(c(sens_x, temp_x), c(sens_y, temp_y), use = "complete.obs"))

        if (!is.na(r_try) && r_try^2 > 0.999) {
          sens_x <- c(sens_x, temp_x)
          sens_y <- c(sens_y, temp_y)
          break
        }
      }
    }

    i <- i + intv
  }

  if (length(sens_x) == 0 || length(sens_y) == 0) {
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
#' @param end Integer. The maximum index in raw curves to consider (e.g., 200).
#' @param intv Integer. Chunk size for sensitivity calculation (e.g., 4).
#' @param useCurve Character. Either "approach" or "retract" to determine which curve to use.
#' @param threads Number of parallel threads to use (default = 1).
#'
#' @return An updated \code{fdObj} with sensitivity values in metadata and segments in senscal_segment.
#' @export
analyze_sensitivity <- function(fdObj, end = 200, intv = 4, useCurve = "approach", threads = 1) {
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
      calc_sensitivity(end = end, intv = intv, x = x, y = y)
    }, future.globals = list(calc_sensitivity = calc_sensitivity))
  } else {
    results <- lapply(curve_names, function(name) {
      df <- raw_list[[name]]
      if (!(x_col %in% colnames(df)) || !(y_col %in% colnames(df))) {
        return(list(senscal_segment = NULL, sensitivity = NULL))
      }
      x <- df[[x_col]]
      y <- df[[y_col]]
      calc_sensitivity(end = end, intv = intv, x = x, y = y)
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
  fdObj@metadata$sensitivity <- sensitivity_values

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
#' This function identifies a flat, noise-stable baseline segment at the end of an AFM (Atomic Force Microscopy)
#' raw force-distance curve. It applies a linear regression on a trailing window to determine if the segment is
#' sufficiently flat and stable to be considered a baseline.
#'
#' @param x Numeric vector. Distance values (e.g., piezo positions or z-sensor values).
#' @param y Numeric vector. Deflection values (e.g., in volts).
#' @param least_length Integer. Minimum number of points in the baseline segment.
#' @param sensitivity Numeric. Scaling factor for the deflection signal (e.g., detector sensitivity in V/nm or V/nN).
#' @param slp_threshold Numeric. Maximum absolute slope for the segment to be considered flat (default: 0.001).
#' @param std_threshold Numeric. Maximum standard error of the slope (default: 0.005).
#'
#' @return A list with two elements:
#' \describe{
#'   \item{baseline}{Numeric. The estimated baseline deflection value (in original units), or \code{NULL} if no valid segment is found.}
#'   \item{segment}{Data frame with \code{x} and \code{y} columns representing the detected baseline segment, or \code{NULL} if not found.}
#' }
#'
#' @details
#' The function assumes the baseline occurs toward the tail end of the curve (typically during the retract phase).
#' It is often used prior to baseline correction or contact point determination in AFM force curve analysis.
#'
#' @examples
#' # Simulated flat tail data
#' x <- 1:1000
#' y <- c(rnorm(980, 0.1, 0.05), rnorm(20, 0, 0.002))  # flat tail
#' result <- find_baseline(least_length = 15, sensitivity = 1, x = x, y = y)
#' print(result$baseline)
#' plot(x, y, type = "l"); lines(result$segment, col = "red", lwd = 2)
#'
#' @export
find_baseline <- function(x, y, least_length, sensitivity, slp_threshold = 0.001, std_threshold = 0.005) {
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
#' Identifies baseline segments in AFM raw force-distance curves using pre-calculated sensitivity values,
#' and stores baseline values and segments in the fdObj.
#'
#' @param fdObj An object of class \code{fdObj}.
#' @param least_length Integer. Minimum number of points in the baseline segment.
#' @param useCurve Character. Either "approach" or "retract" to specify which raw curve to use.
#' @param slp_threshold Numeric. Maximum absolute slope allowed for baseline detection (default = 0.001).
#' @param std_threshold Numeric. Maximum standard error of the slope (default = 0.005).
#' @param threads Integer. Number of parallel threads to use (default = 1).
#'
#' @return An updated \code{fdObj} with baseline values in the metadata column \code{baseline_nm} and baseline segments in \code{baseline_segment}.
#' @export
analyze_baseline <- function(fdObj, least_length = 150, useCurve = NULL,
                             slp_threshold = 0.001, std_threshold = 0.005,
                             threads = 1) {
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
  sensitivity_vec <- fdObj@metadata$sensitivity
  names(sensitivity_vec) <- rownames(fdObj@metadata)

  if (is.null(sensitivity_vec)) stop("No sensitivity values found in metadata.")

  find_result_for_curve <- function(name) {
    df <- raw_list[[name]]
    sensitivity <- sensitivity_vec[name]

    if (is.na(sensitivity) || !(x_col %in% names(df)) || !(y_col %in% names(df))) {
      return(list(baseline = NA_real_, segment = empty_seg))
    }

    x <- df[[x_col]]
    y <- df[[y_col]]
    res <- find_baseline(
      x = x,
      y = y,
      least_length = least_length,
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
        x_col = x_col,
        y_col = y_col,
        empty_seg = empty_seg,
        least_length = least_length,
        slp_threshold = slp_threshold,
        std_threshold = std_threshold
      )
    )
  } else {
    results <- lapply(curve_names, find_result_for_curve)
  }

  # Extract results
  baseline_values <- sapply(results, function(r) if (is.null(r$baseline)) NA_real_ else r$baseline)
  baseline_segments <- lapply(results, function(r) r$segment)
  names(baseline_segments) <- curve_names

  # Update fdObj
  if(useCurve == 'approach') fdObj@metadata$baseline_nm_approach <- baseline_values else fdObj@metadata$baseline_nm_retract <- baseline_values
  fdObj@baseline_segment[[useCurve]] <- baseline_segments

  # Summary
  n_fail <- sum(is.na(baseline_values))
  message(sprintf("Processed %d curves; %d failed baseline detection.", length(curve_names), n_fail))

  return(fdObj)
}

#' Transform a Single AFM Curve into Separation Distance and Force
#'
#' Converts raw AFM deflection and piezo data into calibrated separation distance and force.
#' The zero-separation position is estimated from the sensitivity calibration segment.
#'
#' @param x Numeric vector. Piezo position (distance) in nm.
#' @param y Numeric vector. Deflection signal in volts.
#' @param baseline Numeric. The deflection baseline (in volts).
#' @param sensitivity Numeric. Sensitivity in V/nm.
#' @param spring_constant Numeric. Spring constant in nN/nm.
#' @param senscal_seg_x Numeric vector. X-values of the sensitivity calibration segment.
#' @param senscal_seg_y Numeric vector. Y-values of the sensitivity calibration segment (in volts).
#'
#' @return A data.frame with:
#' \describe{
#'   \item{separation_distance_nm}{Tip-sample separation (nm).}
#'   \item{force_nN}{Force (nN).}
#' }
#' @export
transform_a_curve <- function(x, y,
                              baseline, sensitivity, spring_constant,
                              senscal_seg_x, senscal_seg_y) {
  if (length(x) != length(y)) stop("x and y must be the same length.")
  if (length(senscal_seg_x) != length(senscal_seg_y)) stop("senscal_seg_x and senscal_seg_y must be the same length.")

  # Correct deflection
  new_defl_v <- y - baseline
  new_defl_length_nm <- new_defl_v / sensitivity
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
