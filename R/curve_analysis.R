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
#' x <- seq(0, 100, by = 1)
#' y <- 0.02 * x + rnorm(length(x), sd = 0.01)
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
#' @param end Integer. The maximum index in raw curves to consider (e.g., 200).
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
#' and stores baseline values and segments in the fdObj. As the undulating baseline is defined by forces, 
#' the sensitivity is used to convert the deflection signal into force units for accurate baseline filtering.
#'
#' @param fdObj An object of class \code{fdObj}.
#' @param least_length Either a single integer (minimum number of points in
#'   the baseline segment) or \code{"automatic"}. When \code{"automatic"},
#'   per-curve span values are read from
#'   \code{fdObj@metadata$baseline_span_approach} or
#'   \code{fdObj@metadata$baseline_span_retract} depending on \code{useCurve}.
#' @param useCurve Character. Either "approach" or "retract" to specify which raw curve to use.
#' @param slp_threshold Numeric. Maximum absolute slope allowed for baseline detection (default = 0.001).
#' @param std_threshold Numeric. Maximum standard error of the slope (default = 0.005).
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
#' @export
analyze_baseline <- function(fdObj, least_length = 150, useCurve = NULL,
                             slp_threshold = 0.001, std_threshold = 0.005,
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
#'
#' @param raw_curve A raw curve data frame containing deflection columns.
#' @param p Filter polynomial order.
#' @param n Filter length (must be odd).
#' @param m Order of the derivative to compute.
#' @param ts Sampling interval.
#' @param useCurve Character; one of \code{c("retract", "approach", "both")}.
#'
#' @return A data frame with original columns preserved and additional
#' \code{*_original} backup column(s) for the smoothed deflection column(s).
#' @export
denoise_a_curve <- function(raw_curve,
                    p = 1,
                    n = 3,
                    m = 0,
                    ts = 1,
                    useCurve = c("retract", "approach", "both")) {
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

  raw_curve <- raw_curve %>%
    dplyr::mutate(dplyr::across(dplyr::all_of(target_cols), ~ .x, .names = "{.col}_original")) %>%
    dplyr::mutate(dplyr::across(
      dplyr::all_of(target_cols),
      ~ signal::sgolayfilt(.x, p = p, n = n, m = m, ts = ts)
    ))

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

  run_one <- function(name) {
    denoise_a_curve(
      raw_curve = raw_list[[name]],
      p = p,
      n = n,
      m = m,
      ts = ts,
      useCurve = useCurve
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
#' Converts raw AFM deflection and piezo data into calibrated separation distance and force.
#' If the measurement is performed on a soft sample rather than a hard reference,
#' the \code{soft} argument can be set to \code{TRUE} and a \code{probe_sensitivity}
#' value, which should be acquired by measurements on a hard surface,
#'  needs to be provided to convert deflection to force.
#'
#' @param x Numeric vector. Piezo position (distance) in nm.
#' @param y Numeric vector. Deflection signal in volts.
#' @param baseline Numeric. The deflection baseline (in volts).
#' @param sensitivity Numeric. Sensitivity in V/nm.
#' @param spring_constant Numeric. Spring constant in nN/nm.
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
#' Applies `transform_a_curve()` to all raw curves in an fdObj object.
#' Automatically computes sensitivity and baseline values if they are missing
#' by calling `analyze_sensitivity()` and `analyze_baseline()`, passing through
#' any user-supplied arguments that match those functions' parameters.
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
#'   \code{calc_sensitivity()}. Default is 0.99.
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
                             R_squared_min = 0.99,
                             minimum_length = 4,
                             least_length = 150,
                             slp_threshold = 0.001,
                             std_threshold = 0.005,
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

  # ---- Transform a single curve ----
  transform_one <- function(name) {
    df <- raw_list[[name]]
    if (!(x_col %in% colnames(df)) || !(y_col %in% colnames(df))) {
      return(data.frame(separation_distance_nm = numeric(0), force_nN = numeric(0)))
    }

    x <- df[[x_col]]
    y <- df[[y_col]]
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
#' Computes the adhesive force as the most negative force value in a transformed curve
#' and returns that value along with the separation distance at which it occurs. Note that
#' the value will be negated. For example, if the most negative force is -2 nN,
#' the function will return 2 nN as the adhesive force.
#' @param curve_df A data.frame with columns:
#'   \itemize{
#'     \item{\code{separation_distance_nm}}{Numeric tip-sample separation (nm).}
#'     \item{\code{force_nN}}{Numeric force (nN).}
#'   }
#'
#' @return A named numeric vector of length 2:
#' \describe{
#'   \item{adhesive_force_nN}{The most negative (minimum) force value in nN.}
#'   \item{separation_distance_nm}{The separation distance (nm) at which the adhesive force occurs.}
#' }
#' If no negative force exists (i.e., all forces are \eqn{\ge} 0) or inputs are invalid,
#' returns \code{c(adhesive_force_nN = NA_real_, separation_distance_nm = NA_real_)}.
#'
#' @examples
#' df <- data.frame(
#'   separation_distance_nm = seq(-50, 200, by = 1),
#'   force_nN = 0.02 * seq(-50, 200, by = 1) + c(rep(0, 80), -3, rep(0, 170))
#' )
#' analyze_a_curve_adhesive_force(df)
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
#' Computes a positive noise cutoff (nN) from the baseline window of one transformed
#' AFM curve.
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
#' @return Named numeric vector: c(noiseBand_low = value, noiseBand_high = value).
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

#' Analyze interaction distance from a single AFM curve (flexible thresholding)
#'
#' @description
#' Detects the first significant deviation in force relative to a baseline window,
#' marking rupture length (negative excursion) or repulsive distance (positive excursion).
#' This function uses user-provided noise-band thresholds directly.
#' Scanning can proceed from right-to-left (default) or left-to-right.
#' A detection is accepted only when at least \\code{min_consecutive}
#' consecutive points satisfy the excursion criterion.
#' When y-direction is negative, the function scans the curve to look for the first point where force <  lower bound of noise band (i.e., the curve enters the adhesive region).
#' When y-direction is positive, the function scans the curve to look for the last point where force >  upper bound of noise band (i.e., the curve exits the repulsive region).
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
#' @return c(distance = x_at_first_excursion_or_NA, threshold = numeric_threshold_used)
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
#' Applies \code{analyze_a_curve_interaction_distance()} to each transformed curve
#' in an \code{fdObj} and stores the resulting interaction distances and thresholds
#' in the metadata. These measure the first significant excursion in force relative
#' to a baseline, either repulsive (positive force deviation) or rupture (negative).
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
#' @return The updated \code{fdObj} with two new metadata columns:
#' \code{<type>_distance_nm_<dir>} and \code{<type>_threshold_nN_<dir>},
#' where <type> is "repulsive" or "rupture", depending on \code{y_direction},
#' and <dir> is "approach" or "retract".
#' @seealso analyze_a_curve_interaction_distance
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







#' Analyze the adhesive and repulsive areas of a single AFM curve (data.frame input)
#'
#' Calculates the total areas above (\emph{repulsive}) and below (\emph{adhesive})
#' the noise threshold for one curve supplied as a data.frame with columns
#' \code{separation_distance_nm} (x) and \code{force_nN} (y).
#'
#' The computation proceeds in the given point order (no sorting). Segments entirely
#' above the positive noise threshold or below the negative noise threshold are integrated
#' as trapezoids, and segments that cross the noise thresholds are split at the interpolated
#' crossing point and integrated as triangles. Areas within the noise band (-noise_cutoff to +noise_cutoff) are ignored.
#'
#' As a practical fix for small transformation offsets near the origin, any
#' \code{separation_distance_nm < 0} are clamped to 0 before integration.
#' This works for both approach (x may run high→low) and retract (low→high) traces.
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
#' df <- data.frame(
#'   separation_distance_nm = c(-0.02, 0, 0.10, 0.05, 0.20),  # includes clamp and backtrack
#'   force_nN               = c( 0.50 , 0, 0.30, -0.20, 0.10)
#' )
#' analyze_a_curve_area(df)
#' analyze_a_curve_area(df, noiseBand_low = -0.1, noiseBand_high = 0.1)
#'
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
#' Applies \code{analyze_a_curve_area()} to each transformed curve in an \code{fdObj}
#' and stores the resulting energies (areas under the force–distance curve) in
#' the metadata as:
#' \itemize{
#'   \item \code{adhesive_energy_aJ_<dir>} — total area where \code{y < noiseBand_low}
#'   \item \code{repulsive_energy_aJ_<dir>} — total area where \code{y > noiseBand_high}
#' }
#' where \code{<dir>} is \code{"approach"} or \code{"retract"}.
#'
#' Energies are reported in \strong{attojoules (aJ)}, since
#' \eqn{1~\mathrm{nN·nm} = 10^{-18}~\mathrm{J} = 1~\mathrm{aJ}}.
#'
#' The function preserves the point order (no sorting), tolerates backtracking in \code{x},
#' and inherits clamping of \code{x<0} to 0 from \code{analyze_a_curve_area()}.
#'
#' Behavior:
#' \itemize{
#'   \item If a curve is present/valid but yields no finite result, both energies are \code{NA}.
#'   \item If no transformed curves exist for the selected direction, both metadata columns are created and filled with \code{NA}.
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
#' This function executes the "Analytical metrics calculation" steps in one call.
#' Users can run for approach curves, retract curves, or both.
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


