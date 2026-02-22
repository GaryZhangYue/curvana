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
    },future.packages = "curvana")
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
#' and stores baseline values and segments in the fdObj.
#'
#' @param fdObj An object of class \code{fdObj}.
#' @param least_length Integer. Minimum number of points in the baseline segment.
#' @param useCurve Character. Either "approach" or "retract" to specify which raw curve to use.
#' @param slp_threshold Numeric. Maximum absolute slope allowed for baseline detection (default = 0.001).
#' @param std_threshold Numeric. Maximum standard error of the slope (default = 0.005).
#' @param threads Integer. Number of parallel threads to use (default = 1).
#'
#' @return An updated \code{fdObj} with baseline values in the metadata column \code{baseline_V}, the minimum number of points in the baseline segment \code{baseline_span}, and baseline segments in \code{baseline_segment}.
#' @export
analyze_baseline <- function(fdObj, least_length = 150, useCurve = NULL,
                             slp_threshold = 0.001, std_threshold = 0.005,
                             threads = 1) {
  if (!inherits(fdObj, "fdObj")) stop("fdObj must be of class 'fdObj'")
  if (!useCurve %in% c("approach", "retract")) stop("useCurve must be either 'approach' or 'retract'")

  # Set column names based on approach/retract; select the corresponding sensitivity value
  if (useCurve == "approach") {
    x_col <- "Calc_Ramp_Ex_nm"
    y_col <- "Defl_V_Ex"
    empty_seg <- data.frame(Calc_Ramp_Ex_nm = numeric(0), Defl_V_Ex = numeric(0))
    sensitivity_vec <- fdObj@metadata$sensitivity_V_nm_approach
  } else {
    x_col <- "Calc_Ramp_Rt_nm"
    y_col <- "Defl_V_Rt"
    empty_seg <- data.frame(Calc_Ramp_Rt_nm = numeric(0), Defl_V_Rt = numeric(0))
    sensitivity_vec <- fdObj@metadata$sensitivity_V_nm_retract
  }

  raw_list <- fdObj@rawCurves
  curve_names <- names(raw_list)
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
  if (!all(names(results) == curve_names)) stop("the input and output order does not match")
  baseline_values <- sapply(results, function(r) if (is.null(r$baseline)) NA_real_ else r$baseline)
  baseline_segments <- lapply(results, function(r) r$segment)
  names(baseline_segments) <- curve_names

  # Update fdObj
  if(useCurve == 'approach') {
    fdObj@metadata$baseline_V_approach <- baseline_values
    fdObj@metadata$baseline_span_approach <- least_length
    } else {
    fdObj@metadata$baseline_V_retract <- baseline_values
    fdObj@metadata$baseline_span_retract <- least_length
  }
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
#' If any of the input is NULL, an empty dataframe will be returned
#' @export
transform_a_curve <- function(x, y,
                              baseline, sensitivity, spring_constant,
                              senscal_seg_x, senscal_seg_y) {
  if (length(x) != length(y)) stop("x and y must be the same length.")
  if (length(senscal_seg_x) != length(senscal_seg_y)) stop("senscal_seg_x and senscal_seg_y must be the same length.")

  if(is.na(baseline) || is.na(sensitivity) || is.null(senscal_seg_x) || is.null(senscal_seg_y)){
    return(data.frame(
      separation_distance_nm = numeric(0),
      force_nN = numeric(0)
    ))
  }
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
#' @param ... Additional arguments passed to `analyze_sensitivity()` or
#'   `analyze_baseline()` if they are invoked automatically.
#'
#' @return An updated \code{fdObj} with transformed curves stored in the corresponding slot.
#' @export
transform_curves <- function(fdObj, spring_constant, useCurve = c("approach", "retract"), threads = 1, ...) {
  # ---- Validation ----
  if (!inherits(fdObj, "fdObj"))
    stop("fdObj must be of class 'fdObj'")

  useCurve <- match.arg(useCurve)

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
    call_args <- c(as.list(environment()), list(...))
    sens_args <- call_args[names(call_args) %in% names(formals(analyze_sensitivity))]
    sens_args$fdObj <- fdObj
    if (is.null(sens_args$useCurve)) sens_args$useCurve <- useCurve
    fdObj <- do.call(analyze_sensitivity, sens_args)
  }

  sensitivity_vec <- fdObj@metadata[[sens_col]]
  names(sensitivity_vec) <- rownames(fdObj@metadata)
  senscal_segment <- fdObj@senscal_segment[[useCurve]]

  # ---- Ensure baseline information ----
  base_col <- paste0("baseline_V_", useCurve)
  if (!base_col %in% names(fdObj@metadata)) {
    message(sprintf("Column '%s' not found in metadata. Running analyze_baseline()...", base_col))
    call_args <- c(as.list(environment()), list(...))
    base_args <- call_args[names(call_args) %in% names(formals(analyze_baseline))]
    base_args$fdObj <- fdObj
    if (is.null(base_args$useCurve)) base_args$useCurve <- useCurve
    fdObj <- do.call(analyze_baseline, base_args)
  }

  baseline_vec <- fdObj@metadata[[base_col]]
  names(baseline_vec) <- rownames(fdObj@metadata)

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
        senscal_seg_y = senscal_seg[[2]]
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
#' and returns that value along with the separation distance at which it occurs.
#'
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
#'   force_nN = 0.02 * seq(-50, 200, by = 1) + c(rep(0, 80), -3, rep(0, 171))
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

  c(adhesive_force_nN = unname(min_force),
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
  n_detect  <- sum(!is.na(forces) & forces < 0)

  message(sprintf(
    "Adhesive force analyzed for %d curves: %d with adhesion (negative), %d with no adhesion (0), %d not analyzed (NA).",
    n_total, n_detect, n_zero, n_na
  ))

  fdObj
}

#' Analyze interaction distance from a single AFM curve (flexible thresholding)
#'
#' @description
#' Detects the first significant deviation in force relative to a baseline window,
#' marking rupture length (negative excursion) or repulsive distance (positive excursion).
#' The cutoff can be based on SD, MAD, IQR, quantiles, absolute-quantiles, or a fixed value.
#' Scanning can proceed from right-to-left (default) or left-to-right.
#'
#' @param curve_df data.frame with columns:
#'   - separation_distance_nm (numeric): x values (distance, nm)
#'   - force_nN (numeric): y values (force, nN)
#' @param baseline_span Integer >= 1. Number of last points used as the baseline window.
#' @param y_direction "negative" or "positive".
#'   - "negative": find first y < -threshold (rupture-like)
#'   - "positive": find first y >  threshold (repulsion-like)
#' @param x_direction "left" or "right".
#'   - "left": scan from right to left (from just before baseline toward the origin)
#'   - "right": scan from left to right (from origin up to just before baseline)
#' @param threshold_method One of
#'   c("sd","mad","iqr","quantile","abs_quantile","fixed"). Default "sd".
#'   - "sd":      threshold = multiplier * sd(baseline)
#'   - "mad":     threshold = multiplier * (mad(baseline) * mad_constant)
#'   - "iqr":     threshold = multiplier * IQR(baseline)
#'   - "quantile": for "negative" use |quantile(baseline, probs = q_low)|,
#'                 for "positive" use  quantile(baseline, probs = q_high)
#'   - "abs_quantile": threshold = quantile(abs(baseline), probs = q_abs)
#'   - "fixed":  threshold = fixed_threshold (in nN)
#' @param multiplier Numeric >= 0 used by "sd", "mad", "iqr". Default 3.
#' @param mad_constant Scaling for MAD to be SD-equivalent. Default 1.4826.
#' @param q_low,q_high Quantiles for "quantile" method. Defaults: 0.01, 0.99.
#' @param q_abs Quantile for "abs_quantile". Default 0.99.
#' @param fixed_threshold Numeric (nN) for "fixed" method. Default NULL.
#'
#' @return c(distance = x_at_first_excursion_or_NA, threshold = numeric_threshold_used)
#' @export
analyze_a_curve_interaction_distance <- function(
    curve_df,
    baseline_span,
    y_direction = c("negative", "positive"),
    x_direction = c("left", "right"),
    threshold_method = c("sd","mad","iqr","quantile","abs_quantile","fixed"),
    multiplier = 3,
    mad_constant = 1.4826,
    q_low = 0.01,
    q_high = 0.99,
    q_abs = 0.99,
    fixed_threshold = NULL
) {
  y_direction <- match.arg(y_direction)
  x_direction <- match.arg(x_direction)
  threshold_method <- match.arg(threshold_method)

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

  n <- nrow(curve_df)
  baseline_span <- min(as.integer(baseline_span), n)

  x <- curve_df$separation_distance_nm
  y <- curve_df$force_nN

  # ---- baseline window (last baseline_span points) ----
  b_start <- n - baseline_span + 1L
  y_base  <- y[b_start:n]

  # ---- compute threshold ----
  compute_threshold <- function() {
    guard <- function(val) if (is.na(val) || val <= 0) .Machine$double.eps else val

    if (threshold_method == "sd") {
      sdv <- stats::sd(y_base, na.rm = TRUE)
      return(multiplier * guard(sdv))
    }
    if (threshold_method == "mad") {
      md <- stats::mad(y_base, constant = 1, na.rm = TRUE)  # unscaled MAD
      return(multiplier * guard(md) * mad_constant)
    }
    if (threshold_method == "iqr") {
      iq <- stats::IQR(y_base, na.rm = TRUE)
      return(multiplier * guard(iq))
    }
    if (threshold_method == "quantile") {
      if (y_direction == "negative") {
        qv <- stats::quantile(y_base, probs = q_low, na.rm = TRUE, names = FALSE)
        return(abs(guard(abs(qv))))
      } else {
        qv <- stats::quantile(y_base, probs = q_high, na.rm = TRUE, names = FALSE)
        return(guard(abs(qv)))
      }
    }
    if (threshold_method == "abs_quantile") {
      qv <- stats::quantile(abs(y_base), probs = q_abs, na.rm = TRUE, names = FALSE)
      return(guard(qv))
    }
    if (threshold_method == "fixed") {
      if (is.null(fixed_threshold) || !is.numeric(fixed_threshold) || fixed_threshold < 0) {
        stop("For threshold_method = 'fixed', provide non-negative numeric fixed_threshold (nN).")
      }
      return(guard(fixed_threshold))
    }
    stop("Unknown threshold_method.")
  }

  threshold <- compute_threshold()

  # ---- choose scan indices based on x_direction ----
  if (x_direction == "left") {
    # right -> left (from just before baseline toward origin)
    scan_idx <- seq.int(from = b_start - 1L, to = 1L, by = -1L)
  } else {
    # left -> right (from origin up to just before baseline)
    scan_idx <- seq.int(from = 1L, to = max(b_start - 1L, 1L), by = 1L)
    if (b_start <= 1L) scan_idx <- integer(0)  # no room before baseline
  }

  if (length(scan_idx) == 0L) {
    return(c(distance = NA_real_, threshold = threshold))
  }

  # ---- apply y-direction rule ----
  y_scan <- y[scan_idx]
  hit_mask <- if (y_direction == "negative") {
    y_scan < -threshold
  } else {
    y_scan >  threshold
  }

  hit <- which(hit_mask)[1L]
  if (is.na(hit)) return(c(distance = NA_real_, threshold = threshold))

  c(distance = x[scan_idx[hit]], threshold = threshold)
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
#' @param y_direction "negative" (rupture) or "positive" (repulsive).
#' @param x_direction "left" or "right". Direction to scan from baseline.
#' @param threshold_method Thresholding method, passed to
#'   \code{analyze_a_curve_interaction_distance()}.
#' @param multiplier,mad_constant,q_low,q_high,q_abs,fixed_threshold
#'   Threshold-related parameters, passed through to the inner function.
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
    threshold_method = c("sd","mad","iqr","quantile","abs_quantile","fixed"),
    multiplier = 3,
    mad_constant = 1.4826,
    q_low = 0.01,
    q_high = 0.99,
    q_abs = 0.99,
    fixed_threshold = NULL
) {
  # ---- Validation ----
  if (!inherits(fdObj, "fdObj"))
    stop("fdObj must be of class 'fdObj'")

  useCurve <- match.arg(useCurve)
  y_direction <- match.arg(y_direction)
  x_direction <- match.arg(x_direction)
  threshold_method <- match.arg(threshold_method)

  curve_list <- if (useCurve == "approach") fdObj@approachCurves else fdObj@retractCurves
  dir_tag <- useCurve

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

    analyze_a_curve_interaction_distance(
      curve_df = df,
      baseline_span = bs_value,
      y_direction = y_direction,
      x_direction = x_direction,
      threshold_method = threshold_method,
      multiplier = multiplier,
      mad_constant = mad_constant,
      q_low = q_low,
      q_high = q_high,
      q_abs = q_abs,
      fixed_threshold = fixed_threshold
    )
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
#' @param noise_cutoff Numeric. Noise threshold for energy calculation (default: 0). Only areas where |force| > noise_cutoff are included in energy calculation. Negative values are converted using \code{abs(noise_cutoff)}.
#'
#' @return A named numeric vector of length 2:
#' \describe{
#'   \item{adhesive_area}{Total positive area where \code{y < -noise_cutoff}.}
#'   \item{repulsive_area}{Total positive area where \code{y > noise_cutoff}.}
#' }
#'
#' @examples
#' df <- data.frame(
#'   separation_distance_nm = c(-0.02, 0, 0.10, 0.05, 0.20),  # includes clamp and backtrack
#'   force_nN               = c( 0.50 , 0, 0.30, -0.20, 0.10)
#' )
#' analyze_a_curve_area(df)
#' analyze_a_curve_area(df, noise_cutoff = 0.1)  # Only count forces > 0.1 nN in magnitude
#'
#' @seealso area_trapezoid, area_triangle, crossing_x0
#' @export
analyze_a_curve_area <- function(curve_df, noise_cutoff = 0) {
  noise_cutoff <- abs(noise_cutoff)

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

    # Both above noise cutoff → repulsive trapezoid
    if (y1 > noise_cutoff && y2 > noise_cutoff) {
      repulsive_area <- repulsive_area + area_trapezoid(x1, y1 - noise_cutoff, x2, y2 - noise_cutoff)
      next
    }

    # Both below negative noise cutoff → adhesive trapezoid
    if (y1 < -noise_cutoff && y2 < -noise_cutoff) {
      adhesive_area  <- adhesive_area  + area_trapezoid(x1, y1 + noise_cutoff, x2, y2 + noise_cutoff)
      next
    }

    # Both within noise band (-noise_cutoff to +noise_cutoff) → skip entirely (no area counted)
    if (abs(y1) <= noise_cutoff && abs(y2) <= noise_cutoff) {
      next
    }

    # One endpoint on positive noise cutoff
    if (y1 == noise_cutoff && y2 > noise_cutoff) {
      repulsive_area <- repulsive_area + area_triangle(x2, y2 - noise_cutoff, x1)
      next
    }
    if (y2 == noise_cutoff && y1 > noise_cutoff) {
      repulsive_area <- repulsive_area + area_triangle(x1, y1 - noise_cutoff, x2)
      next
    }

    # One endpoint on negative noise cutoff 
    if (y1 == -noise_cutoff && y2 < -noise_cutoff) {
      adhesive_area <- adhesive_area + area_triangle(x2, y2 + noise_cutoff, x1)
      next
    }
    if (y2 == -noise_cutoff && y1 < -noise_cutoff) {
      adhesive_area <- adhesive_area + area_triangle(x1, y1 + noise_cutoff, x2)
      next
    }

    # Handle crossings through noise cutoff boundaries
    # NOTE: Area within the noise band (-noise_cutoff to +noise_cutoff) is NEVER included
    # Crossing from above noise_cutoff to below -noise_cutoff (or vice versa)
    if ((y1 > noise_cutoff && y2 < -noise_cutoff) || (y1 < -noise_cutoff && y2 > noise_cutoff)) {
      # Find crossing at positive noise cutoff
      x_pos <- x1 + (x2 - x1) * (noise_cutoff - y1) / (y2 - y1)
      # Find crossing at negative noise cutoff  
      x_neg <- x1 + (x2 - x1) * (-noise_cutoff - y1) / (y2 - y1)
      
      if (y1 > noise_cutoff) {
        # y1 positive, y2 negative - only count areas outside noise band
        repulsive_area <- repulsive_area + area_triangle(x1, y1 - noise_cutoff, x_pos)
        adhesive_area <- adhesive_area + area_triangle(x2, y2 + noise_cutoff, x_neg)
      } else {
        # y1 negative, y2 positive - only count areas outside noise band
        adhesive_area <- adhesive_area + area_triangle(x1, y1 + noise_cutoff, x_neg)
        repulsive_area <- repulsive_area + area_triangle(x2, y2 - noise_cutoff, x_pos)
      }
      next
    }

    # Crossing from above noise_cutoff to within noise band (only count area outside noise band)
    if (y1 > noise_cutoff && abs(y2) <= noise_cutoff) {
      x_cross <- x1 + (x2 - x1) * (noise_cutoff - y1) / (y2 - y1)
      repulsive_area <- repulsive_area + area_triangle(x1, y1 - noise_cutoff, x_cross)
      next
    }
    if (y2 > noise_cutoff && abs(y1) <= noise_cutoff) {
      x_cross <- x1 + (x2 - x1) * (noise_cutoff - y1) / (y2 - y1)
      repulsive_area <- repulsive_area + area_triangle(x2, y2 - noise_cutoff, x_cross)
      next
    }

    # Crossing from below -noise_cutoff to within noise band (only count area outside noise band)
    if (y1 < -noise_cutoff && abs(y2) <= noise_cutoff) {
      x_cross <- x1 + (x2 - x1) * (-noise_cutoff - y1) / (y2 - y1)
      adhesive_area <- adhesive_area + area_triangle(x1, y1 + noise_cutoff, x_cross)
      next
    }
    if (y2 < -noise_cutoff && abs(y1) <= noise_cutoff) {
      x_cross <- x1 + (x2 - x1) * (-noise_cutoff - y1) / (y2 - y1)
      adhesive_area <- adhesive_area + area_triangle(x2, y2 + noise_cutoff, x_cross)
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
#'   \item \code{adhesive_energy_aJ_<dir>} — total area where \code{y < -noise_cutoff}
#'   \item \code{repulsive_energy_aJ_<dir>} — total area where \code{y > noise_cutoff}
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
#' @param noise_cutoff Numeric. Noise threshold for energy calculation (default: 0). Only areas where |force| > noise_cutoff are included in energy calculation.
#'
#' @return The updated \code{fdObj} with two new metadata columns:
#' \code{adhesive_energy_aJ_<dir>} and \code{repulsive_energy_aJ_<dir>}.
#' @seealso analyze_a_curve_area, analyze_curves_adhesive_force
#' @export
analyze_curves_energy <- function(fdObj, useCurve = c("retract", "approach"), threads = 1, noise_cutoff = 0) {
  if (!inherits(fdObj, "fdObj"))
    stop("fdObj must be of class 'fdObj'")

  useCurve <- match.arg(useCurve)
  curve_list <- if (useCurve == "approach") fdObj@approachCurves else fdObj@retractCurves
  dir_tag <- useCurve

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
    analyze_a_curve_area(df, noise_cutoff = noise_cutoff)
  }

  res_list <- if (threads > 1) {
    old_plan <- future::plan()
    on.exit(future::plan(old_plan), add = TRUE)
    future::plan(future::multisession, workers = threads)
    future.apply::future_lapply(curve_names, run_one, 
                                 future.globals = list(
                                   run_one = run_one,
                                   curve_list = curve_list,
                                   noise_cutoff = noise_cutoff
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


