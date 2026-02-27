#' Plot raw deflection vs piezo distance from rawCurves slot of fdObj
#'
#' @param fdobj A `fdObj` object
#' @param curve Which segment(s) to plot: "approach", "retract", or "both"
#' @param group_curves_by Metadata column to color by (optional)
#' @param split_curves_by Metadata column to facet by (optional)
#' @param color_map Named vector of colors (optional)
#' @param point_size Size of points (default = 0.5)
#' @param alpha Transparency of points (default = 0.6)
#'
#' @return A ggplot2 object of the scatter plots showing raw deflection signal.
#' @export
#' @importFrom ggplot2 ggplot aes geom_point facet_wrap facet_grid labs theme_minimal scale_color_manual guides element_blank element_rect element_text

plot_deflection_curves <- function(fdobj,
                                   curve = c("both", "approach", "retract"),
                                   group_curves_by = NULL,
                                   split_curves_by = NULL,
                                   point_size = 0.5,
                                   alpha = 0.5,
                                   color_map = NULL) {
  curve <- match.arg(curve)
  meta <- fdobj@metadata
  curves <- fdobj@rawCurves

  # Validate metadata inputs
  if (!is.null(group_curves_by) && !group_curves_by %in% colnames(meta)) {
    stop("group_curves_by must be a column name in metadata")
  }
  if (!is.null(split_curves_by) && !split_curves_by %in% colnames(meta)) {
    stop("split_curves_by must be a column name in metadata")
  }

  # Collect data
  df_list <- lapply(names(curves), function(name) {
    df <- curves[[name]]
    if (nrow(df) == 0) return(NULL)

    segments <- list()
    if (curve %in% c("both", "approach") && all(c("Calc_Ramp_Ex_nm", "Defl_V_Ex") %in% colnames(df))) {
      segments$approach <- data.frame(
        distance = df$Calc_Ramp_Ex_nm,
        deflection = df$Defl_V_Ex,
        segment = "Approach"
      )
    }
    if (curve %in% c("both", "retract") && all(c("Calc_Ramp_Rt_nm", "Defl_V_Rt") %in% colnames(df))) {
      segments$retract <- data.frame(
        distance = df$Calc_Ramp_Rt_nm,
        deflection = df$Defl_V_Rt,
        segment = "Retract"
      )
    }

    if (length(segments) == 0) return(NULL)

    df_combined <- do.call(rbind, segments)
    df_combined$sample <- name

    if (!is.null(group_curves_by)) {
      df_combined[[group_curves_by]] <- meta[name, group_curves_by]
    }
    if (!is.null(split_curves_by)) {
      df_combined[[split_curves_by]] <- meta[name, split_curves_by]
    }

    df_combined
  })

  plot_df <- do.call(rbind, df_list)
  if (is.null(plot_df) || nrow(plot_df) == 0) return(NULL)

  # Base plot
  p <- ggplot(plot_df, aes(x = distance, y = deflection)) +
    labs(x = "Distance (nm)", y = "Deflection (V)")

  if (!is.null(group_curves_by)) {
    p <- p + geom_point(aes(color = .data[[group_curves_by]]),
                        size = point_size, alpha = alpha) +
      labs(color = group_curves_by)
  } else {
    p <- p + geom_point(size = point_size, alpha = alpha)
  }

  # Faceting
  if (!is.null(split_curves_by)) {
    if (curve == "both") {
      p <- p + facet_grid(as.formula(paste(split_curves_by, "~segment")))
    } else {
      p <- p + facet_wrap(as.formula(paste("~", split_curves_by)))
    }
  } else if (curve == "both") {
    p <- p + facet_wrap(~segment)
  }

  # Optional color map
  if (!is.null(color_map)) {
    p <- p + scale_color_manual(values = color_map)
  }

  # Theme
  p <- p + theme_minimal(base_size = 14) +
    theme(
      panel.grid = element_blank(),
      strip.background = element_rect(fill = "#f0f0f0"),
      strip.text = element_text(face = "bold")
    )

  n_samples <- length(fdobj@rawCurves)
  n_approach <- length(unique(plot_df$sample[plot_df$segment == "Approach"]))
  n_retract  <- length(unique(plot_df$sample[plot_df$segment == "Retract"]))

  message(sprintf(
    "%d samples in the input; %d approach and %d retract curves are plotted.",
    n_samples, n_approach, n_retract
  ))

  return(p)
}


#' Plot Force-Distance Curves from an fdObj
#'
#' Generates force-distance plots from the calibrated approach or retract curves in an \code{fdObj}.
#'
#' @param fdobj An object of class \code{fdObj}.
#' @param curve Character. Either "approach", "retract", or "both" (default = "both").
#' @param group_curves_by Metadata column to color by (optional).
#' @param split_curves_by Metadata column to facet by (optional).
#' @param color_map Named vector of colors (optional).
#' @param point_size Size of points (default = 0.5).
#' @param alpha Transparency of points (default = 0.6).
#'
#' @return A ggplot2 object showing the force-distance relationships from AFM curves.
#' @export
#' @importFrom ggplot2 ggplot aes geom_point facet_wrap facet_grid labs theme_minimal scale_color_manual guides
plot_fd_curves <- function(fdobj,
                           curve = "both",
                           group_curves_by = NULL,
                           split_curves_by = NULL,
                           color_map = NULL,
                           point_size = 0.5,
                           alpha = 0.6) {
  if (!inherits(fdobj, "fdObj")) stop("fdobj must be of class 'fdObj'")
  if (!curve %in% c("approach", "retract", "both")) {
    stop("curve must be one of 'approach', 'retract', or 'both'")
  }

  meta <- fdobj@metadata
  curve_names <- names(fdobj@rawCurves)

  # Validate metadata columns
  if (!is.null(group_curves_by) && !(group_curves_by %in% colnames(meta))) {
    stop("group_curves_by must be a column in metadata")
  }
  if (!is.null(split_curves_by) && !(split_curves_by %in% colnames(meta))) {
    stop("split_curves_by must be a column in metadata")
  }

  # Build plotting dataframe
  plot_df <- do.call(rbind, lapply(curve_names, function(name) {
    df_list <- list()

    if (curve %in% c("approach", "both")) {
      df <- fdobj@approachCurves[[name]]
      if (!is.null(df) && nrow(df) > 0) {
        df$segment <- "Approach"
        df$sample <- name
        colnames(df)[1:2] <- c("separation_distance_nm", "force_nN")
        df_list[["approach"]] <- df
      }
    }

    if (curve %in% c("retract", "both")) {
      df <- fdobj@retractCurves[[name]]
      if (!is.null(df) && nrow(df) > 0) {
        df$segment <- "Retract"
        df$sample <- name
        colnames(df)[1:2] <- c("separation_distance_nm", "force_nN")
        df_list[["retract"]] <- df
      }
    }

    df_combined <- do.call(rbind, df_list)
    if (is.null(df_combined) || nrow(df_combined) == 0) {
      return(NULL)
    } else {
      if (!is.null(group_curves_by)) {
        df_combined[[group_curves_by]] <- meta[name, group_curves_by]
      }
      if (!is.null(split_curves_by)) {
        df_combined[[split_curves_by]] <- meta[name, split_curves_by]
      }
    }

    return(df_combined)
  }))

  if (is.null(plot_df) || nrow(plot_df) == 0) {
    stop("No force-distance data available for plotting.")
  }

  # Base plot
  if (!is.null(group_curves_by)) {
    p <- ggplot(plot_df, aes(x = separation_distance_nm, y = force_nN)) +
      geom_point(aes(color = .data[[group_curves_by]]), size = point_size, alpha = alpha) +
      labs(x = "Separation Distance (nm)", y = "Force (nN)", color = group_curves_by)
  } else {
    p <- ggplot(plot_df, aes(x = separation_distance_nm, y = force_nN)) +
      geom_point(size = point_size, alpha = alpha) +
      labs(x = "Separation Distance (nm)", y = "Force (nN)")
  }

  # Facet options
  if (!is.null(split_curves_by)) {
    if (curve == "both") {
      p <- p + facet_grid(as.formula(paste(split_curves_by, "~segment")))
    } else {
      p <- p + facet_wrap(as.formula(paste("~", split_curves_by)))
    }
  } else if (curve == "both") {
    p <- p + facet_wrap(~segment)
  }

  # Optional color map
  if (!is.null(color_map)) {
    p <- p + scale_color_manual(values = color_map)
  }

  # Final theming
  p <- p + theme_minimal(base_size = 14) +
    theme(
      panel.grid = element_blank(),
      strip.background = element_rect(fill = "#f0f0f0"),
      strip.text = element_text(face = "bold")
    )

  # count and print the number of curves expected and to be printed
  n_samples <- length(fdobj@rawCurves)
  n_approach <- length(unique(plot_df$sample[plot_df$segment == "Approach"]))
  n_retract  <- length(unique(plot_df$sample[plot_df$segment == "Retract"]))

  message(sprintf(
    "%d samples in the input; %d approach and %d retract curves are plotted.",
    n_samples, n_approach, n_retract
  ))
  return(p)

}

#' Calculate the trapezoid area between two points
#'
#' Computes the absolute area of a trapezoid formed by two points
#' \code{(x1, y1)} and \code{(x2, y2)} along the x-axis. This is used when
#' both points are on the same side of the x-axis (either both positive or both negative).
#'
#' @param x1 Numeric. The x-coordinate of the first point.
#' @param y1 Numeric. The y-coordinate of the first point.
#' @param x2 Numeric. The x-coordinate of the second point.
#' @param y2 Numeric. The y-coordinate of the second point.
#'
#' @return A single numeric value representing the positive trapezoid area
#' between the two points.
#'
#' @examples
#' area_trapezoid(0, 2, 1, 4)   # 0.5 * (1 - 0) * (2 + 4) = 3
#' area_trapezoid(0, -2, 1, -4) # same absolute area = 3
#'
#' @seealso [area_triangle()], [crossing_x0()], [calculate_area()]
#' @export
area_trapezoid <- function(x1, y1, x2, y2) {
  0.5 * abs(x2 - x1) * abs(y1 + y2)
}

#' Calculate the triangle area between a curve point and the x-axis
#'
#' Computes the absolute area of a triangle formed by a point \code{(x1, y1)}
#' on the curve and its intersection with the x-axis at \code{(x0, 0)}.
#' This function assumes one vertex lies on the x-axis (\code{y = 0}).
#'
#' @param x1 Numeric. The x-coordinate of the curve point.
#' @param y1 Numeric. The y-coordinate of the curve point.
#' @param x0 Numeric. The x-coordinate of the x-axis intersection point (where \code{y = 0}).
#'
#' @return A single numeric value representing the positive triangle area.
#'
#' @examples
#' # Triangle between (0, 4) and (1, 0)
#' area_triangle(0, 4, 1)  # 0.5 * |1 - 0| * |4| = 2
#'
#' @seealso [area_trapezoid()], [crossing_x0()], [calculate_area()]
#' @export
area_triangle <- function(x1, y1, x0) {
  0.5 * abs(x0 - x1) * abs(y1)
}

#' Interpolate the x-intercept (x0) where y = 0
#'
#' Performs linear interpolation between two points \code{(x1, y1)} and
#' \code{(x2, y2)} to find the x-coordinate (\code{x0}) where the line
#' crosses the x-axis (\code{y = 0}). Only valid when \code{y1} and \code{y2}
#' have opposite signs.
#'
#' @param x1 Numeric. The x-coordinate of the first point.
#' @param y1 Numeric. The y-coordinate of the first point.
#' @param x2 Numeric. The x-coordinate of the second point.
#' @param y2 Numeric. The y-coordinate of the second point.
#'
#' @return A single numeric value for the interpolated \code{x0} coordinate.
#' Returns \code{NA} if no valid sign change exists between \code{y1} and \code{y2}.
#'
#' @examples
#' crossing_x0(0, 4, 2, -2)  # 1.333 — line crosses y=0 between x=0 and x=2
#' crossing_x0(0, 4, 2, 6)   # NA — no sign change
#'
#' @seealso [area_trapezoid()], [area_triangle()], [calculate_area()]
#' @export
crossing_x0 <- function(x1, y1, x2, y2) {
  if ((y1 > 0 && y2 < 0) || (y1 < 0 && y2 > 0)) {
    x1 + (-y1) * (x2 - x1) / (y2 - y1)
  } else {
    NA_real_
  }
}

#' Plot a transformed force curve with energy regions highlighted
#'
#' Creates a ggplot visualization of a force-distance curve with colored
#' regions indicating adhesive (blue) and repulsive (coral) energy areas
#' based on a noise cutoff threshold. Internally calls \code{analyze_a_curve_area()}
#' to calculate the energy values displayed in the plot subtitle.
#'
#' @param curve_df A data frame containing the transformed force curve with
#'   columns \code{separation_distance_nm} and \code{force_nN}.
#' @param noise_cutoff Numeric. The force threshold (in nanoNewtons) for
#'   separating signal from noise. Adhesive and repulsive regions are those
#'   exceeding this threshold in magnitude. Default is 0.5.
#' @param title Character. Title for the plot. Default is
#'   "Interaction Energy Calculation with Noise Threshold".
#' @param base_size Numeric. Base font size for the plot. Default is 14.
#' @param point_size Numeric. Size of points on the curve. Default is 2.
#' @param alpha_ribbon Numeric. Transparency of the colored ribbon areas (0-1).
#'   Default is 0.3.
#' @param show_legend Logical. Whether to show the legend. Default is TRUE.
#'
#' @return A ggplot2 object showing the force curve with colored regions
#'   and energy values in the subtitle.
#'
#' @details
#' The function:
#' \enumerate{
#'   \item Calls \code{analyze_a_curve_area()} with the provided \code{noise_cutoff}
#'   \item Creates a region classification (Baseline, Adhesive, Repulsive)
#'   \item Visualizes the noise threshold as a grey band
#'   \item Colors adhesive regions (force < -noise_cutoff) in steelblue
#'   \item Colors repulsive regions (force > noise_cutoff) in coral
#'   \item Displays calculated energy values in the plot subtitle
#' }
#'
#' @examples
#' \dontrun{
#' # Load example data
#' data(curvana_sample)
#' curve <- curvana_sample@retractCurves[[1]]
#' curve_transformed <- transform_a_curve(curve)
#' 
#' # Plot with default noise cutoff (0.5 nN)
#' plot_force_curve_energy(curve_transformed)
#' 
#' # Plot with larger noise cutoff (5 nN)
#' plot_force_curve_energy(curve_transformed, noise_cutoff = 5)
#' 
#' # Customize appearance
#' plot_force_curve_energy(curve_transformed, noise_cutoff = 1.0, 
#'                        title = "My Custom Title", base_size = 12)
#' }
#'
#' @seealso [analyze_a_curve_area()], [transform_a_curve()]
#' @export
#' @importFrom ggplot2 ggplot aes geom_ribbon geom_hline geom_point geom_path
#' @importFrom ggplot2 scale_color_manual labs annotate theme_minimal theme element_blank element_rect element_text
#'
plot_force_curve_energy <- function(curve_df,
                                     noise_cutoff = 0.5,
                                     title = "Interaction Energy Calculation with Noise Threshold",
                                     base_size = 14,
                                     point_size = 2,
                                     alpha_ribbon = 0.3,
                                     show_legend = TRUE) {
  
  # Calculate energy using the internal function
  energy_result <- analyze_a_curve_area(curve_df, noise_cutoff = noise_cutoff)
  
  plot_data <- curve_df
  # Create region classifications
  plot_data$region <- "Baseline"
  plot_data$region[plot_data$force_nN > noise_cutoff] <- "Repulsive"
  plot_data$region[plot_data$force_nN < -noise_cutoff] <- "Adhesive"
  
  # Separate data for ribbon layers
  repulsive_data <- plot_data[plot_data$force_nN > noise_cutoff, ]
  adhesive_data <- plot_data[plot_data$force_nN < -noise_cutoff, ]
  
  # Create the plot
  p <- ggplot(plot_data, aes(x = separation_distance_nm, y = force_nN)) +
    # Grey background for noise band
    annotate("rect", xmin = -Inf, xmax = Inf, 
             ymin = -noise_cutoff, ymax = noise_cutoff,
             alpha = 0.08, fill = "grey50") +
    # Shaded regions for energy calculation
    geom_ribbon(data = repulsive_data, 
                aes(ymin = noise_cutoff, ymax = force_nN),
                fill = "coral", alpha = alpha_ribbon, color = NA) +
    geom_ribbon(data = adhesive_data, 
                aes(ymin = force_nN, ymax = -noise_cutoff),
                fill = "steelblue", alpha = alpha_ribbon, color = NA) +
    # Threshold lines
    geom_hline(yintercept = noise_cutoff, color = "red", 
               linetype = "dashed", linewidth = 0.7) +
    geom_hline(yintercept = -noise_cutoff, color = "red", 
               linetype = "dashed", linewidth = 0.7) +
    geom_hline(yintercept = 0, color = "black", linewidth = 0.5) +
    # Curve rendering
    geom_point(aes(color = region), size = point_size, alpha = 0.8) +
    geom_path(aes(color = region), linewidth = 0.6, alpha = 0.8) +
    # Color mapping
    scale_color_manual(
      values = c("Adhesive" = "steelblue", "Repulsive" = "coral", "Baseline" = "grey70"),
      name = "Region"
    ) +
    # Labels and title
    labs(
      x = "Separation distance (nm)",
      y = "Force (nN)",
      title = title,
      subtitle = sprintf("Adhesive energy = %.3g aJ\nRepulsive energy = %.3g aJ",
             energy_result["adhesive_area"],
             energy_result["repulsive_area"])
    ) +
    # Annotation for noise cutoff value
    annotate("text", x = Inf, y = noise_cutoff,
             label = sprintf("Noise cutoff = %.3g nN", noise_cutoff),
             hjust = 1.1, vjust = -0.5, size = 3.5, color = "red") +
    # Theme
    theme_minimal(base_size = base_size) +
    theme(legend.position = if (show_legend) "bottom" else "none")
  
  return(p)
}


#' Plot a transformed force curve with interaction distance annotations
#'
#' Creates a ggplot visualization of a transformed force-distance curve and
#' annotates the interaction threshold and detected interaction distance.
#' Internally calls \code{analyze_a_curve_interaction_distance()}.
#'
#' @param curve_df A data frame containing the transformed force curve with
#'   columns \code{separation_distance_nm} and \code{force_nN}.
#' @param baseline_span Either a single integer >= 1, or the string
#'   \code{"automatic"}. Passed to
#'   \code{analyze_a_curve_interaction_distance()}.
#' @param y_direction Character. \code{"negative"} (rupture) or
#'   \code{"positive"} (repulsive). Passed to
#'   \code{analyze_a_curve_interaction_distance()}.
#' @param x_direction Character. \code{"left"} or \code{"right"}. Direction
#'   to scan from baseline. Passed to
#'   \code{analyze_a_curve_interaction_distance()}.
#' @param threshold_method Threshold method passed to
#'   \code{analyze_a_curve_interaction_distance()}.
#' @param multiplier Numeric. Threshold multiplier for spread-based methods.
#' @param mad_constant Numeric. MAD scaling constant.
#' @param quantile_low Numeric lower quantile used by
#'   \code{threshold_method = "quantile"}.
#' @param quantile_high Numeric upper quantile used by
#'   \code{threshold_method = "quantile"}.
#' @param fixed_low Numeric lower threshold (nN) when
#'   \code{threshold_method = "fixed"}.
#' @param fixed_high Numeric upper threshold (nN) when
#'   \code{threshold_method = "fixed"}.
#' @param title Character. Plot title.
#' @param base_size Numeric. Base font size for the plot.
#' @param line_color Character. Color of the curve line.
#' @param line_width Numeric. Width of the curve line.
#' @param point_size Numeric. Size of the highlighted interaction point.
#' @param show_positive_threshold Logical. If \code{TRUE}, draw a dotted line at
#'   \code{+threshold} as reference.
#'
#' @return A ggplot2 object with threshold and interaction-distance annotations.
#' @export
#' @importFrom ggplot2 ggplot aes geom_path geom_hline geom_vline geom_point
#' @importFrom ggplot2 annotate labs theme_minimal
plot_force_curve_interaction_distance <- function(
    curve_df,
    baseline_span,
    y_direction = c("negative", "positive"),
    x_direction = c("left", "right"),
    threshold_method = c("sd", "mad", "quantile", "fixed"),
    multiplier = 3,
    mad_constant = 1.4826,
    quantile_low = 0.25,
    quantile_high = 0.75,
    fixed_low = NULL,
    fixed_high = NULL,
    title = "Single-Curve Interaction Distance",
    base_size = 14,
    line_color = "grey40",
    line_width = 0.7,
    point_size = 2,
    show_positive_threshold = TRUE
) {
  y_direction <- match.arg(y_direction)
  x_direction <- match.arg(x_direction)
  threshold_method <- match.arg(threshold_method)

  result <- analyze_a_curve_interaction_distance(
    curve_df = curve_df,
    baseline_span = baseline_span,
    y_direction = y_direction,
    x_direction = x_direction,
    threshold_method = threshold_method,
    multiplier = multiplier,
    mad_constant = mad_constant,
    quantile_low = quantile_low,
    quantile_high = quantile_high,
    fixed_low = fixed_low,
    fixed_high = fixed_high
  )

  distance_nm <- unname(result["distance"])
  threshold_nN <- unname(result["threshold"])
  threshold_line <- if (y_direction == "negative") -threshold_nN else threshold_nN

  p <- ggplot(curve_df, aes(x = separation_distance_nm, y = force_nN)) +
    geom_path(color = line_color, linewidth = line_width) +
    geom_hline(yintercept = 0, color = "black", linewidth = 0.5) +
    geom_hline(yintercept = threshold_line, color = "red", linetype = "dashed", linewidth = 0.7) +
    labs(
      x = "Separation distance (nm)",
      y = "Force (nN)",
      title = title,
      subtitle = ifelse(
        is.na(distance_nm),
        sprintf("Threshold = %s nN | No threshold crossing detected", as.character(threshold_nN)),
        sprintf("Interaction distance = %s nm | Threshold = %s nN", as.character(distance_nm), as.character(threshold_nN))
      )
    ) +
    theme_minimal(base_size = base_size) +
    annotate(
      "text",
      x = Inf,
      y = threshold_line,
      label = sprintf("Threshold = %.3g nN", threshold_nN),
      hjust = 1.05,
      vjust = -0.5,
      color = "red",
      size = 4
    )

  if (isTRUE(show_positive_threshold)) {
    p <- p + geom_hline(yintercept = abs(threshold_nN), color = "red", linetype = "dotted", linewidth = 0.5, alpha = 0.6)
  }

  if (!is.na(distance_nm)) {
    p <- p +
      geom_vline(xintercept = distance_nm, color = "dodgerblue4", linetype = "dashed", linewidth = 0.7) +
      geom_point(
        data = data.frame(separation_distance_nm = distance_nm, force_nN = threshold_line),
        aes(x = separation_distance_nm, y = force_nN),
        color = "dodgerblue4",
        size = point_size,
        inherit.aes = FALSE
      ) +
      annotate(
        "text",
        x = distance_nm,
        y = min(curve_df$force_nN, na.rm = TRUE),
        label = sprintf("Distance = %.3g nm", distance_nm),
        vjust = -0.5,
        color = "dodgerblue4",
        size = 4
      )
  }

  return(p)
}


#' Plot one transformed curve with selectable metric annotations
#'
#' High-level visualization helper that takes an \code{fdObj} and one curve name,
#' then overlays selected analytical annotations (energy regions, adhesive force,
#' and interaction distance/threshold) on the force-distance curve.
#'
#' @param fdobj An object of class \code{fdObj}.
#' @param curve_name Character. Name of one curve to plot. Must match
#'   \code{rownames(fdobj@metadata)}.
#' @param useCurve Character. Either \code{"retract"} or \code{"approach"}.
#' @param repulsive_energy Logical. If \code{TRUE}, shade repulsive-energy region
#'   (force > noise cutoff).
#' @param adhesive_energy Logical. If \code{TRUE}, shade adhesive-energy region
#'   (force < -noise cutoff).
#' @param annotate_adhesive_force Logical. If \code{TRUE}, mark adhesive force
#'   point and annotate force + separation distance.
#' @param annotate_interaction_distance Logical. If \code{TRUE}, annotate one
#'   interaction distance and threshold.
#' @param noise_cutoff Numeric scalar or metadata column name (character).
#'   If character, the function looks up \code{fdobj@metadata[curve_name, noise_cutoff]}.
#' @param interaction_type Character. \code{"rupture"}, \code{"repulsive"}, or
#'   \code{"auto"}. Controls default interaction-distance column names and
#'   threshold sign. When \code{"auto"}, rupture columns are tried first, then
#'   repulsive columns.
#' @param interaction_distance_col Optional metadata column name containing the
#'   interaction distance to annotate for this curve.
#' @param interaction_threshold_col Optional metadata column name containing the
#'   interaction threshold (nN) to annotate for this curve.
#' @param title Character. Plot title.
#' @param base_size Numeric. Base theme text size.
#' @param annotation_text_size Numeric. Text size used for annotation labels
#'   (adhesive force, interaction threshold, interaction distance).
#' @param interaction_label_color_rupture Character. Text color for rupture
#'   interaction label box.
#' @param interaction_label_color_repulsive Character. Text color for repulsive
#'   interaction label box.
#' @param interaction_label_fill Character. Fill color for interaction label box.
#' @param ... Additional ggplot2 components to add to the plot (e.g.,
#'   \code{theme()}, \code{scale_*()}, \code{coord_*()}, \code{labs()},
#'   or extra \code{geom_*()} layers).
#'
#' @return A ggplot2 object.
#' @export
#' @importFrom ggplot2 ggplot aes geom_path geom_hline geom_ribbon geom_point
#' @importFrom ggplot2 geom_segment geom_vline annotate labs theme_minimal
plot_curve_metrics <- function(
    fdobj,
    curve_name,
    useCurve = c("retract", "approach"),
    repulsive_energy = TRUE,
    adhesive_energy = TRUE,
    annotate_adhesive_force = TRUE,
    annotate_interaction_distance = TRUE,
    noise_cutoff = 0.5,
    interaction_type = c("rupture", "repulsive", "auto"),
  adhesive_energy_col = NULL,
  repulsive_energy_col = NULL,
  adhesive_force_col = NULL,
  adhesive_sep_col = NULL,
    interaction_distance_col = NULL,
    interaction_threshold_col = NULL,
    title = NULL,
    base_size = 14,
    annotation_text_size = 3.3,
    interaction_label_color_rupture = "purple4",
    interaction_label_color_repulsive = "darkorange3",
    interaction_label_fill = "white",
    ...
) {
  if (!inherits(fdobj, "fdObj")) {
    stop("fdobj must be an object of class 'fdObj'.")
  }

  useCurve <- match.arg(useCurve)
  interaction_type <- match.arg(interaction_type)

  metadata <- fdobj@metadata
  if (!(curve_name %in% rownames(metadata))) {
    stop("curve_name must match one rowname in fdobj@metadata.")
  }

  curve_list <- if (useCurve == "retract") fdobj@retractCurves else fdobj@approachCurves
  if (!(curve_name %in% names(curve_list))) {
    stop("curve_name not found in the selected curve slot.")
  }

  curve_df <- curve_list[[curve_name]]
  if (is.null(curve_df) || nrow(curve_df) == 0) {
    stop("Selected transformed curve is empty.")
  }
  if (!all(c("separation_distance_nm", "force_nN") %in% colnames(curve_df))) {
    stop("Selected curve must contain columns 'separation_distance_nm' and 'force_nN'.")
  }

  resolve_numeric_or_column <- function(value_or_col, param_name) {
    if (is.numeric(value_or_col) && length(value_or_col) == 1) {
      return(as.numeric(value_or_col))
    }

    if (is.character(value_or_col) && length(value_or_col) == 1) {
      if (value_or_col %in% colnames(metadata)) {
        value <- metadata[curve_name, value_or_col]
        value <- suppressWarnings(as.numeric(value))
        if (is.na(value)) {
          stop(sprintf("Metadata value for '%s' in column '%s' is NA/non-numeric.", curve_name, value_or_col))
        }
        return(value)
      }

      numeric_try <- suppressWarnings(as.numeric(value_or_col))
      if (!is.na(numeric_try)) {
        return(numeric_try)
      }
    }

    stop(sprintf("%s must be a numeric scalar or a metadata column name.", param_name))
  }

  resolve_metadata_value <- function(col_name) {
    if (is.null(col_name)) return(NA_real_)
    if (!(col_name %in% colnames(metadata))) return(NA_real_)
    suppressWarnings(as.numeric(metadata[curve_name, col_name]))
  }

  noise_value <- resolve_numeric_or_column(noise_cutoff, "noise_cutoff")

  curve_df$region <- "Baseline"
  curve_df$region[curve_df$force_nN > noise_value] <- "Repulsive"
  curve_df$region[curve_df$force_nN < -noise_value] <- "Adhesive"

  x_min <- min(curve_df$separation_distance_nm, na.rm = TRUE)
  x_max <- max(curve_df$separation_distance_nm, na.rm = TRUE)
  x_span <- x_max - x_min
  if (!is.finite(x_span) || x_span <= 0) x_span <- 1
  y_min <- min(curve_df$force_nN, na.rm = TRUE)
  y_max <- max(curve_df$force_nN, na.rm = TRUE)
  y_span <- y_max - y_min
  if (!is.finite(y_span) || y_span <= 0) y_span <- 1

  label_x_right <- x_max + 0.25 * x_span
  x_offset <- 0.06 * x_span
  y_offset <- 0.08 * y_span

  if (is.null(title)) {
    title <- sprintf("Curve metrics: %s (%s)", curve_name, useCurve)
  }

  default_adhesive_energy_col <- paste0("adhesive_energy_aJ_", useCurve)
  default_repulsive_energy_col <- paste0("repulsive_energy_aJ_", useCurve)
  default_adhesive_force_col <- paste0("adhesive_force_nN_", useCurve)
  default_adhesive_sep_col <- paste0("adhesive_sep_nm_", useCurve)

  adhesive_energy_col <- if (is.null(adhesive_energy_col)) default_adhesive_energy_col else adhesive_energy_col
  repulsive_energy_col <- if (is.null(repulsive_energy_col)) default_repulsive_energy_col else repulsive_energy_col
  adhesive_force_col <- if (is.null(adhesive_force_col)) default_adhesive_force_col else adhesive_force_col
  adhesive_sep_col <- if (is.null(adhesive_sep_col)) default_adhesive_sep_col else adhesive_sep_col

  adhesive_energy_val <- resolve_metadata_value(adhesive_energy_col)
  repulsive_energy_val <- resolve_metadata_value(repulsive_energy_col)
  adhesive_force_val <- resolve_metadata_value(adhesive_force_col)
  adhesive_sep_val <- resolve_metadata_value(adhesive_sep_col)

  p <- ggplot(curve_df, aes(x = separation_distance_nm, y = force_nN)) +
    annotate("rect", xmin = -Inf, xmax = Inf, ymin = -noise_value, ymax = noise_value,
             alpha = 0.08, fill = "grey50") +
    geom_hline(yintercept = 0, color = "black", linewidth = 0.5) +
    geom_path(aes(color = region), linewidth = 0.7, alpha = 0.9) +
    geom_point(aes(color = region), size = 1.8, alpha = 0.8) +
    labs(
      x = "Separation distance (nm)",
      y = "Force (nN)",
      title = title
    ) +
    theme_minimal(base_size = base_size) +
    coord_cartesian(xlim = c(x_min, label_x_right), clip = "off") +
    theme(plot.margin = ggplot2::margin(5.5, 80, 5.5, 5.5))

  if (isTRUE(repulsive_energy) || isTRUE(adhesive_energy)) {
    if (isTRUE(repulsive_energy)) {
      repulsive_data <- curve_df[curve_df$force_nN > noise_value, ]
      p <- p + geom_ribbon(
        data = repulsive_data,
        aes(ymin = noise_value, ymax = force_nN),
        fill = "coral",
        alpha = 0.3,
        color = NA
      )
    }

    if (isTRUE(adhesive_energy)) {
      adhesive_data <- curve_df[curve_df$force_nN < -noise_value, ]
      p <- p + geom_ribbon(
        data = adhesive_data,
        aes(ymin = force_nN, ymax = -noise_value),
        fill = "steelblue",
        alpha = 0.3,
        color = NA
      )
    }

    subtitle_parts <- character(0)
    if (isTRUE(adhesive_energy) && !is.na(adhesive_energy_val)) {
      subtitle_parts <- c(subtitle_parts, sprintf("Adhesive energy = %.3g", adhesive_energy_val))
    }
    if (isTRUE(repulsive_energy) && !is.na(repulsive_energy_val)) {
      subtitle_parts <- c(subtitle_parts, sprintf("Repulsive energy = %.3g", repulsive_energy_val))
    }
    if (length(subtitle_parts) > 0) {
      p <- p + labs(subtitle = paste(subtitle_parts, collapse = " | "))
    }
  }

  if (isTRUE(annotate_adhesive_force)) {
    if (!is.na(adhesive_force_val) && !is.na(adhesive_sep_val)) {
      adhesive_label_x <- adhesive_sep_val
      adhesive_label_y <- adhesive_force_val - y_offset

      p <- p +
        geom_point(
          data = data.frame(separation_distance_nm = adhesive_sep_val, force_nN = adhesive_force_val),
          aes(x = separation_distance_nm, y = force_nN),
          color = "red",
          size = 3,
          inherit.aes = FALSE
        ) +
        geom_segment(
          data = data.frame(
            x = adhesive_sep_val,
            y = adhesive_force_val,
            xend = adhesive_label_x,
            yend = adhesive_label_y
          ),
          aes(x = x, y = y, xend = xend, yend = yend),
          inherit.aes = FALSE,
          color = "red",
          linewidth = 0.4
        ) +
        annotate(
          "label",
          x = adhesive_label_x,
          y = adhesive_label_y,
          label = sprintf("Adhesive force = %.3g nN\nSeparation = %.3g nm", adhesive_force_val, adhesive_sep_val),
          color = "red",
          fill = "white",
          size = annotation_text_size
        )
    }
  }

  if (isTRUE(annotate_interaction_distance)) {
    preferred_types <- if (interaction_type == "auto") c("rupture", "repulsive") else interaction_type

    interaction_distance <- NA_real_
    interaction_threshold <- NA_real_
    active_type <- preferred_types[1]

    if (!is.null(interaction_distance_col) || !is.null(interaction_threshold_col)) {
      distance_col_to_use <- if (is.null(interaction_distance_col)) paste0(active_type, "_distance_nm_", useCurve) else interaction_distance_col
      threshold_col_to_use <- if (is.null(interaction_threshold_col)) paste0(active_type, "_threshold_nN_", useCurve) else interaction_threshold_col
      interaction_distance <- resolve_metadata_value(distance_col_to_use)
      interaction_threshold <- resolve_metadata_value(threshold_col_to_use)
    } else {
      for (tp in preferred_types) {
        distance_candidate <- resolve_metadata_value(paste0(tp, "_distance_nm_", useCurve))
        threshold_candidate <- resolve_metadata_value(paste0(tp, "_threshold_nN_", useCurve))
        if (!is.na(distance_candidate) || !is.na(threshold_candidate)) {
          interaction_distance <- distance_candidate
          interaction_threshold <- threshold_candidate
          active_type <- tp
          break
        }
      }
    }

    if (!is.na(interaction_distance)) {
      p <- p + geom_vline(
        xintercept = interaction_distance,
        color = "dodgerblue4",
        linetype = "dashed",
        linewidth = 0.7
      )
    }

    type_label <- if (active_type == "rupture") "Rupture" else "Repulsive"
    interaction_label_color <- if (active_type == "rupture") interaction_label_color_rupture else interaction_label_color_repulsive
    interaction_y <- if (!is.na(interaction_threshold)) {
      if (active_type == "rupture") -abs(interaction_threshold) else abs(interaction_threshold)
    } else {
      NA_real_
    }

    if (!is.na(interaction_threshold)) {
      p <- p + geom_hline(
        yintercept = interaction_y,
        color = "purple4",
        linetype = "dashed",
        linewidth = 0.7
      )
    }

    if (!is.na(interaction_distance) && !is.na(interaction_threshold)) {
      p <- p + annotate(
        "point",
        x = interaction_distance,
        y = interaction_y,
        color = "dodgerblue4",
        size = 2
      )
    }

    if (!is.na(interaction_distance) || !is.na(interaction_threshold)) {
      interaction_label <- c()
      if (!is.na(interaction_distance)) {
        interaction_label <- c(interaction_label, sprintf("%s distance = %.3g nm", type_label, interaction_distance))
      }
      if (!is.na(interaction_threshold)) {
        interaction_label <- c(interaction_label, sprintf("%s threshold = %.3g nN", type_label, interaction_threshold))
      }

      point_x <- if (!is.na(interaction_distance)) interaction_distance else min(curve_df$separation_distance_nm, na.rm = TRUE)
      point_y <- if (!is.na(interaction_y)) interaction_y else min(curve_df$force_nN, na.rm = TRUE)

      label_x <- point_x + x_offset
      label_y <- point_y + y_offset
      if (active_type == "rupture") {
        label_y <- max(label_y, 0 + 0.05 * y_span)
      }

      p <- p +
        geom_segment(
          data = data.frame(x = point_x, y = point_y, xend = label_x, yend = label_y),
          aes(x = x, y = y, xend = xend, yend = yend),
          inherit.aes = FALSE,
          color = interaction_label_color,
          linewidth = 0.4
        ) +
        annotate(
          "label",
          x = label_x,
          y = label_y,
          label = paste(interaction_label, collapse = "\n"),
          color = interaction_label_color,
          fill = interaction_label_fill,
          hjust = 0,
          size = annotation_text_size
        )
    }
  }

  extra_layers <- list(...)
  if (length(extra_layers) > 0) {
    p <- p + extra_layers
  }

  return(p)
}

