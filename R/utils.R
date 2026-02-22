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
  
  # Prepare data for plotting (sort by separation distance)
  plot_data <- curve_df[order(curve_df$separation_distance_nm), ]
  
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
      subtitle = sprintf("Adhesive energy = %.2f aJ | Repulsive energy = %.2f aJ",
                         energy_result["adhesive_area"],
                         energy_result["repulsive_area"])
    ) +
    # Annotation for noise cutoff value
    annotate("text", x = Inf, y = noise_cutoff,
             label = sprintf("Noise cutoff = %.1f nN", noise_cutoff),
             hjust = 1.1, vjust = -0.5, size = 3.5, color = "red") +
    # Theme
    theme_minimal(base_size = base_size) +
    theme(legend.position = if (show_legend) "bottom" else "none")
  
  return(p)
}

