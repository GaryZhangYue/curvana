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
#' @param point_alpha Transparency of points (default = 0.6).
#' @param line_alpha Transparency of curve lines (default = 0.5).
#' @param xlim Optional numeric vector of length 2 for x-axis limits.
#' @param ylim Optional numeric vector of length 2 for y-axis limits.
#' @param ... Additional ggplot2 layers/settings to add to the plot
#'   (e.g., \code{theme(...)}, \code{scale_*()}, \code{labs(...)}).
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
                           point_alpha = 0.6,
                           line_alpha = 0.5,
                           xlim = NULL,
                           ylim = NULL,
                           ...) {
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
  if (!is.null(color_map)) {
    if (is.list(color_map)) {
      color_map <- unlist(color_map, use.names = TRUE)
    }
    if (!is.atomic(color_map) || is.null(names(color_map))) {
      stop("color_map must be a named atomic vector (e.g., c('1' = 'darkred', '2' = 'orange')).")
    }
    color_map <- as.character(color_map)
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
      geom_line(
        aes(color = .data[[group_curves_by]], group = interaction(sample, segment)),
        alpha = line_alpha,
        linewidth = 0.35,
        show.legend = FALSE
      ) +
      geom_point(aes(color = .data[[group_curves_by]]), size = point_size, alpha = point_alpha) +
      labs(x = "Separation Distance (nm)", y = "Force (nN)", color = group_curves_by)
  } else {
    p <- ggplot(plot_df, aes(x = separation_distance_nm, y = force_nN)) +
      geom_line(
        aes(group = interaction(sample, segment)),
        alpha = line_alpha,
        linewidth = 0.35,
        color = "grey50"
      ) +
      geom_point(size = point_size, alpha = point_alpha) +
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

  if (!is.null(xlim) || !is.null(ylim)) {
    if (!is.null(xlim) && (!is.numeric(xlim) || length(xlim) != 2)) {
      stop("xlim must be NULL or a numeric vector of length 2.")
    }
    if (!is.null(ylim) && (!is.numeric(ylim) || length(ylim) != 2)) {
      stop("ylim must be NULL or a numeric vector of length 2.")
    }
    p <- p + ggplot2::coord_cartesian(xlim = xlim, ylim = ylim)
  }

  extra_layers <- list(...)
  if (length(extra_layers) > 0) {
    p <- p + extra_layers
  }

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
#' @param noiseBand_low Numeric lower noise-band threshold (nN).
#'   Adhesive region is where \code{force_nN < noiseBand_low}.
#' @param noiseBand_high Numeric upper noise-band threshold (nN).
#'   Repulsive region is where \code{force_nN > noiseBand_high}.
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
#'   \item Calls \code{analyze_a_curve_area()} with \code{noiseBand_low} and \code{noiseBand_high}
#'   \item Creates a region classification (Baseline, Adhesive, Repulsive)
#'   \item Visualizes the noise band as a grey region
#'   \item Colors adhesive regions (force < noiseBand_low) in steelblue
#'   \item Colors repulsive regions (force > noiseBand_high) in coral
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
#' plot_force_curve_energy(curve_transformed, noiseBand_low = -0.5, noiseBand_high = 0.5)
#' 
#' # Plot with larger noise band
#' plot_force_curve_energy(curve_transformed, noiseBand_low = -5, noiseBand_high = 5)
#' 
#' # Customize appearance
#' plot_force_curve_energy(curve_transformed, noiseBand_low = -1.0, noiseBand_high = 1.0,
#'                        title = "My Custom Title", base_size = 12)
#' }
#'
#' @seealso [analyze_a_curve_area()], [transform_a_curve()]
#' @export
#' @importFrom ggplot2 ggplot aes geom_ribbon geom_hline geom_point geom_path
#' @importFrom ggplot2 scale_color_manual labs annotate theme_minimal theme element_blank element_rect element_text
#'
plot_force_curve_energy <- function(curve_df,
                                     noiseBand_low = -0.5,
                                     noiseBand_high = 0.5,
                                     title = "Interaction Energy Calculation with Noise Threshold",
                                     base_size = 14,
                                     point_size = 2,
                                     alpha_ribbon = 0.3,
                                     show_legend = TRUE) {
  if (!is.numeric(noiseBand_low) || length(noiseBand_low) != 1 || is.na(noiseBand_low) || !(noiseBand_low < 0)) {
    stop("noiseBand_low must be a single numeric value < 0.")
  }
  if (!is.numeric(noiseBand_high) || length(noiseBand_high) != 1 || is.na(noiseBand_high) || !(noiseBand_high > 0)) {
    stop("noiseBand_high must be a single numeric value > 0.")
  }
  
  # Calculate energy using the internal function
  energy_result <- analyze_a_curve_area(
    curve_df,
    noiseBand_low = noiseBand_low,
    noiseBand_high = noiseBand_high
  )
  
  plot_data <- curve_df
  # Create region classifications
  plot_data$region <- "Baseline"
  plot_data$region[plot_data$force_nN > noiseBand_high] <- "Repulsive"
  plot_data$region[plot_data$force_nN < noiseBand_low] <- "Adhesive"
  
  # Separate data for ribbon layers
  repulsive_data <- plot_data[plot_data$force_nN > noiseBand_high, ]
  adhesive_data <- plot_data[plot_data$force_nN < noiseBand_low, ]
  
  # Create the plot
  p <- ggplot(plot_data, aes(x = separation_distance_nm, y = force_nN)) +
    # Grey background for noise band
    annotate("rect", xmin = -Inf, xmax = Inf, 
             ymin = noiseBand_low, ymax = noiseBand_high,
             alpha = 0.08, fill = "grey50") +
    # Shaded regions for energy calculation
    geom_ribbon(data = repulsive_data, 
                aes(ymin = noiseBand_high, ymax = force_nN),
                fill = "coral", alpha = alpha_ribbon, color = NA) +
    geom_ribbon(data = adhesive_data, 
                aes(ymin = force_nN, ymax = noiseBand_low),
                fill = "steelblue", alpha = alpha_ribbon, color = NA) +
    # Threshold lines
    geom_hline(yintercept = noiseBand_high, color = "red", 
               linetype = "dashed", linewidth = 0.7) +
    geom_hline(yintercept = noiseBand_low, color = "red", 
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
    # Annotation for noise-band values
    annotate("text", x = Inf, y = noiseBand_high,
         label = sprintf("noiseBand_high = %.3g nN", noiseBand_high),
             hjust = 1.1, vjust = -0.5, size = 3.5, color = "red") +
    annotate("text", x = Inf, y = noiseBand_low,
         label = sprintf("noiseBand_low = %.3g nN", noiseBand_low),
         hjust = 1.1, vjust = 1.3, size = 3.5, color = "red") +
    # Theme
    ggplot2::theme_classic(base_size = base_size) +
    theme(legend.position = if (show_legend) "bottom" else "none")
  
  return(p)
}



#' Plot one transformed curve with optional metadata annotations
#'
#' High-level visualization helper that takes an \code{fdObj} and one curve name,
#' plots the transformed force-distance curve, and optionally overlays selected
#' annotations from predefined metadata columns for the selected \code{useCurve}.
#'
#' @param fdobj An object of class \code{fdObj}.
#' @param curve_name Character. Name of one curve to plot. Must match
#'   \code{rownames(fdobj@metadata)}.
#' @param useCurve Character. Either \code{"retract"} or \code{"approach"}.
#' @param plot_raw Logical. If \code{TRUE}, also plot the corresponding raw curve
#'   and return a two-column panel using \code{cowplot::plot_grid()}.
#' @param annotate_noiseBand Logical. If \code{TRUE}, annotate noise band from
#'   \code{noiseBand_low_nN_<useCurve>} and \code{noiseBand_high_nN_<useCurve>},
#'   and add "Noise band" labels above and below the band boundaries.
#' @param annotate_repulsive_energy Logical. If \code{TRUE}, add repulsive
#'   energy value to subtitle when available.
#' @param annotate_adhesive_energy Logical. If \code{TRUE}, add adhesive force
#'   and adhesive energy values to subtitle when available.
#' @param annotate_adhesive_force Logical. If \code{TRUE}, annotate adhesive force
#'   point and separation using arrow + label.
#' @param annotate_rupture_distance Logical. If \code{TRUE}, annotate rupture
#'   distance/threshold using arrow + label.
#' @param annotate_repulsive_distance Logical. If \code{TRUE}, annotate repulsive
#'   distance/threshold using arrow + label.
#' @param xlim Optional numeric vector of length 2 for x-axis limits.
#' @param ylim Optional numeric vector of length 2 for y-axis limits.
#' @param title Character. Plot title.
#' @param base_size Numeric. Base theme text size.
#' @param line_color Character. Line color for curves.
#' @param point_color Character. Point color for curves.
#' @param line_size Numeric. Line width for curves.
#' @param point_size Numeric. Point size for curves.
#' @param line_alpha Numeric. Line alpha for curves.
#' @param point_alpha Numeric. Point alpha for curves.
#' @param annotation_text_size Numeric. Text size used for annotation labels.
#' @param interaction_label_color_rupture Character. Text/line color for rupture
#'   annotation label.
#' @param interaction_label_color_repulsive Character. Text/line color for
#'   repulsive annotation label.
#' @param interaction_label_fill Character. Fill color for interaction label box.
#' @param ... Additional ggplot2 components to add to the transformed-curve plot
#'   (e.g., \code{theme()}, \code{scale_*()}, \code{coord_*()}, \code{labs()},
#'   or extra \code{geom_*()} layers).
#'
#' @return A ggplot2 object, or a cowplot grid object when \code{plot_raw = TRUE}.
#' @export
#' @importFrom ggplot2 ggplot aes geom_hline geom_line geom_point
#' @importFrom ggplot2 geom_segment geom_vline annotate labs theme_minimal scale_color_manual
plot_curve_metrics <- function(
    fdobj,
    curve_name,
    useCurve = c("retract", "approach"),
    plot_raw = FALSE,
    annotate_noiseBand = TRUE,
    annotate_repulsive_energy = TRUE,
    annotate_adhesive_energy = TRUE,
    annotate_adhesive_force = TRUE,
    annotate_rupture_distance = TRUE,
    annotate_repulsive_distance = TRUE,
    xlim = NULL,
    ylim = NULL,
    title = NULL,
    base_size = 14,
    line_color = "grey35",
    point_color = "grey35",
    line_size = 0.7,
    point_size = 1.8,
    line_alpha = 0.9,
    point_alpha = 0.8,
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

  raw_curve <- fdobj@rawCurves[[curve_name]]
  raw_x_col <- if (useCurve == "retract") "Calc_Ramp_Rt_nm" else "Calc_Ramp_Ex_nm"
  raw_y_col <- if (useCurve == "retract") "Defl_V_Rt" else "Defl_V_Ex"

  resolve_metadata_value <- function(col_name) {
    if (!(col_name %in% colnames(metadata))) {
      return(NA_real_)
    }
    suppressWarnings(as.numeric(metadata[curve_name, col_name]))
  }

  noiseBand_low <- resolve_metadata_value(paste0("noiseBand_low_nN_", useCurve))
  noiseBand_high <- resolve_metadata_value(paste0("noiseBand_high_nN_", useCurve))

  adhesive_energy_val <- resolve_metadata_value(paste0("adhesive_energy_aJ_", useCurve))
  repulsive_energy_val <- resolve_metadata_value(paste0("repulsive_energy_aJ_", useCurve))
  adhesive_force_val <- resolve_metadata_value(paste0("adhesive_force_nN_", useCurve))
  adhesive_sep_val <- resolve_metadata_value(paste0("adhesive_sep_nm_", useCurve))
  rupture_distance_val <- resolve_metadata_value(paste0("rupture_distance_nm_", useCurve))
  rupture_threshold_val <- resolve_metadata_value(paste0("rupture_threshold_nN_", useCurve))
  repulsive_distance_val <- resolve_metadata_value(paste0("repulsive_distance_nm_", useCurve))
  repulsive_threshold_val <- resolve_metadata_value(paste0("repulsive_threshold_nN_", useCurve))

  region_colors <- c(
    "Sensitivity region" = "green3",
    "Baseline region" = "yellow3",
    "Other" = point_color
  )

  raw_point_region <- NULL
  transformed_point_region <- rep("Other", nrow(curve_df))

  if (!is.null(raw_curve) && nrow(raw_curve) > 0 && (raw_x_col %in% colnames(raw_curve))) {
    sens_seg <- fdobj@senscal_segment[[useCurve]][[curve_name]]
    base_seg <- fdobj@baseline_segment[[useCurve]][[curve_name]]

    raw_point_region <- rep("Other", nrow(raw_curve))

    if (!is.null(base_seg) && nrow(base_seg) > 0 && (raw_x_col %in% colnames(base_seg))) {
      raw_point_region[raw_curve[[raw_x_col]] %in% base_seg[[raw_x_col]]] <- "Baseline region"
    }
    if (!is.null(sens_seg) && nrow(sens_seg) > 0 && (raw_x_col %in% colnames(sens_seg))) {
      raw_point_region[raw_curve[[raw_x_col]] %in% sens_seg[[raw_x_col]]] <- "Sensitivity region"
    }

    if (nrow(curve_df) == length(raw_point_region)) {
      transformed_point_region <- raw_point_region
    } else {
      idx <- seq_len(min(nrow(curve_df), length(raw_point_region)))
      transformed_point_region[idx] <- raw_point_region[idx]
    }
  }

  curve_df$point_region <- factor(
    transformed_point_region,
    levels = c("Sensitivity region", "Baseline region", "Other")
  )

  x_min <- min(curve_df$separation_distance_nm, na.rm = TRUE)
  x_max <- max(curve_df$separation_distance_nm, na.rm = TRUE)
  x_span <- x_max - x_min
  if (!is.finite(x_span) || x_span <= 0) x_span <- 1

  y_min <- min(curve_df$force_nN, na.rm = TRUE)
  y_max <- max(curve_df$force_nN, na.rm = TRUE)
  y_span <- y_max - y_min
  if (!is.finite(y_span) || y_span <= 0) y_span <- 1

  x_offset <- 0.06 * x_span
  y_offset <- 0.08 * y_span

  if (!is.null(xlim) && (!is.numeric(xlim) || length(xlim) != 2 || any(is.na(xlim)))) {
    stop("xlim must be NULL or a numeric vector of length 2.")
  }
  if (!is.null(ylim) && (!is.numeric(ylim) || length(ylim) != 2 || any(is.na(ylim)))) {
    stop("ylim must be NULL or a numeric vector of length 2.")
  }

  if (is.null(title)) {
    title <- sprintf("Curve metrics: %s (%s)", curve_name, useCurve)
  }

  p <- ggplot(curve_df, aes(x = separation_distance_nm, y = force_nN)) +
    geom_vline(xintercept = 0, color = "black", linewidth = 0.5) +
    geom_hline(yintercept = 0, color = "black", linewidth = 0.5) +
    geom_line(color = line_color, linewidth = line_size, alpha = line_alpha) +
    geom_point(aes(color = point_region), size = point_size, alpha = point_alpha, show.legend = TRUE) +
    scale_color_manual(values = region_colors, drop = FALSE, name = "Region") +
    labs(
      x = "Separation distance (nm)",
      y = "Force (nN)",
      title = title
    ) +
    ggplot2::theme_classic(base_size = base_size) +
    coord_cartesian(xlim = xlim, ylim = ylim, clip = "off") +
    theme(
      plot.margin = ggplot2::margin(5.5, 90, 5.5, 5.5),
      legend.position = "top"
    )

  if (isTRUE(annotate_noiseBand) && is.finite(noiseBand_low) && is.finite(noiseBand_high) && noiseBand_low < noiseBand_high) {
    p <- p +
      annotate(
        "rect",
        xmin = -Inf,
        xmax = Inf,
        ymin = noiseBand_low,
        ymax = noiseBand_high,
        alpha = 0.10,
        fill = "grey50"
      ) +
      geom_hline(yintercept = noiseBand_low, color = "red", linetype = "dashed", linewidth = 0.7) +
      geom_hline(yintercept = noiseBand_high, color = "red", linetype = "dashed", linewidth = 0.7) +
      annotate(
        "text",
        x = if (is.null(xlim)) x_max else max(xlim, na.rm = TRUE),
        y = noiseBand_high,
        label = "Noise band",
        hjust = 1.05,
        vjust = -0.7,
        size = annotation_text_size,
        color = "grey20"
      ) +
      annotate(
        "text",
        x = if (is.null(xlim)) x_max else max(xlim, na.rm = TRUE),
        y = noiseBand_low,
        label = "Noise band",
        hjust = 1.05,
        vjust = 1.5,
        size = annotation_text_size,
        color = "grey20"
      )
  }

  subtitle_parts <- character(0)
  if (isTRUE(annotate_adhesive_energy) && is.finite(adhesive_force_val)) {
    subtitle_parts <- c(subtitle_parts, sprintf("Adhesive force = %.3g nN", adhesive_force_val))
  }
  if (isTRUE(annotate_adhesive_energy) && is.finite(adhesive_energy_val)) {
    subtitle_parts <- c(subtitle_parts, sprintf("Adhesive energy = %.3g aJ", adhesive_energy_val))
  }
  if (isTRUE(annotate_repulsive_energy) && is.finite(repulsive_energy_val)) {
    subtitle_parts <- c(subtitle_parts, sprintf("Repulsive energy = %.3g aJ", repulsive_energy_val))
  }
  if (length(subtitle_parts) > 0) {
    p <- p + labs(subtitle = paste(subtitle_parts, collapse = " | "))
  }

  if (isTRUE(annotate_adhesive_force) && is.finite(adhesive_force_val) && is.finite(adhesive_sep_val)) {
    adhesive_label_x <- adhesive_sep_val + x_offset
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
        linewidth = 0.4,
        arrow = ggplot2::arrow(length = grid::unit(0.12, "cm"))
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

  add_distance_annotation <- function(plot_obj, distance, threshold, type) {
    if (!is.finite(distance) || !is.finite(threshold)) {
      return(plot_obj)
    }

    interaction_y <- if (type == "rupture") -abs(threshold) else abs(threshold)
    interaction_label_color <- if (type == "rupture") interaction_label_color_rupture else interaction_label_color_repulsive
    type_label <- if (type == "rupture") "Rupture" else "Repulsive"

    label_x <- distance + x_offset
    label_y <- interaction_y + if (type == "rupture") -y_offset else y_offset

    plot_obj +
      annotate("point", x = distance, y = interaction_y, color = "dodgerblue4", size = 2) +
      geom_segment(
        data = data.frame(x = distance, y = interaction_y, xend = label_x, yend = label_y),
        aes(x = x, y = y, xend = xend, yend = yend),
        inherit.aes = FALSE,
        color = interaction_label_color,
        linewidth = 0.4,
        arrow = ggplot2::arrow(length = grid::unit(0.12, "cm"))
      ) +
      annotate(
        "label",
        x = label_x,
        y = label_y,
        label = sprintf("%s distance = %.3g nm\n%s threshold = %.3g nN", type_label, distance, type_label, threshold),
        color = interaction_label_color,
        fill = interaction_label_fill,
        hjust = 0,
        size = annotation_text_size
      )
  }

  if (isTRUE(annotate_rupture_distance)) {
    p <- add_distance_annotation(p, rupture_distance_val, rupture_threshold_val, "rupture")
  }
  if (isTRUE(annotate_repulsive_distance)) {
    p <- add_distance_annotation(p, repulsive_distance_val, repulsive_threshold_val, "repulsive")
  }

  extra_layers <- list(...)
  if (length(extra_layers) > 0) {
    p <- p + extra_layers
  }

  if (isTRUE(plot_raw)) {
    if (is.null(raw_curve) || nrow(raw_curve) == 0) {
      stop("plot_raw = TRUE requires a non-empty raw curve in fdobj@rawCurves for this sample.")
    }

    if (!all(c(raw_x_col, raw_y_col) %in% colnames(raw_curve))) {
      stop("Required raw-curve columns are missing for the selected useCurve.")
    }

    if (is.null(raw_point_region) || length(raw_point_region) != nrow(raw_curve)) {
      raw_point_region <- rep("Other", nrow(raw_curve))
    }

    raw_df <- data.frame(
      x = suppressWarnings(as.numeric(raw_curve[[raw_x_col]])),
      y = suppressWarnings(as.numeric(raw_curve[[raw_y_col]])),
      point_region = factor(raw_point_region, levels = c("Sensitivity region", "Baseline region", "Other"))
    )
    raw_df <- raw_df[is.finite(raw_df$x) & is.finite(raw_df$y), , drop = FALSE]
    if (nrow(raw_df) == 0) {
      stop("Raw curve has no finite x/y values for plotting.")
    }

    p_raw <- ggplot(raw_df, aes(x = x, y = y)) +
      geom_vline(xintercept = 0, color = "black", linewidth = 0.5) +
      geom_hline(yintercept = 0, color = "black", linewidth = 0.5) +
      geom_line(color = line_color, linewidth = line_size, alpha = line_alpha) +
      geom_point(aes(color = point_region), size = point_size, alpha = point_alpha, show.legend = TRUE) +
      scale_color_manual(values = region_colors, drop = FALSE, name = "Region") +
      labs(
        x = raw_x_col,
        y = raw_y_col,
        title = sprintf("Raw curve: %s (%s)", curve_name, useCurve)
      ) +
      ggplot2::theme_classic(base_size = base_size) +
      theme(legend.position = "top")

    if (!is.null(xlim) || !is.null(ylim)) {
      p_raw <- p_raw + coord_cartesian(xlim = xlim, ylim = ylim)
    }

    if (!requireNamespace("cowplot", quietly = TRUE)) {
      stop("plot_raw = TRUE requires package 'cowplot'. Please install it with install.packages('cowplot').")
    }

    return(cowplot::plot_grid(p_raw, p, ncol = 2))
  }

  return(p)
}


#' Plot one metric as a violin plot
#'
#' Visualizes one metric column from a data frame using violin plots, optional
#' jittered points, and an optional linear trend line. Faceting supports up to
#' two columns. Supports one global test (ANOVA or Kruskal-Wallis) shown in
#' subtitle, and pairwise tests (t-test or Wilcoxon) shown as significance
#' asterisks.
#'
#' @param df A data.frame containing grouping and metric columns.
#' @param metric_name Character scalar. One column name in \code{df} to plot
#'   on the y-axis.
#' @param group_by Character scalar. Column name in \code{df} used for the
#'   x-axis grouping.
#' @param color_by Character scalar. Column name in \code{df} used to color
#'   violins and points. Defaults to \code{group_by} when \code{NULL}.
#' @param color_map Optional named character vector of colors for groups in
#'   \code{color_by} (e.g., \code{c(A = "#1b9e77", B = "#d95f02")}).
#' @param facet_by Character vector of length 0 to 2 specifying facet columns.
#'   Use \code{NULL} for no faceting.
#' @param global_test Character scalar. One of \code{"none"}, \code{"anova"},
#'   or \code{"kruskal"}. When not \code{"none"}, the global test p-value is
#'   added as subtitle.
#' @param pairwise_test Character scalar. One of \code{"none"},
#'   \code{"t.test"}, or \code{"wilcox"}. Pairwise comparisons are shown as
#'   significance asterisks.
#' @param pairwise_comparisons Optional list of group-name pairs (e.g.,
#'   \code{list(c("A", "B"), c("A", "C"))}). If \code{NULL}, all pairwise
#'   combinations are used.
#' @param p_adjust_method Character p-value adjustment method passed to
#'   \code{ggpubr::stat_compare_means()}.
#' @param log10 Logical. If \code{TRUE}, applies \code{log10} transform to the
#'   metric values before plotting and testing. Non-positive values are removed.
#' @param add_points Logical. If \code{TRUE}, overlays jittered points.
#' @param add_smooth Logical. If \code{TRUE}, overlays \code{geom_smooth(method = "lm")}
#'   within each facet.
#' @param point_alpha Numeric point transparency.
#' @param point_color Character point color.
#' @param point_size Numeric point size.
#' @param jitter_width Numeric horizontal jitter width.
#' @param violin_fill Character fill color for violins.
#' @param violin_color Character outline color for violins.
#' @param violin_alpha Numeric violin transparency.
#' @param smooth_color Character color for linear trend line.
#' @param base_size Numeric base font size for theme.
#'
#' @return A ggplot object.
#' @export
#' @importFrom ggplot2 ggplot aes geom_violin geom_point geom_smooth
#' @importFrom ggplot2 facet_wrap facet_grid labs position_jitter theme_classic
#' @importFrom ggplot2 scale_color_manual scale_fill_manual
plot_metric_violin <- function(
    df,
    metric_name,
    group_by,
    color_by = NULL,
    color_map = NULL,
    facet_by = NULL,
    global_test = c("none", "anova", "kruskal"),
    pairwise_test = c("none", "t.test", "wilcox"),
    pairwise_comparisons = NULL,
    p_adjust_method = "BH",
    log10 = FALSE,
    add_points = TRUE,
    add_smooth = FALSE,
    point_alpha = 0.6,
    point_color = "black",
    point_size = 1.2,
    jitter_width = 0.15,
    violin_fill = "grey85",
    violin_color = "grey40",
    violin_alpha = 0.8,
    smooth_color = "steelblue",
    base_size = 10
) {
  if (!is.data.frame(df)) {
    stop("df must be a data.frame.")
  }
  if (!is.character(metric_name) || length(metric_name) != 1 || is.na(metric_name) || nchar(metric_name) == 0) {
    stop("metric_name must be one column name.")
  }
  if (!is.character(group_by) || length(group_by) != 1) {
    stop("group_by must be a single column name.")
  }
  if (!(group_by %in% colnames(df))) {
    stop("group_by column not found in df.")
  }

  if (is.null(color_by)) {
    color_by <- group_by
  }
  if (!is.character(color_by) || length(color_by) != 1 || is.na(color_by) || nchar(color_by) == 0) {
    stop("color_by must be NULL or one column name.")
  }
  if (!(color_by %in% colnames(df))) {
    stop("color_by column not found in df.")
  }

  if (!is.null(color_map)) {
    if (is.list(color_map)) {
      color_map <- unlist(color_map, use.names = TRUE)
    }
    if (!is.atomic(color_map) || is.null(names(color_map)) || any(names(color_map) == "")) {
      stop("color_map must be a named atomic vector (e.g., c(A = '#1b9e77', B = '#d95f02')).")
    }
    color_map <- as.character(color_map)
  }

  if (!(metric_name %in% colnames(df))) {
    stop(sprintf("metric_name column '%s' not found in df.", metric_name))
  }

  if (is.null(facet_by)) {
    facet_by <- character(0)
  }
  if (!is.character(facet_by)) {
    stop("facet_by must be NULL or a character vector.")
  }
  if (length(facet_by) > 2) {
    stop("facet_by supports up to two columns.")
  }

  missing_facets <- setdiff(unique(facet_by), colnames(df))
  if (length(missing_facets) > 0) {
    stop(sprintf("These facet columns are missing in df: %s", paste(missing_facets, collapse = ", ")))
  }

  global_test <- match.arg(global_test)
  pairwise_test <- match.arg(pairwise_test)

  plot_df <- data.frame(
    Group = df[[group_by]],
    Value = suppressWarnings(as.numeric(df[[metric_name]])),
    ColorGroup = df[[color_by]],
    stringsAsFactors = FALSE
  )
  if (length(facet_by) > 0) {
    for (fc in facet_by) {
      plot_df[[fc]] <- df[[fc]]
    }
  }

  plot_df <- plot_df[is.finite(plot_df$Value), , drop = FALSE]

  if (!is.logical(log10) || length(log10) != 1 || is.na(log10)) {
    stop("log10 must be TRUE or FALSE.")
  }
  if (isTRUE(log10)) {
    plot_df <- plot_df[plot_df$Value > 0, , drop = FALSE]
    if (nrow(plot_df) == 0) {
      stop("No positive values available for log10 transform.")
    }
    plot_df$Value <- base::log10(plot_df$Value)
  }

  plot_df$Group <- as.factor(plot_df$Group)

  if (nrow(plot_df) == 0) {
    stop("No finite values available for plotting after filtering.")
  }

  p <- ggplot(plot_df, aes(x = Group, y = Value, fill = ColorGroup, color = ColorGroup)) +
    geom_violin(alpha = violin_alpha, trim = FALSE)

  if (isTRUE(add_points)) {
    p <- p + geom_point(
      alpha = point_alpha,
      size = point_size,
      position = position_jitter(width = jitter_width, height = 0)
    )
  }

  if (isTRUE(add_smooth)) {
    p <- p + geom_smooth(aes(group = 1), method = "lm", se = FALSE, color = smooth_color)
  }

  if (length(facet_by) == 1) {
    p <- p + facet_wrap(as.formula(paste("~", facet_by[[1]])), scales = "free_y")
  } else if (length(facet_by) == 2) {
    p <- p + facet_grid(as.formula(paste(facet_by[[1]], "~", facet_by[[2]])), scales = "free_y")
  }

  global_subtitle <- NULL
  if (global_test != "none" && length(unique(stats::na.omit(plot_df$Group))) >= 2) {
    p_global <- tryCatch({
      if (global_test == "anova") {
        summary(stats::aov(Value ~ Group, data = plot_df))[[1]][["Pr(>F)"]][1]
      } else {
        stats::kruskal.test(Value ~ Group, data = plot_df)$p.value
      }
    }, error = function(e) NA_real_)

    if (is.finite(p_global)) {
      test_label <- if (global_test == "anova") "ANOVA" else "Kruskal-Wallis"
      global_subtitle <- sprintf("%s p = %.3g", test_label, p_global)
    }
  }

  y_label <- if (isTRUE(log10)) sprintf("log10(%s)", metric_name) else metric_name
  p <- p + labs(x = group_by, y = y_label, color = color_by, fill = color_by, subtitle = global_subtitle)

  if (!is.null(color_map)) {
    p <- p +
      scale_color_manual(values = color_map) +
      scale_fill_manual(values = color_map)
  }

  if (pairwise_test != "none") {
    if (!requireNamespace("ggpubr", quietly = TRUE)) {
      stop("pairwise_test requires package 'ggpubr'. Please install it with install.packages('ggpubr').")
    }

    if (is.null(pairwise_comparisons)) {
      groups <- as.character(stats::na.omit(unique(plot_df$Group)))
      if (length(groups) >= 2) {
        pairwise_comparisons <- utils::combn(groups, 2, simplify = FALSE)
      } else {
        pairwise_comparisons <- list()
      }
    }

    if (length(pairwise_comparisons) > 0) {
      pairwise_method <- if (pairwise_test == "wilcox") "wilcox.test" else "t.test"
      p <- p + ggpubr::stat_compare_means(
        comparisons = pairwise_comparisons,
        method = pairwise_method,
        p.adjust.method = p_adjust_method,
        label = "p.signif",
        hide.ns = TRUE,
        step.increase = 0.08
      )
    }
  }

  if (requireNamespace("ggpubr", quietly = TRUE)) {
    p <- p + ggpubr::theme_pubr(base_size = base_size)
  } else {
    p <- p + ggplot2::theme_classic(base_size = base_size)
  }

  return(p)
}


#' PCA biplot for selected feature columns
#'
#' Runs principal component analysis (PCA) on selected feature columns after
#' numeric coercion and z-score normalization (
#' \\code{center = TRUE, scale. = TRUE}), then draws a biplot with samples as
#' points and feature-loading arrows showing trend directions.
#'
#' @param df A data.frame containing features and metadata columns.
#' @param include_columns Character vector of column names to include as PCA
#'   features.
#' @param color_by Character scalar column name used to color sample points.
#' @param color_map Optional named character vector for sample colors.
#' @param point_size Numeric point size.
#' @param point_alpha Numeric point alpha.
#' @param arrow_color Character arrow/label color for feature loadings.
#' @param arrow_alpha Numeric alpha for loading arrows.
#' @param arrow_scale Numeric multiplier to scale loading-arrow length.
#' @param show_feature_labels Logical; if \code{TRUE}, labels loading arrows.
#' @param feature_label_size Numeric feature-label text size.
#' @param base_size Numeric base font size.
#'
#' @return A ggplot object. The fitted PCA model is attached as
#'   \\code{attr(plot, "pca_model")}, plus score/loading tables in
#'   \\code{attr(plot, "pca_scores")} and \\code{attr(plot, "pca_loadings")}.
#' @export
#' @importFrom ggplot2 ggplot aes geom_point geom_segment geom_text geom_hline
#' @importFrom ggplot2 geom_vline labs coord_equal theme_classic scale_color_manual
plot_pca_biplot <- function(
    df,
    include_columns,
    color_by,
    color_map = NULL,
    point_size = 2.2,
    point_alpha = 0.9,
    arrow_color = "grey30",
    arrow_alpha = 0.85,
    arrow_scale = 1,
    show_feature_labels = TRUE,
    feature_label_size = 3.5,
    base_size = 12
) {
  if (!is.data.frame(df)) {
    stop("df must be a data.frame.")
  }
  if (!is.character(include_columns) || length(include_columns) < 2) {
    stop("include_columns must contain at least two feature column names.")
  }
  if (!is.character(color_by) || length(color_by) != 1 || is.na(color_by) || nchar(color_by) == 0) {
    stop("color_by must be one column name.")
  }

  missing_features <- setdiff(include_columns, colnames(df))
  if (length(missing_features) > 0) {
    stop(sprintf("These include_columns are missing in df: %s", paste(missing_features, collapse = ", ")))
  }
  if (!(color_by %in% colnames(df))) {
    stop("color_by column not found in df.")
  }

  if (!is.null(color_map)) {
    if (is.list(color_map)) {
      color_map <- unlist(color_map, use.names = TRUE)
    }
    if (!is.atomic(color_map) || is.null(names(color_map)) || any(names(color_map) == "")) {
      stop("color_map must be a named atomic vector (e.g., c(A = '#1b9e77', B = '#d95f02')).")
    }
    color_map <- as.character(color_map)
  }

  work_df <- data.frame(df[, unique(c(include_columns, color_by)), drop = FALSE], stringsAsFactors = FALSE)
  for (nm in include_columns) {
    work_df[[nm]] <- suppressWarnings(as.numeric(work_df[[nm]]))
  }

  keep_idx <- stats::complete.cases(work_df[, include_columns, drop = FALSE]) & !is.na(work_df[[color_by]])
  dropped_n <- sum(!keep_idx)
  work_df <- work_df[keep_idx, , drop = FALSE]

  if (dropped_n > 0) {
    message(sprintf("Dropped %d row(s) with missing/non-finite feature values or missing color group.", dropped_n))
  }
  if (nrow(work_df) < 2) {
    stop("Not enough complete rows for PCA after filtering.")
  }

  feature_mat <- as.matrix(work_df[, include_columns, drop = FALSE])
  col_sd <- apply(feature_mat, 2, stats::sd)
  if (any(!is.finite(col_sd) | col_sd == 0)) {
    bad_cols <- include_columns[!is.finite(col_sd) | col_sd == 0]
    stop(sprintf("These feature columns have zero/invalid variance: %s", paste(bad_cols, collapse = ", ")))
  }

  pca_fit <- stats::prcomp(feature_mat, center = TRUE, scale. = TRUE)

  scores <- as.data.frame(pca_fit$x[, 1:2, drop = FALSE])
  colnames(scores) <- c("PC1", "PC2")
  scores$ColorGroup <- as.factor(work_df[[color_by]])

  loadings <- as.data.frame(pca_fit$rotation[, 1:2, drop = FALSE])
  colnames(loadings) <- c("PC1", "PC2")
  loadings$Feature <- rownames(loadings)

  score_range <- apply(scores[, c("PC1", "PC2"), drop = FALSE], 2, function(v) diff(range(v, na.rm = TRUE)))
  loading_range <- apply(loadings[, c("PC1", "PC2"), drop = FALSE], 2, function(v) diff(range(v, na.rm = TRUE)))

  valid <- is.finite(score_range) & is.finite(loading_range) & (loading_range > 0)
  if (any(valid)) {
    mult <- min(score_range[valid] / loading_range[valid]) * 0.80 * arrow_scale
  } else {
    mult <- 1 * arrow_scale
  }
  if (!is.finite(mult) || mult <= 0) mult <- 1

  loadings$PC1_end <- loadings$PC1 * mult
  loadings$PC2_end <- loadings$PC2 * mult

  var_explained <- (pca_fit$sdev^2) / sum(pca_fit$sdev^2)
  x_lab <- sprintf("PC1 (%.1f%%)", 100 * var_explained[1])
  y_lab <- sprintf("PC2 (%.1f%%)", 100 * var_explained[2])

  p <- ggplot(scores, aes(x = PC1, y = PC2, color = ColorGroup)) +
    geom_hline(yintercept = 0, color = "grey75", linetype = "dashed", linewidth = 0.4) +
    geom_vline(xintercept = 0, color = "grey75", linetype = "dashed", linewidth = 0.4) +
    geom_point(size = point_size, alpha = point_alpha) +
    geom_segment(
      data = loadings,
      aes(x = 0, y = 0, xend = PC1_end, yend = PC2_end),
      inherit.aes = FALSE,
      color = arrow_color,
      alpha = arrow_alpha,
      linewidth = 0.6,
      arrow = ggplot2::arrow(length = grid::unit(0.18, "cm"))
    ) +
    labs(
      x = x_lab,
      y = y_lab,
      color = color_by,
      title = "PCA biplot"
    ) +
    coord_equal() +
    ggplot2::theme_classic(base_size = base_size)

  if (isTRUE(show_feature_labels)) {
    p <- p + geom_text(
      data = loadings,
      aes(x = PC1_end, y = PC2_end, label = Feature),
      inherit.aes = FALSE,
      color = arrow_color,
      size = feature_label_size,
      vjust = -0.3
    )
  }

  if (!is.null(color_map)) {
    p <- p + scale_color_manual(values = color_map)
  }

  attr(p, "pca_model") <- pca_fit
  attr(p, "pca_scores") <- scores
  attr(p, "pca_loadings") <- loadings

  return(p)
}


#' Plot a scaled feature heatmap using ComplexHeatmap
#'
#' Creates a heatmap where selected feature columns are shown as rows and
#' samples are shown as columns. Feature values are z-score scaled by row
#' (feature) before plotting. Optionally annotates sample columns with up to
#' two metadata columns.
#'
#' @param df A data.frame containing feature and metadata columns. Each row is
#'   one sample.
#' @param include_columns Character vector of feature column names to include as
#'   heatmap rows.
#' @param annotate_columns Optional character vector (length 0 to 2) of metadata
#'   column names used to annotate heatmap columns (samples).
#' @param annotation_colors Optional named list of color mappings for annotation
#'   columns passed to \code{ComplexHeatmap::HeatmapAnnotation(col = ...)}.
#' @param cluster_rows Logical; whether to cluster heatmap rows.
#' @param cluster_columns Logical; whether to cluster heatmap columns.
#' @param show_row_dend Logical; whether to display the row dendrogram.
#' @param show_column_dend Logical; whether to display the column dendrogram.
#' @param show_row_names Logical; whether to display row names (feature names).
#' @param show_column_names Logical; whether to display column names (sample names).
#' @param heatmap_name Character legend title for the scaled values.
#' @param row_title Optional row title.
#' @param column_title Optional column title.
#' @param draw Logical; if \code{TRUE}, draws the heatmap immediately.
#'
#' @return A \code{ComplexHeatmap} object. When \code{draw = TRUE}, returns the
#'   object produced by \code{ComplexHeatmap::draw()}.
#' @export
plot_complex_heatmap <- function(
    df,
    include_columns,
    annotate_columns = NULL,
    annotation_colors = NULL,
    cluster_rows = TRUE,
    cluster_columns = TRUE,
    show_row_dend = TRUE,
    show_column_dend = TRUE,
    show_row_names = TRUE,
    show_column_names = TRUE,
    heatmap_name = "z-score",
    row_title = "Features",
    column_title = NULL,
    draw = TRUE
) {
  if (!is.data.frame(df)) {
    stop("df must be a data.frame.")
  }
  if (!is.character(include_columns) || length(include_columns) < 1) {
    stop("include_columns must contain at least one feature column name.")
  }

  include_columns <- unique(include_columns)
  missing_features <- setdiff(include_columns, colnames(df))
  if (length(missing_features) > 0) {
    stop(sprintf("These include_columns are missing in df: %s", paste(missing_features, collapse = ", ")))
  }

  if (is.null(annotate_columns)) {
    annotate_columns <- character(0)
  }
  if (!is.character(annotate_columns)) {
    stop("annotate_columns must be NULL or a character vector.")
  }
  annotate_columns <- unique(annotate_columns)
  if (length(annotate_columns) > 2) {
    stop("annotate_columns supports up to two metadata columns.")
  }

  missing_anno <- setdiff(annotate_columns, colnames(df))
  if (length(missing_anno) > 0) {
    stop(sprintf("These annotate_columns are missing in df: %s", paste(missing_anno, collapse = ", ")))
  }

  if (!is.null(annotation_colors) && !is.list(annotation_colors)) {
    stop("annotation_colors must be NULL or a named list.")
  }
  if (!is.logical(show_row_names) || length(show_row_names) != 1 || is.na(show_row_names)) {
    stop("show_row_names must be TRUE or FALSE.")
  }
  if (!is.logical(show_column_names) || length(show_column_names) != 1 || is.na(show_column_names)) {
    stop("show_column_names must be TRUE or FALSE.")
  }

  if (!requireNamespace("ComplexHeatmap", quietly = TRUE)) {
    stop("This function requires package 'ComplexHeatmap'. Please install it first.")
  }
  if (!requireNamespace("circlize", quietly = TRUE)) {
    stop("This function requires package 'circlize'. Please install it first.")
  }

  work_cols <- unique(c(include_columns, annotate_columns))
  work_df <- data.frame(df[, work_cols, drop = FALSE], stringsAsFactors = FALSE)

  for (nm in include_columns) {
    work_df[[nm]] <- suppressWarnings(as.numeric(work_df[[nm]]))
  }

  keep_idx <- stats::complete.cases(work_df[, include_columns, drop = FALSE])
  dropped_n <- sum(!keep_idx)
  work_df <- work_df[keep_idx, , drop = FALSE]

  if (dropped_n > 0) {
    message(sprintf("Dropped %d row(s) with missing/non-numeric feature values.", dropped_n))
  }
  if (nrow(work_df) == 0) {
    stop("No rows available for heatmap after filtering.")
  }

  sample_ids <- rownames(df)
  if (is.null(sample_ids) || any(is.na(sample_ids)) || any(sample_ids == "")) {
    sample_ids <- paste0("sample_", seq_len(nrow(df)))
  }
  sample_ids <- make.unique(as.character(sample_ids))[keep_idx]

  feature_mat <- as.matrix(work_df[, include_columns, drop = FALSE])
  mat <- t(feature_mat)
  rownames(mat) <- include_columns
  colnames(mat) <- sample_ids

  row_scale <- function(v) {
    v_mean <- mean(v, na.rm = TRUE)
    v_sd <- stats::sd(v, na.rm = TRUE)
    if (!is.finite(v_sd) || v_sd == 0) {
      return(rep(0, length(v)))
    }
    (v - v_mean) / v_sd
  }
  scaled_mat <- t(apply(mat, 1, row_scale))
  if (is.null(dim(scaled_mat))) {
    scaled_mat <- matrix(scaled_mat, nrow = 1)
    rownames(scaled_mat) <- rownames(mat)
    colnames(scaled_mat) <- colnames(mat)
  }

  top_annotation <- NULL
  if (length(annotate_columns) > 0) {
    anno_df <- data.frame(work_df[, annotate_columns, drop = FALSE], stringsAsFactors = FALSE)
    for (nm in annotate_columns) {
      anno_df[[nm]] <- as.factor(anno_df[[nm]])
    }

    anno_col <- NULL
    if (!is.null(annotation_colors)) {
      if (is.null(names(annotation_colors)) || any(names(annotation_colors) == "")) {
        stop("annotation_colors must be a named list using annotate_columns as names.")
      }
      anno_col <- annotation_colors[intersect(names(annotation_colors), annotate_columns)]
    }

    top_annotation <- ComplexHeatmap::HeatmapAnnotation(
      df = anno_df,
      col = anno_col,
      which = "column"
    )
  }

  lim <- max(abs(scaled_mat), na.rm = TRUE)
  if (!is.finite(lim) || lim == 0) {
    lim <- 1
  }
  col_fun <- circlize::colorRamp2(
    c(-lim, 0, lim),
    c("#3B4CC0", "#FFFFFF", "#B40426")
  )

  ht <- ComplexHeatmap::Heatmap(
    scaled_mat,
    name = heatmap_name,
    col = col_fun,
    cluster_rows = cluster_rows,
    cluster_columns = cluster_columns,
    show_row_dend = show_row_dend,
    show_column_dend = show_column_dend,
    show_row_names = show_row_names,
    show_column_names = show_column_names,
    top_annotation = top_annotation,
    row_title = row_title,
    column_title = column_title
  )

  if (isTRUE(draw)) {
    return(ComplexHeatmap::draw(ht))
  }

  return(ht)
}

