#' Plot raw deflection vs piezo distance from rawCurves slot of fdObj
#'
#' Takes an \code{fdObj} containing raw AFM curves in \code{fdobj@rawCurves},
#' extracts the requested approach branch, retract branch, or both, and builds
#' a plotting data frame from raw piezo-distance values and raw deflection
#' signals. Optional metadata columns can be used to color curves or split the
#' display into facets. The output is a \code{ggplot2} object showing raw
#' deflection against distance for all included curves.
#'
#' @param fdobj A `fdObj` object
#' @param curve Which segment(s) to plot: "approach", "retract", or "both"
#' @param group_curves_by Metadata column to color by (optional)
#' @param split_curves_by Metadata column to facet by (optional)
#' @param color_map Named vector of colors (optional)
#' @param point_size Size of points (default = 0.5)
#' @param alpha Transparency of points (default = 0.6)
#' @param line_alpha Transparency of connecting paths (default = 0.5)
#'
#' @return A ggplot2 object of the scatter plots showing raw deflection signal.
#' @examples 
#' folder <- system.file("extdata", package = "curvana")
#' fd_obj <- createFdObjFromFolder(folder)
#' plot_deflection_curves(fd_obj, curve = "retract")
#' @export
#' @importFrom ggplot2 ggplot aes geom_point geom_path facet_wrap facet_grid labs theme_minimal scale_color_manual guides element_blank element_rect element_text

plot_deflection_curves <- function(fdobj,
                                   curve = c("both", "approach", "retract"),
                                   group_curves_by = NULL,
                                   split_curves_by = NULL,
                                   point_size = 0.5,
                                   alpha = 0.5,
                                   line_alpha = 0.5,
                                   color_map = NULL) {
  curve <- match.arg(curve)
  meta <- fdobj@metadata
  curves <- fdobj@rawCurves

  if (!is.null(color_map)) {
    if (is.list(color_map)) {
      color_map <- unlist(color_map, use.names = TRUE)
    }
    if (!is.atomic(color_map) || is.null(names(color_map))) {
      stop("color_map must be a named atomic vector (e.g., c('1' = 'darkred', '2' = 'orange')).")
    }
    color_map <- as.character(color_map)
  }

  # Validate metadata inputs
  if (!is.null(group_curves_by) && !group_curves_by %in% colnames(meta)) {
    stop("group_curves_by must be a column name in metadata")
  }
  if (!is.null(split_curves_by) && !split_curves_by %in% colnames(meta)) {
    stop("split_curves_by must be a column name in metadata")
  }

  meta_scalar <- function(col_name, idx) {
    if (is.na(idx)) return(NA_character_)
    value <- meta[[col_name]][idx]
    if (is.list(value)) {
      value <- unlist(value, recursive = TRUE, use.names = FALSE)
    }
    if (length(value) == 0) return(NA_character_)
    as.character(value[[1]])
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

    idx <- match(name, rownames(meta))

    if (!is.null(group_curves_by)) {
      df_combined[[group_curves_by]] <- meta_scalar(group_curves_by, idx)
    }
    if (!is.null(split_curves_by)) {
      df_combined[[split_curves_by]] <- meta_scalar(split_curves_by, idx)
    }

    df_combined
  })

  plot_df <- do.call(rbind, df_list)
  if (is.null(plot_df) || nrow(plot_df) == 0) return(NULL)

  # Base plot
  p <- ggplot(plot_df, aes(x = distance, y = deflection)) +
    labs(x = "Distance (nm)", y = "Deflection (V)")

  if (!is.null(group_curves_by)) {
    p <- p + geom_path(aes(color = .data[[group_curves_by]], group = interaction(sample, segment)),
                       alpha = line_alpha,
                       linewidth = 0.35,
                       show.legend = FALSE) +
      geom_point(aes(color = .data[[group_curves_by]]),
                        size = point_size, alpha = alpha) +
      labs(color = group_curves_by)
  } else {
    p <- p + geom_path(aes(group = interaction(sample, segment)),
                       alpha = line_alpha,
                       linewidth = 0.35,
                       color = "grey50") +
      geom_point(size = point_size, alpha = alpha)
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


#' Plot raw deflection vs row index from rawCurves slot of fdObj
#'
#' Takes an \code{fdObj} with raw curves as input and returns a \code{ggplot2}
#' object showing raw deflection values against data-point index rather than
#' against distance. The function is similar to
#' \code{plot_deflection_curves()}, but it uses row number on the x-axis and,
#' when available, respects metadata columns that record the imported number of
#' valid approach or retract points. This makes it useful for checking curve
#' length, signal orientation, and general raw-data shape before downstream
#' transformation.
#'
#' This is useful for visualizing:
#' \itemize{
#'   \item How many data points are in each curve
#'   \item Whether the data is oriented in the correct direction
#'   \item The general shape and quality of raw deflection data
#' }
#'
#' @param fdobj A `fdObj` object
#' @param curve Which segment(s) to plot: "approach", "retract", or "both"
#' @param group_curves_by Metadata column to color by (optional)
#' @param split_curves_by Metadata column to facet by (optional)
#' @param color_map Named vector of colors (optional)
#' @param point_size Size of points (default = 0.5)
#' @param alpha Transparency of points (default = 0.6)
#' @param line_alpha Transparency of connecting paths (default = 0.5)
#'
#' @return A ggplot2 object showing raw deflection signal vs data point index.
#' @examples
#' folder <- system.file("extdata", package = "curvana")
#' fd_obj <- createFdObjFromFolder(folder)
#' plot_deflection_curves_by_index(fd_obj, curve = "retract")
#' @export
#' @importFrom ggplot2 ggplot aes geom_point geom_path facet_wrap facet_grid labs theme_minimal scale_color_manual guides element_blank element_rect element_text
plot_deflection_curves_by_index <- function(fdobj,
                                            curve = c("both", "approach", "retract"),
                                            group_curves_by = NULL,
                                            split_curves_by = NULL,
                                            point_size = 0.5,
                                            alpha = 0.5,
                                            line_alpha = 0.5,
                                            color_map = NULL) {
  curve <- match.arg(curve)
  meta <- fdobj@metadata
  curves <- fdobj@rawCurves

  if (!is.null(color_map)) {
    if (is.list(color_map)) {
      color_map <- unlist(color_map, use.names = TRUE)
    }
    if (!is.atomic(color_map) || is.null(names(color_map))) {
      stop("color_map must be a named atomic vector (e.g., c('1' = 'darkred', '2' = 'orange')).")
    }
    color_map <- as.character(color_map)
  }

  # Validate metadata inputs
  if (!is.null(group_curves_by) && !group_curves_by %in% colnames(meta)) {
    stop("group_curves_by must be a column name in metadata")
  }
  if (!is.null(split_curves_by) && !split_curves_by %in% colnames(meta)) {
    stop("split_curves_by must be a column name in metadata")
  }

  meta_scalar <- function(col_name, idx) {
    if (is.na(idx)) return(NA_character_)
    value <- meta[[col_name]][idx]
    if (is.list(value)) {
      value <- unlist(value, recursive = TRUE, use.names = FALSE)
    }
    if (length(value) == 0) return(NA_character_)
    as.character(value[[1]])
  }

  # Collect data
  df_list <- lapply(names(curves), function(name) {
    df <- curves[[name]]
    if (nrow(df) == 0) return(NULL)

    idx <- match(name, rownames(meta))
    segments <- list()

    if (curve %in% c("both", "approach") && all(c("Calc_Ramp_Ex_nm", "Defl_V_Ex") %in% colnames(df))) {
      # Retrieve number of data points from metadata
      n_points_col <- "Number_of_datapoints_approach"
      if (n_points_col %in% colnames(meta) && !is.na(idx)) {
        n_points <- as.integer(meta[idx, n_points_col])
        if (is.na(n_points) || n_points <= 0) {
          n_points <- sum(!is.na(df$Calc_Ramp_Ex_nm))
        }
      } else {
        n_points <- sum(!is.na(df$Calc_Ramp_Ex_nm))
      }
      
      segments$approach <- data.frame(
        row_index = seq_len(n_points),
        deflection = df$Defl_V_Ex[seq_len(n_points)],
        segment = "Approach"
      )
    }
    
    if (curve %in% c("both", "retract") && all(c("Calc_Ramp_Rt_nm", "Defl_V_Rt") %in% colnames(df))) {
      # Retrieve number of data points from metadata
      n_points_col <- "Number_of_datapoints_retract"
      if (n_points_col %in% colnames(meta) && !is.na(idx)) {
        n_points <- as.integer(meta[idx, n_points_col])
        if (is.na(n_points) || n_points <= 0) {
          n_points <- sum(!is.na(df$Calc_Ramp_Rt_nm))
        }
      } else {
        n_points <- sum(!is.na(df$Calc_Ramp_Rt_nm))
      }
      
      segments$retract <- data.frame(
        row_index = seq_len(n_points),
        deflection = df$Defl_V_Rt[seq_len(n_points)],
        segment = "Retract"
      )
    }

    if (length(segments) == 0) return(NULL)

    df_combined <- do.call(rbind, segments)
    df_combined$sample <- name

    if (!is.null(group_curves_by)) {
      df_combined[[group_curves_by]] <- meta_scalar(group_curves_by, idx)
    }
    if (!is.null(split_curves_by)) {
      df_combined[[split_curves_by]] <- meta_scalar(split_curves_by, idx)
    }

    df_combined
  })

  plot_df <- do.call(rbind, df_list)
  if (is.null(plot_df) || nrow(plot_df) == 0) return(NULL)

  # Base plot
  p <- ggplot(plot_df, aes(x = row_index, y = deflection)) +
    labs(x = "Data Point Index (Row Number)", y = "Deflection (V)")

  if (!is.null(group_curves_by)) {
    p <- p + geom_path(aes(color = .data[[group_curves_by]], group = interaction(sample, segment)),
                       alpha = line_alpha,
                       linewidth = 0.35,
                       show.legend = FALSE) +
      geom_point(aes(color = .data[[group_curves_by]]),
                        size = point_size, alpha = alpha) +
      labs(color = group_curves_by)
  } else {
    p <- p + geom_path(aes(group = interaction(sample, segment)),
                       alpha = line_alpha,
                       linewidth = 0.35,
                       color = "grey50") +
      geom_point(size = point_size, alpha = alpha)
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
      panel.grid.minor = element_blank(),
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
#' Takes an \code{fdObj} with transformed approach and/or retract curves as
#' input, extracts the selected transformed curves, and combines them into one
#' plotting table with separation distance on the x-axis and force on the
#' y-axis. Optional metadata columns can be used to color curves or split them
#' across facets, and optional axis limits or extra \code{ggplot2} layers can
#' be applied. The output is a \code{ggplot2} object showing force-distance
#' relationships for the requested curves.
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
#' @examples
#' folder <- system.file("extdata", package = "curvana")
#' fd_obj <- createFdObjFromFolder(folder)
#' fd_obj <- transform_curves(fd_obj, spring_constant = 0.1, useCurve = "retract", threads = 1, least_length= 300)
#' plot_fd_curves(fd_obj, curve = "retract")
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

  meta_scalar <- function(col_name, idx) {
    if (is.na(idx)) return(NA_character_)
    value <- meta[[col_name]][idx]
    if (is.list(value)) {
      value <- unlist(value, recursive = TRUE, use.names = FALSE)
    }
    if (length(value) == 0) return(NA_character_)
    as.character(value[[1]])
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
      idx <- match(name, rownames(meta))
      if (!is.null(group_curves_by)) {
        df_combined[[group_curves_by]] <- meta_scalar(group_curves_by, idx)
      }
      if (!is.null(split_curves_by)) {
        df_combined[[split_curves_by]] <- meta_scalar(split_curves_by, idx)
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
      geom_path(
        aes(color = .data[[group_curves_by]], group = interaction(sample, segment)),
        alpha = line_alpha,
        linewidth = 0.35,
        show.legend = FALSE
      ) +
      geom_point(aes(color = .data[[group_curves_by]]), size = point_size, alpha = point_alpha) +
      labs(x = "Separation Distance (nm)", y = "Force (nN)", color = group_curves_by)
  } else {
    p <- ggplot(plot_df, aes(x = separation_distance_nm, y = force_nN)) +
      geom_path(
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
crossing_x0 <- function(x1, y1, x2, y2) {
  if ((y1 > 0 && y2 < 0) || (y1 < 0 && y2 > 0)) {
    x1 + (-y1) * (x2 - x1) / (y2 - y1)
  } else {
    NA_real_
  }
}

#' Plot one transformed curve with optional metadata annotations
#'
#' Takes an \code{fdObj}, the name of one curve, and the selected transformed
#' branch (approach or retract) as input, then builds a detailed visualization
#' of that single transformed force-distance curve. The function can annotate
#' noise-band bounds, adhesive force, adhesive and repulsive energies, rupture
#' distance, repulsive distance, and region labels derived from stored metadata.
#' When \code{plot_raw = TRUE}, it also plots the matching raw curve and returns
#' a two-panel display. The output is therefore either one annotated
#' \code{ggplot2} object or a combined cowplot panel containing both raw and
#' transformed views.
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
#' @param annotate_adhesive_energy Logical. If \code{TRUE}, add adhesive 
#'   energy value to subtitle when available.
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
plot_a_curve_metrics <- function(
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
  sensitivity_val <- resolve_metadata_value(paste0("sensitivity_V_nm_", useCurve))
  spring_constant_val <- resolve_metadata_value("spring_constant")
  baseline_V_val <- resolve_metadata_value(paste0("baseline_V_", useCurve))

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

  get_label_anchor <- function(position_key) {
    if (position_key == "bottom_left") {
      return(list(
        x = x_min + 0.08 * x_span,
        y = y_min + 0.04 * y_span,
        hjust = 0,
        vjust = 0
      ))
    }
    if (position_key == "bottom_right") {
      return(list(
        x = x_max - 0.08 * x_span,
        y = y_min + 0.5 * y_span,
        hjust = 1,
        vjust = 0
      ))
    }

    list(
      x = x_min + 0.50 * x_span,
      y = y_max - 0.04 * y_span,
      hjust = 0.5,
      vjust = 1
    )
  }

  add_fixed_label <- function(plot_obj,
                              point_x,
                              point_y,
                              label_text,
                              label_color,
                              position_key,
                              label_fill = "white") {
    anchor <- get_label_anchor(position_key)

    plot_obj +
      annotate(
        "segment",
        x = point_x,
        y = point_y,
        xend = anchor$x,
        yend = anchor$y,
        color = label_color,
        linewidth = 0.45,
        alpha = 0.85
      ) +
      annotate(
        "label",
        x = anchor$x,
        y = anchor$y,
        label = label_text,
        color = label_color,
        fill = label_fill,
        size = annotation_text_size,
        label.size = 0.25,
        hjust = anchor$hjust,
        vjust = anchor$vjust,
        label.padding = grid::unit(0.15, "lines")
      )
  }

  if (!is.null(xlim) && (!is.numeric(xlim) || length(xlim) != 2 || any(is.na(xlim)))) {
    stop("xlim must be NULL or a numeric vector of length 2.")
  }
  if (!is.null(ylim) && (!is.numeric(ylim) || length(ylim) != 2 || any(is.na(ylim)))) {
    stop("ylim must be NULL or a numeric vector of length 2.")
  }

  if (is.null(title)) {
    title <- sprintf("Transformed curve: \n%s \n(%s)", curve_name, useCurve)
  }

  p <- ggplot(curve_df, aes(x = separation_distance_nm, y = force_nN)) +
    geom_vline(xintercept = 0, color = "black", linewidth = 0.5) +
    geom_hline(yintercept = 0, color = "black", linewidth = 0.5) +
    geom_path(color = line_color, linewidth = line_size, alpha = line_alpha) +
    geom_point(aes(color = point_region), size = point_size, alpha = point_alpha, show.legend = TRUE) +
    scale_color_manual(values = region_colors, drop = FALSE, name = "Region") +
    labs(
      x = "Separation distance (nm)",
      y = "Force (nN)",
      title = title
    ) +
    ggplot2::theme_classic(base_size = base_size) +
    ggplot2::coord_cartesian(xlim = xlim, ylim = ylim, clip = "off") +
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
        label = sprintf("Noise band upper bound: %.3g nN", noiseBand_high),
        hjust = 1.05,
        vjust = -0.7,
        size = annotation_text_size,
        color = "grey20"
      ) +
      annotate(
        "text",
        x = if (is.null(xlim)) x_max else max(xlim, na.rm = TRUE),
        y = noiseBand_low,
        label = sprintf("Noise band lower bound: %.3g nN", noiseBand_low),
        hjust = 1.05,
        vjust = 1.5,
        size = annotation_text_size,
        color = "grey20"
      )
  }

  subtitle_parts <- character(0)
  if (isTRUE(annotate_adhesive_energy) && is.finite(adhesive_force_val)) {
    subtitle_parts <- c(subtitle_parts, sprintf("Adhesive force = %.3g nN", adhesive_force_val))
  } else {subtitle_parts <- c(subtitle_parts,' ')}
  if (isTRUE(annotate_adhesive_energy) && is.finite(adhesive_energy_val)) {
    subtitle_parts <- c(subtitle_parts, sprintf("Adhesive energy = %.3g aJ", adhesive_energy_val))
  } else {subtitle_parts <- c(subtitle_parts,' ')}
  if (isTRUE(annotate_repulsive_energy) && is.finite(repulsive_energy_val)) {
    subtitle_parts <- c(subtitle_parts, sprintf("Repulsive energy = %.3g aJ", repulsive_energy_val))
  } else {subtitle_parts <- c(subtitle_parts,' ')}
  if (length(subtitle_parts) > 0) {
    p <- p + labs(subtitle = paste(subtitle_parts, collapse = "\n"))
  }

  if (isTRUE(annotate_adhesive_force) && is.finite(adhesive_force_val) && is.finite(adhesive_sep_val)) {
    p <- p +
      geom_point(
        data = data.frame(separation_distance_nm = adhesive_sep_val, force_nN = (-1)*adhesive_force_val),
        aes(x = separation_distance_nm, y = force_nN),
        color = "red",
        size = 3,
        inherit.aes = FALSE
      )

    p <- add_fixed_label(
      p,
      point_x = adhesive_sep_val,
      point_y = (-1) * adhesive_force_val,
      label_text = sprintf("Adhesive force = %.3g nN\nSeparation = %.3g nm", adhesive_force_val, adhesive_sep_val),
      label_color = "red",
      position_key = "bottom_left",
      label_fill = "white"
    )
  }

  add_distance_annotation <- function(plot_obj, distance, threshold, type) {
    if (!is.finite(distance) || !is.finite(threshold)) {
      return(plot_obj)
    }

    interaction_y <- if (type == "rupture") -abs(threshold) else abs(threshold)
    interaction_label_color <- if (type == "rupture") interaction_label_color_rupture else interaction_label_color_repulsive
    type_label <- if (type == "rupture") "Rupture" else "Repulsive"

    plot_obj <- plot_obj +
      annotate("point", x = distance, y = interaction_y, color = "dodgerblue4", size = 2)

    add_fixed_label(
      plot_obj,
      point_x = distance,
      point_y = interaction_y,
      label_text = sprintf("%s distance = %.3g nm\n%s threshold = %.3g nN", type_label, distance, type_label, threshold),
      label_color = interaction_label_color,
      position_key = if (type == "rupture") "bottom_right" else "top_middle",
      label_fill = interaction_label_fill
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

    raw_orig_col <- paste0(raw_y_col, "_original")
    raw_region_colors <- region_colors
    orig_df <- NULL
    if (raw_orig_col %in% colnames(raw_curve)) {
      orig_df <- data.frame(
        x = suppressWarnings(as.numeric(raw_curve[[raw_x_col]])),
        y = suppressWarnings(as.numeric(raw_curve[[raw_orig_col]]))
      )
      orig_df <- orig_df[is.finite(orig_df$x) & is.finite(orig_df$y), , drop = FALSE]
      if (nrow(orig_df) > 0) {
        raw_region_colors <- c("Before denoise" = "darkblue", raw_region_colors)
      } else {
        orig_df <- NULL
      }
    }

    p_raw <- ggplot(raw_df, aes(x = x, y = y)) +
      geom_vline(xintercept = 0, color = "black", linewidth = 0.5) +
      geom_hline(yintercept = 0, color = "black", linewidth = 0.5)

    if (!is.null(orig_df)) {
      p_raw <- p_raw +
        geom_point(
          data = orig_df,
          aes(x = x, y = y, color = "Before denoise"),
          size = point_size,
          alpha = point_alpha,
          shape = 16,
          inherit.aes = FALSE
        )
    }

    p_raw <- p_raw +
      geom_path(color = line_color, linewidth = line_size, alpha = line_alpha) +
      geom_point(aes(color = point_region), size = point_size, alpha = point_alpha, show.legend = TRUE) +
      scale_color_manual(values = raw_region_colors, drop = FALSE, name = "Region") +
      labs(
        x = raw_x_col,
        y = raw_y_col,
        title = sprintf("Raw curve: \n%s \n(%s)", curve_name, useCurve),
      ) +
      ggplot2::theme_classic(base_size = base_size) +
      theme(legend.position = "top")

    raw_subtitle <- paste(
      sprintf("Sensitivity (V/nm) = %s", if (is.finite(sensitivity_val)) sprintf("%.3g", sensitivity_val) else "NA"),
      sprintf("Spring constant (N/nm) = %s", if (is.finite(spring_constant_val)) sprintf("%.3g", spring_constant_val) else "NA"),
      sprintf("Baseline (V) = %s", if (is.finite(baseline_V_val)) sprintf("%.3g", baseline_V_val) else "NA"),
      sep = "\n"
    )
    p_raw <- p_raw + labs(subtitle = raw_subtitle)

    if (!is.null(xlim) || !is.null(ylim)) {
      p_raw <- p_raw + ggplot2::coord_cartesian(xlim = xlim, ylim = ylim)
    }

    return(cowplot::plot_grid(p_raw, p, ncol = 2))
  }

  return(p)
}


#' Plot and save curve metrics for all curves in an fdObj
#'
#' Takes an \code{fdObj} plus plotting and file-output settings as input,
#' iteratively calls \code{plot_a_curve_metrics()} for every curve in the
#' selected transformed-curve slot, and saves each generated plot to disk in the
#' requested format. This function is a batch-export wrapper for per-curve
#' metric plots rather than a plotting function for a single curve. Its output
#' is a data frame summarizing which curves were saved successfully, where each
#' file was written, and any error messages for failed plots.
#'
#' @param fdobj An object of class \code{fdObj}.
#' @param useCurve Character. Either \code{"retract"} or \code{"approach"}.
#' @param threads Integer. Number of parallel workers (default \code{1}). Uses
#'   \pkg{future}+\pkg{future.apply} when \code{threads > 1}.
#' @param plot_raw Logical. Passed to \code{plot_a_curve_metrics()}.
#' @param annotate_noiseBand Logical. Passed to \code{plot_a_curve_metrics()}.
#' @param annotate_repulsive_energy Logical. Passed to \code{plot_a_curve_metrics()}.
#' @param annotate_adhesive_energy Logical. Passed to \code{plot_a_curve_metrics()}.
#' @param annotate_adhesive_force Logical. Passed to \code{plot_a_curve_metrics()}.
#' @param annotate_rupture_distance Logical. Passed to \code{plot_a_curve_metrics()}.
#' @param annotate_repulsive_distance Logical. Passed to \code{plot_a_curve_metrics()}.
#' @param xlim Optional numeric vector of length 2 for x-axis limits.
#' @param ylim Optional numeric vector of length 2 for y-axis limits.
#' @param title Character. Plot title passed to \code{plot_a_curve_metrics()}.
#' @param base_size Numeric. Base theme text size.
#' @param line_color Character. Line color for curves.
#' @param point_color Character. Point color for curves.
#' @param line_size Numeric. Line width for curves.
#' @param point_size Numeric. Point size for curves.
#' @param line_alpha Numeric. Line alpha for curves.
#' @param point_alpha Numeric. Point alpha for curves.
#' @param annotation_text_size Numeric. Text size for annotation labels.
#' @param interaction_label_color_rupture Character. Label color for rupture annotations.
#' @param interaction_label_color_repulsive Character. Label color for repulsive annotations.
#' @param interaction_label_fill Character. Fill color for interaction labels.
#' @param destination_folder Character. Output folder path. If \code{NULL}, a
#'   timestamped folder is created in the current working directory.
#' @param width Numeric. Plot width passed to \code{ggplot2::ggsave()}.
#' @param height Numeric. Plot height passed to \code{ggplot2::ggsave()}.
#' @param format Character. Output graphics format. One of \code{"png"},
#'   \code{"pdf"}, \code{"jpeg"}, \code{"tiff"}, \code{"bmp"}, \code{"svg"}.
#' @param dpi Numeric. Resolution passed to \code{ggplot2::ggsave()}.
#' @param units Character. Units for \code{width}/\code{height}. One of
#'   \code{"in"}, \code{"cm"}, \code{"mm"}.
#' @param ... Additional ggplot2 components passed to \code{plot_a_curve_metrics()}.
#'
#' @return A data.frame with one row per curve and columns:
#'   \code{curve_name}, \code{file_path}, \code{status}, and \code{error_message}.
#'   The output folder is attached as \code{attr(result, "destination_folder")}.
#' @export
#' @importFrom ggplot2 ggsave
plot_all_curve_metrics <- function(
    fdobj,
    useCurve = c("retract", "approach"),
    threads = 1,
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
    destination_folder = NULL,
    width = 12,
    height = 6,
    format = c("png", "pdf", "jpeg", "tiff", "bmp", "svg"),
    dpi = 300,
    units = c("in", "cm", "mm"),
    ...
) {
  if (!inherits(fdobj, "fdObj")) {
    stop("fdobj must be an object of class 'fdObj'.")
  }

  useCurve <- match.arg(useCurve)
  format <- match.arg(format)
  units <- match.arg(units)

  if (!is.numeric(threads) || length(threads) != 1 || is.na(threads) || threads < 1) {
    stop("threads must be a single numeric value >= 1.")
  }
  threads <- as.integer(threads)

  if (!is.numeric(width) || length(width) != 1 || is.na(width) || width <= 0) {
    stop("width must be a single numeric value > 0.")
  }
  if (!is.numeric(height) || length(height) != 1 || is.na(height) || height <= 0) {
    stop("height must be a single numeric value > 0.")
  }
  if (!is.numeric(dpi) || length(dpi) != 1 || is.na(dpi) || dpi <= 0) {
    stop("dpi must be a single numeric value > 0.")
  }

  if (is.null(destination_folder) || !nzchar(destination_folder)) {
    destination_folder <- file.path(
      getwd(),
      paste0("plot_a_curve_metrics_", format(Sys.time(), "%Y%m%d_%H%M%S"))
    )
  }

  if (!dir.exists(destination_folder)) {
    dir.create(destination_folder, recursive = TRUE, showWarnings = FALSE)
  }
  if (!dir.exists(destination_folder)) {
    stop("Failed to create destination folder: ", destination_folder)
  }

  curve_list <- if (useCurve == "retract") fdobj@retractCurves else fdobj@approachCurves
  curve_names <- names(curve_list)
  if (length(curve_names) == 0) {
    stop("No curves found in the selected slot.")
  }

  sanitize_filename <- function(x) {
    x <- gsub("[\\\\/:*?\"<>|]", "_", x)
    x <- gsub("\\s+", "_", x)
    x
  }

  run_one <- function(curve_name) {
    plot_obj <- tryCatch(
      plot_a_curve_metrics(
        fdobj = fdobj,
        curve_name = curve_name,
        useCurve = useCurve,
        plot_raw = plot_raw,
        annotate_noiseBand = annotate_noiseBand,
        annotate_repulsive_energy = annotate_repulsive_energy,
        annotate_adhesive_energy = annotate_adhesive_energy,
        annotate_adhesive_force = annotate_adhesive_force,
        annotate_rupture_distance = annotate_rupture_distance,
        annotate_repulsive_distance = annotate_repulsive_distance,
        xlim = xlim,
        ylim = ylim,
        title = title,
        base_size = base_size,
        line_color = line_color,
        point_color = point_color,
        line_size = line_size,
        point_size = point_size,
        line_alpha = line_alpha,
        point_alpha = point_alpha,
        annotation_text_size = annotation_text_size,
        interaction_label_color_rupture = interaction_label_color_rupture,
        interaction_label_color_repulsive = interaction_label_color_repulsive,
        interaction_label_fill = interaction_label_fill,
        ...
      ),
      error = function(e) e
    )

    if (inherits(plot_obj, "error")) {
      return(data.frame(
        curve_name = curve_name,
        file_path = NA_character_,
        status = "failed",
        error_message = conditionMessage(plot_obj),
        stringsAsFactors = FALSE
      ))
    }

    file_name <- paste0(sanitize_filename(curve_name), "_", useCurve, ".", format)
    file_path <- file.path(destination_folder, file_name)

    save_error <- tryCatch({
      ggplot2::ggsave(
        filename = file_path,
        plot = plot_obj,
        width = width,
        height = height,
        units = units,
        dpi = dpi,
        limitsize = FALSE
      )
      NULL
    }, error = function(e) conditionMessage(e))

    if (is.null(save_error)) {
      return(data.frame(
        curve_name = curve_name,
        file_path = file_path,
        status = "saved",
        error_message = NA_character_,
        stringsAsFactors = FALSE
      ))
    }

    data.frame(
      curve_name = curve_name,
      file_path = NA_character_,
      status = "failed",
      error_message = save_error,
      stringsAsFactors = FALSE
    )
  }

  result_list <- if (threads > 1) {
    old_plan <- future::plan()
    on.exit(future::plan(old_plan), add = TRUE)
    future::plan(future::multisession, workers = threads)
    future.apply::future_lapply(curve_names, run_one, future.packages = "curvana")
  } else {
    lapply(curve_names, run_one)
  }

  result <- do.call(rbind, result_list)
  rownames(result) <- NULL

  n_saved <- sum(result$status == "saved", na.rm = TRUE)
  n_failed <- nrow(result) - n_saved
  message(sprintf("Saved %d plot(s); %d failed. Output: %s", n_saved, n_failed, destination_folder))

  attr(result, "destination_folder") <- destination_folder
  return(result)
}


#' Plot one metric as a violin plot
#'
#' Takes a regular data frame as input together with the name of one numeric
#' metric column and one grouping column, then constructs a violin plot for that
#' metric across groups. Optional inputs can add color grouping, faceting,
#' jittered points, boxplots, a linear trend line, and statistical summaries
#' such as a global ANOVA/Kruskal-Wallis test or pairwise t-test/Wilcoxon
#' comparisons. The output is a \code{ggplot} object that summarizes the
#' distribution of one metric across user-defined groups.
#'
#' @param df A data.frame containing grouping and metric columns. Typically the metadata slot of fdobj.
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
#' @param show_whisker_box Logical. If \code{TRUE}, overlays a whisker boxplot
#'   on top of each violin.
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
    show_whisker_box = FALSE,
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
    color_map <- stats::setNames(as.character(color_map), trimws(as.character(names(color_map))))
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
    Group = trimws(as.character(df[[group_by]])),
    Value = suppressWarnings(as.numeric(df[[metric_name]])),
    ColorGroup = trimws(as.character(df[[color_by]])),
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
  if (!is.logical(show_whisker_box) || length(show_whisker_box) != 1 || is.na(show_whisker_box)) {
    stop("show_whisker_box must be TRUE or FALSE.")
  }
  if (isTRUE(log10)) {
    plot_df <- plot_df[plot_df$Value > 0, , drop = FALSE]
    if (nrow(plot_df) == 0) {
      stop("No positive values available for log10 transform.")
    }
    plot_df$Value <- base::log10(plot_df$Value)
  }

  plot_df$Group <- as.factor(plot_df$Group)
  plot_df$ColorGroup <- as.factor(plot_df$ColorGroup)

  if (!is.null(color_map)) {
    color_levels <- levels(plot_df$ColorGroup)
    color_map <- color_map[names(color_map) %in% color_levels]
    if (length(color_map) == 0) {
      stop("color_map names do not match any levels in color_by after coercion to character.")
    }
    color_map <- color_map[match(color_levels[color_levels %in% names(color_map)], names(color_map))]
  }

  if (nrow(plot_df) == 0) {
    stop("No finite values available for plotting after filtering.")
  }

  use_color_dodge <- !identical(color_by, group_by)
  dodge_width <- 0.8

  p <- ggplot(plot_df, aes(x = Group, y = Value, fill = ColorGroup, color = ColorGroup))

  if (isTRUE(use_color_dodge)) {
    p <- p + geom_violin(
      aes(group = interaction(Group, ColorGroup)),
      alpha = violin_alpha,
      trim = FALSE,
      position = ggplot2::position_dodge(width = dodge_width)
    )
  } else {
    p <- p + geom_violin(alpha = violin_alpha, trim = FALSE)
  }

  if (isTRUE(show_whisker_box)) {
    if (isTRUE(use_color_dodge)) {
      p <- p + geom_boxplot(
        aes(color = ColorGroup, group = interaction(Group, ColorGroup)),
        width = 0.16,
        outlier.shape = NA,
        fill = NA,
        alpha = 1,
        linewidth = 0.35,
        position = ggplot2::position_dodge(width = dodge_width)
      )
    } else {
      p <- p + geom_boxplot(
        aes(color = ColorGroup, group = Group),
        width = 0.16,
        outlier.shape = NA,
        fill = NA,
        alpha = 1,
        linewidth = 0.35
      )
    }
  }

  if (isTRUE(add_points)) {
    if (isTRUE(use_color_dodge)) {
      p <- p + geom_point(
        aes(group = interaction(Group, ColorGroup)),
        alpha = point_alpha,
        size = point_size,
        position = ggplot2::position_jitterdodge(
          jitter.width = jitter_width,
          jitter.height = 0,
          dodge.width = dodge_width
        )
      )
    } else {
      p <- p + geom_point(
        alpha = point_alpha,
        size = point_size,
        position = position_jitter(width = jitter_width, height = 0)
      )
    }
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

  p <- p + ggpubr::theme_pubr(base_size = base_size)

  return(p)
}


#' PCA biplot for selected feature columns
#'
#' Takes a data frame as input together with a set of feature columns and one
#' grouping column used for point color, coerces the selected features to
#' numeric, filters incomplete rows, and runs principal component analysis on
#' the resulting matrix. It then returns a \code{ggplot} biplot in which rows of
#' the input data appear as points in PC space and selected feature loadings are
#' drawn as arrows. In addition to the plotted object, the fitted PCA model and
#' the score/loading tables are attached as attributes to the returned plot.
#'
#' @param df A data.frame containing features and metadata columns. Typically the metadata slot of fdobj.
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
    color_map <- stats::setNames(as.character(color_map), trimws(as.character(names(color_map))))
  }

  work_df <- data.frame(df[, unique(c(include_columns, color_by)), drop = FALSE], stringsAsFactors = FALSE)
  for (nm in include_columns) {
    work_df[[nm]] <- suppressWarnings(as.numeric(work_df[[nm]]))
  }

  keep_idx <- stats::complete.cases(work_df[, include_columns, drop = FALSE]) & !is.na(work_df[[color_by]])
  dropped_n <- sum(!keep_idx)
  work_df <- work_df[keep_idx, , drop = FALSE]
  work_df[[color_by]] <- trimws(as.character(work_df[[color_by]]))

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

  if (!is.null(color_map)) {
    color_levels <- levels(scores$ColorGroup)
    color_map <- color_map[names(color_map) %in% color_levels]
    if (length(color_map) == 0) {
      stop("color_map names do not match any levels in color_by after coercion to character.")
    }
    color_map <- color_map[match(color_levels[color_levels %in% names(color_map)], names(color_map))]
  }

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
    p <- p + ggrepel::geom_text_repel(
      data = loadings,
      aes(x = PC1_end, y = PC2_end, label = Feature),
      inherit.aes = FALSE,
      color = arrow_color,
      size = feature_label_size,
      min.segment.length = 0,
      box.padding = 0.3,
      point.padding = 0.1
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
#' Takes a data frame as input together with a set of feature columns to display
#' and optional metadata columns to use as annotations, converts the selected
#' features to numeric, removes incomplete rows, scales each feature by row, and
#' arranges the result as a heatmap matrix with features as rows and samples as
#' columns. Optional annotation columns are added above the heatmap when
#' supplied. The output is a \code{ComplexHeatmap} object, or the drawn version
#' of that object when \code{draw = TRUE}.
#'
#' @param df A data.frame containing feature and metadata columns. Each row is
#'   one sample. Typically the metadata slot of fdobj.
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

#' Heatmap of raw deflection values across all curves in an fdObj
#'
#' Takes an \code{fdObj} containing raw AFM curves as input and converts all raw
#' approach and retract deflection traces into one heatmap-ready matrix. In the
#' resulting matrix, rows correspond to measurement index, columns correspond to
#' individual sample-segment pairs, and raw deflection values are padded and
#' normalized so curves of unequal length can still be displayed together.
#' Optional metadata annotations can be shown above the columns. The output is a
#' \code{ComplexHeatmap} object, or a drawn heatmap when \code{draw = TRUE}.
#'
#' @param fdobj An object of class \code{fdObj}.
#' @param annotate_columns Character vector of up to 2 metadata column names to
#'   display as column annotations alongside the Segment bar.  Defaults to
#'   \code{NULL}.
#' @param annotation_colors Named list of colour vectors for annotations,
#'   e.g. \code{list(surface = c(groupA = "green", groupB = "orange"))}.
#' @param approach_color Colour for the Approach segment annotation bar.
#'   Default \code{"steelblue"}.
#' @param retract_color Colour for the Retract segment annotation bar.
#'   Default \code{"tomato"}.
#' @param show_row_names Logical; show row index labels.  Default \code{FALSE}.
#' @param show_column_names Logical; show column (sample) names.  Default
#'   \code{FALSE}.
#' @param heatmap_name Character; label for the colour-key legend.
#' @param col_fun Optional \code{circlize::colorRamp2} colour function.  When
#'   \code{NULL} a blue–white–red ramp scaled to the 1st/50th/99th percentile
#'   of finite values is used.
#' @param na_col Colour for padded \code{NA} cells.  Default \code{"grey90"}.
#' @param draw Logical; if \code{TRUE} (default), calls
#'   \code{ComplexHeatmap::draw()} before returning.
#' @param ... Additional arguments forwarded to \code{ComplexHeatmap::Heatmap()}.
#'
#' @return A \code{ComplexHeatmap} object (drawn when \code{draw = TRUE}).
#' @export
plot_raw_deflection_heatmap <- function(
  fdobj,
  annotate_columns     = NULL,
  annotation_colors    = list(),
  approach_color       = "steelblue",
  retract_color        = "tomato",
  show_row_names       = FALSE,
  show_column_names    = FALSE,
  heatmap_name         = "Max-min Scaled Deflection",
  col_fun              = NULL,
  na_col               = "grey90",
  index_tick_interval  = 100,
  draw                 = TRUE,
  ...
) {
  if (!inherits(fdobj, "fdObj")) stop("fdobj must be an object of class 'fdObj'.")

  raw_list <- fdobj@rawCurves
  if (length(raw_list) == 0) stop("fdobj@rawCurves is empty.")
  metadata <- fdobj@metadata

  # Validate annotate_columns ---------------------------------------------
  if (!is.null(annotate_columns)) {
    if (length(annotate_columns) > 2) {
      warning("annotate_columns has more than 2 elements; only the first 2 will be used.")
      annotate_columns <- annotate_columns[seq_len(2)]
    }
    missing_cols <- setdiff(annotate_columns, colnames(metadata))
    if (length(missing_cols) > 0)
      stop(sprintf("Metadata columns not found: %s", paste(missing_cols, collapse = ", ")))
  }

  # Build one column per (sample, segment) --------------------------------
  col_vectors <- list()   # deflection numeric vectors
  col_sample  <- character()
  col_seg     <- character()

  for (nm in names(raw_list)) {
    df <- raw_list[[nm]]
    if ("Defl_V_Ex" %in% colnames(df)) {
      col_vectors[[length(col_vectors) + 1]] <- suppressWarnings(as.numeric(df[["Defl_V_Ex"]]))
      col_sample <- c(col_sample, nm)
      col_seg    <- c(col_seg, "Approach")
    }
    if ("Defl_V_Rt" %in% colnames(df)) {
      col_vectors[[length(col_vectors) + 1]] <- suppressWarnings(as.numeric(df[["Defl_V_Rt"]]))
      col_sample <- c(col_sample, nm)
      col_seg    <- c(col_seg, "Retract")
    }
  }

  if (length(col_vectors) == 0)
    stop("No Defl_V_Ex or Defl_V_Rt columns found in rawCurves.")

  # Pad to common length (rows = measurement index) -----------------------
  max_len <- max(vapply(col_vectors, length, integer(1)))
  mat <- do.call(cbind, lapply(col_vectors, function(v) {
    if (length(v) < max_len) c(v, rep(NA_real_, max_len - length(v))) else v
  }))
  col_ids <- paste0(col_sample, ".", tolower(col_seg))
  colnames(mat) <- col_ids

  # Re-order: all Approach columns first, then all Retract ---------------
  col_order <- order(match(col_seg, c("Approach", "Retract")))
  mat        <- mat[, col_order, drop = FALSE]
  col_sample <- col_sample[col_order]
  col_seg    <- col_seg[col_order]
  col_ids    <- col_ids[col_order]

  # Per-column Min-Max normalisation to [0, 1] ----------------------------
  mat <- apply(mat, 2, function(v) {
    mn <- min(v, na.rm = TRUE); mx <- max(v, na.rm = TRUE)
    if (!is.finite(mn) || !is.finite(mx) || mx == mn) return(v)
    (v - mn) / (mx - mn)
  })

  # Colour function -------------------------------------------------------
  if (is.null(col_fun)) {
    col_fun <- circlize::colorRamp2(c(0, 0.5, 1), c("blue", "white", "red"))
  }

  # Column annotation -----------------------------------------------------
  seg_factor <- factor(col_seg, levels = c("Approach", "Retract"))
  seg_present <- levels(droplevels(seg_factor))
  seg_colors  <- c(Approach = approach_color, Retract = retract_color)[seg_present]

  anno_df <- data.frame(Segment = seg_factor, row.names = col_ids, stringsAsFactors = FALSE)
  anno_col <- list(Segment = seg_colors)

  if (!is.null(annotate_columns)) {
    for (mc in annotate_columns) {
      vals_col <- metadata[col_sample, mc]
      if (!is.factor(vals_col)) vals_col <- as.factor(vals_col)
      anno_df[[mc]] <- vals_col
      if (mc %in% names(annotation_colors)) {
        anno_col[[mc]] <- annotation_colors[[mc]]
      }
    }
  }

  top_anno <- ComplexHeatmap::HeatmapAnnotation(
    df    = anno_df,
    col   = anno_col,
    which = "column",
    show_annotation_name = TRUE
  )

  # Row index tick annotation --------------------------------------------
  right_anno <- NULL
  if (!is.null(index_tick_interval) && is.numeric(index_tick_interval) &&
      index_tick_interval > 0 && nrow(mat) > 0) {
    tick_rows <- seq(index_tick_interval, nrow(mat), by = index_tick_interval)
    if (length(tick_rows) > 0) {
      right_anno <- ComplexHeatmap::rowAnnotation(
        Index = ComplexHeatmap::anno_mark(
          at     = tick_rows,
          labels = as.character(tick_rows),
          which  = "row",
          side   = "right"
        )
      )
    }
  }

  # Build heatmap ---------------------------------------------------------
  ht <- ComplexHeatmap::Heatmap(
    mat,
    name               = heatmap_name,
    col                = col_fun,
    na_col             = na_col,
    cluster_rows       = FALSE,
    cluster_columns    = FALSE,
    show_row_names     = show_row_names,
    show_column_names  = show_column_names,
    top_annotation     = top_anno,
    right_annotation   = right_anno,
    ...
  )

  if (isTRUE(draw)) return(ComplexHeatmap::draw(ht))
  return(ht)
}


#' Read JPK format AFM force curve file
#'
#' @description
#' Reads a JPK format text file exported from JPK AFM software. Automatically detects
#' approach (extend) and retract segments, extracts sensitivity and spring constant,
#' and handles unit conversion from Force (N) to Voltage (V) if needed.
#'
#' @param file_path Path to the JPK .txt file
#' @param height_col Column name for height/distance data. Default is "height".
#'   If not found, falls back to "strainGaugeHeight".
#' @param deflection_col Column name for deflection data. Default is "vDeflection".
#'
#' @return A list with two elements:
#' \itemize{
#'   \item \code{raw_data}: Data frame in curvana format with columns:
#'     \code{Calc_Ramp_Ex_nm}, \code{Calc_Ramp_Rt_nm}, \code{Defl_V_Ex}, \code{Defl_V_Rt}.
#'     If segments have different lengths, the shorter is padded with NA.
#'   \item \code{parameters}: Named vector with segment-specific sensitivity and spring constant values
#'     extracted from the file, plus actual data point counts before padding:
#'     \code{approach_sensitivity_imported}, \code{approach_springConstant_imported},
#'     \code{retract_sensitivity_imported}, \code{retract_springConstant_imported},
#'     \code{Number_of_datapoints_approach}, \code{Number_of_datapoints_retract}
#' }
#'
#' @details
#' The function:
#' \itemize{
#'   \item Detects segments by looking for "# segment: extend" or "# segment: retract"
#'   \item Extracts sensitivity and spring constant separately for each segment
#'   \item Determines if deflection data is in Voltage (V) or Force (N)
#'   \item If Force (N), converts to Voltage using: V = F / (sensitivity * springConstant)
#'   \item Returns data in curvana's expected format with segment-specific parameters
#' }
#'
#' @keywords internal
read_jpk_file <- function(file_path, height_col = "height", deflection_col = "vDeflection") {
  
  # Read all lines
  lines <- readLines(file_path, warn = FALSE)
  
  # Initialize storage for segments
  segments <- list()
  current_segment <- NULL
  
  # Parse the file
  i <- 1
  while (i <= length(lines)) {
    line <- lines[i]
    
    # Check for segment start
    if (grepl("^# segment:", line)) {
      segment_type <- sub("^# segment:\\s*", "", line)
      
      # Initialize new segment
      current_segment <- list(
        type = segment_type,
        sensitivity = NA_real_,
        spring_constant = NA_real_,
        columns = NULL,
        units = NULL,
        data = NULL
      )
    }
    
    # Extract sensitivity
    if (grepl("^# sensitivity:", line)) {
      sens_val <- as.numeric(sub("^# sensitivity:\\s*", "", line))
      if (!is.null(current_segment)) {
        current_segment$sensitivity <- sens_val
      }
    }
    
    # Extract spring constant
    if (grepl("^# springConstant:", line)) {
      sc_val <- as.numeric(sub("^# springConstant:\\s*", "", line))
      if (!is.null(current_segment)) {
        current_segment$spring_constant <- sc_val
      }
    }
    
    # Extract column names
    if (grepl("^# columns:", line)) {
      cols <- sub("^# columns:\\s*", "", line)
      if (!is.null(current_segment)) {
        current_segment$columns <- strsplit(cols, "\\s+")[[1]]
      }
    }
    
    # Extract units
    if (grepl("^# units:", line)) {
      units_str <- sub("^# units:\\s*", "", line)
      if (!is.null(current_segment)) {
        current_segment$units <- strsplit(units_str, "\\s+")[[1]]
      }
    }
    
    # Check if we hit data (non-comment line after segment definition)
    if (!grepl("^#", line) && !is.null(current_segment) && nzchar(trimws(line))) {
      # Read data until next segment or end of file
      data_lines <- character()
      while (i <= length(lines) && !grepl("^# segment:", lines[i])) {
        if (!grepl("^#", lines[i]) && nzchar(trimws(lines[i]))) {
          data_lines <- c(data_lines, lines[i])
        }
        i <- i + 1
      }
      i <- i - 1  # Step back one since we'll increment at end of loop
      
      # Parse data
      if (length(data_lines) > 0) {
        # Read as table
        data_text <- paste(data_lines, collapse = "\n")
        current_segment$data <- read.table(text = data_text, 
                                           header = FALSE, 
                                           stringsAsFactors = FALSE,
                                           col.names = current_segment$columns)
        
        # Store completed segment
        segments <- append(segments, list(current_segment))
        current_segment <- NULL
      }
    }
    
    i <- i + 1
  }
  
  # Process segments into curvana format
  approach_data <- NULL
  retract_data <- NULL
  
  for (seg in segments) {
    if (is.null(seg$data) || nrow(seg$data) == 0) next
    
    # Validate that specified columns exist in segment
    if (!(height_col %in% seg$columns)) {
      stop(sprintf("Height column '%s' not found in segment %s. Available columns: %s",
                   height_col, seg$type, paste(seg$columns, collapse = ", ")))
    }
    
    if (!(deflection_col %in% seg$columns)) {
      stop(sprintf("Deflection column '%s' not found in segment %s. Available columns: %s",
                   deflection_col, seg$type, paste(seg$columns, collapse = ", ")))
    }
    
    seg_height_col <- height_col
    seg_deflection_col <- deflection_col
    
    # Get the data
    height <- seg$data[[seg_height_col]]
    deflection <- seg$data[[seg_deflection_col]]
    
    # Check units and convert if necessary
    deflection_col_idx <- which(seg$columns == seg_deflection_col)
    deflection_unit <- seg$units[deflection_col_idx]
    
    if (deflection_unit == "N") {
      # Convert from Force to Voltage: V = F / (sensitivity * springConstant)
      # Note that ths sensitivity reported in JPK files is distance per voltage (nm/V)
      # In our pipeline, sensitivity is voltage per distance
      if (!is.na(seg$sensitivity) && !is.na(seg$spring_constant)) {
        deflection <- deflection / (seg$sensitivity * seg$spring_constant)
        message(sprintf("Converted deflection from Force (N) to Voltage (V) for %s segment using sensitivity and spring constant", seg$type))
      } else {
        stop(sprintf("Cannot convert Force to Voltage: missing sensitivity or spring constant for %s segment", seg$type))
      }
    }
    
    # Convert height to nm based on unit
    height_col_idx <- which(seg$columns == seg_height_col)
    height_unit <- seg$units[height_col_idx]
    
    height <- switch(height_unit,
      "m"  = height * 1e9,   # meters to nm
      "cm" = height * 1e7,   # centimeters to nm
      "mm" = height * 1e6,   # millimeters to nm
      "um" = height * 1e3,   # micrometers to nm
      "nm" = height,         # already in nm
      stop(sprintf("Unsupported height unit '%s' in segment %s. Expected: m, cm, mm, um, or nm",
                   height_unit, seg$type))
    )
    
    # Store based on segment type
    if (seg$type == "extend") {
      approach_data <- data.frame(
        distance = height,
        deflection = deflection
      )
    } else if (seg$type == "retract") {
      retract_data <- data.frame(
        distance = height,
        deflection = deflection
      )
    }
  }
  
  # Create curvana-format data frame
  # Only include columns for segments that exist
  if (is.null(approach_data) && is.null(retract_data)) {
    stop("No valid data found in file - both segments are missing")
  }
  
  # Record actual data point counts (before any padding)
  n_approach <- if (!is.null(approach_data)) nrow(approach_data) else 0L
  n_retract <- if (!is.null(retract_data)) nrow(retract_data) else 0L
  
  # Determine if we need to align lengths (both segments exist)
  if (!is.null(approach_data) && !is.null(retract_data)) {
    # Both segments exist - pad the shorter one
    max_len <- max(nrow(approach_data), nrow(retract_data))
    
    pad_to_length <- function(x, target_len) {
      if (length(x) < target_len) {
        return(c(x, rep(NA_real_, target_len - length(x))))
      }
      return(x[1:target_len])
    }
    
    raw_data <- data.frame(
      Calc_Ramp_Ex_nm = pad_to_length(approach_data$distance, max_len),
      Calc_Ramp_Rt_nm = pad_to_length(retract_data$distance, max_len),
      Defl_V_Ex = pad_to_length(approach_data$deflection, max_len),
      Defl_V_Rt = pad_to_length(retract_data$deflection, max_len)
    )
  } else if (!is.null(approach_data)) {
    # Only approach exists
    raw_data <- data.frame(
      Calc_Ramp_Ex_nm = approach_data$distance,
      Defl_V_Ex = approach_data$deflection
    )
  } else {
    # Only retract exists
    raw_data <- data.frame(
      Calc_Ramp_Rt_nm = retract_data$distance,
      Defl_V_Rt = retract_data$deflection
    )
  }
  
  # Extract segment-specific sensitivity and spring constant values
  approach_sensitivity <- NA_real_
  approach_spring_constant <- NA_real_
  retract_sensitivity <- NA_real_
  retract_spring_constant <- NA_real_
  
  for (seg in segments) {
    if (seg$type == "extend") {
      if (!is.na(seg$sensitivity)) approach_sensitivity <- seg$sensitivity
      if (!is.na(seg$spring_constant)) approach_spring_constant <- seg$spring_constant
    } else if (seg$type == "retract") {
      if (!is.na(seg$sensitivity)) retract_sensitivity <- seg$sensitivity
      if (!is.na(seg$spring_constant)) retract_spring_constant <- seg$spring_constant
    }
  }
  
  # Create parameters vector with segment-specific values and data point counts
  parameters <- c(
    approach_sensitivity_imported = approach_sensitivity,
    approach_springConstant_imported = approach_spring_constant,
    retract_sensitivity_imported = retract_sensitivity,
    retract_springConstant_imported = retract_spring_constant,
    Number_of_datapoints_approach = n_approach,
    Number_of_datapoints_retract = n_retract
  )
  
  return(list(
    raw_data = raw_data,
    parameters = parameters
  ))
}
