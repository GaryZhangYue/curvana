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
