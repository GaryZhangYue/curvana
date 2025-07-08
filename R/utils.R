#' Plot deflection vs distance from rawCurves slot of fdObj
#'
#' @param obj A `fdObj` object
#' @param group_curves_by Metadata column to color by (optional)
#' @param split_curves_by Metadata column to facet by (optional)
#' @param color_map Named vector of colors (optional)
#' @param point_size Size of points (default = 0.5)
#' @param alpha Transparency of points (default = 0.6)
#' @param ... Additional ggplot2 layers or options
#'
#' @return A ggplot2 object of the scatter plots showing the change of deflection across piezo movement distance. The retraction and extension segments will be plotted in two separate panels.
#' @export
#' @importFrom ggplot2 ggplot aes geom_point facet_wrap facet_grid labs theme_minimal scale_color_manual guides

deflection_plot <- function(obj,
                            group_curves_by = NULL,
                            split_curves_by = NULL,
                            point_size = 0.5,
                            alpha = 0.5,
                            color_map = NULL) {
  meta <- obj@metadata
  curves <- obj@rawCurves

  # Validate inputs
  if (!is.null(group_curves_by)) {
    if (!is.character(group_curves_by) || length(group_curves_by) != 1) {
      stop("group_curves_by must be a single column name (character)")
    }
    if (!(group_curves_by %in% colnames(meta))) {
      stop("group_curves_by must be a column name in metadata")
    }
  }

  if (!is.null(split_curves_by)) {
    if (!is.character(split_curves_by) || length(split_curves_by) != 1) {
      stop("split_curves_by must be a single column name (character)")
    }
    if (!(split_curves_by %in% colnames(meta))) {
      stop("split_curves_by must be a column name in metadata")
    }
  }

  # Build data
  plot_df <- do.call(rbind, lapply(names(curves), function(name) {
    df <- curves[[name]]
    if (nrow(df) == 0) return(NULL)
    df_approach <- data.frame(distance = df$Calc_Ramp_Ex_nm, deflection = df$Defl_V_Ex, segment = "Approach")
    df_retract  <- data.frame(distance = df$Calc_Ramp_Rt_nm, deflection = df$Defl_V_Rt, segment = "Retract")
    df_combined <- rbind(df_approach, df_retract)
    df_combined$sample <- name
    if (!is.null(group_curves_by)) {
      df_combined[[group_curves_by]] <- meta[name, group_curves_by]
    }
    if (!is.null(split_curves_by)) {
      df_combined[[split_curves_by]] <- meta[name, split_curves_by]
    }
    df_combined
  }))

  # --- Base plot ---
  if (!is.null(group_curves_by)) {
    p <- ggplot(plot_df, aes(x = distance, y = deflection)) +
      geom_point(
        aes(color = .data[[group_curves_by]]),
        size = point_size,
        alpha = alpha
      ) +
      labs(x = "Distance (nm)", y = "Deflection (V)", color = group_curves_by)
  } else {
    p <- ggplot(plot_df, aes(x = distance, y = deflection)) +
      geom_point(size = point_size, alpha = alpha) +
      labs(x = "Distance (nm)", y = "Deflection (V)")
  }

  # --- Add facets ---
  if (is.null(split_curves_by)) {
    p <- p + facet_wrap(~segment)
  } else {
    p <- p + facet_grid(as.formula(paste(split_curves_by, "~segment")))
  }

  # --- Apply color map if given ---
  if (!is.null(color_map)) {
    p <- p + scale_color_manual(values = color_map)
  }

  # --- Theme ---
  p <- p + theme_minimal(base_size = 14) +
    theme(
      panel.grid = element_blank(),
      strip.background = element_rect(fill = "#f0f0f0"),
      strip.text = element_text(face = "bold")
    )

  return(p)
}
