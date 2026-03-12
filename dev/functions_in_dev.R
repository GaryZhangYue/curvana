install.packages("signal")
library(signal)
required_packages = c('curvana', 'ggplot2', 'DT', 'dplyr', 'cowplot')
optional_packages = c('ggpubr')

missing_required <- required_packages[!vapply(required_packages, requireNamespace, logical(1), quietly = TRUE)]
if (length(missing_required) > 0) {
  stop('Please install required packages first: ', paste(missing_required, collapse = ', '))
}

invisible(suppressPackageStartupMessages({
  lapply(required_packages, library, character.only = TRUE)
}))

available_optional <- optional_packages[vapply(optional_packages, requireNamespace, logical(1), quietly = TRUE)]
if (length(available_optional) > 0) {
  invisible(suppressPackageStartupMessages({
    lapply(available_optional, library, character.only = TRUE)
  }))
} else {
  message("Optional package 'ggpubr' is not installed; statistical annotation examples will be skipped.")
}

source(system.file("tutorial_helpers.R", package = "curvana"))

folder = system.file("extdata2", package = "curvana")
t = 1
test = createFdObjFromFolder(folder = folder, threads = t)


test@metadata <- test@metadata %>% 
  mutate(surface = sub("_.*","",filename),
         surface = case_when(
            surface == 'F' ~ 'fluorinated',
            surface == 'P' ~ 'PEG',
            surface == 'silicon' ~ 'silicon',
            surface == 'Z' ~ 'zwitterionic',
            TRUE ~ surface),
         region = sub(".*?_([0-9]+)\\.spm.*", "\\1", filename)
  )


t = 1
# test_beforeTransform -> test
test = transform_curves(test,spring_constant = 0.08, # spring constant
                        useCurve = 'approach', # analyze the approach curve
                        # baseline determination and filtering
                        least_length = 200, # set the 200 data points at the far right side of each curve as the baseline region (the part of the curve presuming to be under zero force)
                        slp_threshold = 0.01, # any curves with the baseline region having slope > 0.01 will be discarded
                        std_threshold = 0.01, # any curves with the baseline region having standard deviation of y values > 0.01 will be discarded
                        # sensitivity calibration
                        end = 100, # use the first 100 data points at most on the left side to calibrate sensitivity
                        threads = t)

test = transform_curves(test,spring_constant = 0.08, # spring constant
                        useCurve = 'retract', # analyze the retract curve
                        # baseline determination and filtering
                        least_length = 200, # set the 200 data points at the far right side of each curve as the baseline region (the part of the curve presuming to be under zero force)
                        slp_threshold = 0.01, # any curves with the baseline region having slope > 0.01 will be discarded
                        std_threshold = 0.01, # any curves with the baseline region having standard deviation of y values > 0.01 will be discarded
                        # sensitivity calibration
                        end = 100, # use the first 100 data points at most on the left side to calibrate sensitivity
                        threads = t)
```


test = analyze_curves_all_analytical_metrics(
  test, # the object
  useCurve = "both", # run analysis for both approach and retract 
  threads = 1, 
  noise_baseline_span = "automatic", # use the baseline section previously defined in curve transformation for noise band calculation
  noise_threshold_method = "quantile", # use quantile method to define the noise band
  noise_quantile_low = 0.05, # define the lower end of noise band as 5% percentile of y values of the baseline section
  noise_quantile_high = 1, # define the upper end of noise band as 95% percentile of y values of the baseline section
  analyze_adhesive_force = TRUE, # whether to analyze adhesive force
  analyze_energy = TRUE, # whether to analyze interaction energy (repulsive and adhesive)
  analyze_rupture_distance = TRUE, # whether to analyze adhesive/rupture distance
  analyze_repulsive_distance = TRUE, # whether to analyze repulsive distance
  analyze_rupture_distance_baseline_span = "automatic", # use the baseline section previously defined in curve transformation for rupture distance analysis
  analyze_rupture_distance_x_direction = "left", # scan from right to left to find the last data point before a curve enters the adhesive region from the noise band region
  analyze_repulsive_distance_baseline_span = "automatic", # use the baseline section previously defined in curve transformation for repulsive distance analysis
  analyze_repulsive_distance_x_direction = "right" # scan from left to right to find the last data point before a curve enters the noise band region from the repulsive region
)



plot_a_curve_metrics(
  fdobj = test,
  curve_name = 'F_3.spm-455_ForceCurveIndex_61.spm'	,
  useCurve = "retract",plot_raw = T,base_size = 15,annotation_text_size = 4
)

# test smoothing functions

exp.f.raw = test@rawCurves[['180424_fv_pmon2_loc1-18-22.spm']]
diff(exp.f.raw$Calc_Ramp_Ex_nm) %>% min
diff(exp.f.raw$Calc_Ramp_Ex_nm) %>% max
diff(exp.f.raw$Calc_Ramp_Rt_nm) %>% min
diff(exp.f.raw$Calc_Ramp_Rt_nm) %>% max

exp.f.raw.rt = exp.f.raw[,c(2,4)]

# Input vector from your selected data frame
y <- as.numeric(exp.f.raw.rt$Defl_V_Rt)

# p and n combinations to test
p_vals <- c(1, 2, 3,4,5)
n_vals <- c( 3)
grid <- expand.grid(p = p_vals, n = n_vals)

# Run all combinations and store results in original data frame
failed <- list()

for (i in seq_len(nrow(grid))) {
  p <- grid$p[i]
  n <- grid$n[i]
  col_name <- sprintf("sgolay_p%d_n%d", p, n)

  smoothed <- tryCatch(
    signal::sgolayfilt(y, p = p, n = n),
    error = function(e) {
      failed[[col_name]] <<- conditionMessage(e)
      rep(NA_real_, length(y))
    }
  )

  exp.f.raw[[col_name]] <- smoothed
}

# Show which combinations failed (expected for invalid p/n)
if (length(failed) > 0) {
  message("Failed combinations:")
  print(data.frame(
    combo = names(failed),
    reason = unlist(failed),
    row.names = NULL
  ))
}

# Choose x-axis (use retract ramp if available)
x_col <- if ("Calc_Ramp_Rt_nm" %in% names(exp.f.raw)) "Calc_Ramp_Rt_nm" else names(exp.f.raw)[1]
x <- exp.f.raw[[x_col]]

# Collect valid smoothed columns (non-all-NA)
smooth_cols <- grep("^sgolay_p\\d+_n\\d+$", names(exp.f.raw), value = TRUE)
valid_cols <- smooth_cols[vapply(exp.f.raw[smooth_cols], function(z) any(!is.na(z)), logical(1))]

# Build long data for ggplot2: raw + all valid smooth series
raw_long <- data.frame(
  x = x,
  y = y,
  series = "raw",
  stringsAsFactors = FALSE
)

if (length(valid_cols) > 0) {
  smooth_mat <- as.data.frame(exp.f.raw[valid_cols], check.names = FALSE)
  smooth_long <- stack(smooth_mat)
  names(smooth_long) <- c("y", "series")
  smooth_long$x <- rep(x, times = ncol(smooth_mat))
  plot_df <- rbind(raw_long, smooth_long[, c("x", "y", "series")])
} else {
  plot_df <- raw_long
}

plot_df$series <- factor(plot_df$series, levels = c("raw", valid_cols))

# Aesthetic maps
color_map <- c(
  raw = "black",
  setNames(grDevices::hcl.colors(length(valid_cols), palette = "Dark 3"), valid_cols)
)
size_map <- c(raw = 1.1, setNames(rep(0.6, length(valid_cols)), valid_cols))
alpha_map <- c(raw = 1.0, setNames(rep(0.9, length(valid_cols)), valid_cols))

  # Install/load plotly
if (!requireNamespace("plotly", quietly = TRUE)) {
  install.packages("plotly")
}
library(plotly)

# Interactive line plot: raw + all valid smooth curves
fig <- plot_ly()

# Raw curve
fig <- fig %>%
  add_lines(
    x = x, y = y,
    name = "raw",
    line = list(color = "black", width = 2)
  )

# Smoothed curves
if (length(valid_cols) > 0) {
  line_cols <- grDevices::hcl.colors(length(valid_cols), palette = "Dark 3")

  for (j in seq_along(valid_cols)) {
    col_name <- valid_cols[j]
    label <- sub("^sgolay_p([0-9]+)_n([0-9]+)$", "p=\\1, n=\\2", col_name)

    fig <- fig %>%
      add_lines(
        x = x,
        y = exp.f.raw[[col_name]],
        name = label,
        line = list(color = line_cols[j], width = 1.2),
        connectgaps = FALSE
      )
  }
}

fig <- fig %>%
  layout(
    title = "Savitzky-Golay smoothing across p/n combinations",
    xaxis = list(title = x_col),
    yaxis = list(title = "Defl_V_Rt"),
    hovermode = "x unified"
  )

fig
