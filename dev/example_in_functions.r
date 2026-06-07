# Examples of functions

# raw curves -----
x <- c(
  0.000, 0.977, 1.953, 2.930, 3.906, 4.883, 5.859, 6.836, 7.812, 8.789,
  9.766, 10.742, 11.719, 12.695, 13.672, 14.648, 15.625, 16.602, 17.578, 18.555,
  19.531, 20.508, 21.484, 22.461, 23.438, 24.414, 25.391, 26.367, 27.344, 28.320,
  29.297, 30.273, 31.250, 32.227, 33.203, 34.180, 35.156, 36.133, 37.109, 38.086,
  39.062, 40.039, 41.016, 41.992, 42.969, 43.945, 44.922, 45.898, 46.875, 47.852,
  48.828, 49.805, 50.781, 51.758, 52.734, 53.711, 54.688, 55.664, 56.641, 57.617,
  58.594, 59.570, 60.547, 61.523, 62.500, 63.477, 64.453, 65.430, 66.406, 67.383,
  68.359, 69.336, 70.312, 71.289, 72.266, 73.242, 74.219, 75.195, 76.172, 77.148,
  78.125, 79.102, 80.078, 81.055, 82.031, 83.008, 83.984, 84.961, 85.938, 86.914,
  87.891, 88.867, 89.844, 90.820, 91.797, 92.773, 93.750, 94.727, 95.703, 96.680
)

y <- c(
  0.255, 0.243, 0.231, 0.220, 0.208, 0.196, 0.184, 0.172, 0.160, 0.149,
  0.137, 0.125, 0.114, 0.101, 0.088, 0.074, 0.061, 0.048, 0.035, 0.024,
  0.012, 0.001, -0.012, -0.025, -0.037, -0.050, -0.061, -0.070, -0.078, -0.087,
  -0.097, -0.105, -0.109, -0.098, -0.085, -0.072, -0.071, -0.070, -0.066, -0.061,
  -0.057, -0.055, -0.054, -0.055, -0.054, -0.054, -0.054, -0.055, -0.055, -0.055,
  -0.056, -0.056, -0.056, -0.056, -0.055, -0.056, -0.056, -0.056, -0.056, -0.056,
  -0.056, -0.056, -0.056, -0.056, -0.057, -0.057, -0.056, -0.056, -0.055, -0.055,
  -0.055, -0.055, -0.055, -0.055, -0.056, -0.056, -0.056, -0.055, -0.056, -0.056,
  -0.056, -0.055, -0.056, -0.055, -0.056, -0.056, -0.056, -0.055, -0.055, -0.056,
  -0.056, -0.055, -0.055, -0.053, -0.054, -0.054, -0.055, -0.056, -0.056, -0.057
)

# Sensitivity calculation -----
sens = calc_sensitivity(x,y, end=20)

folder <- system.file("extdata", package = "curvana")
fd_obj <- createFdObjFromFolder(folder)
fd_obj <- analyze_sensitivity(fd_obj, end = 80, intv = 10, useCurve = "retract")
head(fd_obj@metadata$sensitivity_V_nm_retract)

# Baseline finding -----
find_baseline(x,y, least_length = 50,    sensitivity = sens$sensitivity,slp_threshold = 0.02, std_threshold = 0.02)
result <- find_baseline(x,y, least_length = 50,sensitivity = 0.012,slp_threshold = 0.02, std_threshold = 0.02)
print(result$baseline)
plot(x, y, type = "l"); lines(result$segment, col = "red", lwd = 2)

folder <- system.file("extdata", package = "curvana")
fd_obj <- createFdObjFromFolder(folder)
fd_obj <- analyze_sensitivity(fd_obj, end = 80, intv = 10, useCurve = "retract")
fd_obj <- analyze_baseline(fd_obj, least_length = 100, useCurve = "retract", slp_threshold = 0.02, std_threshold = 0.02)
head(fd_obj@metadata$baseline_V_retract)


# Denoising -----
denoised_df <- denoise_a_curve(data.frame(Calc_Ramp_Ex_nm = x, Defl_V_Ex = y), p = 1, n = 5, m = 0, ts = 1, useCurve = "approach", n_approach = length(x))
head(denoised_df)

folder <- system.file("extdata", package = "curvana")
fd_obj <- createFdObjFromFolder(folder)
fd_obj <- denoise_curves(fd_obj, p = 1, n = 5, m = 0, ts = 1, useCurve = "approach", threads = 1)
head(fd_obj@rawCurves[[1]])
#' 
#' 
# Curve transformation -----
transformed_df <- transform_a_curve(x, y,
 baseline = -0.05562,
 sensitivity = 0.012,
 spring_constant = 0.1,
 senscal_seg_x = x[1:10],
 senscal_seg_y = y[1:10]
)
plot(transformed_df$separation_distance_nm, transformed_df$force_nN, type = "l"); lines(result$segment, col = "red", lwd = 2)

folder <- system.file("extdata", package = "curvana")
fd_obj <- createFdObjFromFolder(folder)
fd_obj <- transform_curves(fd_obj,spring_constant = 0.1, useCurve = "approach", threads = 1,denoise_first = TRUE)

# Adhesive force ------
transformed_df <- data.frame(
  separation_distance_nm = c(
    0.22196440, 0.19896440, 0.17496440, 0.23529773, 0.21129773,
    0.18829773, 0.16429773, 0.14129773, 0.11729773, 0.17763107,
    0.15463107, 0.13063107, 0.19096440, 0.08363107, -0.02270227,
    -0.21336893, -0.31970227, -0.42603560, -0.53336893, -0.47303560,
    -0.49703560, -0.43670227, -0.54403560, -0.65036893, -0.67336893,
    -0.78070227, -0.72036893, -0.49436893, -0.18403560, 0.04196440,
    0.18563107, 0.49496440, 1.13863107, 3.03229773, 5.09163107,
    7.15196440, 8.21129773, 9.27163107, 10.58096440, 11.97463107,
    13.28396440, 14.42763107, 15.48796440, 16.38063107, 17.44096440,
    18.41696440, 19.39396440, 20.28663107, 21.26363107, 22.24063107,
    23.13329773, 24.11029773, 25.08629773, 26.06329773, 27.12263107,
    28.01629773, 28.99329773, 29.96929773, 30.94629773, 31.92229773,
    32.89929773, 33.87529773, 34.85229773, 35.82829773, 36.72196440,
    37.69896440, 38.75829773, 39.73529773, 40.79463107, 41.77163107,
    42.74763107, 43.72463107, 44.70063107, 45.67763107, 46.57129773,
    47.54729773, 48.52429773, 49.58363107, 50.47729773, 51.45329773,
    52.43029773, 53.49063107, 54.38329773, 55.44363107, 56.33629773,
    57.31329773, 58.28929773, 59.34963107, 60.32663107, 61.21929773,
    62.19629773, 63.25563107, 64.23263107, 65.37529773, 66.26896440,
    67.24496440, 68.13863107, 69.03229773, 70.00829773, 70.90196440
  ),
  force_nN = c(
    2.588500000, 2.488500000, 2.388500000, 2.296833333, 2.196833333,
    2.096833333, 1.996833333, 1.896833333, 1.796833333, 1.705166667,
    1.605166667, 1.505166667, 1.413500000, 1.305166667, 1.196833333,
    1.080166667, 0.971833333, 0.863500000, 0.755166667, 0.663500000,
    0.563500000, 0.471833333, 0.363500000, 0.255166667, 0.155166667,
    0.046833333, -0.044833333, -0.119833333, -0.186500000, -0.261500000,
    -0.344833333, -0.411500000, -0.444833333, -0.353166667, -0.244833333,
    -0.136500000, -0.128166667, -0.119833333, -0.086500000, -0.044833333,
    -0.011500000, 0.005166667, 0.013500000, 0.005166667, 0.013500000,
    0.013500000, 0.013500000, 0.005166667, 0.005166667, 0.005166667,
    -0.003166667, -0.003166667, -0.003166667, -0.003166667, 0.005166667,
    -0.003166667, -0.003166667, -0.003166667, -0.003166667, -0.003166667,
    -0.003166667, -0.003166667, -0.003166667, -0.003166667, -0.011500000,
    -0.011500000, -0.003166667, -0.003166667, 0.005166667, 0.005166667,
    0.005166667, 0.005166667, 0.005166667, 0.005166667, -0.003166667,
    -0.003166667, -0.003166667, 0.005166667, -0.003166667, -0.003166667,
    -0.003166667, 0.005166667, -0.003166667, 0.005166667, -0.003166667,
    -0.003166667, -0.003166667, 0.005166667, 0.005166667, -0.003166667,
    -0.003166667, 0.005166667, 0.005166667, 0.021833333, 0.013500000,
    0.013500000, 0.005166667, -0.003166667, -0.003166667, -0.011500000
  )
)
plot(transformed_df)
analyze_a_curve_adhesive_force(transformed_df)

folder <- system.file("extdata", package = "curvana")
fd_obj <- createFdObjFromFolder(folder)
fd_obj <- transform_curves(fd_obj, spring_constant = 0.1, useCurve = "retract", threads = 1)
fd_obj <- analyze_curves_adhesive_force(fd_obj, useCurve = "retract", threads = 1)
print(fd_obj@metadata[, c("adhesive_force_nN_retract", "adhesive_sep_nm_retract")])

# Noise analysis -----
transformed_df <- data.frame(
  separation_distance_nm = c(
    0.22196440, 0.19896440, 0.17496440, 0.23529773, 0.21129773,
    0.18829773, 0.16429773, 0.14129773, 0.11729773, 0.17763107,
    0.15463107, 0.13063107, 0.19096440, 0.08363107, -0.02270227,
    -0.21336893, -0.31970227, -0.42603560, -0.53336893, -0.47303560,
    -0.49703560, -0.43670227, -0.54403560, -0.65036893, -0.67336893,
    -0.78070227, -0.72036893, -0.49436893, -0.18403560, 0.04196440,
    0.18563107, 0.49496440, 1.13863107, 3.03229773, 5.09163107,
    7.15196440, 8.21129773, 9.27163107, 10.58096440, 11.97463107,
    13.28396440, 14.42763107, 15.48796440, 16.38063107, 17.44096440,
    18.41696440, 19.39396440, 20.28663107, 21.26363107, 22.24063107,
    23.13329773, 24.11029773, 25.08629773, 26.06329773, 27.12263107,
    28.01629773, 28.99329773, 29.96929773, 30.94629773, 31.92229773,
    32.89929773, 33.87529773, 34.85229773, 35.82829773, 36.72196440,
    37.69896440, 38.75829773, 39.73529773, 40.79463107, 41.77163107,
    42.74763107, 43.72463107, 44.70063107, 45.67763107, 46.57129773,
    47.54729773, 48.52429773, 49.58363107, 50.47729773, 51.45329773,
    52.43029773, 53.49063107, 54.38329773, 55.44363107, 56.33629773,
    57.31329773, 58.28929773, 59.34963107, 60.32663107, 61.21929773,
    62.19629773, 63.25563107, 64.23263107, 65.37529773, 66.26896440,
    67.24496440, 68.13863107, 69.03229773, 70.00829773, 70.90196440
  ),
  force_nN = c(
    2.588500000, 2.488500000, 2.388500000, 2.296833333, 2.196833333,
    2.096833333, 1.996833333, 1.896833333, 1.796833333, 1.705166667,
    1.605166667, 1.505166667, 1.413500000, 1.305166667, 1.196833333,
    1.080166667, 0.971833333, 0.863500000, 0.755166667, 0.663500000,
    0.563500000, 0.471833333, 0.363500000, 0.255166667, 0.155166667,
    0.046833333, -0.044833333, -0.119833333, -0.186500000, -0.261500000,
    -0.344833333, -0.411500000, -0.444833333, -0.353166667, -0.244833333,
    -0.136500000, -0.128166667, -0.119833333, -0.086500000, -0.044833333,
    -0.011500000, 0.005166667, 0.013500000, 0.005166667, 0.013500000,
    0.013500000, 0.013500000, 0.005166667, 0.005166667, 0.005166667,
    -0.003166667, -0.003166667, -0.003166667, -0.003166667, 0.005166667,
    -0.003166667, -0.003166667, -0.003166667, -0.003166667, -0.003166667,
    -0.003166667, -0.003166667, -0.003166667, -0.003166667, -0.011500000,
    -0.011500000, -0.003166667, -0.003166667, 0.005166667, 0.005166667,
    0.005166667, 0.005166667, 0.005166667, 0.005166667, -0.003166667,
    -0.003166667, -0.003166667, 0.005166667, -0.003166667, -0.003166667,
    -0.003166667, 0.005166667, -0.003166667, 0.005166667, -0.003166667,
    -0.003166667, -0.003166667, 0.005166667, 0.005166667, -0.003166667,
    -0.003166667, 0.005166667, 0.005166667, 0.021833333, 0.013500000,
    0.013500000, 0.005166667, -0.003166667, -0.003166667, -0.011500000
  )
)
analyze_a_curve_noise(transformed_df, baseline_span = 50)

folder <- system.file("extdata", package = "curvana")
fd_obj <- createFdObjFromFolder(folder)
fd_obj <- transform_curves(fd_obj, spring_constant = 0.1, useCurve = "retract", threads = 1, least_length= 300)
fd_obj <- analyze_curves_noise(fd_obj, useCurve = "retract", threads = 1)


# interaction distance -----
transformed_df <- data.frame(
  separation_distance_nm = c(
    0.22196440, 0.19896440, 0.17496440, 0.23529773, 0.21129773,
    0.18829773, 0.16429773, 0.14129773, 0.11729773, 0.17763107,
    0.15463107, 0.13063107, 0.19096440, 0.08363107, -0.02270227,
    -0.21336893, -0.31970227, -0.42603560, -0.53336893, -0.47303560,
    -0.49703560, -0.43670227, -0.54403560, -0.65036893, -0.67336893,
    -0.78070227, -0.72036893, -0.49436893, -0.18403560, 0.04196440,
    0.18563107, 0.49496440, 1.13863107, 3.03229773, 5.09163107,
    7.15196440, 8.21129773, 9.27163107, 10.58096440, 11.97463107,
    13.28396440, 14.42763107, 15.48796440, 16.38063107, 17.44096440,
    18.41696440, 19.39396440, 20.28663107, 21.26363107, 22.24063107,
    23.13329773, 24.11029773, 25.08629773, 26.06329773, 27.12263107,
    28.01629773, 28.99329773, 29.96929773, 30.94629773, 31.92229773,
    32.89929773, 33.87529773, 34.85229773, 35.82829773, 36.72196440,
    37.69896440, 38.75829773, 39.73529773, 40.79463107, 41.77163107,
    42.74763107, 43.72463107, 44.70063107, 45.67763107, 46.57129773,
    47.54729773, 48.52429773, 49.58363107, 50.47729773, 51.45329773,
    52.43029773, 53.49063107, 54.38329773, 55.44363107, 56.33629773,
    57.31329773, 58.28929773, 59.34963107, 60.32663107, 61.21929773,
    62.19629773, 63.25563107, 64.23263107, 65.37529773, 66.26896440,
    67.24496440, 68.13863107, 69.03229773, 70.00829773, 70.90196440
  ),
  force_nN = c(
    2.588500000, 2.488500000, 2.388500000, 2.296833333, 2.196833333,
    2.096833333, 1.996833333, 1.896833333, 1.796833333, 1.705166667,
    1.605166667, 1.505166667, 1.413500000, 1.305166667, 1.196833333,
    1.080166667, 0.971833333, 0.863500000, 0.755166667, 0.663500000,
    0.563500000, 0.471833333, 0.363500000, 0.255166667, 0.155166667,
    0.046833333, -0.044833333, -0.119833333, -0.186500000, -0.261500000,
    -0.344833333, -0.411500000, -0.444833333, -0.353166667, -0.244833333,
    -0.136500000, -0.128166667, -0.119833333, -0.086500000, -0.044833333,
    -0.011500000, 0.005166667, 0.013500000, 0.005166667, 0.013500000,
    0.013500000, 0.013500000, 0.005166667, 0.005166667, 0.005166667,
    -0.003166667, -0.003166667, -0.003166667, -0.003166667, 0.005166667,
    -0.003166667, -0.003166667, -0.003166667, -0.003166667, -0.003166667,
    -0.003166667, -0.003166667, -0.003166667, -0.003166667, -0.011500000,
    -0.011500000, -0.003166667, -0.003166667, 0.005166667, 0.005166667,
    0.005166667, 0.005166667, 0.005166667, 0.005166667, -0.003166667,
    -0.003166667, -0.003166667, 0.005166667, -0.003166667, -0.003166667,
    -0.003166667, 0.005166667, -0.003166667, 0.005166667, -0.003166667,
    -0.003166667, -0.003166667, 0.005166667, 0.005166667, -0.003166667,
    -0.003166667, 0.005166667, 0.005166667, 0.021833333, 0.013500000,
    0.013500000, 0.005166667, -0.003166667, -0.003166667, -0.011500000
  )
)
rupdis = analyze_a_curve_interaction_distance(transformed_df,
                                                      y_direction = "negative", 
                                                      x_direction = "left",
                                                      baseline_span = 50,
                                                      noiseBand_low = -0.1,
                                                      noiseBand_high = 0.1,
                                                      min_consecutive = 3
                                                      )
plot(transformed_df$separation_distance_nm, transformed_df$force_nN, type = "l") + abline(v = rupdis['distance'], col= "red", lwd = 2)

repdis = analyze_a_curve_interaction_distance(transformed_df,
                                                      y_direction = "positive",
                                                      x_direction = "right",
                                                      baseline_span = 50,
                                                      noiseBand_low = -0.1,
                                                      noiseBand_high = 0.1,
                                                      min_consecutive = 3
                                                      )
plot(transformed_df$separation_distance_nm, transformed_df$force_nN, type = "l") + abline(v = repdis['distance'], col= "blue", lwd = 2)

folder <- system.file("extdata", package = "curvana")
fd_obj <- createFdObjFromFolder(folder)
fd_obj <- transform_curves(fd_obj, spring_constant = 0.1, useCurve = "retract", threads = 1, least_length= 300)
fd_obj <- analyze_curves_noise(fd_obj, useCurve = "retract", threads = 1)
fd_obj <- analyze_curves_interaction_distance(fd_obj, useCurve = "retract", baseline_span = 'automatic', x_direction = "left")

# energy calculation -----
transformed_df <- data.frame(
  separation_distance_nm = c(
    0.22196440, 0.19896440, 0.17496440, 0.23529773, 0.21129773,
    0.18829773, 0.16429773, 0.14129773, 0.11729773, 0.17763107,
    0.15463107, 0.13063107, 0.19096440, 0.08363107, -0.02270227,
    -0.21336893, -0.31970227, -0.42603560, -0.53336893, -0.47303560,
    -0.49703560, -0.43670227, -0.54403560, -0.65036893, -0.67336893,
    -0.78070227, -0.72036893, -0.49436893, -0.18403560, 0.04196440,
    0.18563107, 0.49496440, 1.13863107, 3.03229773, 5.09163107,
    7.15196440, 8.21129773, 9.27163107, 10.58096440, 11.97463107,
    13.28396440, 14.42763107, 15.48796440, 16.38063107, 17.44096440,
    18.41696440, 19.39396440, 20.28663107, 21.26363107, 22.24063107,
    23.13329773, 24.11029773, 25.08629773, 26.06329773, 27.12263107,
    28.01629773, 28.99329773, 29.96929773, 30.94629773, 31.92229773,
    32.89929773, 33.87529773, 34.85229773, 35.82829773, 36.72196440,
    37.69896440, 38.75829773, 39.73529773, 40.79463107, 41.77163107,
    42.74763107, 43.72463107, 44.70063107, 45.67763107, 46.57129773,
    47.54729773, 48.52429773, 49.58363107, 50.47729773, 51.45329773,
    52.43029773, 53.49063107, 54.38329773, 55.44363107, 56.33629773,
    57.31329773, 58.28929773, 59.34963107, 60.32663107, 61.21929773,
    62.19629773, 63.25563107, 64.23263107, 65.37529773, 66.26896440,
    67.24496440, 68.13863107, 69.03229773, 70.00829773, 70.90196440
  ),
  force_nN = c(
    2.588500000, 2.488500000, 2.388500000, 2.296833333, 2.196833333,
    2.096833333, 1.996833333, 1.896833333, 1.796833333, 1.705166667,
    1.605166667, 1.505166667, 1.413500000, 1.305166667, 1.196833333,
    1.080166667, 0.971833333, 0.863500000, 0.755166667, 0.663500000,
    0.563500000, 0.471833333, 0.363500000, 0.255166667, 0.155166667,
    0.046833333, -0.044833333, -0.119833333, -0.186500000, -0.261500000,
    -0.344833333, -0.411500000, -0.444833333, -0.353166667, -0.244833333,
    -0.136500000, -0.128166667, -0.119833333, -0.086500000, -0.044833333,
    -0.011500000, 0.005166667, 0.013500000, 0.005166667, 0.013500000,
    0.013500000, 0.013500000, 0.005166667, 0.005166667, 0.005166667,
    -0.003166667, -0.003166667, -0.003166667, -0.003166667, 0.005166667,
    -0.003166667, -0.003166667, -0.003166667, -0.003166667, -0.003166667,
    -0.003166667, -0.003166667, -0.003166667, -0.003166667, -0.011500000,
    -0.011500000, -0.003166667, -0.003166667, 0.005166667, 0.005166667,
    0.005166667, 0.005166667, 0.005166667, 0.005166667, -0.003166667,
    -0.003166667, -0.003166667, 0.005166667, -0.003166667, -0.003166667,
    -0.003166667, 0.005166667, -0.003166667, 0.005166667, -0.003166667,
    -0.003166667, -0.003166667, 0.005166667, 0.005166667, -0.003166667,
    -0.003166667, 0.005166667, 0.005166667, 0.021833333, 0.013500000,
    0.013500000, 0.005166667, -0.003166667, -0.003166667, -0.011500000
  )
)
analyze_a_curve_area(transformed_df, noiseBand_low = -0.1, noiseBand_high = 0.1)


folder <- system.file("extdata", package = "curvana")
fd_obj <- createFdObjFromFolder(folder)
fd_obj <- transform_curves(fd_obj, spring_constant = 0.1, useCurve = "retract", threads = 1, least_length= 300)
fd_obj <- analyze_curves_noise(fd_obj, useCurve = "retract", threads = 1)
fd_obj = analyze_curves_energy(fd_obj, useCurve = "retract")


# wrapper for metric calculation -----
folder <- system.file("extdata", package = "curvana")
fd_obj <- createFdObjFromFolder(folder)
fd_obj <- transform_curves(fd_obj, spring_constant = 0.1, useCurve = "retract", threads = 1, least_length= 300)
fd_obj = analyze_curves_all_analytical_metrics(
  fd_obj, # the object
  useCurve = "retract", # run analysis for both approach and retract 
  threads = 1, 

  # noise band parameters
  noise_baseline_span = "automatic", # use quantile method to define the noise band
  noise_threshold_method = "quantile", # use quantile method to define the noise band
  noise_quantile_low = 0.00, # define the lower end of noise band as minimum y value of the baseline section
  noise_quantile_high = 1, # define the upper end of noise band as maximum y value of the baseline section
  noise_multiplier = 1, # multiply the noise band by 1 to directly use the min-max range of the force in baseline region without further scaling.

  # adhesive force
  analyze_adhesive_force = TRUE, # whether to analyze adhesive force

  # adhesive energy and repulsive energy
  analyze_energy = TRUE, # whether to analyze interaction energy (repulsive and adhesive)

  # rupture (adhesive) distance
  analyze_rupture_distance = TRUE, # whether to analyze adhesive/rupture distance
  analyze_rupture_distance_baseline_span = "automatic", # use the baseline section previously defined in curve transformation for rupture distance analysis
  analyze_rupture_distance_x_direction = "left", # scan from right to left to find the first data point at which a curve enters the adhesive region from the noise band region
  analyze_rupture_distance_min_consecutive = 3, # to reduce false positives from random fluctuations, require at least 3 consecutive points below the lower noise-band threshold for classifying the curve as entering the adhesive region.

  # repulsive distance
  analyze_repulsive_distance = TRUE, # whether to analyze repulsive distance
  analyze_repulsive_distance_baseline_span = "automatic", # use the baseline section previously defined in curve transformation for repulsive distance analysis
  analyze_repulsive_distance_x_direction = "right", # scan from left to right to find the last data point before a curve first enters the noise band region from the repulsive region
  analyze_repulsive_distance_min_consecutive = 1 # because the curve is expected to start from the repulsive region, we set min_consecutive to 1, meaning that a single point above the positive noise threshold is sufficient to mark the start of the repulsive region.
)

# plot deflection curves ---
folder <- system.file("extdata", package = "curvana")
fd_obj <- createFdObjFromFolder(folder)
plot_deflection_curves(fd_obj, curve = "retract")
plot_deflection_curves_by_index(fd_obj, curve = "retract")


# plot force curves ---
folder <- system.file("extdata", package = "curvana")
fd_obj <- createFdObjFromFolder(folder)
fd_obj <- transform_curves(fd_obj, spring_constant = 0.1, useCurve = "retract", threads = 1, least_length= 300)
plot_fd_curves(fd_obj, curve = "retract")


# plot_a_curve_metrics and plot_all_curve_metrics examples
folder <- system.file("extdata", package = "curvana")
fd_obj <- createFdObjFromFolder(folder)
fd_obj <- transform_curves(fd_obj, spring_constant = 0.1, useCurve = "retract", threads = 1, least_length= 300)
fd_obj <- analyze_curves_all_analytical_metrics(fd_obj, useCurve = "retract") 
plot_a_curve_metrics(fd_obj, curve_name = 'F_1.spm-F4E1_ForceCurveIndex_1.spm', useCurve = "retract", plot_raw = TRUE)


plot_all_curve_metrics(fdobj = fd_obj, useCurve = "retract",plot_raw = T, destination_folder = '../test_plot_metrics_denoised', format = 'png', width = 18, height = 9, threads = 1 )


# Violin
folder <- system.file("extdata", package = "curvana")
fd_obj <- createFdObjFromFolder(folder)
fd_obj <- transform_curves(fd_obj, spring_constant = 0.1, useCurve = "retract", threads = 1, least_length= 300)
fd_obj <- analyze_curves_all_analytical_metrics(fd_obj, useCurve = "retract") 
md <- fd_obj@metadata
surface <- sub("_.*", "", md$filename)
surface[surface == "F"] <- "Group_A"
surface[surface == "P"] <- "Group_B"
surface[surface == "silicon"] <- "Group_C"
surface[surface == "Z"] <- "Group_D"
md$surface <- surface
fd_obj@metadata <- md
plot_metric_violin(df = fd_obj@metadata, metric_name = "adhesive_force_nN_retract",group_by = "surface",global_test = 'kruskal', pairwise_test = 'wilcox', p_adjust_method = 'BH')

# pca biplot
folder <- system.file("extdata", package = "curvana")
fd_obj <- createFdObjFromFolder(folder)
fd_obj <- transform_curves(fd_obj, spring_constant = 0.1, useCurve = "retract", threads = 1, least_length= 300)
fd_obj <- analyze_curves_all_analytical_metrics(fd_obj, useCurve = "retract") 
md <- fd_obj@metadata
surface <- sub("_.*", "", md$filename)
surface[surface == "F"] <- "Group_A"
surface[surface == "P"] <- "Group_B"
surface[surface == "silicon"] <- "Group_C"
surface[surface == "Z"] <- "Group_D"
md$surface <- surface
fd_obj@metadata <- md
plot_pca_biplot(fd_obj@metadata,include_columns = c("adhesive_force_nN_retract", "repulsive_energy_aJ_retract","adhesive_energy_aJ_retract","rupture_threshold_nN_retract"), color_by = "surface")

# result heatmap
folder <- system.file("extdata", package = "curvana")
fd_obj <- createFdObjFromFolder(folder)
fd_obj <- transform_curves(fd_obj, spring_constant = 0.1, useCurve = "retract", threads = 1, least_length= 300)
fd_obj <- analyze_curves_all_analytical_metrics(fd_obj, useCurve = "retract") 
md <- fd_obj@metadata
surface <- sub("_.*", "", md$filename)
surface[surface == "F"] <- "Group_A"
surface[surface == "P"] <- "Group_B"
surface[surface == "silicon"] <- "Group_C"
surface[surface == "Z"] <- "Group_D"
md$surface <- surface
fd_obj@metadata <- md
plot_complex_heatmap(fd_obj@metadata,include_columns = c("adhesive_force_nN_retract", "repulsive_energy_aJ_retract","adhesive_energy_aJ_retract","rupture_threshold_nN_retract"), annotate_columns = "surface")

# raw heatmap
folder <- system.file("extdata", package = "curvana")
fd_obj <- createFdObjFromFolder(folder)
md <- fd_obj@metadata
surface <- sub("_.*", "", md$filename)
surface[surface == "F"] <- "Group_A"
surface[surface == "P"] <- "Group_B"
surface[surface == "silicon"] <- "Group_C"
surface[surface == "Z"] <- "Group_D"
md$surface <- surface
fd_obj@metadata <- md
plot_raw_deflection_heatmap(fd_obj,annotate_columns = "surface")

# read jpk file
read_jpk_file(system.file("extdata_jpk", "jpk1.txt", package = "curvana"))


# extract from fdobj
folder <- system.file("extdata", package = "curvana")
fd_obj <- createFdObjFromFolder(folder)
md <- fd_obj@metadata
surface <- sub("_.*", "", md$filename)
surface[surface == "F"] <- "Group_A"
surface[surface == "P"] <- "Group_B"
surface[surface == "silicon"] <- "Group_C"
surface[surface == "Z"] <- "Group_D"
md$surface <- surface
fd_obj@metadata <- md
fd_obj2 = extract(fd_obj,by_col = list(surface = 'Group_A'))
fd_obj3 = extract(fd_obj,by_sample = 'F_1.spm-F4E1_ForceCurveIndex_0.spm')

# combine two fdobjs
folder <- system.file("extdata", package = "curvana")
fd_obj <- createFdObjFromFolder(folder)
md <- fd_obj@metadata
surface <- sub("_.*", "", md$filename)
surface[surface == "F"] <- "Group_A"
surface[surface == "P"] <- "Group_B"
surface[surface == "silicon"] <- "Group_C"
surface[surface == "Z"] <- "Group_D"
md$surface <- surface
fd_obj@metadata <- md
fd_obj2 = extract(fd_obj,by_col = list(surface = 'Group_B'))
fd_obj3 = extract(fd_obj,by_sample = 'F_1.spm-F4E1_ForceCurveIndex_0.spm')
fd_obj4 = combineFdObj(fd_obj2, fd_obj3)
