
library(ggpubr)
library(afmToolkit)
library(dplyr)
library(devtools)
load_all()

# NOTE: We now use a custom JPK reader (read_jpk_file in utils.R) instead of afmToolkit::afmReadJPK
# The custom reader:
# - Automatically detects extend (approach) and retract segments
# - Extracts sensitivity and spring constant SEPARATELY for each segment
# - Handles unit conversion from Force (N) to Voltage (V) if needed
# - Returns data in curvana's standard format

jpk_file <- 'dev/input_files/JPK/force-save-JPK-2h.txt'

# Old approach (for comparison):
test = afmReadJPK(jpk_file,ZColStr = "Height (measured & smoothed)", FColStr = "Vertical Deflection")
View(test$data)
ggscatter(data = test$data, x='Z', y='Force', facet.by = 'Segment')

# New approach:
load_all()

test.cvn = read_jpk_file(jpk_file,height_col = 'smoothedStrainGaugeHeight',deflection_col = 'vDeflection')

# Check the raw data
head(test.cvn$raw_data)
ggscatter(data = test.cvn$raw_data, x='Calc_Ramp_Rt_nm', y='Defl_V_Rt')

test.cvn$converted_data_retract <- test.cvn$raw_data %>%
transmute(
  # Convert from nm to m
  Z = Calc_Ramp_Rt_nm * 1e-9,
  Force = Defl_V_Rt * test.cvn$parameters[['retract_sensitivity_imported']] * test.cvn$parameters[['retract_springConstant_imported']], # Convert from V to N using retract sensitivity
  # Add segment label
  Segment = 'retract'
)
test.cvn$converted_data_approach <- test.cvn$raw_data %>%
transmute(
  # Convert from nm to m
  Z = Calc_Ramp_Ex_nm * 1e-9,
  Force = Defl_V_Ex * test.cvn$parameters[['approach_sensitivity_imported']] * test.cvn$parameters[['approach_springConstant_imported']], # Convert from V to N using approach sensitivity
  # Add segment label
  Segment = 'approach'
)
test.cvn$converted_data <- bind_rows(test.cvn$converted_data_approach, test.cvn$converted_data_retract)


# check the number of rows in the converted data
##  it looks like afmReadJPK misses the last row in approach
nrow(test.cvn$converted_data) # Should be the same as nrow(test.cvn$raw_data) * 2 (since we have approach and retract)
nrow(test.cvn$raw_data) # Should be the same as nrow(test.cvn$converted_data) / 2
nrow(test$data) # it looks like afmReadJPK misses the last row in approach

# check the numbers -- All matched
sum((test.cvn$converted_data_approach[1:613,'Z'] - test$data[1:613,'Z'])*1e9) # should be close to 0 (after converting from m to nm)
sum((test.cvn$converted_data_retract[,'Z'] - test$data[614:1227,'Z'])*1e9) # should be close to 0 (after converting from m to nm)
sum((test.cvn$converted_data_approach[1:613,'Force'] - test$data[1:613,'Force'])) # should be close to 0
sum((test.cvn$converted_data_retract[,'Force'] - test$data[614:1227,'Force'])) # should be close to 0

# Check single-segment data import
jpk_file <- 'dev/input_files/JPK/force-save-JPK-2h_ext_only.txt'
test = read_jpk_file(jpk_file,height_col = 'smoothedStrainGaugeHeight',deflection_col = 'vDeflection')
test$raw_data %>% names
test$parameters

jpk_file <- 'dev/input_files/JPK/force-save-JPK-2h_rt_only.txt'
test = read_jpk_file(jpk_file,height_col = 'smoothedStrainGaugeHeight',deflection_col = 'vDeflection')
test$raw_data %>% names
test$parameters

# Check different segment-length data import
jpk_file <- 'dev/input_files/JPK/force-save-JPK-2h_ext5lines.txt'
test = read_jpk_file(jpk_file,height_col = 'smoothedStrainGaugeHeight',deflection_col = 'vDeflection')
test$raw_data %>% names
test$parameters

jpk_file <- 'dev/input_files/JPK/force-save-JPK-2h_rt5lines.txt'
test = read_jpk_file(jpk_file,height_col = 'smoothedStrainGaugeHeight',deflection_col = 'vDeflection')
test$raw_data %>% names
test$parameters

# Check the order of datapoints in segments
jpk_file <- 'dev/input_files/JPK/force-save-JPK-2h.txt'
test = read_jpk_file(jpk_file,height_col = 'smoothedStrainGaugeHeight',deflection_col = 'vDeflection')
ggpubr::ggscatter(test$raw_data, x='Calc_Ramp_Ex_nm', y='Defl_V_Ex') + ggtitle('Approach segment')
ggpubr::ggscatter(test$raw_data, x='Calc_Ramp_Rt_nm', y='Defl_V_Rt') + ggtitle('Retract segment')


# check batch read with multiple files
jpk_dir <- c('dev/input_files/JPK')
fdobj.jpk <- createFdObjFromJPKFolder(folder = jpk_dir,
                                     suffix = ".txt",
                                     pattern = "",
                                     metadata = NULL,
                                     threads = 1,
                                     height_col = "height",
                                     deflection_col = "vDeflection",
                                     reverse_Displacement_Approach = FALSE,
                                     reverse_Displacement_Retract = FALSE,
                                     reverse_Deflection_Approach = FALSE,
                                     reverse_Deflection_Retract = FALSE)

fdobj.jpk@metadata$sample = rownames(fdobj.jpk@metadata)

plot_deflection_curves(
  fdobj.jpk,
  curve = c("both"),
  split_curves_by = 'sample',
  point_size = 0.5,
  alpha = 0.5,
  line_alpha = 0.5,
  color_map = NULL
)
fdobj.jpk.s = extract(fdobj.jpk,by_sample = c('force-save-JPK-2h','force-save-JPK-3h'))


plot_deflection_curves(
  fdobj.jpk.s,
  curve = c("both"),
  split_curves_by = 'sample',
  point_size = 0.5,
  alpha = 0.5,
  line_alpha = 0.5,
  color_map = NULL
)

fdobj.jpk.s = extract(fdobj.jpk,by_sample = c('force-save-JPK-2h'))
fdobj.jpk.s = transform_curves(fdobj.jpk.s,
                        useCurve = 'approach', # analyze the approach curve

                        denoise_first = TRUE, # whether to denoise raw curves before transformation. Denoising can improve the reliability of sensitivity and baseline calculations, especially for noisy curves. The smoothed curve will be used for downstream analysis, while the original raw curve will be preserved in the rawCurves slot for reference.
                        p = 1, n = 3, # parameters for the Savitzky-Golay filter used for denoising. p is the polynomial order (default 1, meaning linear), and n is the window size (default 3, meaning each point is smoothed by fitting a polynomial to itself and its 2 neighbors). Users can adjust these parameters based on curve noise and shape. This setting is equivalent to a simple moving average.
                        
                        spring_constant = 0.08, # spring constant; can be a single numeric value applied to all curves, or the name of a metadata column containing per-curve spring constant values.

                        # baseline determination and filtering
                        least_length = 50, # use the last (right side) 300 points as baseline
                        slp_threshold = 0.2, # any curve with a baseline-region slope > 0.01 will be discarded
                        std_threshold = 1, # any curve with a baseline-region standard deviation of y values > 0.01 will be discarded

                        # sensitivity calibration
                        intv = 10, # number of data points added per iteration when building the linear sensitivity segment
                        end = 100, # use at most the first 20 data points on the left side of the curve to calculate sensitivity
                        R_squared_min = 0.5, # minimum R^2 threshold for accepting a segment as sufficiently linear during sensitivity calculation
                        minimum_length = 10, # minimum number of accumulated points required for a valid sensitivity result; if shorter, sensitivity is reported as NA
                        threads = 1 # number of threads to use for parallel processing; set to 1 for no parallelization; increase for faster processing of large datasets (e.g., > 5000 curves)
                        )


y <- fdobj.jpk.s@rawCurves$`force-save-JPK-2h`$Defl_V_Ex[564:614] 
x <- fdobj.jpk.s@rawCurves$`force-save-JPK-2h`$Calc_Ramp_Ex_nm[564:614] 

plot(x, y)

model <- lm(y ~ x)
summary(model)          # includes slope, R², p-values, etc.
