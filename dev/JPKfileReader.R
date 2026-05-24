
library(ggpubr)
library(afmToolkit)
library(dplyr)
library(devtools)

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

test_result = read_jpk_file(jpk_file)

# Check the raw data
head(test_result$raw_data)
ggscatter(data = test_result$raw_data, x='Calc_Ramp_Rt_nm', y='Defl_V_Rt')

# Check the segment-specific parameters
print(test_result$parameters)
# Should show:
# approach_sensitivity_imported, approach_springConstant_imported,
# retract_sensitivity_imported, retract_springConstant_imported


