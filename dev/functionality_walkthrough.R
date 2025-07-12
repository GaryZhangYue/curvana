library(dplyr)
# initalize
# Install devtools if not already
#install.packages("devtools")

# Load devtools
library(devtools)

# Install all dependencies and load the package
# devtools::install(dependencies = TRUE)
# devtools::load_all()

# Load necessary libraries
library(dplyr)
library(ggplot2)

# test: creating fdOBJ
folder = system.file("extdata", package = "fdafmR")
test = createFdObjFromFolder(folder = folder)
test = createFdObjFromFolder(folder = folder,threads = 2)

# test: the extract function
test@metadata$date = sub(pattern = '_.*','',test@metadata$filename)
test@metadata$bacteria = sub(".*f?_(.*)_loc.*", "\\1", test@metadata$filename)

test.paeru = extract(test,by_col = list(bacteria = "paeru"))
test.paeru2 = extract(test,by_sample = rownames(test@metadata[test@metadata$bacteria == 'paeru',]))
test.pmon = extract(test,by_col = list(bacteria = 'pmon2'))

# test: combine
test.combine.1 = combineFdObj(test.paeru,test.pmon)
test.combine.1@metadata
test.combine.2 = combineFdObj(test.paeru,test.paeru2) # should return error

# test: plot_deflection_curves
plot_deflection_curves(test,split_curves_by = 'bacteria')
plot_deflection_curves(test,split_curves_by = 'bacteria',group_curves_by = 'date',
                alpha = 0.05,point_size = 0.5,
                color_map = list(`180424` = 'darkred',
                                 `180425` = 'blue'))
plot_deflection_curves(test,split_curves_by = 'date',group_curves_by = 'bacteria',
                alpha = 0.3,point_size = 0.5,
                color_map = list(`paeru` = 'darkred',
                                 `pmon2` = 'blue',
                                 sc = 'orange',
                                 mica = 'lightblue'))

plot_deflection_curves(test,curve = 'approach', split_curves_by = 'date',group_curves_by = 'bacteria',
                       alpha = 0.3,point_size = 0.5,
                       color_map = list(`paeru` = 'darkred',
                                        `pmon2` = 'blue',
                                        sc = 'orange',
                                        mica = 'lightblue'))
plot_deflection_curves(test,curve = 'retract', split_curves_by = 'date',group_curves_by = 'bacteria',
                       alpha = 0.3,point_size = 0.5,
                       color_map = list(`paeru` = 'darkred',
                                        `pmon2` = 'blue',
                                        sc = 'orange',
                                        mica = 'lightblue'))
# test: calc_sensitivity
x = test@rawCurves$`180424_fc_mica_loc1.001`$Calc_Ramp_Ex_nm
y = test@rawCurves$`180424_fc_mica_loc1.001`$Defl_V_Ex
calc_sensitivity(end = 200,intv = 4,x = x,y = y)


# test: analyze_sensitivity
test = analyze_sensitivity(test,useCurve = 'approach',threads = 1)
test = analyze_sensitivity(test,useCurve = 'retract',threads = 1)
test = analyze_sensitivity(test,useCurve = 'approach',threads = 10)

# test: find_baseline
x = test@rawCurves$`180424_fc_mica_loc1.001`$Calc_Ramp_Ex_nm
y = test@rawCurves$`180424_fc_mica_loc1.001`$Defl_V_Ex
sens = calc_sensitivity(end = 200,intv = 4,x = x,y = y)$sensitivity

tmp = find_baseline(x = x, y = y, least_length = 150, sensitivity = sens,slp_threshold = 0.001,std_threshold = 0.005)
tmp$segment
tmp$baseline

# test: analyze_baseline
tmp2 = extract(test,by_sample = '180424_fc_mica_loc1.001')
tmp2 = analyze_baseline(tmp2,useCurve = 'approach',least_length = 150)
tmp2 = analyze_baseline(tmp2,useCurve = 'retract',least_length = 150)
test = analyze_baseline(test,useCurve = 'approach',least_length = 100,threads = 2)
test = analyze_baseline(test,useCurve = 'retract',least_length = 100,threads = 2)

test@metadata
test@baseline_segment$approach
test@baseline_segment$retract
test = analyze_baseline(test,useCurve = 'retract',least_length = 100,threads = 2)
test@metadata
test@baseline_segment$retract

# test: transform_a_curve
x = test@rawCurves$`180424_fc_mica_loc1.001`$Calc_Ramp_Ex_nm
y = test@rawCurves$`180424_fc_mica_loc1.001`$Defl_V_Ex
baseline = test@metadata['180424_fc_mica_loc1.001', 'baseline_nm_approach']
sens.res = calc_sensitivity(end = 200,intv = 4,x = x,y = y)
sensitivity = sens.res$sensitivity
senscal_seg_x = sens.res$senscal_segment$x
senscal_seg_y = sens.res$senscal_segment$y
curve.fd = transform_a_curve(x = x, y= y, baseline = baseline, sensitivity = sensitivity,
                             spring_constant = 0.3188, senscal_seg_x = senscal_seg_x,
                             senscal_seg_y = senscal_seg_y)
ggplot(curve.fd, aes(x = separation_distance_nm, y = force_nN)) +
  geom_point(size = 1, alpha = 1) +
  labs(x = "Distance (nm)", y = "Force (nN)")

test@metadata
x = test@rawCurves$`180424_fv_pmon2_loc1-18-22.spm`$Calc_Ramp_Rt_nm
y = test@rawCurves$`180424_fv_pmon2_loc1-18-22.spm`$Defl_V_Rt
baseline = test@metadata['180424_fv_pmon2_loc1-18-22.spm', 'baseline_nm_retract']
sensitivity = test@metadata['180424_fv_pmon2_loc1-18-22.spm', 'sensitivity_V_nm_retract']
senscal_seg_x = test@senscal_segment$retract$`180424_fv_pmon2_loc1-18-22.spm`$Calc_Ramp_Rt_nm
senscal_seg_y = test@senscal_segment$retract$`180424_fv_pmon2_loc1-18-22.spm`$Defl_V_Rt
curve.fd = transform_a_curve(x = x, y= y, baseline = baseline, sensitivity = sensitivity,
                             spring_constant = 0.3188, senscal_seg_x = senscal_seg_x,
                             senscal_seg_y = senscal_seg_y)
ggplot(curve.fd, aes(x = separation_distance_nm, y = force_nN)) +
  geom_point(size = 1, alpha = 1) +
  labs(x = "Distance (nm)", y = "Force (nN)")

# test: transform_curves
test = transform_curves(test,spring_constant = 0.3188,useCurve = 'approach',threads = 1)
test@approachCurves

ggplot(test@approachCurves[[2]], aes(x = separation_distance_nm, y = force_nN)) +
  geom_point(size = 1, alpha = 1) +
  labs(x = "Distance (nm)", y = "Force (nN)")

test = transform_curves(test,spring_constant = 0.3188,useCurve = 'retract',threads = 1)
test@retractCurves

ggplot(test@retractCurves[[5]], aes(x = separation_distance_nm, y = force_nN)) +
  geom_point(size = 1, alpha = 1) +
  labs(x = "Distance (nm)", y = "Force (nN)")

# test: plot_fd_curves
plot_fd_curves(test,split_curves_by = 'bacteria')
plot_fd_curves(test,split_curves_by = 'bacteria',group_curves_by = 'date',
                       alpha = 0.05,point_size = 0.5,
                       color_map = list(`180424` = 'darkred',
                                        `180425` = 'blue'))
plot_fd_curves(test,split_curves_by = 'date',group_curves_by = 'bacteria',
                       alpha = 0.3,point_size = 0.5,
                       color_map = list(`paeru` = 'darkred',
                                        `pmon2` = 'blue',
                                        sc = 'orange',
                                        mica = 'lightblue'))

plot_fd_curves(test,curve = 'approach', split_curves_by = 'date',group_curves_by = 'bacteria',
                       alpha = 0.3,point_size = 0.5,
                       color_map = list(`paeru` = 'darkred',
                                        `pmon2` = 'blue',
                                        sc = 'orange',
                                        mica = 'lightblue'))
plot_fd_curves(test,curve = 'retract', split_curves_by = 'date',group_curves_by = 'bacteria',
                       alpha = 0.3,point_size = 0.5,
                       color_map = list(`paeru` = 'darkred',
                                        `pmon2` = 'blue',
                                        sc = 'orange',
                                        mica = 'lightblue'))

test2 = extract(test,by_col = list(bacteria = 'paeru'))
plot_fd_curves(test2,split_curves_by = 'bacteria',group_curves_by = 'bacteria',
               alpha = 0.3,point_size = 0.5,
               color_map = list(`paeru` = 'darkred',
                                `pmon2` = 'blue',
                                sc = 'orange',
                                mica = 'lightblue'))
