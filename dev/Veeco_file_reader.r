
load_all()
test = createFdObjFromFolder(folder = "dev/input_files/veeco",
                                      suffix = ".txt",
                                      Displacement_Approach = "Calc_Ramp_Ex_nm", 
                                      Deflection_Approach = "Defl_pN_Ex")


View(test@rawCurves[[1]])


test = createFdObjFromFolder(folder = "dev/input_files/bruker_with_NA",
                                      suffix = ".txt")

test2 = denoise_curves(test,useCurve = 'both')
View(test2@rawCurves[[1]])
View(test@rawCurves[[1]])
load_all()

test3 = transform_curves(test,spring_constant = 0.8, useCurve = 'approach')
test3 = transform_curves(test3,spring_constant = 0.8, useCurve = 'retract')
nrow(test3@approachCurves[[1]])
nrow(test3@retractCurves[[1]])
test3@metadata

test3 = analyze_curves_all_analytical_metrics(test3, useCurve = 'both')
plot_a_curve_metrics(test3,useCurve = 'approach',plot_raw = T,curve_name = rownames(test@metadata)[1],
)
