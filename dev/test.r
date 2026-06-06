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
