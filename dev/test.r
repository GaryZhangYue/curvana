tmp.name = 'F_2.spm-6034_ForceCurveIndex_27.spm'
tmp.transformed = test@retractCurves[[tmp.name]]
ggplot(tmp.transformed, aes(x = separation_distance_nm, y = force_nN)) +
  geom_point() + geom_path()
plot_a_curve_metrics(test,tmp.name,useCurve = 'retract')
