# Create sample 4-column raw curve
set.seed(42)
n_points <- 10
raw_curve <- data.frame(
  Calc_Ramp_Ex_nm = seq(0, 10, 1),
  Calc_Ramp_Rt_nm = rev(seq(0, 10,1)),
  Defl_V_Ex = seq(0, 10, 1),  # noisy data
  Defl_V_Rt = seq(0, 20, 2)   # noisy data
)

# Test denoising retract curve
denoised_retract <- denoise_a_curve(raw_curve, useCurve = "retract", p = 1, n = 3)
head(denoised_retract)
# Should show: Calc_Ramp_Rt_nm, Defl_V_Rt, (other cols), Defl_V_Rt_original

# Test denoising approach curve
denoised_approach <- denoise_a_curve(raw_curve, useCurve = "approach", p = 3, n = 5)
head(denoised_approach)
# Should show: Calc_Ramp_Ex_nm, Defl_V_Ex, (other cols), Defl_V_Ex_original

# Test denoising both
denoised_both <- denoise_a_curve(raw_curve, useCurve = "both", p = 3, n = 5)
head(denoised_both)
# Should show: Calc_Ramp_Ex_nm, Calc_Ramp_Rt_nm, Defl_V_Ex, Defl_V_Rt, Defl_V_Ex_original, Defl_V_Rt_original

# Verify column order (4 core cols first, then originals)
colnames(denoised_both)