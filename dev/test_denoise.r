library(dplyr)
library(purrr)
# Create sample 4-column raw curve
set.seed(42)
n_points <- 10
raw_curve <- data.frame(
  Calc_Ramp_Ex_nm = seq(0, 10, 1),
  Calc_Ramp_Rt_nm = rev(seq(0, 10,1)),
  Defl_V_Ex = seq(0, 10, 1),  # noisy data
  Defl_V_Rt = seq(0, 20, 2)   # noisy data
)


folder = system.file("extdata", package = "curvana")
test = createFdObjFromFolder(folder = folder)
library(dplyr)
library(purrr)
library(ggplot2)

raw_curve <- test@rawCurves$`Z_3.spm-310E_ForceCurveIndex_9.spm`

params <- expand.grid(
  p = c(1, 2, 3),
  n = c(3, 5)
) %>%
  filter(n > p)

plot_df_denoised <- params %>%
  mutate(
    denoised_df = pmap(list(p, n), ~ denoise_a_curve(
      raw_curve = raw_curve,
      useCurve = "retract",
      p = ..1,
      n = ..2
    )),
    label = paste0("p=", p, ", n=", n)
  ) %>%
  mutate(
    df = pmap(list(denoised_df, label, p, n), function(res, lbl, pp, nn) {
      data.frame(
        distance = res$Calc_Ramp_Rt_nm,
        force = res$Defl_V_Rt,
        label = lbl,
        p = pp,
        n = nn
      )
    })
  ) %>%
  pull(df) %>%
  bind_rows()

plot_df_raw <- data.frame(
  distance = raw_curve$Calc_Ramp_Rt_nm,
  force = raw_curve$Defl_V_Rt,
  label = "raw",
  p = NA,
  n = NA
)

plot_df_all <- bind_rows(plot_df_raw, plot_df_denoised)

fig = ggplot(plot_df_all, aes(x = distance, y = force, color = label)) +
  geom_line(linewidth = 0.3) +
  theme_classic() +
  labs(
    title = "Retract curve denoising comparison",
    x = "Distance (nm)",
    y = "Deflection (V)",
    color = "Curve"
  )+
  coord_cartesian(ylim = c(-0.205, -0.21))
plotly
