library(dplyr)
library(ggplot2)
library(readr)

figdir <- "/Users/sm3466/YSE Dropbox/Sparkle Malone/Research/FluxGradient/lterwg-flux-gradient-eval/Figures/WF_Version1"

site_slopes_all <- read_csv(
  file.path(figdir, "delta_z_all_canopy_site_slope_plot_data.csv"),
  show_col_types = FALSE
) %>%
  mutate(
    gas = factor(gas, levels = c("CO2", "H2O")),
    Approach = factor(Approach, levels = c("MBR", "AE", "WP")),
    Canopy_L2 = factor(Canopy_L2, levels = c("AA", "AA+", "AW", "AW-", "AW+", "AW+-"))
  ) %>%
  filter(Canopy_L1 != "WW", !is.na(Canopy_L2), !is.na(canopyHeight_m), !is.na(CCC_slope))

plot_slope_canopy_all <- ggplot(
  site_slopes_all,
  aes(x = canopyHeight_m, y = CCC_slope, color = gas, shape = Approach)
) +
  geom_hline(yintercept = 0, linetype = "dashed", color = "grey50") +
  geom_point(size = 2.2, alpha = 0.85) +
  geom_smooth(
    data = site_slopes_all,
    aes(x = canopyHeight_m, y = CCC_slope, color = gas, group = gas),
    method = "lm",
    se = FALSE,
    linewidth = 0.8,
    inherit.aes = FALSE
  ) +
  facet_wrap(~ Canopy_L2, scales = "free_y") +
  scale_color_manual(values = c("CO2" = "#000CCC", "H2O" = "#009966")) +
  scale_shape_manual(values = c("MBR" = 16, "AE" = 17, "WP" = 15)) +
  labs(
    x = "Canopy Height (m)",
    y = expression(paste("CCC slope versus ", Delta, "z")),
    color = "Gas",
    shape = "Approach"
  ) +
  theme_bw() +
  theme(
    legend.position = "top",
    strip.background = element_rect(fill = "grey95", colour = "grey80"),
    panel.grid.minor = element_blank()
  )

ggsave(
  file.path(figdir, "delta_z_all_canopy_CCC_slope_vs_canopy_height.png"),
  plot = plot_slope_canopy_all,
  width = 12,
  height = 8,
  units = "in"
)
