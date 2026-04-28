library(dplyr)
library(ggplot2)
library(readr)

figdir <- "/Users/sm3466/YSE Dropbox/Sparkle Malone/Research/FluxGradient/lterwg-flux-gradient-eval/Figures/WF_Version1"

site_slopes_all <- read_csv(
  file.path(figdir, "delta_z_all_canopy_site_trend_detail.csv"),
  show_col_types = FALSE
) %>%
  mutate(
    Site = factor(Site),
    Approach = factor(Approach, levels = c("MBR", "AE", "WP")),
    gas = factor(gas, levels = c("CO2", "H2O")),
    Canopy_L2 = factor(Canopy_L2, levels = c("AA", "AA+", "AW", "AW-", "AW+", "AW+-"))
  ) %>%
  filter(Canopy_L1 != "WW", !is.na(Canopy_L2), !is.na(CCC_slope)) %>%
  arrange(Canopy_L2, desc(canopyHeight_m), Site) %>%
  group_by(Canopy_L2) %>%
  mutate(SiteCanopy = factor(
    paste(Site, sprintf("(%.1fm)", canopyHeight_m)),
    levels = unique(paste(Site, sprintf("(%.1fm)", canopyHeight_m)))
  )) %>%
  ungroup()

plot_site_slopes_all <- ggplot(
  site_slopes_all,
  aes(x = CCC_slope, y = SiteCanopy, color = slope_sign, shape = Approach)
)

site_slopes_all <- site_slopes_all %>%
  mutate(slope_sign = CCC_slope > 0)

plot_site_slopes_all <- ggplot(
  site_slopes_all,
  aes(x = CCC_slope, y = SiteCanopy, color = slope_sign, shape = Approach)
) +
  geom_vline(xintercept = 0, linetype = "dashed", color = "grey50") +
  geom_point(size = 2.2, alpha = 0.9) +
  facet_grid(Canopy_L2 ~ gas, scales = "free_y", space = "free_y") +
  scale_color_manual(values = c("TRUE" = "darkmagenta", "FALSE" = "goldenrod3")) +
  scale_shape_manual(values = c("MBR" = 16, "AE" = 17, "WP" = 15)) +
  labs(
    x = expression(paste("CCC slope versus ", Delta, "z")),
    y = "Site (Canopy Height)",
    color = "Direction",
    shape = "Approach"
  ) +
  theme_bw() +
  theme(
    legend.position = "top",
    strip.background = element_rect(fill = "grey95", colour = "grey80"),
    panel.grid.minor = element_blank(),
    axis.text.y = element_text(size = 7)
  )

write_csv(site_slopes_all, file.path(figdir, "delta_z_all_canopy_site_slope_plot_data.csv"))

ggsave(
  file.path(figdir, "delta_z_all_canopy_site_slopes.png"),
  plot = plot_site_slopes_all,
  width = 11,
  height = 16,
  units = "in"
)
