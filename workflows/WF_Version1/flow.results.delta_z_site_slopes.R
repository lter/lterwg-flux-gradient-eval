library(dplyr)
library(tidyr)
library(ggplot2)
library(readr)

localdir <- "/Volumes/MaloneLab/Research/FluxGradient/FluxData"
figdir <- "/Users/sm3466/YSE Dropbox/Sparkle Malone/Research/FluxGradient/lterwg-flux-gradient-eval/Figures/WF_Version1"

load(file.path(localdir, "SITES_One2One_AA_AW.Rdata"))

site_height <- SITES_One2One %>%
  select(Site, canopyHeight_m) %>%
  distinct() %>%
  group_by(Site) %>%
  summarise(canopyHeight_m = mean(canopyHeight_m, na.rm = TRUE), .groups = "drop") %>%
  arrange(desc(canopyHeight_m), Site)

site_slopes <- read_csv(
  file.path(figdir, "delta_z_AAplus_site_trend_detail.csv"),
  show_col_types = FALSE
) %>%
  select(
    Site, gas, Approach, n_pairs, delta_z_min, delta_z_max,
    CCC_slope, CCC_p, R2_slope, R2_p
  ) %>%
  pivot_longer(
    cols = c(CCC_slope, R2_slope),
    names_to = "metric",
    values_to = "slope"
  ) %>%
  mutate(
    p_value = case_when(
      metric == "CCC_slope" ~ CCC_p,
      metric == "R2_slope" ~ R2_p
    ),
    metric = recode(metric, CCC_slope = "CCC", R2_slope = "R2"),
    Site = factor(Site, levels = site_height$Site),
    Approach = factor(Approach, levels = c("MBR", "AE", "WP"))
  ) %>%
  left_join(site_height, by = "Site") %>%
  filter(metric == "CCC", !is.na(slope))

plot_site_slopes <- ggplot(
  site_slopes,
  aes(x = slope, y = Site, color = slope > 0, shape = Approach)
) +
  geom_vline(xintercept = 0, linetype = "dashed", color = "grey50") +
  geom_point(size = 2.8, alpha = 0.9) +
  facet_grid(. ~ gas, scales = "free_x") +
  scale_color_manual(values = c("TRUE" = "darkmagenta", "FALSE" = "goldenrod3")) +
  scale_shape_manual(values = c("MBR" = 16, "AE" = 17, "WP" = 15)) +
  labs(
    x = expression(paste("Slope of metric versus ", Delta, "z")),
    y = "Site",
    color = "Direction",
    shape = "Approach"
  ) +
  theme_bw() +
  theme(
    legend.position = "top",
    strip.background = element_rect(fill = "grey95", colour = "grey80"),
    panel.grid.minor = element_blank()
  )

write_csv(site_slopes, file.path(figdir, "delta_z_AAplus_site_slope_plot_data.csv"))

ggsave(
  file.path(figdir, "delta_z_AAplus_site_slopes.png"),
  plot = plot_site_slopes,
  width = 10,
  height = 5,
  units = "in"
)
