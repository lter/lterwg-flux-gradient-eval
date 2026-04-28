library(dplyr)
library(tidyr)
library(ggplot2)
library(readr)

localdir <- "/Volumes/MaloneLab/Research/FluxGradient/FluxData"
figdir <- "/Users/sm3466/YSE Dropbox/Sparkle Malone/Research/FluxGradient/lterwg-flux-gradient-eval/Figures/WF_Version1"

load(file.path(localdir, "SITES_One2One_AA_AW.Rdata"))

delta_z_eval <- SITES_One2One %>%
  mutate(delta_z = abs(MeasurementDist)) %>%
  filter(Canopy_L2 %in% c("AA", "AA+")) %>%
  select(Site, gas, Approach, Canopy_L2, dLevelsAminusB, delta_z, CCC, R2) %>%
  pivot_longer(
    cols = c(CCC, R2),
    names_to = "metric",
    values_to = "value"
  ) %>%
  mutate(
    metric = factor(metric, levels = c("CCC", "R2")),
    Canopy_L2 = factor(Canopy_L2, levels = c("AA", "AA+")),
    gas = factor(gas, levels = c("CO2", "H2O")),
    Approach = factor(Approach, levels = c("MBR", "AE", "WP"))
  ) %>%
  drop_na(delta_z, value)

plot_delta_z <- ggplot(
  delta_z_eval,
  aes(x = delta_z, y = value, color = Canopy_L2, shape = Canopy_L2)
) +
  geom_point(alpha = 0.65, size = 2) +
  geom_smooth(method = "lm", se = FALSE, linewidth = 0.9) +
  facet_grid(metric ~ gas + Approach) +
  scale_color_manual(values = c("AA" = "goldenrod3", "AA+" = "darkmagenta")) +
  scale_shape_manual(values = c("AA" = 16, "AA+" = 17)) +
  scale_y_continuous(limits = c(-1, 1)) +
  labs(
    x = expression(paste(Delta, "z (m)")),
    y = "Performance",
    color = "Pair",
    shape = "Pair"
  ) +
  theme_bw() +
  theme(
    legend.position = "top",
    legend.title = element_blank(),
    strip.background = element_rect(fill = "grey95", colour = "grey80"),
    panel.grid.minor = element_blank()
  )

site_levels <- delta_z_eval %>%
  distinct(Site) %>%
  arrange(Site) %>%
  pull(Site)

site_groups <- split(site_levels, ceiling(seq_along(site_levels) / 12))

write_csv(delta_z_eval, file.path(figdir, "delta_z_panel_data_AA_AAplus.csv"))

ggsave(
  file.path(figdir, "delta_z_CCC_R2_AA_AAplus.png"),
  plot = plot_delta_z,
  width = 12,
  height = 6,
  units = "in"
)

for (i in seq_along(site_groups)) {
  plot_site <- delta_z_eval %>%
    filter(Site %in% site_groups[[i]]) %>%
    ggplot(aes(x = delta_z, y = value, color = Canopy_L2, shape = Canopy_L2)) +
    geom_point(alpha = 0.7, size = 1.8) +
    geom_smooth(method = "lm", se = FALSE, linewidth = 0.7) +
    facet_grid(Site ~ metric + gas + Approach) +
    scale_color_manual(values = c("AA" = "goldenrod3", "AA+" = "darkmagenta")) +
    scale_shape_manual(values = c("AA" = 16, "AA+" = 17)) +
    scale_y_continuous(limits = c(-1, 1)) +
    labs(
      x = expression(paste(Delta, "z (m)")),
      y = "Performance",
      color = "Pair",
      shape = "Pair"
    ) +
    theme_bw() +
    theme(
      legend.position = "top",
      legend.title = element_blank(),
      strip.background = element_rect(fill = "grey95", colour = "grey80"),
      panel.grid.minor = element_blank(),
      axis.text.x = element_text(angle = 45, hjust = 1),
      strip.text.y = element_text(angle = 0)
    )

  ggsave(
    file.path(figdir, paste0("delta_z_CCC_R2_AA_AAplus_by_site_page", i, ".png")),
    plot = plot_site,
    width = 16,
    height = 18,
    units = "in"
  )
}
