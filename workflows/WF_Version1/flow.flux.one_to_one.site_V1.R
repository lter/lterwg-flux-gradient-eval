library(tidyverse)

DirRepo.eval <- "/Users/sm3466/YSE Dropbox/Sparkle Malone/Research/FluxGradient/lterwg-flux-gradient-eval"
localdir <- "/Volumes/MaloneLab/Research/FluxGradient/FluxData"

load(fs::path(localdir, "SITE_DATA_ENSEMBLE_V1_AA_AW.Rdata"))

output_dir <- fs::path(DirRepo.eval, "Figures", "WF_Version1")
fs::dir_create(output_dir)

site_scatter_data <- purrr::imap_dfr(SITE_DATA_ENSEMBLE, function(df_site, site_name) {
  if (is.null(df_site) || nrow(df_site) == 0) {
    return(tibble())
  }

  df_site %>%
    mutate(Site = site_name) %>%
    select(Site, gas, FG_ENSEMBLE, EC_mean) %>%
    filter(
      !is.na(FG_ENSEMBLE),
      !is.na(EC_mean),
      is.finite(FG_ENSEMBLE),
      is.finite(EC_mean)
    ) %>%
    mutate(gas = factor(gas, levels = c("CO2", "H2O")))
})

site_r2_summary <- site_scatter_data %>%
  reframe(
    .by = c(Site, gas),
    n = n(),
    r2 = if (n() >= 2) summary(lm(EC_mean ~ FG_ENSEMBLE))$r.squared else NA_real_,
    label = if (n() >= 2) paste0("R^2 == ", format(round(r2, 2), nsmall = 2)) else "R^2 == NA"
  ) %>%
  arrange(gas, Site)

write.csv(
  site_r2_summary,
  file = fs::path(output_dir, "SITE_DATA_ENSEMBLE_one_to_one_R2_summary_AA_AW.csv"),
  row.names = FALSE
)

plot_one_to_one_by_gas <- function(gas_name, x_label, y_label) {
  plot_data <- site_scatter_data %>%
    filter(gas == gas_name)

  label_data <- site_r2_summary %>%
    filter(gas == gas_name)

  ggplot(plot_data, aes(x = EC_mean, y = FG_ENSEMBLE)) +
    geom_abline(intercept = 0, slope = 1, color = "black", linetype = "dashed", linewidth = 0.4) +
    geom_point(color = "black", alpha = 0.35, size = 0.45) +
    geom_smooth(method = "lm", se = FALSE, color = "#D95F02", linewidth = 0.5) +
    geom_text(
      data = label_data,
      aes(label = label),
      x = -Inf,
      y = Inf,
      hjust = -0.1,
      vjust = 1.1,
      parse = TRUE,
      inherit.aes = FALSE,
      size = 2.7
    ) +
    facet_wrap(~Site, scales = "free", ncol = 5) +
    labs(
      title = paste(gas_name, "EC vs GF by Site"),
      subtitle = "Dashed line shows the 1:1 relationship; orange line is the linear fit",
      x = x_label,
      y = y_label
    ) +
    theme_bw(base_size = 10) +
    theme(
      legend.position = "none",
      panel.grid.minor = element_blank(),
      strip.background = element_rect(fill = "grey95", color = "grey70")
    )
}

plot_co2 <- plot_one_to_one_by_gas(
  "CO2",
  expression(EC ~ "(" * mu * "mol m"^-2 * " s"^-1 * ")"),
  expression(GF ~ "(" * mu * "mol m"^-2 * " s"^-1 * ")")
)
plot_h2o <- plot_one_to_one_by_gas(
  "H2O",
  expression(EC ~ "(mmol m"^-2 * " s"^-1 * ")"),
  expression(GF ~ "(mmol m"^-2 * " s"^-1 * ")")
)

ggsave(
  filename = fs::path(output_dir, "SITE_DATA_ENSEMBLE_one_to_one_by_site_CO2_AA_AW.png"),
  plot = plot_co2,
  width = 13,
  height = 18,
  units = "in",
  dpi = 300
)

ggsave(
  filename = fs::path(output_dir, "SITE_DATA_ENSEMBLE_one_to_one_by_site_H2O_AA_AW.png"),
  plot = plot_h2o,
  width = 13,
  height = 18,
  units = "in",
  dpi = 300
)
