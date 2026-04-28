library(tidyverse)

DirRepo.eval <- "/Users/sm3466/YSE Dropbox/Sparkle Malone/Research/FluxGradient/lterwg-flux-gradient-eval"
localdir <- "/Volumes/MaloneLab/Research/FluxGradient/FluxData"

load(fs::path(localdir, "SITE_DATA_ENSEMBLE_V1_AA_AW.Rdata"))

output_dir <- fs::path(DirRepo.eval, "Figures", "WF_Version1")
fs::dir_create(output_dir)

site_density_data <- purrr::imap_dfr(SITE_DATA_ENSEMBLE, function(df_site, site_name) {
  if (is.null(df_site) || nrow(df_site) == 0) {
    return(tibble())
  }

  df_site %>%
    mutate(Site = site_name) %>%
    pivot_longer(
      cols = c(FG_ENSEMBLE, EC_mean),
      names_to = "Method",
      values_to = "Flux"
    ) %>%
    mutate(
      Method = recode(Method, FG_ENSEMBLE = "GF", EC_mean = "EC"),
      gas = factor(gas, levels = c("CO2", "H2O"))
    ) %>%
    filter(!is.na(Flux), is.finite(Flux))
})

site_density_summary <- site_density_data %>%
  reframe(
    .by = c(Site, gas, Method),
    n = n(),
    mean_flux = mean(Flux, na.rm = TRUE),
    median_flux = median(Flux, na.rm = TRUE),
    sd_flux = sd(Flux, na.rm = TRUE)
  ) %>%
  arrange(gas, Site, Method)

write.csv(
  site_density_summary,
  file = fs::path(output_dir, "SITE_DATA_ENSEMBLE_site_density_summary_AA_AW.csv"),
  row.names = FALSE
)

plot_density_by_gas <- function(gas_name, x_label) {
  plot_data <- site_density_data %>%
    filter(gas == gas_name) %>%
    group_by(Site, Method) %>%
    filter(n() >= 2) %>%
    ungroup()

  mean_data <- site_density_summary %>%
    filter(gas == gas_name, n >= 2)

  ggplot(plot_data, aes(x = Flux, color = Method, fill = Method)) +
    geom_density(alpha = 0.18, linewidth = 0.5, adjust = 1.1) +
    geom_vline(
      data = mean_data,
      aes(xintercept = mean_flux, color = Method),
      linewidth = 0.45,
      linetype = "dashed",
      show.legend = FALSE
    ) +
    facet_wrap(~Site, scales = "free", ncol = 5) +
    scale_color_manual(values = c(GF = "#D95F02", EC = "#1B9E77")) +
    scale_fill_manual(values = c(GF = "#D95F02", EC = "#1B9E77")) +
    labs(
      title = paste(gas_name, "Flux Distributions by Site"),
      subtitle = "Overlapping density curves compare GF and EC; dashed lines mark method means",
      x = x_label,
      y = "Density"
    ) +
    theme_bw(base_size = 10) +
    theme(
      legend.position = "top",
      panel.grid.minor = element_blank(),
      strip.background = element_rect(fill = "grey95", color = "grey70")
    )
}

plot_co2 <- plot_density_by_gas("CO2", expression(Flux~"("*mu*"mol m"^-2*" s"^-1*")"))
plot_h2o <- plot_density_by_gas("H2O", expression(Flux~"(mmol m"^-2*" s"^-1*")"))

ggsave(
  filename = fs::path(output_dir, "SITE_DATA_ENSEMBLE_density_by_site_CO2_AA_AW.png"),
  plot = plot_co2,
  width = 13,
  height = 18,
  units = "in",
  dpi = 300
)

ggsave(
  filename = fs::path(output_dir, "SITE_DATA_ENSEMBLE_density_by_site_H2O_AA_AW.png"),
  plot = plot_h2o,
  width = 13,
  height = 18,
  units = "in",
  dpi = 300
)
