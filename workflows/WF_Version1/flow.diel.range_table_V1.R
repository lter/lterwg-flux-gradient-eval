if (!exists("run.tag")) run.tag <- "_AA_AW_2021_2026" # output-name tag; set in flow.evaluation.batch_V1.R ("_AA_AW" = preprint run)
if (!exists("localdir")) localdir <- "/Volumes/MaloneLab/Research/FluxGradient"
if (!exists("DirRepo.eval")) DirRepo.eval <- "/Users/sm3466/Library/CloudStorage/Dropbox-YSE/Sparkle Malone/Research/FluxGradient/lterwg-flux-gradient-eval"

library(tidyverse)

DirRepo.eval <- "/Users/sm3466/Library/CloudStorage/Dropbox-YSE/Sparkle Malone/Research/FluxGradient/lterwg-flux-gradient-eval"
input_file <-paste0("/Volumes/MaloneLab/Research/FluxGradient/NEON_GradientFlux_Ensemble/DIEL_SUMMARY_ENSEMBLE_V1", run.tag, ".RDATA")
output_dir <- fs::path(DirRepo.eval, "Figures", "WF_Version1")

load(input_file)
fs::dir_create(output_dir)

season_order <- c("Winter", "Spring", "Summer", "Autumn")

season_summary <- ENSEMBLE_DIELS %>%
  filter(!is.na(site), !is.na(season)) %>%
  distinct(site, season) %>%
  mutate(season = factor(season, levels = season_order)) %>%
  arrange(site, season) %>%
  reframe(
    .by = site,
    Season = paste(as.character(season), collapse = ", ")
  )

overall_ranges <- ENSEMBLE_DIELS %>%
  filter(
    !is.na(site),
    !is.na(gas),
    !is.na(FG),
    !is.na(EC),
    is.finite(FG),
    is.finite(EC)
  ) %>%
  reframe(
    .by = c(site, gas),
    GF_overall_range = sprintf("%.2f to %.2f", min(FG, na.rm = TRUE), max(FG, na.rm = TRUE)),
    EC_overall_range = sprintf("%.2f to %.2f", min(EC, na.rm = TRUE), max(EC, na.rm = TRUE))
  ) %>%
  pivot_wider(
    names_from = gas,
    values_from = c(GF_overall_range, EC_overall_range),
    names_glue = "{gas}_{.value}"
  )

diel_range_table <- season_summary %>%
  left_join(overall_ranges, by = "site") %>%
  arrange(site)

write.csv(
  diel_range_table,
  file = fs::path(output_dir, paste0("DIEL_ENSEMBLE_site_gas_flux_ranges", run.tag, ".csv")),
  row.names = FALSE
)

readr::write_tsv(
  diel_range_table,
  file = fs::path(output_dir, paste0("DIEL_ENSEMBLE_site_gas_flux_ranges", run.tag, ".tsv"))
)

