library(dplyr)
library(readr)
library(tidyr)

localdir <- "/Volumes/MaloneLab/Research/FluxGradient/FluxData"
figdir <- "/Users/sm3466/YSE Dropbox/Sparkle Malone/Research/FluxGradient/lterwg-flux-gradient-eval/Figures/WF_Version1"

load(file.path(localdir, "SITES_One2One_AA_AW.Rdata"))

delta_z_site_detail <- SITES_One2One %>%
  mutate(delta_z = abs(MeasurementDist)) %>%
  filter(Canopy_L2 == "AA+") %>%
  select(Site, gas, Approach, dLevelsAminusB, delta_z, CCC, R2) %>%
  drop_na(delta_z, CCC, R2) %>%
  group_by(Site, gas, Approach) %>%
  group_modify(~ {
    d <- .x

    if (nrow(d) < 3 || dplyr::n_distinct(d$delta_z) < 2) {
      return(tibble(
        n_pairs = nrow(d),
        delta_z_min = min(d$delta_z),
        delta_z_max = max(d$delta_z),
        CCC_mean = mean(d$CCC),
        R2_mean = mean(d$R2),
        CCC_slope = NA_real_,
        CCC_p = NA_real_,
        CCC_rho = NA_real_,
        R2_slope = NA_real_,
        R2_p = NA_real_,
        R2_rho = NA_real_
      ))
    }

    ccc_fit <- summary(lm(CCC ~ delta_z, data = d))$coefficients["delta_z", ]
    r2_fit <- summary(lm(R2 ~ delta_z, data = d))$coefficients["delta_z", ]

    tibble(
      n_pairs = nrow(d),
      delta_z_min = min(d$delta_z),
      delta_z_max = max(d$delta_z),
      CCC_mean = mean(d$CCC),
      R2_mean = mean(d$R2),
      CCC_slope = unname(ccc_fit["Estimate"]),
      CCC_p = unname(ccc_fit["Pr(>|t|)"]),
      CCC_rho = suppressWarnings(cor(d$delta_z, d$CCC, method = "spearman")),
      R2_slope = unname(r2_fit["Estimate"]),
      R2_p = unname(r2_fit["Pr(>|t|)"]),
      R2_rho = suppressWarnings(cor(d$delta_z, d$R2, method = "spearman"))
    )
  }) %>%
  ungroup() %>%
  mutate(
    abs_CCC_slope = abs(CCC_slope),
    abs_R2_slope = abs(R2_slope)
  ) %>%
  arrange(desc(abs_CCC_slope), desc(abs_R2_slope), Site, gas, Approach)

delta_z_site_summary <- delta_z_site_detail %>%
  filter(!is.na(abs_CCC_slope) | !is.na(abs_R2_slope)) %>%
  group_by(Site) %>%
  summarise(
    combos_tested = n(),
    strongest_CCC_change = max(abs_CCC_slope, na.rm = TRUE),
    strongest_CCC_combo = paste(gas[which.max(abs_CCC_slope)], Approach[which.max(abs_CCC_slope)], sep = " / "),
    strongest_CCC_direction = if_else(CCC_slope[which.max(abs_CCC_slope)] < 0, "decrease", "increase"),
    strongest_R2_change = max(abs_R2_slope, na.rm = TRUE),
    strongest_R2_combo = paste(gas[which.max(abs_R2_slope)], Approach[which.max(abs_R2_slope)], sep = " / "),
    strongest_R2_direction = if_else(R2_slope[which.max(abs_R2_slope)] < 0, "decrease", "increase"),
    delta_z_min = min(delta_z_min, na.rm = TRUE),
    delta_z_max = max(delta_z_max, na.rm = TRUE),
    .groups = "drop"
  ) %>%
  arrange(desc(strongest_CCC_change), desc(strongest_R2_change), Site)

write_csv(delta_z_site_detail, file.path(figdir, "delta_z_AAplus_site_trend_detail.csv"))
write_csv(delta_z_site_summary, file.path(figdir, "delta_z_AAplus_site_trend_summary.csv"))
