# Designed to evaluate RSHP sensitivity to alternative CCC thresholds.

library(tidyverse)

localdir <- '/Volumes/MaloneLab/Research/FluxGradient/FluxData'
DirRepo.eval <- "/Users/sm3466/YSE Dropbox/Sparkle Malone/Research/FluxGradient/lterwg-flux-gradient-eval"

load(file = fs::path(localdir, paste0("SITES_One2One_canopy_model_AA_AW.Rdata")))
load(fs::path(localdir, paste0("SITE_DATA_FILTERED_CCC_AA_AW.Rdata")))
source(fs::path(DirRepo.eval, "./functions/calc_validation.R"))
source(fs::path(DirRepo.eval, "./functions/calc.linear.terms.R"))
source(fs::path(DirRepo.eval, "./functions/calc.lins.ccc.R"))

sensitivity.thresholds <- tibble(
  threshold.workflow = c("CCC.0.3", "CCC.0.5", "CCC.0.7"),
  ccc_cutoff = c(0.3, 0.5, 0.7),
  threshold_label = c("CCC >= 0.3", "CCC >= 0.5", "CCC >= 0.7"),
  ecological_relevance = c(
    "Screen for directional consistency while retaining broad site coverage.",
    "Balance directional agreement and magnitude fidelity for RSHP screening.",
    "Restrict RSHP to highly concordant pairs that preserve diel dynamics."
  ),
  daily_sign_agreement = c(
    "Useful when the primary question is whether daytime uptake versus nighttime release is preserved.",
    "Appropriate when both the sign and most day-to-day transitions should agree.",
    "Reserved for cases where sign reversals should be rare across the daily cycle."
  ),
  mean_flux_bias_tolerance = c(
    "Highest tolerance; suitable when broad mean flux differences from EC are acceptable.",
    "Moderate tolerance; suitable when mean flux differences should stay bounded.",
    "Lowest tolerance; suitable when mean flux differences should remain close to EC benchmarks."
  ),
  diel_shape_retention = c(
    "Retains a recognizable diel pattern but allows muted peaks and broader phase error.",
    "Retains the main diel structure, including timing and relative peak-trough contrast.",
    "Retains diel shape most faithfully, including amplitude and phase."
  )
)

normalize_diel_cycle <- function(x) {
  scale_factor <- max(abs(x), na.rm = TRUE)
  if (!is.finite(scale_factor) || scale_factor == 0) {
    return(rep(NA_real_, length(x)))
  }
  x / scale_factor
}

calc_fg_ec_pair_metrics <- function(df_pair) {
  subset_df <- df_pair %>%
    select(time.rounded, EC_mean, FG_mean) %>%
    filter(is.finite(EC_mean), is.finite(FG_mean))

  if (nrow(subset_df) == 0) {
    return(tibble(
      n_obs = 0,
      daily_sign_agreement = NA_real_,
      mean_flux_bias = NA_real_,
      mean_absolute_flux_bias = NA_real_,
      diel_shape_correlation = NA_real_,
      diel_shape_rmse = NA_real_
    ))
  }

  diel_metrics <- subset_df %>%
    mutate(hour = lubridate::hour(time.rounded)) %>%
    reframe(
      .by = hour,
      fg_diel = mean(FG_mean, na.rm = TRUE),
      ec_diel = mean(EC_mean, na.rm = TRUE)
    ) %>%
    mutate(
      fg_diel_norm = normalize_diel_cycle(fg_diel),
      ec_diel_norm = normalize_diel_cycle(ec_diel)
    ) %>%
    drop_na(fg_diel_norm, ec_diel_norm)

  diel_shape_correlation <- if (nrow(diel_metrics) >= 3) {
    cor(diel_metrics$fg_diel_norm, diel_metrics$ec_diel_norm)
  } else {
    NA_real_
  }

  diel_shape_rmse <- if (nrow(diel_metrics) >= 3) {
    sqrt(mean((diel_metrics$fg_diel_norm - diel_metrics$ec_diel_norm)^2))
  } else {
    NA_real_
  }

  tibble(
    n_obs = nrow(subset_df),
    daily_sign_agreement = mean(sign(subset_df$FG_mean) == sign(subset_df$EC_mean)),
    mean_flux_bias = mean(subset_df$FG_mean - subset_df$EC_mean, na.rm = TRUE),
    mean_absolute_flux_bias = mean(abs(subset_df$FG_mean - subset_df$EC_mean), na.rm = TRUE),
    diel_shape_correlation = diel_shape_correlation,
    diel_shape_rmse = diel_shape_rmse
  )
}

build_fg_ec_threshold_metrics <- function(canopy_data, ccc_cutoff) {
  pair_metrics <- purrr::map_dfr(names(SITE_DATA_FILTERED_CCC), function(site) {
    df_site <- SITE_DATA_FILTERED_CCC[[site]] %>%
      filter(gas != "CH4")

    canopy.sub <- canopy_data %>%
      filter(Site == site) %>%
      select(Site, dLevelsAminusB, gas, Approach, CCC.EC) %>%
      distinct()

    retained <- df_site %>%
      left_join(canopy.sub, by = c("Site", "dLevelsAminusB", "gas", "Approach"), relationship = "many-to-many") %>%
      filter(CCC.EC >= ccc_cutoff) %>%
      mutate(time.rounded = lubridate::round_date(timeEndA.local, unit = "30 minutes")) %>%
      reframe(
        .by = c(Site, gas, Approach, dLevelsAminusB, time.rounded),
        EC_mean = mean(EC_mean, na.rm = TRUE),
        FG_mean = mean(FG_mean, na.rm = TRUE)
      )

    if (nrow(retained) == 0) {
      return(tibble())
    }

    retained %>%
      group_by(Site, gas, Approach, dLevelsAminusB) %>%
      group_modify(~ calc_fg_ec_pair_metrics(.x)) %>%
      ungroup()
  })

  if (nrow(pair_metrics) == 0) {
    return(tibble(
      gas = factor(levels = c("CO2", "H2O")),
      pairs_with_metrics = integer(),
      daily_sign_agreement = numeric(),
      mean_flux_bias = numeric(),
      mean_absolute_flux_bias = numeric(),
      diel_shape_correlation = numeric(),
      diel_shape_rmse = numeric()
    ))
  }

  pair_metrics %>%
    reframe(
      .by = gas,
      pairs_with_metrics = n(),
      measured_daily_sign_agreement = mean(daily_sign_agreement, na.rm = TRUE),
      measured_mean_flux_bias = mean(mean_flux_bias, na.rm = TRUE),
      measured_mean_absolute_flux_bias = mean(mean_absolute_flux_bias, na.rm = TRUE),
      measured_diel_shape_correlation = mean(diel_shape_correlation, na.rm = TRUE),
      measured_diel_shape_rmse = mean(diel_shape_rmse, na.rm = TRUE)
    )
}

build_rshp_threshold_summary <- function(threshold_row, canopy_data, pair_fn) {
  threshold.workflow <- threshold_row$threshold.workflow[[1]]
  ccc_cutoff <- threshold_row$ccc_cutoff[[1]]

  val.SHP <- pair_fn(threshold = threshold.workflow) %>%
    mutate(
      Approach.1 = sapply(strsplit(var1, "-"), `[`, 1),
      level.1 = sapply(strsplit(var1, "-"), `[`, 2),
      Approach.2 = sapply(strsplit(var2, "-"), `[`, 1),
      level.2 = sapply(strsplit(var2, "-"), `[`, 2),
      Combination = paste(var1, var2, sep = "-")
    )

  val.SHP.long <- bind_rows(
    val.SHP %>%
      select(Site, CCC, var1, gas, Approach.1, level.1, Combination) %>%
      rename(var = var1, CCC.GF = CCC, Approach = Approach.1, dLevelsAminusB = level.1),
    val.SHP %>%
      select(Site, CCC, var2, gas, Approach.2, level.2, Combination) %>%
      rename(var = var2, CCC.GF = CCC, Approach = Approach.2, dLevelsAminusB = level.2)
  ) %>%
    mutate(Approach = factor(Approach, levels = c("MBR", "AE", "WP"))) %>%
    distinct() %>%
    na.omit()

  canopy.sub <- canopy_data %>%
    select(Site, Approach, gas, Canopy_L1, dLevelsAminusB, CCC.EC) %>%
    distinct()

  fg_ec_metrics <- build_fg_ec_threshold_metrics(canopy_data = canopy_data, ccc_cutoff = ccc_cutoff)

  val.SHP.long %>%
    full_join(canopy.sub, by = c("Site", "Approach", "gas", "dLevelsAminusB")) %>%
    mutate(Good.CCC = case_when(CCC.EC >= ccc_cutoff ~ 1, CCC.EC < ccc_cutoff ~ 0) %>% as.factor()) %>%
    reframe(
      .by = gas,
      threshold.workflow = threshold.workflow,
      ccc_cutoff = ccc_cutoff,
      threshold_label = threshold_row$threshold_label[[1]],
      ecological_relevance = threshold_row$ecological_relevance[[1]],
      daily_sign_agreement = threshold_row$daily_sign_agreement[[1]],
      mean_flux_bias_tolerance = threshold_row$mean_flux_bias_tolerance[[1]],
      diel_shape_retention = threshold_row$diel_shape_retention[[1]],
      n_pairs = sum(!is.na(CCC.EC)),
      n_rshp = sum(as.numeric(as.character(Good.CCC)), na.rm = TRUE),
      prop_rshp = mean(as.numeric(as.character(Good.CCC)), na.rm = TRUE),
      mean_ccc_ec = mean(CCC.EC, na.rm = TRUE),
      mean_ccc_gf = mean(CCC.GF, na.rm = TRUE)
    ) %>%
    left_join(fg_ec_metrics, by = "gas")
}

canopy <- SITES_One2One_model %>%
  select(Site, rf_model, predicted, Good.CCC, CCC, dLevelsAminusB, Approach, gas, Canopy_L1) %>%
  rename(CCC.EC = CCC)

SITES_One2One_canopy_model <- SITES_One2One_model

rshp.threshold.sensitivity <- bind_rows(lapply(seq_len(nrow(sensitivity.thresholds)), function(i) {
  build_rshp_threshold_summary(
    threshold_row = sensitivity.thresholds[i, ],
    canopy_data = canopy,
    pair_fn = Compare.SamplingHeightPairs
  )
}))

rshp.threshold.sensitivity <- rshp.threshold.sensitivity %>%
  mutate(
    threshold_label = factor(threshold_label, levels = c("CCC >= 0.3", "CCC >= 0.5", "CCC >= 0.7")),
    gas = factor(gas, levels = c("CO2", "H2O"))
  )

rshp.threshold.summary.table <- rshp.threshold.sensitivity %>%
  mutate(
    pct_rshp = round(prop_rshp * 100, 1),
    mean_ccc_ec = round(mean_ccc_ec, 3),
    mean_ccc_gf = round(mean_ccc_gf, 3),
    measured_daily_sign_agreement = round(measured_daily_sign_agreement, 3),
    measured_mean_flux_bias = round(measured_mean_flux_bias, 3),
    measured_mean_absolute_flux_bias = round(measured_mean_absolute_flux_bias, 3),
    measured_diel_shape_correlation = round(measured_diel_shape_correlation, 3),
    measured_diel_shape_rmse = round(measured_diel_shape_rmse, 3),
    cutoff_performance = case_when(
      ccc_cutoff == 0.3 ~ "Permissive: broad retention, weaker FG-EC screening.",
      ccc_cutoff == 0.5 ~ "Balanced: strongest tradeoff between retention and concordance.",
      ccc_cutoff == 0.7 ~ "Conservative: highest concordance, lowest retention."
    ),
    fg_ec_interpretation = case_when(
      ccc_cutoff == 0.3 ~ "Best for directional consistency, but too permissive for a primary FG-versus-EC cutoff.",
      ccc_cutoff == 0.5 ~ "Best working threshold for FG-versus-EC comparison in AA_AW.",
      ccc_cutoff == 0.7 ~ "Useful only when a very strict, shape-faithful FG-versus-EC subset is needed."
    )
  ) %>%
  select(
    gas,
    threshold_label,
    n_pairs,
    n_rshp,
    pairs_with_metrics,
    pct_rshp,
    mean_ccc_ec,
    mean_ccc_gf,
    daily_sign_agreement,
    measured_daily_sign_agreement,
    mean_flux_bias_tolerance,
    measured_mean_flux_bias,
    measured_mean_absolute_flux_bias,
    diel_shape_retention,
    measured_diel_shape_correlation,
    measured_diel_shape_rmse,
    cutoff_performance,
    fg_ec_interpretation
  ) %>%
  arrange(gas, threshold_label)

rshp.threshold.manuscript.table <- rshp.threshold.summary.table %>%
  transmute(
    Gas = gas,
    Threshold = threshold_label,
    `RSHP retained (%)` = pct_rshp,
    `FG-EC sign agreement` = measured_daily_sign_agreement,
    `Mean flux bias (FG - EC)` = measured_mean_flux_bias,
    `Mean absolute flux bias` = measured_mean_absolute_flux_bias
  )

methods.section <- paste(
  "To identify an operational concordance threshold for defining reliable sampling-height pairs (RSHP),",
  "we performed a sensitivity analysis across three candidate cutoffs for flux-gradient versus eddy covariance (EC) agreement:",
  "`CCC >= 0.3`, `CCC >= 0.5`, and `CCC >= 0.7`.",
  "For each threshold, we reran the AA_AW RSHP workflow and summarized both the fraction of candidate sampling-height pairs retained",
  "and the resulting agreement between retained gradient fluxes and EC.",
  "Daily sign agreement was quantified as the fraction of retained half-hourly intervals for which `sign(FG_mean) == sign(EC_mean)`.",
  "Mean flux bias was quantified as the average difference between retained gradient and EC fluxes, `mean(FG_mean - EC_mean)`,",
  "and mean absolute flux bias as `mean(abs(FG_mean - EC_mean))`.",
  "We interpreted the lowest threshold (`CCC >= 0.3`) as a permissive screen emphasizing directional consistency,",
  "the highest threshold (`CCC >= 0.7`) as a conservative screen emphasizing low mean flux bias,",
  "and the intermediate threshold (`CCC >= 0.5`) as a candidate balance point.",
  "We selected the working threshold based on the strongest tradeoff between retaining enough RSHP for analysis and improving sign agreement,",
  "mean flux bias, and mean absolute flux bias relative to EC.",
  sep = " "
)

markdown_table <- {
  header <- paste(names(rshp.threshold.manuscript.table), collapse = " | ")
  divider <- paste(rep("---", ncol(rshp.threshold.manuscript.table)), collapse = " | ")
  rows <- apply(rshp.threshold.manuscript.table, 1, function(x) paste(x, collapse = " | "))
  c(
    "# RSHP Threshold Methods",
    "",
    methods.section,
    "",
    "# RSHP Threshold Table",
    "",
    paste0("| ", header, " |"),
    paste0("| ", divider, " |"),
    paste0("| ", rows, " |")
  )
}

plot.n_rshp <- ggplot(rshp.threshold.sensitivity, aes(x = threshold_label, y = n_rshp, fill = gas)) +
  geom_col(position = position_dodge(width = 0.75), width = 0.65) +
  geom_text(aes(label = n_rshp), position = position_dodge(width = 0.75), vjust = -0.3, size = 3) +
  scale_fill_manual(values = c("CO2" = "#1b9e77", "H2O" = "#377eb8")) +
  labs(x = "CCC threshold", y = "RSHP count", fill = "Gas", title = "RSHP sensitivity by CCC threshold") +
  theme_bw()

plot.prop_rshp <- ggplot(rshp.threshold.sensitivity, aes(x = threshold_label, y = prop_rshp, color = gas, group = gas)) +
  geom_line(linewidth = 0.8) +
  geom_point(size = 2.5) +
  geom_text(aes(label = sprintf("%.2f", prop_rshp)), vjust = -0.7, size = 3, show.legend = FALSE) +
  scale_color_manual(values = c("CO2" = "#1b9e77", "H2O" = "#377eb8")) +
  labs(x = "CCC threshold", y = "RSHP fraction", color = "Gas", title = "Fraction of candidate pairs retained") +
  theme_bw() +
  ylim(0, 1)

plot.path <- fs::path(DirRepo.eval, "Figures/WF_Version1/RSHP_threshold_sensitivity_AA_AW.png")
ggsave(plot.path, ggpubr::ggarrange(plot.n_rshp, plot.prop_rshp, ncol = 2, common.legend = TRUE), width = 11, height = 4.8, units = "in")

write.csv(
  rshp.threshold.sensitivity,
  fs::path(localdir, "/RSHP_threshold_sensitivity_AA_AW.csv"),
  row.names = FALSE
)

write.csv(
  rshp.threshold.summary.table,
  fs::path(localdir, "/RSHP_threshold_summary_table_AA_AW.csv"),
  row.names = FALSE
)

write.csv(
  rshp.threshold.manuscript.table,
  fs::path(localdir, "/RSHP_threshold_manuscript_table_AA_AW.csv"),
  row.names = FALSE
)

writeLines(
  methods.section,
  fs::path(localdir, "/RSHP_threshold_methods_AA_AW.md")
)

writeLines(
  markdown_table,
  fs::path(localdir, "/RSHP_threshold_methods_and_table_AA_AW.md")
)

fileSave <- fs::path(localdir, "/SITE_RSHP_threshold_sensitivity_AA_AW.Rdata")
save(
  rshp.threshold.sensitivity,
  rshp.threshold.summary.table,
  rshp.threshold.manuscript.table,
  methods.section,
  sensitivity.thresholds,
  file = fileSave
)
