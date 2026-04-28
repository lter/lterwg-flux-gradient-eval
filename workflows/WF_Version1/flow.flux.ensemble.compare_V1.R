library(tidyverse)
library(lubridate)

DirRepo.eval <- "/Users/sm3466/YSE Dropbox/Sparkle Malone/Research/FluxGradient/lterwg-flux-gradient-eval"
localdir <- "/Volumes/MaloneLab/Research/FluxGradient/FluxData"

source(fs::path(DirRepo.eval, "functions/calc.lins.ccc.R"))

load(fs::path(localdir, "SITE_DATA_FILTERED_CCC_AA_AW.Rdata"))
load(fs::path(localdir, "SITE_RSHP_AA_AW.Rdata"))

safe_inverse <- function(x, power = 1, fallback = 0) {
  out <- rep(fallback, length(x))
  keep <- !is.na(x) & is.finite(x) & x > 0
  out[keep] <- 1 / (x[keep]^power)
  out
}

weighted_median <- function(x, w) {
  keep <- !is.na(x) & !is.na(w) & is.finite(x) & is.finite(w) & (w > 0)
  x <- x[keep]
  w <- w[keep]

  if (length(x) == 0) {
    return(NA_real_)
  }

  ord <- order(x)
  x <- x[ord]
  w <- w[ord]
  cutoff <- sum(w) * 0.5
  x[which(cumsum(w) >= cutoff)[1]]
}

weighted_mean_or_mean <- function(x, w) {
  keep <- !is.na(x) & is.finite(x)
  x <- x[keep]
  w <- w[keep]

  if (length(x) == 0) {
    return(NA_real_)
  }

  w[!is.finite(w) | is.na(w) | w < 0] <- 0
  if (sum(w) <= 0) {
    return(mean(x, na.rm = TRUE))
  }

  weighted.mean(x, w = w, na.rm = TRUE)
}

calc_site_metrics <- function(df) {
  df <- df %>% drop_na(EC_mean, FG_ENSEMBLE)

  if (nrow(df) <= 20) {
    return(tibble(n_halfhours = nrow(df), R2 = NA_real_, CCC = NA_real_, RMSE = NA_real_))
  }

  fit <- summary(lm(EC_mean ~ FG_ENSEMBLE, data = df))
  ccc_fit <- calc.lins.ccc(df$EC_mean, df$FG_ENSEMBLE)
  rmse_fit <- sqrt(mean((df$EC_mean - df$FG_ENSEMBLE)^2, na.rm = TRUE))

  tibble(
    n_halfhours = nrow(df),
    R2 = fit$r.squared,
    CCC = ccc_fit$rho.c$est,
    RMSE = rmse_fit
  )
}

# RSHP index from the RSHP workflow. Good.CCC == 1 is equivalent to CCC.EC >= 0.5.
rshp_index <- val.SHP.total.canopy.summary %>%
  filter(Good.CCC == 1) %>%
  distinct(Site, gas, Approach, dLevelsAminusB, Canopy_L1)

# Global method reliabilities used by method-specific and Bayesian weighting.
method_priors <- bind_rows(SITE_DATA_FILTERED_CCC) %>%
  inner_join(rshp_index, by = c("Site", "gas", "Approach", "dLevelsAminusB", "Canopy_L1")) %>%
  filter(CCC >= 0.5, Canopy_L1 != "WW") %>%
  reframe(
    .by = c(gas, Approach),
    mean_rmse = mean(RMSE, na.rm = TRUE),
    mean_r2 = mean(R2, na.rm = TRUE),
    mean_ccc = mean(CCC, na.rm = TRUE)
  ) %>%
  mutate(
    method_weight_rmse = safe_inverse(mean_rmse, power = 1, fallback = NA_real_),
    method_weight_r2 = if_else(is.finite(mean_r2) & !is.na(mean_r2) & mean_r2 > 0, mean_r2, 0),
    method_weight_bayes = safe_inverse(mean_rmse, power = 2, fallback = NA_real_)
  )

ensemble_site_results <- list()
ensemble_timeseries <- list()

for (site in names(SITE_DATA_FILTERED_CCC)) {
  message("Processing site: ", site)

  df_site <- SITE_DATA_FILTERED_CCC[[site]] %>%
    inner_join(rshp_index, by = c("Site", "gas", "Approach", "dLevelsAminusB", "Canopy_L1")) %>%
    left_join(method_priors, by = c("gas", "Approach")) %>%
    mutate(
      time.rounded = round_date(timeEndA.local, unit = "30 minutes"),
      obs_weight_rmse = safe_inverse(RMSE, power = 1, fallback = 0),
      obs_weight_var = safe_inverse(RMSE, power = 2, fallback = 0),
      method_weight = coalesce(method_weight_rmse, 0),
      bayes_weight = coalesce(method_weight_bayes, 0) * obs_weight_var
    ) %>%
    filter(CCC >= 0.5, Canopy_L1 != "WW") %>%
    distinct()

  if (nrow(df_site) == 0) {
    next
  }

  df_ensemble <- df_site %>%
    reframe(
      .by = c(gas, time.rounded),
      n_pairs = n(),
      EC_mean = mean(EC_mean, na.rm = TRUE),
      mean_reliable = mean(FG_mean, na.rm = TRUE),
      median_reliable = median(FG_mean, na.rm = TRUE),
      weighted_median_uncertainty = weighted_median(FG_mean, obs_weight_rmse),
      inverse_rmse_weighted = weighted_mean_or_mean(FG_mean, obs_weight_rmse),
      method_specific_weighted = weighted_mean_or_mean(FG_mean, method_weight),
      bayesian_fusion = weighted_mean_or_mean(FG_mean, bayes_weight)
    ) %>%
    pivot_longer(
      cols = c(
        mean_reliable,
        median_reliable,
        weighted_median_uncertainty,
        inverse_rmse_weighted,
        method_specific_weighted,
        bayesian_fusion
      ),
      names_to = "ensemble_method",
      values_to = "FG_ENSEMBLE"
    ) %>%
    mutate(Site = site)

  ensemble_timeseries[[site]] <- df_ensemble

  site_stats <- df_ensemble %>%
    group_by(Site, gas, ensemble_method) %>%
    group_modify(~ calc_site_metrics(.x)) %>%
    ungroup()

  ensemble_site_results[[site]] <- site_stats
}

ensemble_site_metrics <- bind_rows(ensemble_site_results)
ensemble_timeseries_all <- bind_rows(ensemble_timeseries)

ensemble_r2_summary <- ensemble_site_metrics %>%
  reframe(
    .by = c(gas, ensemble_method),
    n_site_gas = sum(!is.na(R2)),
    mean_R2 = mean(R2, na.rm = TRUE),
    median_R2 = median(R2, na.rm = TRUE),
    sd_R2 = sd(R2, na.rm = TRUE),
    mean_CCC = mean(CCC, na.rm = TRUE),
    median_CCC = median(CCC, na.rm = TRUE),
    sd_CCC = sd(CCC, na.rm = TRUE),
    mean_RMSE = mean(RMSE, na.rm = TRUE),
    median_RMSE = median(RMSE, na.rm = TRUE),
    sd_RMSE = sd(RMSE, na.rm = TRUE)
  ) %>%
  bind_rows(
    ensemble_site_metrics %>%
      reframe(
        .by = ensemble_method,
        gas = "Overall",
        n_site_gas = sum(!is.na(R2)),
        mean_R2 = mean(R2, na.rm = TRUE),
        median_R2 = median(R2, na.rm = TRUE),
        sd_R2 = sd(R2, na.rm = TRUE),
        mean_CCC = mean(CCC, na.rm = TRUE),
        median_CCC = median(CCC, na.rm = TRUE),
        sd_CCC = sd(CCC, na.rm = TRUE),
        mean_RMSE = mean(RMSE, na.rm = TRUE),
        median_RMSE = median(RMSE, na.rm = TRUE),
        sd_RMSE = sd(RMSE, na.rm = TRUE)
      )
  ) %>%
  arrange(match(gas, c("Overall", "CO2", "H2O")), desc(mean_R2), desc(median_R2))

best_method <- ensemble_r2_summary %>%
  filter(gas == "Overall") %>%
  slice_max(order_by = mean_R2, n = 1, with_ties = FALSE)

message("Best overall method by mean R2: ", best_method$ensemble_method, " (", round(best_method$mean_R2, 3), ")")

method_levels <- ensemble_r2_summary %>%
  filter(gas == "Overall") %>%
  arrange(mean_R2) %>%
  pull(ensemble_method)

ensemble_r2_summary <- ensemble_r2_summary %>%
  mutate(
    gas = factor(gas, levels = c("Overall", "CO2", "H2O")),
    ensemble_method = factor(ensemble_method, levels = method_levels)
  )

plot_data <- ensemble_r2_summary %>%
  filter(gas != "Overall") %>%
  mutate(
    ensemble_label = case_when(
      ensemble_method == "bayesian_fusion" ~ "Bayesian fusion",
      ensemble_method == "inverse_rmse_weighted" ~ "Inverse RMSE",
      ensemble_method == "method_specific_weighted" ~ "Method-specific",
      ensemble_method == "mean_reliable" ~ "Mean reliable",
      ensemble_method == "median_reliable" ~ "Median reliable",
      ensemble_method == "weighted_median_uncertainty" ~ "Weighted median",
      TRUE ~ as.character(ensemble_method)
    )
  )

overall_labels <- ensemble_r2_summary %>%
  filter(gas == "Overall") %>%
  mutate(
    label = paste0("Overall mean R2 = ", sprintf("%.3f", mean_R2)),
    ensemble_method = factor(ensemble_method, levels = method_levels)
  )

plot.ensemble.method.comparison <- ggplot(
  plot_data,
  aes(x = mean_R2, y = ensemble_method)
) +
  geom_segment(
    aes(x = 0, xend = mean_R2, yend = ensemble_method),
    linewidth = 1.1,
    color = "#c7d4c0"
  ) +
  geom_point(aes(fill = gas), shape = 21, size = 4, color = "black", stroke = 0.35) +
  geom_text(
    data = overall_labels,
    aes(x = mean_R2 + 0.008, y = ensemble_method, label = label),
    inherit.aes = FALSE,
    hjust = 0,
    size = 3.2,
    family = "sans"
  ) +
  facet_wrap(~gas, ncol = 1, scales = "free_y") +
  scale_fill_manual(values = c("CO2" = "#1b7f5a", "H2O" = "#2f6db2")) +
  scale_y_discrete(labels = rev(c(
    "Weighted median",
    "Median reliable",
    "Mean reliable",
    "Method-specific",
    "Inverse RMSE",
    "Bayesian fusion"
  ))) +
  scale_x_continuous(limits = c(0, 0.56), expand = expansion(mult = c(0, 0.08))) +
  labs(
    title = "Ensemble Method Comparison",
    subtitle = "RSHP-filtered sampling heights with CCC >= 0.5",
    x = expression(paste("Mean site-level ", R^2, " against EC")),
    y = NULL
  ) +
  theme_bw(base_size = 11) +
  theme(
    strip.background = element_rect(fill = "#f3efe3", color = "black"),
    strip.text = element_text(face = "bold"),
    panel.grid.major.y = element_blank(),
    panel.grid.minor = element_blank(),
    legend.position = "none",
    plot.title = element_text(face = "bold"),
    plot.subtitle = element_text(size = 10)
  )

write_csv(
  ensemble_site_metrics,
  fs::path(DirRepo.eval, "Figures/WF_Version1/ENSEMBLE_site_level_metrics_RSHP_CCC05.csv")
)

write_csv(
  ensemble_r2_summary,
  fs::path(DirRepo.eval, "Figures/WF_Version1/ENSEMBLE_method_comparison_R2_RSHP_CCC05.csv")
)

write_csv(
  method_priors,
  fs::path(DirRepo.eval, "Figures/WF_Version1/ENSEMBLE_method_priors_RSHP_CCC05.csv")
)

ggsave(
  fs::path(DirRepo.eval, "Figures/WF_Version1/ENSEMBLE_method_comparison_R2_RSHP_CCC05.png"),
  plot = plot.ensemble.method.comparison,
  width = 8,
  height = 7,
  units = "in",
  dpi = 300
)

ggsave(
  fs::path(DirRepo.eval, "Figures/WF_Version1/ENSEMBLE_method_comparison_R2_RSHP_CCC05.pdf"),
  plot = plot.ensemble.method.comparison,
  width = 8,
  height = 7,
  units = "in"
)

save(
  ensemble_timeseries_all,
  ensemble_site_metrics,
  ensemble_r2_summary,
  method_priors,
  plot.ensemble.method.comparison,
  file = fs::path(DirRepo.eval, "Figures/WF_Version1/ENSEMBLE_METHOD_COMPARISON_RSHP_CCC05_AA_AW.Rdata")
)

print(ensemble_r2_summary)
