# aggregate_ens.R  OUT_DIR
# Summarises ensemble_site.R outputs: data retained and flux changes, previous SD rule vs SE rule.
suppressPackageStartupMessages(library(data.table))
out <- commandArgs(TRUE)[1]
S <- rbindlist(lapply(list.files(file.path(out, "ensemble"), "_ensemble_stats\\.csv$", recursive = TRUE, full.names = TRUE), fread), fill = TRUE)
N <- rbindlist(lapply(list.files(file.path(out, "ensemble"), "_noEC_pairs\\.csv$", recursive = TRUE, full.names = TRUE), fread), fill = TRUE)
fwrite(S, file.path(out, "ENSEMBLE_stats_SE_vs_SD.csv")); fwrite(N, file.path(out, "ENSEMBLE_noEC_pairs_SE_vs_SD.csv"))
S[, coverage := 100 * n_halfhours / n_halfhours_record]
S[, repr := EC_mean_on_windows - EC_mean_all]
sink(file.path(out, "ENSEMBLE_SUMMARY_SE_vs_SD.txt"), split = TRUE)
cat("30-min ensembles, previous SD-based SNR rule (3) vs SE-based rule (MBR 30, AE/WP 3); sites:", uniqueN(S$Site), "\n")
cat("RSHP = CO2/H2O ensemble of pairs with CCC >= 0.5 vs EC (calc.ensemble.site);",
    "noEC = AA MBR reference + rescaled AE/WP with CCC_ref >= 0.5 (Supplement S2; the CH4 method)\n\n")
cat("1. Data retained\n")
print(S[, .(sites = sum(n_halfhours > 0), halfhours_total = sum(n_halfhours), halfhours_median = as.numeric(median(n_halfhours)),
            coverage_pct_median = round(median(coverage, na.rm = TRUE), 1), months_ge48_median = as.numeric(median(months_ge48)),
            pairs_per_halfhour = round(mean(mean_pairs, na.rm = TRUE), 2), AEWP_selected = sum(n_AEWP_selected, na.rm = TRUE)),
        by = .(method, gas, filter)][order(method, gas, filter)])
w <- dcast(S, Site + method + gas ~ filter, value.var = c("n_halfhours", "mean_flux", "median_flux", "mean_abs_flux", "CCC_EC", "bias_EC", "repr"))
cat("\n   Sites losing >= 50 % of ensemble half-hours with the SE rule:\n")
print(w[n_halfhours_SE < 0.5 * n_halfhours_SD, .(Site, method, gas, n_halfhours_SD, n_halfhours_SE)][order(method, gas, Site)])
cat("\n2. Agreement with EC (CO2, H2O): mean CCC over sites; median |bias|; median |EC on ensemble windows - EC on all windows|\n")
print(S[gas != "CH4", .(CCC_EC = round(mean(CCC_EC, na.rm = TRUE), 3), sites_CCC_ge05 = sum(CCC_EC >= 0.5, na.rm = TRUE),
                         abs_bias = round(median(abs(bias_EC), na.rm = TRUE), 3), representativeness = round(median(abs(repr), na.rm = TRUE), 3)),
        by = .(method, gas, filter)][order(method, gas, filter)])
cat("\n3. Flux change (site means of the 30-min ensemble), SE vs SD\n")
print(w[is.finite(mean_flux_SD) & is.finite(mean_flux_SE),
        .(sites = .N, mean_SD = signif(median(mean_flux_SD), 3), mean_SE = signif(median(mean_flux_SE), 3),
          median_SD = signif(median(median_flux_SD), 3), median_SE = signif(median(median_flux_SE), 3),
          abs_mean_smaller_SE = sum(abs(mean_flux_SE) < abs(mean_flux_SD)),
          ratio_abs_mean_SE_SD = round(median(abs(mean_flux_SE) / abs(mean_flux_SD)), 2),
          sign_change = sum(sign(mean_flux_SE) != sign(mean_flux_SD))), by = .(method, gas)][order(method, gas)])
cat("\n4. CH4 by site (noEC ensemble)\n")
print(w[method == "noEC" & gas == "CH4", .(Site, n_SD = n_halfhours_SD, n_SE = n_halfhours_SE,
        mean_SD = signif(mean_flux_SD, 3), mean_SE = signif(mean_flux_SE, 3), median_SD = signif(median_flux_SD, 3), median_SE = signif(median_flux_SE, 3))][order(Site)])
sink()
fwrite(w, file.path(out, "ENSEMBLE_by_site_SE_vs_SD.csv"))
