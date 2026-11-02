# aggregate_ch4method.R  OUT_DIR : compares EC-free pair-selection rules (scored on CO2/H2O vs EC) and their CH4 result
suppressPackageStartupMessages(library(data.table))
out <- commandArgs(TRUE)[1]
R <- rbindlist(lapply(list.files(file.path(out, "ch4method"), "_ch4method_stats\\.csv$", recursive = TRUE, full.names = TRUE), fread), fill = TRUE)
P <- rbindlist(lapply(list.files(file.path(out, "ch4method"), "_ch4method_pairs\\.csv$", recursive = TRUE, full.names = TRUE), fread), fill = TRUE)
fwrite(R, file.path(out, "CH4METHOD_stats_by_site.csv")); fwrite(P, file.path(out, "CH4METHOD_pairs_by_site.csv"))
R[, repr := abs(EC_on_windows - EC_all)]
sink(file.path(out, "CH4METHOD_SUMMARY.txt"), split = TRUE)
cat("Pair selection without the target gas's EC (previous SD-based SNR rule, despike k = 6); sites:", uniqueN(R$Site), "\n")
cat("EC = benchmark (CCC >= 0.5 vs own EC); S2 = AA MBR ref + AE/WP with 30-min CCC_ref >= 0.5; S2day = same on daily means;\n")
cat("XG_<gas> = pairs that are RSHP for <gas> (EC-based), equal weights; XG_both = RSHP for both CO2 and H2O\n\n")
cat("1. Coverage and agreement with EC\n")
print(R[, .(sites = sum(n_halfhours > 0), pairs_per_site = round(mean(n_pairs), 1), halfhours_total = sum(n_halfhours),
            halfhours_median = as.numeric(median(n_halfhours)), months_ge48_median = as.numeric(median(months_ge48, na.rm = TRUE)),
            CCC_EC = round(mean(CCC_EC, na.rm = TRUE), 3), sites_CCC_ge05 = sum(CCC_EC >= 0.5, na.rm = TRUE),
            abs_bias = round(median(abs(bias_EC), na.rm = TRUE), 3), repr = round(median(repr, na.rm = TRUE), 3)),
        by = .(gas, rule)][order(gas, rule)])
cat("\n2. CO2/H2O: each EC-free rule vs the EC benchmark at the same site (paired)\n")
b <- R[rule == "EC", .(Site, gas, CCC_bench = CCC_EC, n_bench = n_halfhours)]
x <- merge(R[rule != "EC" & gas != "CH4"], b, by = c("Site", "gas"))
print(x[, .(sites = .N, dCCC_median = round(median(CCC_EC - CCC_bench, na.rm = TRUE), 3),
            within_0.1_of_bench = sum(CCC_EC >= CCC_bench - 0.1, na.rm = TRUE),
            halfhours_ratio_median = round(median(n_halfhours / n_bench, na.rm = TRUE), 2)), by = .(gas, rule)][order(gas, rule)])
cat("\n3. CH4 by site: half-hours and mean flux per rule\n")
w <- dcast(R[gas == "CH4"], Site ~ rule, value.var = c("n_halfhours", "mean_flux"))
print(w)
sink()
