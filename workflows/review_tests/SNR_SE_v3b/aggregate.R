# aggregate.R  OUT_DIR
# Combines the per-site outputs of run_site.R and compares the SE-based SNR filter (MBR 30, AE/WP 3)
# with the previous SD rule (3): retention, pair CCC vs EC, RSHP selection (CCC >= 0.5; sites with none
# get their best pair, as in 02_NEON_SensorHeightPairs.R), near-zero coverage and selection bias.
suppressPackageStartupMessages(library(data.table))
out <- commandArgs(TRUE)[1]
rd <- function(pat) rbindlist(lapply(list.files(file.path(out, "sites"), pat, recursive = TRUE, full.names = TRUE), fread), fill = TRUE)
P <- rd("_pairs_CCC\\.csv$"); R <- rd("_retention\\.csv$"); S <- rd("_selection\\.csv$")
fwrite(P, file.path(out, "ALL_pairs_CCC_SE_vs_SD.csv")); fwrite(R, file.path(out, "ALL_retention_SE_vs_SD.csv"))
fwrite(S, file.path(out, "ALL_selection_SE_vs_SD.csv"))
sink(file.path(out, "SUMMARY_SE_vs_SD.txt"), split = TRUE)
cat("SE-based SNR (MBR 30, AE/WP 3) vs previous SD rule (3); sites:", uniqueN(P$Site), "\n\n")

cat("1. Windows kept (% of u*-passed windows)\n")
r <- R[, .(pct = round(100 * sum(n_kept) / sum(n_base), 1)), by = .(gas, Approach, filter)]
print(dcast(r, gas + Approach ~ filter, value.var = "pct"))

P <- P[is.finite(CCC)]
rshp <- function(q) q[, if (any(CCC >= 0.5)) .SD[CCC >= 0.5] else .SD[CCC == max(CCC)], by = .(Site, gas)]
RS <- rbind(rshp(P[filter == "SD"])[, filter := "SD"], rshp(P[filter == "SE"])[, filter := "SE"])
fwrite(RS, file.path(out, "RSHP_SE_vs_SD.csv"))
cat("\n2. RSHP (pair CCC vs EC >= 0.5)\n")
key <- function(d) paste(d$Site, d$gas, d$Approach, d$dLevelsAminusB)
for (g in c("CO2", "H2O")) {
  a <- RS[filter == "SD" & gas == g]; b <- RS[filter == "SE" & gas == g]
  ka <- key(a); kb <- key(b)
  cat(sprintf("%s  RSHP: SD %d, SE %d | kept %d, lost %d, added %d | Jaccard %.2f | sites with CCC>=0.5: SD %d, SE %d\n", g,
              nrow(a), nrow(b), length(intersect(ka, kb)), length(setdiff(ka, kb)), length(setdiff(kb, ka)),
              length(intersect(ka, kb)) / length(union(ka, kb)),
              uniqueN(P[filter == "SD" & gas == g & CCC >= .5]$Site), uniqueN(P[filter == "SE" & gas == g & CCC >= .5]$Site)))
}
cat("\n   Mean pair CCC by approach, and mean best-pair CCC per site\n")
print(dcast(P[, .(CCC = round(mean(CCC), 3)), by = .(gas, Approach, filter)], gas + Approach ~ filter, value.var = "CCC"))
print(dcast(P[, .(best = max(CCC)), by = .(Site, gas, filter)][, .(best_pair_CCC = round(mean(best), 3)), by = .(gas, filter)],
            gas ~ filter, value.var = "best_pair_CCC"))
st <- merge(RS[filter == "SD", .(SD = paste(sort(paste(Approach, dLevelsAminusB)), collapse = "; ")), by = .(Site, gas)],
            RS[filter == "SE", .(SE = paste(sort(paste(Approach, dLevelsAminusB)), collapse = "; ")), by = .(Site, gas)],
            by = c("Site", "gas"), all = TRUE)
st[, same := SD == SE]; fwrite(st, file.path(out, "RSHP_by_site_SE_vs_SD.csv"))
cat("   Site x gas with identical RSHP sets:", sum(st$same, na.rm = TRUE), "of", nrow(st), "\n")

cat("\n3. Near-zero coverage (share of kept windows in the 30% of windows nearest zero |EC|; unbiased = 0.30)\n")
print(dcast(S[, .(share = round(weighted.mean(share_near0, n_kept), 3)), by = .(gas, Approach, filter)],
            gas + Approach ~ filter, value.var = "share"))
cat("\n4. Selection bias: median over sites of |mean EC on kept windows - mean EC on all windows|\n")
print(dcast(S[, .(sel = round(median(abs(EC_kept - EC_all)), 3)), by = .(gas, Approach, filter)],
            gas + Approach ~ filter, value.var = "sel"))
cat("\n   |mean GF kept - mean EC all| (what a mean/budget from kept windows would be off by)\n")
print(dcast(S[, .(tot = round(median(abs(FG_kept - EC_all)), 3)), by = .(gas, Approach, filter)],
            gas + Approach ~ filter, value.var = "tot"))
sink()
