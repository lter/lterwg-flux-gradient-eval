# Sects. 2.8, 3.3, 3.4 and 4.3: 30-min ensemble (inverse-RMSE weighted mean of the reliable pairs).
# - ensemble vs single-pair agreement; one-to-one R2 summary; method comparison (Table 5 is the csv itself)
# - overlap: share of ensemble half-hours with > 1 pair and > 1 approach, approach combinations
# - spread: Spearman rho(|GF - EC|, between-pair SD) and share of |GF - EC| within 1 and 2 SD (Sect. 2.8)
# - CPER and CLBJ CO2 ranges (Sect. 3.4, Fig. A13); largest |GF| by site
# Inputs: NEON_GradientFlux_Ensemble/SITE_DATA_ENSEMBLE_V1<run.tag>.RData; SITES_One2One<run.tag>.RData;
#         Figures/WF_Version1/ENSEMBLE_site_level_metrics_RSHP_CCC05, ENSEMBLE_method_comparison_R2_RSHP_CCC05,
#         SITE_DATA_ENSEMBLE_one_to_one_R2_summary (<run.tag>.csv).
if (!exists("ms_rdata")) source(file.path(DirRepo.eval, "workflows", "manuscript", "ms_setup.R"))

S <- ms_load("NEON_GradientFlux_Data_Filter", paste0("SITES_One2One", run.tag, ".RData"))$SITES_One2One %>% filter(Canopy_L1 %in% c("AA", "AW"))
R <- S %>% filter(CCC >= 0.5)

hdr("ensemble vs mean reliable-pair CCC (inverse-RMSE ensemble)")
M <- ms_fig_csv("ENSEMBLE_site_level_metrics_RSHP_CCC05") %>% filter(ensemble_method == "inverse_rmse_weighted")
pr(M %>% inner_join(R %>% group_by(Site, gas) %>% summarise(pairCCC = mean(CCC), .groups = "drop"), by = c("Site", "gas")) %>%
     group_by(gas) %>% summarise(sites = n(), ensemble = round(mean(CCC), 2), pairs = round(mean(pairCCC), 2), pct_ens_ge_pairs = round(100 * mean(CCC >= pairCCC))))
hdr("ensemble windows relative to the best single reliable pair (count of matched values)")
best <- R %>% group_by(Site, gas) %>% summarise(best = max(count, na.rm = TRUE), .groups = "drop")
pr(M %>% inner_join(best, by = c("Site", "gas")) %>% mutate(ratio = n_halfhours / best) %>% group_by(gas) %>%
     summarise(median = round(median(ratio), 2), q25 = round(quantile(ratio, .25), 2), q75 = round(quantile(ratio, .75), 2), n = n()))
hdr("one-to-one R2 summary"); pr(ms_fig_csv("SITE_DATA_ENSEMBLE_one_to_one_R2_summary") %>% group_by(gas) %>% summarise(n = n(), mean_R2 = round(mean(r2), 2)))
hdr("ensemble methods, all gases (Table 5)")
pr(ms_fig_csv("ENSEMBLE_method_comparison_R2_RSHP_CCC05") %>% filter(gas == "Overall") %>%
     transmute(ensemble_method, R2 = round(mean_R2, 3), sdR2 = round(sd_R2, 2), CCC = round(mean_CCC, 3), sdCCC = round(sd_CCC, 2), RMSE = round(mean_RMSE, 2), sdRMSE = round(sd_RMSE, 1)))

e <- ms_load("NEON_GradientFlux_Ensemble", paste0("SITE_DATA_ENSEMBLE_V1", run.tag, ".RData"))
E <- bind_rows(lapply(names(e$SITE_DATA_ENSEMBLE), function(s) { x <- e$SITE_DATA_ENSEMBLE[[s]]
  if (!NROW(x)) return(NULL); x$time.rounded <- as.numeric(x$time.rounded); x$Site <- s; x })) %>% filter(is.finite(FG_ENSEMBLE))
p("ensemble half-hours with a value: %d", nrow(E))

hdr("overlap (mean across sites, % of ensemble half-hours)")
split_app <- function(v) lapply(strsplit(v, "[,;+ ]+"), function(a) sort(unique(a[a != ""])))
O <- E %>% mutate(apps = split_app(FG_ENSEMBLE_APPROACHES), nApp = lengths(apps), comb = vapply(apps, paste, "", collapse = "+"))
pr(O %>% group_by(gas, Site) %>% summarise(pPair = 100 * mean(FG_ENSEMBLE_NPAIRS > 1), pApp = 100 * mean(nApp > 1), .groups = "drop") %>%
     group_by(gas) %>% summarise(gt1_pair = round(mean(pPair)), gt1_approach = round(mean(pApp)),
                                 range_pair = paste(round(range(pPair)), collapse = "-"), range_approach = paste(round(range(pApp)), collapse = "-")))
hdr("approach combinations, pooled over all ensemble half-hours (% ; Sect. 3.3 text)")
pr(O %>% count(gas, comb) %>% group_by(gas) %>% mutate(pct = round(100 * n / sum(n))) %>% arrange(gas, -pct))
hdr("approach combinations, mean of site shares (%)")
nsite <- O %>% group_by(gas) %>% summarise(ns = n_distinct(Site))
pr(O %>% count(gas, Site, comb) %>% group_by(gas, Site) %>% mutate(pct = 100 * n / sum(n)) %>% group_by(gas, comb) %>%
     summarise(tot = sum(pct), .groups = "drop") %>% left_join(nsite, by = "gas") %>% mutate(mean_pct = round(tot / ns)) %>% select(gas, comb, mean_pct) %>% arrange(gas, -mean_pct))

hdr("between-pair spread vs error (half-hours with > 1 pair and EC)")
SP <- E %>% filter(is.finite(EC_mean), is.finite(FG_ENSEMBLE_SD), FG_ENSEMBLE_NPAIRS > 1) %>% mutate(d = abs(FG_ENSEMBLE - EC_mean))
pr(SP %>% group_by(gas) %>% summarise(rho = round(cor(d, FG_ENSEMBLE_SD, method = "spearman"), 2),
                                      within_1sd = round(100 * mean(d <= FG_ENSEMBLE_SD)), within_2sd = round(100 * mean(d <= 2 * FG_ENSEMBLE_SD)), n = n()))

hdr("CO2 ranges at CPER and CLBJ (0.5th-99.5th percentile; umol m-2 s-1)")
for (s in c("CPER", "CLBJ", "WREF")) { x <- E %>% filter(Site == s, gas == "CO2"); y <- x$EC_mean[is.finite(x$EC_mean)]
  p("%s GF %s; EC %s; GF min/max %s", s, paste(round(quantile(x$FG_ENSEMBLE, c(.005, .995)), 1), collapse = " to "),
    paste(round(quantile(y, c(.005, .995)), 1), collapse = " to "), paste(round(range(x$FG_ENSEMBLE), 1), collapse = " / ")) }
pr(E %>% filter(gas == "CO2") %>% group_by(Site) %>% summarise(max_abs = round(max(abs(FG_ENSEMBLE)), 1), q995_abs = round(quantile(abs(FG_ENSEMBLE), .995), 1)) %>% arrange(-max_abs) %>% head(6))
