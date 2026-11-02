# Sect. 3.1 (and abstract): CCC ranges, reliable sampling-height pairs (RSHP, CCC >= 0.5), shares by approach,
# canopy level and ecosystem, Kruskal-Wallis / Wilcoxon tests, seasonal and hourly coverage of filtered values.
# Inputs: NEON_GradientFlux_Data_Filter/SITES_One2One<run.tag>.RData; flow.flux.counts<run.tag>.RData (flow.flux.counts.R).
if (!exists("ms_rdata")) source(file.path(DirRepo.eval, "workflows", "manuscript", "ms_setup.R"))

S0 <- ms_load("NEON_GradientFlux_Data_Filter", paste0("SITES_One2One", run.tag, ".RData"))$SITES_One2One %>% filter(Canopy_L1 != "WW")
S  <- S0 %>% filter(!is.na(CCC)) %>% mutate(Approach = as.character(Approach))
for (g in c("CO2", "H2O")) { x <- S[S$gas == g, ]
  p("%s CCC range %.2f to %.2f; combinations with CCC %d of %d", g, min(x$CCC), max(x$CCC), nrow(x), sum(S0$gas == g)) }
hdr("mean CCC by approach"); pr(S %>% group_by(gas, Approach) %>% summarise(m = round(mean(CCC), 2), .groups = "drop") %>% pivot_wider(names_from = Approach, values_from = m))
hdr("mean CCC by canopy level"); pr(S %>% group_by(gas, Canopy_L1) %>% summarise(m = round(mean(CCC), 2), .groups = "drop") %>% pivot_wider(names_from = Canopy_L1, values_from = m))
for (g in c("CO2", "H2O")) { x <- S[S$gas == g, ]
  p("%s Kruskal-Wallis approach p = %.3g, canopy level p = %.3g", g,
    kruskal.test(CCC ~ factor(Approach), x)$p.value, kruskal.test(CCC ~ factor(Canopy_L1), x)$p.value)
  print(signif(pairwise.wilcox.test(x$CCC, x$Approach, p.adjust.method = "holm")$p.value, 2)) }

R <- S %>% filter(CCC >= 0.5); sites <- unique(S0$Site)
hdr("reliable pairs (CCC >= 0.5)")
for (g in c("CO2", "H2O")) { s <- unique(R$Site[R$gas == g])
  p("%s sites with RSHP %d; none at: %s", g, length(s), paste(sort(setdiff(sites, s)), collapse = ", ")) }
p("sites with RSHP for both gases: %d", length(intersect(unique(R$Site[R$gas == "CO2"]), unique(R$Site[R$gas == "H2O"]))))
for (g in c("CO2", "H2O")) { n <- sum(R$gas == g)
  p("%s RSHP %d of %d combinations (%.1f %%; %.1f %% of those with a CCC)", g, n, sum(S0$gas == g), 100 * n / sum(S0$gas == g), 100 * n / sum(S$gas == g)) }
p("pairs reliable for both gases: %d", nrow(R %>% count(Site, Approach, dLevelsAminusB) %>% filter(n == 2)))
pr(R %>% count(gas, Site) %>% group_by(gas) %>% summarise(per_site_mean = round(mean(n), 1), per_site_sd = round(sd(n), 1)))
pr(R %>% count(gas, Approach) %>% group_by(gas) %>% mutate(pct = round(100 * n / sum(n))))
pr(R %>% count(gas, Canopy_L1) %>% group_by(gas) %>% mutate(pct = round(100 * n / sum(n))))
pr(S %>% group_by(gas, Canopy_L1) %>% summarise(pct_reliable = round(100 * mean(CCC >= 0.5), 1), .groups = "drop"))
pr(R %>% group_by(gas, Canopy_L1) %>% summarise(CCC = round(mean(CCC), 2), R2 = round(mean(R2), 2), .groups = "drop"))
p("RSHP with fewer than 30 matched values: %d of %d", sum(R$count < 30, na.rm = TRUE), nrow(R))

hdr("ecosystem types")
eco <- ms_ecotype(); Re <- R %>% left_join(eco, by = "Site")
pr(Re %>% count(EcoType, gas, Approach) %>% group_by(EcoType, gas) %>% mutate(pct = round(100 * n / sum(n))) %>% select(-n) %>% pivot_wider(names_from = Approach, values_from = pct))
pr(Re %>% group_by(EcoType, gas) %>% summarise(AW_pct = round(100 * mean(Canopy_L1 == "AW")), .groups = "drop") %>% pivot_wider(names_from = gas, values_from = AW_pct))
pr(S %>% left_join(eco, by = "Site") %>% group_by(EcoType, gas) %>% summarise(pct_reliable = round(100 * mean(CCC >= 0.5)), .groups = "drop") %>% pivot_wider(names_from = gas, values_from = pct_reliable))

hdr("coverage of filtered values (flow.flux.counts.R)")
e2 <- tryCatch(ms_load(paste0("flow.flux.counts", run.tag, ".RData")), error = function(err) ms_load("flow.flux.counts.RData"))
pr(e2$SITE_DATA_FLUX_COUNTS_SEASON_Total %>% group_by(gas, season) %>% summarise(mean_pct = round(mean(percent)), sd = round(sd(percent)), .groups = "drop"))
pr(e2$SITE_DATA_FLUX_COUNTS_HOUR_Season %>% group_by(gas, hour) %>% summarise(pct = round(mean(percent), 1), .groups = "drop") %>% pivot_wider(names_from = gas, values_from = pct))
pr(e2$SITE_DATA_FLUX_COUNTS_Approach %>% group_by(gas, Approach) %>% summarise(mean_values_per_site = round(mean(count.approach)), .groups = "drop"))
