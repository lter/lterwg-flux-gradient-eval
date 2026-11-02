# Sect. 3.4 (and abstract): seasonal diel curves of the ensemble vs EC, and daily totals (g m-2 d-1, hourly units).
# Input: NEON_GradientFlux_Ensemble/DIEL_SUMMARY_ENSEMBLE_V1<run.tag>.RData (flow.evaluation.diel_V1.R).
# Table A2 (diel ranges) is written by flow.diel.range_table_V1.R.
if (!exists("ms_rdata")) source(file.path(DirRepo.eval, "workflows", "manuscript", "ms_setup.R"))

e <- ms_load("NEON_GradientFlux_Ensemble", paste0("DIEL_SUMMARY_ENSEMBLE_V1", run.tag, ".RData"))
E <- e$ENSEMBLE_DIELS_EVAL; T <- e$ENSEMBLE_DIELS_Site
eco <- ms_ecotype() %>% rename(site = Site)
hdr("diel R2 (seasonal curves)")
pr(E %>% group_by(gas) %>% summarise(sites = n_distinct(site), curves = n(), median = round(median(R2), 2), q05 = round(quantile(R2, .05), 2), q95 = round(quantile(R2, .95), 2)))
pr(E %>% group_by(gas, season) %>% summarise(median = round(median(R2), 2), .groups = "drop") %>% pivot_wider(names_from = season, values_from = median))
pr(E %>% group_by(gas, site) %>% summarise(sp = any(season == "Spring") & any(season == "Summer"), .groups = "drop") %>% group_by(gas) %>% summarise(pct_sites_spring_and_summer = round(100 * mean(sp))))
pr(E %>% left_join(eco, by = "site") %>% group_by(gas, EcoType) %>% summarise(median = round(median(R2), 2), n = n(), .groups = "drop"))
hdr("daily totals, GF ensemble vs EC")
D <- T %>% filter(is.finite(total.FG), is.finite(total.EC))
for (g in c("CO2", "H2O")) { x <- D[D$gas == g, ]; m <- lm(total.FG ~ total.EC, x); d <- abs(x$total.FG - x$total.EC)
  p("%s n=%d r=%.2f slope=%.2f mean|diff|=%.2f sd=%.2f median=%.2f same sign %.0f %%", g, nrow(x), cor(x$total.FG, x$total.EC), coef(m)[2],
    mean(d), sd(d), median(d), 100 * mean(sign(x$total.FG) == sign(x$total.EC))) }
pr(D %>% left_join(eco, by = "site") %>% group_by(gas, EcoType) %>% summarise(
  relbias_sum = round(100 * (sum(total.FG) - sum(total.EC)) / abs(sum(total.EC))),
  relbias_abs = round(100 * sum(total.FG - total.EC) / sum(abs(total.EC))),
  relbias_median = round(median(100 * (total.FG - total.EC) / abs(total.EC))), n = n(), .groups = "drop"))
