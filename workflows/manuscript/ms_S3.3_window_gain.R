# Sects. 3.3 and 4.3: how many more half-hours the ensemble covers than the best single reliable pair
# ("1.6x for CO2, 1.5x for H2O"). Counted from the filtered 9-min values: half-hour of the sampling midpoint
# (as in calc.ensemble.R), union over the reliable pairs vs the best single pair at each site.
# Inputs: SITES_One2One<run.tag>.RData; NEON_GradientFlux_Data_Filter/SITES_{MBR,AE,WP}_9min_FILTER<run.tag>.RData;
#         SITE_DATA_ENSEMBLE_V1<run.tag>.RData (check that the union equals the ensemble half-hours).
if (!exists("ms_rdata")) source(file.path(DirRepo.eval, "workflows", "manuscript", "ms_setup.R"))

R <- ms_load("NEON_GradientFlux_Data_Filter", paste0("SITES_One2One", run.tag, ".RData"))$SITES_One2One %>%
  filter(Canopy_L1 %in% c("AA", "AW"), CCC >= 0.5) %>% transmute(Site, gas, Approach = as.character(Approach), dLevelsAminusB) %>% distinct()
W <- list()
for (m in c("MBR", "AE", "WP")) {
  f <- ms_load("NEON_GradientFlux_Data_Filter", paste0("SITES_", m, "_9min_FILTER", run.tag, ".RData")); L <- f[[ls(f)[1]]]
  for (s in names(L)) { x <- L[[s]]; if (!NROW(x)) next
    W[[length(W) + 1]] <- x %>% filter(is.finite(FG_mean)) %>%
      mutate(Site = s, Approach = m, hh = floor(((as.numeric(timeEnd_A) + as.numeric(timeBgn_B)) / 2) / 1800)) %>%
      semi_join(R, by = c("Site", "gas", "Approach", "dLevelsAminusB")) %>% distinct(Site, gas, Approach, dLevelsAminusB, hh) }
  rm(f, L); gc()
}
W <- bind_rows(W)
best  <- W %>% count(Site, gas, Approach, dLevelsAminusB) %>% group_by(Site, gas) %>% summarise(best = max(n), .groups = "drop")
union <- W %>% distinct(Site, gas, hh) %>% count(Site, gas, name = "union")
e <- ms_load("NEON_GradientFlux_Ensemble", paste0("SITE_DATA_ENSEMBLE_V1", run.tag, ".RData"))
ens <- bind_rows(lapply(names(e$SITE_DATA_ENSEMBLE), function(s) { x <- e$SITE_DATA_ENSEMBLE[[s]]; if (!NROW(x)) return(NULL)
  tibble(Site = s, gas = x$gas, ok = is.finite(x$FG_ENSEMBLE)) })) %>% filter(ok) %>% count(Site, gas, name = "ens")
X <- inner_join(best, union, by = c("Site", "gas")) %>% left_join(ens, by = c("Site", "gas")) %>% mutate(ratio = union / best)
p("union / ensemble half-hours, median: %.3f (1 = the ensemble covers exactly the union)", median(X$union / X$ens, na.rm = TRUE))
pr(X %>% group_by(gas) %>% summarise(median_ratio = round(median(ratio), 2), q25 = round(quantile(ratio, .25), 2), q75 = round(quantile(ratio, .75), 2),
                                     median_ens_over_best = round(median(ens / best, na.rm = TRUE), 2), sites = n()))
