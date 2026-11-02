# Sect. 3.2: inlet placement, canopy height and structure vs CCC; tall-canopy sites; inlet separation (delta z).
# Canopy height hc = lidar CHM mean (NEON site value at DCFS, KONA, LAJA, TREE), as everywhere in the manuscript.
# Inputs: SITES_One2One<run.tag>.RData; Figures/WF_Version1/delta_z_AAplus_site_trend_detail<run.tag>.csv and
#         delta_z_AAplus_site_slope_plot_data<run.tag>.csv (flow.results.delta_z_*.R).
if (!exists("ms_rdata")) source(file.path(DirRepo.eval, "workflows", "manuscript", "ms_setup.R"))

S <- ms_load("NEON_GradientFlux_Data_Filter", paste0("SITES_One2One", run.tag, ".RData"))$SITES_One2One %>%
  filter(Canopy_L1 %in% c("AA", "AW"), !is.na(CCC)) %>%
  mutate(Approach = as.character(Approach), hc = coalesce(CHM.mean, canopyHeight_m),
         RelDistA = MeasurementHeight_m_A - hc, RelDistB = MeasurementHeight_m_B - hc,
         MeasurementDist = MeasurementHeight_m_A - MeasurementHeight_m_B)

hdr("site canopy height (lidar)")
H <- S %>% distinct(Site, hc, CHM.mean)
p("range %.2f-%.2f m; sites >= 5 m: %d; >= 15 m: %d; no lidar (NEON site value): %s", min(H$hc), max(H$hc),
  sum(H$hc >= 5), sum(H$hc >= 15), paste(sort(H$Site[is.na(H$CHM.mean)]), collapse = ", "))
p("tall sites (hc >= 15 m): %s", paste(sort(H$Site[H$hc >= 15]), collapse = ", "))

hdr("pair-level r(CCC, height of the inlet relative to hc); A = lower, B = upper inlet in the evaluation tables")
pr(S %>% group_by(gas, Approach) %>% summarise(r_lower = r2(CCC, RelDistA), r_upper = r2(CCC, RelDistB), r_sep = r2(CCC, MeasurementDist), .groups = "drop"))
hdr("pair-level r(CCC, structure)")
pr(S %>% group_by(gas) %>% summarise(hc = r2(CCC, hc), FHD = r2(CCC, Cutoff05.Grain01.FHD), VCI = r2(CCC, Cutoff05.VCI), DGF = r2(CCC, Cutoff05.DGF)))
hdr("site best CCC vs structure")
B <- S %>% group_by(gas, Site) %>% summarise(best = max(CCC), hc = first(hc), FHD = first(Cutoff05.Grain01.FHD),
                                             VCI = first(Cutoff05.VCI), DGF = first(Cutoff05.DGF), .groups = "drop")
pr(B %>% group_by(gas) %>% summarise(hc = r2(best, hc), FHD = r2(best, FHD), VCI = r2(best, VCI), DGF = r2(best, DGF)))
pr(B %>% group_by(gas) %>% slice_max(best, n = 5) %>% transmute(gas, Site, best = round(best, 2), hc = round(hc, 1)))

hdr("tall canopies (hc >= 15 m)")
pr(B %>% filter(hc >= 15) %>% group_by(gas) %>% summarise(sites_with_RSHP = sum(best >= 0.5), sites = n()))
pr(S %>% filter(hc >= 15, CCC >= 0.5) %>% count(gas, Approach))
pr(S %>% filter(hc >= 15, Approach != "MBR") %>% group_by(gas, Approach, Canopy_L1) %>% summarise(pct_reliable = round(100 * mean(CCC >= 0.5)), n = n(), .groups = "drop"))
p("sites below 15 m without a CO2 RSHP: %s", paste(B$Site[B$gas == "CO2" & B$hc < 15 & B$best < 0.5], collapse = ", "))
hdr("mean CCC: MBR AA and AE/WP")
pr(S %>% filter(Approach == "MBR", Canopy_L1 == "AA") %>% group_by(gas) %>% summarise(MBR_AA = round(mean(CCC), 2)))
pr(S %>% filter(Approach != "MBR") %>% group_by(gas, Approach) %>% summarise(m = round(mean(CCC), 2), .groups = "drop"))
hdr("TOOL reliable pairs"); pr(S %>% filter(Site == "TOOL", CCC >= 0.5) %>% select(gas, Approach, dLevelsAminusB, CCC, hc))

hdr("inlet separation (AA+ pairs)")
D <- ms_fig_csv("delta_z_AAplus_site_trend_detail")
pr(D %>% mutate(A2 = ifelse(Approach == "MBR", "MBR", "AE/WP")) %>% group_by(gas, A2) %>%
     summarise(sites = n_distinct(Site), pct_CCC_slope_pos = round(100 * mean(CCC_slope > 0, na.rm = TRUE)), n_slopes = sum(!is.na(CCC_slope)), .groups = "drop"))
p("delta z sites: %d", n_distinct(D$Site))
P <- ms_fig_csv("delta_z_AAplus_site_slope_plot_data") %>% left_join(H %>% select(Site, hcL = hc), by = "Site")
print(table(P$metric))
P1 <- P %>% filter(metric == unique(metric)[1])
pr(P1 %>% group_by(gas) %>% summarise(r_slope_hc = r2(slope, hcL), R2 = round(r2(slope, hcL)^2, 2), n = n()))
pr(P1 %>% mutate(A2 = ifelse(Approach == "MBR", "MBR", "AE/WP")) %>% group_by(gas, A2) %>% summarise(sites = n_distinct(Site), pct_pos = round(100 * mean(slope > 0)), .groups = "drop"))
