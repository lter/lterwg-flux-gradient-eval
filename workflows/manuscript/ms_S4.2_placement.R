# Sect. 4.2 / Fig. 9: inlet placement of the best pairs. For short (hc < 5 m) and tall (hc >= 15 m) canopies,
# the top quartile of pairs by CCC per gas and approach: median CCC and the lower/upper inlet height relative to hc.
# Input: SITES_One2One_canopy_model<run.tag>.RData (flow.evaluation.GoodDrivers.V2.R; CanopyHeight = lidar hc).
if (!exists("ms_rdata")) source(file.path(DirRepo.eval, "workflows", "manuscript", "ms_setup.R"))

df <- ms_load(paste0("SITES_One2One_canopy_model", run.tag, ".RData"))$SITES_One2One_model %>%
  filter(Canopy_L1 %in% c("AA", "AW"), !is.na(CCC)) %>%
  mutate(RelLower = MeasurementHeight_m_A - CanopyHeight, RelUpper = MeasurementHeight_m_B - CanopyHeight,
         cls = cut(CanopyHeight, c(-0.1, 1, 5, 15, 100), labels = c("VS", "S", "M", "T"), include.lowest = TRUE),
         b = ifelse(cls %in% c("VS", "S"), "short", ifelse(cls == "T", "tall", NA)))
pr(df %>% filter(!is.na(b)) %>% group_by(b, gas, Approach) %>% arrange(desc(CCC), .by_group = TRUE) %>% slice_head(prop = 0.25) %>%
     summarise(n = n(), CCC = round(median(CCC), 2),
               lower = round(median(RelLower), 1), lower_q25 = round(quantile(RelLower, .25), 1), lower_q75 = round(quantile(RelLower, .75), 1),
               upper = round(median(RelUpper), 1), upper_q25 = round(quantile(RelUpper, .25), 1), upper_q75 = round(quantile(RelUpper, .75), 1),
               AW = sum(Canopy_L1 == "AW"), .groups = "drop"))
