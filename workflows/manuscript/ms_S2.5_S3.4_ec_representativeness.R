# Sects. 2.5 and 3.4 (GitHub issue #5): does the SNR/u* filtering change which EC conditions are represented?
# Input: per-site output of workflows/flow.ec.filter.selection.R (run for v3b with hpc/bouchet_v3b/run_ecfilter_v3b.sh):
#        <ec.filter.dir>/{SITE}_hourly.csv, {SITE}_daily.csv. This script first stacks the Approach == "ANY" rows of
#        all sites into <ec.filter.dir>/_summary/ANY_hourly.csv and ANY_daily.csv (47 sites in the manuscript).
# Text: retention by stage, day vs night retention, EC mean / |EC| of retained vs all half-hours, daily-total differences.
if (!exists("ms_rdata")) source(file.path(DirRepo.eval, "workflows", "manuscript", "ms_setup.R"))

sdir <- file.path(ec.filter.dir, "_summary"); dir.create(sdir, showWarnings = FALSE)
for (k in c("hourly", "daily")) {
  fs <- list.files(ec.filter.dir, paste0("^[A-Z]{4}_", k, "\\.csv$"), full.names = TRUE)
  if (!length(fs)) { if (file.exists(file.path(sdir, paste0("ANY_", k, ".csv")))) next else stop("No ", k, " files in ", ec.filter.dir) }
  x <- do.call(rbind, lapply(fs, function(f) { d <- read.csv(f); d[d$Approach == "ANY", , drop = FALSE] }))
  write.csv(x, file.path(sdir, paste0("ANY_", k, ".csv")), row.names = FALSE, na = "NA")
  p("%s: %d site files, %d ANY rows, %d sites", k, length(fs), nrow(x), length(unique(x$Site)))
}

H  <- read.csv(file.path(sdir, "ANY_hourly.csv")); Dd <- read.csv(file.path(sdir, "ANY_daily.csv"))
p("sites %d; stages %s; SNR thresholds %s", n_distinct(H$Site), paste(unique(H$stage), collapse = ","), paste(unique(H$threshold), collapse = ","))
h3 <- H %>% filter(threshold == 3 | is.na(threshold))   # production SNR threshold = 3

hdr("retention by stage (% of reference half-hours with EC)")
pr(h3 %>% group_by(gas, stage) %>% summarise(ret = round(100 * sum(n_retained) / sum(n_reference), 1), .groups = "drop") %>%
     pivot_wider(names_from = gas, values_from = ret))
hdr("retention by hour, after all filters (finite GF)")
pr(h3 %>% filter(stage == "finite_GF_after_filters") %>% group_by(gas, hour) %>%
     summarise(ret = round(100 * sum(n_retained) / sum(n_reference), 1), .groups = "drop") %>% pivot_wider(names_from = gas, values_from = ret))

hdr("EC of retained vs all half-hours (same site, season, hour; >= 5 retained)")
ref <- h3 %>% filter(stage == "all_reference") %>% select(Site, gas, season, hour, EC_ref = EC_mean, EC_ref_abs = EC_mean_absolute)
cmp <- h3 %>% filter(stage %in% c("turbulence", "turbulence_target", "turbulence_tracer", "turbulence_target_tracer", "finite_GF_after_filters")) %>%
  inner_join(ref, by = c("Site", "gas", "season", "hour")) %>% filter(n_retained >= 5)
pr(cmp %>% group_by(gas, stage) %>% summarise(mean_diff = round(mean(EC_mean - EC_ref, na.rm = TRUE), 3),
                                               abs_ratio = round(median(EC_mean_absolute / EC_ref_abs, na.rm = TRUE), 2), .groups = "drop"))

hdr("daily EC totals of retained half-hours vs reference (g m-2 d-1)")
d3 <- Dd %>% filter(threshold == 3 | is.na(threshold), stage != "all_reference")
pr(d3 %>% group_by(gas, stage) %>% summarise(
  n_total = n(), n_ok = sum(is.finite(EC_daily_g_m2)),
  med_abs_diff = round(median(abs(daily_difference_g_m2), na.rm = TRUE), 2),
  med_rel_pct = round(100 * median(abs(daily_difference_g_m2) / abs(EC_reference_daily_g_m2), na.rm = TRUE)),
  med_diff = round(median(daily_difference_g_m2, na.rm = TRUE), 2),
  sign_agree_pct = round(100 * mean(sign(EC_daily_g_m2) == sign(EC_reference_daily_g_m2), na.rm = TRUE)),
  r = round(cor(EC_daily_g_m2, EC_reference_daily_g_m2, use = "complete"), 2), .groups = "drop"))
print(table(Dd$reference_type))
