# flow.supplement_review.R -----------------------------------------------------
# Analyses, tables and figures answering the referee comments on essd-2026-413 (RC1, RC2) that
# need data. Outputs go to localdir/Review_Supplement/ (tables) and Figures/WF_Version1/REVIEW_*.png.
#
#  R1  Sampling-height pairs: heights, canopy height and canopy-level category of every pair, with the
#      category rules checked against the heights            (RC1 L129-130, L515-525; RC2 [2] Sect. 2.1)
#  R2  Tower geometry: top inlet vs tower (EC) height, inlet tube delays (RC2 [3]3-4)
#  R3  Profile sampling schedule: sampling time per inlet and how often each inlet is revisited (RC2 [3]1)
#  R4  MBR tracer used for each target gas                     (RC1 Sect. 2.4.1; RC2 [4])
#  R5  Seasonal day/night CO2 and H2O concentration profiles; height of the main sink/source layer
#      relative to canopy height and to the 2nd-highest inlet  (RC2 [2], [7], [10]2)
#      R5b: Supplement S3, profiles at all sites and seasons (S3_profiles_*.png, REVIEW_R5_sink_source_by_season)
#  R6  Temporal change in canopy height (aerodynamic canopy height by month) vs LiDAR canopy height
#                                                              (RC2 [2] Sect. 2.1 point 3, [7])
#  R7  Flux ranges: unfiltered, after u*/SNR filter, after despike (RC1 Fig. A13)
#  R8  TOOL diagnostics (RC1 Fig. 6F, Figs. A13/A15)
#
# Section 1 aggregates the stacked EDI tables (~250 GB) once into small files (Review_Supplement/ALL_*.csv).
# It takes ~1 h; set make.aggregates <- TRUE to (re)build them. Review_Supplement/review_agg.py does the same
# aggregation in Python (it was used for the 2021-2026 run).
# -----------------------------------------------------------------------------

library(data.table)
library(ggplot2)
library(ggpubr)

if (!exists("run.tag"))         run.tag <- "_AA_AW_2021_2026"
if (!exists("localdir"))        localdir <- "/Volumes/MaloneLab/Research/FluxGradient"
if (!exists("DirRepo.eval"))    DirRepo.eval <- "/Users/sm3466/Library/CloudStorage/Dropbox-YSE/Sparkle Malone/Research/FluxGradient/lterwg-flux-gradient-eval"
if (!exists("edi.dir"))         edi.dir <- file.path(localdir, paste0("NEON_GradientFlux_EDI", sub("^_AA_AW", "", run.tag)))
if (!grepl("^(/|[A-Za-z]:)", edi.dir)) edi.dir <- file.path(localdir, edi.dir)   # batch file gives the folder name
if (!exists("make.aggregates")) make.aggregates <- FALSE
rev.dir <- file.path(localdir, "Review_Supplement")
agg.dir <- file.path(rev.dir, paste0("aggregates", run.tag)); dir.create(agg.dir, recursive = TRUE, showWarnings = FALSE)
# Aggregates are specific to a run (canopy height, flux ranges and tracer counts change with each rerun):
# they are tagged with run.tag and built automatically when missing.
if (!all(file.exists(file.path(rev.dir, paste0("ALL_", c("profile","schedule","zveg","tracer","fluxrange"), run.tag, ".csv"))))) make.aggregates <- TRUE
fig.dir <- file.path(DirRepo.eval, "Figures/WF_Version1")
filter.dir <- file.path(localdir, "NEON_GradientFlux_Data_Filter")
spike.extract.dir <- file.path(localdir, "Spike_Eval", paste0("extract", sub("^_AA_AW", "", run.tag)))
season_of <- function(m) fcase(m %in% c(12, 1, 2), "Winter", m %in% 3:5, "Spring", m %in% 6:8, "Summer", default = "Autumn")
th <- theme_bw(base_size = 10) + theme(panel.grid.minor = element_blank(), legend.position = "top",
                                       strip.background = element_rect(fill = "grey95", colour = "grey80"))

# 1. Aggregate the EDI tables (one pass per site) -------------------------------------------
# In the EDI tables A is the UPPER inlet of a pair (dLevelsAminusB = "A_B").
if (isTRUE(make.aggregates)) {
  qs <- c(0, .001, .01, .5, .99, .999, 1)
  for (s in list.dirs(edi.dir, recursive = FALSE, full.names = FALSE)) {
    f <- file.path(edi.dir, s, paste0(s, "_MBR_9min.df.final.csv")); if (!file.exists(f)) next
    d <- fread(f, select = c("gas","tracer","FG_mean","TowerPosition_A","TowerPosition_B","TowerHeight_A","TowerHeight_B",
                             "mean_A","mean_B","timeBgn_A","timeEnd_A","timeBgn_B","timeEnd_B","z_veg_aero","timeEndA.local"))[gas %in% c("CO2","H2O")]
    d[, `:=`(month = as.integer(substr(timeEndA.local, 6, 7)), hour = as.integer(substr(timeEndA.local, 12, 13)), ym = substr(timeEndA.local, 1, 7))]
    d[, `:=`(season = season_of(month), daynight = fifelse(hour %in% 7:18, "day", "night"))]
    top <- max(d$TowerPosition_A, na.rm = TRUE)
    # R5 input: each lower inlet B against the top inlet in the same cycle
    p <- unique(d[TowerPosition_A == top & !is.na(mean_A) & !is.na(mean_B)], by = c("gas","TowerPosition_B","timeBgn_A","timeBgn_B"))
    prof <- p[, .(dC_top_mean = mean(mean_B - mean_A), dC_top_sd = sd(mean_B - mean_A), n = .N),
              by = .(gas, season, daynight, level = TowerPosition_B, height_m = TowerHeight_B)]
    topc <- unique(d[TowerPosition_A == top], by = c("gas","timeBgn_A"))[, .(C_top_mean = mean(mean_A, na.rm = TRUE), n_top = .N), by = .(gas, season, daynight)]
    prof <- topc[prof, on = .(gas, season, daynight)][, `:=`(Site = s, top_position = top, height_top_m = median(d[TowerPosition_A == top, TowerHeight_A], na.rm = TRUE))]
    fwrite(prof, file.path(agg.dir, paste0(s, "_profile.csv")))
    # R3 input: sampling duration and revisit interval of each inlet
    lv <- unique(rbind(d[, .(gas, pos = TowerPosition_A, b = timeBgn_A, e = timeEnd_A)], d[, .(gas, pos = TowerPosition_B, b = timeBgn_B, e = timeEnd_B)])[!is.na(b)], by = c("gas","pos","b"))
    lv[, `:=`(b = as.POSIXct(b, tz = "UTC"), e = as.POSIXct(e, tz = "UTC"))]; setorder(lv, gas, pos, b)
    lv[, `:=`(dur_min = as.numeric(e - b, units = "mins"), revisit_min = as.numeric(b - shift(b), units = "mins")), by = .(gas, pos)]
    fwrite(lv[, .(Site = s, n_samples = .N, dur_min_median = median(dur_min, na.rm = TRUE), revisit_min_median = median(revisit_min, na.rm = TRUE),
                  revisit_min_p25 = quantile(revisit_min, .25, na.rm = TRUE), revisit_min_p75 = quantile(revisit_min, .75, na.rm = TRUE)), by = .(gas, pos)],
           file.path(agg.dir, paste0(s, "_schedule.csv")))
    # R6 input: aerodynamic canopy height by month
    fwrite(unique(d, by = "timeBgn_A")[!is.na(z_veg_aero), .(Site = s, z_veg_aero = median(z_veg_aero)), by = ym], file.path(agg.dir, paste0(s, "_zveg.csv")))
    # R4 input: tracer used for each target gas
    fwrite(d[!is.na(FG_mean), .(Site = s, n = .N), by = .(gas, tracer)], file.path(agg.dir, paste0(s, "_tracer.csv")))
    # R7 input: unfiltered flux quantiles, all approaches
    rng <- list(MBR = d[!is.na(FG_mean), .(gas, FG_mean)])
    for (m in c("AE","WP")) { ff <- file.path(edi.dir, s, paste0(s, "_", m, "_9min.df.final.csv"))
      if (file.exists(ff)) rng[[m]] <- fread(ff, select = c("gas","FG_mean"))[gas %in% c("CO2","H2O") & !is.na(FG_mean)] }
    fr <- rbindlist(lapply(names(rng), function(m) rng[[m]][, as.list(setNames(quantile(FG_mean, qs), paste0("q", c("0","0.1","1","50","99","99.9","100")))),
                                                             by = gas][, `:=`(Site = s, Approach = m)]))
    fwrite(fr, file.path(agg.dir, paste0(s, "_fluxrange.csv")))
    rm(d, p, lv, rng); gc(); message(s)
  }
  for (k in c("profile","schedule","zveg","tracer","fluxrange"))
    fwrite(rbindlist(lapply(list.files(agg.dir, paste0("_", k, "\\.csv$"), full.names = TRUE), fread), fill = TRUE),
           file.path(rev.dir, paste0("ALL_", k, run.tag, ".csv")))
}
PROF <- fread(file.path(rev.dir, paste0("ALL_profile", run.tag, ".csv"))); SCHED <- fread(file.path(rev.dir, paste0("ALL_schedule", run.tag, ".csv")))
ZVEG <- fread(file.path(rev.dir, paste0("ALL_zveg", run.tag, ".csv"))); TRACER <- fread(file.path(rev.dir, paste0("ALL_tracer", run.tag, ".csv"))); FRANGE <- fread(file.path(rev.dir, paste0("ALL_fluxrange", run.tag, ".csv")))

load(file.path(filter.dir, paste0("SITES_One2One", run.tag, ".Rdata")))
O <- as.data.table(SITES_One2One)[, .(Site, gas, Approach = as.character(Approach), pair = dLevelsAminusB, CCC, count,
        z_lower = MeasurementHeight_m_A, z_upper = MeasurementHeight_m_B, hc = CanopyHeight, Canopy_L1, Canopy_L2, CHM.years)]
rm(SITES_One2One)
attr <- fread(file.path(localdir, "Site_Attributes.csv"))

# R1. Sampling-height pairs and canopy-level categories -------------------------------------
pairs <- unique(O[, .(Site, pair, z_lower, z_upper, hc, Canopy_L1, Canopy_L2)])
pairs[, `:=`(lower_rel_hc = z_lower - hc, upper_rel_hc = z_upper - hc, lower_above = z_lower > hc, upper_above = z_upper > hc)]
# category rule check: AA = both inlets above hc; AW = upper above, lower below; WW = both below
pairs[, rule_L1 := fcase(lower_above & upper_above, "AA", !lower_above & upper_above, "AW", default = "WW")]
cat("R1: canopy-level L1 matches the height rule for", pairs[, sum(rule_L1 == Canopy_L1)], "of", nrow(pairs), "pairs\n")
tab.R1.counts <- pairs[Canopy_L1 != "WW", .(pairs = .N, towers = uniqueN(Site), lower_minus_hc_m = paste(round(min(lower_rel_hc), 1), "to", round(max(lower_rel_hc), 1)),
                                            upper_minus_hc_m = paste(round(min(upper_rel_hc), 1), "to", round(max(upper_rel_hc), 1))), by = Canopy_L2][order(Canopy_L2)]
print(tab.R1.counts)
fwrite(pairs[order(Site, pair)], file.path(rev.dir, paste0("REVIEW_R1_sampling_height_pairs", run.tag, ".csv")))
fwrite(tab.R1.counts, file.path(rev.dir, paste0("REVIEW_R1_canopy_level_counts", run.tag, ".csv")))
inlets <- unique(rbind(pairs[, .(Site, z = z_lower, hc)], pairs[, .(Site, z = z_upper, hc)]))
inlets[, pos := fifelse(z > hc, "above canopy", "within canopy")]
ord <- unique(inlets[, .(Site, hc)])[order(hc), Site]; inlets[, Site := factor(Site, levels = ord)]
ex <- c("JORN","KONZ","GUAN","HARV")
pR1 <- ggplot(inlets, aes(Site, z)) +
  geom_col(data = unique(inlets[, .(Site, hc)]), aes(Site, hc), fill = "#7FB77E", alpha = 0.45, width = 0.8) +
  geom_point(aes(colour = pos), size = 1.4) + scale_colour_manual(values = c("above canopy" = "#3B5BA5", "within canopy" = "#C0504D"), name = NULL) +
  scale_y_sqrt(breaks = c(1, 5, 10, 20, 40, 60)) + labs(x = NULL, y = "Height above ground (m, square-root scale)") + th +
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, size = 7))
ggsave(file.path(fig.dir, paste0("REVIEW_R1_inlet_heights", run.tag, ".png")), pR1, width = 10, height = 4.5, dpi = 250, bg = "white")

# R2. Tower geometry: top inlet vs tower height, tube delays ---------------------------------
geo <- attr[, .(levels = .N, top_inlet_m = max(as.numeric(DistZaxsLvlMeasTow)), tower_m = as.numeric(DistZaxsTow[1]), canopy_m = as.numeric(DistZaxsCnpy[1]),
                tube_delay_s_min = min(as.numeric(TimeTube)), tube_delay_s_max = max(as.numeric(TimeTube))), by = Site]
geo[, `:=`(top_over_tower = round(top_inlet_m / tower_m, 2), top_over_canopy = round(top_inlet_m / canopy_m, 1))]
print(geo[, .(sites = .N, median_top_over_tower = median(top_over_tower), range_top_over_tower = paste(range(top_over_tower), collapse = "-"),
              median_top_over_canopy = median(top_over_canopy), tube_delay_s = paste(min(tube_delay_s_min), "to", max(tube_delay_s_max)))])
fwrite(geo, file.path(rev.dir, paste0("REVIEW_R2_tower_geometry", run.tag, ".csv")))

# R3. Profile sampling schedule --------------------------------------------------------------
sched <- SCHED[gas == "CO2", .(levels = .N, sample_min = round(median(dur_min_median), 1), revisit_min = median(revisit_min_median)), by = Site]
print(sched[, .(sites = .N), by = .(levels, sample_min, revisit_min)][order(levels)])
fwrite(sched, file.path(rev.dir, paste0("REVIEW_R3_sampling_schedule", run.tag, ".csv")))

# R4. MBR tracers --------------------------------------------------------------------------
tab.R4 <- TRACER[, .(sites = uniqueN(Site), mbr_values = sum(n)), by = .(target_gas = gas, tracer)]; print(tab.R4)
fwrite(tab.R4, file.path(rev.dir, paste0("REVIEW_R4_MBR_tracers", run.tag, ".csv")))

# R5. Concentration profiles and the main sink/source layer -------------------------------
hc.site <- unique(O[, .(Site, hc)])
lvh <- PROF[level != top_position, .(height_m = weighted.mean(height_m, n)), by = .(Site, level)]   # inlets moved slightly over time: use the mean height
P <- PROF[level != top_position][, .(dC = weighted.mean(dC_top_mean, n), n = sum(n), height_top_m = median(height_top_m)), by = .(Site, gas, season, daynight, level)]
P <- lvh[P, on = .(Site, level)][hc.site, on = "Site", nomatch = 0]
P[, z_rel := height_m / hc]
# Adding the top inlet (dC = 0) closes each profile
Pt <- unique(P[, .(Site, gas, season, daynight, hc, height_top_m)])[, `:=`(level = NA_integer_, dC = 0, n = NA_integer_, height_m = height_top_m)][, z_rel := height_m / hc]
P <- rbind(P, Pt[, names(P), with = FALSE])
# Two descriptors of the sink/source layout, per site x gas x season x day/night:
#  - z_extreme: inlet height with the lowest (CO2, daytime: canopy uptake) or highest concentration relative to the
#    top inlet (CO2 night, H2O: respiration / evaporation sources);
#  - z_maxgrad: midpoint of the adjacent-inlet interval with the largest concentration change per metre.
setorder(P, Site, gas, season, daynight, height_m)
ext <- P[, { k <- if (gas[1] == "CO2" && daynight[1] == "day") which.min(dC) else which.max(dC)
             .(z_extreme_m = height_m[k], dC_extreme = dC[k]) }, by = .(Site, gas, season, daynight, hc)]
G <- P[, .(z_mid = (height_m[-1] + height_m[-.N]) / 2, grad = diff(dC) / diff(height_m)), by = .(Site, gas, season, daynight, hc)]
mg <- G[G[, .I[which.max(abs(grad))], by = .(Site, gas, season, daynight)]$V1, .(Site, gas, season, daynight, z_maxgrad_m = z_mid, max_gradient_per_m = grad)]
second <- unique(PROF[, .(Site, level, height_m, top_position)])[level == top_position - 1, .(z_second_m = median(height_m)), by = Site]
tab.R5 <- second[mg[ext, on = .(Site, gas, season, daynight)], on = "Site"]
tab.R5[, `:=`(z_extreme_rel_hc = round(z_extreme_m / hc, 2), z_maxgrad_rel_hc = round(z_maxgrad_m / hc, 2), z_second_rel_hc = round(z_second_m / hc, 2),
              second_above_extreme = z_second_m > z_extreme_m + 0.05)]   # 5-cm tolerance: inlet heights are means over time, so equal heights must not flip on rounding
fwrite(tab.R5, file.path(rev.dir, paste0("REVIEW_R5_sink_source_layer", run.tag, ".csv")))
fwrite(P, file.path(rev.dir, paste0("REVIEW_R5_concentration_profiles", run.tag, ".csv")))
lowest <- P[, .(z_low = min(height_m)), by = Site]
tab.R5 <- lowest[tab.R5, on = "Site"]
tab.R5.sum <- tab.R5[season == "Summer", .(sites = .N, median_extreme_rel_hc = round(median(z_extreme_rel_hc), 2),
    pct_extreme_at_lowest_inlet = round(100 * mean(z_extreme_m == z_low)), pct_extreme_within_canopy = round(100 * mean(z_extreme_m < hc)),
    median_second_rel_hc = round(median(z_second_rel_hc), 2), pct_second_inlet_above_extreme = round(100 * mean(second_above_extreme))),
  by = .(gas, daynight, canopy = fifelse(hc < 5, "hc < 5 m", "hc >= 5 m"))][order(gas, daynight, canopy)]
print(tab.R5.sum)
fwrite(tab.R5.sum, file.path(rev.dir, paste0("REVIEW_R5_sink_source_summary", run.tag, ".csv")))
pR5 <- ggplot(P[Site %in% ex], aes(dC, height_m, colour = daynight, group = interaction(daynight, season))) +
  geom_vline(xintercept = 0, colour = "grey60") + geom_hline(aes(yintercept = hc), linetype = 2, colour = "#4A7C3A") +
  geom_path(aes(linetype = season), linewidth = 0.5) + geom_point(size = 0.8) +
  facet_grid(gas ~ factor(Site, levels = ex), scales = "free") +
  scale_colour_manual(values = c(day = "#E69F00", night = "#3B5BA5"), name = NULL) +
  labs(x = "Concentration minus top inlet (ppm CO2; mmol mol-1 H2O)", y = "Inlet height (m)", linetype = NULL) + th
ggsave(file.path(fig.dir, paste0("REVIEW_R5_concentration_profiles", run.tag, ".png")), pR5, width = 10, height = 6, dpi = 250, bg = "white")

# R5b. Supplement S3: seasonal day/night concentration profiles at every site -----------------
# Uses P (profiles closed with the top inlet, dC = 0) and tab.R5 from R5. Writes
#  REVIEW_R5_sink_source_by_season{run.tag}.csv  (summary of tab.R5 for every season, not only summer)
#  S3_profiles_CO2{run.tag}.png, S3_profiles_H2O{run.tag}.png  (all 47 sites, ordered by canopy height)
tab.R5.season <- tab.R5[, .(sites = .N,
    pct_extreme_at_lowest_inlet = round(100 * mean(z_extreme_m == z_low)),
    pct_extreme_within_canopy = round(100 * mean(z_extreme_m < hc)),
    pct_second_inlet_above_extreme = round(100 * mean(second_above_extreme))),
  by = .(gas, season, daynight, canopy = fifelse(hc < 5, "hc < 5 m", "hc >= 5 m"))]
tab.R5.season[, season := factor(season, levels = c("Winter", "Spring", "Summer", "Autumn"))]
setorder(tab.R5.season, gas, canopy, daynight, season)
print(tab.R5.season)
fwrite(tab.R5.season, file.path(rev.dir, paste0("REVIEW_R5_sink_source_by_season", run.tag, ".csv")))

site.order <- unique(P[, .(Site, hc)])[order(hc)]
site.lab <- setNames(sprintf("%s (%.1f m)", site.order$Site, site.order$hc), site.order$Site)
plot.S3 <- function(g, xlab) {
  d <- P[gas == g][, `:=`(Site = factor(Site, levels = site.order$Site), season = factor(season, levels = c("Winter", "Spring", "Summer", "Autumn")))]
  setorder(d, Site, season, daynight, height_m)
  hcl <- unique(d[, .(Site, hc)])
  ggplot(d, aes(dC, height_m, colour = season, linetype = daynight, group = interaction(season, daynight))) +
    geom_vline(xintercept = 0, colour = "grey70", linewidth = 0.3) +
    geom_hline(data = hcl, aes(yintercept = hc), colour = "#4A7C3A", linetype = 2, linewidth = 0.3, inherit.aes = FALSE) +
    geom_path(linewidth = 0.45) + geom_point(size = 0.6) +
    facet_wrap(~ Site, scales = "free", ncol = 7, labeller = as_labeller(site.lab)) +
    scale_colour_manual(values = c(Winter = "#3B5BA5", Spring = "#4E9A3E", Summer = "#E69F00", Autumn = "#B5523B"), name = NULL) +
    scale_linetype_manual(values = c(day = "solid", night = "22"), name = NULL) +
    labs(x = xlab, y = "Inlet height (m)") + th + theme(strip.text = element_text(size = 7), axis.text = element_text(size = 6))
}
pS3c <- plot.S3("CO2", expression("CO"[2]*" minus top inlet (ppm)"))
pS3h <- plot.S3("H2O", expression("H"[2]*"O minus top inlet (mmol mol"^-1*")"))
ggsave(file.path(fig.dir, paste0("S3_profiles_CO2", run.tag, ".png")), pS3c, width = 11, height = 12, dpi = 200, bg = "white")
ggsave(file.path(fig.dir, paste0("S3_profiles_H2O", run.tag, ".png")), pS3h, width = 11, height = 12, dpi = 200, bg = "white")

# R6. Seasonal change in canopy height ----------------------------------------------------
Z <- ZVEG[z_veg_aero > 0.05 * ave(z_veg_aero, Site, FUN = median)]          # drop start-up months with near-zero estimates
Z[, month := as.integer(substr(ym, 6, 7))]
clim <- Z[, .(z = median(z_veg_aero)), by = .(Site, month)]
zs <- Z[, .(months = .N, z_aero_median = median(z_veg_aero)), by = Site][clim[, .(z_min_month = min(z), z_max_month = max(z)), by = Site], on = "Site"]
zs <- unique(O[, .(Site, hc, CHM.years)])[zs, on = "Site"]
zs[, `:=`(seasonal_amplitude_pct = round(100 * (z_max_month - z_min_month) / z_aero_median), z_aero_over_lidar = round(z_aero_median / hc, 2))]
print(zs[, .(sites = .N, median_amplitude_pct = median(seasonal_amplitude_pct), sites_amplitude_gt_25pct = sum(seasonal_amplitude_pct > 25),
             median_aero_over_lidar = median(z_aero_over_lidar))])
fwrite(zs, file.path(rev.dir, paste0("REVIEW_R6_canopy_height_variability", run.tag, ".csv")))
mclim <- Z[, .(z = median(z_veg_aero)), by = .(Site, month)][zs[, .(Site, z_aero_median, hc)], on = "Site"]
pR6 <- ggplot(mclim, aes(month, z / z_aero_median, group = Site, colour = hc < 5)) + geom_line(alpha = 0.6) +
  scale_colour_manual(values = c("TRUE" = "#E69F00", "FALSE" = "#3B5BA5"), labels = c("TRUE" = "hc < 5 m", "FALSE" = "hc >= 5 m"), name = NULL) +
  scale_x_continuous(breaks = 1:12) + labs(x = "Month", y = "Aerodynamic canopy height / site median") + th
ggsave(file.path(fig.dir, paste0("REVIEW_R6_canopy_height_seasonal", run.tag, ".png")), pR6, width = 6.5, height = 4, dpi = 250, bg = "white")

# R7. Flux ranges: unfiltered, filtered (before despike), filtered + despiked --------------
filt <- rbindlist(lapply(c("MBR","AE","WP"), function(m) {
  e <- new.env(); load(file.path(filter.dir, paste0("SITES_", m, "_9min_FILTER", run.tag, ".Rdata")), envir = e); L <- get(ls(e)[1], envir = e)
  rbindlist(lapply(names(L), function(s) data.table(Site = s, gas = L[[s]]$gas, FG = L[[s]]$FG_mean)[!is.na(FG) & gas %in% c("CO2","H2O")][
    , .(q0.1 = quantile(FG, .001), q99.9 = quantile(FG, .999), min = min(FG), max = max(FG)), by = .(Site, gas)]))[, Approach := m] }))
pre <- rbindlist(lapply(list.files(spike.extract.dir, "\\.csv\\.gz$", full.names = TRUE), function(f) {
  x <- fread(f, select = c("Site","Approach","gas","FG_mean")); x[, .(min = min(FG_mean), max = max(FG_mean)), by = .(Site, Approach, gas)] }))
R7 <- FRANGE[, .(Site, Approach, gas, unfiltered_min = q0, unfiltered_max = q100)][
  pre[, .(Site, Approach, gas, filtered_min = min, filtered_max = max)], on = .(Site, Approach, gas)][
  filt[, .(Site, Approach, gas, despiked_min = min, despiked_max = max)], on = .(Site, Approach, gas)]
fwrite(R7, file.path(rev.dir, paste0("REVIEW_R7_flux_ranges", run.tag, ".csv")))
print(R7[Site == "CPER"], digits = 3)
R7s <- R7[, .(sites = .N, median_abs_max_unfiltered = signif(median(pmax(abs(unfiltered_min), abs(unfiltered_max))), 3),
              median_abs_max_filtered = signif(median(pmax(abs(filtered_min), abs(filtered_max))), 3),
              median_abs_max_despiked = signif(median(pmax(abs(despiked_min), abs(despiked_max))), 3)), by = .(gas, Approach)]
print(R7s); fwrite(R7s, file.path(rev.dir, paste0("REVIEW_R7_flux_range_summary", run.tag, ".csv")))

# R8. TOOL ----------------------------------------------------------------------------------
tool <- O[Site == "TOOL", .(gas, Approach, pair, Canopy_L2, z_lower, z_upper, hc, CCC = round(CCC, 2), n = count)][order(gas, -CCC)]
print(tool); fwrite(tool, file.path(rev.dir, paste0("REVIEW_R8_TOOL_pairs", run.tag, ".csv")))
