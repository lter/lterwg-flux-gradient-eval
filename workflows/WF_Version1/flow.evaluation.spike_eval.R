# flow.evaluation.spike_eval.R ------------------------------------------------
# What causes gradient-flux (GF) spikes?
#
# Spikes are defined exactly as in despike_fluxes() (functions/calc.filter_FG.R):
# after the u*/SNR filter, a GF is a spike when |GF - median| > despike.k * MAD within
# site x approach x gas x sampling-height pair x day/night (07-18 h local) x season,
# for groups with >= 20 values. This script flags (does not remove) those values and
# relates spike occurrence to canopy level, approach, gas and measurement conditions.
#
# Inputs : stacked EDI CSVs (localdir/<edi.dir>/SITE/SITE_<MBR|AE|WP>_9min.df.final.csv, Aug 2021 - Aug 2026)
#          canopy_commbined.csv
# Outputs: localdir/Spike_Eval/ (tables, Rdata) and Figures/WF_Version1/SPIKE_*<run.tag>.png
# -----------------------------------------------------------------------------

library(data.table)
library(tidyverse)
library(ggpubr)

if (!exists("run.tag"))   run.tag <- "_AA_AW_2021_2026"
if (!exists("despike.k")) despike.k <- 6
if (!exists("localdir"))  localdir <- "/Volumes/MaloneLab/Research/FluxGradient"
if (!exists("DirRepo.eval")) DirRepo.eval <- "/Users/sm3466/Library/CloudStorage/Dropbox-YSE/Sparkle Malone/Research/FluxGradient/lterwg-flux-gradient-eval"
if (!exists("edi.dir")) edi.dir <- "NEON_GradientFlux_EDI_2021_2026"   # stacked output of flow.evaluation.dataframe_EDI.R
p2.start <- as.POSIXct("2024-07-01", tz = "UTC")                            # start of the extension period
spike.dir  <- file.path(localdir, "Spike_Eval"); dir.create(spike.dir, showWarnings = FALSE)
extract.dir <- file.path(spike.dir, paste0("extract", sub("^_AA_AW", "", run.tag))); dir.create(extract.dir, showWarnings = FALSE)
fig.dir <- file.path(DirRepo.eval, "Figures/WF_Version1")

# 1. Extract filtered GF with diagnostics (skipped for files already in extract.dir) ---------
common.cols <- c("timeEndA.local","dLevelsAminusB","TowerPosition_A","TowerPosition_B","TowerHeight_A","TowerHeight_B","gas",
  "FG_mean","FG_sd","effective_h","dConc","dConc_sd","mean_A","mean_B","vari_A","vari_B","numSamp_A","numSamp_B",
  "timeBgn_A","timeEnd_A","timeBgn_B","timeEnd_B","ustar_interp","ustar_threshold","FC_turb_interp","FH2O_interp",
  "H_turb_interp","PAR","ubar1","ubar2","ubar3","ubar4","Tair1","VPD","RH","z_veg_aero","z_displ_calc","roughLength_calc",
  "L_obukhov","zoL","zeta","Stability_500","cross_grad_flag")
extra.cols <- list(MBR = c("dConc.tracer","dConc.tracer_sd","tracer"),
                   AE  = c("EddyDiff","Kgas","phih","effective_h","canopy.flag","Eddy_outlier"),
                   WP  = c("EddyDiff","Kgas","phih","effective_h","canopy.flag","Eddy_outlier"))
canopy <- read.csv(file.path(localdir, "canopy_commbined.csv")) %>% distinct() %>% filter(Canopy_L1 != "WW")
sites <- list.dirs(file.path(localdir, edi.dir), recursive = FALSE, full.names = FALSE)
for (s in sites) for (m in c("MBR","AE","WP")) {
  out <- file.path(extract.dir, paste0(s, "_", m, ".csv.gz")); if (file.exists(out)) next
  f <- file.path(localdir, edi.dir, s, paste0(s, "_", m, "_9min.df.final.csv")); if (!file.exists(f)) next
  pairs <- canopy$dLevelsAminusB[canopy$Site == s]
  hdr <- names(fread(f, nrows = 0)); x <- fread(f, select = intersect(c(common.cols, extra.cols[[m]]), hdr))
  x <- x[gas %in% c("CO2","H2O") & TowerPosition_A != TowerPosition_B & abs(dConc)/dConc_sd >= 3 &
           ustar_interp >= ustar_threshold & !is.na(FG_mean) & dLevelsAminusB %in% pairs]
  if (m == "MBR") x <- x[abs(dConc.tracer)/dConc.tracer_sd >= 3]
  x[, period := fifelse(as.POSIXct(timeEnd_A, tz = "UTC") >= p2.start, "P2", "P1")]
  if (nrow(x)) { x[, `:=`(Site = s, Approach = m)]; fwrite(x, out) }
}
# (The extraction was run on the server with an equivalent Python reader, Spike_Eval/extract_spike2.py.)

# 2. Assemble and flag spikes (same rule as despike_fluxes), one approach at a time -----------
files <- list.files(extract.dir, "\\.csv\\.gz$", full.names = TRUE)
use.cols <- c("Site","Approach","period","timeEndA.local","dLevelsAminusB","gas","FG_mean","dConc","dConc_sd","vari_A","vari_B",
              "ustar_interp","ustar_threshold","FC_turb_interp","FH2O_interp","ubar1","ubar2","ubar3","ubar4",
              "zeta","cross_grad_flag","dConc.tracer","dConc.tracer_sd","EddyDiff","effective_h","roughLength_calc","phih",
              "timeEnd_A","TowerHeight_A","TowerHeight_B","L_obukhov","MO.param")
# Stability parameter of each sampling-height pair, zeta = (z - d)/L with z = sqrt(zA zB), d = 0.66 h and
# z - d >= 0.1 m, as in calc.stability.correction() (MO.param, used for phi_h in the AE and WP).
# The extracts' "zeta" column is (h - d)/L from flag.all.gas.stability(), i.e. a canopy-height value that is
# the same for every pair, so it is not used here. The MBR extracts have no MO.param and carry the older
# turbulence-based h of the formatting step, so for the MBR d is taken from the aerodynamic canopy height
# (Pennypacker and Baldocchi, 2016) of the AE extract of the same site, interpolated in time.
h.aero <- rbindlist(lapply(files[grepl("_AE\\.csv\\.gz$", files)], function(f) {
  d <- unique(fread(f, select = c("timeEnd_A", "z_veg_aero")))
  d[, Site := sub("_AE\\.csv\\.gz$", "", basename(f))]
}))
h.aero[, t := as.numeric(as.POSIXct(timeEnd_A, tz = "UTC"))]
h.aero <- unique(h.aero[is.finite(t) & is.finite(z_veg_aero), .(Site, t, z_veg_aero)], by = c("Site", "t"))
setkey(h.aero, Site, t)
cj <- unique(as.data.table(canopy)[, .(Site, dLevelsAminusB, Canopy_L1, CanopyHeight, MeasurementHeight_m_A, MeasurementHeight_m_B)],
             by = c("Site", "dLevelsAminusB"))
flag_approach <- function(m) {
  ff <- files[grepl(paste0("_", m, "\\.csv\\.gz$"), files)]
  x <- rbindlist(lapply(ff, function(f) { h <- names(fread(f, nrows = 0)); fread(f, select = intersect(use.cols, h)) }), fill = TRUE)
  x[, `:=`(hour = as.integer(substr(timeEndA.local, 12, 13)), month = as.integer(substr(timeEndA.local, 6, 7)),
           date = as.IDate(substr(timeEndA.local, 1, 10)))]                     # local clock time
  x[, timeEndA.local := NULL]
  if (m == "MBR" || !"MO.param" %in% names(x)) {
    x[, t := as.numeric(as.POSIXct(timeEnd_A, tz = "UTC"))]
    x[, h_pb := {
      hs <- h.aero[.BY$Site]
      if (nrow(hs) >= 2) approx(hs$t, hs$z_veg_aero, t, rule = 2)$y else rep(NA_real_, .N)
    }, by = Site]
    x[, MO.param := pmax(sqrt(TowerHeight_A * TowerHeight_B) - 0.66 * h_pb, 0.1) / L_obukhov]
    x[, c("t", "h_pb") := NULL]
  }
  x[, zeta := MO.param]   # pair stability parameter (z - d)/L
  for (cc in setdiff(c("EddyDiff","effective_h","roughLength_calc","phih","dConc.tracer","dConc.tracer_sd","zeta","cross_grad_flag"), names(x))) x[, (cc) := NA_real_]
  x[, daynight := fifelse(hour %in% 7:18, "day", "night")]
  x[, season := fcase(month %in% c(12,1,2), "Winter", month %in% 3:5, "Spring", month %in% 6:8, "Summer", default = "Autumn")]
  x[, `:=`(med = median(FG_mean), mad = mad(FG_mean), n = .N), by = .(Site, gas, dLevelsAminusB, daynight, season)]
  x[, spike := n >= 20 & abs(FG_mean - med) > despike.k * pmax(mad, 1e-6)]
  x[, `:=`(zscore = abs(FG_mean - med) / pmax(mad, 1e-6), med = NULL, mad = NULL, n = NULL)]
  x[cj, on = .(Site, dLevelsAminusB), `:=`(Canopy_L1 = i.Canopy_L1, CanopyHeight = i.CanopyHeight,
                                         zA = i.MeasurementHeight_m_A, zB = i.MeasurementHeight_m_B)]
  x[, `:=`(
    level = substr(Canopy_L1, 1, 2),
    EC = fifelse(gas == "CO2", FC_turb_interp, FH2O_interp),
    snr = abs(dConc) / dConc_sd,
    tsnr = abs(dConc.tracer) / dConc.tracer_sd,
    ustar_ratio = ustar_interp / ustar_threshold,
    ubar = pmax(ubar1, ubar2, ubar3, ubar4, na.rm = TRUE),
    nonstat = (sqrt(pmax(vari_A, 0)) + sqrt(pmax(vari_B, 0))) / abs(dConc),   # within-window concentration noise / gradient
    zA_rel = zA - CanopyHeight, zB_rel = zB - CanopyHeight, dz = zA - zB,
    stab = fcase(is.na(zeta), NA_character_, zeta < -0.1, "unstable", zeta <= 0.1, "near-neutral", default = "stable"),
    K = EddyDiff, h_floor = Approach != "MBR" & !is.na(effective_h) & effective_h <= 0.1001,
    zd_z0 = effective_h / roughLength_calc, phih = phih)]   # WP: K ~ 1/ln((z-d)/z0) -> singular as (z-d)/z0 -> 1
  x[, `:=`(ratio_GF_EC = FG_mean / EC, sign_ok = sign(FG_mean) == sign(EC), transition = hour %in% c(5:8, 17:20))]
  keep <- c("Site","Approach","period","gas","dLevelsAminusB","Canopy_L1","level","CanopyHeight","zA_rel","zB_rel","dz",
            "hour","month","date","daynight","season","transition","FG_mean","EC","ratio_GF_EC","sign_ok","spike","zscore",
            "snr","tsnr","ustar_ratio","ubar","nonstat","zeta","stab","K","h_floor","zd_z0","phih","cross_grad_flag")
  x <- x[, ..keep]; gc(); x
}
D <- rbindlist(lapply(c("MBR", "AE", "WP"), flag_approach), fill = TRUE); gc()

# 3. Summaries --------------------------------------------------------------------------------
rate <- function(x) 100 * mean(x, na.rm = TRUE)
site_mean <- function(DT, by) DT[, .(rate = rate(spike), n = .N), by = c("Site", by)][n >= 50, .(spike_pct = mean(rate), sites = .N, obs = sum(n)), by = by]

tab.overall <- D[, .(obs = .N, spikes = sum(spike), spike_pct = rate(spike)), by = .(Approach, gas)]
tab.level   <- D[!is.na(level), .(obs = .N, spikes = sum(spike), spike_pct = rate(spike)), by = .(Approach, gas, level)]
tab.level.site <- site_mean(D[!is.na(level)], c("Approach", "gas", "level"))
# Paired AA vs AW comparison within sites that have both levels (Wilcoxon signed-rank on site spike rates)
tab.paired <- dcast(D[!is.na(level), .(rate = rate(spike), n = .N), by = .(Site, Approach, gas, level)][n >= 50], Site + Approach + gas ~ level, value.var = "rate")[
  !is.na(AA) & !is.na(AW), .(sites = .N, median_AA = median(AA), median_AW = median(AW), AW_higher = sum(AW > AA),
                            p_wilcox = wilcox.test(AW, AA, paired = TRUE)$p.value), by = .(Approach, gas)]
# WP near-singular log profile: share of WP spikes with (z-d)/z0 <= 1.5
tab.wp.sing <- D[Approach == "WP", .(obs = .N, spikes = sum(spike), pct_obs_sing = rate(zd_z0 <= 1.5), spike_pct_sing = rate(spike[zd_z0 <= 1.5]),
                                     spike_pct_rest = rate(spike[zd_z0 > 1.5]), pct_spikes_sing = 100 * sum(spike & zd_z0 <= 1.5, na.rm = TRUE) / sum(spike)), by = gas]
tab.L2      <- D[!is.na(Canopy_L1), .(obs = .N, spike_pct = rate(spike)), by = .(Approach, gas, Canopy_L1)]
tab.period  <- D[, .(obs = .N, spike_pct = rate(spike)), by = .(Approach, gas, period)]
tab.stab    <- D[!is.na(stab), .(obs = .N, spike_pct = rate(spike)), by = .(Approach, gas, stab)]
tab.floor   <- D[Approach != "MBR", .(obs = .N, spike_pct = rate(spike)), by = .(Approach, gas, level, h_floor)]
tab.daynight<- D[, .(obs = .N, spike_pct = rate(spike)), by = .(Approach, gas, daynight, transition)]

# Binned responses for figures
bin_rate <- function(var, breaks, by = c("Approach","gas")) {
  D[, bin := cut(get(var), breaks, include.lowest = TRUE)]
  out <- D[!is.na(bin), .(spike_pct = rate(spike), obs = .N), by = c(by, "bin")][obs >= 200]; D[, bin := NULL]; out }
B.hour  <- D[, .(spike_pct = rate(spike), obs = .N), by = .(Approach, gas, level, hour)][!is.na(level)]
B.snr   <- bin_rate("snr",  c(3, 4, 5, 7, 10, 20, 50, Inf))
B.tsnr  <- bin_rate("tsnr", c(3, 4, 5, 7, 10, 20, 50, Inf), by = "gas")
B.ustar <- bin_rate("ustar_ratio", c(1, 1.25, 1.5, 2, 3, 5, Inf))
B.zeta  <- bin_rate("zeta", c(-Inf, -2, -1, -0.5, -0.1, 0.1, 0.5, 1, Inf))
B.ns    <- bin_rate("nonstat", c(0, 0.25, 0.5, 1, 2, 4, Inf))
B.zrel  <- bin_rate("zB_rel", c(-Inf, -10, -5, -2, 0, 2, 5, 10, Inf), by = c("Approach","gas"))
B.zdz0  <- bin_rate("zd_z0", c(1, 1.5, 2, 3, 5, 10, 30, Inf), by = c("Approach","gas"))[Approach == "WP"]
B.K     <- D[Approach != "MBR" & !is.na(K) & K > 0, .(K, spike, Approach, gas)][, bin := cut(K, unique(quantile(K, seq(0, 1, .1))), include.lowest = TRUE), by = .(Approach, gas)][, .(spike_pct = rate(spike), obs = .N, K_med = median(K)), by = .(Approach, gas, bin)]

# Spike character: magnitude and sign relative to EC
tab.character <- D[!is.na(EC), .(obs = .N, sign_agree_pct = rate(sign_ok), med_abs_GF = median(abs(FG_mean)), med_abs_EC = median(abs(EC)),
                                 med_ratio = median(abs(ratio_GF_EC)[is.finite(ratio_GF_EC)])), by = .(Approach, gas, spike)]

# Temporal clustering: share of spikes falling on days with >= 3 spikes at the same site x approach x gas
day.cnt <- D[spike == TRUE, .N, by = .(Site, Approach, gas, date)]
tab.cluster <- day.cnt[, .(spike_days = .N, spikes = sum(N), pct_spikes_on_clustered_days = 100 * sum(N[N >= 3]) / sum(N)), by = .(Approach, gas)]
tab.sites <- D[, .(obs = .N, spike_pct = rate(spike)), by = .(Site, Approach, gas)][order(-spike_pct)]

# 4. Multivariable model: which conditions raise the odds of a spike? ---------------------------
mods <- list(); OR <- list(); auc <- list()
# log_nonstat is left out: it and log_snr both contain |dC| (r = -0.99: the within-window variance is what dConc_sd is built from), so nonstat is effectively 1/SNR.
fit.vars <- list(MBR = c("level","gas","daynight","season","stab","log_snr","log_tsnr","log_ustar_ratio","zB_rel","dz","transition"),
                 AE  = c("level","gas","daynight","season","stab","log_snr","log_ustar_ratio","zB_rel","dz","transition","log_K"),
                 WP  = c("level","gas","daynight","season","stab","log_snr","log_ustar_ratio","zB_rel","dz","transition","log_K"))
D[, `:=`(log_snr = log(snr), log_tsnr = log(tsnr), log_ustar_ratio = log(ustar_ratio), log_nonstat = log(pmax(nonstat, 1e-3)),
         log_K = log(pmax(K, 1e-4)))]
cat("cor(log_snr, log_nonstat):", round(D[is.finite(log_snr) & is.finite(log_nonstat), cor(log_snr, log_nonstat)], 2), "\n")
auc_fn <- function(y, p) { r <- rank(p); n1 <- as.numeric(sum(y)); n0 <- as.numeric(length(y)) - n1; (sum(r[y == 1]) - n1 * (n1 + 1) / 2) / (n1 * n0) }
for (m in c("MBR","AE","WP")) {
  x <- D[Approach == m, c("spike", fit.vars[[m]]), with = FALSE]; x <- x[complete.cases(x)]
  set.seed(42); x <- rbind(x[spike == TRUE], x[spike == FALSE][sample(.N, min(.N, 10 * sum(x$spike)))])  # case-control sample (ORs unbiased)
  x <- x[is.finite(rowSums(x[, sapply(x, is.numeric), with = FALSE]))]
  num <- names(x)[sapply(x, is.numeric) & names(x) != "spike"]; x[, (num) := lapply(.SD, function(v) as.numeric(scale(v))), .SDcols = num]
  x[, level := factor(level, c("AA","AW"))]; x[, stab := factor(stab, c("near-neutral","unstable","stable"))]
  f <- as.formula(paste("spike ~", paste(fit.vars[[m]], collapse = " + ")))
  g <- glm(f, data = x, family = binomial()); mods[[m]] <- g
  ci <- suppressMessages(confint.default(g))
  OR[[m]] <- data.table(Approach = m, term = names(coef(g)), OR = exp(coef(g)), lo = exp(ci[, 1]), hi = exp(ci[, 2]))[term != "(Intercept)"]
  auc[[m]] <- data.table(Approach = m, n = nrow(x), spikes = sum(x$spike), AUC = auc_fn(x$spike, fitted(g)))
}
OR <- rbindlist(OR); auc <- rbindlist(auc)

save(tab.overall, tab.floor, tab.paired, tab.wp.sing, tab.level, tab.level.site, tab.L2, tab.period, tab.stab, tab.daynight, tab.character, tab.cluster, tab.sites,
     B.hour, B.snr, B.tsnr, B.ustar, B.zeta, B.ns, B.zrel, B.K, B.zdz0, OR, auc,
     file = file.path(spike.dir, paste0("SPIKE_EVAL", run.tag, ".Rdata")))
fwrite(tab.level.site, file.path(spike.dir, paste0("SPIKE_rate_by_level", run.tag, ".csv")))
fwrite(tab.paired, file.path(spike.dir, paste0("SPIKE_AA_vs_AW_paired", run.tag, ".csv")))
fwrite(OR, file.path(spike.dir, paste0("SPIKE_odds_ratios", run.tag, ".csv")))
fwrite(tab.sites, file.path(spike.dir, paste0("SPIKE_rate_by_site", run.tag, ".csv")))

# 5. Figures -----------------------------------------------------------------------------------
pal.ap <- c(MBR = "#B8860B", AE = "#2E8B57", WP = "#6A0DAD"); pal.lv <- c(AA = "goldenrod3", AW = "darkmagenta")
th <- theme_bw(base_size = 10) + theme(strip.background = element_rect(fill = "grey95", colour = "grey80"), panel.grid.minor = element_blank(), legend.position = "top")
ap.f <- function(d) d %>% mutate(Approach = factor(Approach, c("MBR","AE","WP")))

# S1: spike rate by approach x gas x canopy level (site means +- SE across sites)
S1d <- D[!is.na(level), .(rate = rate(spike), n = .N), by = .(Site, Approach, gas, level)][n >= 50] %>% ap.f()
S1 <- ggplot(S1d, aes(Approach, rate, fill = level)) +
  geom_boxplot(outlier.size = 0.6, position = position_dodge(0.8), width = 0.7, alpha = 0.8) +
  facet_wrap(~gas) + scale_fill_manual(values = pal.lv, name = NULL) +
  labs(y = "Spikes (% of filtered 9-min GF, per site)", x = NULL) + th
ggsave(file.path(fig.dir, paste0("SPIKE_rate_level", run.tag, ".png")), S1, width = 7, height = 3.6, dpi = 250)

# S2: diel cycle of spike rate
S2 <- ggplot(ap.f(B.hour[obs >= 200]), aes(hour, spike_pct, colour = level)) + geom_line() + geom_point(size = 0.8) +
  facet_grid(gas ~ Approach, scales = "free_y") + scale_colour_manual(values = pal.lv, name = NULL) +
  annotate("rect", xmin = 7, xmax = 18, ymin = -Inf, ymax = Inf, alpha = 0.06) +
  labs(x = "Hour (local)", y = "Spikes (%)") + th
ggsave(file.path(fig.dir, paste0("SPIKE_rate_hour", run.tag, ".png")), S2, width = 8, height = 4.5, dpi = 250)

# S3: spike rate along measurement-condition gradients
bins <- bind_rows(B.snr %>% mutate(var = "Concentration SNR |dC|/sd"),
                  B.tsnr %>% mutate(Approach = "MBR", var = "Tracer SNR (MBR)"),
                  B.ustar %>% mutate(var = "u* / u* threshold"),
                  B.zeta %>% mutate(var = "Stability (z - d)/L")) %>% ap.f() %>%
  group_by(var, Approach, gas) %>% mutate(i = as.integer(factor(bin, levels = unique(bin)))) %>% ungroup()
S3 <- ggplot(bins, aes(bin, spike_pct, colour = Approach, linetype = gas, group = interaction(Approach, gas))) +
  geom_line() + geom_point(size = 0.9) + facet_wrap(~var, scales = "free", nrow = 2) +
  scale_colour_manual(values = pal.ap) + labs(x = NULL, y = "Spikes (%)", linetype = NULL, colour = NULL) +
  th + theme(axis.text.x = element_text(angle = 40, hjust = 1, size = 7))
ggsave(file.path(fig.dir, paste0("SPIKE_rate_conditions", run.tag, ".png")), S3, width = 9.5, height = 6, dpi = 250)

# S4: eddy diffusivity (AE, WP) and effective-height floor
S4a <- ggplot(ap.f(B.K[obs >= 200]), aes(K_med, spike_pct, colour = Approach, linetype = gas)) + geom_line() + geom_point(size = 0.9) +
  scale_x_log10() + scale_colour_manual(values = pal.ap) + labs(x = expression(K~"decile median (m"^2~"s"^-1*")"), y = "Spikes (%)", colour = NULL, linetype = NULL) + th
S4b <- ggplot(ap.f(tab.floor[!is.na(level)]), aes(interaction(level, ifelse(h_floor, "floored", "z-d > 0.1")), spike_pct, fill = Approach)) +
  geom_col(position = "dodge") + facet_wrap(~gas) + scale_fill_manual(values = pal.ap) + labs(x = NULL, y = "Spikes (%)", fill = NULL) +
  th + theme(axis.text.x = element_text(angle = 30, hjust = 1))
S4c <- ggplot(B.zdz0, aes(bin, spike_pct, linetype = gas, group = gas)) + geom_line(colour = pal.ap["WP"]) + geom_point(colour = pal.ap["WP"], size = 0.9) +
  labs(x = expression((z - d) / z[0]~"(WP)"), y = "Spikes (%)", linetype = NULL) + th + theme(axis.text.x = element_text(angle = 30, hjust = 1))
ggsave(file.path(fig.dir, paste0("SPIKE_K", run.tag, ".png")), ggarrange(S4a, S4b, S4c, ncol = 3, labels = c("a", "b", "c"), widths = c(1, 1.1, 0.9)), width = 12, height = 3.8, dpi = 250)

# S5: odds ratios from the multivariable model
lab <- c(levelAW = "Canopy level AW (vs AA)", gasH2O = "H2O (vs CO2)", daynightnight = "Night", seasonSpring = "Spring", seasonSummer = "Summer",
         seasonWinter = "Winter", stabunstable = "Unstable (vs near-neutral)", stabstable = "Stable (vs near-neutral)", log_snr = "log SNR (+1 SD)",
         log_tsnr = "log tracer SNR (+1 SD)", log_ustar_ratio = "log u*/threshold (+1 SD)", log_nonstat = "log noise/gradient (+1 SD)",
         zB_rel = "Upper inlet - hc (+1 SD)", dz = "zA - zB (+1 SD)", transitionTRUE = "Morning/evening transition", log_K = "log K (+1 SD)")
S5 <- ggplot(ap.f(OR %>% mutate(term = factor(lab[term], rev(unname(lab))))), aes(OR, term, colour = Approach)) +
  geom_vline(xintercept = 1, linetype = 2, colour = "grey50") + geom_pointrange(aes(xmin = lo, xmax = hi), position = position_dodge(0.6), size = 0.25) +
  scale_x_log10() + scale_colour_manual(values = pal.ap) + labs(x = "Odds ratio of a spike (log scale)", y = NULL, colour = NULL) + th
ggsave(file.path(fig.dir, paste0("SPIKE_odds_ratios", run.tag, ".png")), S5, width = 6.5, height = 5.5, dpi = 250)
