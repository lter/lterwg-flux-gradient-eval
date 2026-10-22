# flow.evaluation.sampling_time.R ---------------------------------------------
# Does the time between the two sequential inlet samples of a gradient flux (GF) affect its agreement
# with EC (CCC)? NEON samples one inlet at a time (6-min average after ~4 min of flushing, top to bottom),
# so the two concentrations of a pair are measured 10-60 min apart (centre to centre), in either order.
#
# Inputs : spike-evaluation extracts (filtered GF; localdir/Spike_Eval/extract<period>/SITE_<approach>.csv.gz,
#          written by flow.evaluation.spike_eval.R), canopy_commbined.csv
# Despike: as despike_fluxes() (median +/- 6 MAD by site x approach x gas x pair x day/night x season)
# Outputs: Review_Supplement/SAMPLING_TIME_*<run.tag>.csv, Figures/WF_Version1/SAMPLING_TIME<run.tag>.png
# Analyses:
#   1. distribution of the centre-to-centre separation (sep, min) of the two samples
#   2. between pairs: CCC vs median sep, alone and controlling for geometry (sep grows with inlet separation)
#   3. within pairs: CCC of the same pair computed separately for its values with sep <= 15, 15-30 and > 30 min
#      (the same pair is sampled 10 min apart when the lower inlet precedes the upper one and up to 50 min
#      apart in the other order), which holds geometry and site fixed
# -----------------------------------------------------------------------------
suppressMessages({library(data.table); library(ggplot2); library(ggpubr)})
if (!exists("run.tag"))  run.tag  <- "_AA_AW_2021_2026"
if (!exists("localdir")) localdir <- "/Volumes/MaloneLab/Research/FluxGradient"
if (!exists("DirRepo.eval")) DirRepo.eval <- "/Users/sm3466/Library/CloudStorage/Dropbox-YSE/Sparkle Malone/Research/FluxGradient/lterwg-flux-gradient-eval"
ex.dir  <- file.path(localdir, "Spike_Eval", paste0("extract", sub("^_AA_AW", "", run.tag)))
out.dir <- file.path(localdir, "Review_Supplement"); dir.create(out.dir, showWarnings = FALSE)
fig.dir <- file.path(DirRepo.eval, "Figures", "WF_Version1")

ccc <- function(x, y) { ok <- is.finite(x) & is.finite(y); x <- x[ok]; y <- y[ok]; n <- length(x); if (n < 30) return(NA_real_)
  mx <- mean(x); my <- mean(y); 2 * sum((x - mx) * (y - my)) / n / (sum((x - mx)^2)/n + sum((y - my)^2)/n + (mx - my)^2) }
canopy <- unique(fread(file.path(localdir, "canopy_commbined.csv"))[Canopy_L1 != "WW",
            .(Site, dLevelsAminusB, Canopy_L1, hc = CanopyHeight, zA = MeasurementHeight_m_A, zB = MeasurementHeight_m_B)], by = c("Site", "dLevelsAminusB"))
tt <- function(v) as.numeric(as.POSIXct(v, tz = "UTC"))

R <- list()
for (f in list.files(ex.dir, "\\.csv\\.gz$", full.names = TRUE)) {
  s <- sub("_(AE|WP|MBR).*", "", basename(f)); m <- sub(".*_(AE|WP|MBR)\\.csv\\.gz", "\\1", basename(f))
  x <- fread(f, select = c("gas","dLevelsAminusB","FG_mean","FC_turb_interp","FH2O_interp","timeBgn_A","timeEnd_A","timeBgn_B","timeEnd_B","timeEndA.local"))
  x <- x[gas %in% c("CO2","H2O")]
  x[, EC := fifelse(gas == "CO2", FC_turb_interp, FH2O_interp)]
  x[, sep := abs((tt(timeBgn_A) + tt(timeEnd_A)) - (tt(timeBgn_B) + tt(timeEnd_B))) / 2 / 60]   # centre to centre, min
  hr <- as.integer(substr(x$timeEndA.local, 12, 13)); mo <- as.integer(substr(x$timeEndA.local, 6, 7))
  x[, `:=`(dn = hr %in% 7:18, season = (mo %% 12) %/% 3)]
  x[, `:=`(med = median(FG_mean), mad_ = mad(FG_mean), n_ = .N), by = .(gas, dLevelsAminusB, dn, season)]
  x <- x[n_ < 20 | abs(FG_mean - med) <= 6 * pmax(mad_, 1e-6)]
  R[[f]] <- x[, .(Site = s, Approach = m, gas, dLevelsAminusB, FG_mean, EC, sep)]
}
D <- merge(rbindlist(R), canopy, by = c("Site", "dLevelsAminusB")); rm(R); gc()
D[, sbin := cut(sep, c(-Inf, 15, 30, Inf), labels = c("10 min", "20-30 min", "> 30 min"))]

# 1. distribution
dist <- D[, .N, by = .(gas, sbin)][, pct := round(100 * N / sum(N), 1), by = gas]
# 2. between pairs
P <- D[, .(n = .N, CCC = ccc(EC, FG_mean), sep = median(sep), zA_rel = zA[1] - hc[1], dz = zB[1] - zA[1],
           level = substr(Canopy_L1[1], 1, 2), hc = hc[1]), by = .(Site, Approach, gas, dLevelsAminusB)][is.finite(CCC)]
between <- rbindlist(lapply(split(P, by = c("Approach", "gas")), function(d) {
  cf <- summary(lm(CCC ~ I(sep / 10) + zA_rel + dz + level + log(hc + 0.1), data = d))$coefficients
  data.table(Approach = d$Approach[1], gas = d$gas[1], pairs = nrow(d),
             rho = cor(d$CCC, d$sep, method = "spearman"), p_rho = cor.test(d$CCC, d$sep, method = "spearman", exact = FALSE)$p.value,
             coef_per10min_adj = cf[2, 1], se = cf[2, 2], p_adj = cf[2, 4], r_sep_dz = cor(d$sep, d$dz)) }))
# 3. within pairs
W <- D[, .(n = .N, CCC = ccc(EC, FG_mean)), by = .(Site, Approach, gas, dLevelsAminusB, sbin)][n >= 100 & is.finite(CCC)]
Wd <- dcast(W, Site + Approach + gas + dLevelsAminusB ~ sbin, value.var = "CCC")
cmp <- function(a, b) { k <- is.finite(Wd[[a]]) & is.finite(Wd[[b]])
  Wd[k, .(comparison = paste(b, "vs", a), pairs = .N, median_dCCC = median(get(b) - get(a)), pct_pairs_lower = 100 * mean(get(b) < get(a)),
          p_wilcoxon = if (.N >= 5) wilcox.test(get(b), get(a), paired = TRUE, exact = FALSE)$p.value else NA_real_), by = .(Approach, gas)] }
within <- rbind(cmp("10 min", "20-30 min"), cmp("20-30 min", "> 30 min"), cmp("10 min", "> 30 min"))
rel <- D[P[CCC >= 0.5, .(Site, Approach, gas, dLevelsAminusB)], on = .(Site, Approach, gas, dLevelsAminusB), nomatch = 0][, .(pct_values_gt30 = 100 * mean(sep > 30)), by = gas]
print(dist); print(between, digits = 2); print(within, digits = 2); print(rel)
fwrite(dist, file.path(out.dir, paste0("SAMPLING_TIME_distribution", run.tag, ".csv")))
fwrite(P, file.path(out.dir, paste0("SAMPLING_TIME_pair_CCC", run.tag, ".csv")))
fwrite(between, file.path(out.dir, paste0("SAMPLING_TIME_between_pairs", run.tag, ".csv")))
fwrite(W, file.path(out.dir, paste0("SAMPLING_TIME_within_pair_CCC", run.tag, ".csv")))
fwrite(within, file.path(out.dir, paste0("SAMPLING_TIME_within_pair_tests", run.tag, ".csv")))

# Figure
th <- theme_bw(base_size = 10) + theme(strip.background = element_rect(fill = "grey95", colour = "grey80"), panel.grid.minor = element_blank(), legend.position = "top")
pal <- c(MBR = "#B8860B", AE = "#2E8B57", WP = "#6A0DAD")
pa <- ggplot(dist, aes(sbin, pct, fill = gas)) + geom_col(position = "dodge", width = 0.7) +
  scale_fill_manual(values = c(CO2 = "grey35", H2O = "steelblue"), name = NULL) +
  labs(x = "Time between samples (centre to centre)", y = "Filtered GF values (%)") + th
Wl <- melt(Wd, id.vars = c("Site", "Approach", "gas", "dLevelsAminusB"), variable.name = "sbin", value.name = "CCC")[is.finite(CCC)]
Wl[, ref := Wd[.SD, on = .(Site, Approach, gas, dLevelsAminusB), `10 min`]][, dCCC := CCC - ref]
pb <- ggplot(Wl[sbin != "10 min" & is.finite(dCCC)], aes(sbin, dCCC, fill = factor(Approach, c("MBR", "AE", "WP")))) +
  geom_hline(yintercept = 0, linetype = 2, colour = "grey50") + geom_boxplot(outlier.size = 0.5, width = 0.7) + facet_wrap(~ gas) +
  scale_fill_manual(values = pal, name = NULL) + coord_cartesian(ylim = c(-0.6, 0.4)) +
  labs(x = "Same pair, samples further apart", y = "CCC change vs samples 10 min apart") + th
pc <- ggplot(P, aes(sep, CCC, colour = factor(Approach, c("MBR", "AE", "WP")))) +
  geom_hline(yintercept = 0.5, linetype = 2, colour = "red") + geom_jitter(width = 1.5, height = 0, size = 0.7, alpha = 0.6) +
  geom_smooth(method = "loess", se = FALSE, span = 1, linewidth = 0.7) + facet_wrap(~ gas) + scale_colour_manual(values = pal, name = NULL) +
  labs(x = "Median time between the two samples of the pair (min)", y = "CCC of the pair") + th
fig <- ggarrange(ggarrange(pa, pc, ncol = 2, widths = c(0.8, 1.4), labels = c("(a)", "(b)")), pb, nrow = 2, labels = c("", "(c)"))
ggsave(file.path(fig.dir, paste0("SAMPLING_TIME", run.tag, ".png")), fig, width = 10, height = 8, dpi = 250, bg = "white")
