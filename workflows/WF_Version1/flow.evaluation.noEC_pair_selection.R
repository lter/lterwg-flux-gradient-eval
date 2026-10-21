# flow.evaluation.noEC_pair_selection.R ----------------------------------------
# Supplement S2: choosing sampling-height pairs without an eddy-covariance (EC) reference,
# e.g. for CH4. Tested on CO2 and H2O, where EC is used ONLY to score the result.
#
# Rule (no EC):
#   1. Reference REF = equal-weight 30-min mean of all above-canopy (AA) MBR pairs at a site.
#   2. Compute Lin's CCC between every AE / WP pair (AA or AW) and REF (>= 30 shared windows);
#      keep pairs with CCC_ref >= ccc.cut.
#   3. Scale each kept pair to REF with a reduced-major-axis fit over the shared windows:
#        b = sign(r) * sd(F) / sd(REF),  a = mean(F) - b * mean(REF),  F' = (F - a) / b
#      (F' has REF's mean and variance but keeps its own time series).
#   4. Ensemble = equal-weight 30-min mean of the AA MBR fluxes and the scaled AE / WP fluxes.
#
# Pre-checks (does the rule work at a site? no EC for the target gas needed):
#   A. AA MBR works for the OTHER gases at the site: AA MBR ensemble CCC vs EC >= ccc.cut for another
#      gas with EC (CO2/H2O have EC at every NEON site, so this is available for CH4 too).
#   B. Independent methods agree with MBR: at least one AE / WP pair has CCC_ref >= ccc.cut.
#   Site passes if A or B. Validated here on CO2 and H2O against the scaled ensemble's CCC vs EC.
#
# Compared with: all AA pairs, AA MBR only, AA MBR + unscaled AE/WP, and pairs selected on
# CCC >= 0.5 against EC (the current reliable sampling-height pairs; uses EC).
#
# Inputs : localdir/NEON_GradientFlux_Data_Filter/SITES_One2One<run.tag>.Rdata
#          localdir/NEON_GradientFlux_Data_Filter/SITES_<MBR|AE|WP>_9min_FILTER<run.tag>.Rdata
# Outputs: localdir/Spike_Eval/noEC_*<run.tag>.csv
#          DirRepo.eval/Figures/WF_Version1/S2_noEC_pair_selection<run.tag>.png
# -----------------------------------------------------------------------------

library(data.table)
library(ggplot2)
library(ggpubr)

if (!exists("run.tag"))      run.tag <- "_AA_AW_2021_2026"
if (!exists("localdir"))     localdir <- "/Volumes/MaloneLab/Research/FluxGradient"
if (!exists("DirRepo.eval")) DirRepo.eval <- "/Users/sm3466/Library/CloudStorage/Dropbox-YSE/Sparkle Malone/Research/FluxGradient/lterwg-flux-gradient-eval"
if (!exists("ccc.cut"))      ccc.cut <- 0.5   # CCC threshold, against EC (current rule) and against REF (new rule)
if (!exists("min.windows"))  min.windows <- 30
filter.dir <- file.path(localdir, "NEON_GradientFlux_Data_Filter")
out.dir    <- file.path(localdir, "Spike_Eval"); dir.create(out.dir, showWarnings = FALSE)
fig.dir    <- file.path(DirRepo.eval, "Figures/WF_Version1")

lccc <- function(x, y) { mx <- mean(x); my <- mean(y)
  2 * mean((x - mx) * (y - my)) / (mean((x - mx)^2) + mean((y - my)^2) + (mx - my)^2) }

# 1. Pair table (CCC against EC, canopy level) ---------------------------------------------
load(file.path(filter.dir, paste0("SITES_One2One", run.tag, ".Rdata")))
pairs <- as.data.table(SITES_One2One)[!is.na(CCC), .(Site, gas, Approach = as.character(Approach),
                                                     pair = dLevelsAminusB, CCC, level = substr(Canopy_L1, 1, 2))]
# (SITES_One2One is left in place for scripts sourced after this one)

# 2. 30-min gradient fluxes per pair (filtered + despiked 9-min GF averaged to 30 min) -----
read_approach <- function(m) {
  e <- new.env(); load(file.path(filter.dir, paste0("SITES_", m, "_9min_FILTER", run.tag, ".Rdata")), envir = e)
  L <- get(ls(e)[1], envir = e)
  x <- rbindlist(lapply(names(L), function(s) { d <- L[[s]]
    data.table(Site = s, gas = d$gas, pair = d$dLevelsAminusB, GF = d$FG_mean, EC = d$EC_mean,
               # local clock time (stored as POSIXlt); converted via text so the time zone is kept
               t = as.numeric(as.POSIXct(format(d$timeEndA.local, "%Y-%m-%d %H:%M:%S"), tz = "UTC"))) }))
  x[gas %in% c("CO2", "H2O") & !is.na(GF)][, `:=`(Approach = m, t30 = floor(t / 1800))]
}
P30 <- rbindlist(lapply(c("MBR", "AE", "WP"), read_approach))[
  , .(GF = mean(GF), EC = mean(EC, na.rm = TRUE)), by = .(Site, gas, Approach, pair, t30)]
invisible(gc())
EC30 <- P30[!is.na(EC), .(EC = mean(EC)), by = .(Site, gas, t30)]

# 3. Reference = AA MBR ensemble; CCC of every AE / WP pair against it (no EC) -------------
aa.mbr <- pairs[Approach == "MBR" & level == "AA", .(Site, gas, Approach, pair)]
REF <- P30[aa.mbr, on = .(Site, gas, Approach, pair), nomatch = 0][, .(REF = mean(GF)), by = .(Site, gas, t30)]
X <- P30[Approach != "MBR"][REF, on = .(Site, gas, t30), nomatch = 0]
ref.stats <- X[, .(n_ref = .N,
                   CCC_ref = if (.N >= min.windows) lccc(GF, REF) else NA_real_,
                   b = sign(cor(GF, REF)) * sd(GF) / sd(REF),
                   a = mean(GF) - sign(cor(GF, REF)) * sd(GF) / sd(REF) * mean(REF)),
               by = .(Site, gas, Approach, pair)]
pairs[ref.stats, on = .(Site, gas, Approach, pair), `:=`(CCC_ref = i.CCC_ref, n_ref = i.n_ref, scale_b = i.b, scale_a = i.a)]
sel <- pairs[Approach != "MBR" & CCC_ref >= ccc.cut]

# 4. Ensembles, scored against EC ----------------------------------------------------------
get_flux <- function(k) P30[k[, .(Site, gas, Approach, pair)], on = .(Site, gas, Approach, pair), nomatch = 0]
score <- function(Y, lab) Y[, .(GF = mean(GF)), by = .(Site, gas, t30)][EC30, on = .(Site, gas, t30), nomatch = 0][
  , .(windows = .N, CCC = lccc(GF, EC)), by = .(Site, gas)][, rule := lab]
scaled <- get_flux(sel)[sel, on = .(Site, gas, Approach, pair), GF := (GF - i.scale_a) / i.scale_b]
rules <- c("All AA pairs\n(no EC)", "AA MBR\n(no EC)", "AA MBR +\nunscaled AE/WP\n(no EC)", "AA MBR +\nscaled AE/WP\n(no EC)",
           "Pairs with\nCCC >= 0.5\n(uses EC)")
ens <- rbind(
  score(get_flux(pairs[level == "AA"]), rules[1]),
  score(get_flux(aa.mbr), rules[2]),
  score(rbind(get_flux(aa.mbr), get_flux(sel)), rules[3]),
  score(rbind(get_flux(aa.mbr), scaled), rules[4]),
  score(get_flux(pairs[CCC >= ccc.cut]), rules[5]))
ens[, rule := factor(rule, levels = rules)]

# 5. Tables --------------------------------------------------------------------------------
tab.pairs <- pairs[, .(pairs = .N, pct_CCC_ge = round(100 * mean(CCC >= ccc.cut)), median_CCC = round(median(CCC), 2)),
                   by = .(gas, Approach, level)][order(gas, level, Approach)]
tab.screen <- pairs[Approach != "MBR" & !is.na(CCC_ref), .(
  pairs = .N, r_CCCref_CCC_EC = round(cor(CCC_ref, CCC), 2),
  pass_screen = sum(CCC_ref >= ccc.cut), pass_both = sum(CCC_ref >= ccc.cut & CCC >= ccc.cut),
  reliable_vs_EC = sum(CCC >= ccc.cut), median_scale_b = round(median(scale_b[CCC_ref >= ccc.cut]), 2))]
tab.ens <- ens[, .(sites = .N, mean_CCC = round(mean(CCC), 2), median_CCC = round(median(CCC), 2),
                   sites_ge_0.5 = sum(CCC >= 0.5), sites_ge_0.3 = sum(CCC >= 0.3)), by = .(gas, rule)][order(gas, rule)]
gain <- dcast(ens[rule %in% rules[2:4]], Site + gas ~ as.integer(rule), value.var = "CCC")
setnames(gain, c("2", "3", "4"), c("AA_MBR", "unscaled", "scaled"))
gain <- gain[paste(Site, gas) %in% sel[, paste(Site, gas)]]          # site-gas combinations that gain AE/WP pairs
tab.gain <- gain[, .(site_gas = .N, unscaled_better = sum(unscaled > AA_MBR + .05), unscaled_worse = sum(unscaled < AA_MBR - .05),
                     scaled_better = sum(scaled > AA_MBR + .05), scaled_worse = sum(scaled < AA_MBR - .05))]
print(tab.pairs); print(tab.screen); print(tab.ens); print(tab.gain)

# 5b. EC-free pre-checks: will the rule work at this site? -----------------------------------
#   A = AA MBR ensemble CCC vs EC of the other gas(es) at the site (max over other gases)
#   B = best CCC_ref of any AE / WP pair of this gas (gradient fluxes only)
ens.w <- dcast(ens, Site + gas ~ as.integer(rule), value.var = "CCC")
ens.w <- ens.w[, .(Site, gas, AAMBR_CCC = `2`, ensemble_CCC = `4`)]   # ensemble_CCC = the rule's result (truth for validation)
pre <- ens.w[, .(Site, gas, ensemble_CCC)]
pre[, other_gas_AAMBR_CCC := sapply(seq_len(.N), function(i)
  suppressWarnings(max(ens.w[Site == pre$Site[i] & gas != pre$gas[i], AAMBR_CCC], na.rm = TRUE)))]
pre[!is.finite(other_gas_AAMBR_CCC), other_gas_AAMBR_CCC := NA_real_]
agree <- pairs[Approach != "MBR", .(max_AEWP_CCC_ref = suppressWarnings(max(CCC_ref, na.rm = TRUE)),
                                    n_AEWP_agree = sum(CCC_ref >= ccc.cut, na.rm = TRUE)), by = .(Site, gas)]
agree[!is.finite(max_AEWP_CCC_ref), max_AEWP_CCC_ref := NA_real_]
pre <- agree[pre, on = .(Site, gas)]
pre[, `:=`(check_A_other_gas = !is.na(other_gas_AAMBR_CCC) & other_gas_AAMBR_CCC >= ccc.cut,
           check_B_methods_agree = !is.na(n_AEWP_agree) & n_AEWP_agree >= 1)]
pre[, precheck_pass := check_A_other_gas | check_B_methods_agree]
auc <- function(y, s) { ok <- !is.na(s); y <- y[ok]; s <- s[ok]; r <- rank(s); n1 <- sum(y); n0 <- length(y) - n1
  (sum(r[y]) - n1 * (n1 + 1) / 2) / (n1 * n0) }
validate <- function(ok, lab) pre[, .(check = lab, pass = sum(ok), pass_ensemble_ge_cut_pct = round(100 * mean(ensemble_CCC[ok] >= ccc.cut)),
  pass_median_CCC = round(median(ensemble_CCC[ok]), 2), fail = sum(!ok), fail_ensemble_lt_cut_pct = round(100 * mean(ensemble_CCC[!ok] < ccc.cut)),
  fail_median_CCC = round(median(ensemble_CCC[!ok]), 2))]
tab.precheck <- rbind(validate(pre$check_A_other_gas, "A: other gas AA MBR CCC >= cut"),
                      validate(pre$check_B_methods_agree, "B: an AE/WP pair agrees with AA MBR"),
                      validate(pre$precheck_pass, "A or B (pre-check)"),
                      validate(pre$check_A_other_gas & pre$check_B_methods_agree, "A and B"))
tab.precheck.auc <- pre[, .(AUC_other_gas = round(auc(ensemble_CCC >= ccc.cut, other_gas_AAMBR_CCC), 2),
                            AUC_max_AEWP_CCC_ref = round(auc(ensemble_CCC >= ccc.cut, max_AEWP_CCC_ref), 2))]
print(tab.precheck); print(tab.precheck.auc)
print(pre[precheck_pass == FALSE, .(Site, gas, ensemble_CCC = round(ensemble_CCC, 2))][order(gas, Site)])
fwrite(pre, file.path(out.dir, paste0("noEC_prechecks_by_site", run.tag, ".csv")))
fwrite(tab.precheck, file.path(out.dir, paste0("noEC_precheck_validation", run.tag, ".csv")))

fwrite(pairs, file.path(out.dir, paste0("noEC_pairs_CCC_vs_AAMBR_reference", run.tag, ".csv")))
fwrite(dcast(ens, Site + gas ~ rule, value.var = "CCC"), file.path(out.dir, paste0("noEC_ensemble_CCC_by_rule", run.tag, ".csv")))
fwrite(tab.ens, file.path(out.dir, paste0("noEC_ensemble_summary", run.tag, ".csv")))

# 6. Figure S6 -----------------------------------------------------------------------------
th <- theme_bw(base_size = 10) + theme(panel.grid.minor = element_blank(), legend.position = "top",
                                       strip.background = element_rect(fill = "grey95", colour = "grey80"))
pa <- ggplot(ens[rule != rules[3]], aes(rule, CCC)) +
  geom_hline(yintercept = 0.5, linetype = 2, colour = "grey50") +
  geom_boxplot(outlier.shape = NA, width = 0.55, fill = "grey92") +
  geom_jitter(width = 0.12, size = 0.9, alpha = 0.6, colour = "#B8860B") +
  facet_wrap(~gas) + labs(x = NULL, y = "Site ensemble CCC vs EC (30 min)") + th
pb <- ggplot(pairs[Approach != "MBR" & !is.na(CCC_ref)], aes(CCC_ref, CCC, colour = Approach)) +
  geom_abline(linetype = 3, colour = "grey60") +
  annotate("rect", xmin = ccc.cut, xmax = 1, ymin = ccc.cut, ymax = 1, alpha = 0.08, fill = "darkgreen") +
  geom_vline(xintercept = ccc.cut, linetype = 2, colour = "grey50") + geom_hline(yintercept = ccc.cut, linetype = 2, colour = "grey50") +
  geom_point(size = 0.9, alpha = 0.5) + scale_colour_manual(values = c(AE = "#2E8B57", WP = "#6A0DAD")) +
  coord_cartesian(xlim = c(-0.5, 1), ylim = c(-0.5, 1)) +
  labs(x = "CCC vs AA MBR reference (no EC)", y = "CCC vs EC", colour = NULL) + th
pc <- ggplot(gain, aes(AA_MBR, scaled, colour = gas)) + geom_abline(linetype = 2, colour = "grey50") + geom_point(size = 1.6) +
  scale_colour_manual(values = c(CO2 = "#000CCC", H2O = "#009966")) + coord_equal(xlim = c(0, 1), ylim = c(0, 1)) +
  labs(x = "AA MBR ensemble CCC", y = "AA MBR + scaled AE/WP CCC", colour = NULL) + th
fig <- ggarrange(pa, ggarrange(pb, pc, ncol = 2, labels = c("b", "c"), widths = c(1.15, 1)), nrow = 2, labels = c("a", ""))
ggsave(file.path(fig.dir, paste0("S2_noEC_pair_selection", run.tag, ".png")), fig, width = 9, height = 8, dpi = 250, bg = "white")
