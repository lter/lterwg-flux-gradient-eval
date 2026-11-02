# ch4method_site.R  SITE  EDI_DIR  OUT_DIR  CODE_DIR
# Tests ways to choose sampling-height pairs for a gas WITHOUT that gas's EC (the CH4 case), using the
# previous (published) SD-based SNR filter (3) + despike (k = 6). Each rule is applied to CO2 and H2O,
# where EC scores it, and to CH4.
#   EC      : pairs with CCC >= 0.5 vs EC of the gas itself (current CO2/H2O product; benchmark)
#   S2      : AA MBR reference + AE/WP with 30-min CCC_ref >= 0.5 (>= 30 shared half-hours), rescaled (RMA)
#   S2day   : as S2, agreement on daily means (>= 10 shared days)
#   XG_CO2, XG_H2O, XG_both : pairs that are RSHP (CCC >= 0.5 vs EC) for CO2 / H2O / both; equal weights
#             (for CO2 only XG_H2O, for H2O only XG_CO2 are EC-free)
# Ensembles: calc.ensemble.site() (half-hour of the sampling midpoint) with equal weights for XG rules.
suppressPackageStartupMessages({ library(data.table); library(dplyr) })
a <- commandArgs(TRUE); site <- a[1]; edi <- a[2]; out <- a[3]; code <- a[4]
source(file.path(code, "calc.filter_FG.R")); source(file.path(code, "calc.ensemble.R"))
od <- file.path(out, "ch4method", site); dir.create(od, recursive = TRUE, showWarnings = FALSE)
canopy <- fread(file.path(code, "canopy_L1.csv"))[Site == site & Canopy_L1 != "WW"]
pairs.ok <- unique(canopy$dLevelsAminusB); lvl <- setNames(canopy$Canopy_L1, canopy$dLevelsAminusB)
cols <- c("timeEnd_A", "timeBgn_B", "timeEndA.local", "gas", "dLevelsAminusB", "TowerPosition_A", "TowerPosition_B",
          "dConc", "dConc_sd", "vari_A", "vari_B", "numSamp_A", "numSamp_B", "dConc.tracer", "dConc.tracer_sd", "tracer",
          "ustar_interp", "ustar_threshold", "FG_mean", "FC_turb_interp", "FH2O_interp")
read_app <- function(app) {
  f <- file.path(edi, site, paste0(site, "_", app, "_9min.df.final.csv"))
  if (!file.exists(f)) return(NULL)
  x <- fread(f, select = intersect(cols, names(fread(f, nrows = 1))), showProgress = FALSE)
  for (cc in c("timeEnd_A", "timeBgn_B", "timeEndA.local")) if (cc %in% names(x)) x[[cc]] <- as.character(x[[cc]])
  x <- x[dLevelsAminusB %in% pairs.ok]
  x[, timeEndA.local := as.POSIXct(substr(timeEndA.local, 1, 19), tz = "UTC", format = "%Y-%m-%d %H:%M:%S")]
  x[, `:=`(EC_mean = fifelse(gas == "CO2", FC_turb_interp, fifelse(gas == "H2O", FH2O_interp, NA_real_)),
           Canopy_L1 = lvl[dLevelsAminusB], Approach = app, Site = site)]
  as.data.frame(x)
}
D <- list(MBR = read_app("MBR"), AE = read_app("AE"), WP = read_app("WP")); D <- D[!sapply(D, is.null)]
lccc <- function(x, y) { mx <- mean(x); my <- mean(y)
  2 * mean((x - mx) * (y - my)) / (mean((x - mx)^2) + mean((y - my)^2) + (mx - my)^2) }
K <- rbindlist(lapply(names(D), function(app) {
  k <- filter_fluxes(D[[app]], dConcSNR.min = 3, approach = app, snr.type = "SD")
  k <- k[is.finite(k$FG_mean), ]
  as.data.table(despike_fluxes(k, k = 6))[, .(Site, Approach, gas, dLevelsAminusB, Canopy_L1, timeEnd_A, timeBgn_B,
                                            timeEndA.local, FG_mean, EC_mean)]
}), fill = TRUE)
K[, pair := paste(Approach, dLevelsAminusB)]
pc <- K[gas %in% c("CO2", "H2O") & is.finite(EC_mean), .(CCC = if (.N >= 3) lccc(EC_mean, FG_mean) else NA_real_,
       RMSE = sqrt(mean((FG_mean - EC_mean)^2))), by = .(gas, Approach, dLevelsAminusB, pair)]
rshp <- function(g) pc[gas == g & CCC >= 0.5, pair]
ens_from <- function(g, pairs, weights = c("rmse", "equal")) {
  weights <- match.arg(weights)
  x <- K[gas == g & pair %in% pairs]
  if (!nrow(x)) return(NULL)
  x[, CCC := 1]
  if (weights == "rmse") x <- merge(x, pc[gas == g, .(pair, RMSE)], by = "pair", all.x = TRUE) else x[, RMSE := 1]
  as.data.table(calc.ensemble.site(as.data.frame(x)))[, .(t = as.numeric(time_start_utc), FG = FG_ENSEMBLE, EC = EC_mean, npairs = FG_ENSEMBLE_NPAIRS)]
}
# S2 rules on 30-min means (t30 = half-hour of timeEndA.local, as in flow.evaluation.noEC_pair_selection.R)
K[, t30 := floor(as.numeric(timeEndA.local) / 1800)]
P30 <- K[, .(GF = mean(FG_mean), EC = mean(EC_mean)), by = .(gas, Approach, Canopy_L1, pair, t30)]
s2 <- function(g, daily = FALSE) {
  aa <- P30[gas == g & Approach == "MBR" & Canopy_L1 == "AA"]
  if (!nrow(aa)) return(list(ens = NULL, sel = character()))
  REF <- aa[, .(REF = mean(GF)), by = t30]; sel <- list(); info <- list()
  for (pp in unique(P30[gas == g & Approach != "MBR", pair])) {
    G <- merge(P30[gas == g & pair == pp], REF, by = "t30")
    if (daily) {
      Gd <- G[, .(GF = mean(GF), REF = mean(REF)), by = .(d = t30 %/% 48)]
      cr <- if (nrow(Gd) >= 10) lccc(Gd$GF, Gd$REF) else NA_real_
    } else cr <- if (nrow(G) >= 30) lccc(G$GF, G$REF) else NA_real_
    b <- if (nrow(G) >= 3) sign(cor(G$GF, G$REF)) * sd(G$GF) / sd(G$REF) else NA_real_
    a0 <- if (nrow(G) >= 3) mean(G$GF) - b * mean(G$REF) else NA_real_
    info[[pp]] <- data.table(pair = pp, n = nrow(G), CCC_ref = cr)
    if (is.finite(cr) && cr >= 0.5 && is.finite(b) && b != 0)
      sel[[pp]] <- P30[gas == g & pair == pp][, GF := (GF - a0) / b]
  }
  e <- rbindlist(c(list(aa), sel), fill = TRUE)[, .(FG = mean(GF), EC = mean(EC, na.rm = TRUE), npairs = .N), by = t30][, t := t30 * 1800][, t30 := NULL]
  list(ens = e, sel = names(sel), info = rbindlist(info), rows = rbindlist(c(list(aa), sel), fill = TRUE))
}
# Cross-gas with a CO2-RSHP reference: REF = equal-weight 30-min mean of the CO2-RSHP pairs (target gas);
# pairs that are RSHP for H2O but not CO2 enter only if CCC_ref >= 0.5 vs REF (>= 30 half-hours), rescaled (RMA).
xg_ref <- function(g, base.pairs, cand.pairs, rescale = TRUE, cut = 0.5) {
  b <- P30[gas == g & pair %in% base.pairs]
  if (!nrow(b)) return(list(ens = NULL, sel = character()))
  REF <- b[, .(REF = mean(GF)), by = t30]; sel <- list()
  for (pp in setdiff(cand.pairs, base.pairs)) {
    G <- merge(P30[gas == g & pair == pp], REF, by = "t30")
    cr <- if (nrow(G) >= 30) lccc(G$GF, G$REF) else NA_real_
    if (!is.finite(cr) || cr < cut) next
    bb <- sign(cor(G$GF, G$REF)) * sd(G$GF) / sd(G$REF); a0 <- mean(G$GF) - bb * mean(G$REF)
    if (!is.finite(bb) || bb == 0) next
    x <- P30[gas == g & pair == pp]; if (rescale) x[, GF := (GF - a0) / bb]; sel[[pp]] <- x
  }
  e <- rbindlist(c(list(b), sel), fill = TRUE)[, .(FG = mean(GF), EC = mean(EC, na.rm = TRUE), npairs = uniqueN(pair)), by = t30][, t := t30 * 1800][, t30 := NULL]
  list(ens = e, sel = c(base.pairs, names(sel)))
}
rshp_cut <- function(g, cut) pc[gas == g & CCC >= cut, pair]
res <- list(); sel.out <- list()
add <- function(g, rule, e, pairs) {
  if (is.null(e) || !nrow(e)) { res[[length(res) + 1]] <<- data.table(gas = g, rule = rule, n_pairs = length(pairs), n_halfhours = 0); return() }
  ok <- is.finite(e$EC) & is.finite(e$FG)
  res[[length(res) + 1]] <<- data.table(gas = g, rule = rule, n_pairs = length(pairs), n_halfhours = nrow(e),
    months_ge48 = sum(table(format(as.POSIXct(e$t, origin = "1970-01-01", tz = "UTC"), "%Y-%m")) >= 48),
    mean_flux = mean(e$FG), median_flux = median(e$FG), mean_pairs = mean(e$npairs),
    CCC_EC = if (g != "CH4" && sum(ok) > 20) lccc(e$EC[ok], e$FG[ok]) else NA_real_,
    bias_EC = if (g != "CH4" && sum(ok) > 20) mean(e$FG[ok] - e$EC[ok]) else NA_real_,
    EC_on_windows = if (g != "CH4") mean(e$EC, na.rm = TRUE) else NA_real_)
  sel.out[[length(sel.out) + 1]] <<- data.table(gas = g, rule = rule, pairs = paste(sort(pairs), collapse = "; "))
}
for (g in c("CO2", "H2O", "CH4")) {
  if (g != "CH4") { p <- rshp(g); add(g, "EC", ens_from(g, p, "rmse"), p) }
  r <- s2(g);           add(g, "S2", r$ens, c("AA MBR", r$sel))
  r <- s2(g, TRUE);     add(g, "S2day", r$ens, c("AA MBR", r$sel))
  for (src in c("CO2", "H2O")) if (src != g) { p <- rshp(src); add(g, paste0("XG_", src), ens_from(g, p, "equal"), p) }
  if (g == "CH4") { p <- intersect(rshp("CO2"), rshp("H2O")); add(g, "XG_both", ens_from(g, p, "equal"), p) }
  p <- union(rshp("CO2"), rshp("H2O")); add(g, "XG_either", ens_from(g, p, "equal"), p)
  # corrected H2O pairs (CO2-RSHP reference); baseline on the same 30-min basis
  r <- xg_ref(g, rshp("CO2"), character());            add(g, "XG_CO2_30", r$ens, r$sel)
  r <- xg_ref(g, rshp("CO2"), rshp("H2O"), TRUE);      add(g, "XG_CO2+H2Oref", r$ens, r$sel)
  r <- xg_ref(g, rshp("CO2"), rshp("H2O"), FALSE);     add(g, "XG_CO2+H2Oref_noscale", r$ens, r$sel)
  p <- rshp_cut("H2O", 0.7); if (g != "H2O") add(g, "XG_H2O70", ens_from(g, p, "equal"), p)
  p <- union(rshp("CO2"), rshp_cut("H2O", 0.7)); add(g, "XG_CO2_or_H2O70", ens_from(g, p, "equal"), p)
  # S2 with the best reference: CO2-RSHP ensemble where the site has CO2 RSHPs (all other pairs screened
  # against it, CCC_ref >= 0.5, rescaled); AA MBR reference (standard S2) where it has none.
  allp <- unique(P30[gas == g, pair])
  rr <- xg_ref(g, rshp("CO2"), allp, TRUE)
  add(g, "S2_RSHPref", rr$ens, rr$sel)
  if (!is.null(rr$ens)) {
    rr_add <- rr$ens[!t %in% xg_ref(g, rshp("CO2"), character())$ens$t]
    add(g, "S2_RSHPref_addedonly", rr_add, "half-hours added by screened pairs")
    add(g, "S2_RSHPref+fallback", rr$ens, rr$sel)
  } else { r0 <- s2(g); add(g, "S2_RSHPref+fallback", r0$ens, c("AA MBR (fallback)", r0$sel)) }
  # stricter screens for the pairs added to the CO2-RSHP reference (AA MBR S2 fallback where no CO2 RSHP)
  for (cut in c(0.6, 0.7, 0.8)) {
    rc <- xg_ref(g, rshp("CO2"), allp, TRUE, cut)
    lab <- paste0("S2_RSHPref", cut * 100, "+fallback")
    if (!is.null(rc$ens)) add(g, lab, rc$ens, rc$sel) else { r0 <- s2(g); add(g, lab, r0$ens, c("AA MBR (fallback)", r0$sel)) }
  }
  # CO2-RSHP pairs combined with S2 (30-min basis): gap fill, filled windows only, and pooled pairs
  # gap-fill only: CO2-RSHP 30-min mean where it exists; screened, rescaled pairs only where it does not
  for (cut in c(0.5, 0.7)) {
    lab <- paste0("S2_RSHPref", cut * 100, "_gapfill+fallback")
    b0 <- xg_ref(g, rshp("CO2"), character())
    if (!is.null(b0$ens)) {
      rc <- xg_ref(g, rshp("CO2"), allp, TRUE, cut)
      fill <- rc$ens[!t %in% b0$ens$t]
      add(g, lab, rbind(b0$ens, fill), rc$sel)
      add(g, paste0("S2_RSHPref", cut * 100, "_gapfill_filledonly"), fill, "filled half-hours only")
    } else { r0 <- s2(g); add(g, lab, r0$ens, c("AA MBR (fallback)", r0$sel)) }
  }
  r2 <- s2(g); x30 <- xg_ref(g, rshp("CO2"), character())
  if (!is.null(r2$ens)) {
    xe <- if (is.null(x30$ens)) r2$ens[0] else x30$ens
    fillonly <- r2$ens[!t %in% xe$t]
    add(g, "XG_CO2_30+S2fill", rbind(xe, fillonly), c(x30$sel, "S2"))
    add(g, "S2fill_only", fillonly, "S2 (half-hours without CO2-RSHP)")
    pool <- unique(rbind(P30[gas == g & pair %in% rshp("CO2")], r2$rows, fill = TRUE), by = c("pair", "t30"))
    pe <- pool[, .(FG = mean(GF), EC = mean(EC, na.rm = TRUE), npairs = uniqueN(pair)), by = t30][, t := t30 * 1800][, t30 := NULL]
    add(g, "XG_CO2_30+S2pool", pe, unique(c(x30$sel, "AA MBR", r2$sel)))
    # S2 rescaled to the CO2-RSHP ensemble (RMA on shared half-hours, >= 30) before filling gaps
    ov <- merge(xe[, .(t, X = FG)], r2$ens[, .(t, S = FG)], by = "t")
    if (nrow(ov) >= 30 && sd(ov$S) > 0 && sd(ov$X) > 0) {
      bb <- sign(cor(ov$S, ov$X)) * sd(ov$S) / sd(ov$X); a0 <- mean(ov$S) - bb * mean(ov$X)
      fs <- copy(fillonly)[, FG := (FG - a0) / bb]
      add(g, "XG_CO2_30+S2fill_scaled", rbind(xe, fs), c(x30$sel, "S2 scaled"))
      add(g, "S2fill_scaled_only", fs, "S2 scaled (half-hours without CO2-RSHP)")
      res[[length(res)]][, `:=`(scale_b = bb, scale_a = a0, overlap = nrow(ov), r_overlap = cor(ov$S, ov$X))]
    }
  }
}
ec_all <- K[gas %in% c("CO2", "H2O") & is.finite(EC_mean), .(EC = mean(EC_mean)), by = .(gas, t30)][, .(EC_all = mean(EC)), by = gas]
R <- merge(rbindlist(res, fill = TRUE), ec_all, by = "gas", all.x = TRUE)[, Site := site]
fwrite(R, file.path(od, paste0(site, "_ch4method_stats.csv")))
fwrite(rbindlist(sel.out)[, Site := site], file.path(od, paste0(site, "_ch4method_pairs.csv")))
cat(site, "ch4method done\n")
