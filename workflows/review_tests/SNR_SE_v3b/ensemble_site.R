# ensemble_site.R  SITE  EDI_DIR  OUT_DIR  CODE_DIR
# Runs one site's 9-min gradient fluxes through filter -> despike -> pair CCC -> 30-min ensemble under
# the previous SD-based SNR rule (3) and the SE-based rule (MBR 30, AE/WP 3), and compares the results.
#   CO2/H2O: RSHP ensemble as in flow.flux.ensemble._V1R.R / calc.ensemble.site(): pairs with CCC >= 0.5
#            against EC, inverse-RMSE-weighted mean of their 9-min GF per half-hour (sampling midpoint).
#   All gases: no-EC rule (Supplement S2, flow.evaluation.noEC_pair_selection.R): reference = mean of the
#            above-canopy (AA) MBR pairs per 30 min; AE/WP pairs with CCC_ref >= 0.5 (>= 30 shared
#            windows) are rescaled to the reference (reduced major axis) and averaged with it.
# Writes OUT_DIR/ensemble/<SITE>/: <SITE>_ENSEMBLE_30min_{SD,SE}.csv, <SITE>_ensemble_stats.csv,
#   <SITE>_noEC_pairs.csv
suppressPackageStartupMessages({ library(data.table); library(dplyr) })
a <- commandArgs(TRUE); site <- a[1]; edi <- a[2]; out <- a[3]; code <- a[4]
source(file.path(code, "calc.filter_FG.R")); source(file.path(code, "calc.ensemble.R"))
od <- file.path(out, "ensemble", site); dir.create(od, recursive = TRUE, showWarnings = FALSE)

canopy <- fread(file.path(code, "canopy_L1.csv"))[Site == site & Canopy_L1 != "WW"]
pairs.ok <- unique(canopy$dLevelsAminusB); lvl <- setNames(canopy$Canopy_L1, canopy$dLevelsAminusB)
cols <- c("timeEnd_A", "timeBgn_B", "timeEnd_B", "timeEndA.local", "gas", "dLevelsAminusB", "TowerPosition_A",
          "TowerPosition_B", "dConc", "dConc_sd", "vari_A", "vari_B", "numSamp_A", "numSamp_B", "dConc.tracer",
          "dConc.tracer_sd", "tracer", "ustar_interp", "ustar_threshold", "FG_mean", "FC_turb_interp", "FH2O_interp")
read_app <- function(app) {
  f <- file.path(edi, site, paste0(site, "_", app, "_9min.df.final.csv"))
  if (!file.exists(f)) return(NULL)
  hdr <- names(fread(f, nrows = 1))
  x <- fread(f, select = intersect(cols, hdr), showProgress = FALSE)
  for (cc in c("timeEnd_A", "timeBgn_B", "timeEnd_B", "timeEndA.local")) if (cc %in% names(x)) x[[cc]] <- as.character(x[[cc]])
  x <- x[dLevelsAminusB %in% pairs.ok]
  x[, timeEndA.local := as.POSIXct(substr(timeEndA.local, 1, 19), tz = "UTC", format = "%Y-%m-%d %H:%M:%S")]
  x[, `:=`(EC_mean = fifelse(gas == "CO2", FC_turb_interp, fifelse(gas == "H2O", FH2O_interp, NA_real_)),
           Canopy_L1 = lvl[dLevelsAminusB], Approach = app, Site = site)]
  as.data.frame(x)
}
D <- list(MBR = read_app("MBR"), AE = read_app("AE"), WP = read_app("WP")); D <- D[!sapply(D, is.null)]
D <- lapply(D, add_snr_se)
lccc <- function(x, y) { mx <- mean(x); my <- mean(y)
  2 * mean((x - mx) * (y - my)) / (mean((x - mx)^2) + mean((y - my)^2) + (mx - my)^2) }
# all half-hours of the record (for coverage): span of the u*-passed windows of each gas
span <- rbindlist(lapply(D, function(x) as.data.table(x)[!is.na(timeEndA.local), .(t0 = min(timeEndA.local), t1 = max(timeEndA.local)), by = gas]))
span <- span[, .(n_halfhours = as.numeric(difftime(max(t1), min(t0), units = "mins")) / 30), by = gas]

FILT <- list(SD = list(type = "SD", min = 3), SE = list(type = "SE", min = c(MBR = 30, AE = 3, WP = 3)))
stats <- list(); noec.out <- list()
for (fn in names(FILT)) {
  K <- rbindlist(lapply(names(D), function(app) {
    k <- filter_fluxes(D[[app]], dConcSNR.min = FILT[[fn]]$min, approach = app, snr.type = FILT[[fn]]$type)
    k <- k[is.finite(k$FG_mean), ]
    as.data.table(despike_fluxes(k, k = 6))[, .(Site, Approach, gas, dLevelsAminusB, Canopy_L1, timeEnd_A, timeBgn_B,
                                              timeEndA.local, FG_mean, EC_mean)]
  }), fill = TRUE)
  # pair CCC / RMSE against EC (CO2, H2O)
  pc <- K[gas %in% c("CO2", "H2O") & is.finite(EC_mean), .(n = .N, CCC = if (.N >= 3) lccc(EC_mean, FG_mean) else NA_real_,
        RMSE = sqrt(mean((FG_mean - EC_mean)^2))), by = .(Approach, gas, dLevelsAminusB)]
  KK <- merge(K, pc, by = c("Approach", "gas", "dLevelsAminusB"), all.x = TRUE)
  # 1. RSHP ensemble (CO2, H2O), repository function
  ens <- calc.ensemble.site(as.data.frame(KK[gas %in% c("CO2", "H2O") & is.finite(CCC)]))
  ens <- as.data.table(ens)[, method := "RSHP"]
  # 2. no-EC ensemble (all gases), Supplement S2 rule on 30-min means
  K[, t30 := floor(as.numeric(timeEndA.local) / 1800)]
  P30 <- K[, .(GF = mean(FG_mean), EC = mean(EC_mean)), by = .(gas, Approach, dLevelsAminusB, Canopy_L1, t30)]
  noec <- list()
  for (g in c("CO2", "H2O", "CH4")) {
    aa <- P30[gas == g & Approach == "MBR" & Canopy_L1 == "AA"]
    if (!nrow(aa)) next
    REF <- aa[, .(REF = mean(GF)), by = t30]
    sel <- list()
    for (pp in unique(P30[gas == g & Approach != "MBR", paste(Approach, dLevelsAminusB)])) {
      G <- merge(P30[gas == g & paste(Approach, dLevelsAminusB) == pp], REF, by = "t30")
      cr <- if (nrow(G) >= 30) lccc(G$GF, G$REF) else NA_real_
      b <- if (nrow(G) >= 3) sign(cor(G$GF, G$REF)) * sd(G$GF) / sd(G$REF) else NA_real_
      a0 <- if (nrow(G) >= 3) mean(G$GF) - b * mean(G$REF) else NA_real_
      noec.out[[length(noec.out) + 1]] <- data.table(Site = site, filter = fn, gas = g, pair = pp, n_ref = nrow(G), CCC_ref = cr, scale_b = b)
      if (is.finite(cr) && cr >= 0.5 && is.finite(b) && b != 0)
        sel[[pp]] <- P30[gas == g & paste(Approach, dLevelsAminusB) == pp][, GF := (GF - a0) / b]
    }
    allp <- rbindlist(c(list(aa), sel), fill = TRUE)
    noec[[g]] <- allp[, .(FG_ENSEMBLE = mean(GF), EC_mean = mean(EC, na.rm = TRUE), FG_ENSEMBLE_NPAIRS = .N), by = t30][
      , `:=`(gas = g, n_AEWP_selected = length(sel))]
  }
  noec <- rbindlist(noec, fill = TRUE)[, method := "noEC"]
  if (nrow(noec)) noec[, time.rounded := as.POSIXct(t30 * 1800, origin = "1970-01-01", tz = "UTC")]
  fwrite(rbind(ens[, .(method, gas, time.rounded, FG_ENSEMBLE, FG_ENSEMBLE_SD, FG_ENSEMBLE_NPAIRS, EC_mean)],
               if (nrow(noec)) noec[, .(method, gas, time.rounded, FG_ENSEMBLE, FG_ENSEMBLE_SD = NA_real_, FG_ENSEMBLE_NPAIRS, EC_mean)], fill = TRUE),
         file.path(od, paste0(site, "_ENSEMBLE_30min_", fn, ".csv")))
  # 3. statistics per method x gas
  ecall <- K[gas %in% c("CO2", "H2O") & is.finite(EC_mean), .(t30, EC_mean, gas)][, .(EC = mean(EC_mean)), by = .(gas, t30)][
    , .(EC_mean_all = mean(EC)), by = gas]
  one <- function(E, m) {
    if (!nrow(E)) return(NULL)
    E <- as.data.table(E)
    E[, .(n_halfhours = .N, months = uniqueN(format(time.rounded, "%Y-%m")),
          months_ge48 = sum(table(format(time.rounded, "%Y-%m")) >= 48),
          mean_flux = mean(FG_ENSEMBLE), median_flux = median(FG_ENSEMBLE),
          mean_abs_flux = mean(abs(FG_ENSEMBLE)), frac_negative = mean(FG_ENSEMBLE < 0),
          mean_pairs = mean(FG_ENSEMBLE_NPAIRS),
          CCC_EC = if (.BY$gas %in% c("CO2", "H2O") && sum(is.finite(EC_mean)) > 20)
            lccc(EC_mean[is.finite(EC_mean)], FG_ENSEMBLE[is.finite(EC_mean)]) else NA_real_,
          bias_EC = if (sum(is.finite(EC_mean)) > 20) mean((FG_ENSEMBLE - EC_mean)[is.finite(EC_mean)]) else NA_real_,
          EC_mean_on_windows = mean(EC_mean, na.rm = TRUE),
          n_rshp = if (m == "RSHP") NA_real_ else NA_real_), by = gas][, method := m]
  }
  st <- rbind(one(ens, "RSHP"), one(noec, "noEC"), fill = TRUE)
  rs <- pc[CCC >= 0.5, .(n_rshp = .N), by = gas]
  st <- merge(st[, n_rshp := NULL], rs, by = "gas", all.x = TRUE)
  st <- merge(st, ecall, by = "gas", all.x = TRUE)
  st <- merge(st, span, by = "gas", all.x = TRUE, suffixes = c("", "_record"))
  if (nrow(noec)) st <- merge(st, unique(noec[, .(gas, n_AEWP_selected)]), by = "gas", all.x = TRUE)
  stats[[fn]] <- st[, `:=`(Site = site, filter = fn)]
}
fwrite(rbindlist(stats, fill = TRUE), file.path(od, paste0(site, "_ensemble_stats.csv")))
fwrite(rbindlist(noec.out, fill = TRUE), file.path(od, paste0(site, "_noEC_pairs.csv")))
cat(site, "ensemble done\n")
