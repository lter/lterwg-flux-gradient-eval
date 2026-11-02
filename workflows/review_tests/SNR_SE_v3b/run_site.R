# run_site.R  SITE  EDI_DIR  OUT_DIR  CODE_DIR
# Applies the SE-based SNR filter (MBR 30, AE/WP 3) and the previous SD rule (3) to one site's
# 9-min gradient fluxes, despikes (k = 6), and computes pair CCC against EC for CO2/H2O.
# Writes OUT_DIR/sites/<SITE>/: <SITE>_FILTER_SE.Rdata (SE-filtered, despiked MBR/AE/WP, all gases),
# <SITE>_pairs_CCC.csv, <SITE>_retention.csv, <SITE>_selection.csv
suppressPackageStartupMessages({ library(data.table); library(dplyr) })
a <- commandArgs(TRUE); site <- a[1]; edi <- a[2]; out <- a[3]; code <- a[4]
source(file.path(code, "calc.filter_FG.R"))
od <- file.path(out, "sites", site); dir.create(od, recursive = TRUE, showWarnings = FALSE)

canopy <- fread(file.path(code, "canopy_L1.csv"))[Site == site & Canopy_L1 != "WW"]
pairs.ok <- unique(canopy$dLevelsAminusB)
if (!length(pairs.ok)) stop("No AA/AW pairs for ", site)
lvl <- setNames(canopy$Canopy_L1, canopy$dLevelsAminusB)

cols <- c("timeEnd_A", "timeEnd_B", "timeEndA.local", "gas", "dLevelsAminusB", "TowerPosition_A", "TowerPosition_B",
          "dConc", "dConc_sd", "vari_A", "vari_B", "numSamp_A", "numSamp_B", "dConc.tracer", "dConc.tracer_sd",
          "tracer", "ustar_interp", "ustar_threshold", "FG_mean", "FG_sd", "FC_turb_interp", "FH2O_interp",
          "cross_grad_flag", "MeasurementHeight_m_A", "MeasurementHeight_m_B")
read_app <- function(app) {
  f <- file.path(edi, site, paste0(site, "_", app, "_9min.df.final.csv"))
  if (!file.exists(f)) { message("missing ", f); return(NULL) }
  hdr <- names(fread(f, nrows = 1))
  x <- fread(f, select = intersect(cols, hdr), showProgress = FALSE)
  for (cc in c("timeEnd_A", "timeEnd_B", "timeEndA.local")) if (cc %in% names(x)) x[[cc]] <- as.character(x[[cc]])
  x[, timeEndA.local := as.POSIXct(substr(timeEndA.local, 1, 19), tz = "UTC", format = "%Y-%m-%d %H:%M:%S")]
  x <- x[dLevelsAminusB %in% pairs.ok]
  x[, EC_mean := fifelse(gas == "CO2", FC_turb_interp, fifelse(gas == "H2O", FH2O_interp, NA_real_))]
  x[, Canopy_L1 := lvl[dLevelsAminusB]]
  as.data.frame(x)
}
D <- list(MBR = read_app("MBR"), AE = read_app("AE"), WP = read_app("WP"))
D <- D[!sapply(D, is.null)]
D <- lapply(D, add_snr_se)                                  # full frames: MBR tracer SE from tracer rows
tracer.exact <- if ("MBR" %in% names(D)) attr(D$MBR, "tracer_se_exact") else NA

lccc <- function(x, y) { mx <- mean(x); my <- mean(y)
  2 * mean((x - mx) * (y - my)) / (mean((x - mx)^2) + mean((y - my)^2) + (mx - my)^2) }
FILT <- list(SE = list(type = "SE", min = c(MBR = 30, AE = 3, WP = 3)), SD = list(type = "SD", min = 3))
pairs.out <- ret.out <- sel.out <- list()
for (fn in names(FILT)) {
  K <- list()
  for (app in names(D)) {
    x <- D[[app]]
    base <- x[x$TowerPosition_A != x$TowerPosition_B & !is.na(x$ustar_interp) & x$ustar_interp >= x$ustar_threshold &
                is.finite(x$FG_mean), ]
    k <- filter_fluxes(x, dConcSNR.min = FILT[[fn]]$min, approach = app, snr.type = FILT[[fn]]$type)
    k <- k[is.finite(k$FG_mean), ]
    k <- despike_fluxes(k, k = 6)
    K[[app]] <- k
    for (g in c("CO2", "H2O", "CH4"))
      ret.out[[length(ret.out) + 1]] <- data.frame(Site = site, filter = fn, Approach = app, gas = g,
                                                   n_base = sum(base$gas == g), n_kept = sum(k$gas == g))
    for (g in c("CO2", "H2O")) {
      b <- base[base$gas == g & is.finite(base$EC_mean), ]; kk <- k[k$gas == g & is.finite(k$EC_mean), ]
      if (nrow(b) < 100 || nrow(kk) < 30) next
      cut30 <- quantile(abs(b$EC_mean), 0.3)
      sel.out[[length(sel.out) + 1]] <- data.frame(Site = site, filter = fn, Approach = app, gas = g,
        EC_all = mean(b$EC_mean), absEC_all = mean(abs(b$EC_mean)), EC_kept = mean(kk$EC_mean), FG_kept = mean(kk$FG_mean),
        share_near0 = mean(abs(kk$EC_mean) <= cut30), n_kept = nrow(kk))
      for (p in unique(kk$dLevelsAminusB)) {
        q <- kk[kk$dLevelsAminusB == p, ]
        pairs.out[[length(pairs.out) + 1]] <- data.frame(Site = site, filter = fn, Approach = app, gas = g,
          dLevelsAminusB = p, Canopy_L1 = lvl[[p]], n = nrow(q),
          CCC = if (nrow(q) >= 3) lccc(q$EC_mean, q$FG_mean) else NA_real_,
          RMSE = sqrt(mean((q$FG_mean - q$EC_mean)^2)), bias = mean(q$FG_mean - q$EC_mean))
      }
    }
  }
  if (fn == "SE") {
    MBR_9min_FILTER <- K$MBR; AE_9min_FILTER <- K$AE; WP_9min_FILTER <- K$WP
    save(MBR_9min_FILTER, AE_9min_FILTER, WP_9min_FILTER, file = file.path(od, paste0(site, "_FILTER_SE.Rdata")))
  }
}
fwrite(rbindlist(pairs.out), file.path(od, paste0(site, "_pairs_CCC.csv")))
fwrite(rbindlist(ret.out), file.path(od, paste0(site, "_retention.csv")))
fwrite(rbindlist(sel.out), file.path(od, paste0(site, "_selection.csv")))
cat(site, "done; MBR tracer SE exact for", round(100 * tracer.exact), "% of rows\n")
