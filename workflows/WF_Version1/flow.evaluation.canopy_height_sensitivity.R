suppressMessages({library(data.table)})
# Sensitivity of AE/WP concordance to the aerodynamic canopy height (h).
# calc.aerodynamic.canopy.height() uses the mean height of the top inlet pair as z (u is measured at the
# top inlet) and d = 0.6 h, while the fluxes use d = 0.66 h. Here h is recomputed with z = top inlet and
# d = 0.66 h (h_new = z_top / (z_mean / h + 0.06)), the AE and WP fluxes are rescaled by K_new / K_old
# (flux is linear in K), despiked as in despike_fluxes(), and CCC against EC is compared with the original.
# Inputs: the spike-evaluation extracts (flow.evaluation.spike_eval.R). Outputs: Spike_Eval/canopy_height_sensitivity_*.csv
if (!exists("localdir")) localdir <- "/Volumes/MaloneLab/Research/FluxGradient"
dir <- file.path(localdir, "Spike_Eval", "extract_2021_2026")
ccc <- function(x, y) { ok <- is.finite(x) & is.finite(y); x <- x[ok]; y <- y[ok]; if (length(x) < 30) return(NA_real_)
  mx <- mean(x); my <- mean(y); 2 * mean((x - mx) * (y - my)) / (var(x) * (length(x)-1)/length(x) + var(y) * (length(y)-1)/length(y) + (mx - my)^2) }
phi <- function(z) ifelse(z > 0, 1 + 5 * z, (1 - 16 * z)^-0.5)
despike <- function(x, col) {
  x[, med := median(get(col), na.rm = TRUE), by = .(gas, dLevelsAminusB, dn, season)]
  x[, mad_ := mad(get(col), na.rm = TRUE), by = .(gas, dLevelsAminusB, dn, season)]
  x[, n_ := sum(is.finite(get(col))), by = .(gas, dLevelsAminusB, dn, season)]
  keep <- !is.finite(x[[col]]) | x$n_ < 20 | abs(x[[col]] - x$med) <= 6 * pmax(x$mad_, 1e-6)
  out <- x[[col]]; out[!keep] <- NA; x[, c("med","mad_","n_") := NULL]; out }
res <- list(); hs <- list()
for (f in list.files(dir, "_(AE|WP)\\.csv\\.gz$", full.names = TRUE)) {
  site <- sub("_(AE|WP).*", "", basename(f)); m <- sub(".*_(AE|WP)\\.csv\\.gz", "\\1", basename(f))
  x <- fread(f, select = c("gas","dLevelsAminusB","TowerPosition_A","TowerPosition_B","TowerHeight_A","TowerHeight_B","FG_mean",
                           "FC_turb_interp","FH2O_interp","z_veg_aero","effective_h","roughLength_calc","phih","L_obukhov","timeEndA.local"))
  if (!nrow(x)) next
  pos <- unique(rbind(x[, .(p = TowerPosition_A, z = TowerHeight_A)], x[, .(p = TowerPosition_B, z = TowerHeight_B)]))[, .(z = mean(z)), by = p][order(-p)]
  if (nrow(pos) < 2) next
  z.top <- pos$z[1]; z.mean <- mean(pos$z[1:2])
  x[, h_new := z.top / (z.mean / z_veg_aero + 0.06)]          # Pennypacker h with z of the top inlet and d = 0.66 h
  hs[[paste(site, m)]] <- data.table(Site = site, h_old = median(x$z_veg_aero), h_new = median(x$h_new), z_top = z.top, z_mean = z.mean)
  zg <- sqrt(x$TowerHeight_A * x$TowerHeight_B)
  eff.new <- pmax(zg - 0.66 * x$h_new, 0.1); z0.new <- 0.1 * x$h_new; phi.new <- phi(eff.new / x$L_obukhov)
  if (m == "AE") r <- (eff.new / phi.new) / (x$effective_h / x$phih) else {
    r <- (eff.new / (log(eff.new / z0.new) * phi.new)) / (x$effective_h / (log(x$effective_h / x$roughLength_calc) * x$phih))
    r[eff.new <= z0.new] <- NA }
  x[, FG_new := FG_mean * r]
  hr <- as.integer(substr(x$timeEndA.local, 12, 13)); mo <- as.integer(substr(x$timeEndA.local, 6, 7))
  x[, `:=`(dn = hr %in% 7:18, season = (mo %% 12) %/% 3, EC = fifelse(gas == "CO2", FC_turb_interp, FH2O_interp))]
  x[, `:=`(FG_old_d = despike(x, "FG_mean"), FG_new_d = despike(x, "FG_new"))]
  res[[paste(site, m)]] <- x[gas %in% c("CO2","H2O"), .(n = .N, CCC_old = ccc(FG_old_d, EC), CCC_new = ccc(FG_new_d, EC),
                                     med_ratio = median(FG_new / FG_mean, na.rm = TRUE)), by = .(gas, dLevelsAminusB)][, `:=`(Site = site, Approach = m)]
}
R <- rbindlist(res); H <- unique(rbindlist(hs), by = "Site")
H[, change_pct := round(100 * (h_new / h_old - 1))]
cat("Canopy height change (median over record), %:\n"); print(summary(H$change_pct)); print(H[order(-abs(change_pct))][1:8])
fwrite(H, file.path(localdir, "Spike_Eval", "canopy_height_sensitivity_by_site.csv")); fwrite(R, file.path(localdir, "Spike_Eval", "canopy_height_sensitivity_CCC.csv"))
s <- R[is.finite(CCC_old) & is.finite(CCC_new), .(pairs = .N, RSHP_old = sum(CCC_old >= 0.5), RSHP_new = sum(CCC_new >= 0.5),
       mean_CCC_old = round(mean(CCC_old), 3), mean_CCC_new = round(mean(CCC_new), 3),
       gained = sum(CCC_old < 0.5 & CCC_new >= 0.5), lost = sum(CCC_old >= 0.5 & CCC_new < 0.5),
       med_abs_dCCC = round(median(abs(CCC_new - CCC_old)), 3), med_flux_ratio = round(median(med_ratio, na.rm=TRUE), 2)), by = .(Approach, gas)]
print(s)
