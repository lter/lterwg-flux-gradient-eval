suppressMessages({library(data.table)})
# Does the RMSE definition change the inverse-RMSE ensemble?
# ccc.parms() computes RMSE as the scatter about lm(GF ~ EC); Sect. 2.8 describes the root-mean-square
# GF - EC difference. This rebuilds the ensemble from the spike-evaluation extracts (filtered, then
# despiked as in despike_fluxes()) with both definitions and compares weights and agreement with EC.
if (!exists("localdir")) localdir <- "/Volumes/MaloneLab/Research/FluxGradient"
dir <- file.path(localdir, "Spike_Eval", "extract_2021_2026")
out.dir <- file.path(localdir, "NEON_GradientFlux_Ensemble")
ccc <- function(x, y) { ok <- is.finite(x) & is.finite(y); x <- x[ok]; y <- y[ok]; if (length(x) < 3) return(NA_real_)
  mx <- mean(x); my <- mean(y); n <- length(x); 2 * sum((x - mx) * (y - my)) / n / (sum((x - mx)^2)/n + sum((y - my)^2)/n + (mx - my)^2) }
wmean <- function(x, w) { k <- is.finite(x) & is.finite(w) & w > 0; if (!any(k)) return(mean(x[is.finite(x)])); sum(x[k] * w[k]) / sum(w[k]) }
sites <- unique(sub("_(AE|WP|MBR)\\.csv\\.gz$", "", list.files(dir)))
P <- list(); S <- list()
for (s in sites) {
  x <- rbindlist(lapply(c("MBR","AE","WP"), function(m) { f <- file.path(dir, paste0(s, "_", m, ".csv.gz")); if (!file.exists(f)) return(NULL)
    d <- fread(f, select = c("gas","dLevelsAminusB","FG_mean","FC_turb_interp","FH2O_interp","timeEndA.local")); d[, Approach := m]; d }), fill = TRUE)
  x <- x[gas %in% c("CO2","H2O")]
  x[, EC := fifelse(gas == "CO2", FC_turb_interp, FH2O_interp)]
  hr <- as.integer(substr(x$timeEndA.local, 12, 13)); mo <- as.integer(substr(x$timeEndA.local, 6, 7))
  x[, `:=`(dn = hr %in% 7:18, season = (mo %% 12) %/% 3)]
  # despike as despike_fluxes(): median +/- 6 MAD by approach x gas x pair x day/night x season (>= 20 values)
  x[, `:=`(med = median(FG_mean), mad_ = mad(FG_mean), n_ = .N), by = .(Approach, gas, dLevelsAminusB, dn, season)]
  x <- x[n_ < 20 | abs(FG_mean - med) <= 6 * pmax(mad_, 1e-6)]
  x <- x[is.finite(FG_mean) & is.finite(EC)]
  # pair statistics: CCC, RMSE as coded (scatter about lm(GF ~ EC)) and RMSE as described (GF - EC)
  p <- x[, { fit <- if (.N >= 3) lm(FG_mean ~ EC) else NULL
             .(n = .N, CCC = ccc(EC, FG_mean),
               RMSE_fit = if (is.null(fit)) NA_real_ else sqrt(mean(residuals(fit)^2)),
               RMSE_dir = sqrt(mean((FG_mean - EC)^2)),
               bias = mean(FG_mean - EC), slope = if (is.null(fit)) NA_real_ else coef(fit)[2]) },
         by = .(Approach, gas, dLevelsAminusB)][, Site := s]
  P[[s]] <- p
  r <- p[CCC >= 0.5]
  if (!nrow(r)) next
  y <- x[r, on = .(Approach, gas, dLevelsAminusB), nomatch = 0]
  y[, t30 := as.POSIXct(round(as.numeric(as.POSIXct(timeEndA.local, tz = "UTC")) / 1800) * 1800, origin = "1970-01-01", tz = "UTC")]
  e <- y[, .(EC = mean(EC), F_fit = wmean(FG_mean, 1 / RMSE_fit), F_dir = wmean(FG_mean, 1 / RMSE_dir), n = .N), by = .(gas, t30)]
  S[[s]] <- e[, .(Site = s, windows = .N, pairs = nrow(r[gas == .BY$gas]),
                  CCC_fit = ccc(EC, F_fit), CCC_dir = ccc(EC, F_dir),
                  R2_fit = cor(EC, F_fit)^2, R2_dir = cor(EC, F_dir)^2,
                  RMSE_fit = sqrt(mean((F_fit - EC)^2)), RMSE_dir = sqrt(mean((F_dir - EC)^2)),
                  med_abs_change = median(abs(F_dir - F_fit)), rel_change = median(abs(F_dir - F_fit)) / sd(F_fit),
                  share_changed_gt10pct = mean(abs(F_dir - F_fit) > 0.1 * abs(F_fit))), by = gas]
}
P <- rbindlist(P); S <- rbindlist(S)
fwrite(P, file.path(out.dir, "RMSE_definition_pair_stats_AA_AW_2021_2026.csv")); fwrite(S, file.path(out.dir, "RMSE_definition_ensemble_compare_AA_AW_2021_2026.csv"))
cat("Pairs with CCC >= 0.5:\n")
print(P[CCC >= 0.5, .(pairs = .N, med_ratio_dir_fit = round(median(RMSE_dir / RMSE_fit), 2),
                      share_dir_gt_1.25fit = round(mean(RMSE_dir > 1.25 * RMSE_fit), 2),
                      spearman_weights = round(cor(1/RMSE_fit, 1/RMSE_dir, method = "spearman"), 2)), by = gas])
cat("\nWithin-site weight rank agreement (Spearman, sites with >= 3 reliable pairs):\n")
print(P[CCC >= 0.5, if (.N >= 3) .(rho = cor(RMSE_fit, RMSE_dir, method = "spearman")), by = .(Site, gas)][, .(sites = .N, median_rho = round(median(rho), 2), min_rho = round(min(rho), 2)), by = gas])
cat("\nEnsemble vs EC (mean over sites):\n")
print(S[, .(sites = .N, CCC_fit = round(mean(CCC_fit), 3), CCC_dir = round(mean(CCC_dir), 3), R2_fit = round(mean(R2_fit), 3), R2_dir = round(mean(R2_dir), 3),
            RMSE_fit = round(mean(RMSE_fit), 3), RMSE_dir = round(mean(RMSE_dir), 3),
            sites_CCC_up = sum(CCC_dir > CCC_fit + 0.01), sites_CCC_down = sum(CCC_dir < CCC_fit - 0.01),
            max_abs_dCCC = round(max(abs(CCC_dir - CCC_fit)), 3), med_rel_change = round(median(rel_change, na.rm = TRUE), 3),
            share_windows_gt10pct = round(median(share_changed_gt10pct), 2)), by = gas])
cat("\nLargest site changes:\n"); print(S[order(-abs(CCC_dir - CCC_fit))][1:6, .(Site, gas, pairs, CCC_fit = round(CCC_fit,3), CCC_dir = round(CCC_dir,3), RMSE_fit = round(RMSE_fit,2), RMSE_dir = round(RMSE_dir,2))])
