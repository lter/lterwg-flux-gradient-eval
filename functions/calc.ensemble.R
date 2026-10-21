# calc.ensemble.R --------------------------------------------------------------
# 30-min gradient-flux (GF) ensemble from reliable sampling-height pairs (RSHP), with the
# spread between pairs published alongside each value.
#
# calc.ensemble.site(df, ccc.cut = 0.5)
#   df : one site's element of SITE_DATA_FILTERED_CCC (9-min rows; needs timeEndA.local, gas,
#        Approach, dLevelsAminusB, Canopy_L1, FG_mean, EC_mean, CCC, RMSE)
#   Returns one row per gas x 30-min window (time.rounded = timeEndA.local rounded to the nearest
#   30 min, local time) with:
#     FG_ENSEMBLE           inverse-RMSE weighted mean of all RSHP 9-min GF in the window
#                           (unchanged from the published ensemble; weights 1/RMSE of each pair vs EC)
#     FG_ENSEMBLE_SD        standard deviation between pairs: SD of the per-pair 30-min mean GF
#                           (NA when only one pair contributes)
#     FG_ENSEMBLE_NPAIRS    number of RSHP pairs contributing to the window
#     FG_ENSEMBLE_NVALUES   number of 9-min GF values contributing
#     FG_ENSEMBLE_APPROACHES approaches contributing, e.g. "AE;MBR"
#     EC_mean               mean eddy-covariance flux of the contributing rows (evaluation only)
# Notes on FG_ENSEMBLE_SD: it ranks uncertainty well (absolute error vs EC rises ~3.5x (CO2) and ~8x
# (H2O) from the lowest to the highest spread quartile) but is not a calibrated interval: EC lies
# within +/- 1 SD in only 42-48% and within +/- 2 SD in 66-70% of windows (2021-2026 run). Use it as a
# relative uncertainty flag, not as a confidence interval.
# -----------------------------------------------------------------------------

calc.ensemble.site <- function(df, ccc.cut = 0.5) {
  wmean <- function(x, w) {
    keep <- is.finite(x); x <- x[keep]; w <- w[keep]
    if (!length(x)) return(NA_real_)
    w[!is.finite(w) | w < 0] <- 0
    if (sum(w) <= 0) return(mean(x))
    sum(x * w) / sum(w)
  }
  base <- df %>%
    dplyr::distinct() %>%
    dplyr::filter(CCC >= ccc.cut, Canopy_L1 != "WW") %>%
    dplyr::mutate(weight.rmse = dplyr::if_else(!is.na(RMSE) & is.finite(RMSE) & RMSE > 0, 1 / RMSE, 0),
                  time.rounded = lubridate::round_date(timeEndA.local, unit = "30 minutes"))
  ens <- base %>%
    dplyr::reframe(.by = c(gas, time.rounded),
                   EC_mean = mean(EC_mean, na.rm = TRUE),
                   FG_ENSEMBLE = wmean(FG_mean, weight.rmse),
                   FG_ENSEMBLE_NVALUES = sum(is.finite(FG_mean)))
  spread <- base %>%
    dplyr::filter(is.finite(FG_mean)) %>%
    dplyr::reframe(.by = c(gas, time.rounded, Approach, dLevelsAminusB), fg = mean(FG_mean)) %>%
    dplyr::reframe(.by = c(gas, time.rounded),
                   FG_ENSEMBLE_SD = if (dplyr::n() > 1) stats::sd(fg) else NA_real_,
                   FG_ENSEMBLE_NPAIRS = dplyr::n(),
                   FG_ENSEMBLE_APPROACHES = paste(sort(unique(as.character(Approach))), collapse = ";"))
  dplyr::left_join(ens, spread, by = c("gas", "time.rounded")) %>%
    dplyr::mutate(FG_ENSEMBLE_NPAIRS = dplyr::coalesce(FG_ENSEMBLE_NPAIRS, 0L))
}

# Column descriptions for the EDI data table written by write.ensemble.edi()
ensemble.edi.metadata <- function() {
  data.frame(
    file_name_pattern = "{SITE}_GF_ENSEMBLE_30min.csv",
    column_name = c("site", "gas", "time_end_local", "time_end_utc", "FG_ENSEMBLE", "FG_ENSEMBLE_SD", "FG_ENSEMBLE_NPAIRS",
                    "FG_ENSEMBLE_NVALUES", "FG_ENSEMBLE_APPROACHES", "EC_mean", "ensemble_method"),
    data_type = c("character", "character", "datetime", "datetime", "numeric", "numeric", "integer", "integer", "character", "numeric", "character"),
    units = c("unitless", "unitless", "YYYY-MM-DD hh:mm:ss (site local time, including daylight saving time)", "YYYY-MM-DD hh:mm:ss (UTC)",
              "umol CO2 m-2 s-1 (CO2); mmol H2O m-2 s-1 (H2O)", "same as FG_ENSEMBLE", "count", "count", "unitless",
              "umol CO2 m-2 s-1 (CO2); mmol H2O m-2 s-1 (H2O)", "unitless"),
    description = c(
      "NEON site code.",
      "Gas of the flux (CO2 or H2O).",
      "End of the 9-min gradient-flux windows (timeEndA.local) rounded to the nearest 30 min, in site local time. Repeats once a year at the end of daylight saving time; use time_end_utc as the unique key.",
      "time_end_local expressed in UTC (unique).",
      "Gradient-flux ensemble: inverse-RMSE weighted mean of all 9-min gradient fluxes in the window from reliable sampling-height pairs (CCC >= 0.5 against EC; any approach: MBR, AE, WP).",
      "Spread between sampling-height pairs: standard deviation of the per-pair 30-min mean gradient flux. NA when only one pair contributes. Ranks uncertainty (larger spread = larger expected error) but is not a calibrated confidence interval; EC lies within +/- 1 SD in 42-48% and within +/- 2 SD in 66-70% of windows (2021-2026).",
      "Number of reliable sampling-height pairs contributing to the window.",
      "Number of 9-min gradient-flux values contributing to the window.",
      "Gradient-flux approaches contributing to the window, separated by ';'.",
      "Mean eddy-covariance turbulent flux (FC_turb_interp or FH2O_interp) of the contributing rows; for evaluation only.",
      "Weighting used for FG_ENSEMBLE."),
    stringsAsFactors = FALSE)
}

# Write one CSV per site plus the metadata table into edi.dir
write.ensemble.edi <- function(SITE_DATA_ENSEMBLE, edi.dir, method = "inverse_rmse_weighted") {
  dir.create(edi.dir, showWarnings = FALSE, recursive = TRUE)
  for (site in names(SITE_DATA_ENSEMBLE)) {
    d <- SITE_DATA_ENSEMBLE[[site]]
    if (is.null(d) || !nrow(d)) next
    out <- data.frame(site = site, gas = d$gas,
                      time_end_local = format(d$time.rounded, "%Y-%m-%d %H:%M:%S"),
                      time_end_utc = format(d$time.rounded, "%Y-%m-%d %H:%M:%S", tz = "UTC"),
                      FG_ENSEMBLE = d$FG_ENSEMBLE, FG_ENSEMBLE_SD = d$FG_ENSEMBLE_SD,
                      FG_ENSEMBLE_NPAIRS = d$FG_ENSEMBLE_NPAIRS, FG_ENSEMBLE_NVALUES = d$FG_ENSEMBLE_NVALUES,
                      FG_ENSEMBLE_APPROACHES = d$FG_ENSEMBLE_APPROACHES, EC_mean = d$EC_mean,
                      ensemble_method = method)
    out <- out[is.finite(out$FG_ENSEMBLE), ]
    out <- out[order(out$gas, out$time_end_utc), ]
    dir.create(file.path(edi.dir, site), showWarnings = FALSE)
    utils::write.csv(out, file.path(edi.dir, site, paste0(site, "_GF_ENSEMBLE_30min.csv")), row.names = FALSE, na = "NA")
  }
  utils::write.csv(ensemble.edi.metadata(), file.path(edi.dir, "Metadata_table_GF_ENSEMBLE.csv"), row.names = FALSE)
  invisible(TRUE)
}
