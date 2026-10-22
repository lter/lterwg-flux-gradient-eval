# calc.ensemble.R --------------------------------------------------------------
# 30-min gradient-flux (GF) ensemble from reliable sampling-height pairs (RSHP), with the
# spread between pairs published alongside each value.
#
# Half-hour assignment (ensemble.halfhour): each 9-min GF value is placed in the fixed half-hour
# interval [hh:00, hh:30) or [hh:30, hh+1:00) that contains the midpoint between the two inlet samples
# (timeEnd_A + (timeBgn_B - timeEnd_A) / 2). This is the time to which the 30-min EC fluxes were
# interpolated for the GF calculation, so GF and EC refer to the same half-hour. (Previously the end of
# the first sample, timeEndA.local, was rounded to the nearest 30 min.) Intervals are built in UTC and
# expressed in site local time for the time.rounded column, which is the START of the interval.
#
# calc.ensemble.site(df, ccc.cut = 0.5)
#   df : one site's element of SITE_DATA_FILTERED_CCC (9-min rows; needs timeEnd_A, timeBgn_B,
#        timeEndA.local, gas, Approach, dLevelsAminusB, Canopy_L1, FG_mean, EC_mean, CCC, RMSE)
#   Returns one row per gas x half-hour that has at least one reliable GF, with:
#     time.rounded          start of the half-hour, site local time (POSIXct with the site tzone)
#     time_start_utc, time_end_utc   interval start and end (UTC)
#     FG_ENSEMBLE           inverse-RMSE weighted mean of all RSHP 9-min GF in the half-hour
#     FG_ENSEMBLE_SD        standard deviation between pairs: SD of the per-pair 30-min mean GF
#                           (pair = approach x sampling-height pair; NA when only one contributes)
#     FG_ENSEMBLE_NPAIRS    number of RSHP pairs contributing
#     FG_ENSEMBLE_NVALUES   number of 9-min GF values contributing
#     FG_ENSEMBLE_APPROACHES approaches contributing, e.g. "AE;MBR"
#     EC_mean               mean of the EC flux interpolated to the contributing GF midpoints (evaluation only)
#
# write.ensemble.edi(SITE_DATA_ENSEMBLE, edi.dir, method, complete = TRUE, ec30 = NULL)
#   Writes {SITE}_GF_ENSEMBLE_30min.csv. With complete = TRUE every half-hour from the first to the last
#   ensemble value of the site is written; half-hours without a reliable GF have FG_ENSEMBLE = NA,
#   FG_ENSEMBLE_NPAIRS = 0 and FG_ENSEMBLE_FLAG = "no_reliable_GF". No gap filling is done.
#   ec30 (optional): data frame with Site, gas, time_start_utc and EC_30min (NEON 30-min turbulent flux,
#   e.g. from load.ec30.neon()); written as EC_30min alongside EC_mean.
#
# Notes on FG_ENSEMBLE_SD: it ranks uncertainty well but is not a calibrated interval: EC lies within
# +/- 1 SD in only 42-48% and within +/- 2 SD in 66-70% of windows (2021-2026 run, before the
# midpoint assignment). Use it as a relative uncertainty flag, not as a confidence interval.
# -----------------------------------------------------------------------------

# Start of the half-hour containing the midpoint between the two inlet samples, in site local time.
ensemble.halfhour <- function(timeEnd_A, timeBgn_B, local = NULL) {
  tA <- as.POSIXct(timeEnd_A, tz = "UTC")
  tB <- as.POSIXct(timeBgn_B, tz = "UTC")
  mid <- as.numeric(tA) + (as.numeric(tB) - as.numeric(tA)) / 2
  bad <- !is.finite(mid)
  if (any(bad) && !is.null(local)) mid[bad] <- as.numeric(as.POSIXct(local))[bad]   # fallback: end of first sample
  start <- as.POSIXct(floor(mid / 1800) * 1800, origin = "1970-01-01", tz = "UTC")
  tz <- if (!is.null(local)) attr(as.POSIXct(local), "tzone") else NULL
  if (is.null(tz) || !nzchar(tz[1])) tz <- "UTC"
  attr(start, "tzone") <- tz[1]
  start
}

calc.ensemble.site <- function(df, ccc.cut = 0.5) {
  wmean <- function(x, w) {
    keep <- is.finite(x); x <- x[keep]; w <- w[keep]
    if (!length(x)) return(NA_real_)
    w[!is.finite(w) | w < 0] <- 0
    if (sum(w) <= 0) return(mean(x))
    sum(x * w) / sum(w)
  }
  for (cc in c("timeEnd_A", "timeBgn_B")) if (!cc %in% names(df)) {
    warning(cc, " missing: half-hours assigned from timeEndA.local"); df[[cc]] <- NA }
  base <- df %>%
    dplyr::distinct() %>%
    dplyr::filter(CCC >= ccc.cut, Canopy_L1 != "WW") %>%
    dplyr::mutate(weight.rmse = dplyr::if_else(!is.na(RMSE) & is.finite(RMSE) & RMSE > 0, 1 / RMSE, 0),
                  time.rounded = ensemble.halfhour(timeEnd_A, timeBgn_B, timeEndA.local))
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
    dplyr::mutate(FG_ENSEMBLE_NPAIRS = dplyr::coalesce(FG_ENSEMBLE_NPAIRS, 0L),
                  time_start_utc = lubridate::with_tz(time.rounded, "UTC"),
                  time_end_utc = time_start_utc + 1800) %>%
    dplyr::arrange(gas, time.rounded)
}

# NEON 30-min turbulent fluxes (CO2 umol m-2 s-1; H2O mmol m-2 s-1 from LE) for the EC_30min column.
# Reads SITE_30min.Rdata (min30.list from flow.neon.data.extract.v2.R) in each folder of tower.dirs;
# values with turb.qfFinl == 1 are set to NA. Sites or periods without the file are skipped.
load.ec30.neon <- function(site, tower.dirs) {
  out <- list()
  for (dd in tower.dirs) {
    f <- file.path(dd, "data", site, paste0(site, "_30min.Rdata"))
    if (!file.exists(f)) next
    e <- new.env(); load(f, envir = e); m <- e$min30.list
    tbgn <- function(x) as.POSIXct(substr(x, 1, 19), format = "%Y-%m-%dT%H:%M:%S", tz = "UTC")
    co2 <- data.frame(time_start_utc = tbgn(m$F_co2$timeBgn), EC_30min = as.numeric(m$F_co2$turb))
    co2$EC_30min[m$F_co2$turb.qfFinl == 1] <- NA
    le <- data.frame(time_start_utc = tbgn(m$F_LE$timeBgn), LE = as.numeric(m$F_LE$turb))
    le$LE[m$F_LE$turb.qfFinl == 1] <- NA
    ta <- m$Tair
    if (!is.null(ta) && nrow(ta)) {
      top <- max(as.numeric(ta$TowerPosition), na.rm = TRUE)
      ta <- ta[as.numeric(ta$TowerPosition) == top, ]
      le$Tair <- as.numeric(ta$mean)[match(le$time_start_utc, tbgn(ta$timeBgn))]
    } else le$Tair <- NA_real_
    lambda <- (2.501 - 0.00237 * ifelse(is.finite(le$Tair), le$Tair, 20)) * 1e6     # J kg-1
    h2o <- data.frame(time_start_utc = le$time_start_utc, EC_30min = le$LE / lambda / 0.018 * 1000)
    out[[dd]] <- rbind(cbind(Site = site, gas = "CO2", co2), cbind(Site = site, gas = "H2O", h2o))
  }
  if (!length(out)) return(NULL)
  d <- do.call(rbind, out)
  d[!duplicated(d[, c("gas", "time_start_utc")]), ]
}

ensemble.edi.metadata <- function(ec30 = FALSE) {
  m <- data.frame(
    file_name_pattern = "{SITE}_GF_ENSEMBLE_30min.csv",
    column_name = c("site", "gas", "time_start_utc", "time_end_utc", "time_start_local", "time_end_local",
                    "FG_ENSEMBLE", "FG_ENSEMBLE_SD", "FG_ENSEMBLE_NPAIRS", "FG_ENSEMBLE_NVALUES", "FG_ENSEMBLE_APPROACHES",
                    "FG_ENSEMBLE_FLAG", "EC_mean", "EC_30min", "ensemble_method"),
    data_type = c("character", "character", "datetime", "datetime", "datetime", "datetime", "numeric", "numeric", "integer",
                  "integer", "character", "character", "numeric", "numeric", "character"),
    units = c("unitless", "unitless", "YYYY-MM-DD hh:mm:ss (UTC)", "YYYY-MM-DD hh:mm:ss (UTC)",
              "YYYY-MM-DD hh:mm:ss (site local time, including daylight saving time)",
              "YYYY-MM-DD hh:mm:ss (site local time, including daylight saving time)",
              "umol CO2 m-2 s-1 (CO2); mmol H2O m-2 s-1 (H2O)", "same as FG_ENSEMBLE", "count", "count", "unitless", "unitless",
              "umol CO2 m-2 s-1 (CO2); mmol H2O m-2 s-1 (H2O)", "umol CO2 m-2 s-1 (CO2); mmol H2O m-2 s-1 (H2O)", "unitless"),
    description = c(
      "NEON site code.",
      "Gas of the flux (CO2 or H2O).",
      "Start of the half-hour interval (UTC); every half-hour between the first and last ensemble value is listed. Unique key with gas.",
      "End of the half-hour interval (UTC; AmeriFlux TIMESTAMP_END convention).",
      "time_start_utc in site local time (repeats once a year at the end of daylight saving time).",
      "time_end_utc in site local time.",
      "Gradient-flux ensemble: inverse-RMSE weighted mean of the 9-min gradient fluxes from reliable sampling-height pairs (CCC >= 0.5 against EC; MBR, AE or WP) whose sampling midpoint (between the two inlet samples) falls in the half-hour. NA when no reliable value exists; not gap filled.",
      "Spread between pairs: standard deviation of the per-pair (approach x sampling-height pair) 30-min mean gradient flux. NA when fewer than two pairs contribute. Ranks uncertainty but is not a calibrated confidence interval.",
      "Number of reliable approach x sampling-height pairs contributing (0 when no reliable value).",
      "Number of 9-min gradient-flux values contributing.",
      "Gradient-flux approaches contributing, separated by ';'.",
      "ok: two or more pairs; single_pair: one pair (no spread); no_reliable_GF: no reliable gradient flux in the half-hour.",
      "Mean of the eddy-covariance turbulent flux interpolated to the midpoints of the contributing gradient-flux samples; for evaluation only.",
      "NEON 30-min eddy-covariance turbulent flux for the half-hour (quality flag 0 only; H2O from LE); NA where not available.",
      "Weighting used for FG_ENSEMBLE."),
    stringsAsFactors = FALSE)
  if (!ec30) m <- m[m$column_name != "EC_30min", ]
  m
}

write.ensemble.edi <- function(SITE_DATA_ENSEMBLE, edi.dir, method = "inverse_rmse_weighted", complete = TRUE, ec30 = NULL) {
  dir.create(edi.dir, showWarnings = FALSE, recursive = TRUE)
  fmt <- function(x, tz) format(x, "%Y-%m-%d %H:%M:%S", tz = tz)
  for (site in names(SITE_DATA_ENSEMBLE)) {
    d <- SITE_DATA_ENSEMBLE[[site]]
    if (is.null(d) || !nrow(d)) next
    d <- d[is.finite(d$FG_ENSEMBLE), ]
    if (!nrow(d)) next
    tz <- attr(d$time.rounded, "tzone"); if (is.null(tz) || !nzchar(tz[1])) tz <- "UTC"
    d$time_start_utc <- lubridate::with_tz(d$time.rounded, "UTC")
    if (complete) {
      grid <- expand.grid(gas = sort(unique(d$gas)),
                          time_start_utc = seq(min(d$time_start_utc), max(d$time_start_utc), by = 1800),
                          stringsAsFactors = FALSE)
      d <- merge(grid, d[, setdiff(names(d), "time.rounded")], by = c("gas", "time_start_utc"), all.x = TRUE)
    }
    d$FG_ENSEMBLE_NPAIRS[is.na(d$FG_ENSEMBLE_NPAIRS)] <- 0L
    d$FG_ENSEMBLE_NVALUES[is.na(d$FG_ENSEMBLE_NVALUES)] <- 0L
    flag <- ifelse(d$FG_ENSEMBLE_NPAIRS >= 2, "ok", ifelse(d$FG_ENSEMBLE_NPAIRS == 1, "single_pair", "no_reliable_GF"))
    out <- data.frame(site = site, gas = d$gas,
                      time_start_utc = fmt(d$time_start_utc, "UTC"), time_end_utc = fmt(d$time_start_utc + 1800, "UTC"),
                      time_start_local = fmt(d$time_start_utc, tz[1]), time_end_local = fmt(d$time_start_utc + 1800, tz[1]),
                      FG_ENSEMBLE = d$FG_ENSEMBLE, FG_ENSEMBLE_SD = d$FG_ENSEMBLE_SD,
                      FG_ENSEMBLE_NPAIRS = d$FG_ENSEMBLE_NPAIRS, FG_ENSEMBLE_NVALUES = d$FG_ENSEMBLE_NVALUES,
                      FG_ENSEMBLE_APPROACHES = d$FG_ENSEMBLE_APPROACHES, FG_ENSEMBLE_FLAG = flag,
                      EC_mean = d$EC_mean, stringsAsFactors = FALSE)
    if (!is.null(ec30)) {
      e <- ec30[ec30$Site == site, ]
      out$EC_30min <- e$EC_30min[match(paste(out$gas, out$time_start_utc), paste(e$gas, fmt(e$time_start_utc, "UTC")))]
    }
    out$ensemble_method <- method
    out <- out[order(out$gas, out$time_start_utc), ]
    dir.create(file.path(edi.dir, site), showWarnings = FALSE)
    utils::write.csv(out, file.path(edi.dir, site, paste0(site, "_GF_ENSEMBLE_30min.csv")), row.names = FALSE, na = "NA")
  }
  utils::write.csv(ensemble.edi.metadata(ec30 = !is.null(ec30)), file.path(edi.dir, "Metadata_table_GF_ENSEMBLE.csv"), row.names = FALSE)
  invisible(TRUE)
}
