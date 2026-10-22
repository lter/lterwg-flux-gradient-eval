# EC selection diagnostic. Run per site; see docs/ec-filter-selection.md.
# Inputs use site-local POSIXct times. Reference times must match the ensemble's
# round_date(..., "30 minutes") labels. EC units: CO2 umol/m2/s, H2O mmol/m2/s.
ec_filter_selection <- function(df, thresholds = c(2, 3, 4),
                                min_per_hour = 5L, ec_reference = NULL,
                                ensemble_times = NULL) {
  required <- c("gas", "Approach", "dLevelsAminusB", "timeEndA.local",
                "TowerPosition_A", "TowerPosition_B", "dConc", "dConc_sd",
                "ustar_interp", "ustar_threshold", "FG_mean",
                "FC_turb_interp", "FH2O_interp")
  stopifnot(all(required %in% names(df)), inherits(df$timeEndA.local, "POSIXt"),
            min_per_hour >= 1, length(thresholds) > 0,
            all(is.finite(thresholds) & thresholds > 0))
  df <- as.data.frame(df)
  df <- df[df$gas %in% c("CO2", "H2O") &
             !is.na(df$TowerPosition_A) & !is.na(df$TowerPosition_B) &
             df$TowerPosition_A != df$TowerPosition_B &
             !is.na(df$timeEndA.local), ]
  if (!nrow(df)) stop("No eligible profile rows.")
  if (any(!df$Approach %in% c("MBR", "AE", "WP"))) stop("Unknown approach.")
  if (any(df$Approach == "MBR") &&
      !all(c("dConc.tracer", "dConc.tracer_sd") %in% names(df)))
    stop("MBR requires tracer gradient and SD.")
  if (!"dConc.tracer" %in% names(df)) df$dConc.tracer <- NA_real_
  if (!"dConc.tracer_sd" %in% names(df)) df$dConc.tracer_sd <- NA_real_
  tz <- attr(as.POSIXct(df$timeEndA.local), "tzone")
  if (is.null(tz) || !length(tz) || !nzchar(tz[1])) stop("Set an explicit site timezone.")
  tz <- tz[1]
  df$.time <- as.numeric(lubridate::round_date(df$timeEndA.local, "30 minutes"))
  df$.ec <- ifelse(df$gas == "CO2", df$FC_turb_interp, df$FH2O_interp)

  if (is.null(ec_reference)) {
    # Fixed reference, not recomputed after each mask. Exact duplicate method/pair
    # records cannot give an EC estimate extra weight.
    ref <- df %>%
      dplyr::filter(is.finite(.ec)) %>%
      dplyr::distinct(gas, .time, .ec) %>%
      dplyr::summarise(EC = mean(.ec), .by = c(gas, .time))
    reference_type <- "profile_aligned_EC_proxy"
  } else {
    stopifnot(all(c("gas", "time", "EC") %in% names(ec_reference)),
              inherits(ec_reference$time, "POSIXt"))
    ref <- ec_reference %>%
      dplyr::transmute(gas, .time = as.numeric(time), EC) %>%
      dplyr::filter(gas %in% c("CO2", "H2O"), is.finite(.time), is.finite(EC))
    if (anyDuplicated(ref[c("gas", ".time")]))
      stop("EC reference must contain one value per gas and half-hour.")
    if (any(as.numeric(lubridate::round_date(
      as.POSIXct(ref$.time, origin = "1970-01-01", tz = tz), "30 minutes")) != ref$.time))
      stop("EC reference times must use half-hour labels.")
    reference_type <- "supplied_quality_controlled_EC"
  }
  if (!nrow(ref)) stop("No finite EC reference values.")
  unmatched <- df %>% dplyr::distinct(gas, .time) %>%
    dplyr::anti_join(ref, by = c("gas", ".time")) %>%
    dplyr::count(gas, name = "profile_windows_without_EC")

  # Reproduce production comparisons, including their NA -> rejected behavior.
  pass <- function(x) !is.na(x) & x
  u <- pass(df$ustar_interp >= df$ustar_threshold)
  groups <- c("gas", "Approach", "pair", "threshold", "stage", "season")
  summarize_windows <- function(windows) {
    local <- as.POSIXct(windows$.time, origin = "1970-01-01", tz = tz)
    month <- as.integer(format(local, "%m"))
    windows$hour <- as.integer(format(local, "%H"))
    windows$season <- c("Winter", "Spring", "Summer", "Autumn")[
      ifelse(month %in% c(12, 1, 2), 1, ifelse(month <= 5, 2, ifelse(month <= 8, 3, 4)))]
    windows %>% dplyr::summarise(
      n_reference = dplyr::n(), n_retained = sum(retained),
      retention = mean(retained),
      EC_mean = if (any(retained)) mean(EC[retained]) else NA_real_,
      EC_mean_absolute = if (any(retained)) mean(abs(EC[retained])) else NA_real_,
      .by = dplyr::all_of(c(groups, "hour")))
  }
  results <- list()
  configs <- df %>% dplyr::distinct(Approach, dLevelsAminusB)
  configs <- dplyr::bind_rows(configs,
    data.frame(Approach = "ANY", dLevelsAminusB = "ANY"))
  for (cut in unique(thresholds)) {
    target <- pass(abs(df$dConc) / df$dConc_sd >= cut)
    tracer <- df$Approach != "MBR" |
      pass(abs(df$dConc.tracer) / df$dConc.tracer_sd >= cut)
    masks <- list(profile_available = rep(TRUE, nrow(df)),
                  turbulence = u,
                  turbulence_target = u & target,
                  turbulence_tracer = u & tracer,
                  turbulence_target_tracer = u & target & tracer,
                  finite_GF_after_filters = u & target & tracer & is.finite(df$FG_mean))
    for (j in seq_len(nrow(configs))) {
      cfg <- configs[j, ]
      belongs <- if (cfg$Approach == "ANY") rep(TRUE, nrow(df)) else
        df$Approach == cfg$Approach & df$dLevelsAminusB == cfg$dLevelsAminusB
      for (stage in c("all_reference", names(masks))) {
        keys <- if (stage == "all_reference") ref[c("gas", ".time")] else
          unique(df[belongs & masks[[stage]], c("gas", ".time")])
        windows <- ref %>% dplyr::left_join(
          dplyr::mutate(keys, retained = TRUE), by = c("gas", ".time")) %>%
          dplyr::mutate(retained = dplyr::coalesce(retained, FALSE),
                        Approach = cfg$Approach, pair = cfg$dLevelsAminusB,
                        threshold = cut, stage = stage)
        results[[length(results) + 1L]] <- summarize_windows(windows)
      }
    }
  }
  # Optional actual published ensemble availability. It is not an SNR rerun:
  # CCC selection and despiking are included only through this supplied mask.
  if (!is.null(ensemble_times)) {
    stopifnot(all(c("gas", "time") %in% names(ensemble_times)),
              inherits(ensemble_times$time, "POSIXt"))
    keys <- ensemble_times %>%
      dplyr::transmute(gas, .time = as.numeric(time), retained = TRUE) %>%
      dplyr::distinct()
    windows <- ref %>%
      dplyr::left_join(keys, by = c("gas", ".time")) %>%
      dplyr::mutate(retained = dplyr::coalesce(retained, FALSE),
                    Approach = "ANY", pair = "ANY", threshold = NA_real_,
                    stage = "published_ensemble")
    results[[length(results) + 1L]] <- summarize_windows(windows)
  }
  hourly <- dplyr::bind_rows(results)
  # Add absent hours explicitly, including when the reference itself lacks them.
  hourly <- hourly %>% tidyr::complete(
    tidyr::nesting(gas, Approach, pair, threshold, stage, season), hour = 0:23,
    fill = list(n_reference = 0L, n_retained = 0L))
  daily <- hourly %>% dplyr::summarise(
    hours_supported = sum(n_retained >= min_per_hour),
    min_hour_count = min(n_retained),
    EC_daily_g_m2 = if (all(n_retained >= min_per_hour))
      sum(EC_mean) * 3600 * ifelse(gas[1] == "CO2", 44.01 / 1e6, 0.018015) else NA_real_,
    .by = dplyr::all_of(groups))
  baseline <- daily %>% dplyr::filter(stage == "all_reference") %>%
    dplyr::select(-stage, -hours_supported, -min_hour_count) %>%
    dplyr::rename(EC_reference_daily_g_m2 = EC_daily_g_m2)
  # Baseline is independent of threshold and configuration; this also handles
  # the separately supplied published-ensemble mask (threshold = NA).
  baseline <- baseline %>%
    dplyr::distinct(gas, season, EC_reference_daily_g_m2)
  daily <- daily %>% dplyr::left_join(baseline, by = c("gas", "season")) %>%
    dplyr::mutate(daily_difference_g_m2 = EC_daily_g_m2 - EC_reference_daily_g_m2)
  hourly$reference_type <- daily$reference_type <- reference_type
  list(hourly = hourly, daily = daily, unmatched = unmatched)
}

