# Rscript tests/test-ec-filter-selection.R
suppressPackageStartupMessages(library(dplyr))
suppressPackageStartupMessages(library(tidyr))
source("functions/calc.ec.filter.selection.R")
t <- as.POSIXct("2024-06-01", tz = "UTC") + 0:191 * 1800
d <- data.frame(gas = "CO2", Approach = "MBR", dLevelsAminusB = "2_1",
  timeEndA.local = t, TowerPosition_A = 2, TowerPosition_B = 1,
  dConc = rep(c(1, 4), 96), dConc_sd = 1,
  dConc.tracer = 4, dConc.tracer_sd = 1, ustar_interp = 1,
  ustar_threshold = .2, FG_mean = 1,
  FC_turb_interp = rep(c(1, 3), 96), FH2O_interp = 1)
a <- ec_filter_selection(d, thresholds = 3, min_per_hour = 1)
day <- a$daily %>% filter(Approach == "ANY")
stopifnot(abs(day$EC_daily_g_m2[day$stage == "profile_available"] -
  2 * 86400 * 44.01 / 1e6) < 1e-8)
stopifnot(abs(day$EC_daily_g_m2[day$stage == "turbulence_target"] -
  3 * 86400 * 44.01 / 1e6) < 1e-8)
# Duplicate method/pair rows must not change EC or the mask summaries.
b <- ec_filter_selection(bind_rows(d, d), 3, 1)
stopifnot(isTRUE(all.equal(a$hourly, b$hourly)),
          isTRUE(all.equal(a$daily, b$daily)))
# Independent target/tracer masks; exact agreement with production filter.
source("functions/calc.filter_FG.R")
d$dConc.tracer <- rep(c(4, 1), 96)
d$ustar_interp[1] <- NA
a <- ec_filter_selection(d, 3, 1)
both <- a$hourly %>% filter(Approach == "ANY", stage == "turbulence_target_tracer")
stopifnot(sum(both$n_retained) == nrow(filter_fluxes(d, 3, "MBR")))
stopifnot(sum(both$n_retained) == 0, all(is.na(both$EC_mean)))
# An absent hour prevents a complete daily total; it is never zero-filled.
d <- d[as.integer(format(d$timeEndA.local, "%H")) != 12, ]
a <- ec_filter_selection(d, 3, 1)
stopifnot(all(is.na(a$daily$EC_daily_g_m2)), any(a$hourly$n_reference == 0))
# Full EC includes times outside the profile record; optional final mask.
ref <- data.frame(gas = "CO2", time = t, EC = 2)
ens <- ref[1:10, c("gas", "time")]
a <- ec_filter_selection(d, 3, 1, ref, ens)
full <- a$daily %>% filter(stage == "all_reference")
stopifnot(all(full$hours_supported == 24), all(is.finite(full$EC_daily_g_m2)))
stopifnot(sum(a$hourly$n_retained[a$hourly$stage == "published_ensemble"]) == 10)
# H2O conversion and negative signed EC.
d$gas <- "H2O"; d$FH2O_interp <- -1
a <- ec_filter_selection(d, 3, 1,
  data.frame(gas = "H2O", time = t, EC = -1))
stopifnot(all(abs(a$daily$EC_daily_g_m2[a$daily$stage == "all_reference"] +
  86400 * .018015) < 1e-8))
message("EC selection diagnostic checks passed.")

# Exercise the standalone reader and CSV writer with production-shaped inputs.
local({
  root <- tempfile("ec-selection-")
  dir.create(file.path(root, "inputs", "TEST"), recursive = TRUE)
  on.exit(unlink(root, recursive = TRUE))
  d$timeEndA <- d$timeEnd_A <- d$timeEndA.local
  MBR_9min.df.final <- AE_9min.df.final <- WP_9min.df.final <- d
  save(MBR_9min.df.final, AE_9min.df.final, WP_9min.df.final,
       file = file.path(root, "inputs", "TEST", "TEST_Evaluation.Rdata"))
  write.csv(data.frame(Site = "TEST", dLevelsAminusB = "2_1", Canopy_L1 = "AA"),
            file.path(root, "canopy.csv"), row.names = FALSE)
  config <- file.path(root, "config.R")
  writeLines(c(paste0("localdir <- ", encodeString(root, quote = '"')),
    'eval.data.dirs <- "inputs"', 'site.timezones <- c(TEST = "UTC")',
    'canopy.file <- file.path(localdir, "canopy.csv")',
    'output.dir <- file.path(localdir, "output")',
    'thresholds <- 3', 'min_per_hour <- 1L'), config)
  status <- system2(file.path(R.home("bin"), "Rscript"),
    c("workflows/flow.ec.filter.selection.R", shQuote(config)))
  stopifnot(status == 0, file.exists(file.path(root, "output", "TEST_daily.csv")),
            nrow(read.csv(file.path(root, "output", "TEST_hourly.csv"))) > 0)
})
message("Standalone workflow check passed.")

