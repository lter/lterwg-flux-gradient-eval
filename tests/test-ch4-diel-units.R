# Run from the repository root: Rscript tests/test-ch4-diel-units.R
suppressPackageStartupMessages(library(dplyr))
source("functions/calc.diel.R")

check_equal <- function(actual, expected) {
  stopifnot(isTRUE(all.equal(as.numeric(actual), as.numeric(expected), tolerance = 1e-10)))
}

check_daily_mass <- function(gas, rate, grams_per_input_unit) {
  # The existing function plots the model; keep test output out of the repo.
  plot_file <- tempfile(fileext = ".pdf")
  pdf(plot_file)
  on.exit({ dev.off(); unlink(plot_file) })
  d <- data.frame(
    time.rounded = as.POSIXct("2024-01-01", tz = "UTC") + 0:191 * 1800,
    gas = gas, season = "Winter", FG_ENSEMBLE = rate, EC_mean = 2 * rate)
  curve <- DIEL.COMPILE.ENSEMBLE(d, "FG_ENSEMBLE", "EC_mean", gas)
  stopifnot(nrow(curve) == 24, identical(sort(curve$Hour), 0:23))
  # Check both curve branches and the same 24-value sum used by the workflow.
  check_equal(curve$FG, rep(rate * grams_per_input_unit * 3600, 24))
  check_equal(sum(curve$FG), rate * grams_per_input_unit * 86400)
  check_equal(sum(curve$EC), 2 * rate * grams_per_input_unit * 86400)
  check_equal(sum(curve$DIFF.DIEL), -rate * grams_per_input_unit * 86400)
}

# CH4 is in umol/m2/s, with one carbon atom per molecule.
# A constant 1 umol/m2/s must yield 1.0377504 g C/m2 over 24 hours.
check_daily_mass("CH4", 1, 12.011 / 1e6)
check_daily_mass("CH4", -1, 12.011 / 1e6)
message("CH4 hourly carbon units and daily mass checks passed.")
