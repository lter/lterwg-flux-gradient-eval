# Run from the repository root: Rscript tests/test-diel-units.R
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

# CO2 inputs are umol/m2/s; H2O inputs are mmol/m2/s.
# Negative CO2 also checks preservation of the uptake sign.
check_daily_mass("CO2", -1, 44.01 / 1e6)
check_daily_mass("H2O", 1, 0.018015)
message("Diel hourly units and daily mass checks passed.")
