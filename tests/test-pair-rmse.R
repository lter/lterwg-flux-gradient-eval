# Run from the repository root: Rscript tests/test-pair-rmse.R
suppressPackageStartupMessages(library(dplyr))
DirRepo.eval <- getwd()
source("functions/calc.One2One.CCC_testing.R")

check_equal <- function(actual, expected) {
  stopifnot(isTRUE(all.equal(as.numeric(actual), as.numeric(expected), tolerance = 1e-10)))
}

ec <- seq(-10, 10, length.out = 101)
biased <- data.frame(EC_mean = ec, FG_mean = 2 * ec, dLevelsAminusB = "2_1")
result <- suppressWarnings(ccc.parms("FG_mean", "EC_mean", biased, "test"))
check_equal(result$RMSE, sqrt(34))
check_equal(result$CCC, 0.8)
check_equal(result$Slope, 2)
check_equal(result$count, 101)
annotation <- suppressWarnings(get_facet_stats(biased, "2_1", "EC_mean", "FG_mean"))
stopifnot(grepl("RMSE = 5.83", annotation, fixed = TRUE))

identity <- transform(biased, FG_mean = EC_mean)
result <- suppressWarnings(ccc.parms("FG_mean", "EC_mean", identity, "test"))
check_equal(result$RMSE, 0)

# Nonfinite values in either column must not break or misalign the pairs.
invalid <- data.frame(EC_mean = c(NA, 1, Inf, 1, NaN),
                      FG_mean = c(1, NA, 1, -Inf, 1), dLevelsAminusB = "2_1")
mixed <- rbind(biased, invalid)
result <- suppressWarnings(ccc.parms("FG_mean", "EC_mean", mixed, "test"))
check_equal(result$RMSE, sqrt(34))
check_equal(result$count, 101)
stopifnot(identical(annotation, suppressWarnings(
  get_facet_stats(mixed, "2_1", "EC_mean", "FG_mean"))))
few <- rbind(biased[1:2, ], invalid)
stopifnot(is.na(ccc.parms("FG_mean", "EC_mean", few, "test")$RMSE))
message("Pair RMSE checks passed.")
