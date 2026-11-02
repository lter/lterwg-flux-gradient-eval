suppressPackageStartupMessages({ library(dplyr); library(ggplot2) })
args <- commandArgs(trailingOnly = TRUE)
root <- normalizePath(if (length(args)) args[1] else "..")
calc <- file.path(root, "lterwg-flux-gradient")
DirRepo.eval <- file.path(root, "lterwg-flux-gradient-eval")
source(file.path(calc, "functions/workflow.runtime.R"))
assert_error <- function(expr) stopifnot(inherits(tryCatch({force(expr); NULL}, error = identity), "error"))
checks <- 0L
passed <- function(label) { checks <<- checks + 1L; cat("PASS:", label, "\n") }
work <- tempfile("workflow-tests-"); dir.create(work)
x <- data.frame(a = 1:3)
target <- file.path(work, "data.RData")
atomic_save(x, file = target)
assert_error(atomic_save(no_such_object, file = target))
loaded <- new.env(); load(target, loaded); stopifnot(identical(loaded$x, x))
stopifnot(length(list.files(work, all.files = TRUE, pattern = "^\\.data")) == 0L)
passed("failed serialization preserves previous output and removes temporary file")
corrupt <- file.path(work, "corrupt.RData"); writeBin(as.raw(1:10), corrupt)
assert_error(suppressWarnings(validate_rdata(corrupt, "x")))
assert_error(validate_rdata(target, "wrong_object"))
passed("checkpoint validation rejects corrupt and wrong-object files")
archive <- file.path(work, "data.zip")
atomic_zip(archive, target)
old <- readBin(archive, "raw", n = file.info(archive)$size)
assert_error(atomic_zip(archive, file.path(work, "missing")))
stopifnot(identical(old, readBin(archive, "raw", n = file.info(archive)$size)))
passed("verified archive publication and failure preservation")
atomic_write_csv(x, file.path(work, "data.csv"), row.names = FALSE)
stopifnot(nrow(read.csv(file.path(work, "data.csv"))) == 3)
passed("atomic CSV publication")
source(file.path(calc, "functions/calc.aerodynamic.canopy.height.R"))
source(file.path(calc, "functions/calc.stability.correction.R"))
source(file.path(calc, "functions/calc.MO.length.R"))
t <- as.POSIXct("2026-01-01", tz = "UTC") + seq(0, by = 600, length.out = 4)
gas <- data.frame(P_kPa = 100, Tair2 = 20, H_turb_interp = 30, LE_turb_interp = 50,
                  ustar_interp = 0.1, TowerPosition_A = 2, TowerPosition_B = 1,
                  TowerHeight_A = 20, TowerHeight_B = 10, dLevelsAminusB = "2_1", timeEnd_A = t, ubar2 = 2)
min9Diff.list <- list(H2O = transform(gas, TowerPosition_A = c(2, 1, 2, 1)))
result <- calc.stability.correction("TEST", gas, min9Diff.list)
stopifnot(all(is.na(result$z_veg_aero)), all(is.na(result$phih)))
passed("zero qualifying canopy periods propagate NA without aborting")
gas$ustar_interp <- 0.4; gas$timeEnd_A <- rep(t[1], 4); gas$H_turb_interp <- 0.001; gas$LE_turb_interp <- 0
min9Diff.list <- list(H2O = transform(gas, TowerPosition_A = c(2, 1, 2, 1)))
result <- calc.stability.correction("TEST", gas, min9Diff.list)
stopifnot(all(is.finite(result$z_veg_aero)), length(unique(result$z_veg_aero)) == 1)
passed("one distinct canopy timestamp uses constant finite height")
source(file.path(DirRepo.eval, "functions/calc.filter_FG.R"))
d <- data.frame(dLevelsAminusB = "2_1", TowerPosition_A = 2, TowerPosition_B = 1,
                dConc = c(NA, 10, 1, 10), dConc_sd = 1, ustar_interp = c(0.4, NA, 0.4, 0.4),
                ustar_threshold = 0.2, gas = "CO2", FG_mean = 1, FC_turb_interp = 1,
                cross_grad_flag = 0, timeEnd_A = t, Stability_500 = "neutral")
f <- filter_fluxes(d, 3, "AE")
r <- filter_report(d, 3, "AE"); rs <- filter_report_stability(d, 3, "AE")
stopifnot(nrow(f) == 1L, r$retained == 1L, r$rejected == 3L, r$missing_inputs == 2L,
          r$flag.interaction.ALL == 75, rs$rejected == 3L)
passed("filter and both reports agree on missing inputs and rejected rows")
stages <- filter_stage_report(d, f, f[FALSE, ], "AE")
stopifnot(stages$input == 4, stages$rejected_snr_ustar == 3, stages$rejected_spikes == 1,
          stages$retained == 0, stages$input == stages$rejected_total + stages$retained)
passed("SNR, friction velocity, and spike counts reconcile")
snr <- parse(file.path(DirRepo.eval, "workflows/flow.evaluation.SNR.R"))
loop <- snr[[which(vapply(snr, function(x) is.call(x) && identical(x[[1]], as.name("for")), logical(1)))[1]]]
fn <- Filter(function(x) is.call(x) && identical(x[[1]], as.name("<-")) && identical(x[[2]], as.name("get.rmse")), as.list(loop[[4]]))[[1]]
eval(fn)
d <- data.frame(dLevelsAminusB = "2_1", TowerPosition_A = 2, TowerPosition_B = 1,
                dConc = 10, dConc_sd = 1, ustar_interp = 0.4, ustar_threshold = 0.2,
                gas = "CO2", FG_mean = 1:20, FC_nee_interp = 1:20, FC_turb_interp = (1:20) + 10)
stopifnot(get.rmse(d, 3, "AE", "CO2", "2_1") == 10)
d$FC_turb_interp <- NA_real_
stopifnot(is.na(get.rmse(d, 3, "AE", "CO2", "2_1")))
passed("SNR uses direct turbulent flux RMSE, retains bias, handles no finite pairs")
source(file.path(calc, "functions/calc.eqn.aero.windprof.flux.R"))
source(file.path(calc, "functions/calc.gas.aero.windprof.flux.R"))
g <- data.frame(mean_A = 10 + 1:12, mean_B = 5, vari_A = 1, vari_B = 2,
                rhoa_kgm3 = 1.2, TowerHeight_A = 20, TowerHeight_B = 10, EddyDiff = 2, EddyDiff_WP = 3)
min9 <- list(H2O = g, CO2 = g, CH4 = g)
set.seed(123); saved <- .Random.seed
one <- calc.gas.aero.windprof.flux(min9, bootstrap = 1, nsamp = 50, rng.key = c("v3", "TEST", "AE"))
stopifnot(identical(saved, .Random.seed))
invisible(runif(31))
calc.gas.aero.windprof.flux(min9, bootstrap = 1, nsamp = 50, rng.key = c("v3", "OTHER", "AE")) -> other
two <- calc.gas.aero.windprof.flux(min9, bootstrap = 1, nsamp = 50, rng.key = c("v3", "TEST", "AE"))
stopifnot(identical(one, two), !identical(one$CO2$FG_mean, other$CO2$FG_mean))
passed("AE/WP bootstrap streams reproduce across site order and preserve caller RNG state")
source(file.path(calc, "functions/calc.mbr.R"))
g$timeEnd_A <- t[1] + seq_len(12) * 600; g$timeEnd_B <- g$timeEnd_A + 60
g$match_time <- g$timeEnd_A; g$dLevelsAminusB <- "2_1"; g$FH2O_interp <- 2; g$FC_turb_interp <- 3
min9 <- list(H2O = g, CO2 = g, CH4 = g)
a <- calc.mbr(min9, 1, 50, rng.key = c("v3", "TEST", "MBR"))
invisible(runif(13)); z <- calc.mbr(min9, 1, 50, rng.key = c("v3", "TEST", "MBR"))
stopifnot(identical(a, z))
passed("MBR bootstrap reproduces after unrelated random draws")
source(file.path(DirRepo.eval, "functions/calc.One2One.CCC_testing.R"))
# Capture the actual constructed panels, avoiding file rendering or production inputs.
plotfn <- ccc.plots
walk <- function(x) {
  if (!is.call(x)) return(x)
  if (identical(x[[1]], quote(ggpubr::ggarrange))) return(quote(list(p.1, p.3, p.2)))
  for (j in seq_along(x)[-1]) if (is.call(x[[j]])) x[[j]] <- walk(x[[j]])
  x
}
body(plotfn) <- walk(body(plotfn))
for (gas.name in c("CO2", "H2O")) {
  mk <- function(offset) data.frame(gas = gas.name, dLevelsAminusB = "2_1", EC_mean = 1:20, FG_mean = (1:20) + offset)
  panels <- suppressWarnings(plotfn(mk(1), mk(2), mk(3), gas.name))
  for (i in 1:3) {
    label <- panels[[i]]$facet$params$labeller(data.frame(dLevelsAminusB = "2_1"))[[1]]
    stopifnot(grepl(sprintf("RMSE = %.2f", i), label, fixed = TRUE))
    invisible(suppressWarnings(ggplotGrob(panels[[i]])))
  }
}
passed("all six rendered facets use their own method statistics")
unlink(work, recursive = TRUE)
cat(checks, "regression checks passed\n")
