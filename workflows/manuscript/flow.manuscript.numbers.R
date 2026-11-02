# flow.manuscript.numbers.R -----------------------------------------------------------------------------
# Recomputes the in-text numbers of essd-2026-413 that are not written to a table or figure by
# flow.evaluation.batch_V1.R. Run it after the batch (it is sourced at the end of the batch) and, for Sects. 2.5/3.4,
# after the EC-filter diagnostic (workflows/flow.ec.filter.selection.R).
# Each section's output is written to <localdir>/Manuscript_numbers/<script><run.tag>.txt.
#
# Where every manuscript number comes from:
#   Abstract, 3.1  ms_S3.1_reliable_pairs.R          RSHP counts/shares, CCC by approach/canopy, ecotypes, coverage
#   2.4.2          ms_S2.4_stability_range.R         share of AE/WP values outside the Eq. 4-5 stability range
#   2.5            ms_S2.5_filters.R                 filter removal shares
#   2.5, 3.4       ms_S2.5_S3.4_ec_representativeness.R  EC conditions kept by the filters (GitHub issue #5)
#   2.7, 3.2       ms_S3.2_random_forest.R           random-forest OOB, accuracy, NIR
#   3.2            ms_S3.2_geometry_structure.R      placement, lidar canopy height, structure, delta z
#   2.8, 3.3, 3.4  ms_S3.3_ensemble.R                ensemble vs pairs, overlap, spread, CPER/CLBJ ranges
#   3.3, 4.3       ms_S3.3_window_gain.R             ensemble half-hours vs best single pair (slow: reads 9-min files)
#   3.4, abstract  ms_S3.4_diel.R                    diel R2, daily totals
#   4.2, Fig. 9    ms_S4.2_placement.R               placement of the best pairs
#   App. B, S1     ms_AppB_spikes.R                  spike rates (flow.evaluation.spike_eval.R outputs)
#   App. C, S2     ms_AppC_noEC.R                    no-EC pair selection, pre-checks, AUCs, top-3 RF (slow)
# Tables and figures come from the batch itself (see README, "Manuscript map").

if (!exists("localdir"))     localdir     <- "/Volumes/MaloneLab/Research/FluxGradient"
if (!exists("run.tag"))      run.tag      <- "_AA_AW_2021_2026_v3b"
if (!exists("DirRepo.eval")) DirRepo.eval <- getwd()
if (!exists("ms.sections"))  ms.sections  <- c("ms_S2.4_stability_range.R", "ms_S2.5_filters.R", "ms_S2.5_S3.4_ec_representativeness.R", "ms_S3.1_reliable_pairs.R",
                                               "ms_S3.2_random_forest.R", "ms_S3.2_geometry_structure.R", "ms_S3.3_ensemble.R",
                                               "ms_S3.3_window_gain.R", "ms_S3.4_diel.R", "ms_S4.2_placement.R", "ms_AppB_spikes.R",
                                               "ms_AppC_noEC.R")
ms.dir <- file.path(DirRepo.eval, "workflows", "manuscript")
out.dir <- file.path(localdir, "Manuscript_numbers"); dir.create(out.dir, showWarnings = FALSE)
source(file.path(ms.dir, "ms_setup.R"))

for (f in ms.sections) {
  out <- file.path(out.dir, paste0(sub("\\.R$", "", f), run.tag, ".txt"))
  message("Manuscript numbers: ", f, " -> ", out)
  con <- file(out, open = "wt"); sink(con); sink(con, type = "message")
  cat("#", f, "| run.tag", run.tag, "|", format(Sys.time()), "\n")
  ok <- tryCatch({ source(file.path(ms.dir, f), local = new.env()); TRUE },
                 error = function(e) { cat("ERROR:", conditionMessage(e), "\n"); FALSE })
  sink(type = "message"); sink(); close(con)
  if (!ok) warning(f, " failed; see ", out)
}
