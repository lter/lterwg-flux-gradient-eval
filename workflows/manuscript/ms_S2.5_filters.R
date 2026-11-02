# Sect. 2.5: share of gradient-flux values removed by each filter (filter reports written by flow.evaluation.filter.R).
# Input: NEON_GradientFlux_Data_Filter/SITES_FILTER_REPORTS<run.tag>.RData
# Text: removal among rows with complete inputs (rejected - missing_inputs)/(total - missing_inputs), by approach;
#       per-filter flag shares (u*, SNR, TSNR, cross-gradient), weighted by the number of values of each pair.
if (!exists("ms_rdata")) source(file.path(DirRepo.eval, "workflows", "manuscript", "ms_setup.R"))

e <- ms_load("NEON_GradientFlux_Data_Filter", paste0("SITES_FILTER_REPORTS", run.tag, ".RData"))
for (g in c("CO2", "H2O")) {
  x <- e[[paste0("filter.report.", g)]] %>% mutate(n = total - missing_inputs)
  hdr(g)
  p("removed among rows with complete inputs: %.1f %%   (all rejected / all rows: %.1f %%)",
    100 * sum(x$rejected - x$missing_inputs) / sum(x$n), 100 * sum(x$rejected) / sum(x$total))
  p("rows with complete inputs: %.0f %%", 100 * sum(x$n) / sum(x$total))
  y <- x %>% mutate(rem = 100 * (rejected - missing_inputs) / n)
  p("removal, 10th-90th percentile across pairs: %s %%", paste(round(quantile(y$rem, c(.1, .9), na.rm = TRUE)), collapse = "-"))
  w <- function(v) round(weighted.mean(v, x$total, na.rm = TRUE), 1)
  p("flag shares (%% of values): all filters %s, u* %s, SNR %s, cross-gradient %s",
    w(x$flag.interaction.ALL), w(x$flag.ustar_interp), w(x$flag.dConcSNR), w(x$flag.cross_grad))
  # u* is also flagged where EC (u*) is missing; share among values with complete inputs (text, Sect. 2.5):
  miss <- 100 * sum(x$missing_inputs) / sum(x$total)
  p("u* flag among values with complete inputs: %.0f %%  ((u* share - missing share) / complete share)",
    100 * (w(x$flag.ustar_interp) - miss) / (100 - miss))
  pr(x %>% group_by(approach) %>% summarise(
    removed_pct = round(100 * sum(rejected - missing_inputs) / sum(n)),
    complete_inputs_pct = round(100 * sum(n) / sum(total)),
    all = round(weighted.mean(flag.interaction.ALL, total, na.rm = TRUE)),
    ustar = round(weighted.mean(flag.ustar_interp, total, na.rm = TRUE)),
    snr = round(weighted.mean(flag.dConcSNR, total, na.rm = TRUE)),
    tsnr = round(weighted.mean(flag.dConcTSNR, total, na.rm = TRUE)),
    cross_grad = round(weighted.mean(flag.cross_grad, total, na.rm = TRUE))))
}
# Note: the reports count rows without EC as well, so the u* share of values that have EC is estimated in the text
# by removing missing-input rows (Sect. 2.5).
