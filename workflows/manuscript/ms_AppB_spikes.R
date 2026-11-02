# Appendix B / Supplement S1 and Sect. 2.5: share of gradient-flux values removed as spikes by despike_fluxes()
# (median +/- 6 MAD per gas x pair x day/night x season), overall, by approach and gas, by tower level and by site.
# Inputs: Spike_Eval/SPIKE_rate_by_level<run.tag>.csv, SPIKE_rate_by_site<run.tag>.csv, SPIKE_odds_ratios<run.tag>.csv,
#         SPIKE_AA_vs_AW_paired<run.tag>.csv (flow.evaluation.spike_eval.R; Tables B1-B2 are the last two files).
if (!exists("ms_rdata")) source(file.path(DirRepo.eval, "workflows", "manuscript", "ms_setup.R"))

x <- ms_data_csv("Spike_Eval", "SPIKE_rate_by_level") %>% mutate(sp = spike_pct * obs / 100)
p("all values: %d, spikes %.2f %%", sum(x$obs), 100 * sum(x$sp) / sum(x$obs))
hdr("by approach and gas"); pr(x %>% group_by(Approach, gas) %>% summarise(obs = sum(obs), spike_pct = round(100 * sum(sp) / sum(obs), 1), .groups = "drop"))
hdr("by approach");         pr(x %>% group_by(Approach) %>% summarise(spike_pct = round(100 * sum(sp) / sum(obs), 1)))
lvl <- intersect(c("level", "Level", "TowerPosition", "lower_level", "Canopy_L1"), names(x))
if (length(lvl)) { hdr(paste("by", lvl[1])); pr(x %>% group_by(.data[[lvl[1]]]) %>% summarise(spike_pct = round(100 * sum(sp) / sum(obs), 1))) }
s <- ms_data_csv("Spike_Eval", "SPIKE_rate_by_site")
hdr("by site: range of spike rates"); print(summary(s[[grep("pct", names(s), value = TRUE)[1]]]))
hdr("Table B1 (odds ratios)");        pr(ms_data_csv("Spike_Eval", "SPIKE_odds_ratios"))
hdr("Table B2 (AA vs AW, paired)");   pr(ms_data_csv("Spike_Eval", "SPIKE_AA_vs_AW_paired"))
