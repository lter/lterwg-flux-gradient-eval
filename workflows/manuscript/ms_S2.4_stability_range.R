# Sect. 2.4.2: validity range of the stability corrections (Eqs. 4-5, derived for about -5 < zeta < 0 and 0 <= zeta < 1).
# Share of the filtered AE and WP values (CO2, H2O) whose pair stability zeta = (z - d)/L lies outside -5 < zeta < 1.
# Input: the per-site filter files NEON_GradientFlux_Data_Filter/{SITE}/{SITE}_FILTER<run.tag>.RData written by
# flow.evaluation.filter.R (AE_9min_FILTER, WP_9min_FILTER); the combined SITE_DATA_FILTERED* tables drop zeta.
# Manuscript (v3b): < 0.2 % of AE and < 0.4 % of WP values outside. Not re-run on 2 Nov 2026 (files only on Bouchet).
if (!exists("ms_rdata")) source(file.path(DirRepo.eval, "workflows", "manuscript", "ms_setup.R"))

fdir  <- file.path(localdir, "NEON_GradientFlux_Data_Filter")
sites <- list.dirs(fdir, full.names = FALSE, recursive = FALSE)
Z <- list()
for (s in sites) {
  f <- tryCatch(ms_rdata("NEON_GradientFlux_Data_Filter", s, paste0(s, "_FILTER", run.tag, ".RData")), error = function(e) NULL)
  if (is.null(f)) next
  e <- new.env(); load(f, envir = e)
  for (m in c("AE", "WP")) { d <- e[[paste0(m, "_9min_FILTER")]]; if (!NROW(d)) next
    zc <- intersect(c("zeta", "zoL"), names(d))[1]; if (is.na(zc)) stop("No zeta column in ", f)
    d <- d[d$gas %in% c("CO2", "H2O") & is.finite(d$FG_mean), , drop = FALSE]
    Z[[length(Z) + 1]] <- tibble(Site = s, Approach = m, gas = d$gas, zeta = d[[zc]]) }
  rm(e); gc()
}
Z <- bind_rows(Z); if (!nrow(Z)) stop("No per-site filter files for ", run.tag, " in ", fdir)
p("sites: %d", n_distinct(Z$Site))
pr(Z %>% group_by(Approach) %>% summarise(n = n(), zeta_missing_pct = round(100 * mean(!is.finite(zeta)), 2),
                                          outside_pct = round(100 * mean(zeta <= -5 | zeta >= 1, na.rm = TRUE), 2),
                                          le_minus5_pct = round(100 * mean(zeta <= -5, na.rm = TRUE), 2),
                                          ge_1_pct = round(100 * mean(zeta >= 1, na.rm = TRUE), 2)))
pr(Z %>% group_by(Approach, gas) %>% summarise(outside_pct = round(100 * mean(zeta <= -5 | zeta >= 1, na.rm = TRUE), 2), .groups = "drop"))
