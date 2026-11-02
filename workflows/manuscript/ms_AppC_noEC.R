# Appendix C (and Supplement S2): choosing pairs without an EC reference.
# Part 1 (Tables C1-C3, text): from the outputs of flow.evaluation.noEC_pair_selection.R (Spike_Eval/noEC_*<run.tag>.csv).
# Part 2: AA-MBR self-agreement as a predictor of a reliable ensemble (AUC) and a leave-one-site-out random forest
#         that picks the top 3 pairs per site (median CCC of their ensemble against EC). Reads the filtered 9-min values.
# Inputs: Spike_Eval/noEC_pairs_CCC_vs_AAMBR_reference, noEC_ensemble_CCC_by_rule, noEC_prechecks_by_site (<run.tag>.csv);
#         NEON_GradientFlux_Data_Filter/SITES_{MBR,AE,WP}_9min_FILTER<run.tag>.RData; flow.flux.counts<run.tag>.RData.
if (!exists("ms_rdata")) source(file.path(DirRepo.eval, "workflows", "manuscript", "ms_setup.R"))
suppressPackageStartupMessages({ library(data.table); library(randomForest) })

P <- ms_data_csv("Spike_Eval", "noEC_pairs_CCC_vs_AAMBR_reference")
E <- ms_data_csv("Spike_Eval", "noEC_ensemble_CCC_by_rule"); names(E) <- gsub("\n", " ", names(E))
K <- ms_data_csv("Spike_Eval", "noEC_prechecks_by_site")

hdr("Table C1: AE/WP/MBR pairs, CCC against EC by canopy level")
pr(P %>% filter(level %in% c("AA", "AW")) %>% group_by(Approach, level, gas) %>%
     summarise(n = sum(!is.na(CCC)), pct = round(100 * mean(CCC >= 0.5, na.rm = TRUE)), med = round(median(CCC, na.rm = TRUE), 2), .groups = "drop") %>%
     pivot_wider(names_from = gas, values_from = c(n, pct, med)))
A <- P %>% filter(Approach == "MBR", level == "AA"); p("AA MBR pairs per site: %.1f", nrow(A %>% distinct(Site, pair)) / n_distinct(P$Site))
X <- P %>% filter(Approach != "MBR", !is.na(CCC_ref), !is.na(CCC))
p("r(CCC against AA MBR, CCC against EC) = %.2f (n = %d)", cor(X$CCC_ref, X$CCC), nrow(X))
Sel <- X %>% filter(CCC_ref >= 0.5, n_ref >= 30)
p("selected AE/WP pairs %d: CO2 %d at %d sites, H2O %d at %d sites", nrow(Sel), sum(Sel$gas == "CO2"), n_distinct(Sel$Site[Sel$gas == "CO2"]),
  sum(Sel$gas == "H2O"), n_distinct(Sel$Site[Sel$gas == "H2O"]))
p("of those reliable against EC: %d (%.0f %%); others: median CCC %.2f (10th-90th %s)", sum(Sel$CCC >= 0.5), 100 * mean(Sel$CCC >= 0.5),
  median(Sel$CCC[Sel$CCC < 0.5]), paste(round(quantile(Sel$CCC[Sel$CCC < 0.5], c(.1, .9)), 2), collapse = "-"))
p("AE/WP pairs reliable against EC: %d, found by the rule: %d; selected AA: %d, AW AE H2O: %d", sum(X$CCC >= 0.5), sum(Sel$CCC >= 0.5),
  sum(Sel$level == "AA"), sum(Sel$level == "AW" & Sel$Approach == "AE" & Sel$gas == "H2O"))
p("median |scale b|: %.2f", median(abs(Sel$scale_b)))

hdr("Tables C2-C3: ensemble CCC against EC by rule (mean/median; sites >= 0.5, >= 0.3)")
for (cl in names(E)[-(1:2)]) { cat(sprintf("%-45s", cl))
  for (gg in c("CO2", "H2O")) { v <- E[E$gas == gg, cl]; v <- v[!is.na(v)]
    cat(sprintf(" %s %.2f/%.2f >=.5:%d >=.3:%d n=%d |", gg, mean(v), median(v), sum(v >= 0.5), sum(v >= 0.3), length(v))) }; cat("\n") }
un <- grep("unscaled", names(E), value = TRUE); sc <- grep(" scaled", names(E), value = TRUE); mb <- names(E)[4]
G <- E %>% filter(abs(.data[[sc]] - .data[[mb]]) > 1e-9 | abs(.data[[un]] - .data[[mb]]) > 1e-9)
p("site-gas with added pairs %d: unscaled better/worse by > 0.05: %d/%d; scaled %d/%d", nrow(G),
  sum(G[[un]] - G[[mb]] > 0.05), sum(G[[un]] - G[[mb]] < -0.05), sum(G[[sc]] - G[[mb]] > 0.05), sum(G[[sc]] - G[[mb]] < -0.05))
pr(G %>% mutate(d = .data[[sc]] - .data[[mb]]) %>% arrange(d) %>% transmute(Site, gas, AA_MBR = round(.data[[mb]], 2), scaled = round(.data[[sc]], 2), d = round(d, 2)))
hdr("pre-checks (C.4)")
pr(K %>% filter(Site %in% c("ABBY", "GUAN", "OSBS", "PUUM", "TEAK")) %>% select(Site, gas, ensemble_CCC, precheck_pass))
y <- K$ensemble_CCC >= 0.5
p("AUC: check A (other gas AA MBR CCC) %.2f, check B (max AE/WP CCC_ref) %.2f", auc(K$other_gas_AAMBR_CCC, y), auc(K$max_AEWP_CCC_ref, y))
p("RMNP CO2 AA MBR: %.2f; NIWO CO2: %.2f", E[E$Site == "RMNP" & E$gas == "CO2", mb], E[E$Site == "NIWO" & E$gas == "CO2", mb])

# ---- Part 2: from the filtered 9-min values ----
read_approach <- function(m) {
  e <- ms_load("NEON_GradientFlux_Data_Filter", paste0("SITES_", m, "_9min_FILTER", run.tag, ".RData")); L <- get(ls(e)[1], envir = e)
  x <- rbindlist(lapply(names(L), function(s) { d <- L[[s]]; if (!NROW(d)) return(NULL)
    data.table(Site = s, gas = d$gas, pair = d$dLevelsAminusB, GF = d$FG_mean, EC = d$EC_mean,
               t = as.numeric(as.POSIXct(format(d$timeEndA.local, "%Y-%m-%d %H:%M:%S"), tz = "UTC"))) }))
  x[gas %in% c("CO2", "H2O") & !is.na(GF)][, `:=`(Approach = m, t30 = floor(t / 1800))] }
P30  <- rbindlist(lapply(c("MBR", "AE", "WP"), read_approach))[, .(GF = mean(GF), EC = mean(EC, na.rm = TRUE)), by = .(Site, gas, Approach, pair, t30)]
EC30 <- P30[!is.na(EC), .(EC = mean(EC)), by = .(Site, gas, t30)]
e2 <- tryCatch(ms_load(paste0("flow.flux.counts", run.tag, ".RData")), error = function(err) ms_load("flow.flux.counts.RData"))
S  <- as.data.table(e2$SITES_One2One)[Canopy_L1 %in% c("AA", "AW")]
pairs <- S[!is.na(CCC), .(Site, gas, Approach = as.character(Approach), pair = dLevelsAminusB, CCC, level = Canopy_L1)]

hdr("AA MBR self-agreement (mean CCC between AA MBR pairs, >= 30 shared half-hours) as a pre-check")
aa <- pairs[Approach == "MBR" & level == "AA"]
selfag <- rbindlist(lapply(split(aa, by = c("Site", "gas")), function(k) {
  X <- P30[k[, .(Site, gas, Approach, pair)], on = .(Site, gas, Approach, pair), nomatch = 0]; pr_ <- unique(X$pair)
  if (length(pr_) < 2) return(data.table(Site = k$Site[1], gas = k$gas[1], self = NA_real_))
  cc <- c(); for (i in 1:(length(pr_) - 1)) for (j in (i + 1):length(pr_)) {
    m <- merge(X[pair == pr_[i], .(t30, x = GF)], X[pair == pr_[j], .(t30, y = GF)], by = "t30")
    if (nrow(m) >= 30) cc <- c(cc, lccc(m$x, m$y)) }
  data.table(Site = k$Site[1], gas = k$gas[1], self = if (length(cc)) mean(cc) else NA_real_) }))
Z <- merge(as.data.table(K)[, .(Site, gas, ensemble_CCC)], selfag, by = c("Site", "gas"))
p("AUC of AA-MBR self-agreement for ensemble CCC >= 0.5: %.2f (n = %d)", auc(Z$self, Z$ensemble_CCC >= 0.5), sum(!is.na(Z$self)))

hdr("leave-one-site-out random forest: top 3 pairs per site")
vars <- c("Approach", "RelativeDistB", "CHM.sd", "Cutoff05.SDSDH", "RelativeDistA")
M <- S[!is.na(Good.CCC), c("Site", "gas", "dLevelsAminusB", "Good.CCC", vars), with = FALSE]; M <- M[complete.cases(M[, ..vars])]
M[, Good.CCC := factor(Good.CCC)]; M[, Approach := factor(as.character(Approach))]
res <- list(); set.seed(2026)
for (s in unique(M$Site)) {
  tr <- M[Site != s]; te <- M[Site == s]; if (!nrow(te)) next
  rf <- randomForest(Good.CCC ~ ., data = as.data.frame(tr[, c("Good.CCC", vars), with = FALSE]), ntree = 300)
  te[, prob := predict(rf, as.data.frame(te[, vars, with = FALSE]), type = "prob")[, "1"]]
  for (g in c("CO2", "H2O")) { k <- te[gas == g][order(-prob)][1:min(3, .N)]
    X <- P30[k[, .(Site, gas, Approach = as.character(Approach), pair = dLevelsAminusB)], on = .(Site, gas, Approach, pair), nomatch = 0]
    Y <- X[, .(GF = mean(GF)), by = .(Site, gas, t30)][EC30, on = .(Site, gas, t30), nomatch = 0]
    if (nrow(Y) >= 30) res[[length(res) + 1]] <- data.table(Site = s, gas = g, CCC = lccc(Y$GF, Y$EC), nMBR = sum(k$Approach == "MBR")) } }
pr(rbindlist(res)[, .(median_CCC = round(median(CCC), 2), sites = .N, MBR_share = round(mean(nMBR / 3), 2)), by = gas])
