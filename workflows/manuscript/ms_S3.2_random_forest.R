# Sect. 3.2 / Sect. 2.7: random forest predicting reliable pairs (Good.CCC) from placement and structure.
# Same variables as the final model of flow.evaluation.GoodDrivers.V2.R; 80/20 split with a fixed seed so the
# reported OOB error, accuracy and no-information rate are reproducible.
# Input: flow.flux.counts<run.tag>.RData (SITES_One2One with RelativeDistA/B, CHM.sd, Cutoff05.SDSDH, Good.CCC).
if (!exists("ms_rdata")) source(file.path(DirRepo.eval, "workflows", "manuscript", "ms_setup.R"))
suppressPackageStartupMessages(library(randomForest))

e <- tryCatch(ms_load(paste0("flow.flux.counts", run.tag, ".RData")), error = function(err) ms_load("flow.flux.counts.RData"))
X <- e$SITES_One2One %>% drop_na("Good.CCC")
vars <- c("Approach", "RelativeDistB", "CHM.sd", "Cutoff05.SDSDH", "RelativeDistA")
set.seed(2026)
train <- X %>% sample_frac(0.80)
test  <- suppressMessages(anti_join(X, train))
rf <- randomForest(Good.CCC ~ ., data = train %>% select(c(Good.CCC, all_of(vars))) %>% na.omit, importance = TRUE, keep.inbag = TRUE)
print(rf); print(round(importance(rf), 1))
p("n train %d, test %d", nrow(train), nrow(test))
te <- test %>% select(c(Good.CCC, all_of(vars))) %>% na.omit
pred <- predict(rf, te); y <- factor(te$Good.CCC, levels = levels(pred))
tab <- table(pred = pred, ref = y); print(tab)
acc <- mean(pred == y); nir <- max(table(y)) / length(y)
p("test accuracy %.1f %%, no-information rate %.1f %%, p (acc > NIR) = %.3g", 100 * acc, 100 * nir,
  binom.test(sum(pred == y), length(y), nir, alternative = "greater")$p.value)
p("precision: not reliable %.2f, reliable %.2f; recall reliable %.0f %%",
  tab["0", "0"] / sum(tab["0", ]), tab["1", "1"] / sum(tab["1", ]), 100 * tab["1", "1"] / sum(tab[, "1"]))
