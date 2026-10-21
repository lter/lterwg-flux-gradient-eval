# Run from repo root: Rscript workflows/flow.ec.filter.selection.R config.R
suppressPackageStartupMessages(library(dplyr))
suppressPackageStartupMessages(library(tidyr))
suppressPackageStartupMessages(library(lubridate))
source("functions/calc.ec.filter.selection.R")
args <- commandArgs(trailingOnly = TRUE)
if (length(args) != 1L) stop("Supply one config.R file; see docs/ec-filter-selection.md")
cfg <- new.env(parent = baseenv())
sys.source(args[1], envir = cfg)
for (x in c("localdir", "eval.data.dirs", "site.timezones", "canopy.file", "output.dir"))
  if (!exists(x, cfg, inherits = FALSE)) stop("Missing config: ", x)
thresholds <- if (exists("thresholds", cfg)) cfg$thresholds else c(2, 3, 4)
min_n <- if (exists("min_per_hour", cfg)) cfg$min_per_hour else 5L
canopy <- read.csv(cfg$canopy.file)
reference <- if (exists("ec.reference.rds", cfg)) readRDS(cfg$ec.reference.rds) else NULL
ensemble <- if (exists("ensemble.times.rds", cfg)) readRDS(cfg$ensemble.times.rds) else NULL
dir.create(cfg$output.dir, recursive = TRUE, showWarnings = FALSE)
for (site in names(cfg$site.timezones)) {
  parts <- list()
  for (period in cfg$eval.data.dirs) {
    folder <- file.path(cfg$localdir, period, site)
    candidates <- file.path(folder, paste0(site, c("_Evaluation.Rdata", "_Evaluation.RDATA")))
    path <- candidates[file.exists(candidates)]
    if (!length(path)) stop("Missing evaluation file: ", folder)
    env <- new.env()
    load(path[1], envir = env)
    for (method in c("MBR", "AE", "WP")) {
      x <- get(paste0(method, "_9min.df.final"), envir = env)
      x$Approach <- method
      time_col <- if (method == "MBR") "timeEndA" else "timeEnd_A"
      if (!inherits(x[[time_col]], "POSIXt")) stop("Evaluation times must be POSIXt.")
      x$timeEndA.local <- as.POSIXct(x[[time_col]], tz = cfg$site.timezones[[site]])
      parts[[length(parts) + 1L]] <- x
    }
  }
  # Match production AA/AW eligibility without creating duplicate rows.
  eligible <- canopy %>% filter(Site == site, Canopy_L1 %in% c("AA", "AW")) %>%
    distinct(dLevelsAminusB)
  input <- bind_rows(parts) %>% semi_join(eligible, by = "dLevelsAminusB") %>% distinct()
  ref <- if (is.null(reference)) NULL else reference %>% filter(Site == site)
  ens <- if (is.null(ensemble)) NULL else ensemble %>% filter(Site == site)
  ans <- ec_filter_selection(input, thresholds, min_n, ref, ens)
  for (name in names(ans)) {
    ans[[name]]$Site <- rep(site, nrow(ans[[name]]))
    write.csv(ans[[name]], file.path(cfg$output.dir, paste0(site, "_", name, ".csv")),
              row.names = FALSE, na = "NA")
  }
}

