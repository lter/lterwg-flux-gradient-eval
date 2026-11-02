# ms_setup.R -------------------------------------------------------------------------------------------
# Shared settings for the manuscript-number scripts (essd-2026-413, "Evaluating gradient fluxes at NEON").
# These scripts recompute the in-text numbers that are not written to a table or figure by the batch:
# they only read outputs of flow.evaluation.batch_V1.R (and the EC-filter diagnostic), never recompute fluxes.
# Set localdir, run.tag and DirRepo.eval before sourcing (flow.manuscript.numbers.R does this), or use the defaults.

suppressPackageStartupMessages({ library(dplyr); library(tidyr) })

if (!exists("localdir"))     localdir     <- "/Volumes/MaloneLab/Research/FluxGradient"
if (!exists("run.tag"))      run.tag      <- "_AA_AW_2021_2026_v3b"
if (!exists("DirRepo.eval")) DirRepo.eval <- getwd()
if (!exists("figdir"))       figdir       <- file.path(DirRepo.eval, "Figures", "WF_Version1")
# Folder written by workflows/flow.ec.filter.selection.R for this run (hpc/bouchet_v3b/ec_filter_config_v3b.R)
if (!exists("ec.filter.dir")) ec.filter.dir <- file.path(localdir, paste0("EC_filter_selection", sub("^_AA_AW_2021_2026", "", run.tag)))

# Find an .RData file whatever the capitalisation of its extension (.RData, .Rdata, .RDATA).
ms_rdata <- function(...) {
  stem <- sub("\\.R[Dd][Aa][Tt][Aa]$", "", file.path(localdir, ...))
  for (ext in c(".RData", ".Rdata", ".RDATA")) if (file.exists(paste0(stem, ext))) return(paste0(stem, ext))
  stop("Missing input: ", stem, ".RData")
}
ms_load <- function(...) { e <- new.env(); load(ms_rdata(...), envir = e); e }
# CSV written by a batch script to Figures/WF_Version1 (or to a folder in localdir)
ms_fig_csv  <- function(name) { f <- file.path(figdir, paste0(name, run.tag, ".csv")); if (!file.exists(f)) stop("Missing input: ", f); read.csv(f, check.names = FALSE) }
ms_data_csv <- function(folder, name) { f <- file.path(localdir, folder, paste0(name, run.tag, ".csv")); if (!file.exists(f)) stop("Missing input: ", f); read.csv(f, check.names = FALSE) }

p   <- function(...) cat(sprintf(...), "\n")
hdr <- function(x) cat("\n----", x, "----\n")
pr  <- function(x) print(as.data.frame(x), row.names = FALSE)
r2  <- function(x, y) round(cor(x, y, use = "complete.obs"), 2)

# Site canopy height used in the manuscript: NEON AOP lidar CHM mean in the tower area, NEON site value where
# there is no lidar summary (DCFS, KONA, LAJA, TREE). Same as CanopyHeight in the evaluation tables.
site_hc <- function(df) dplyr::coalesce(df$CHM.mean, df$canopyHeight_m)

# Ecosystem types as in flow.attr.map.R / flow.results.ecotype.R
ms_ecotype <- function() {
  meta <- read.csv(file.path(localdir, "Ameriflux_NEON field-sites.csv"))
  meta %>% transmute(Site = Site_Id.NEON, EcoType = case_when(
    Vegetation.Abbreviation..IGBP. %in% c("ENF", "DBF", "MF", "EBF", "SAV") ~ "Forest",
    Vegetation.Abbreviation..IGBP. == "WET" ~ "Wetland",
    Vegetation.Abbreviation..IGBP. == "GRA" ~ "Grassland",
    Vegetation.Abbreviation..IGBP. %in% c("CVM", "CRO") ~ "Cropland",
    Vegetation.Abbreviation..IGBP. == "OSH" ~ "Shrubland"))
}

# Lin's concordance correlation coefficient (population form, as in calc.lins.ccc.R)
lccc <- function(x, y) { mx <- mean(x); my <- mean(y)
  2 * mean((x - mx) * (y - my)) / (mean((x - mx)^2) + mean((y - my)^2) + (mx - my)^2) }
# AUC from ranks (Mann-Whitney)
auc <- function(score, y) { ok <- !is.na(score) & !is.na(y); s <- score[ok]; y <- y[ok]
  r <- rank(s); n1 <- sum(y); n0 <- sum(!y); (sum(r[y]) - n1 * (n1 + 1) / 2) / (n1 * n0) }
