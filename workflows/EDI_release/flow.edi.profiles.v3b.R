## ------------------------------------------------------------------ ##
##  EDI v3b release: monthly day/night concentration profiles
## ------------------------------------------------------------------ ##
# Script: flow.edi.profiles.v3b.R
# Builds Site_ConcentrationProfiles.csv in the v3b EDI folder: for each site, gas
# (CO2, H2O, CH4), calendar month (local time, pooled over 2021-2026), time of day
# (day 07:00-18:59 / night, local time) and NEON tower inlet level, the mean, SD and
# median of the 6-min inlet concentrations, with the inlet height and its height
# relative to the lidar canopy height hc (Site_Canopy_Attributes.csv).
#
# Source: the {SITE}_AE_9min.df.final.csv files of the release. Every 6-min inlet
# sample appears there as level A or B of one or more sampling-height pairs; each
# sample is counted once (unique gas x level x sample start/end). Samples with a NEON
# final quality flag (qfFinl = 1) or a missing mean are excluded. Local time comes
# from the files' own UTC (timeEnd_A) and local (timeEndA.local) times.
#
# Usage (Bouchet; requires data.table):
#   Rscript flow.edi.profiles.v3b.R site <SITE>   # one site -> EDI_v3b_scripts/profiles_tmp/<SITE>.csv
#   Rscript flow.edi.profiles.v3b.R combine       # all sites -> <EDI folder>/Site_ConcentrationProfiles.csv
#   sbatch --array=1-47 EDI_v3b_scripts/run_profiles_v3b.sh, then the combine step
# Run flow.edi.metadata.v3b.R first (it writes Site_Canopy_Attributes.csv) and again
# afterwards (it checks the profile table against its column descriptions).
# Output is deterministic: the same inputs give byte-identical files.

suppressMessages(library(data.table))
options(stringsAsFactors = FALSE)

root <- Sys.getenv("FG_ROOT", "")
if (root == "") {
  cand <- c("/nfs/roberts/project/pi_sm3466/sm3466/FluxGradient",
            "/Volumes/MaloneLab/Research/FluxGradient")
  root <- cand[dir.exists(cand)][1]
}
if (is.na(root) || !dir.exists(root)) stop("FluxGradient root not found; set FG_ROOT")
edi.dir <- file.path(root, Sys.getenv("FG_EDI_DIR", "NEON_GradientFlux_EDI_2021_2026_v3b"))
tmp.dir <- file.path(root, "EDI_v3b_scripts", "profiles_tmp")

args <- commandArgs(trailingOnly = TRUE)
mode <- if (length(args)) args[1] else "combine"

day.start <- 7; day.end <- 18   # day = 07:00-18:59 local, as in the manuscript (Sect. 2.5)

site_profiles <- function(site) {
  f <- file.path(edi.dir, site, paste0(site, "_AE_9min.df.final.csv"))
  if (!file.exists(f)) stop("missing ", f)
  side <- c("TowerPosition", "TowerHeight", "timeBgn", "timeEnd", "mean", "qfFinl")
  cols <- c("gas", "timeEndA.local", paste0(side, "_A"), paste0(side, "_B"))
  d <- fread(f, select = cols, colClasses = "character", showProgress = FALSE)
  # site UTC offset per row (handles daylight saving), rounded to 15 min
  utc <- as.POSIXct(d$timeEnd_A, tz = "UTC", format = "%Y-%m-%d %H:%M:%OS")
  loc <- as.POSIXct(d[["timeEndA.local"]], tz = "UTC", format = "%Y-%m-%d %H:%M:%OS")
  d[, off := round(as.numeric(loc - utc, units = "secs") / 900) * 900]
  long <- rbind(
    d[, .(gas, pos = TowerPosition_A, z = TowerHeight_A, tb = timeBgn_A, te = timeEnd_A,
          conc = mean_A, qf = qfFinl_A, off)],
    d[, .(gas, pos = TowerPosition_B, z = TowerHeight_B, tb = timeBgn_B, te = timeEnd_B,
          conc = mean_B, qf = qfFinl_B, off)])
  rm(d); gc()
  long <- long[qf == "0" & conc != "NA" & conc != "" & !is.na(off)]
  long <- unique(long, by = c("gas", "pos", "tb", "te"))
  long[, `:=`(conc = as.numeric(conc), z = as.numeric(z), pos = as.integer(pos))]
  tb <- as.POSIXct(long$tb, tz = "UTC", format = "%Y-%m-%d %H:%M:%OS")
  te <- as.POSIXct(long$te, tz = "UTC", format = "%Y-%m-%d %H:%M:%OS")
  mid <- tb + (te - tb) / 2 + long$off
  long[, `:=`(month = as.integer(format(mid, "%m", tz = "UTC")),
              year = as.integer(format(mid, "%Y", tz = "UTC")),
              hour = as.integer(format(mid, "%H", tz = "UTC")))]
  long <- long[!is.na(month) & !is.na(conc)]
  long[, tod := ifelse(hour >= day.start & hour <= day.end, "day", "night")]
  out <- long[, .(n_samples = .N, n_years = uniqueN(year),
                  conc_mean = mean(conc), conc_sd = sd(conc), conc_median = median(conc)),
              by = .(gas, month, tod, TowerPosition = pos, z_m = z)]
  setorder(out, gas, month, tod, z_m, TowerPosition)
  out[, Site := site]
  setcolorder(out, "Site")
  out
}

if (mode == "site") {
  site <- args[2]
  dir.create(tmp.dir, showWarnings = FALSE, recursive = TRUE)
  out <- site_profiles(site)
  fwrite(out, file.path(tmp.dir, paste0(site, ".csv")))
  message(site, ": ", nrow(out), " profile rows; gases ", paste(unique(out$gas), collapse = " "))
} else if (mode == "combine") {
  sites <- sort(basename(list.dirs(edi.dir, recursive = FALSE)))
  sites <- sites[grepl("^[A-Z]{4}$", sites)]
  fs <- file.path(tmp.dir, paste0(sites, ".csv"))
  if (!all(file.exists(fs))) stop("missing site outputs: ", paste(sites[!file.exists(fs)], collapse = " "))
  p <- rbindlist(lapply(fs, fread, colClasses = list(character = c("Site", "gas", "tod"))))
  sa <- fread(file.path(edi.dir, "Site_Canopy_Attributes.csv"), select = c("Site", "hc_m"))
  p <- merge(p, sa, by = "Site", all.x = TRUE, sort = FALSE)
  if (anyNA(p$hc_m)) stop("hc missing for some sites")
  p[, z_minus_hc_m := round(z_m - hc_m, 4)]
  p[, `:=`(conc_mean = signif(conc_mean, 7), conc_sd = signif(conc_sd, 5),
           conc_median = signif(conc_median, 7))]
  p[, units := c(CO2 = "umol mol-1", H2O = "mmol mol-1", CH4 = "umol mol-1")[gas]]
  setcolorder(p, c("Site", "gas", "units", "month", "tod", "TowerPosition", "z_m", "hc_m",
                   "z_minus_hc_m", "n_samples", "n_years", "conc_mean", "conc_sd", "conc_median"))
  setorder(p, Site, gas, month, tod, z_m, TowerPosition)
  out <- file.path(edi.dir, "Site_ConcentrationProfiles.csv")
  write.csv(as.data.frame(p), out, row.names = FALSE, na = "NA", fileEncoding = "UTF-8")
  message("wrote Site_ConcentrationProfiles.csv (", nrow(p), " rows, ", uniqueN(p$Site), " sites)")
  print(p[, .(sites = uniqueN(Site), rows = .N, conc_mean_min = min(conc_mean), conc_mean_max = max(conc_mean)), by = gas])
  cat("md5", unname(tools::md5sum(out)), "\n")
} else stop("mode must be 'site' or 'combine'")
