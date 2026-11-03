# Adds the half-hourly storage fluxes (CO2, H2O, CH4) to the EDI v3b release.
# Merges the two storage runs into one 2021-2026 file per site:
#   NEON_Storage_Flux/{SITE}_storage_flux.csv               (Aug 2021 - Jun 2024, made 23 Jun 2026)
#   NEON_Storage_Flux_202407_202608/{SITE}_storage_flux.csv (Jul 2024 - Aug 2026, made 8 Oct 2026)
# Both runs have identical columns and methods. Where they overlap (the 1 Jul 2024 00:00 half-hour of CO2 and
# H2O), the row from the newer run is kept. Rows are grouped by gas, then in time order.
# CH4 unit labels: both runs predate the 21 Oct 2026 fix in flow.neon.storage.R, so their CH4 rows say "nmol"
# (nmol m-3, nmol m-2, nmol m-2 s-1) although the values are umol (NEON reports CH4 in umol mol-1). The label
# fields are corrected here ("nmol m-" -> "umol m-"); no values change. Only CH4 rows carry nmol labels.
# Output: NEON_GradientFlux_EDI_2021_2026_v3b/{SITE}/{SITE}_STORAGE_FLUX_30min.csv, and
#         EDI_v3b_scripts/storage_merge_v3b_summary.txt (row counts and md5 per site).
# Run from anywhere: Rscript flow.edi.storage.v3b.R  (set FG_ROOT if the server is not at /Volumes/MaloneLab/...).
# Re-running skips sites already written (delete a site's line in the summary to redo it). ~1-2 min per site.
root <- Sys.getenv("FG_ROOT", "/Volumes/MaloneLab/Research/FluxGradient")
dirA <- file.path(root, "NEON_Storage_Flux")
dirB <- file.path(root, "NEON_Storage_Flux_202407_202608")
out.dir <- file.path(root, "NEON_GradientFlux_EDI_2021_2026_v3b")
summ <- file.path(root, "EDI_v3b_scripts", "storage_merge_v3b_summary.txt")
done <- if (file.exists(summ)) sub("\t.*", "", readLines(summ)) else character()
sites <- sort(substr(list.files(dirB, "_storage_flux\\.csv$"), 1, 4))
field <- function(x, i) gsub('"', "", vapply(strsplit(x, ",", fixed = TRUE), `[`, "", i))
for (site in setdiff(sites, done)) {
  t0 <- Sys.time()
  a <- readLines(file.path(dirA, paste0(site, "_storage_flux.csv")))
  b <- readLines(file.path(dirB, paste0(site, "_storage_flux.csv")))
  stopifnot(identical(a[1], b[1]))
  ga <- field(a[-1], 2); ta <- field(a[-1], 3); gb <- field(b[-1], 2); tb <- field(b[-1], 3)
  bmin <- tapply(tb, gb, min)
  drop <- !is.na(bmin[ga]) & ta >= bmin[ga]
  gases <- sort(unique(c(ga, gb)))
  body <- unlist(lapply(gases, function(g) c(a[-1][ga == g & !drop], b[-1][gb == g])), use.names = FALSE)
  body <- gsub('"nmol m-', '"umol m-', body, fixed = TRUE)  # CH4 unit labels (values were always umol)
  stopifnot(length(body) == length(a) - 1 + length(b) - 1 - sum(drop))
  out <- file.path(out.dir, site, paste0(site, "_STORAGE_FLUX_30min.csv"))
  tmp <- paste0(out, ".tmp")
  writeLines(c(a[1], body), tmp)
  stopifnot(file.rename(tmp, out))
  md5 <- unname(tools::md5sum(out))
  cat(sprintf("%s\trows_2021_2024=%d\trows_2024_2026=%d\toverlap_dropped=%d\trows_out=%d\tfirst=%s\tlast=%s\tmd5=%s\n",
              site, length(a) - 1, length(b) - 1, sum(drop), length(body), min(c(ta, tb)), max(c(ta, tb)), md5),
      file = summ, append = TRUE)
  message(site, " done (", round(difftime(Sys.time(), t0, units = "secs")), " s)")
}
message("Sites written: ", length(readLines(summ)), " of ", length(sites))
