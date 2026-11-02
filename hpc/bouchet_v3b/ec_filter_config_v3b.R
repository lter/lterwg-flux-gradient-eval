localdir <- "/nfs/roberts/project/pi_sm3466/sm3466/FluxGradient"
eval.data.dirs <- "NEON_GradientFlux_Data_2021_2026_v3b"
meta <- utils::read.csv(file.path(localdir, "Ameriflux_NEON field-sites.csv"), check.names = FALSE)
have <- list.dirs(file.path(localdir, eval.data.dirs), full.names = FALSE, recursive = FALSE)
meta <- meta[meta[["Site_Id NEON"]] %in% have, ]
site.timezones <- stats::setNames(lutz::tz_lookup_coords(meta[["Latitude (degrees)"]], meta[["Longitude (degrees)"]],
                                  method = "fast", warn = FALSE), meta[["Site_Id NEON"]])
canopy.file <- file.path(localdir, "canopy_commbined.csv")
output.dir <- file.path(localdir, "EC_filter_selection_v3b")
thresholds <- c(2, 3, 4)
min_per_hour <- 5L
