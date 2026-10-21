if (!exists("run.tag")) run.tag <- "_AA_AW" # output-name tag; set in flow.evaluation.batch_V1.R
# Filter Gradient Flux Data:

library(lutz)
library(sf)

source(fs::path(DirRepo.eval,'functions/calc.filter_FG.R' ))
source(fs::path(DirRepo.eval,'functions/calc.SITELIST_FORMATTING.R'))

for( site in site.list){
  
  print( site)
  
  # Canopy information:
  canopy.sub <- read.csv(file.path(paste(localdir, "canopy_commbined.csv", sep="/"))) %>% 
    distinct %>% mutate(RelativeDistB = MeasurementHeight_m_B - CanopyHeight, 
                        RelativeDistA = MeasurementHeight_m_A - CanopyHeight, 
                        MeasurementDist = MeasurementHeight_m_A - MeasurementHeight_m_B) %>% 
    filter(Site == site, Canopy_L1 != "WW") %>% select(Site, Canopy_L1, dLevelsAminusB)
  
  
  # Load the files (one SITE_Evaluation.Rdata per data period in eval.data.dirs; stacked):
  if (!exists("eval.data.dirs")) eval.data.dirs <- "NEON_GradientFlux_Data"
  period.list <- list(MBR = list(), AE = list(), WP = list())
  for (period.dir in eval.data.dirs) {
    localdir.site <- paste(localdir, "/", period.dir, "/", site, sep = "")
    files <- paste(site, "_Evaluation.Rdata", sep = "")
    if (!file.exists(paste(localdir.site, "/", files, sep = ""))) {
      message("No evaluation file for ", site, " in ", period.dir); next }
    load(paste(localdir.site, "/", files, sep = ""))
    period.list$MBR[[period.dir]] <- MBR_9min.df.final
    period.list$AE[[period.dir]]  <- AE_9min.df.final
    period.list$WP[[period.dir]]  <- WP_9min.df.final
  }
  MBR_9min.df.final <- bind_rows(period.list$MBR)
  AE_9min.df.final  <- bind_rows(period.list$AE)
  WP_9min.df.final  <- bind_rows(period.list$WP)
  rm(period.list)
  
  # Add the canopy information to the file:
  MBR_9min.df.final <- MBR_9min.df.final  %>% 
    full_join( canopy.sub , by = c('dLevelsAminusB'), relationship = "many-to-many") %>% filter(Canopy_L1 != "WW")
  AE_9min.df.final <- AE_9min.df.final  %>% 
    full_join( canopy.sub , by = c('dLevelsAminusB'), relationship = "many-to-many") %>% filter(Canopy_L1 != "WW")
  WP_9min.df.final <- WP_9min.df.final  %>% 
    full_join( canopy.sub , by = c('dLevelsAminusB'), relationship = "many-to-many") %>% filter(Canopy_L1 != "WW")
  
  # Change the time to local:
  
  # Get NEON sites from the server and find the time zones: https://cran.r-project.org/web/packages/lutz/readme/README.html
  sites.location <- metadata %>%  st_as_sf(coords = c("Longitude..degrees.", "Latitude..degrees."),
                                           crs = "+proj=longlat +datum=WGS84")
  
  sites.location$TZ <- tz_lookup(sites.location, method = "accurate")
  sites.location.sub <- sites.location %>%  select( "Site_Id.NEON" , "TZ")
  
  site.tz <- sites.location.sub$TZ[which( sites.location.sub$Site_Id.NEON == site)]
  
  MBR_9min.df.final$timeEndA.local <- MBR_9min.df.final$timeEndA %>%  as.POSIXlt( tz = site.tz)
  AE_9min.df.final$timeEndA.local <- AE_9min.df.final$timeEnd_A %>%  as.POSIXlt( tz = site.tz)
  WP_9min.df.final$timeEndA.local <- WP_9min.df.final$timeEnd_A %>%  as.POSIXlt( tz = site.tz)
  
  MBR_9min.df.final$Month.local <- MBR_9min.df.final$timeEndA.local %>% format("%m")
  AE_9min.df.final$Month.local  <- AE_9min.df.final$timeEndA.local %>% format("%m")
  WP_9min.df.final$Month.local  <- WP_9min.df.final$timeEndA.local %>% format("%m")
  
  MBR_9min.df.final$time.local <- MBR_9min.df.final$timeEndA.local %>% format("%H:%M")
  AE_9min.df.final$time.local <- AE_9min.df.final$timeEndA.local %>% format("%H:%M")
  WP_9min.df.final$time.local <- WP_9min.df.final$timeEndA.local %>% format("%H:%M")
  
  MBR_9min.df.final$hour.local <- MBR_9min.df.final$timeEndA.local %>% format("%H")
  AE_9min.df.final$hour.local <- AE_9min.df.final$timeEndA.local %>% format("%H")
  WP_9min.df.final$hour.local <- WP_9min.df.final$timeEndA.local %>% format("%H")
  
  # Run the filter functions... Report:
  # CO2
  
  WP_9min.report.CO2 <-  filter_report(df = WP_9min.df.final %>% filter(gas == "CO2"),
                                       dConcSNR.min = 3,
                                       approach = "WP") %>% mutate(approach = "WP",
                                                                   site =site)
  
  AE_9min.report.CO2  <-  filter_report( df = AE_9min.df.final %>% filter(gas == "CO2"),
                                         dConcSNR.min = 3,
                                         approach = "AE") %>% mutate(approach = "AE",
                                                                     site =site)
  
  
  MBR_9min.report.CO2  <-  filter_report( df = MBR_9min.df.final %>% filter(gas == "CO2"),
                                          dConcSNR.min = 3,
                                          approach = "MBR") %>% mutate(approach = "MBR",
                                                                       site =site)
  
  
  WP_9min.report.stability.CO2 <-  filter_report_stability(df = WP_9min.df.final %>% filter(gas == "CO2"),
                                                           dConcSNR.min = 3,
                                                           approach = "WP") %>% mutate(approach = "WP",
                                                                                       site =site)
  
  AE_9min.report.stability.CO2 <-  filter_report_stability( df = AE_9min.df.final%>% filter(gas == "CO2"),
                                                            dConcSNR.min = 3,
                                                            approach = "AE") %>% mutate(approach = "AE",
                                                                                        site =site)
  
  MBR_9min.report.stability.CO2 <-  filter_report_stability( df = MBR_9min.df.final%>% filter(gas == "CO2"),
                                                             dConcSNR.min = 3,
                                                             approach = "MBR") %>% mutate(approach = "MBR",
                                                                                          site =site)
  
  SITE_9min.report.CO2 <- rbind( WP_9min.report.CO2, AE_9min.report.CO2, MBR_9min.report.CO2)
  
  SITE_9min.report.stability.CO2 <- rbind( WP_9min.report.stability.CO2, AE_9min.report.stability.CO2, MBR_9min.report.stability.CO2)
  
  # H2O
  
  WP_9min.report.H2O <-  filter_report(df = WP_9min.df.final %>% filter(gas == "H2O"),
                                       dConcSNR.min = 3,
                                       approach = "WP") %>% mutate(approach = "WP",
                                                                   site =site)
  
  AE_9min.report.H2O  <-  filter_report( df = AE_9min.df.final %>% filter(gas == "H2O"),
                                         dConcSNR.min = 3,
                                         approach = "AE") %>% mutate(approach = "AE",
                                                                     site =site)
  
  MBR_9min.report.H2O  <-  filter_report( df = MBR_9min.df.final %>% filter(gas == "H2O"),
                                          dConcSNR.min = 3,
                                          approach = "MBR") %>% mutate(approach = "MBR",
                                                                       site =site)
  
  
  WP_9min.report.stability.H2O <-  filter_report_stability(df = WP_9min.df.final%>% filter(gas == "H2O"),
                                                           dConcSNR.min = 3,
                                                           approach = "WP") %>% mutate(approach = "WP",
                                                                                       site =site)
  
  AE_9min.report.stability.H2O <-  filter_report_stability( df = AE_9min.df.final%>% filter(gas == "H2O"),
                                                            dConcSNR.min = 3,
                                                            approach = "AE") %>% mutate(approach = "AE",
                                                                                        site =site)
  
  MBR_9min.report.stability.H2O <-  filter_report_stability( df = MBR_9min.df.final%>% filter(gas == "H2O"),
                                                             dConcSNR.min = 3,
                                                             approach = "MBR") %>% mutate(approach = "MBR",
                                                                                          site =site)
  
  SITE_9min.report.H2O <- rbind( WP_9min.report.H2O, AE_9min.report.H2O, MBR_9min.report.H2O)
  SITE_9min.report.stability.H2O <- rbind( WP_9min.report.stability.H2O, AE_9min.report.stability.H2O, MBR_9min.report.stability.H2O)
  
  
  # Run the filter functions... FILTER data:
  MBR_9min_FILTER <- filter_fluxes( df = MBR_9min.df.final,
                                    dConcSNR.min = 3,
                                    approach = "MBR") %>% 
    mutate( EC_mean = case_when(gas == "CO2" ~   FC_turb_interp,
                                gas == "H2O" ~ FH2O_interp,
                                gas == "CH4"~ NA))
  
  AE_9min_FILTER <- filter_fluxes( df = AE_9min.df.final,
                                   dConcSNR.min = 3,
                                   approach = "AE")%>% 
    mutate( EC_mean = case_when(gas == "CO2" ~   FC_turb_interp,
                                gas == "H2O" ~ FH2O_interp,
                                gas == "CH4"~ NA))
  
  WP_9min_FILTER <- filter_fluxes ( df = WP_9min.df.final,
                                    dConcSNR.min = 3,
                                    approach = "WP")%>% 
    mutate( EC_mean = case_when(gas == "CO2" ~   FC_turb_interp,
                                gas == "H2O" ~ FH2O_interp,
                                gas == "CH4"~ NA))
  
  
  # Gradient-flux spike removal (GF only; EC is not used). Drops FG_mean outside
  # median +/- despike.k * MAD within gas x sampling-height pair x day/night x season,
  # computed over all periods loaded above. Set despike.k <- NULL in the batch file to skip.
  if (!exists("despike.k")) despike.k <- NULL
  if (!is.null(despike.k)) {
    MBR_9min_FILTER <- despike_fluxes(MBR_9min_FILTER, k = despike.k)
    AE_9min_FILTER  <- despike_fluxes(AE_9min_FILTER,  k = despike.k)
    WP_9min_FILTER  <- despike_fluxes(WP_9min_FILTER,  k = despike.k)
  }
  
  # Output the files
  rm(localdir.site)
  
  localdir.site <- paste(localdir, "/NEON_GradientFlux_Data_Filter/", site, sep="" )
  
  dir.create(localdir.site)
  
  write.csv( SITE_9min.report.stability.CO2,  paste(localdir.site, "/", site,paste0("_9min.report.stability.CO2", run.tag, ".csv"), sep=""))
  write.csv( SITE_9min.report.CO2,  paste(localdir.site, "/", site,paste0("_9min.report.CO2", run.tag, ".csv"), sep=""))
  
  write.csv( SITE_9min.report.stability.H2O,  paste(localdir.site, "/", site,paste0("_9min.report.stability.H2O", run.tag, ".csv"), sep=""))
  write.csv( SITE_9min.report.H2O,  paste(localdir.site, "/", site,paste0("_9min.report.H2O", run.tag, ".csv"), sep=""))
  
  
  save( AE_9min_FILTER,
        WP_9min_FILTER,
        MBR_9min_FILTER, file = paste(localdir.site, "/", site, paste0("_FILTER", run.tag, ".Rdata"), sep=""))
  
  
  # Upload files to the google
  
  site_folder <-data_folder$id[data_folder$name==site]
  
  fileSave <- paste(localdir.site, "/", site, paste0("_FILTER", run.tag, ".Rdata"), sep="")
  if (!exists("upload.drive") || isTRUE(upload.drive)) googledrive::drive_upload(media = fileSave, overwrite = T, path = site_folder)
  
  fileSave <- paste(localdir.site, "/", site,paste0("_9min.report.CO2", run.tag, ".csv"), sep="")
  if (!exists("upload.drive") || isTRUE(upload.drive)) googledrive::drive_upload(media = fileSave, overwrite = T, path =site_folder)
  
  fileSave <- paste(localdir.site, "/", site,paste0("_9min.report.stability.CO2", run.tag, ".csv"), sep="")
  if (!exists("upload.drive") || isTRUE(upload.drive)) googledrive::drive_upload(media = fileSave, overwrite = T, path =site_folder)
  
  fileSave <- paste(localdir.site, "/", site,paste0("_9min.report.H2O", run.tag, ".csv"), sep="")
  if (!exists("upload.drive") || isTRUE(upload.drive)) googledrive::drive_upload(media = fileSave, overwrite = T, path =site_folder)
  
  fileSave <- paste(localdir.site, "/", site,paste0("_9min.report.stability.H2O", run.tag, ".csv"), sep="")
  if (!exists("upload.drive") || isTRUE(upload.drive)) googledrive::drive_upload(media = fileSave, overwrite = T, path =site_folder)
  
  message( paste("Done with filtering", site))
  
  rm( AE_9min_FILTER,
      WP_9min_FILTER,
      MBR_9min_FILTER,
      SITE_9min.report.CO2, 
      WP_9min.report.CO2, 
      AE_9min.report.CO2, 
      MBR_9min.report.CO2,
      SITE_9min.report.stability.CO2, 
      WP_9min.report.stability.CO2, 
      AE_9min.report.stability.CO2, 
      MBR_9min.report.stability.CO2,
      SITE_9min.report.H2O, 
      WP_9min.report.H2O, 
      AE_9min.report.H2O, 
      MBR_9min.report.H2O,
      SITE_9min.report.stability.H2O, 
      WP_9min.report.stability.H2O, 
      AE_9min.report.stability.H2O, 
      MBR_9min.report.stability.H2O,
      SITE_9min.report.H2O,
      sample.diel, sample.month)
}