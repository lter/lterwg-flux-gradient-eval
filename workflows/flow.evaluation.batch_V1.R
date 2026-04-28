# flow.evaluation:
## This Evaluation follows the workflow for CCC_EC:

# The Evaluation dataframe is made in "lterwg-flux-gradient-eval" by the file flow.evaluation_dataframe

rm(list=ls())

library(tidyverse)
library(ggplot2)
library(ggpubr)
library(sf)

# -------------- Change this stuff -------------
#DirRepo <- 'C:/Users/csturtevant/Documents/Git/lterwg-flux-gradient' # Relative or absolute path to lterwg-flux-gradient git repo on your local machine. Make sure you've pulled the latest from main!
DirRepo.eval <-"/Users/sm3466/YSE Dropbox/Sparkle Malone/Research/FluxGradient/lterwg-flux-gradient-eval"
setwd(DirRepo.eval)

#localdir <- 'C:/Users/csturtevant/OneDrive - Battelle Ecology/FluxGradient/filterTesting' # We'll deposit output files here prior to uploading to Google Drive

localdir <- '/Volumes/MaloneLab/Research/FluxGradient/FluxData'
DnldFromGoogleDrive <- FALSE # Enter TRUE to grab files listed in dnld_files from Google Drive. Enter FALSE if you have the most up-to-date versions locally in localdir

email <- 'sparklelmalone@gmail.com'
googledrive::drive_auth(email = TRUE) 
drive_url <- googledrive::as_id("https://drive.google.com/drive/folders/1Q99CT77DnqMl2mrUtuikcY47BFpckKw3") # The Data 
data_folder <- googledrive::drive_ls(path = drive_url)

metadata <- read.csv('/Volumes/MaloneLab/Research/FluxGradient/Ameriflux_NEON field-sites.csv') # has a list of all the sites

# -------------------------------------------------------
site.list <- metadata$Site_Id.NEON %>% unique
# ---------------------------------------------

# Download necessary files from Google Drive
# Download files from lter-flux-gradient Google Earth:
source(fs::path(DirRepo.eval,'workflows/WF_Version1/flow.evaluation.download.R'))
# ---------------------------------------------
# Evaluation of SNR Threshold: (ETA 1 Hour) ####
SNR.plot.dir <- '/Volumes/MaloneLab/Research/FluxGradient/SNR_plot' # Where do you want to save the plots
SNR.summary.dir <-"/Volumes/MaloneLab/Research/FluxGradient/SNR_Summary/" # Where to save the summary file
source(fs::path(DirRepo.eval,'workflows/flow.evaluation.SNR.R'))

# Application of Filter Functions: ####
message('Running Filter...')
source(fs::path(DirRepo.eval,'workflows/flow.evaluation.filter.R')) # 20 minutes
# ---------------------------------------------
# Compiles Dataframes into one list: ####
source(fs::path(DirRepo.eval,'workflows/flow.evaluation.sitelist_FILTER.R'))
# 4 Objects are created
fileSave <- fs::path(localdir,paste0("SITE_DATA_FILTERED_AA_AW.Rdata"))
save( SITE_DATA_FILTERED,file=fileSave)
googledrive::drive_upload(media = fileSave, overwrite = T, path = drive_url)

fileSave <- fs::path(localdir,paste0("SITES_MBR_9min_FILTER_AA_AW.Rdata"))
save( SITES_MBR_9min_FILTER,file=fileSave)
googledrive::drive_upload(media = fileSave, overwrite = T, path = drive_url)

fileSave <- fs::path(localdir,paste0("SITES_AE_9min_FILTER_AA_AW.Rdata"))
save( SITES_AE_9min_FILTER ,file=fileSave)
googledrive::drive_upload(media = fileSave, overwrite = T, path = drive_url)

fileSave <- fs::path(localdir,paste0("SITES_WP_9min_FILTER_AA_AW.Rdata"))
save( SITES_WP_9min_FILTER,file=fileSave)
googledrive::drive_upload(media = fileSave, overwrite = T, path = drive_url)


# Save the reports as well:
fileSave <- fs::path(localdir,paste0("SITES_FILTER_REPORTS_AA_AW.Rdata"))
save( filter.report.H2O,
      filter.report.CO2, 
      filter.report.stability.H2O,
      filter.report.stability.CO2, file=fileSave)
googledrive::drive_upload(media = fileSave, overwrite = T, path = drive_url)

# ---------------------------------------------
# Application of the CCC (EC) Analysis ####
message('Running  CCC computation by Sampling Height...')

source(fs::path(DirRepo.eval,'workflows/flow.evaluation.One2One.CCC.R'))

fileSave <- fs::path(localdir,paste0("SITES_One2One_AA_AW.Rdata"))
save( SITES_One2One,file=fileSave)
googledrive::drive_upload(media = fileSave, overwrite = T, path = drive_url)

# Compile Data with the CCC in it:
source(fs::path(DirRepo.eval,'workflows/flow.evaluation.sitelist_CCC.R')) # ENSURE all SUBSEQUENT FILES ARE USING THE FILES PRODUCED HERE:
fileSave <- fs::path(localdir,paste0("SITE_DATA_FILTERED_CCC_AA_AW.Rdata"))
save( SITE_DATA_FILTERED_CCC,file=fileSave)
googledrive::drive_upload(media = fileSave, overwrite = T, path = drive_url)

# ----------------------WF_Version1--------------------

# Canopy Analysis:
source(fs::path(DirRepo.eval,'workflows/WF_Version1/flow.evaluation.GoodDrivers.V2.R'))
fileSave <- fs::path(localdir,paste0("SITES_One2One_canopy_model_AA_AW.Rdata"))
save( SITES_One2One_model,file=fileSave )

# ENSEMBLE:
source(fs::path(DirRepo.eval,'workflows/WF_Version1/flow.flux.ensemble._V1R.R'))
# Something is up with the CCC EC- Not clear why the CCC is recalculated in the season analysis... Just add season to the output

source(fs::path(DirRepo.eval,'workflows/WF_Version1/flow.evaluation.diel_V1.R'))
fileSave <- '/Volumes/MaloneLab/Research/FluxGradient/DIEL_SUMMARY_ENSEMBLE_V1_AA_AW.RDATA'
googledrive::drive_upload(media = fileSave, overwrite = T, path = drive_url)
# RESULTS #### Update Below to produce the same files!

# These files already use the CCC > 0.5:
# Evaluate what data is left after filtering summarizing by what sampling pairs are left:
source(fs::path(DirRepo.eval,'workflows/WF_Version1/flow.CCC.VIZ.R'))

# Evaluate what data is left after filtering summarizing by what flux data remains:
source(fs::path(DirRepo.eval,'workflows/WF_Version1/flow.flux.counts.R')) 

# Reliable Sampling Height Pairs (CCC_GF):
##source(fs::path(DirRepo.eval,'workflows/WF_Version1/flow.evaluation.RSHP_V1.R'))

# RSHP sensitivity to alternative CCC thresholds:
source(fs::path(DirRepo.eval,'workflows/WF_Version1/flow.evaluation.RSHP_threshold_sensitivity_V1.R'))
fileSave <- fs::path(localdir, paste0("SITE_RSHP_threshold_sensitivity_AA_AW.Rdata"))
googledrive::drive_upload(media = fileSave, overwrite = T, path = drive_url)

# CCC Threshold Evaluation:
source(fs::path(DirRepo.eval,'workflows/WF_Version1/flow.evaluation.RSHP_threshold_sensitivity_V1.R'))

#Results:
load( fs::path(localdir,paste0("SITES_One2One_AA_AW.Rdata")))
load( fs::path(localdir,paste0("SITE_DATA_FILTERED_CCC_AA_AW.Rdata")))

source(fs::path(DirRepo.eval,'workflows/flow.igbp.R'))


# Results Section 1: ####
SITES_One2One$CCC %>% summary


SITES_One2One %>% reframe(.by=c(gas, Approach), R2 = mean(R2), CCC = mean(CCC))
SITES_One2One %>% reframe(.by=c(gas, Canopy_L1), R2 = mean(R2), CCC = mean(CCC))
SITES_One2One %>% filter(CCC >= 0.5) %>% reframe(.by=c(gas, Canopy_L1), R2 = mean(R2), CCC = mean(CCC))


CO2.sites <- SITES_One2One %>% filter(CCC >= 0.5, gas == "CO2") 
H2O.sites <- SITES_One2One %>% filter(CCC >= 0.5, gas == "H2O") 

RSHP.CO2 <- CO2.sites$Site %>% unique
RSHP.H2O <- H2O.sites$Site %>% unique

# Number of Sites with RSHPs: 
length(RSHP.CO2)
length(RSHP.H2O)

# Both:
RSHP.H2O  %in% RSHP.CO2 

SITES_One2One <- SITES_One2One %>% mutate(Counter = 1)

# Total counts
SITES_One2One.summary.gas <- SITES_One2One %>% 
  reframe( .by=gas, Total = sum(Counter)) %>% 
  left_join(by ="gas", 
            SITES_One2One %>% filter(CCC > 0.5) %>% 
              reframe( .by=gas, Count.RSHP = sum(Counter))) %>% 
  mutate(percent =Count.RSHP/Total *100 )

SITES_One2One.summary.gas.site <- SITES_One2One %>% filter(CCC > 0.5) %>% 
  reframe( .by=c(gas,Site), Count.RSHP = sum(Counter)) %>% reframe(
    .by=c(gas), mean.Count.RSHP = mean( Count.RSHP ),
    sd.Count.RSHP = sd( Count.RSHP ))


SITES_One2One.summary.approach <-SITES_One2One %>% filter( CCC > 0.5) %>% 
  reframe( .by= gas, Total = sum(Counter)) %>% 
  left_join(by =c('gas'), 
            SITES_One2One %>% filter(CCC > 0.5) %>% 
              reframe( .by=c(gas, Approach), Count.RSHP = sum(Counter))) %>% 
  mutate(percent =Count.RSHP/Total *100 )

SITES_One2One.summary.canopy <-SITES_One2One %>% filter( CCC > 0.5) %>% 
  reframe( .by= gas, Total = sum(Counter)) %>% 
  left_join(by =c('gas'), 
            SITES_One2One %>% filter(CCC > 0.5) %>% 
              reframe( .by=c(gas, Canopy_L1), Count.RSHP = sum(Counter))) %>% 
  mutate(percent =Count.RSHP/Total *100 )

SITES_One2One %>% filter( CCC > 0.5) %>% 
  reframe( .by= c(gas, Canopy_L1), Mean_CCC = mean(CCC),
           Mean_R2 = mean(R2) )


# Section....
# Next go to flow.CCC.VIX for results
# Next go to flow.flux.counts for results
# Next go to flow.results.ecotype for results
# flow.results.summary
# Good Drivers 

load( file='/Volumes/MaloneLab/Research/FluxGradient/DIEL_SUMMARY_ENSEMBLE_V1_AA_AW.RDATA')