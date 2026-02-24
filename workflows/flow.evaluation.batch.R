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
setwd(DirRepo)
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
drive_url <- googledrive::as_id("https://drive.google.com/drive/folders/1Q99CT77DnqMl2mrUtuikcY47BFpckKw3")
googledrive::drive_auth(email = email) # Likely will not work on RStudio Server. If you get an error, try email=TRUE to open an interactive auth session.
data_folder <- googledrive::drive_ls(path = drive_url)


if(DnldFromGoogleDrive == TRUE){
  for( site in site.list){
    print(site)
    
    site <- site
    
    paste(site, "_Evaluation.Rdata", sep = "")
    dnld_files <- c()
    
    localdir.site <- paste(localdir,"/", site, sep = "")
    
  for (focal_file in dnld_files){
    
    message('Downloading ',focal_file, ' to ',localdir)
  
    site_folder <- googledrive::drive_ls(path = data_folder$id[data_folder$name==site])
    file_id <- subset(site_folder, name == focal_file)
    
    pathDnld <- fs::path(localdir.site,focal_file)
    googledrive::drive_download(file = file_id$id, 
                              path = fs::path(localdir.site,focal_file),
                              overwrite = T)
    zip.file <- fs::path(localdir.site,focal_file)
    unzip(zip.file, exdir=localdir.site)
  
  }
  } }

# Evaluation of SNR Threshold: (ETA 1 Hour) ####
SNR.plot.dir <- '/Volumes/MaloneLab/Research/FluxGradient/SNR_plot' # Where do you want to save the plots
SNR.summary.dir <-"/Volumes/MaloneLab/Research/FluxGradient/SNR_Summary/" # Where to save the summary file
source(fs::path(DirRepo.eval,'workflows/flow.evaluation.SNR.R'))

# Application of Filter Functions: ####
message('Running Filter...')
source(fs::path(DirRepo.eval,'workflows/flow.evaluation.filter.R'))

# Compiles Dataframes into one list: ####
source(fs::path(DirRepo.eval,'workflows/flow.evaluation_SITELIST.R'))

# 4 Objects are created
fileSave <- fs::path(localdir,paste0("SITE_DATA_FILTERED.Rdata"))
save( SITE_DATA_FILTERED,file=fileSave)
googledrive::drive_upload(media = fileSave, overwrite = T, path = drive_url)

fileSave <- fs::path(localdir,paste0("SITES_MBR_9min_FILTER.Rdata"))
save( SITES_MBR_9min_FILTER,file=fileSave)
googledrive::drive_upload(media = fileSave, overwrite = T, path = drive_url)

fileSave <- fs::path(localdir,paste0("SITES_AE_9min_FILTER.Rdata"))
save( SITES_AE_9min_FILTER ,file=fileSave)
googledrive::drive_upload(media = fileSave, overwrite = T, path = drive_url)

fileSave <- fs::path(localdir,paste0("SITES_WP_9min_FILTER.Rdata"))
save( SITES_WP_9min_FILTER,file=fileSave)
googledrive::drive_upload(media = fileSave, overwrite = T, path = drive_url)

# Application of the CCC (EC) Analysis ####
message('Running  CCC computation for best height...')

# set where you want plots to go:
dir.one2one <- '/Volumes/MaloneLab/Research/FluxGradient/One2One_Plots'
source(fs::path(DirRepo.eval,'workflows/flow.evaluation.One2One.CCC.R'))

fileSave <- fs::path(localdir,paste0("SITES_One2One.Rdata"))
save( SITES_One2One,file=fileSave)
googledrive::drive_upload(media = fileSave, overwrite = T, path = drive_url)

# Compile Data with the CCC in it:
source(fs::path(DirRepo.eval,'workflows/flow.evaluation.sitelist_CCC.R')) # ENSURE all SUBSEQUENT FILES ARE USING THE FILES PRODUCED HERE:
fileSave <- fs::path(localdir,paste0("SITE_DATA_FILTERED_CCC.Rdata"))
save( SITE_DATA_FILTERED_CCC,file=fileSave)
googledrive::drive_upload(media = fileSave, overwrite = T, path = drive_url)

# Reliable Sampling Height Pairs (CCC_GF):
source(fs::path(DirRepo.eval,'workflows/WF_Version1/flow.evaluation.RSHP.R'))

source(fs::path(DirRepo.eval,'workflows/WF_Version1/flow.evaluation.PreEnsemble.R'))

# Sampling Height Pair Model:
source(fs::path(DirRepo.eval,'workflows/flow.GoodDrivers.V2.R'))
fileSave <- fs::path(localdir,paste0("SITES_One2One_canopy_model.Rdata"))
save( SITES_One2One_model,file=fileSave )


# RESULTS #### Update Below to produce the same files!

# THese files already use the CCC > 0.5:
# Evaluate what data is left after filtering summarizing by what sampling pairs are left:
source(fs::path(DirRepo.eval,'workflows/Results/flow.CCC.VIZ.R'))

# Evaluate what data is left after filtering summarizing by what flux data remains:
source(fs::path(DirRepo.eval,'workflows/flow.flux.counts.R'))


# Ensemble GF:
# This is where I deal with sign changes!
source(fs::path(DirRepo.eval,'workflows/WF_Version1/flow.flux.ensemble_V1.R'))

# Application of the Diel Analysis ####
source(fs::path(DirRepo.eval,'workflows/WF_Version1/flow.evaluation.diel_V1.R'))

fileSave <- '/Volumes/MaloneLab/Research/FluxGradient/DIEL_SUMMARY_ENSEMBLE_V1.RDATA'
googledrive::drive_upload(media = fileSave, overwrite = T, path = drive_url)


# Site based Figure: ####
source(fs::path(DirRepo.eval,'workflows/flow.evaluation.sitebased.summary.R'))
