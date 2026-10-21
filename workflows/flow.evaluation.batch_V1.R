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
DirRepo.eval <-"/Users/sm3466/Library/CloudStorage/Dropbox-YSE/Sparkle Malone/Research/FluxGradient/lterwg-flux-gradient-eval"
setwd(DirRepo.eval)

#localdir <- 'C:/Users/csturtevant/OneDrive - Battelle Ecology/FluxGradient/filterTesting' # We'll deposit output files here prior to uploading to Google Drive

localdir <- '/Volumes/MaloneLab/Research/FluxGradient'

# Data periods and run settings ----------------------------------------------
# Each entry is a folder in localdir holding SITE/SITE_Evaluation.Rdata (built from the EDI
# CSVs with flow.recreate.evaluation.dataframe_from_EDI.R). Periods are stacked per site.
# lterwg-flux-gradient (WorkFlow_master.R) now stacks all periods before calculating fluxes and writes
# one SITE_Evaluation.RDATA per site to NEON_GradientFlux_Data_2021_2026 (Aug 2021 - Aug 2026).
# Several folders can still be listed here; they are stacked per site.
eval.data.dirs <- "NEON_GradientFlux_Data_2021_2026"
despike.k <- 6        # GF spike removal: drop FG outside median +/- k*MAD per gas x height pair x day/night x season (NULL = off)
refit.rf  <- TRUE     # refit the canopy random-forest on this run's CCC table
run.tag   <- "_AA_AW_2021_2026"  # suffix for all outputs/figures of this run
# To reproduce the published (preprint) outputs instead:
#   eval.data.dirs <- "NEON_GradientFlux_Data"; despike.k <- NULL; refit.rf <- FALSE; run.tag <- "_AA_AW"
upload.drive <- FALSE
DnldFromGoogleDrive <- FALSE # Enter TRUE to grab files listed in dnld_files from Google Drive. Enter FALSE if you have the most up-to-date versions locally in localdir

email <- 'sparklelmalone@gmail.com'
if (upload.drive || DnldFromGoogleDrive) googledrive::drive_auth(email = TRUE) 
drive_url <- googledrive::as_id("https://drive.google.com/drive/folders/1Q99CT77DnqMl2mrUtuikcY47BFpckKw3") 
# The Data 
data_folder <- if (upload.drive || DnldFromGoogleDrive) googledrive::drive_ls(path = drive_url) else data.frame(id = character(), name = character())

metadata <- read.csv('/Volumes/MaloneLab/Research/FluxGradient/Ameriflux_NEON field-sites.csv') # has a list of all the sites

# -------------------------------------------------------
site.list <- metadata$Site_Id.NEON %>% unique
# ---------------------------------------------

# Download necessary files from Google Drive
# Download files from lter-flux-gradient Google Earth:
if (DnldFromGoogleDrive) source(fs::path(DirRepo.eval,'workflows/WF_Version1/flow.evaluation.download.R'))

# ---------------------------------------------

# With the EDI files:
# Step 2 already writes fresh evaluation RDATA files to eval.data.dirs.
# Do not rebuild the older EDI dataset here.

# ---------------------------------------------

# Evaluation of SNR Threshold: (ETA 1 Hour) ####
SNR.plot.dir <- file.path(localdir, paste0("SNR_plot", run.tag)) # Where do you want to save the plots
SNR.summary.dir <- paste0(file.path(localdir, paste0("SNR_Summary", run.tag)), "/")
dir.create(SNR.plot.dir, showWarnings = FALSE)
dir.create(SNR.summary.dir, showWarnings = FALSE) # Where to save the summary file
source(fs::path(DirRepo.eval,'workflows/flow.evaluation.SNR.R'))

# Application of Filter Functions: ####
message('Running Filter...')
source(fs::path(DirRepo.eval,'workflows/flow.evaluation.filter.R')) # 20 minutes
# ---------------------------------------------
# Compiles Dataframes into one list: ####
source(fs::path(DirRepo.eval,'workflows/flow.evaluation.sitelist_FILTER.R'))
# 4 Objects are created
fileSave <- fs::path(localdir,paste0(paste0("/NEON_GradientFlux_Data_Filter/SITE_DATA_FILTERED", run.tag, ".Rdata")))
save( SITE_DATA_FILTERED,file=fileSave)
if (!exists("upload.drive") || isTRUE(upload.drive)) googledrive::drive_upload(media = fileSave, overwrite = T, path = drive_url)

fileSave <- fs::path(localdir,paste0(paste0("/NEON_GradientFlux_Data_Filter/SITES_MBR_9min_FILTER", run.tag, ".Rdata")))
save( SITES_MBR_9min_FILTER,file=fileSave)
if (!exists("upload.drive") || isTRUE(upload.drive)) googledrive::drive_upload(media = fileSave, overwrite = T, path = drive_url)

fileSave <- fs::path(localdir,paste0(paste0("/NEON_GradientFlux_Data_Filter/SITES_AE_9min_FILTER", run.tag, ".Rdata")))
save( SITES_AE_9min_FILTER ,file=fileSave)
if (!exists("upload.drive") || isTRUE(upload.drive)) googledrive::drive_upload(media = fileSave, overwrite = T, path = drive_url)

fileSave <- fs::path(localdir,paste0(paste0("/NEON_GradientFlux_Data_Filter/SITES_WP_9min_FILTER", run.tag, ".Rdata")))
save( SITES_WP_9min_FILTER,file=fileSave)
if (!exists("upload.drive") || isTRUE(upload.drive)) googledrive::drive_upload(media = fileSave, overwrite = T, path = drive_url)

# Save the reports as well:
fileSave <- fs::path(localdir,paste0(paste0("/NEON_GradientFlux_Data_Filter/SITES_FILTER_REPORTS", run.tag, ".Rdata")))
save( filter.report.H2O,
      filter.report.CO2, 
      filter.report.stability.H2O,
      filter.report.stability.CO2, file=fileSave)
if (!exists("upload.drive") || isTRUE(upload.drive)) googledrive::drive_upload(media = fileSave, overwrite = T, path = drive_url)

# ---------------------------------------------
# Application of the CCC (EC) Analysis ####
message('Running  CCC computation by Sampling Height...')

source(fs::path(DirRepo.eval,'workflows/flow.evaluation.One2One.CCC.R'))

fileSave <- fs::path(localdir,paste0(paste0("/NEON_GradientFlux_Data_Filter/SITES_One2One", run.tag, ".Rdata")))
save( SITES_One2One, file=fileSave)
if (!exists("upload.drive") || isTRUE(upload.drive)) googledrive::drive_upload(media = fileSave, overwrite = T, path = drive_url)

# Compile Data with the CCC in it:
source(fs::path(DirRepo.eval,'workflows/flow.evaluation.sitelist_CCC.R')) # ENSURE all SUBSEQUENT FILES ARE USING THE FILES PRODUCED HERE:
fileSave <- fs::path(localdir,paste0(paste0("/NEON_GradientFlux_Data_Filter/SITE_DATA_FILTERED_CCC", run.tag, ".Rdata")))
save( SITE_DATA_FILTERED_CCC,file=fileSave)
if (!exists("upload.drive") || isTRUE(upload.drive)) googledrive::drive_upload(media = fileSave, overwrite = T, path = drive_url)

# ----------------------WF_Version1--------------------

# Canopy Analysis:
source(fs::path(DirRepo.eval,'workflows/WF_Version1/flow.evaluation.GoodDrivers.V2.R'))
fileSave <- fs::path(localdir,paste0(paste0("SITES_One2One_canopy_model", run.tag, ".Rdata")))
save( SITES_One2One_model,file=fileSave )

# ENSEMBLE:
source(fs::path(DirRepo.eval,'workflows/WF_Version1/flow.flux.ensemble._V1R.R'))
# Something is up with the CCC EC- Not clear why the CCC is recalculated in the season analysis... Just add season to the output

source(fs::path(DirRepo.eval,'workflows/WF_Version1/flow.evaluation.diel_V1.R'))
fileSave <- paste0("/Volumes/MaloneLab/Research/FluxGradient/NEON_GradientFlux_Ensemble/DIEL_SUMMARY_ENSEMBLE_V1", run.tag, ".RDATA")
save( ENSEMBLE_DIELS ,ENSEMBLE_DIELS_Site,ENSEMBLE_DIELS_Site_summary,ENSEMBLE_DIELS_EVAL,
      DIEL_curve_diagnostics, 
      file= fileSave )

if (!exists("upload.drive") || isTRUE(upload.drive)) googledrive::drive_upload(media = fileSave, overwrite = T, path = drive_url)
# RESULTS #### Update Below to produce the same files!

# These files already use the CCC > 0.5:
# Evaluate what data is left after filtering summarizing by what sampling pairs are left:
source(fs::path(DirRepo.eval,'workflows/WF_Version1/flow.CCC.VIZ.R'))

# Evaluate what data is left after filtering summarizing by what flux data remains:
source(fs::path(DirRepo.eval,'workflows/WF_Version1/flow.flux.counts.R')) 

# Reliable Sampling Height Pairs (CCC_GF):
##source(fs::path(DirRepo.eval,'workflows/WF_Version1/flow.evaluation.RSHP_V1.R'))

## CCC Threshold Evaluation:
source(fs::path(DirRepo.eval,'workflows/WF_Version1/flow.evaluation.RSHP_threshold_sensitivity_V1.R'))
fileSave <- fs::path(localdir, paste0(paste0("SITE_RSHP_threshold_sensitivity", run.tag, ".Rdata")))
if (!exists("upload.drive") || isTRUE(upload.drive)) googledrive::drive_upload(media = fileSave, overwrite = T, path = drive_url)

#Results:
load(fs::path(localdir,paste0(paste0("/NEON_GradientFlux_Data_Filter/SITES_One2One", run.tag, ".Rdata"))))
load( fs::path(localdir,paste0(paste0("/NEON_GradientFlux_Data_Filter/SITE_DATA_FILTERED_CCC", run.tag, ".Rdata"))))


source(fs::path(DirRepo.eval,'workflows/WF_Version1/flow.CCC.placement.recommendations.R'))

source(fs::path(DirRepo.eval,'workflows/WF_Version1/flow.CCC.drivers.R'))

source(fs::path(DirRepo.eval,'workflows/WF_Version1/flow.diel.range_table_V1.R'))

source(fs::path(DirRepo.eval,'workflows/WF_Version1/flow.results.ecotype.R'))

# Figure 4 (summary of reliable pairs): Figures/WF_Version1/CCC_results_summary<run.tag>.png
source(fs::path(DirRepo.eval,'workflows/flow.CCC.results.summary.R'), local = new.env())
# Table 5 (ensemble methods): Figures/WF_Version1/ENSEMBLE_method_comparison_R2_RSHP_CCC05<run.tag>.{csv,png}
source(fs::path(DirRepo.eval,'workflows/WF_Version1/flow.flux.ensemble.compare_V1.R'), local = new.env())
# Figures A13-A14 (GF vs EC distributions by site) and per-site 1:1 plots
source(fs::path(DirRepo.eval,'workflows/WF_Version1/flow.flux.distributions.site_density_V1.R'), local = new.env())
source(fs::path(DirRepo.eval,'workflows/WF_Version1/flow.flux.one_to_one.site_V1.R'), local = new.env())
# Inlet separation (delta z) results; order matters (each step reads the csv written by the previous one)
for (f in c("flow.results.delta_z_AA_AAplus.R", "flow.results.delta_z_site_rankings.R", "flow.results.delta_z_site_slopes.R",
            "flow.results.delta_z_slope_vs_canopyheight.R", "flow.results.delta_z_site_rankings_all_canopy.R",
            "flow.results.delta_z_site_slopes_all_canopy.R", "flow.results.delta_z_slope_vs_canopyheight_all_canopy.R"))
  source(fs::path(DirRepo.eval, 'workflows/WF_Version1', f), local = new.env())

# Supplementary analyses (Supplements S1 and S2): ####
# S1: what causes gradient-flux spikes (same rule as despike_fluxes()); reads the stacked EDI export
#     (or Spike_Eval/extract<period>/ if already extracted). Outputs: Spike_Eval/SPIKE_*<run.tag>, Figures/WF_Version1/SPIKE_*<run.tag>.png
source(fs::path(DirRepo.eval,'workflows/WF_Version1/flow.evaluation.spike_eval.R'), local = new.env())  # own environment: does not overwrite objects used below
# S2: choosing sampling-height pairs without an EC reference (e.g. CH4): AA MBR reference + AE/WP pairs with
#     CCC >= 0.5 against it, scaled to it. Outputs: Spike_Eval/noEC_*<run.tag>.csv, Figures/WF_Version1/S2_noEC_pair_selection<run.tag>.png
source(fs::path(DirRepo.eval,'workflows/WF_Version1/flow.evaluation.noEC_pair_selection.R'), local = new.env())
# Review: analyses answering the referee comments (RC1, RC2). Set make.aggregates <- TRUE once to build Review_Supplement/ALL_*.csv from the EDI tables (~1 h).
source(fs::path(DirRepo.eval,'workflows/WF_Version1/flow.supplement_review.R'), local = new.env())

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
load( file=paste0("/Volumes/MaloneLab/Research/FluxGradient/NEON_GradientFlux_Ensemble/DIEL_SUMMARY_ENSEMBLE_V1", run.tag, ".RDATA"))