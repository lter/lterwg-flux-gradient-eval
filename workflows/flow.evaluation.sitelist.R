# Builds the Filtered dataframe:
rm(list=ls())

# The Diel analysis is currently set up by CCC threshold:
library(ggpubr)
library(ggplot2)
library(tidyverse)
library(gtools)
library(ggplot2)
library(ggpmisc)

DirRepo <-"/Users/sm3466/YSE Dropbox/Sparkle Malone/Research/FluxGradient/lterwg-flux-gradient-eval"
localdir <- '/Volumes/MaloneLab/Research/FluxGradient/FluxData'

source(fs::path(DirRepo,'functions/calc.diel.R' ))
load(fs::path(localdir,paste0("SITES_One2One.Rdata"))) # Import CCC results
metadata <- read.csv('/Volumes/MaloneLab/Research/FluxGradient/Ameriflux_NEON field-sites.csv') 
site.list <- metadata$Site_Id.NEON %>% unique

# Need to bring in new canopy file with all the canopy_L1 options
canopy <- read.csv(file.path(paste(localdir, "canopy_commbined.csv", sep="/"))) %>% distinct
canopy$Canopy_L1 %>% unique
# Data Prep:
Highest.CCC <- SITES_One2One %>% reframe(.by= c(Site, gas, Approach), CCC.max = max(CCC, na.rm=T))

SITES_One2One_canopy <- SITES_One2One %>% full_join(canopy, by=c("Site", "dLevelsAminusB" ) ) %>% 
  full_join( Highest.CCC,by = c("Site", "gas", "Approach")) %>% mutate(Approach = factor(Approach, levels = c("MBR", "AE", "WP") ),
                                                                       RelativeDistB = MeasurementHeight_m_B - CanopyHeight, 
                                                                       RelativeDistA = MeasurementHeight_m_A - CanopyHeight, 
                                                                       MeasurementDist = MeasurementHeight_m_A - MeasurementHeight_m_A)

SITE_DATA_FILTERED <- list() # Save all the data here:

# Filtered Data:
for( site in site.list){
  
  print(paste("Working on " , site))
  
  message( paste("Importing the data for ", site))
  
  localdir.site <- paste(localdir,"/", site, sep = "")
  load(paste(localdir.site, "/", site, "_FILTER.Rdata", sep=""))
  
  canopy.sub <- canopy %>% filter( Site == site) %>% select(Site, Canopy_L1, dLevelsAminusB)
  canopy.sub$Canopy_L1 %>% unique
  
  SITES_One2One_sub <- SITES_One2One_canopy %>% 
    select( Site, CCC, dLevelsAminusB, Approach, R2) %>% filter(Site == site) %>% left_join(canopy.sub , by= c('Site', 'dLevelsAminusB'))  %>% filter(CCC >= 0.5)
  
  MBR_9min_FILTER_CCC <- SITES_One2One_sub %>% filter( Approach == "MBR") %>% 
    full_join( MBR_9min_FILTER , by = c('dLevelsAminusB'), relationship = "many-to-many")  %>% 
    filter(CCC >= 0.5 , cross_grad_flag == 0) %>% 
    select( timeEndA.local,FG_mean ,CCC , R2, Approach, Canopy_L1, gas, 
            TowerPosition_A, TowerPosition_B,  EC_mean, cross_grad_flag, dLevelsAminusB)
  
  WP_9min_FILTER_CCC <- SITES_One2One_sub %>% filter( Approach == "WP") %>% 
    full_join( WP_9min_FILTER , by = c('dLevelsAminusB'), relationship = "many-to-many" )  %>% 
    filter(CCC >= 0.5 , cross_grad_flag == 0) %>% 
    select( timeEndA.local,FG_mean ,CCC , R2, Approach, Canopy_L1, gas, 
            TowerPosition_A, TowerPosition_B,  EC_mean, cross_grad_flag, dLevelsAminusB)
  
  AE_9min_FILTER_CCC <- SITES_One2One_sub %>% 
    filter( Approach == "AE") %>% 
    full_join( AE_9min_FILTER , by = c('dLevelsAminusB'), 
               relationship = "many-to-many")  %>% 
    filter(CCC >= 0.5 , cross_grad_flag == 0) %>% 
    select( timeEndA.local,FG_mean ,CCC , R2, Approach, Canopy_L1, gas, 
            TowerPosition_A, TowerPosition_B,  EC_mean, cross_grad_flag, dLevelsAminusB)
  
  Data <- rbind( MBR_9min_FILTER_CCC,  AE_9min_FILTER_CCC, WP_9min_FILTER_CCC) %>% as.data.frame() 
  
  
  SITE_DATA_FILTERED[[site]] <- Data
}

print("Create Site List by approach and reports")

# REPORTS: ####
SITES_MBR_9min_FILTER <- list()
SITES_AE_9min_FILTER <- list()
SITES_WP_9min_FILTER <- list()

filter.report.CO2 <- data.frame()
filter.report.stability.CO2 <- data.frame()

filter.report.H2O <- data.frame()
filter.report.stability.H2O <- data.frame()

# Site List DF and filter report
for( site in site.list){
  
  print( site)
  
  # Load the files:
  localdir.site <- paste(localdir,"/", site, sep = "")
  
  files <- paste(site, "_FILTER.Rdata", sep = "")
  
  load(paste(localdir.site, "/", files, sep=""))
  
  
  SITES_MBR_9min_FILTER[[site]] <- MBR_9min_FILTER 
  SITES_AE_9min_FILTER[[site]] <- AE_9min_FILTER
  SITES_WP_9min_FILTER[[site]] <- WP_9min_FILTER
  
  files.CO2 <- paste(site, "_9min.report.CO2.csv", sep = "")
  
  report.CO2 <- read.csv( paste( localdir.site,"/", files.CO2, sep="" ))
  
  filter.report.CO2 <- rbind(filter.report.CO2,  report.CO2 )
  
  files.H2O <- paste(site, "_9min.report.H2O.csv", sep = "")
  
  report.H2O <- read.csv( paste( localdir.site,"/", files.H2O, sep="" ))
  
  filter.report.H2O <- rbind(filter.report.H2O,  report.H2O )
  
  message("Done with ", site)
}

for( site in site.list){
  
  print( site)
  
  # Load the files:
  localdir.site <- paste(localdir,"/", site, sep = "")
  
  files.CO2 <- paste(site, "_9min.report.stability.CO2.csv", sep = "")
  report.CO2 <- read.csv( paste( localdir.site,"/", files.CO2, sep="" ))
  filter.report.stability.CO2 <- rbind(filter.report.stability.CO2,  report.CO2 )
  
  files.H2O <- paste(site, "_9min.report.stability.H2O.csv", sep = "")
  report.H2O <- read.csv( paste( localdir.site,"/", files.H2O, sep="" ))
  filter.report.stability.H2O <- rbind(filter.report.stability.H2O,  report.H2O )
  
  message("Done with ", site)
}