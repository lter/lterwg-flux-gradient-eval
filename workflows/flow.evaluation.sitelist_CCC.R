# Builds the Filtered dataframe:
# The file produced does not include CH4!

rm(list=ls())

# The Diel analysis is currently set up by CCC threshold:
library(ggpubr)
library(ggplot2)
library(tidyverse)
library(gtools)
library(ggplot2)
library(ggpmisc)

drive_url <- googledrive::as_id("https://drive.google.com/drive/folders/1Q99CT77DnqMl2mrUtuikcY47BFpckKw3") # The Data 

load(fs::path(localdir,paste0("SITE_DATA_FILTERED.Rdata")))

source(fs::path(DirRepo.eval,'functions/calc.diel.R' ))
load(fs::path(localdir,paste0("SITES_One2One.Rdata"))) # Import CCC results
metadata <- read.csv('/Volumes/MaloneLab/Research/FluxGradient/Ameriflux_NEON field-sites.csv') 
site.list <- SITES_One2One$Site %>% unique

SITE_DATA_FILTERED_CCC <- list() # Save all the data here:

# Filtered Data:
for( site in site.list){
  
  print(paste("Working on " , site))
  
  message( paste("Importing the data for ", site))
  
  localdir.site <- paste(localdir,"/", site, sep = "")
  load(paste(localdir.site, "/", site, "_FILTER.Rdata", sep=""))
  
  SITES_One2One_sub <- SITES_One2One %>% 
    select( -c('Canopy_L1', 'TowerPosition_A', 'TowerPosition_B')) %>% filter(Site == site)

  SITE_DATA_FILTERED_CCC[[site]] <- SITE_DATA_FILTERED[[site]] %>% filter(gas != "CH4") %>%  full_join( SITES_One2One_sub , by = c('dLevelsAminusB', 'Approach', 'gas'), relationship = "many-to-many")  
  
  SITE_DATA_FILTERED_CCC[[site]] %>% summary

}
