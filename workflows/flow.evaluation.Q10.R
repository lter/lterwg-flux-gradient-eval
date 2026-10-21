# LEGACY (not used for the 2021-2026 figures; reads preprint-era files). Current version/workflow: workflows/flow.evaluation.batch_V1.R
# The Diel analysis is currently set up by season for the harmonized data:
library(tidyverse)
library(ggpubr)
library(ggplot2)

localdir <- '/Volumes/MaloneLab/Research/FluxGradient/FluxData'
DirRepo <-"/Users/sm3466/Library/CloudStorage/Dropbox-YSE/Sparkle Malone/Research/FluxGradient/lterwg-flux-gradient-eval"
load( fs::path(localdir,paste0("SITE_DATA_Harmonized.Rdata")))
source(paste(DirRepo,"/functions/calc.diel.R", sep="") )
# Calculate diels by Season: ####
Harmonized_Q10 <- data.frame()

for( site in site.list[-c(31, 42)]){
  print(site)
  
  df <-SITE_DATA_Harmonized[[site]] %>% mutate(month = format(timeEndA.local,'%m') %>% as.numeric,
                                               season = case_when(
                                                 month %in% c(12, 1, 2) ~ "Winter",
                                                 month %in% c(3, 4, 5) ~ "Spring",
                                                 month %in% c(6, 7, 8) ~ "Summer",
                                                 TRUE ~ "Autumn" # TRUE acts as the 'else' statement
                                               ),
                                               hour = format(timeEndA.local,'%H'),
                                               count= case_when( is.na(FG_harmonized) == FALSE ~ 1,
                                                                 TRUE ~ 0)) %>% distinct
  
  message( paste("Running Q10 functions- CO2 for ", site))
  # Calculate Diurnal Patterns by Year-month:
  

  
}
