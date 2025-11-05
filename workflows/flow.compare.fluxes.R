# Compare 30 miniute fluxes:

# Final diel:
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

# Import canopy information:
metadata <- read.csv('/Volumes/MaloneLab/Research/FluxGradient/Ameriflux_NEON field-sites.csv') %>% mutate( Site = Site_Id.NEON, IGBP = Vegetation.Abbreviation..IGBP., Koppen = Climate.Class.Abbreviation..Koeppen.) %>%  select( Site, IGBP, Koppen)

canopy <- read.csv(file.path(paste(localdir, "canopy_commbined.csv", sep="/"))) %>% distinct %>% left_join(metadata, by="Site" )

# Combine a halfhourly flux file: ####
SITE_FLUXES <- data.frame() # Save all the data here:

for( site in site.list){
  
  print(paste("Working on " ,site))
  
  message( paste("Importing the data for ", site))
  localdir.site <- paste(localdir,"/", site, sep = "")
  load(paste(localdir.site, "/", site, "_FILTER.Rdata", sep=""))
  
  canopy.sub <- canopy %>% filter( Site == site) 
  
  SITES_One2One_sub <-  SITES_One2One %>% filter(Site == site) %>% left_join(canopy.sub , by= c('Site', 'dLevelsAminusB')) 
  
  vars <-c(   "Site" , "CCC", "Approach","dLevelsAminusB","gas","canopyHeight_m", "MeasurementHeight_m_A","DistZaxsDisp",
              "DistZaxsGrndOfst","MeasurementHeight_m_B", "Max1_Tower_Position",   "Max2_Tower_Position","EVI.mean", "EVI.sd" ,"EVI.years","NDVI.mean", "NDVI.sd", "LAI.mean","LAI.sd","PRI.mean" , "PRI.sd","PRI.years","CHM.mean","CHM.sd","SAVI.mean","SAVI.sd","Cutoff05.CVH","Cutoff05.DGF","Cutoff05.GFP","Cutoff05.GiniCoeff","Cutoff05.Grain01.FHD", "Cutoff05.Grain01.VAI",  "Cutoff05.Grain10.FHD", "Cutoff05.Grain10.VAI",  "Cutoff05.Grain30.FHD",  "Cutoff05.Grain30.VAI",  "Cutoff05.MCH","Cutoff05.MH",  "Cutoff05.MSDH", "Cutoff05.RumpleIndex",  "Cutoff05.SDH" ,"Cutoff05.SDSDH" ,"Cutoff05.TopRugosity","Cutoff05.VCI","CanopyHeight" , "Canopy_L1",      "Canopy_L2","IGBP","Koppen" , "match_time" , "zeta" ,"Stability_100","Stability_500", "Stability_Exteme","FG_mean" ,"FG_sd","dConc","dConc_pvalue" ,"dConc_sd", "FC_turb_interp","FC_stor_interp","FC_nee_interp","LE_turb_interp","LE_stor_interp","LE_nsae_interp", "H_turb_interp","H_stor_interp","H_nsae_interp","ustar_interp", "roughLength_interp",    "RH","P_kPa","PAR" , "z_veg_aero" ,"z_displ_calc" , "roughLength_calc","Tair_K","esat_Pa","e_Pa","VPD", "rhoa_kgm3" ,"rhov_kgm3", "rho_kgm3" ,"specificHumidity_pct","Cp_moist", "Ftemp" ,"lambda", "FH2O_interp","L_obukhov","zoL" ,"cross_grad_flag","ustar_threshold","timeEndA.local", "dConcSNR" , "dConcTSNR") 
  
  
  MBR_9min_FILTER_CCC <- SITES_One2One_sub %>% filter( Approach == "MBR") %>% full_join( MBR_9min_FILTER , by = c('dLevelsAminusB','gas')) %>% select(vars) %>% data.frame()%>% distinct()

  WP_9min_FILTER_CCC <- SITES_One2One_sub %>% filter( Approach == "WP") %>% full_join( WP_9min_FILTER , by = c('dLevelsAminusB','gas')) %>% select(vars)%>% data.frame() %>% distinct()
  
  AE_9min_FILTER_CCC <- SITES_One2One_sub %>% filter( Approach == "AE") %>% full_join( AE_9min_FILTER , by = c('dLevelsAminusB','gas'))  %>% select(vars)%>% data.frame() %>% distinct()
  
  rownames(AE_9min_FILTER_CCC) <- NULL
  rownames(WP_9min_FILTER_CCC) <- NULL
  rownames(MBR_9min_FILTER_CCC) <- NULL
  
  Data <- rbind(MBR_9min_FILTER_CCC,  AE_9min_FILTER_CCC, WP_9min_FILTER_CCC) %>% distinct()
 
  rownames(Data) <- NULL
  rownames(SITE_FLUXES) <- NULL
  
  SITE_FLUXES  <- rbind(SITE_FLUXES, Data)
  rm(Data)
  
}
