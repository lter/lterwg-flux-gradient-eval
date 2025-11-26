# cross gradient flag figure: 


# Need to make a for loop that brings in the site fluxes and extracts information needed for this figure.
rm(list=ls())

library(dplyr)
library(ggplot2)
library(ggridges)
library(ggpubr)
library(VSURF)
library(randomForest)


library(tidyverse)
library(colorspace)
library(ggpubr)
library(ggplot2)

localdir <- '/Volumes/MaloneLab/Research/FluxGradient/FluxData'
DirRepo <-"/Users/sm3466/YSE Dropbox/Sparkle Malone/Research/FluxGradient/lterwg-flux-gradient-eval"
load( fs::path(localdir,paste0("SITES_One2One.Rdata")))
load( fs::path(localdir,paste0("SITE_DATA_FILTERED.Rdata")))

# Extract month from POSIXct dates
months <- as.numeric(format(dates, "%m"))

# Build the dataset for canopy Information: ####
canopy <- read.csv(file.path(paste(localdir, "canopy_commbined.csv", sep="/"))) %>% distinct

SITES_One2One$R2 %>% round(2)

Highest.CCC <- SITES_One2One %>% reframe(.by= c(Site, gas, Approach), CCC.max = max(CCC, na.rm=T), R2.max = max(R2, na.rm=T) )

Highest.CCC %>% ggplot( aes( x= CCC.max, y= R2.max)) + geom_point()

SITES_One2One_canopy <- SITES_One2One %>% full_join(canopy, by=c("Site", "dLevelsAminusB" ) ) %>% 
  full_join( Highest.CCC,by = c("Site", "gas", "Approach")) %>%
  mutate(Approach = factor(Approach, levels = c("MBR", "AE", "WP") ),
         Good.CCC = case_when(  CCC >= 0.5 ~ 1, CCC < 0.5 ~ 0) %>% as.factor,
         RelativeDistB = MeasurementHeight_m_B - CanopyHeight ) %>% distinct

# Build file with all fluxes and information of interest.
Site_Fluxes <- data.frame()
for( site in site.list){
  
  print(paste("Working on " ,site))
  localdir.site <- paste(localdir,"/", site, sep = "")
  
  load(paste(localdir.site, "/", site, "_FILTER.Rdata", sep=""))
  
  
  canopy.sub <- canopy %>% filter( Site == site) %>% select(Site, Canopy_L1, dLevelsAminusB)
  
  SITES_One2One_sub <- SITES_One2One_canopy  %>% 
    select( Site, Good.CCC, dLevelsAminusB, Approach, RelativeDistB) %>% filter(Site == site) %>% left_join(canopy.sub , by= c('Site', 'dLevelsAminusB')) 
  
  MBR_9min_FILTER_CCC <- SITES_One2One_sub %>% filter( Approach == "MBR") %>% full_join( MBR_9min_FILTER , by = c('dLevelsAminusB'))  %>% select( timeEndA.local,FG_mean ,Good.CCC , Approach, Canopy_L1, gas, TowerPosition_A, TowerPosition_B,  FC_turb_interp, cross_grad_flag, timeEndA.local,  time.local, hour.local,dConcSNR, dConcTSNR, roughLength_calc, Stability_100, Stability_500, Stability_Exteme, RelativeDistB  )
  
  WP_9min_FILTER_CCC <- SITES_One2One_sub %>% filter( Approach == "WP") %>% full_join( WP_9min_FILTER , by = c('dLevelsAminusB'))   %>%  select( timeEndA.local,FG_mean ,Good.CCC , Approach, Canopy_L1, gas, TowerPosition_A, TowerPosition_B,  FC_turb_interp, cross_grad_flag, timeEndA.local, time.local, hour.local,dConcSNR, dConcTSNR, roughLength_calc, Stability_100, Stability_500, Stability_Exteme, RelativeDistB )
  
  AE_9min_FILTER_CCC <- SITES_One2One_sub %>% filter( Approach == "AE") %>% full_join( AE_9min_FILTER , by = c('dLevelsAminusB')) %>% select( timeEndA.local,FG_mean ,Good.CCC , Approach, Canopy_L1, gas, TowerPosition_A, TowerPosition_B,  FC_turb_interp, cross_grad_flag, timeEndA.local, time.local, hour.local,dConcSNR, dConcTSNR, roughLength_calc, Stability_100, Stability_500, Stability_Exteme, RelativeDistB )
  
  
  Data <- rbind(MBR_9min_FILTER_CCC,  AE_9min_FILTER_CCC, WP_9min_FILTER_CCC) %>% as.data.frame() %>% mutate(Site = site)
  
  Site_Fluxes <- rbind( Site_Fluxes ,Data)
  
  rm(  Data, WP_9min_FILTER_CCC, AE_9min_FILTER_CCC,  MBR_9min_FILTER_CCC,   canopy.sub, SITES_One2One_sub)
  
  
}



# What is good or bad: ####

# Determine the fraction of flagged data for each canopy level and determine what flags will remain after Good Level filtering. * May need to add this as a filter prior to diel calculation!

Site_Fluxes_Summary = Site_Fluxes  %>% reframe( .by = c(Canopy_L1, Site, Approach, Good.CCC, gas),
                                                roughLength_calc = mean(roughLength_calc),
                                                cgf.total1 = sum(cross_grad_flag, na.rm=T),
                                                total = length( Site),
                                                cgf.percent = cgf.total1/total*100 %>% round(2),
                                                RelativeDistB = mean(RelativeDistB, na.rm=T)) %>% filter(gas != "CH4") 


CGF.plot <- Site_Fluxes_Summary %>% ggplot( aes( x= Canopy_L1, y = cgf.percent , col=gas)) + geom_boxplot() + facet_wrap( ~ Approach) + theme_bw() + ylab('% Flagged') + xlab("") + scale_color_manual(values=c("black", "grey"),name= "Gas")  + theme(legend.position="top", strip.background = element_rect(colour = "black", fill = "transparent"))


ggsave("Figures/CrossGradientFlag_plot.png", plot = CGF.plot, width = 4, height = 2.5, units = "in")


Site_Fluxes_Summary %>% filter(gas == "CO2") %>% ggplot() + 
  geom_bar( aes( x = Canopy_L1 , fill = Good.CCC)) + scale_fill_manual(values=c("black", "grey")) + theme_bw()

Site_Fluxes_Summary %>% reframe(.by=c(Canopy_L1),
                                count = length(Good.CCC ))

Site_Fluxes_Summary %>% reframe(.by=c(Good.CCC),
                                count = length(Good.CCC ))

Site_Fluxes_Summary %>% filter(Good.CCC == 1) %>% reframe(.by=c( Canopy_L1),
                                                          count = length(Canopy_L1)/940)

Site_Fluxes_Summary %>% filter(Good.CCC == 1) %>% reframe(.by=c( Canopy_L1),
                                                          count = length(Canopy_L1)/312)

Site_Fluxes_Summary %>% reframe(.by=c(Good.CCC, Canopy_L1),
                                count = case_when( Canopy_L1 == 'WW' ~ length(Good.CCC )/ 202,
                                                   Canopy_L1 == 'AW' ~ length(Good.CCC )/ 328,
                                                   Canopy_L1 == 'AA' ~ length(Good.CCC )/ 410),
                                RelativeDistB.mean = mean(RelativeDistB),
                                RelativeDistB.min = min(RelativeDistB), 
                                RelativeDistB.max = max(RelativeDistB)) %>% distinct 

Site_Fluxes_Summary %>% ggplot(aes( x=Canopy_L1, y= RelativeDistB, col=Good.CCC )) +  geom_boxplot()

Site_Fluxes %>% ggplot(aes( x=Canopy_L1, fill=Stability_100 )) +  geom_bar() + facet_wrap(~Good.CCC)