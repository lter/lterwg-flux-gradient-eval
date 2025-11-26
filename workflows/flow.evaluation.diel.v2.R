
# The Diel analysis is currently set up by season for the harmonized data:
library(tidyverse)
library(ggpubr)
library(ggplot2)
library(colorspace)

localdir <- '/Volumes/MaloneLab/Research/FluxGradient/FluxData'
DirRepo <-"/Users/sm3466/YSE Dropbox/Sparkle Malone/Research/FluxGradient/lterwg-flux-gradient-eval"
load( fs::path(localdir,paste0("SITE_DATA_Harmonized.Rdata")))
source(paste(DirRepo,"/functions/calc.diel.R", sep="") )

# Calculate diels by Season: ####
Harmonized_DIELS <- data.frame()

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
    
    message( paste("Running DIEL functions- CO2 for ", site))
    # Calculate Diurnal Patterns by Year-month:
    
    
    try(DIEL.CO2 <- DIEL.COMPILE.Harmonized( dataframe = df, 
                                  FG_flux = 'FG_harmonized', 
                                  EC_flux = 'FC_turb_interp',
                                  Gas = "CO2") %>% mutate(gas= "CO2"), silent =TRUE)
    
    try(DIEL.CO2 %>%  ggplot() + geom_point(aes(x=Hour, y = FG, col=season))+ 
          geom_point(aes(x=Hour, y = EC), col="black") + facet_wrap(~season) , silent =TRUE)
    
 
    message( paste("Running DIEL functions- H2O for ", site))
    
    # Diel for H2O
    
    try(DIEL.H2O <- DIEL.COMPILE.Harmonized( dataframe = df, 
                                  FG_flux = 'FG_harmonized', 
                                  EC_flux = 'FC_turb_interp',
                                  Gas = "H2O") %>% mutate(gas= "H2O"), silent =TRUE)
    
        try(DIEL.H2O %>%  ggplot() + geom_point(aes(x=Hour, y = FG, col=season))+ 
              geom_point(aes(x=Hour, y = EC), col="black") + facet_wrap( ~ season), silent =TRUE)
    
   try( final.DIEL <- rbind(   DIEL.CO2,   DIEL.H2O) %>% mutate( site = site) , silent =TRUE)
    
   try( Harmonized_DIELS <- rbind( Harmonized_DIELS, final.DIEL), silent =TRUE)
    
    rm( final.DIEL, DIEL.CO2,   DIEL.H2O)
  
  }
    
Harmonized_DIELS %>%  names
Harmonized_DIELS_Site <- Harmonized_DIELS %>% 
  reframe( .by=c(site, gas, season), 
           total.FG = sum(FG),
           max.FG = max(FG),
           min.FG = min(FG),
           total.EC = sum(EC),
           max.EC = max(EC),
           min.EC = min(EC),
           Day.DIFF = min.EC-min.FG,
           Night.DIFF = max.EC-max.FG,
           total.diff = sum(DIFF.DIEL),
           percent.diff = total.diff/ total.EC *100,
           total.count = sum(count),
           Day.over.est.count = case_when( Day.DIFF > 0 ~ 1),
           Day.under.est.count = case_when( Day.DIFF < 0 ~ 1),
           Night.over.est.count = case_when( Night.DIFF > 0 ~ 1),
           Night.under.est.count = case_when( Night.DIFF < 0 ~ 1))

Harmonized_DIELS_Site %>% names

# Under/over estimation rates:
Harmonized_DIELS_Site_summary <- Harmonized_DIELS_Site %>% reframe( .by=c("gas", "site"), 
                                   Day.over.est.total = sum( Day.over.est.count, na.rm=T) ,
                                   Day.over.est.count = case_when(Day.over.est.total >0~1 ),
                                   Day.under.est.total = sum( Day.under.est.count, na.rm=T) ,
                                   Day.under.est.count = case_when(Day.under.est.total >0~1 ),
                                   
                                   Night.over.est.total = sum( Night.over.est.count, na.rm=T) ,
                                   Night.over.est.count = case_when(Night.over.est.total >0~1 ),
                                   Night.under.est.total = sum( Night.under.est.count, na.rm=T) ,
                                   Night.under.est.count = case_when(Night.under.est.total >0~1 )) %>% 
  reframe(.by=gas, Day.over.est.count = sum(Day.over.est.count, na.rm=T),
          Day.under.est.count = sum(Day.under.est.count, na.rm=T),
          Night.over.est.count = sum(Night.over.est.count, na.rm=T),
          Night.under.est.count = sum(Night.under.est.count, na.rm=T))

# Deviations:

Harmonized_DIELS_Site %>% reframe( .by=c( gas), 
                                   total.diff.mean = mean(total.diff, na.rm=T),
                                   total.diff.sd = sd(total.diff, na.rm=T))


# DIEL PLOTS: ####


plot.diel.gas.season.fg <- Harmonized_DIELS  %>%  ggplot()+ 
  geom_point(aes(x = Hour , y = FG, col=season)) +
  scale_colour_discrete_qualitative(palette = "Harmonic") +
  theme_bw() + facet_wrap(~ gas, , scales = "free_y")+ 
  theme(legend.position = "top", 
        strip.background = element_rect(fill = "transparent", size = 0.5),
        legend.title = element_blank()) + ylab(expression(paste( "GF (g m"^2, ")")))

plot.diel.gas.season.ec <- Harmonized_DIELS  %>%  ggplot() + 
  geom_point(aes(x = Hour , y = EC, col=season)) +
  scale_colour_discrete_qualitative(palette = "Harmonic") +
  theme_bw() + facet_wrap(~ gas, , scales = "free_y")+ 
  theme(legend.position = "top",
        strip.background = element_rect(fill = "transparent", size = 0.5),
        legend.title = element_blank())  + ylab(expression(paste( "EC (g m"^2, ")"))) + xlab("")


plot.diel.gas.season.regression <-Harmonized_DIELS  %>%  ggplot(aes(x = FG , y = EC, col=season))+
  geom_point() +
  geom_smooth(method = "lm", se = FALSE, size=0.5, col='black') +
  stat_regline_equation(aes(label = paste(..rr.label.., sep = "~`,`~")), col='black') +
  scale_colour_discrete_qualitative(palette = "Harmonic") +
  theme_bw() + 
  facet_wrap(~ gas+ season, , scales = "free", ncol=4) + 
  theme(legend.position = "none", 
        strip.background = element_rect(fill = "transparent", size = 0.5),
        legend.title = element_blank()) + ylab(expression(paste( "EC (g m"^2, ")")))  + xlab(expression(paste( "GF (g m"^2, ")"))) 

diel.plot.season <- ggarrange ( ggarrange(plot.diel.gas.season.ec,  plot.diel.gas.season.fg, ncol=1, common.legend = TRUE, labels= c("A", "B")), 
ggarrange(plot.diel.gas.season.regression, ncol=1,
          labels= "C"), ncol=1)

ggsave("/Users/sm3466/YSE Dropbox/Sparkle Malone/Research/FluxGradient/lterwg-flux-gradient-eval/Figures/DIEL_PLOTS_SEASON.png", plot = diel.plot.season, width = 8, height =9, units = "in")

Harmonized_DIELS_Site  %>%   ggplot() + 
  geom_boxplot(aes(x = total.diff, y = site , fill = season)) +
  theme_bw() + facet_wrap(~ gas , scales = "free_x") +
  theme(legend.position = "top", 
        strip.background = element_rect(fill = "transparent", size = 0.5)) 

# ALL SITE DIEL PLOTS: #####
plot.diels.co2 <- Harmonized_DIELS %>% filter(season == 'Summer', gas=="CO2") %>%  ggplot()+ 
  stat_smooth(aes(x = Hour , y = FG), col="black") + 
  stat_smooth(aes(x = Hour , y = EC), col="black", linetype="dashed") +
  geom_ribbon(aes(x=Hour, ymin = FG, ymax = EC), fill = "red", alpha = 0.2)+
  theme_bw() + facet_wrap(~ site, scales = "free_y", ncol=5) +  ylab(expression(paste( "CO"[2]," (g m"^2, ")"))) 

plot.diels.h2o <- Harmonized_DIELS %>% filter(season == 'Summer', gas=="H2O") %>%  ggplot()+ 
  stat_smooth(aes(x = Hour , y = FG), col="black") + 
  stat_smooth(aes(x = Hour , y = EC), col="black", linetype="dashed") +
  geom_ribbon(aes(x=Hour, ymin = FG, ymax = EC), fill = "red", alpha = 0.2)+
  theme_bw() + facet_wrap(~ site, scales = "free_y", ncol=5) +  ylab(expression(paste( "H"[2],"O (g m"^2, ")"))) 

ggsave("/Users/sm3466/YSE Dropbox/Sparkle Malone/Research/FluxGradient/lterwg-flux-gradient-eval/Figures/DIEL_PLOTS_SITES_CO2.png", plot = plot.diels.co2 , width = 8, height =9, units = "in")

ggsave("/Users/sm3466/YSE Dropbox/Sparkle Malone/Research/FluxGradient/lterwg-flux-gradient-eval/Figures/DIEL_PLOTS_SITES_H2O.png", plot = plot.diels.h2o , width = 8, height =9, units = "in")

save( Harmonized_DIELS ,Harmonized_DIELS_Site,Harmonized_DIELS_Site_summary,
     file='/Volumes/MaloneLab/Research/FluxGradient/DIEL_SUMMARY_Harmonized.RDATA')
