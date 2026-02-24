
# The Diel analysis is currently set up by season for the ENSEMBLE data:

library(tidyverse)
library(ggpubr)
library(ggplot2)
library(colorspace)

localdir <- '/Volumes/MaloneLab/Research/FluxGradient/FluxData'
DirRepo.eval <-"/Users/sm3466/YSE Dropbox/Sparkle Malone/Research/FluxGradient/lterwg-flux-gradient-eval"
load( fs::path(localdir,paste0("SITE_DATA_ENSEMBLE_V1.Rdata")))

metadata <- read.csv('/Volumes/MaloneLab/Research/FluxGradient/Ameriflux_NEON field-sites.csv') %>% 
  mutate(EcoType = case_when( Vegetation.Abbreviation..IGBP. == 'ENF' ~ 'Forest',
                              Vegetation.Abbreviation..IGBP. == 'DBF' ~ 'Forest', 
                              Vegetation.Abbreviation..IGBP. == 'MF' ~ 'Forest',
                              Vegetation.Abbreviation..IGBP. == 'EBF' ~ 'Forest',
                              Vegetation.Abbreviation..IGBP. == 'SAV' ~ 'Forest',
                              Vegetation.Abbreviation..IGBP. == 'WET' ~ 'Wetland',
                              
                              Vegetation.Abbreviation..IGBP. == 'GRA' ~ 'Grassland',
                              
                              Vegetation.Abbreviation..IGBP. == 'CVM' ~ 'Cropland',
                              Vegetation.Abbreviation..IGBP. == 'CRO' ~ 'Cropland',
                              Vegetation.Abbreviation..IGBP. == 'OSH' ~ 'Shrubland')) %>% rename( Site = Site_Id.NEON)


source(paste(DirRepo.eval,"/functions/calc.diel.R", sep="") )

# Calculate diels by Season: ####
ENSEMBLE_DIELS <- data.frame()
ENSEMBLE_DIELS_EVAL <- data.frame()

site.list <- SITE_DATA_ENSEMBLE %>% names 

for( site in site.list){
  print(site)

  df <-SITE_DATA_ENSEMBLE[[site]] %>% mutate(month = format(time.rounded,'%m') %>% as.numeric,
         season = case_when(
           month %in% c(12, 1, 2) ~ "Winter",
           month %in% c(3, 4, 5) ~ "Spring",
           month %in% c(6, 7, 8) ~ "Summer",
           TRUE ~ "Autumn" # TRUE acts as the 'else' statement
         ),
         hour = format(time.rounded,'%H'),
         count= case_when( is.na(FG_ENSEMBLE) == FALSE ~ 1,
                           TRUE ~ 0)) %>% distinct
    
    message( paste("Running DIEL functions- CO2 for ", site))
    # Calculate Diurnal Patterns by Year-month:
    
    
    try(DIEL.CO2 <- DIEL.COMPILE.ENSEMBLE( dataframe = df, 
                                  FG_flux = 'FG_ENSEMBLE', 
                                  EC_flux = 'EC_mean',
                                  Gas = "CO2") %>% mutate(gas= "CO2"), silent =TRUE)
    
    try(DIEL.CO2 %>%  ggplot() + geom_point(aes(x=Hour, y = FG, col=season))+ 
          geom_point(aes(x=Hour, y = EC), col="black") + facet_wrap(~season) , silent =TRUE)
    
 
    message( paste("Running DIEL functions- H2O for ", site))
    
    # Diel for H2O
    
    try(DIEL.H2O <- DIEL.COMPILE.ENSEMBLE( dataframe = df, 
                                  FG_flux = 'FG_ENSEMBLE', 
                                  EC_flux = 'EC_mean',
                                  Gas = "H2O") %>% mutate(gas= "H2O"), silent =TRUE)
    
        try(DIEL.H2O %>%  ggplot() + geom_point(aes(x=Hour, y = FG, col=season))+ 
              geom_point(aes(x=Hour, y = EC), col="black") + facet_wrap( ~ season), silent =TRUE)
    
    linear.model.season <- function(df){
      
      seasons <- df$season %>% unique
      summary.seasons <- data.frame()
      
      for( i in seasons){
        df.sub <- df %>% filter( season == i)
        linear.model <- lm(df.sub$FG ~ df.sub$EC) %>% summary 
        summary.sub <- data.frame( season = i, site = site, 
                                   R2 = linear.model$r.squared %>% round(2), gas = df.sub$gas %>% unique)
        summary.seasons <- rbind( summary.seasons, summary.sub)}
      
      return( summary.seasons)
      
    }
    
    
    if(exists('DIEL.CO2') & exists('DIEL.H2O') ){
      
      ENSEMBLE_DIELS_EVAL <- rbind(ENSEMBLE_DIELS_EVAL, 
                                     try(linear.model.season(df = DIEL.CO2), silent =TRUE),
                                     try(linear.model.season(df = DIEL.H2O), silent =TRUE))
      
      final.DIEL <- rbind(DIEL.CO2, DIEL.H2O) %>% mutate( site = site)
      
    } else if(exists('DIEL.CO2') & !exists('DIEL.H2O') ) {
      
      ENSEMBLE_DIELS_EVAL <- rbind(ENSEMBLE_DIELS_EVAL, 
                                     try(linear.model.season(df = DIEL.CO2), silent =TRUE))
      
      final.DIEL <- rbind(DIEL.CO2) %>% mutate( site = site)
      
    } else if(exists('DIEL.H2O') & !exists('DIEL.CO2')  ) {
      
      ENSEMBLE_DIELS_EVAL <- rbind(ENSEMBLE_DIELS_EVAL, 
                                     try(linear.model.season(df = DIEL.H2O), silent =TRUE))
      
      final.DIEL <- rbind(DIEL.H2O) %>% mutate( site = site)
    }
    
 
    

    

    
   try( ENSEMBLE_DIELS <- rbind( ENSEMBLE_DIELS, final.DIEL), silent =TRUE)
    
    rm( final.DIEL, DIEL.CO2,   DIEL.H2O)
  
  }
    
ENSEMBLE_DIELS_Site <- ENSEMBLE_DIELS %>% 
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

# DIEL PLOTS: ####

plot.diel.gas.season.fg <- ENSEMBLE_DIELS  %>%  ggplot()+ 
  geom_point(aes(x = Hour , y = FG, col=season)) +
  scale_colour_discrete_qualitative(palette = "Harmonic") +
  theme_bw() + facet_wrap(~ gas, , scales = "free_y")+ 
  theme(legend.position = "top", 
        strip.background = element_rect(fill = "transparent", linewidth = 0.5),
        legend.title = element_blank()) + ylab(expression(paste( "GF (g m"^2, ")")))

plot.diel.gas.season.ec <- ENSEMBLE_DIELS  %>%  ggplot() + 
  geom_point(aes(x = Hour , y = EC, col=season)) +
  scale_colour_discrete_qualitative(palette = "Harmonic") +
  theme_bw() + facet_wrap(~ gas, , scales = "free_y")+ 
  theme(legend.position = "top",
        strip.background = element_rect(fill = "transparent", size = 0.5),
        legend.title = element_blank())  + ylab(expression(paste( "EC (g m"^2, ")"))) + xlab("")

plot.diel.gas.season.regression <-ENSEMBLE_DIELS  %>%  ggplot(aes(x = FG , y = EC, col=season))+
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

ggsave("/Users/sm3466/YSE Dropbox/Sparkle Malone/Research/FluxGradient/lterwg-flux-gradient-eval/Figures/WF_Version1/DIEL_PLOTS_SEASON.png", 
       plot = diel.plot.season, width = 9, height =8, units = "in")

# ALL SITE DIEL PLOTS: #####

plot.diels.co2 <- ENSEMBLE_DIELS %>% filter(season == 'Spring', gas=="CO2") %>%  ggplot()+ 
  stat_smooth(aes(x = Hour , y = FG), col="black") + 
  stat_smooth(aes(x = Hour , y = EC), col="black", linetype="dashed") +
  geom_ribbon(aes(x=Hour, ymin = FG, ymax = EC), fill = "red", alpha = 0.2)+
  theme_bw() + facet_wrap(~ site, scales = "free_y", ncol=5) +  ylab(expression(paste( "CO"[2]," (g m"^2, ")"))) 

plot.diels.h2o <- ENSEMBLE_DIELS %>% filter(season == 'Spring', gas=="H2O") %>%  ggplot()+ 
  stat_smooth(aes(x = Hour , y = FG), col="black") + 
  stat_smooth(aes(x = Hour , y = EC), col="black", linetype="dashed") +
  geom_ribbon(aes(x=Hour, ymin = FG, ymax = EC), fill = "red", alpha = 0.2)+
  theme_bw() + facet_wrap(~ site, scales = "free_y", ncol=5) +  ylab(expression(paste( "H"[2],"O (g m"^2, ")"))) 

ggsave("/Users/sm3466/YSE Dropbox/Sparkle Malone/Research/FluxGradient/lterwg-flux-gradient-eval/Figures/WF_Version1/DIEL_PLOTS_SITES_CO2_V1.png", plot = plot.diels.co2 , width = 8, height =9, units = "in")

ggsave("/Users/sm3466/YSE Dropbox/Sparkle Malone/Research/FluxGradient/lterwg-flux-gradient-eval/Figures/WF_Version1/DIEL_PLOTS_SITES_H2O_V1.png", plot = plot.diels.h2o , width = 8, height =9, units = "in")


# Evaluation plots for DIELS: #####

ENSEMBLE_DIELS_EVAL %>% summary


Plot_ENSEMBLE_DIEL_EVAL_SITES <- ENSEMBLE_DIELS_EVAL %>% ggplot() + 
  geom_point(aes( y = site, x=R2, col=gas), shape=15, alpha=0.5)+
  facet_wrap(~season, nrow=1) + theme_bw() +
  scale_color_manual(values = c('darkgreen', "blue")) +
  theme( legend.text = element_text(size = 12),
         legend.title = element_text(size = 12),
         strip.background = element_rect(fill = "transparent", size = 0.5),
         legend.position = "top")+ ylab("") +  xlab(expression(paste("Ensemble GF R"^2)))

ggsave("/Users/sm3466/YSE Dropbox/Sparkle Malone/Research/FluxGradient/lterwg-flux-gradient-eval/Figures/WF_Version1/DIEL_PLOTS_SEASON_EVAL_SITES_V1.png", 
       plot = Plot_ENSEMBLE_DIEL_EVAL_SITES, width = 9, height =8, units = "in")


plot.density <- ENSEMBLE_DIELS_EVAL %>%ggplot() + 
  geom_density( aes( x= R2, col=gas)) +  xlim(0,1) + scale_color_manual(values = c('darkgreen', "blue")) +
  theme_bw()+ xlab(expression(paste("DIEL R"^2))) +
  ylab("") + theme(strip.background =element_rect(fill="transparent"))

Plot_ENSEMBLE_DIEL_EVAL <- ENSEMBLE_DIELS_EVAL %>% ggplot() + 
  geom_boxplot(aes( y = site, x=R2, col=gas), shape=15, alpha=0.5)+
  facet_wrap(~gas, nrow=1) + theme_bw() +
  scale_color_manual(values = c('darkgreen', "blue")) +
  theme( legend.text = element_text(size = 12),
         legend.title = element_text(size = 12),
         strip.background = element_rect(fill = "transparent", size = 0.5),
         legend.position = "top")+ ylab("") +  
  xlab(expression(paste("Ensemble GF R"^2)))

ggsave("/Users/sm3466/YSE Dropbox/Sparkle Malone/Research/FluxGradient/lterwg-flux-gradient-eval/Figures/WF_Version1/DIEL_PLOTS_SEASON_EVAL_V1.png", 
       plot = Plot_ENSEMBLE_DIEL_EVAL, width = 9, height =8, units = "in")


# Figure: Reliable Towers by season ####
# Add in the canopy information:
site.list.df <- data.frame(Site = site.list)
season.df <- data.frame(season=ENSEMBLE_DIELS_EVAL$season %>% unique)
gas.df<- data.frame(gas=c('CO2', 'H2O'))
ecotype <- metadata %>% select(Site, EcoType)

complete.site.season.list <- site.list.df %>%  cross_join(season.df) %>% cross_join(gas.df) %>% left_join(ecotype, by='Site')

ENSEMBLE_DIELS_EVAL_canopy <- ENSEMBLE_DIELS_EVAL %>% rename(Site = site) %>% full_join(complete.site.season.list , by= c("Site", 'gas', 'season')) %>% 
  mutate( R2  = replace_na(R2,NA),
          reliable = case_when(R2 >= 0.45  ~ 1,
                               .default= NA))

 plot.reliable.season <- ENSEMBLE_DIELS_EVAL_canopy %>%  
   ggplot(aes(x = reliable, y = Site, fill = season)) +
   geom_col() + facet_wrap(~ gas) +  ylab("")+ xlab('')+
   theme_bw() +  scale_fill_discrete_qualitative(palette = "Harmonic") + theme(legend.position = "top") + labs(fill='Season')+ 
   theme(axis.text.x = element_blank(), 
         axis.ticks.x = element_blank(),
         legend.text = element_text(size = 12),
         legend.title = element_text(size = 12),
         strip.background = element_rect(fill = "transparent", size = 0.5),
         legend.position = "top")

 
 ENSEMBLE_DIELS_EVAL_canopy_gas <- ENSEMBLE_DIELS_EVAL_canopy %>% 
   reframe( .by= c(gas, season), reliable.percent = sum(reliable, na.rm=T)/47*100 %>% round(0))
 
 
 # Percent of sites with each month
 plot.reliable.season.total <-  ENSEMBLE_DIELS_EVAL_canopy_gas  %>%  
   ggplot(aes(y = gas, x = reliable.percent, fill = season)) +
   geom_col()  +
   theme_bw() +  scale_fill_discrete_qualitative(palette = "Harmonic") + theme(legend.position = "top") + labs(fill='Season') + xlab('Season') + 
   theme(axis.text.x = element_blank(), 
         axis.ticks.x = element_blank(),
          legend.position='none') + ylab("")+ xlab('')+
   geom_text(aes(label = sprintf("%.0f%%", reliable.percent)), position = position_stack(vjust = 0.5), size = 3) +
   ggtitle("Towers with Reliable Fluxes")

 

 plot.2 <- ggarrange(  Plot_ENSEMBLE_DIEL_EVAL,
                       plot.reliable.season , labels=c("A", "B" ))
                       
 plot.3 <- ggarrange(   plot.reliable.season.total, labels="C" )
 
 plot.23 <- ggarrange(   plot.2,  plot.3 , nrow=2 , heights= c(3,1))

 ggsave("/Users/sm3466/YSE Dropbox/Sparkle Malone/Research/FluxGradient/lterwg-flux-gradient-eval/Figures/WF_Version1/DIEL_PLOTS_SEASON_Final_V1.png", 
        plot =  plot.23, width = 8, height =8, units = "in")
 
# Figure: EVAL by Ecotype: #### 
 
Plot_ENSEMBLE_DIEL_Eco_EVAL <- ENSEMBLE_DIELS_EVAL_canopy %>%  ggplot() + 
  geom_boxplot(aes( y = EcoType, x=R2, col=gas), shape=15, alpha=0.5)+
  facet_wrap(~season, nrow=1) + theme_bw() +
  scale_color_manual(values = c('darkgreen', "blue")) +
  theme( legend.text = element_text(size = 12),
         legend.title = element_text(size = 12),
         strip.background = element_rect(fill = "transparent", size = 0.5),
         legend.position = "top")+ ylab("") +  xlab(expression(paste("DIEL R"^2)))

Plot_ENSEMBLE_DIEL_Eco_EVAL_Season <- ENSEMBLE_DIELS_EVAL_canopy %>%  ggplot() + 
  geom_boxplot(aes( y = season, x=R2, col=gas), shape=15, alpha=0.5)+
  facet_wrap(~EcoType, nrow=1) + theme_bw() +
  scale_color_manual(values = c('darkgreen', "blue")) +
  theme( legend.text = element_text(size = 12),
         legend.title = element_text(size = 12),
         strip.background = element_rect(fill = "transparent", size = 0.5),
         legend.position = "top")+ ylab("") +  xlab(expression(paste("Ensemble GF R"^2)))

ggsave("/Users/sm3466/YSE Dropbox/Sparkle Malone/Research/FluxGradient/lterwg-flux-gradient-eval/Figures/WF_Version1/DIEL_PLOTS_SEASON_EcoType_EVAL_V1.png", 
       plot = Plot_ENSEMBLE_DIEL_Eco_EVAL, width = 9, height =4, units = "in")

# Text Summary: ####
# This is saved below:
load( file='/Volumes/MaloneLab/Research/FluxGradient/DIEL_SUMMARY_ENSEMBLE_V1.RDATA')

ENSEMBLE_DIELS_Site %>% names
ENSEMBLE_DIELS_Site %>% summary

# Text Summary# Under/over estimation rates:
ENSEMBLE_DIELS_Site_summary <- ENSEMBLE_DIELS_Site %>% reframe( .by=c("gas", "site"), 
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

ENSEMBLE_DIELS_Site %>% reframe( .by=c( gas), 
                                   total.diff.mean = mean(total.diff, na.rm=T),
                                   total.diff.sd = sd(total.diff, na.rm=T))

save( ENSEMBLE_DIELS ,ENSEMBLE_DIELS_Site,ENSEMBLE_DIELS_Site_summary,
      file='/Volumes/MaloneLab/Research/FluxGradient/DIEL_SUMMARY_ENSEMBLE_V1.RDATA')


ENSEMBLE_DIELS_EVAL_canopy$Site[ENSEMBLE_DIELS_EVAL_canopy$R2 < 0.45] %>% unique


