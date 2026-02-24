# Harmonization
# This script explores the distribution of fluxes:
library(ggforce)
library(tidyverse)
library(colorspace)
library(ggpubr)
library(ggplot2)

DirRepo.eval <-"/Users/sm3466/YSE Dropbox/Sparkle Malone/Research/FluxGradient/lterwg-flux-gradient-eval"

source(fs::path(DirRepo.eval, 'functions/calc.One2One.CCC_testing.R'))

localdir <- '/Volumes/MaloneLab/Research/FluxGradient/FluxData'

load( fs::path(localdir,paste0("SITES_One2One.Rdata")))
load( fs::path(localdir,paste0("SITE_DATA_FILTERED_CCC.Rdata")))
load(file= paste(localdir, "SITES_One2One_canopy_model.Rdata", sep="" ))
load(  file= paste(localdir, "SITE_RSHP_MODEL.Rdata", sep="") )

canopy.adj <- val.SHP.total.canopy.summary %>% select( Site, Approach, gas, dLevelsAminusB, Good.CCC) %>% rename( Good.CCC.EC = Good.CCC)

# Build the dataset for canopy Information: ####

metadata.igbp <- read.csv('/Volumes/MaloneLab/Research/FluxGradient/Ameriflux_NEON field-sites.csv') %>% 
  rename(igbp = Vegetation.Abbreviation..IGBP.,
         Site = Site_Id.NEON) %>% select(Site, igbp)

# What is the distribution of fluxes across hours and seasons?

# Ensemble GF Data

site.list <- SITE_DATA_FILTERED_CCC %>% names()
Linear_ENSEMBLE_plots_CO2 <- list()
Linear_ENSEMBLE_plots_H2O <- list()
Linear_ENSEMBLE_Stats <- data.frame()
SITE_DATA_ENSEMBLE <- list()

for( site in site.list){
  print(site)
  SITE_DATA_FILTERED_CCC[[site]] %>% summary
  df <-SITE_DATA_FILTERED_CCC[[site]] %>% 
    mutate(month = format(timeEndA.local,'%m') %>% as.numeric,
           season = case_when(
             month %in% c(12, 1, 2) ~ "Winter",
             month %in% c(3, 4, 5) ~ "Spring",
             month %in% c(6, 7, 8) ~ "Summer",
             TRUE ~ "Autumn"),
           hour = format(timeEndA.local,'%H'),
           count= case_when( is.na(FG_mean) == FALSE ~ 1,
                             TRUE ~ 0)) %>% distinct
  
  
  canopy.sub <- canopy.adj %>% select( Site, dLevelsAminusB, gas, Approach, Good.CCC.EC) %>% filter( Site == site)
  
  df.ENSEMBLE <- df %>% left_join(canopy.sub , by=c("dLevelsAminusB", "gas", "Approach" ), 
                                relationship = "many-to-many") %>% 
    mutate( time.rounded = timeEndA.local %>% round_date( unit = "30 minutes") ) %>% 
    filter( Good.CCC.EC  == '1') %>%
    reframe( .by= c( gas, time.rounded), 
             EC_mean = mean(EC_mean, na.rm=T), 
             FG_ENSEMBLE = mean(FG_mean, na.rm=T))

  
  SITE_DATA_ENSEMBLE[[site]] <-  df.ENSEMBLE
  
  
  if( df.ENSEMBLE %>% filter(gas == "CO2") %>% nrow > 20 ){
    co2.df <- df.ENSEMBLE %>% filter(gas== "CO2")
    df.h.lm.co2 <- lm( data =co2.df,  EC_mean ~FG_ENSEMBLE)%>% summary
    ccc_result.co2 <- calc.lins.ccc(co2.df$EC_mean, co2.df$FG_ENSEMBLE)
    summary.co2.df <- data.frame( Site = site, R2= df.h.lm.co2$r.squared, 
                                  intercept = df.h.lm.co2$coefficients[1],
                                  slope = df.h.lm.co2$coefficients[2],
                                  CCC=ccc_result.co2$rho.c$est) %>% mutate(gas = "CO2")
    
  }else if(df.ENSEMBLE %>% filter(gas == "CO2") %>% nrow <= 20 ){
    
    summary.co2.df <- data.frame( Site = site, 
                                  R2= NA, 
                                  intercept = NA,
                                  slope = NA,
                                  CCC=NA) %>% mutate(gas = "CO2")
  }

    
       
  if( df.ENSEMBLE %>% filter(gas == "H2O") %>% nrow > 20 ){
  h2o.df <- df.ENSEMBLE %>% filter(gas== "H2O")
  df.h.lm.h2o <- lm( data =h2o.df,  EC_mean ~ FG_ENSEMBLE)%>% summary
  ccc_result.h2o <- calc.lins.ccc(h2o.df$EC_mean, h2o.df$FG_ENSEMBLE)
  
  summary.h2o.df <- data.frame( Site = site, 
                                R2= df.h.lm.h2o$r.squared, 
                                intercept = df.h.lm.h2o$coefficients[1],
                                slope = df.h.lm.h2o$coefficients[2],
                                CCC=ccc_result.h2o$rho.c$est) %>% mutate(gas = "H2O")
  
  } else if( df.ENSEMBLE %>% filter(gas == "H2O") %>% nrow <= 20 ){
    
    summary.h2o.df <- data.frame( Site = site, 
                                  R2= NA, 
                                  intercept = NA,
                                  slope = NA,
                                  CCC=NA) %>% mutate(gas = "H2O")
  }
  
  
  # Store information about Harmonization Fit here
  Linear_ENSEMBLE_Stats <- rbind(Linear_ENSEMBLE_Stats, summary.co2.df, summary.h2o.df )  
  
  rm(df.h.lm.co2 ,   df.h.lm.h20)
  
  Linear_ENSEMBLE_plots_CO2[[site]] <- df.ENSEMBLE %>% filter(gas=="CO2") %>% 
    ggplot( aes( x= FG_ENSEMBLE, y = EC_mean)) + 
    geom_point(size=0.1, alpha=0.1) + theme_bw() + xlim(-40, 40) + ylim(-40, 40) +
    geom_abline(intercept = 0, slope = 1, color = "black", linetype = "dashed") +
    geom_smooth(method = "lm", se = FALSE, size=0.5) +
    #stat_regline_equation(aes(label = paste(..eq.label.., ..rr.label.., sep = "~`,`~")), label.x = 2, label.y = c(40, 35, 30)) + xlab('GF') + ylab('EC') + 
    ggtitle(site) + xlab("GF") + ylab("EC")
  
  Linear_ENSEMBLE_plots_H2O[[site]] <- df.ENSEMBLE %>% filter(gas =="H2O") %>% 
    ggplot( aes( x= FG_ENSEMBLE, y = EC_mean)) + 
    geom_point(size=0.1, alpha=0.1) + theme_bw() + xlim(-40, 40) + ylim(-40, 40) +
    geom_abline(intercept = 0, slope = 1, color = "black", linetype = "dashed") +
    geom_smooth(method = "lm", se = FALSE, size=0.5) +
    #stat_regline_equation(aes(label = paste(..eq.label.., ..rr.label.., sep = "~`,`~")), label.x = 2, label.y = c(40, 35, 30)) + xlab('GF') + ylab('EC') + 
    ggtitle(site) + xlab("GF") + ylab("EC")
  
  rm(df.ENSEMBLE, df )
}

# Linear plots by site: ####
plots.linear.co2.1 <- ggarrange(plotlist = Linear_ENSEMBLE_plots_CO2[1:25], common.legend = TRUE)
plots.linear.co2.2 <- ggarrange(plotlist = Linear_ENSEMBLE_plots_CO2[26:50], common.legend = TRUE)

plots.linear.h2o.1 <- ggarrange(plotlist = Linear_ENSEMBLE_plots_H2O[1:25], common.legend = TRUE)
plots.linear.h2o.2 <- ggarrange(plotlist = Linear_ENSEMBLE_plots_H2O[26:50], common.legend = TRUE)

plots.linear.co2 <- ggarrange( plots.linear.co2.1, plots.linear.co2.2, ncol=1)
plots.linear.h2o <- ggarrange( plots.linear.h2o.1, plots.linear.h2o.2, ncol=1)

ggsave("/Users/sm3466/YSE Dropbox/Sparkle Malone/Research/FluxGradient/lterwg-flux-gradient-eval/Figures/WF_Version1/ENSEMBLE_Linear_plots_co2.png", plot = plots.linear.co2, width = 8, height = 16, units = "in")
ggsave("/Users/sm3466/YSE Dropbox/Sparkle Malone/Research/FluxGradient/lterwg-flux-gradient-eval/Figures/WF_Version1/ENSEMBLE_Linear_plots_h2o.png", plot = plots.linear.h2o, width = 8, height = 16, units = "in")

# ENSEMBLE_ linear plots #####
Linear_ENSEMBLE_Stats %>% 
  ggplot(aes(x = CCC, y = Site)) +
  geom_col() +
  theme_bw() +
  facet_wrap(~gas)

# Seasonal Analysis: ####
season <- c("Winter", "Spring", "Summer", "Autumn")
Linear_ENSEMBLE_Season_Stats <-list()

for( site in site.list){
  
  for(i in season){
    print(site)
  print(i)
  
  df <-SITE_DATA_FILTERED_CCC[[site]] %>% filter(gas != "CH4") %>% 
    mutate(month = format(timeEndA.local,'%m') %>% as.numeric,
           season = case_when(
             month %in% c(12, 1, 2) ~ "Winter",
             month %in% c(3, 4, 5) ~ "Spring",
             month %in% c(6, 7, 8) ~ "Summer",
             TRUE ~ "Autumn"),
           hour = format(timeEndA.local,'%H'),
           count= case_when( is.na(FG_mean) == FALSE ~ 1,
                             TRUE ~ 0)) %>% distinct %>% filter( season == i)
  
  
  canopy.sub <- canopy.adj %>% select( Site, dLevelsAminusB, gas, Approach, Good.CCC.EC) %>% filter( Site == site)
  
  df.ENSEMBLE <- df %>% left_join(canopy.sub , by=c("dLevelsAminusB", "gas", "Approach" ), 
                                    relationship = "many-to-many") %>% 
    mutate( time.rounded = timeEndA.local %>% round_date( unit = "30 minutes") ) %>% 
    filter( Good.CCC.EC  == '1') %>%
    reframe( .by= c( gas, time.rounded), 
             EC_mean = mean(EC_mean, na.rm=T), 
             FG_ENSEMBLE = mean(FG_mean, na.rm=T))
  
  
  threshold.co2 <- df.ENSEMBLE$FG_ENSEMBLE[ df.ENSEMBLE$gas == "CO2"] %>% length
  threshold.h2o <- df.ENSEMBLE$FG_ENSEMBLE[ df.ENSEMBLE$gas == "H2O"] %>% length
  
  if( threshold.co2 > 20){
    co2.df <- df.ENSEMBLE %>% filter(gas== "CO2")
    df.h.lm.co2 <- lm( data =co2.df,  EC_mean ~FG_ENSEMBLE)%>% summary
    ccc_result.co2 <- calc.lins.ccc(co2.df$EC_mean, co2.df$FG_ENSEMBLE)
    summary.co2.df <- data.frame( Site = site, R2= df.h.lm.co2$r.squared, CCC=ccc_result.co2$rho.c$est) %>% mutate(gas = "CO2", season=i, count = threshold.co2)
    Linear_ENSEMBLE_Season_Stats <- rbind(Linear_ENSEMBLE_Season_Stats, summary.co2.df)  
  }
   
  if( threshold.h2o >20){
    h2o.df <- df.ENSEMBLE %>% filter(gas== "H2O")
    df.h.lm.h2o <- lm( data =h2o.df,  EC_mean ~ FG_ENSEMBLE)%>% summary
    ccc_result.h2o <- calc.lins.ccc(h2o.df$EC_mean, h2o.df$FG_ENSEMBLE)
    summary.h2o.df <- data.frame( Site = site, R2= df.h.lm.h2o$r.squared, 
                                  CCC=ccc_result.h2o$rho.c$est) %>% 
      mutate(gas = "H2O", season=i, count = threshold.h2o)
    
    Linear_ENSEMBLE_Season_Stats <- rbind(Linear_ENSEMBLE_Season_Stats, summary.h2o.df )  
  }
  rm(df.h.lm.co2 ,   df.h.lm.h20)
  
  }}

plot.CCC.sites <- Linear_ENSEMBLE_Season_Stats %>%
  ggplot(aes(x = CCC, y = Site, col=gas)) +
  geom_point(alpha = 0.5, size =2, shape=15) +
  theme_bw() +
  facet_wrap(~ season, ncol=4) + scale_color_manual(values = c('darkgreen', "blue")) +
  theme( legend.text = element_text(size = 14),
         legend.title = element_text(size = 14),
         strip.background = element_rect(fill = "transparent", 
                                         size = 0.5),
         legend.position = "top")+ 
  ylab("") +  xlab(expression(paste("Ensemble CCC (EC)"))) 

plot.CCC.density <- Linear_ENSEMBLE_Season_Stats %>% ggplot( aes(x = CCC, col=gas)) +
  geom_density() + theme_bw() + ylab("Density" ) + facet_wrap(~season, ncol=4) +
  scale_color_manual(values = c('darkgreen', "blue")) +
  theme( legend.text = element_text(size = 14),
         legend.title = element_text(size = 14),
         strip.background = element_rect(fill = "transparent", size = 0.5),
         legend.position = "none")+ ylab("") +  xlab(expression(paste("Ensemble CCC (EC)"))) +
  ylab("Density")

Linear_ENSEMBLE_Season_Stats %>% reframe(.by=c(gas), 
                                           min.CCC= min(CCC), 
                                           mean.CCC= mean(CCC),
                                           max.CCC= max(CCC),
                                           sd.CCC= sd(CCC))

Linear_ENSEMBLE_Season_Stats %>% reframe(.by=c(gas, season), 
                                           min.CCC= min(CCC), 
                                           mean.CCC= mean(CCC),
                                           max.CCC= max(CCC))



plot.CCC.season <- Linear_ENSEMBLE_Season_Stats %>% ggplot( aes(x = season, y= CCC, col=gas)) +
  geom_boxplot()  +theme_bw() + xlab("Season") +
  scale_color_manual(values = c('#009966', "#000CCC"))  +  
  ylab(expression(paste("Ensemble CCC (EC)"))) + theme( legend.position = "top", legend.title = element_blank()) + ylim(-1,1)

# Plots for ENSEMBLE : ####
sub.sites =c("HARV", "GUAN", "KONZ", "JORN")

SITES_ENSEMBLE_canopy <- val.SHP.total.canopy.summary %>% 
  filter( Good.CCC == '1') %>% 
  full_join(Linear_ENSEMBLE_Season_Stats %>% 
              rename( CCC.ENSEMBLE = CCC), 
            by=c("Site", "gas"), relationship = "many-to-many") %>%  distinct %>% left_join( SITES_One2One_canopy_model %>% select(Site, canopyHeight_m) %>% distinct, by="Site")



plot.CCC.canopy <- SITES_ENSEMBLE_canopy %>% select(Canopy_L1,  CCC.ENSEMBLE, gas) %>% na.omit %>%  ggplot( ) + 
  geom_boxplot( aes( x= Canopy_L1, y = CCC.ENSEMBLE, col=Canopy_L1)) + 
  facet_wrap(~gas) + theme_bw() + 
  scale_colour_discrete_sequential(palette = "OrYel")+
  ylab(expression(paste('Ensemble CCC (EC)'))) + xlab('Canopy Level')+
  theme( legend.text = element_text(size = 14),
         legend.title = element_text(size = 14),
         strip.background = element_rect(fill = "transparent", size = 0.5),
         legend.position = "none") + ylim(-1,1)

plot.CCC.canopy.subsites <- SITES_ENSEMBLE_canopy %>% filter(Site %in% sub.sites) %>% select(Site , Canopy_L1,  CCC.ENSEMBLE, gas) %>% na.omit %>%  ggplot( ) + 
  geom_boxplot( aes( x= Canopy_L1, y = CCC.ENSEMBLE, col=Canopy_L1)) + 
  facet_wrap(~ factor(Site, levels=sub.sites), ncol=1) + theme_bw() + 
  scale_colour_discrete_sequential(palette = "OrYel")+
  ylab('') + xlab('Canopy Level')+
  theme( legend.text = element_text(size = 14),
         legend.title = element_text(size = 14),
         strip.background = element_rect(fill = "transparent", size = 0.5),
         legend.position = "none") + ylim(-1,1) +
  theme(
    axis.title.y = element_blank(), # Removes the axis title
    axis.text.y = element_blank(),  # Removes the tick labels
    axis.ticks.y = element_blank(), # Removes the tick marks
    axis.line.y = element_blank()   # Removes the axis line
  )


plot.CCC.canopyHt <- SITES_ENSEMBLE_canopy %>% ggplot(aes( x= canopyHeight_m, y = CCC.ENSEMBLE) ) + 
  geom_point( ) + facet_wrap(~gas) + geom_smooth(method="lm") + theme_bw() + xlab('Canopy Height (m)') +
  ylab(expression(paste("Ensemble CCC (EC)"))) +
  theme( legend.text = element_text(size = 14),
         legend.title = element_text(size = 14),
         strip.background = element_rect(fill = "transparent", size = 0.5),
         legend.position = "none") + ylim(-1,1)


# ENSEMBLE_ plots R2 by Site Plot: ####

final.plot.CCC <- ggarrange(plot.CCC.sites ,plot.CCC.density , ncol= 1, heights = c(2.5, 1), 
          labels=c("A", "B"))

final.plot.CCC.support <- ggarrange( plot.CCC.canopy,plot.CCC.canopyHt,  ncol=1,
           labels=c("A", "B"), heights=c(1, 1)) 


ggsave("/Users/sm3466/YSE Dropbox/Sparkle Malone/Research/FluxGradient/lterwg-flux-gradient-eval/Figures/WF_Version1/ENSEMBLE_Eval_R2_V1.png", plot = final.plot.CCC, width = 7, height = 8, units = "in")
ggsave("/Users/sm3466/YSE Dropbox/Sparkle Malone/Research/FluxGradient/lterwg-flux-gradient-eval/Figures/WF_Version1/ENSEMBLE_Eval_R2_Canopy_V1.png", plot = final.plot.CCC.support, width = 6, height = 10, units = "in")

# Save the data: #####

fileSave <- fs::path(localdir,paste0("SITE_DATA_ENSEMBLE_V1.Rdata"))

save( SITE_DATA_ENSEMBLE,
      Linear_ENSEMBLE_plots_H2O, 
      Linear_ENSEMBLE_plots_CO2,  
      Linear_ENSEMBLE_Stats,
      Linear_ENSEMBLE_Season_Stats,
      file=fileSave)

googledrive::drive_upload(media = fileSave, overwrite = T, path = drive_url)

# Linear terms versus canopy:
Linear_ENSEMBLE_Stats_canopy <- Linear_ENSEMBLE_Stats %>% 
  full_join(SITES_One2One_canopy_model , by=c('Site', 'gas'))

Linear_ENSEMBLE_Stats_canopy %>% 
  ggplot(aes(x = Cutoff05.SDSDH, y =slope )) +
  geom_point() +
  theme_bw() +
  facet_wrap(~gas)

# ENSEMBLE_ MS by IGBP : ####
source(fs::path(DirRepo.eval,'workflows/flow.igbp.R'))

SITES_One2One_canopy_summary <- SITES_One2One_canopy_model %>% 
  full_join(canopy.adj, by=c('Site', 'Approach', 'gas', 'dLevelsAminusB')) %>% 
  filter( Good.CCC.EC =="1") %>% 
  select( Site, Approach, gas, dLevelsAminusB, R2, Cutoff05.TopRugosity, count, Cutoff05.SDSDH, CCC) %>% reframe( .by= c(Site, gas), 
           CCC= mean(CCC) %>% round(2), 
           count = sum(count),
           Rugosity = mean( Cutoff05.TopRugosity),
           Cutoff05.SDSDH = mean(Cutoff05.SDSDH))

# Plots: 

Linear_ENSEMBLE_Stats_Canopy <- 
  Linear_ENSEMBLE_Stats %>% rename(CCC.ENSEMBLE = CCC) %>% 
  full_join(SITES_One2One_canopy_summary, by=c("Site", "gas"), relationship = "many-to-many") %>% full_join(metadata.igbp , by="Site")
 
plot_ENSEMBLE_siteLevel.CCC.sub.subsites  <- Linear_ENSEMBLE_Stats_Canopy %>% filter(Site %in% sub.sites)  %>% 
  ggplot() + geom_point( aes( x=CCC, y=CCC.ENSEMBLE, col=gas)) +
  geom_abline( slope = 1, color = "red", linetype = "dashed") + theme_bw() + xlab("CCC") + ylab("Ensemble CCC (EC)") + ylim(-1, 1)+ xlim(-1, 1) + 
  scale_color_manual(values = c('#009966', "#000CCC", ""))+ 
  facet_wrap( ~ factor(Site, levels=sub.sites), ncol=1) +
  theme(legend.title = element_blank(), 
        legend.position ="none",
        strip.background = element_rect(fill = "transparent", size = 0.5))



plot_ENSEMBLE_siteLevel.CCC <- Linear_ENSEMBLE_Stats_Canopy  %>% ggplot() + geom_point( aes( x=CCC, y=CCC.ENSEMBLE, col=gas)) +
  geom_abline( slope = 1, color = "red", linetype = "dashed") + theme_bw() + xlab("CCC (EC)") + ylab("Ensemble CCC (EC)") + ylim(-1, 1)+ xlim(-1, 1) + 
  scale_color_manual(values = c('#009966', "#000CCC", "")) +
  theme(legend.title = element_blank()) 

plot_ENSEMBLE_siteLevel_sampleSize <-Linear_ENSEMBLE_Stats_Canopy %>% drop_na(gas) %>% ggplot() + geom_point( aes( x=count, y=CCC.ENSEMBLE, col=gas))  + theme_bw() + xlab("Sample Size") + ylab("Ensemble CCC (EC)")+ scale_color_manual(values = c('#009966', "#000CCC")) +theme(legend.title = element_blank())+geom_smooth(method="lm", aes(  x=count, y=CCC.ENSEMBLE),col="purple" )

plot_ENSEMBLE_siteLevelR2_Rugosity <-Linear_ENSEMBLE_Stats_Canopy %>% 
  ggplot(aes( x=Rugosity, y=CCC.ENSEMBLE, col=gas)) + 
  geom_point(alpha=0.5)  + theme_bw() + xlab("Rugosity") + 
  ylab(" Ensemble CCC (EC)") + geom_smooth( method="lm", col="red", linetype = "dashed") + 
         scale_color_manual(values = c('#009966', "#000CCC", "")) + ylim(-1,1)

plot.CCC.EcoType <- Linear_ENSEMBLE_Stats_Canopy %>% ggplot() + geom_boxplot( aes( x=EcoType, y=CCC.ENSEMBLE, col=gas))  + theme_bw() + xlab("") + ylab("Ensemble CCC (EC)")+ scale_color_manual(values = c('#009966', "#000CCC", "")) +theme(legend.title = element_blank(), legend.position="none")+ ylim(-1,1)

plot.CCC.EcoType.samplesize <- Linear_ENSEMBLE_Stats_Canopy %>% ggplot() + geom_boxplot( aes( x=EcoType, y=count, col=gas))  + theme_bw() + ylab("Sample Size") + xlab("") 


plot.CCC.season <- Linear_ENSEMBLE_Season_Stats %>% ggplot( aes(x = season, y= R2, col=gas)) +
  geom_boxplot()  +theme_bw() + xlab("Season") +
  scale_color_manual(values = c('#009966', "#000CCC"))  +  
  ylab("Ensemble CCC (EC)") + 
  theme( legend.position = "top", legend.title = element_blank() ) + ylim(-1,1)


# ENSEMBLE_ Figure Configuration: ####



plots.1.final <- ggarrange( plot_ENSEMBLE_siteLevel.CCC,
                            plot_ENSEMBLE_siteLevelR2_Rugosity,
                             ncol=2, nrow=1, labels = c("A", "B"), common.legend = T)

plots.2.final <- ggarrange(plot.CCC.EcoType ,
                            plot.CCC.canopy ,
                             ncol=2, nrow=1, labels = c("C", "D"))


plots.3.final <- ggarrange(plot.CCC.season, labels = "E")

ENSEMBLE_.plot.final <-ggarrange(plots.1.final,plots.2.final,plots.3.final,  ncol=1 , heights= c(1,1,1) )

ggsave("/Users/sm3466/YSE Dropbox/Sparkle Malone/Research/FluxGradient/lterwg-flux-gradient-eval/Figures/WF_Version1/ENSEMBLE_Eval_R2_Summary_V1.png", plot = ENSEMBLE_.plot.final, width =8, height = 9, units = "in")


Linear_ENSEMBLE_Stats_Canopy  %>% 
  reframe(.by=gas, CCC.mean = CCC.ENSEMBLE %>% mean(na.rm=T),
          CC.sd = CCC.ENSEMBLE %>% sd(na.rm=T))

plot.insert <- ggarrange( plot_ENSEMBLE_siteLevel.CCC.sub.subsites,
                          plot.CCC.canopy.subsites, ncol=2,
                          widths = c(1, 0.74))

ggsave("/Users/sm3466/YSE Dropbox/Sparkle Malone/Research/FluxGradient/lterwg-flux-gradient-eval/Figures/WF_Version1/ENSEMBLE_Insert_V1.png", plot =plot.insert, width =3, height = 5, units = "in")

