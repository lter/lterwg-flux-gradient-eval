# Harmonization
# This script explores the distribution of fluxes:
library(ggforce)
library(tidyverse)
library(colorspace)
library(ggpubr)
library(ggplot2)

localdir <- '/Volumes/MaloneLab/Research/FluxGradient/FluxData'
DirRepo <-"/Users/sm3466/YSE Dropbox/Sparkle Malone/Research/FluxGradient/lterwg-flux-gradient-eval"
drive_url <- googledrive::as_id("https://drive.google.com/drive/folders/1Q99CT77DnqMl2mrUtuikcY47BFpckKw3")

email <- 'sparklelmalone@gmail.com'
googledrive::drive_auth(email = TRUE) 

googledrive::drive_auth(email = email) # Likely will not work on RStudio Server. If you get an error, try email=TRUE to open an interactive auth session.
data_folder <- googledrive::drive_ls(path = drive_url)

load( fs::path(localdir,paste0("SITES_One2One.Rdata")))
load( fs::path(localdir,paste0("SITE_DATA_FILTERED.Rdata")))
load(paste(localdir, "SITES_One2One_canopy_model_CCCGF.Rdata", sep="") )

canopy.adj <- val.SHP.resample %>% select( Site, Approach, gas, dLevelsAminusB, Good.CCC.FG.rf, Good.CCC.EC ,Good.CCC.GF) %>% rename( Good.CCC.EC.FG = Good.CCC.FG.rf)

canopy.adj$Site %>% unique
# Build the dataset for canopy Information: ####

metadata.igbp <- read.csv('/Volumes/MaloneLab/Research/FluxGradient/Ameriflux_NEON field-sites.csv') %>% 
  rename(igbp = Vegetation.Abbreviation..IGBP.,
         Site = Site_Id.NEON) %>% select(Site, igbp)

# What is the distribution of fluxes across hours and seasons?

# Harmonize Data

site.list <- SITE_DATA_FILTERED %>% names()
Linear_Harmonized_plots_CO2 <- list()
Linear_Harmonized_plots_H2O <- list()
Linear_Harmonized_Stats <- data.frame()
SITE_DATA_Harmonized <- list()

Linear_Harmonized_plots_CO2[['ONAQ']]
Linear_Harmonized_plots_H2O[['ONAQ']]

# PUUM and TOOL have no flux data left!
for( site in site.list[-c(31, 42)]){
  print(site)
  
  df <-SITE_DATA_FILTERED[[site]] %>% filter(gas != "CH4") %>% 
    mutate(month = format(timeEndA.local,'%m') %>% as.numeric,
           season = case_when(
             month %in% c(12, 1, 2) ~ "Winter",
             month %in% c(3, 4, 5) ~ "Spring",
             month %in% c(6, 7, 8) ~ "Summer",
             TRUE ~ "Autumn"),
           hour = format(timeEndA.local,'%H'),
           count= case_when( is.na(FG_mean) == FALSE ~ 1,
                             TRUE ~ 0)) %>% distinct
  
  
  canopy.sub <- canopy.adj %>% select( Site, dLevelsAminusB, gas, Approach, Good.CCC.EC.FG) %>% filter( Site == site)
  
  df.harmonized <- df %>% left_join(canopy.sub , by=c("dLevelsAminusB", "gas", "Approach" ), 
                                relationship = "many-to-many") %>% 
    mutate( time.rounded = timeEndA.local %>% round_date( unit = "30 minutes") ) %>% 
    filter( Good.CCC.EC.FG  == '1') %>%
    reframe( .by= c( gas, time.rounded), 
             EC_mean = mean(EC_mean, na.rm=T), 
             FG_harmonized = mean(FG_mean, na.rm=T))
  
  
  SITE_DATA_Harmonized[[site]] <-  df.harmonized
  
  
  if( df.harmonized %>% filter(gas == "CO2") %>% nrow > 20 ){
    df.h.lm.co2 <- lm( data =df.harmonized %>% filter(gas== "CO2"),  EC_mean ~FG_harmonized)%>% summary
    
    summary.co2.df <- data.frame( Site = site, R2= df.h.lm.co2$r.squared, 
                                  intercept = df.h.lm.co2$coefficients[1],
                                  slope = df.h.lm.co2$coefficients[2]) %>% mutate(gas = "CO2")
  }else if(df.harmonized %>% filter(gas == "CO2") %>% nrow <= 20 ){
    
    summary.co2.df <- data.frame( Site = site, 
                                  R2= NA, 
                                  intercept = NA,
                                  slope = NA) %>% mutate(gas = "CO2")
  }

    
       
  if( df.harmonized %>% filter(gas == "H2O") %>% nrow > 20 ){
  
  df.h.lm.h2o <- lm( data =df.harmonized %>% filter(gas== "H2O"),  EC_mean ~ FG_harmonized)%>% summary

  summary.h2o.df <- data.frame( Site = site, 
                                R2= df.h.lm.h2o$r.squared, 
                                intercept = df.h.lm.h2o$coefficients[1],
                                slope = df.h.lm.h2o$coefficients[2]) %>% mutate(gas = "H2O")
  
  } else if( df.harmonized %>% filter(gas == "H2O") %>% nrow <= 20 ){
    
    summary.h2o.df <- data.frame( Site = site, 
                                  R2= NA, 
                                  intercept = NA,
                                  slope = NA) %>% mutate(gas = "H2O")
  }
  
  
  # Store information about Harmonization Fit here
  Linear_Harmonized_Stats <- rbind(Linear_Harmonized_Stats, summary.co2.df, summary.h2o.df )  
  
  rm(df.h.lm.co2 ,   df.h.lm.h20)
  
  Linear_Harmonized_plots_CO2[[site]] <- df.harmonized %>% filter(gas=="CO2") %>% 
    ggplot( aes( x= FG_harmonized, y = EC_mean)) + 
    geom_point(size=0.1, alpha=0.1) + theme_bw() + xlim(-40, 40) + ylim(-40, 40) +
    geom_abline(intercept = 0, slope = 1, color = "black", linetype = "dashed") +
    geom_smooth(method = "lm", se = FALSE, size=0.5) +
    #stat_regline_equation(aes(label = paste(..eq.label.., ..rr.label.., sep = "~`,`~")), label.x = 2, label.y = c(40, 35, 30)) + xlab('GF') + ylab('EC') + 
    ggtitle(site) + xlab("GF") + ylab("EC")
  
  Linear_Harmonized_plots_H2O[[site]] <- df.harmonized %>% filter(gas =="H2O") %>% 
    ggplot( aes( x= FG_harmonized, y = EC_mean)) + 
    geom_point(size=0.1, alpha=0.1) + theme_bw() + xlim(-40, 40) + ylim(-40, 40) +
    geom_abline(intercept = 0, slope = 1, color = "black", linetype = "dashed") +
    geom_smooth(method = "lm", se = FALSE, size=0.5) +
    #stat_regline_equation(aes(label = paste(..eq.label.., ..rr.label.., sep = "~`,`~")), label.x = 2, label.y = c(40, 35, 30)) + xlab('GF') + ylab('EC') + 
    ggtitle(site) + xlab("GF") + ylab("EC")
  
  rm(df.harmonized, df )
}

# Linear plots by site: ####
plots.linear.co2.1 <- ggarrange(plotlist = Linear_Harmonized_plots_CO2[1:25], common.legend = TRUE)
plots.linear.co2.2 <- ggarrange(plotlist = Linear_Harmonized_plots_CO2[26:50], common.legend = TRUE)

plots.linear.h2o.1 <- ggarrange(plotlist = Linear_Harmonized_plots_H2O[1:25], common.legend = TRUE)
plots.linear.h2o.2 <- ggarrange(plotlist = Linear_Harmonized_plots_H2O[26:50], common.legend = TRUE)

plots.linear.co2 <- ggarrange( plots.linear.co2.1, plots.linear.co2.2, ncol=1)
plots.linear.h2o <- ggarrange( plots.linear.h2o.1, plots.linear.h2o.2, ncol=1)

ggsave("/Users/sm3466/YSE Dropbox/Sparkle Malone/Research/FluxGradient/lterwg-flux-gradient-eval/Figures/Linear_plots_co2.png", plot = plots.linear.co2, width = 8, height = 16, units = "in")
ggsave("/Users/sm3466/YSE Dropbox/Sparkle Malone/Research/FluxGradient/lterwg-flux-gradient-eval/Figures/Linear_plots_h2o.png", plot = plots.linear.h2o, width = 8, height = 16, units = "in")

# Harmonized linear plots #####
Linear_Harmonized_Stats %>% 
  ggplot(aes(x = R2, y = Site)) +
  geom_col() +
  theme_bw() +
  facet_wrap(~gas)

# Seasonal Analysis: ####
season <- c("Winter", "Spring", "Summer", "Autumn")
Linear_Harmonized_Season_Stats <-list()

for( site in site.list[-c(31, 42)]){
  for(i in season){
    print(site)
  print(i)
  
  df <-SITE_DATA_FILTERED[[site]] %>% filter(gas != "CH4") %>% 
    mutate(month = format(timeEndA.local,'%m') %>% as.numeric,
           season = case_when(
             month %in% c(12, 1, 2) ~ "Winter",
             month %in% c(3, 4, 5) ~ "Spring",
             month %in% c(6, 7, 8) ~ "Summer",
             TRUE ~ "Autumn"),
           hour = format(timeEndA.local,'%H'),
           count= case_when( is.na(FG_mean) == FALSE ~ 1,
                             TRUE ~ 0)) %>% distinct
  
  
  canopy.sub <- canopy.adj %>% select( Site, dLevelsAminusB, gas, Approach, Good.CCC.EC.FG) %>% filter( Site == site)
  
  df.harmonized <- df %>% left_join(canopy.sub , by=c("dLevelsAminusB", "gas", "Approach" ), 
                                    relationship = "many-to-many") %>% 
    mutate( time.rounded = timeEndA.local %>% round_date( unit = "30 minutes") ) %>% 
    filter( Good.CCC.EC.FG  == '1') %>%
    reframe( .by= c( gas, time.rounded), 
             EC_mean = mean(EC_mean, na.rm=T), 
             FG_harmonized = mean(FG_mean, na.rm=T))
  
  
  threshold.co2 <- df.harmonized$FG_harmonized[ df.harmonized$gas == "CO2"] %>% length
  threshold.h2o <- df.harmonized$FG_harmonized[ df.harmonized$gas == "H2O"] %>% length
  
  if( threshold.co2 > 30){
    df.h.lm.co2 <- lm( data =df.harmonized %>% filter(gas== "CO2"),  EC_mean ~FG_harmonized)%>% summary
    summary.co2.df <- data.frame( Site = site, R2= df.h.lm.co2$r.squared) %>% mutate(gas = "CO2", season=i, count = threshold.co2)
    Linear_Harmonized_Season_Stats <- rbind(Linear_Harmonized_Season_Stats, summary.co2.df)  
  }
   
  if( threshold.h2o >30){
    df.h.lm.h2o <- lm( data =df.harmonized %>% filter(gas== "H2O"),  EC_mean ~FG_harmonized)%>% summary
    summary.h2o.df <- data.frame( Site = site, R2= df.h.lm.h2o$r.squared) %>% mutate(gas = "H2O", season=i, count = threshold.h2o)
    Linear_Harmonized_Season_Stats <- rbind(Linear_Harmonized_Season_Stats, summary.h2o.df )  
  }
  rm(df.h.lm.co2 ,   df.h.lm.h20)
  
  }}

plot.r2 <- Linear_Harmonized_Season_Stats %>%
  ggplot(aes(x = R2, y = Site, col=gas)) +
  geom_point(alpha = 1, size =2, shape=15) +
  theme_bw() +
  facet_wrap(~ season, ncol=4) + scale_color_manual(values = c('darkgreen', "blue")) +
  theme( legend.text = element_text(size = 14),
         legend.title = element_text(size = 14),
         strip.background = element_rect(fill = "transparent", size = 0.5),
         legend.position = "top")+ ylab("") +  xlab(expression(paste("Harmonized GF R"^2))) 

plot.r2.density <- Linear_Harmonized_Season_Stats %>% ggplot( aes(x = R2, col=gas)) +
  geom_density() + theme_bw() + ylab("Density" ) + facet_wrap(~season, ncol=4) +
  scale_color_manual(values = c('darkgreen', "blue")) +
  theme( legend.text = element_text(size = 14),
         legend.title = element_text(size = 14),
         strip.background = element_rect(fill = "transparent", size = 0.5),
         legend.position = "none")+ ylab("") +  xlab(expression(paste("Harmonized GF R"^2))) +
  ylab("Density")

Linear_Harmonized_Season_Stats %>% reframe(.by=c(gas), 
                                           min.R2= min(R2), 
                                           mean.R2= mean(R2),
                                           max.R2= max(R2),
                                           sd.R2= sd(R2))

Linear_Harmonized_Season_Stats %>% reframe(.by=c(gas, season), 
                                           min.R2= min(R2), 
                                           mean.R2= mean(R2),
                                           max.R2= max(R2))

plot.r2.ellipse <- Linear_Harmonized_Season_Stats %>% ggplot( aes(x = R2, y= season, col=gas)) +
  geom_density() +
  geom_mark_ellipse() +theme_bw() + xlim(0, 1) + ylab("Season") +
  scale_color_manual(values = c('#009966', "#000CCC"))  +  
  xlab(expression(paste("Harmonized R"^2))) + theme( legend.position = "top", legend.title = element_blank())


plot.r2.season <- Linear_Harmonized_Season_Stats %>% ggplot( aes(x = season, y= R2, col=gas)) +
  geom_boxplot()  +theme_bw() + xlab("Season") +
  scale_color_manual(values = c('#009966', "#000CCC"))  +  
  ylab(expression(paste("Harmonized R"^2))) + theme( legend.position = "top", legend.title = element_blank())

# Relationship between R2 and counts...

Linear_Harmonized_Season_Stats %>% ggplot( aes(x = count, y= R2, col=season)) +
  geom_point() + facet_wrap(~gas) + geom_smooth()


# Sample plots:
sample.plots.co2 <- c(
Linear_Harmonized_plots_CO2$JORN,
Linear_Harmonized_plots_CO2$KONZ,
Linear_Harmonized_plots_CO2$GUAN,
Linear_Harmonized_plots_CO2$HARV)

sample.plots.h2o <- c(
  Linear_Harmonized_plots_H2O$JORN,
  Linear_Harmonized_plots_H2O$KONZ,
  Linear_Harmonized_plots_H2O$GUAN,
  Linear_Harmonized_plots_H2O$HARV)

# Plots for Harmonized : ####

SITES_HARMONIZED_canopy <- val.SHP.resample %>% filter( Good.CCC.FG.rf == '1') %>% 
  full_join(Linear_Harmonized_Season_Stats %>% 
              rename( R2.harmonized = R2), by=c("Site", "gas"), relationship = "many-to-many") %>% distinct %>% left_join( SITES_One2One_canopy_model %>% select(Site, canopyHeight_m) %>% distinct, by="Site")



plot.r2.canopy <- SITES_HARMONIZED_canopy %>% select(Canopy_L1,  R2.harmonized, gas) %>% na.omit %>%  ggplot( ) + 
  geom_boxplot( aes( x= Canopy_L1, y = R2.harmonized, col=Canopy_L1)) + 
  facet_wrap(~gas) + theme_bw() + 
  scale_colour_discrete_sequential(palette = "OrYel")+
  ylab(expression(paste("Harmonized R"^2))) + xlab('Canopy Level')+
  theme( legend.text = element_text(size = 14),
         legend.title = element_text(size = 14),
         strip.background = element_rect(fill = "transparent", size = 0.5),
         legend.position = "none") 


plot.r2.canopyHt <- SITES_HARMONIZED_canopy %>% ggplot(aes( x= canopyHeight_m, y = R2.harmonized) ) + 
  geom_point( ) + facet_wrap(~gas) + geom_smooth() + theme_bw() + xlab('Canopy Height (m)') +
  ylab(expression(paste("Harmonized GF R"^2))) +
  theme( legend.text = element_text(size = 14),
         legend.title = element_text(size = 14),
         strip.background = element_rect(fill = "transparent", size = 0.5),
         legend.position = "none")


# Harmonized plots R2 by Site Plot: ####

final.plot.r2 <- ggarrange(plot.r2 ,plot.r2.density , ncol= 1, heights = c(2.5, 1), 
          labels=c("A", "B"))

final.plot.r2.support <- ggarrange( plot.r2.ellipse,plot.r2.canopy,plot.r2.canopyHt,  ncol=1,
           labels=c("A", "B", "C"), heights=c(2.5, 1, 1)) 


ggsave("/Users/sm3466/YSE Dropbox/Sparkle Malone/Research/FluxGradient/lterwg-flux-gradient-eval/Figures/Harmonized_Eval_R2.png", plot = final.plot.r2, width = 7, height = 8, units = "in")
ggsave("/Users/sm3466/YSE Dropbox/Sparkle Malone/Research/FluxGradient/lterwg-flux-gradient-eval/Figures/Harmonized_Eval_R2_Canopy.png", plot = final.plot.r2.support, width = 6, height = 10, units = "in")

# Save the data: #####

fileSave <- fs::path(localdir,paste0("SITE_DATA_Harmonized.Rdata"))

save( SITE_DATA_Harmonized,
      Linear_Harmonized_plots_H2O, 
      Linear_Harmonized_plots_CO2,  
      Linear_Harmonized_Stats,
      Linear_Harmonized_Season_Stats,
      file=fileSave)
googledrive::drive_upload(media = fileSave, overwrite = T, path = drive_url)

# Linear terms versus canopy:
Linear_Harmonized_Stats_canopy <- Linear_Harmonized_Stats %>% 
  full_join(SITES_One2One_canopy_model , by=c('Site', 'gas'))

Linear_Harmonized_Stats_canopy %>% 
  ggplot(aes(x = Cutoff05.SDSDH, y =slope )) +
  geom_point() +
  theme_bw() +
  facet_wrap(~gas)

# Harmonized MS by IGBP : ####
source(fs::path(DirRepo,'workflows/flow.igbp.R'))

SITES_One2One_canopy_summary <- SITES_One2One_canopy_model %>% 
  full_join(canopy.adj, by=c('Site', 'Approach', 'gas', 'dLevelsAminusB')) %>% 
  filter( Good.CCC.EC.FG =="1") %>% 
  select( Site, Approach, gas, dLevelsAminusB, R2, Cutoff05.TopRugosity, count, Cutoff05.SDSDH) %>% 
  reframe( .by= Site, 
           R2= mean(R2) %>% round(2), 
           count = sum(count),
           Rugosity = mean( Cutoff05.TopRugosity),
           Cutoff05.SDSDH = mean(Cutoff05.SDSDH))

# Plots: 

Linear_Harmonized_Stats_Canopy <- 
  Linear_Harmonized_Stats %>% rename(R2.Harmonized = R2) %>% 
  full_join(SITES_One2One_canopy_summary, by="Site", relationship = "many-to-many") %>% 
  full_join(metadata.igbp , by="Site")

plot_harmonized_siteLevelR2 <- Linear_Harmonized_Stats_Canopy %>% drop_na(gas) %>% ggplot() + geom_point( aes( x=R2, y=R2.Harmonized, col=gas)) +
  geom_abline( slope = 1, color = "red", linetype = "dashed") + theme_bw() + xlab(expression(paste("R"^2))) + ylab(expression(paste("Harmonized R"^2))) + xlim(0, 0.8) + scale_color_manual(values = c('#009966', "#000CCC", "")) +theme(legend.title = element_blank()) 

plot_harmonized_siteLevelR2_sampleSize <-Linear_Harmonized_Stats_Canopy %>% drop_na(gas) %>% ggplot() + geom_point( aes( x=count, y=R2.Harmonized, col=gas))  + theme_bw() + xlab("Sample Size") + ylab(expression(paste("Harmonized R"^2)))+ scale_color_manual(values = c('#009966', "#000CCC")) +theme(legend.title = element_blank())+geom_smooth(aes(  x=count, y=R2.Harmonized),col="purple" )

plot_harmonized_siteLevelR2_Rugosity <-Linear_Harmonized_Stats_Canopy %>% drop_na(gas)%>% ggplot(aes( x=Rugosity, y=R2.Harmonized, col=gas)) + geom_point()  + theme_bw() + xlab("Rugosity") + ylab(expression(paste("Harmonized R"^2))) + geom_smooth( col="purple") + scale_color_manual(values = c('#009966', "#000CCC", "")) 

plot.R2.EcoType <- Linear_Harmonized_Stats_Canopy %>% ggplot() + geom_boxplot( aes( x=EcoType, y=R2.Harmonized, col=gas))  + theme_bw() + xlab("") + ylab(expression(paste("Harmonized R"^2)))+ scale_color_manual(values = c('#009966', "#000CCC", "")) +theme(legend.title = element_blank(), legend.position="none")

plot.R2.EcoType.samplesize <- Linear_Harmonized_Stats_Canopy %>% ggplot() + geom_boxplot( aes( x=EcoType, y=count, col=gas))  + theme_bw() + ylab("Sample Size") + xlab("") 


plot.r2.season <- Linear_Harmonized_Season_Stats %>% ggplot( aes(x = season, y= R2, col=gas)) +
  geom_boxplot()  +theme_bw() + xlab("Season") +
  scale_color_manual(values = c('#009966', "#000CCC"))  +  
  ylab(expression(paste("Harmonized R"^2))) + theme( legend.position = "top", legend.title = element_blank())

# Harmonized Figure Configuration: ####

plots.1.final <- ggarrange(  plot_harmonized_siteLevelR2 ,
                             plot_harmonized_siteLevelR2_sampleSize,
                             plot_harmonized_siteLevelR2_Rugosity,
                             ncol=3, nrow=1, labels = c("A", "B", "C"), common.legend = T)

plots.2.final <- ggarrange(  plot.r2.canopy,
                             plot.R2.EcoType ,
                             ncol=2, nrow=1, labels = c("D", "E"))


plots.3.final <- ggarrange(plot.r2.season, labels = "F")

harmonized.plot.final <-ggarrange(plots.1.final,plots.2.final,plots.3.final,  ncol=1 , heights= c(1,1,1) )

ggsave("/Users/sm3466/YSE Dropbox/Sparkle Malone/Research/FluxGradient/lterwg-flux-gradient-eval/Figures/Harmonized_Eval_R2_Summary.png", plot = harmonized.plot.final, width =8, height = 9, units = "in")


Linear_Harmonized_Stats_Canopy  %>% reframe(.by=gas, R2.mean = R2.Harmonized %>% mean(na.rm=T),
                                            , R2.sd = R2.Harmonized %>% sd(na.rm=T))

