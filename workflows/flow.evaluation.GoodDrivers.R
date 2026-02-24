# Good levels:

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


# Set up VSURF ####
# Divide into test and training datasets:
train <- SITES_One2One_canopy %>% 
  sample_frac(0.65) 

test <- anti_join(SITES_One2One_canopy, train )

SITES_One2One_canopy %>% summary
train  %>% summary
test  %>% summary

# Variable Selection : ####

SITES_One2One_canopy %>% names
train[, c(7, 10,73, 81, 82)] %>% names

rf_index.sdesign.vsurf <- VSURF(train[, c(7, 10,73, 81, 82)], 
                            train[["Good.CCC"]],
                            ntree = 1000,
                            RFimplem = "randomForest", 
                            clusterType = "PSOCK", 
                            verbose = TRUE,
                            ncores = parallelly::availableCores() - 2, parallel= TRUE)

rf_index.sdesign.vsurf$varselect.pred
rf_index.sdesign.vars <-names( train[, c(7, 10,73, 81, 82)]) [rf_index.sdesign.vsurf$varselect.interp] 
rf_index.sdesign.vars <- train[, c(7, 10,73, 81, 82)] %>% names

train.sub <- train[, c(21:34, 36:38,80)] %>% na.omit

rf_index.Cspec.vsurf <- VSURF(train.sub[,-18], 
                              train.sub[["Good.CCC"]],
                                ntree = 500,
                              mtry=2,
                                RFimplem = "randomForest", 
                                clusterType = "PSOCK", 
                                verbose = TRUE,
                                ncores = parallelly::availableCores() - 2, parallel= TRUE)

rf_index.Cspec.vsurf$varselect.pred
rf_index.Cspec.vars <-names( train.sub[-18]) [rf_index.Cspec.vsurf$varselect.pred] 



train.sub <- train[, c(40:51, 61:64,80)] %>% na.omit
rf_index.Cstructure.vsurf <- VSURF(train.sub[-17], 
                                   train.sub[["Good.CCC"]],
                                   ntree = 500,
                                   mtry=2,
                                   RFimplem = "randomForest", 
                                   clusterType = "PSOCK", 
                                   verbose = TRUE,
                                   ncores = parallelly::availableCores() - 2, parallel= TRUE)

rf_index.Cstructure.vsurf$varselect.pred
rf_index.Cstructure.vars <-names( train.sub[-17]) [rf_index.Cstructure.vsurf$varselect.pred] 

final.vars <- c(rf_index.sdesign.vars, rf_index.Cspec.vars,rf_index.Cstructure.vars ) 

save(final.vars, file= paste(dir, "FinalVarsSelection.Rdata", sep="") )

# Final Model Fit : ####

load( file= paste(dir, "FinalVarsSelection.Rdata", sep="") )

rf_model <- randomForest( Good.CCC ~ .,
                              data= train %>% select(c(Good.CCC, all_of(final.vars))) %>% na.omit,
                              importance=TRUE,
                              predicted=TRUE,
                              keep.inbag=TRUE)

rf_model 

varImpPlot(rf_model, main="")

library(caret)
train$rf_model <- predict(rf_model , train)
confusionMatrix(train$rf_model, train$Good.CCC)

# Save the model 
save(train, test,rf_model, final.vars, SITES_One2One_canopy, file= paste(dir, "Good_Fluxes.Rdata", sep="") )


# Sensitivity Analysis: # Build file with all fluxes and information of interest.
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


####

load(file= paste(dir, "Good_Fluxes.Rdata", sep="") )
final.vars

SITES_One2One_canopy$Approach %>% unique
SITES_One2One_canopy$Canopy_L1 %>% unique
SITES_One2One_canopy$gas %>% unique

SITES_One2One_canopy$RelativeDistB %>% summary
SITES_One2One_canopy$RelativeDistA %>% summary
SITES_One2One_canopy$Cutoff05.SDH %>% summary
SITES_One2One_canopy$NDVI.mean %>% summary
SITES_One2One_canopy$LAI.sd %>% summary

mean.df <- SITES_One2One_canopy %>% reframe( .by=c(Approach, Canopy_L1, gas), 
                                             RelativeDistB = mean(RelativeDistB, na.rm=T),
                                             RelativeDistA = mean(RelativeDistA, na.rm=T),
                                             Cutoff05.SDH  = mean(Cutoff05.SDH ,na.rm=T),
                                             NDVI.mean = mean(NDVI.mean,na.rm=T),
                                             LAI.sd = mean(LAI.sd,na.rm=T))

min.df <- SITES_One2One_canopy %>% reframe( .by=c(Approach, Canopy_L1, gas), 
                                             RelativeDistB = min(RelativeDistB, na.rm=T),
                                             RelativeDistA = min(RelativeDistA, na.rm=T),
                                             Cutoff05.SDH  = min(Cutoff05.SDH ,na.rm=T),
                                             NDVI.mean = min(NDVI.mean,na.rm=T),
                                             LAI.sd = min(LAI.sd,na.rm=T))

max.df <- SITES_One2One_canopy %>% reframe( .by=c(Approach, Canopy_L1, gas), 
                                             RelativeDistB = max(RelativeDistB, na.rm=T),
                                             RelativeDistA = max(RelativeDistA, na.rm=T),
                                             Cutoff05.SDH  = max(Cutoff05.SDH ,na.rm=T),
                                             NDVI.mean = max(NDVI.mean,na.rm=T),
                                             LAI.sd = max(LAI.sd,na.rm=T))

summary.df <- rbind( mean.df, min.df, max.df)

RelativeDistB.df <- data.frame(  RelativeDistB = seq(SITES_One2One_canopy$ RelativeDistB %>% min(na.rm=T), SITES_One2One_canopy$ RelativeDistB %>% max(na.rm=T), 2) )
RelativeDistA.df <- data.frame(  RelativeDistA = seq(SITES_One2One_canopy$RelativeDistA %>% min(na.rm=T), SITES_One2One_canopy$RelativeDistA %>% max(na.rm=T), 2) )
Cutoff05.SDH.df <- data.frame( Cutoff05.SDH = seq(SITES_One2One_canopy$Cutoff05.SDH %>% min(na.rm=T), SITES_One2One_canopy$Cutoff05.SDH %>% max(na.rm=T), 0.2) )
NDVI.mean.df <- data.frame( NDVI.mean = seq(SITES_One2One_canopy$NDVI.mean %>% min(na.rm=T), SITES_One2One_canopy$EVI.mean %>% max(na.rm=T), 0.01) )
LAI.sd.df <- data.frame( LAI.sd = seq(SITES_One2One_canopy$LAI.sd %>% min(na.rm=T), SITES_One2One_canopy$LAI.sd %>% max(na.rm=T), 0.01) )

# Format Factors

format.factors <- function( data){
  data$Approach <- factor( data$Approach , levels = c("MBR", "AE", "WP"))
  data$Canopy_L1 <- factor( data$Canopy_L1 , levels = c("AA" , "AW", "WW"))
  
  return(data)
}


Cutoff05.SDH.final <- summary.df %>% select(!Cutoff05.SDH) %>% cross_join(Cutoff05.SDH.df )
RelativeDistB.final <- summary.df %>% select(! RelativeDistB) %>% cross_join(RelativeDistB.df )
RelativeDistA.final <- summary.df %>% select(! RelativeDistA) %>% cross_join(RelativeDistA.df )
LAI.sd.final <- summary.df %>% select(! LAI.sd) %>% cross_join(LAI.sd.df )
NDVI.mean.final <- summary.df %>% select(! NDVI.mean) %>% cross_join(NDVI.mean.df )

Cutoff05.SDH.final$model <-  predict(rf_model  ,Cutoff05.SDH.final,'prob')[,2]
NDVI.mean.final$model <-  predict(rf_model  ,NDVI.mean.final,'prob')[,2]
LAI.sd.final$model <-  predict(rf_model  , LAI.sd.final,'prob')[,2]
RelativeDistB.final$model <-  predict(rf_model  , RelativeDistB.final,'prob')[,2]
RelativeDistA.final$model <-  predict(rf_model  , RelativeDistA.final,'prob')[,2]

Sensitivity_plot <- function(df, approach, label, var){

 
  plot <-  ggplot() + geom_smooth(data= df %>% filter(Approach == approach),
                                  aes_string( x= var , y = 'model', col= 'Canopy_L2'), alpha=0.2) + 
    scale_color_manual(values=c("goldenrod4", "goldenrod2",
                                "green1","green3", "aquamarine2","aquamarine4",
                                "purple", "darkmagenta")) +theme_bw() + xlab(label)
  
  return(plot)
}

Sensitivity_plot2 <- function(df, approach, label, var){
  
  
  plot <-  ggplot() + geom_smooth(data= df %>% filter(Approach == approach), col="black",
                                  aes_string( x= var , y = 'model', linetype= 'Canopy_L1'), alpha=0.2)+
    theme_bw() + xlab(label)
  
  return(plot)
}

Sensitivity_approach_plot <- function(approach, labels){
  
  plot.mbr.1 <- Sensitivity_plot2(df = NDVI.mean.final, approach = approach, label= "NDVI", var='NDVI.mean' ) + theme(legend.title = element_blank())

  
  plot.mbr.2 <- Sensitivity_plot2(df = Cutoff05.SDH.final, approach = approach, label= "SDH", var='Cutoff05.SDH' )+ theme(legend.title = element_blank())
  
  plot.mbr.4 <- Sensitivity_plot2(df = RelativeDistB.final, approach = approach, label= "Relative Dist B", var='RelativeDistB' )+ theme(legend.title = element_blank())
  
  plot.mbr.5 <- Sensitivity_plot2(df = RelativeDistA.final, approach = approach, label= "Relative Dist A", var='RelativeDistA' )+ theme(legend.title = element_blank())
  
  plot.mbr.3 <- Sensitivity_plot2(df = LAI.sd.final , approach = approach, label= "LAI", var='LAI.sd' )+ theme(legend.title = element_blank())
  
  
  gd.plot.mbr <- ggarrange(plot.mbr.2,
                           plot.mbr.5,
                           plot.mbr.4,
                           plot.mbr.1,
                           plot.mbr.3, common.legend = TRUE , nrow=1, labels=labels)
  return( gd.plot.mbr )
}

gd.plot.mbr <-Sensitivity_approach_plot(approach = "MBR", labels = c("A", "B", "C", "D", "E"))
gd.plot.ae <-Sensitivity_approach_plot(approach = "AE", labels = c("F", "G", "H", "I", "J"))
gd.plot.wp <-Sensitivity_approach_plot(approach = "WP", labels = c("K", "L", "M", "N", "P"))

final.gd.plot <- ggarrange( gd.plot.mbr, gd.plot.ae, gd.plot.wp, nrow=3, ncol=1, common.legend = TRUE)


ggsave("Figures/GoodData_plot.png", plot = final.gd.plot, width = 11, height = 9, units = "in")


metadata <- read.csv('/Volumes/MaloneLab/Research/FluxGradient/Ameriflux_NEON field-sites.csv') 
site.list <- metadata$Site_Id.NEON %>% unique

rf_model 

