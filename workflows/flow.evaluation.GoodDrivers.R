# Good levels:

rm(list=ls())

library(dplyr)
library(ggplot2)
library(ggridges)
library(ggpubr)
library(VSURF)
library(randomForest)

dir <- '/Volumes/MaloneLab/Research/FluxGradient/RandomForestModel/'
localdir <- '/Volumes/MaloneLab/Research/FluxGradient/FluxData'
load(fs::path(localdir,paste0("SITES_One2One.Rdata")))
canopy <- read.csv(file.path(paste(localdir, "canopy_commbined.csv", sep="/"))) %>% distinct

Highest.CCC <- SITES_One2One %>% reframe(.by= c(Site, gas, Approach), CCC.max = max(CCC, na.rm=T))

# Define the Good
SITES_One2One_canopy <- SITES_One2One %>% full_join(canopy, by=c("Site", "dLevelsAminusB" ) ) %>% 
  full_join( Highest.CCC,by = c("Site", "gas", "Approach")) %>% mutate(Approach = factor(Approach, levels = c("MBR", "AE", "WP") ),
                                                                       Good.CCC = case_when( gas == 'CO2' & CCC >= 0.5 & Approach == "MBR" ~ 1,
                                                                                             gas == 'CO2' &CCC >= 0.75 & Approach == "WP" ~ 1,
                                                                                             gas == 'CO2' &CCC >= 0.7 & Approach == "AE" ~ 1,
                                                                                             gas == 'CO2' &CCC < 0.5 & Approach == "MBR" ~ 0,
                                                                                             gas == 'CO2' & CCC < 0.75 & Approach == "WP" ~ 0,
                                                                                             gas == 'CO2' &CCC < 0.7 & Approach == "AE" ~ 0,
                                                                                             
                                                                                             gas == 'H2O' & CCC < 0.5  ~ 0,
                                                                                             gas == 'H2O' & CCC > 0.5  ~ 1) %>% as.factor)
# Set up VSURF

SITES_One2One_canopy %>% names

# Divide into test and training datasets:
train <- SITES_One2One_canopy %>% 
  sample_frac(0.65) 

test <- anti_join(SITES_One2One_canopy, train )

SITES_One2One_canopy %>% summary
train  %>% summary
test  %>% summary

# Variable Selection : ####

SITES_One2One_canopy %>% names
train[, c(7:8, 10,78)] %>% names

rf_index.sdesign.vsurf <- VSURF(train[, c(7:8, 10,78)], 
                            train[["Good.CCC"]],
                            ntree = 500,
                            RFimplem = "randomForest", 
                            clusterType = "PSOCK", 
                            verbose = TRUE,
                            ncores = parallelly::availableCores() - 2, parallel= TRUE)

rf_index.sdesign.vsurf$varselect.pred
rf_index.sdesign.vars <-names( train[, c(7:8, 10,78)]) [rf_index.sdesign.vsurf$varselect.interp] 

train[, c(21:34, 36:38,80)] %>% names
train.sub <- train[, c(21:34, 36:38,80)] %>% na.omit
rf_index.Cspec.vsurf <- VSURF(train.sub[-18], 
                              train.sub[["Good.CCC"]],
                                ntree = 500,
                              mtry=2,
                                RFimplem = "randomForest", 
                                clusterType = "PSOCK", 
                                verbose = TRUE,
                                ncores = parallelly::availableCores() - 2, parallel= TRUE)

rf_index.Cspec.vsurf$varselect.pred
rf_index.Cspec.vars <-names( train.sub[-18]) [rf_index.Cspec.vsurf$varselect.interp] 



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


# Final Model Fit : ####
rf_index.Cstructure.vars
rf_index.Cspec.vars 
rf_index.sdesign.vars

final.vars <- c(rf_index.sdesign.vars, rf_index.Cspec.vars,rf_index.Cstructure.vars )   

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


# Sensitivity Analysis: ####
load(file= paste(dir, "Good_Fluxes.Rdata", sep="") )

SITES_One2One_canopy$Approach %>% unique
SITES_One2One_canopy$Canopy_L2 %>% unique

SITES_One2One_canopy$CHM.mean %>% summary
SITES_One2One_canopy$Cutoff05.SDSDH %>% summary
SITES_One2One_canopy$EVI.sd %>% summary
SITES_One2One_canopy$LAI.sd %>% summary

mean.df <- SITES_One2One_canopy %>% reframe( .by=c(Approach, Canopy_L2), 
                                             CHM.mean = mean(CHM.mean,na.rm=T),
                                             Cutoff05.SDSDH = mean(Cutoff05.SDSDH,na.rm=T),
                                             EVI.sd = mean(EVI.sd,na.rm=T),
                                             LAI.sd = mean(LAI.sd,na.rm=T))

min.df <- SITES_One2One_canopy %>% reframe(  .by=c(Approach, Canopy_L2), 
                                             CHM.mean = min(CHM.mean,na.rm=T),
                                             Cutoff05.SDSDH = min(Cutoff05.SDSDH,na.rm=T),
                                             EVI.sd = min(EVI.sd,na.rm=T),
                                             LAI.sd = min(LAI.sd,na.rm=T))

max.df <- SITES_One2One_canopy %>% reframe(  .by=c(Approach, Canopy_L2), 
                                             CHM.mean = max(CHM.mean,na.rm=T),
                                             Cutoff05.SDSDH = max(Cutoff05.SDSDH,na.rm=T),
                                             EVI.sd = max(EVI.sd,na.rm=T),
                                             LAI.sd = max(LAI.sd,na.rm=T))

summary.df <- rbind( mean.df, min.df, max.df)

CHM.mean.df <- data.frame( CHM.mean = seq(SITES_One2One_canopy$CHM.mean %>% min(na.rm=T), SITES_One2One_canopy$CHM.mean %>% max(na.rm=T), 2) )

Cutoff05.SDSDH.df <- data.frame( Cutoff05.SDSDH = seq(SITES_One2One_canopy$Cutoff05.SDSDH %>% min(na.rm=T), SITES_One2One_canopy$Cutoff05.SDSDH %>% max(na.rm=T), 0.2) )
EVI.sd.df <- data.frame( EVI.sd = seq(SITES_One2One_canopy$EVI.sd %>% min(na.rm=T), SITES_One2One_canopy$EVI.mean %>% max(na.rm=T), 0.01) )
LAI.sd.df <- data.frame( LAI.sd = seq(SITES_One2One_canopy$LAI.sd %>% min(na.rm=T), SITES_One2One_canopy$LAI.sd %>% max(na.rm=T), 0.01) )

# Format Factors

format.factors <- function( data){
  data$Approach <- factor( data$Approach , levels = c("MBR", "AE", "WP"))
  data$Canopy_L2 <- factor( data$Canopy_L2 , levels = c("AA+" , "AA", "AW+-", "AW+", "AW-" , "AW",   "WW-", "WW" ))
  
  return(data)
}

CHM.mean.final <- summary.df %>% select(! CHM.mean) %>% cross_join(CHM.mean.df )
Cutoff05.SDSDH.final <- summary.df %>% select(!Cutoff05.SDSDH) %>% cross_join(Cutoff05.SDSDH.df )
EVI.sd.final <- summary.df %>% select(! EVI.sd) %>% cross_join(EVI.sd.df )
LAI.sd.final <- summary.df %>% select(! LAI.sd) %>% cross_join(LAI.sd.df )

CHM.mean.final$model <-  predict(rf_model  ,CHM.mean.final,'prob')[,2]
Cutoff05.SDSDH.final$model <-  predict(rf_model  ,Cutoff05.SDSDH.final,'prob')[,2]
EVI.sd.final$model <-  predict(rf_model  ,EVI.sd.final,'prob')[,2]
LAI.sd.final$model <-  predict(rf_model  , LAI.sd.final,'prob')[,2]

CHM.mean.final <-  CHM.mean.final%>% format.factors %>% mutate( Canopy_L1 = case_when(Canopy_L2 == "AA+"~ "AA",
                                                                                      Canopy_L2 == "AA-"~ "AA",
                                                                                      Canopy_L2 == "AA"~ "AA",
                                                                                      Canopy_L2 == "AW+"~ "AW",
                                                                                      Canopy_L2 == "AW-"~ "AW",
                                                                                      Canopy_L2 == "AW"~ "AW",
                                                                                      Canopy_L2 == "WW+"~ "WW",
                                                                                      Canopy_L2 == "WW-"~ "WW",
                                                                                      Canopy_L2 == "WW"~ "WW"))
Cutoff05.SDSDH.final <- Cutoff05.SDSDH.final %>% format.factors%>% mutate( Canopy_L1 = case_when(Canopy_L2 == "AA+"~ "AA",
                                                                                                 Canopy_L2 == "AA-"~ "AA",
                                                                                                 Canopy_L2 == "AA"~ "AA",
                                                                                                 Canopy_L2 == "AW+"~ "AW",
                                                                                                 Canopy_L2 == "AW-"~ "AW",
                                                                                                 Canopy_L2 == "AW"~ "AW",
                                                                                                 Canopy_L2 == "WW+"~ "WW",
                                                                                                 Canopy_L2 == "WW-"~ "WW",
                                                                                                 Canopy_L2 == "WW"~ "WW"))

EVI.sd.final <- EVI.sd.final%>% format.factors%>% mutate( Canopy_L1 = case_when(Canopy_L2 == "AA+"~ "AA",
                                                                                Canopy_L2 == "AA-"~ "AA",
                                                                                Canopy_L2 == "AA"~ "AA",
                                                                                Canopy_L2 == "AW+"~ "AW",
                                                                                Canopy_L2 == "AW-"~ "AW",
                                                                                Canopy_L2 == "AW"~ "AW",
                                                                                Canopy_L2 == "WW+"~ "WW",
                                                                                Canopy_L2 == "WW-"~ "WW",
                                                                                Canopy_L2 == "WW"~ "WW"))
LAI.sd.final <- LAI.sd.final %>% format.factors%>% mutate( Canopy_L1 = case_when(Canopy_L2 == "AA+"~ "AA",
                                                                                 Canopy_L2 == "AA-"~ "AA",
                                                                                 Canopy_L2 == "AA"~ "AA",
                                                                                 Canopy_L2 == "AW+"~ "AW",
                                                                                 Canopy_L2 == "AW-"~ "AW",
                                                                                 Canopy_L2 == "AW"~ "AW",
                                                                                 Canopy_L2 == "WW+"~ "WW",
                                                                                 Canopy_L2 == "WW-"~ "WW",
                                                                                 Canopy_L2 == "WW"~ "WW"))
 

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

# MBR
plot.mbr.1 <- Sensitivity_plot2(df = CHM.mean.final, approach = "MBR", label= "Canopy Height", var='CHM.mean' )

plot.mbr.2 <- Sensitivity_plot2(df = Cutoff05.SDSDH.final, approach = "MBR", label= "SDSDH", var='Cutoff05.SDSDH' )

plot.mbr.3 <- Sensitivity_plot2(df = EVI.sd.final, approach = "MBR", label= "EVI", var='EVI.sd' )

plot.mbr.4 <- Sensitivity_plot2(df = LAI.sd.final , approach = "MBR", label= "LAI", var='LAI.sd' )  


gd.plot.mbr <- ggarrange(plot.mbr.1,
          plot.mbr.2,
          plot.mbr.3,
          plot.mbr.4, common.legend = TRUE , nrow=1)

plot.ae.1 <- Sensitivity_plot2(df = CHM.mean.final, approach = "AE", label= "Canopy Height", var='CHM.mean' )

plot.ae.2 <- Sensitivity_plot2(df = Cutoff05.SDSDH.final, approach = "AE", label= "SDSDH", var='Cutoff05.SDSDH' )

plot.ae.3 <- Sensitivity_plot2(df = EVI.sd.final, approach = "AE", label= "EVI", var='EVI.sd' )

plot.ae.4 <- Sensitivity_plot2(df = LAI.sd.final , approach = "AE", label= "LAI", var='LAI.sd' )  

gd.plot.ae <-ggarrange(plot.ae.1,
          plot.ae.2,
          plot.ae.3,
          plot.ae.4, common.legend = TRUE, nrow=1 )

plot.wp.1 <- Sensitivity_plot2(df = CHM.mean.final, approach = "WP", label= "Canopy Height", var='CHM.mean' )

plot.wp.2 <- Sensitivity_plot2(df = Cutoff05.SDSDH.final, approach = "WP", label= "SDSDH", var='Cutoff05.SDSDH' )

plot.wp.3 <- Sensitivity_plot2(df = EVI.sd.final, approach = "WP", label= "EVI", var='EVI.sd' )

plot.wp.4 <- Sensitivity_plot2(df = LAI.sd.final , approach = "WP", label= "LAI", var='LAI.sd' )  

gd.plot.wp <-ggarrange(plot.wp.1,
          plot.wp.2,
          plot.wp.3,
          plot.wp.4, common.legend = TRUE , nrow=1)


final.gd.plot <- ggarrange( plot.mbr.1,
           plot.mbr.2,
           plot.mbr.3,
           plot.mbr.4,
           
           plot.ae.1,
           plot.ae.2,
           plot.ae.3,
           plot.ae.4,
           
           plot.wp.1,
           plot.wp.2,
           plot.wp.3,
           plot.wp.4, nrow=3, ncol=4,
           common.legend = TRUE,
           labels=c("A", "B", "C", "D",
                    "E", "F", "G", "H",
                    "I", "J", "K", "L" ))


ggsave("Figures/GoodData_plot.png", plot = final.gd.plot, width = 9, height = 9, units = "in")

# Next : #####

message('run flow.bhatt')
