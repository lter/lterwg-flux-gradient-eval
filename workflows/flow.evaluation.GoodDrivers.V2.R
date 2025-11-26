# Good levels:

rm(list=ls())

library(dplyr)
library(ggplot2)
library(ggridges)
library(ggpubr)
library(VSURF)
library(randomForest)

localdir <- '/Volumes/MaloneLab/Research/FluxGradient/FluxData'
DirRepo <-"/Users/sm3466/YSE Dropbox/Sparkle Malone/Research/FluxGradient/lterwg-flux-gradient-eval"
load( fs::path(localdir,paste0("SITES_One2One.Rdata")))
load( fs::path(localdir,paste0("SITE_DATA_FILTERED.Rdata")))

# Stop.vsurf:

stop <- 'yes' # change this to no is you want to re-run variable selection

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


# Set up VSURF ####
# Divide into test and training datasets:
if( stop == 'no'){
  train <- SITES_One2One_canopy %>% 
    sample_frac(0.80) 
  
  test <- anti_join(SITES_One2One_canopy, train )
  
  SITES_One2One_canopy %>% summary
  train  %>% summary
  test  %>% summary
  
  # Variable Selection : ####
  
  SITES_One2One_canopy %>% names
  train[, c(7, 10,73, 81, 82)] %>% names
  
  rf_index.sdesign.vsurf <- VSURF(train[, c(7, 10,73, 81, 82)], 
                                  train[["Good.CCC"]],
                                  ntree = 5000,
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
  
  final.vars <- c(rf_index.sdesign.vars, rf_index.Cspec.vars,rf_index.Cstructure.vars) 
  
  save(final.vars, file= paste(dir, "FinalVarsSelection.Rdata", sep="") )
}

# Final Model Fit : ####
if( stop == 'no'){
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
}

# Sensitivity Analysis: ####

load(file= paste(dir, "Good_Fluxes.Rdata", sep="") )
final.vars

SITES_One2One_canopy$Approach %>% unique
SITES_One2One_canopy$Canopy_L1 %>% unique
SITES_One2One_canopy$gas %>% unique

SITES_One2One_canopy$RelativeDistB %>% summary
SITES_One2One_canopy$RelativeDistA %>% summary
SITES_One2One_canopy$Cutoff05.SDSDH %>% summary
SITES_One2One_canopy$LAI.sd %>% summary

mean.df <- SITES_One2One_canopy %>% reframe( .by=c(Approach, Canopy_L1, gas), 
                                             RelativeDistB = mean(RelativeDistB, na.rm=T),
                                             RelativeDistA = mean(RelativeDistA, na.rm=T),
                                             Cutoff05.SDSDH  = mean(Cutoff05.SDSDH ,na.rm=T),
                                             LAI.sd = mean(LAI.sd,na.rm=T))

min.df <- SITES_One2One_canopy %>% reframe( .by=c(Approach, Canopy_L1, gas), 
                                             RelativeDistB = min(RelativeDistB, na.rm=T),
                                             RelativeDistA = min(RelativeDistA, na.rm=T),
                                             Cutoff05.SDSDH  = min(Cutoff05.SDSDH ,na.rm=T),
                                             LAI.sd = min(LAI.sd,na.rm=T))

max.df <- SITES_One2One_canopy %>% reframe( .by=c(Approach, Canopy_L1, gas), 
                                             RelativeDistB = max(RelativeDistB, na.rm=T),
                                             RelativeDistA = max(RelativeDistA, na.rm=T),
                                             Cutoff05.SDSDH  = max(Cutoff05.SDSDH ,na.rm=T),
                                             LAI.sd = max(LAI.sd,na.rm=T))

summary.df <- rbind( mean.df, min.df, max.df)

summary.df %>% summary

RelativeDistB.df <- data.frame(  RelativeDistB = seq(SITES_One2One_canopy$ RelativeDistB %>% min(na.rm=T), SITES_One2One_canopy$ RelativeDistB %>% max(na.rm=T), 2) )
RelativeDistA.df <- data.frame(  RelativeDistA = seq(SITES_One2One_canopy$RelativeDistA %>% min(na.rm=T), SITES_One2One_canopy$RelativeDistA %>% max(na.rm=T), 2) )
Cutoff05.SDSDH.df <- data.frame( Cutoff05.SDSDH = seq(SITES_One2One_canopy$Cutoff05.SDSDH %>% min(na.rm=T), SITES_One2One_canopy$Cutoff05.SDSDH %>% max(na.rm=T), 0.2) )
LAI.sd.df <- data.frame( LAI.sd = seq(SITES_One2One_canopy$LAI.sd %>% min(na.rm=T), SITES_One2One_canopy$LAI.sd %>% max(na.rm=T), 0.01) )

# Format Factors

format.factors <- function( data){
  data$Approach <- factor( data$Approach , levels = c("MBR", "AE", "WP"))
  data$Canopy_L1 <- factor( data$Canopy_L1 , levels = c("AA" , "AW", "WW"))
  
  return(data)
}


Cutoff05.SDSDH.final <- summary.df %>% select(!Cutoff05.SDSDH) %>% cross_join(Cutoff05.SDSDH.df )
RelativeDistB.final <- summary.df %>% select(! RelativeDistB) %>% cross_join(RelativeDistB.df )
RelativeDistA.final <- summary.df %>% select(! RelativeDistA) %>% cross_join(RelativeDistA.df )
LAI.sd.final <- summary.df %>% select(! LAI.sd) %>% cross_join(LAI.sd.df )


Cutoff05.SDSDH.final$model <-  predict(rf_model  , Cutoff05.SDSDH.final,'prob')[,2]
LAI.sd.final$model <-  predict(rf_model  , LAI.sd.final,'prob')[,2]
RelativeDistB.final$model <-  predict(rf_model  , RelativeDistB.final,'prob')[,2]
RelativeDistA.final$model <-  predict(rf_model  , RelativeDistA.final,'prob')[,2]

Sensitivity_plot <- function(df, approach, label, var, col){

 
  plot <-  ggplot() + geom_smooth(data= df %>% filter(Approach == approach),
                                  aes_string( x= var , y = 'model', col= 'Canopy_L2'), alpha=0.2) + 
    scale_color_manual(values=c("goldenrod4", "goldenrod2",
                                "green1","green3", "aquamarine2","aquamarine4",
                                "purple", "darkmagenta")) +theme_bw() + xlab(label)
  
  return(plot)
}

Sensitivity_plot2 <- function(df, approach, label, var, col){
  
  
  plot <-  ggplot() + geom_smooth(data= df %>% filter(Approach == approach), col=col,
                                  aes_string( x= var , y = 'model', linetype= 'Canopy_L1'), alpha=0.2)+
    theme_bw() + xlab(label) + ylab( "P(CCC > 0.5)")
  
  return(plot)
}

Sensitivity_approach_plot <- function(approach, labels){
  
  
if( approach == "MBR"){
  col = "goldenrod"} else if( approach == "AE"){
    col = "aquamarine4"} else if( approach == "WP"){
    col = "darkmagenta"}
  
  
  plot.mbr.2 <- Sensitivity_plot2(df = Cutoff05.SDSDH.final, approach = approach, label= "SDSDH", var='Cutoff05.SDSDH' , col =col)+ theme(legend.title = element_blank())
  
  plot.mbr.4 <- Sensitivity_plot2(df = RelativeDistB.final, approach = approach, label= "Relative Dist B", var='RelativeDistB' , col =col)+ theme(legend.title = element_blank())
  
  plot.mbr.5 <- Sensitivity_plot2(df = RelativeDistA.final, approach = approach, label= "Relative Dist A", var='RelativeDistA', col =col )+ theme(legend.title = element_blank())
  
  plot.mbr.3 <- Sensitivity_plot2(df = LAI.sd.final , approach = approach, label= "LAI", var='LAI.sd' , col =col)+ theme(legend.title = element_blank())
  
  
  gd.plot.mbr <- ggarrange(plot.mbr.2,
                           plot.mbr.5,
                           plot.mbr.4,
                           plot.mbr.3, common.legend = TRUE , nrow=1, labels=labels)
  return( gd.plot.mbr )
}

gd.plot.mbr <-Sensitivity_approach_plot(approach = "MBR", labels = c("A", "B", "C", "D"))
gd.plot.ae <-Sensitivity_approach_plot(approach = "AE", labels = c("E", "F", "G", "H"))
gd.plot.wp <-Sensitivity_approach_plot(approach = "WP", labels = c( "I", "J", "K", "L"))

final.gd.plot <- ggarrange( gd.plot.mbr, gd.plot.ae, gd.plot.wp, nrow=3, ncol=1, common.legend = TRUE)


ggsave("Figures/GoodData_plot.png", plot = final.gd.plot, width = 11, height = 9, units = "in")


metadata <- read.csv('/Volumes/MaloneLab/Research/FluxGradient/Ameriflux_NEON field-sites.csv') 
site.list <- metadata$Site_Id.NEON %>% unique

rf_model 


