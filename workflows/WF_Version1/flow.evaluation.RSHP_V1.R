# Designed to explore the performance of sampling height pairs relative to other pairs

library(ggforce)
library(tidyverse)
library(colorspace)
library(ggpubr)
library(ggplot2)
library(randomForest)
library(caret)
library(rpart)

localdir <- '/Volumes/MaloneLab/Research/FluxGradient/FluxData'
DirRepo.eval <-"/Users/sm3466/YSE Dropbox/Sparkle Malone/Research/FluxGradient/lterwg-flux-gradient-eval"

load(file= paste(localdir, "SITES_One2One_canopy_model.Rdata", sep="" ))
load( fs::path(localdir,paste0("SITE_DATA_FILTERED_CCC.Rdata")))
source(fs::path(DirRepo.eval,"./functions/calc_validation.R"))
source(fs::path(DirRepo.eval,"./functions/calc.linear.terms.R"))
source(fs::path(DirRepo.eval,"./functions/calc.lins.ccc.R"))
       
# Canopy Information:
canopy <- SITES_One2One_canopy_model %>% select( Site, rf_model, predicted, Good.CCC, CCC, dLevelsAminusB, Approach, gas, Canopy_L1) %>% rename(CCC.EC = CCC )

# Calculate CCC.GF: #### 

# See how all levels compare to each other regardless of the CCC.EC:
# Define the threshold for this workflow ('rf_model.1' or 'rf_model.all' or 'CCC.0.5') :
threshold.workflow ='CCC.0.5'

val.SHP <- Compare.SamplingHeightPairs(threshold = threshold.workflow ) %>% 
  mutate( Approach.1 = sapply(strsplit(var1, "-"), `[`, 1),
          level.1 = sapply(strsplit(var1, "-"), `[`, 2),
          Approach.2 = sapply(strsplit(var2, "-"), `[`, 1),
          level.2 = sapply(strsplit(var2, "-"), `[`, 2),
          Combination = paste(var1, var2, sep ="-"))

# Reformat the data: 

val.SHP.var1 <- val.SHP %>% select( Site, CCC, var1, gas, Approach.1, Approach.2, level.1, level.2, Combination) %>% 
  rename( var= var1, CCC.GF = CCC, Approach= Approach.1, dLevelsAminusB = level.1) %>% 
  mutate( Approach = Approach %>% factor(levels=c("MBR", "AE","WP")) ) %>% 
  select(Site, CCC.GF, var, gas, Approach, dLevelsAminusB, Combination) %>% 
  distinct


val.SHP.var2 <- val.SHP %>% select( Site, CCC, var2, gas, Approach.1, Approach.2, level.1, level.2, Combination) %>% 
  rename( var= var2, CCC.GF = CCC, Approach= Approach.2, dLevelsAminusB = level.2) %>% 
  mutate( Approach = Approach %>% factor(levels=c("MBR", "AE","WP")) ) %>% 
  select(Site, CCC.GF, var, gas, Approach, dLevelsAminusB, Combination) %>% 
  distinct


# this file now has all possible combinations formatted long: 
val.SHP.total <- rbind( val.SHP.var1 , val.SHP.var2) %>% distinct() %>% na.omit()

val.SHP.total %>% ggplot() + geom_boxplot( aes(x=CCC.GF, y = Site)) + 
  facet_wrap(~gas) + theme_bw() + xlim(-1,1)

# Summary information for CCC.GF by var:

val.SHP.total.summary <- val.SHP.total %>% na.omit %>% 
  reframe( .by=c(Site, gas, var), 
           CCC.GF.min = min(CCC.GF, na.rm=T),
           CCC.GF.mean = mean(CCC.GF, na.rm=T),
           CCC.GF.max = max(CCC.GF, na.rm=T),
           CCC.GF.median = median(CCC.GF, na.rm=T),
           CCC.GF.range = range(CCC.GF, na.rm=T)) %>% distinct

# Canopy information with CCC.EC
canopy.sub <- canopy %>% select(Site, Approach, gas, Canopy_L1, dLevelsAminusB, CCC.EC, Good.CCC) %>% distinct()

val.SHP.total.canopy <- val.SHP.total %>% full_join(canopy.sub, by = c("Site", 'Approach', 'gas', 'dLevelsAminusB')) %>% full_join(val.SHP.total.summary, by = c("Site", 'gas', 'var'))

val.SHP.total.canopy %>% names()
val.SHP.total.canopy.summary <- val.SHP.total.canopy %>% reframe(.by= c(Site, gas, Approach,dLevelsAminusB, var,Canopy_L1 ), 
                                                                 CCC.EC = mean(CCC.EC),
                                                                 Good.CCC = case_when( CCC.EC >= 0.5 ~1, CCC.EC < 0.5 ~ 0) %>% as.factor,
                                                                 CCC.GF.min = mean(CCC.GF.min),
                                                                 CCC.GF.mean = mean(CCC.GF.mean),
                                                                 CCC.GF.max = mean(CCC.GF.max),
                                                                 CCC.GF.median = mean(CCC.GF.median),
                                                                 CCC.GF.range = mean(CCC.GF.range),
                                                                 CCC.GF = mean(CCC.GF))
  
  
fileSave <- fs::path(localdir,paste0("SITE_RSHP.Rdata"))
save(val.SHP.total.canopy ,val.SHP.total.canopy.summary, file=fileSave)

# Detect RSHP with CCC.GF: RANDOM FOREST MODEL DEVELOPMENT ####

load(fs::path(localdir,paste0("SITE_RSHP.Rdata")))

train <- val.SHP.total.canopy.summary %>% 
  sample_frac(0.80) 

test <- anti_join(val.SHP.total.canopy.summary, train )
dict_weights = c(1, 100)

rf.good.ccc.ec <- randomForest::randomForest(Good.CCC ~ CCC.GF.mean + CCC.GF.range + CCC.GF.min , 
                                             data = train, ntree= 1000,
                                             strata=train$Good.CCC,
                                             classwt = dict_weights,
                                             sampsize = rep(379, nlevels(train$Good.CCC)))

rf.good.ccc.ec

randomForest::varImpPlot(rf.good.ccc.ec)

test$Predictions <-  predict(object = rf.good.ccc.ec , test )
train$Predictions <- predict(rf.good.ccc.ec , train)

confusionMatrix(train$Predictions, train$Good.CCC)
confusionMatrix(test$Predictions, test$Good.CCC)

# Detect Breakpoints:

val.SHP.total.canopy.summary$Prob <-  predict(object = rf.good.ccc.ec , val.SHP.total.canopy.summary , type = "prob")[,2]
val.SHP.total.canopy.summary %>% ggplot() + geom_boxplot(aes(x=Good.CCC, y = Prob))

modelWeights <- ifelse(val.SHP.total.canopy.summary$Good.CCC == 1, 2, 0.5)

tree_surrogate <- rpart(Prob ~ CCC.GF.mean + CCC.GF.min + CCC.GF.range,
                        data = val.SHP.total.canopy.summary,
                        method = "anova",
                        weights = modelWeights,
                        control=rpart.control(minsplit = 20, maxdepth = 30))

rpart.plot::rpart.plot(tree_surrogate)
tree.plot <- gridGraphics::echoGrob()
library(ggdendro)
tree_data <- dendro_data(tree_surrogate)

# Adjust the labels:
tree_data$labels$label
tree_data$labels$label2 <- gsub("CCC.GF.", "", tree_data$labels$label)

tree.plot <- ggplot() +
  geom_segment(data = tree_data$segments, 
               aes(x = x, y = y, xend = xend, yend = yend), col='grey') +
  geom_text(data = tree_data$labels, 
            aes(x = x, y = y, label = label2), vjust = -0.5)  +
  geom_text(data = tree_data$leaf_labels, 
            aes(x = x, y = y, label = label),
            vjust = 2, size=3, fontface = "bold") +  
  labs(title = "    CCC (GF)") + 
  theme_bw() +theme(legend.position = "bottom",
                      axis.title.x = element_blank(), # Remove x-axis title
                      axis.text.x = element_blank(),  # Remove x-axis tick labels
                      axis.ticks.x = element_blank(), # Remove x-axis ticks
                      axis.title.y = element_blank(), # Remove y-axis title
                      axis.text.y = element_blank(),  # Remove y-axis tick labels
                      axis.ticks.y = element_blank(), # Remove y-axis ticks
                      axis.line = element_blank())
  
  

val.SHP.total.canopy.summary <- val.SHP.total.canopy.summary %>% 
  mutate( Thresholds = case_when( CCC.GF.mean > 0.23 &
                                    CCC.GF.mean < 0.35 &
                                    CCC.GF.min < -0.17 ~ 1,
                                  
                                  CCC.GF.mean > 0.23 &
                                    CCC.GF.mean < 0.35 &
                                    CCC.GF.min >= -0.17 ~ 1,
    
                                  CCC.GF.mean > 0.23 &
                                    CCC.GF.mean >= 0.35 &
                                    CCC.GF.range < 0.44 ~ 1,
                                  
                                  CCC.GF.mean > 0.23 &
                                    CCC.GF.mean >= 0.35 &
                                    CCC.GF.range >= 0.44 &
                                    CCC.GF.min < -0.22  ~ 1,
                                  
                                  CCC.GF.mean > 0.23 &
                                    CCC.GF.mean >= 0.35 &
                                    CCC.GF.range >= 0.44 &
                                    CCC.GF.min >= -0.22 &
                                    CCC.GF.mean > 0.31 ~ 1,

                                  TRUE ~ 0) %>% as.factor,
          Thresholds.prob = case_when(Prob >= 0.65 ~ 1,
                                      TRUE ~ 0) %>% as.factor)

confusionMatrix(val.SHP.total.canopy.summary$Thresholds, val.SHP.total.canopy.summary$Good.CCC)

confusionMatrix(val.SHP.total.canopy.summary$Thresholds.prob, val.SHP.total.canopy.summary$Good.CCC)

# Plots for defined thresholds and probability:
threshold.matrix <- confusionMatrix(val.SHP.total.canopy.summary$Thresholds, val.SHP.total.canopy.summary$Good.CCC)
threshold.matrix.df <- threshold.matrix$table %>% as.data.frame()

plot.threshold.matrix.gf <- ggplot(data = threshold.matrix.df, 
                                   aes(x = Reference, y = Prediction)) +
  geom_tile(color = 'black', fill='transparent') + # Add white borders to tiles
  geom_text(aes(label = Freq), vjust = .5, fontface = "bold") + # Customize fill color
  labs(title = "Confusion Matrix",
       x = "CCC ≥ 0.5 (EC)",
       y = "Model-Based Threshold (P > 0.65)") +
  theme_minimal() + # Use a minimal theme
  theme(plot.title = element_text(hjust = 0.5)) # Center the title


Model.matrix <- confusionMatrix(val.SHP.total.canopy.summary$Thresholds.prob, val.SHP.total.canopy.summary$Good.CCC)
Model.matrix.df <- Model.matrix$table %>% as.data.frame()

plot.Model.matrix.gf <- ggplot(data = Model.matrix.df, 
                                   aes(x = Reference, y = Prediction)) +
  geom_tile(color = 'black', fill='transparent') + # Add white borders to tiles
  geom_text(aes(label = Freq), vjust = .5, fontface = "bold") + # Customize fill color
  labs(title = "Confusion Matrix",
       x = "CCC ≥ 0.5 (EC)",
       y = "Model (P > 0.65)") +
  theme_minimal() + # Use a minimal theme
  theme(plot.title = element_text(hjust = 0.5)) # Center the title


#Sensitivity Analysis:
sensitive.focus.df <- function( data, model){
  
  vars <-  model$importance %>% data.frame %>% row.names
  
  
  sensitivity.summary <- data %>% select( all_of(vars)) %>%
    reframe(across(where(is.numeric), quantile, na.rm = TRUE, probs = seq(0, 1, 0.05))) %>% mutate(summary = seq(0, 1, 0.05) %>% as.factor)
  
  vars.df <- data.frame()
  for( i in vars){
    minimum <- data[,i] %>% min
    maximum <- data[,i] %>% max
    var <- data.frame( new =seq(minimum, maximum, maximum/30) )
    var[, i] <-  seq(minimum, maximum, maximum/30)
    sensitivity.mean.df.var <- sensitivity.summary %>% select(-c(i)) %>% cross_join(var %>% select(i)) %>% mutate(focus = i)
    vars.df <- rbind( vars.df, sensitivity.mean.df.var)
  }
  return(vars.df)
}

sensitivity.df <- sensitive.focus.df( data = val.SHP.total.canopy.summary,
                                      model=rf.good.ccc.ec)  %>% as.data.frame

sensitivity.df$Predictions <- predict(object = rf.good.ccc.ec, sensitivity.df) 

sensitivity.df %>% filter(focus == 'CCC.GF.mean') %>% ggplot() + geom_point( aes(x= summary, col= Predictions, y =CCC.GF.mean)) 

sensitivity.df %>% filter(focus == 'CCC.GF.mean',
                          Predictions == 1) %>% ggplot() + geom_point( aes(x= summary, col= Predictions, y =CCC.GF.mean)) 

sensitivity.df %>% filter(focus == 'CCC.GF.range') %>% ggplot() + geom_point( aes(x= summary, col= Predictions, y =CCC.GF.range)) 
sensitivity.df %>% filter(focus == 'CCC.GF.min') %>% ggplot() + geom_point( aes(x= summary, col= Predictions, y =CCC.GF.min))

sensitivity.df %>% ggplot() + geom_point( aes(x= CCC.GF.min, y =CCC.GF.mean, col=Predictions))


sensitivity.df %>% filter(focus == 'CCC.GF.mean') %>% ggplot() + geom_point( aes(x= summary, col= Predictions, y =CCC.GF.mean)) 
sensitivity.df %>% filter(focus == 'CCC.GF.mean') %>% ggplot() + geom_point( aes(x= summary, col= Thresholds, y =CCC.GF.mean)) 

# UPDATE BELOW: ####

# matrix for overlap"
confusion.matrix.RSHP <- confusionMatrix(val.SHP.total.canopy.summary$Thresholds., val.SHP.total.canopy.summary$Good.CCC)
confusio.matrix.df <- confusion.matrix.RSHP$table %>% as.data.frame()

plot.boxplot.approach.gf <- val.SHP.total.canopy.summary %>%  
  ggplot( aes(x=Approach, y =Prob, col=Good.CCC %>% as.factor)) + 
  geom_boxplot() + theme_bw() + scale_color_manual(values = c("0" = "skyblue", "1" = "navy")) + 
  labs( x = "Approach", y = "Probability",colour="CCC ≥ 0.5 (EC)") + theme(legend.position = "none")

plot.boxplot.canopylevels.gf <-val.SHP.total.canopy.summary   %>%  
  ggplot( aes(x=Canopy_L1, y =Prob, col=Good.CCC %>% as.factor)) + 
  geom_boxplot() + theme_bw() + scale_color_manual(values = c("0" = "skyblue", "1" = "navy")) +
  labs( x = "Canopy Level", y = "Probability", colour="CCC ≥ 0.5 (EC)") + theme(legend.position = "none")

plot.boxplot.CCC.gf <- val.SHP.total.canopy.summary %>%  
  ggplot( aes(x=Good.CCC %>% as.factor, y = Prob, col= Good.CCC)) + 
  geom_boxplot() + theme_bw() + labs( x = "CCC ≥ 0.5 (EC)",
                                      y = "Probability",
                                      colour="CCC ≥ 0.5 (EC)") + 
  scale_color_manual(values = c("0" = "skyblue", "1" = "navy"))



tree.plot.grid <- ggarrange(tree.plot, nrow=1, ncol=1, labels=c("A"))

tree.plot
plot.boxplot.CCC.gf
plot.threshold.matrix.gf
plot.Model.matrix.gf

plot.boxplot.approach.gf
plot.boxplot.canopylevels.gf

model.plots.gf <- ggarrange(ggarrange(plot.Model.matrix.gf,
                                            plot.boxplot.CCC.gf,
                                             nrow= 1, common.legend = T, labels=c("B","C")),
                                  ggarrange(plot.boxplot.approach.gf,
                                            plot.boxplot.canopylevels.gf, 
                                            nrow= 1, labels=c("D","F")), 
                                  ncol=1, common.legend = T )

final.model.plots.gf <- ggarrange(tree.plot.grid, model.plots.gf, ncol=2 )

ggsave("/Users/sm3466/YSE Dropbox/Sparkle Malone/Research/FluxGradient/lterwg-flux-gradient-eval/Figures/WF_Version1/RSHP_V1.png", 
       plot =  final.model.plots.gf, width = 10, height = 5, units = "in")


# Save this file for use:
save(val.SHP.total.canopy.summary ,rf.good.ccc.ec,
     file= paste(localdir, 'SITE_RSHP_MODEL.Rdata', sep="") )
