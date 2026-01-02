# Validation:

library(ggforce)
library(tidyverse)
library(colorspace)
library(ggpubr)
library(ggplot2)

localdir <- '/Volumes/MaloneLab/Research/FluxGradient/FluxData'
DirRepo <-"/Users/sm3466/YSE Dropbox/Sparkle Malone/Research/FluxGradient/lterwg-flux-gradient-eval"

load(file= paste(localdir, "SITES_One2One_canopy_model.Rdata", sep="" ))
load( fs::path(localdir,paste0("SITE_DATA_FILTERED.Rdata")))
source(fs::path(DirRepo,"./functions/calc_validation.R"))

# Canopy Information:
canopy <- SITES_One2One_canopy_model %>% select( Site, rf_model, predicted, Good.CCC, CCC, dLevelsAminusB, Approach, gas, Canopy_L1) %>% rename(CCC.EC = CCC )

# Additional Sampling Height Pairs: #### 

# See how all levels compare to each other regardless of the CCC.EC:
# Define the threshold for this workflow ('rf_model.1' or 'rf_model.all' or 'CCC.0.5') :
threshold.workflow ='rf_model.all'

val.SHP <- Compare.SamplingHeightPairs(threshold = threshold.workflow ) %>% 
  mutate( Approach.1 = sapply(strsplit(var1, "-"), `[`, 1),
          level.1 = sapply(strsplit(var1, "-"), `[`, 2),
          Approach.2 = sapply(strsplit(var2, "-"), `[`, 1),
          level.2 = sapply(strsplit(var2, "-"), `[`, 2))


summary.val.SHP <- val.SHP %>% 
  mutate(Combination = paste(var1, var2, sep ="-")) %>% 
  reframe( .by=c(Combination, Site, gas, var1), CCC.1.max = max(CCC, na.rm=T), CCC.1.min = min(CCC, na.rm=T)) %>% 
  rename( var= var1) %>% 
  rbind(val.SHP %>% 
          mutate(Combination = paste(var1, var2, sep ="-")) %>% 
          reframe( .by=c(Combination, Site, gas, var2), CCC.1.max = max(CCC, na.rm=T),CCC.1.min = min(CCC, na.rm=T)) %>% 
          rename( var= var2) )

summary.val.SHP %>% ggplot() + geom_boxplot( aes(x=CCC.1.max, y = Site)) + 
  facet_wrap(~gas) + theme_bw() + xlim(-1,1)

# add the rf_model output and the CCC.EC into this file:
summary.val.SHP.2 <- summary.val.SHP %>% 
  mutate( Approach = sapply(strsplit(var, "-"), `[`, 1),
          dLevelsAminusB = sapply(strsplit(var, "-"), `[`, 2)) %>% 
  full_join(canopy, by= c("Site", "dLevelsAminusB", 'gas', 'Approach')) %>% distinct

val.SHP.resample <- summary.val.SHP.2 %>% mutate( 
  Approach = Approach %>% factor(levels=c("MBR", "AE","WP")) ) %>% 
  reframe(.by = c(Approach, gas, Site, dLevelsAminusB, Canopy_L1) ,
           CCC.GF = mean(CCC.1.max, na.rm=T ),
           CCC.EC = mean(CCC.EC, na.rm=T),
          rf_model = mean( rf_model, na.rm=T)) %>% 
  mutate(Good.CCC.EC = case_when(
               CCC.EC  >= 0.5 ~ 1,.default = 0) %>% as.factor,
         Good.CCC.GF = case_when(
               CCC.GF >= 0.5 ~ 1,.default = 0) %>% as.factor,
         predicted = case_when(
               rf_model >= 0.52 ~ 1,.default = 0) %>% as.factor,
         Good.CCC.FG.rf = case_when(
               Good.CCC.GF == 1 | predicted == 1 ~ 1,.default = 0) %>% as.factor)

val.SHP.resample %>% summary

# matrix for overlap"
confusion.matrix.ECGF <- caret::confusionMatrix(val.SHP.resample$Good.CCC.EC, val.SHP.resample$Good.CCC.FG.rf) 

confusio.matrix.df <- confusion.matrix.ECGF$table %>% as.data.frame()

plot.Cmatrix.gf <- ggplot(data = confusio.matrix.df, aes(x = Reference, y = Prediction, fill = Freq)) +
  geom_tile(color = "white") + # Add white borders to tiles
  geom_text(aes(label = Freq), vjust = .5, fontface = "bold") + labs(fill = "Frequency")+
  scale_fill_gradient(low = "white", high = "blue") + # Customize fill color
  labs(title = "Confusion Matrix",
       x = "CCC > 0.5 (EC)",
       y = "CCC > 0.5 (GF) + RF Model") +
  theme_minimal() + # Use a minimal theme
  theme(plot.title = element_text(hjust = 0.5)) # Center the title

val.SHP.resample %>% names()

plot.approach.gf <- val.SHP.resample %>% drop_na(predicted)%>%  
  ggplot( aes(x=Approach, y =CCC.GF, col=Good.CCC.FG.rf %>% as.factor)) + 
  geom_boxplot() + theme_bw() + scale_color_manual(values = c("0" = "skyblue", "1" = "navy")) + 
  geom_hline( yintercept=0.5, col="red") + labs( x = "Approach",
                                                 y = "CCC (GF)",
                                                 colour="Prediction") 

plot.canopylevels.gf <-val.SHP.resample  %>% drop_na(predicted)%>%  
  ggplot( aes(x=Canopy_L1, y =CCC.GF, col=Good.CCC.FG.rf %>% as.factor)) + 
  geom_boxplot() + theme_bw() + scale_color_manual(values = c("0" = "skyblue", "1" = "navy")) +
  geom_hline( yintercept=0.5, col="red")+ labs( x = "Canopy Level",
                                                y = "CCC (GF)",
                                                colour="Prediction") 

plot.CCC.gf <- val.SHP.resample   %>% drop_na(predicted)%>%  
  ggplot( aes(x=Good.CCC.FG.rf %>% as.factor, y =CCC.GF, col= Good.CCC.FG.rf)) + 
  geom_boxplot() + theme_bw() + labs( x = "Reliable Pair",
                                                y = "CCC (GF)",
                                                colour="Prediction") + scale_color_manual(values = c("0" = "skyblue", "1" = "navy"))+ 
  theme( legend.position = "none")

final.model.plots.gf <- ggarrange( ggarrange(plot.CCC.gf, plot.Cmatrix.gf, nrow=1, ncol=2, labels=c("A", "B")),
                                ggarrange(plot.approach.gf , plot.canopylevels.gf, nrow= 1, common.legend = T, labels=c("C", "D")), ncol=1 )

ggsave("/Users/sm3466/YSE Dropbox/Sparkle Malone/Research/FluxGradient/lterwg-flux-gradient-eval/Figures/Model.plot.matrix_GFMODELPRED.png", plot =  final.model.plots.gf, width = 7, height = 7, units = "in")

# How many additional pairs were added ####
val.SHP.resample %>% names()
pairs.MODEL <- val.SHP.resample  %>% filter( predicted  == 1)
pairs.GF <- val.SHP.resample  %>% filter( Good.CCC.GF == "1" ) 
additional.pairs.GF <- val.SHP.resample  %>% filter( Good.CCC.GF == "0" & Good.CCC.FG.rf== 1) 
additional.pairs.EC <- val.SHP.resample  %>% filter( Good.CCC.EC == "0" & Good.CCC.FG.rf== 1) 

pairs.MODEL  %>% unique %>%  nrow # Pairs
pairs.GF %>% unique %>%  nrow # Pairs
additional.pairs.EC  %>% unique %>%  nrow # Pairs
additional.pairs.GF  %>% unique %>%  nrow # Pairs

sites.model <- pairs.MODEL  %>% select (Site) %>% unique # sites
sites.pairs.GF <- pairs.GF  %>% select (Site) %>% unique # sites
sites.EC <- additional.pairs.EC  %>% select (Site) %>% unique  # sites
sites.GF <- additional.pairs.GF  %>% select (Site) %>% unique# sites

sites.model %>% nrow
sites.pairs.GF %>% nrow
sites.EC %>% nrow
sites.GF %>% nrow

setdiff(sites.model, sites.EC)
setdiff(sites.model, sites.pairs.GF)
setdiff(sites.model, sites.GF)
setdiff(sites.EC, sites.GF)

# Save this file for use:
save(SITES_One2One_canopy_model, val.SHP.resample ,
     file= paste(localdir, "SITES_One2One_canopy_model_CCCGF.Rdata", sep="") )

# pre-validation: ####.  START HERE.. ####
load(  file= paste(localdir, "SITES_One2One_canopy_model_CCCGF.Rdata", sep="") )

# Select information needed to ID the reliable levels:

canopy.adj <- val.SHP.resample %>% select( Site, Approach, gas, dLevelsAminusB, Good.CCC.FG.rf, Good.CCC.EC ,Good.CCC.GF) %>% rename( Good.CCC.EC.FG = Good.CCC.FG.rf)

canopy.adj$Good.CCC.EC.FG %>% as.factor %>% summary

canopy.adj.reliable <- canopy.adj %>% filter(Good.CCC.EC.FG == "1" )
canopy.adj.reliable$Site %>% unique %>% length # 41 Sites

# Run-pre.validation analysis for sampling height pairs:  #####

pre.val.SHP <- validation.SamplingHeightPairs(index.df =  canopy.adj) %>% mutate(combination = paste(x, y, sep="-")) %>% rename( CCC.GF = CCC)

plot.preval.SHP.sites <- pre.val.SHP %>% ggplot() + 
  geom_boxplot( aes( x= CCC.GF,  y = Site)) +
  facet_wrap(~gas)+ theme_bw()+ xlab(" CCC (Sampling Height Pairs)") +
  ylab("") + geom_vline(xintercept= 0.5, col="red") + xlim(-1,1) +
  theme(strip.background =element_rect(fill="transparent"))

plot.preval.SHP.density <- pre.val.SHP %>% ggplot() + 
  geom_density( aes( x= CCC.GF)) +  xlim(-1,1) + 
  facet_wrap(~gas)+ theme_bw()+ xlab(" CCC (Sampling Height Pairs)") +
  ylab("") + geom_vline(xintercept= 0.5, col="red") +
  theme(strip.background =element_rect(fill="transparent"))

final.plots.preval.SHP <- ggarrange( plot.preval.SHP.sites,
                                     plot.preval.SHP.density, 
                                     ncol=1, labels = c("A", "B"),
                                     heights=c(3, 1))

pre.val.approach <- validation.approach(index.df =  canopy.adj) %>% mutate(combination = paste(x, y, sep="-")) %>% rename( CCC.GF = CCC)

plot.preval.approach <- pre.val.approach %>% ggplot() + 
  geom_boxplot( aes( x= CCC.GF,  y = combination)) +
  geom_point( aes( x= CCC.GF,  y = combination)) +
  facet_wrap(~gas) + theme_bw() + xlim(-1,1) +
  theme(strip.background =element_rect(fill="transparent"))+ 
  xlab(" CCC (Approach)") + ylab( "Approach") +
  geom_vline( xintercept = 0.5, col="red") 

plot.preval.approach.sites <- pre.val.approach %>% ggplot() + 
  geom_boxplot( aes( x= CCC.GF,  y = Site)) +
  geom_point( aes( x= CCC.GF,  y = Site))+xlim(-1,1) +
  facet_wrap(~gas)+ theme_bw()+ xlab(" CCC (Approach)") + ylab("") +
  theme(strip.background =element_rect(fill="transparent")) +
  geom_vline( xintercept = 0.5, col="red") 

finalplot.preval.approach <- ggarrange( plot.preval.approach.sites, plot.preval.approach, labels=c("A", "B"),
           ncol=1, heights=c(3, 1))

pre.val.canopy <- validation.canopy.l1( index.df = canopy.adj) %>% mutate(combination = paste(x, y, sep="-")) %>% rename( CCC.GF = CCC) %>% mutate(
  combination.adj = case_when( combination == "AW-WW" | combination =="WW-AW" ~ "AW-WW",
                               combination == "AW-AA" | combination =="AA-AW" ~ "AA-AW",
                               combination == "AA-WW" | combination =="WW-AA" ~ "AA-WW") )

plot.preval.canopy <- pre.val.canopy  %>% 
  ggplot() + geom_boxplot( aes( x= CCC.GF,  y = combination.adj)) +
  facet_wrap(~gas) + theme_bw() +
  theme(strip.background =element_rect(fill="transparent"))+ 
  xlab(" CCC (Canopy Level)") + ylab("") +   xlim(-1,1)  +
  geom_vline( xintercept = 0.5, col="red") 

plot.preval.canopy.sites <- pre.val.canopy %>% ggplot() + 
  geom_boxplot( aes( x= CCC.GF,  y = Site)) +
  facet_wrap(~gas)+ theme_bw()+ xlab(" CCC (Canopy Level)")+
  theme(strip.background =element_rect(fill="transparent"))+
  xlim(-1,1) +
  geom_vline( xintercept = 0.5, col="red") 
  
finalplot.preval.canopy <- ggarrange(plot.preval.canopy.sites,plot.preval.canopy, ncol=1,
                                       labels=c("A", "B"), heights = c(3,1))


final.pre.validation.plot <- ggarrange(plot.preval.SHP.density,
          plot.preval.approach, plot.preval.canopy , ncol=1,
          labels=c("A", "B", "C"), heights = c(1,1,1))

# Save the plots:

ggsave("/Users/sm3466/YSE Dropbox/Sparkle Malone/Research/FluxGradient/lterwg-flux-gradient-eval/Figures/PreValidation.SHP.APPROACH.CL1.png",
       plot =  final.pre.validation.plot, width = 6, height = 7, units = "in")

ggsave("/Users/sm3466/YSE Dropbox/Sparkle Malone/Research/FluxGradient/lterwg-flux-gradient-eval/Figures/PreValidation.APPROACH.png",
       plot =  finalplot.preval.approach, width = 6, height = 7, units = "in")

ggsave("/Users/sm3466/YSE Dropbox/Sparkle Malone/Research/FluxGradient/lterwg-flux-gradient-eval/Figures/PreValidation.CL1.png",
       plot =  finalplot.preval.canopy, width = 6, height = 7, units = "in")

ggsave("/Users/sm3466/YSE Dropbox/Sparkle Malone/Research/FluxGradient/lterwg-flux-gradient-eval/Figures/PreValidation.SHP.png",
       plot =  final.plots.preval.SHP, width = 6, height = 7, units = "in")

# Post-validation: ####
post.validation <- validation( index.df = canopy.adj) %>% rename( CCC.EC = CCC)

plot.post.val <- post.validation %>% ggplot() + 
  geom_point( aes( x= CCC.EC,  y = Site)) + xlim(-1,1) +
  facet_wrap(~gas) + theme_bw() + xlab(" CCC (EC)") +
  geom_vline( xintercept = 0.5, col="red") +
  theme(strip.background =element_rect(fill="transparent")) + ylab("")

plot.post.val.density <- post.validation %>% ggplot() + 
  geom_density( aes( x= CCC.EC)) + xlim(-1,1) +
  facet_wrap(~gas) + theme_bw() + xlab(" CCC (EC)") +
  geom_vline( xintercept = 0.5, col="red") +
  theme(strip.background =element_rect(fill="transparent")) + ylab("")

final.post.validation.plot <- ggarrange(plot.post.val,
                                        plot.post.val.density, ncol=1,
                                       labels=c("A", "B"),
                                       heights=c(3,1))

ggsave("/Users/sm3466/YSE Dropbox/Sparkle Malone/Research/FluxGradient/lterwg-flux-gradient-eval/Figures/Validation.PLOTS.png",
       plot =  final.post.validation.plot, width = 6, height = 7, units = "in")

final.post.validation.plot <- ggarrange(plot.post.val,
                                        plot.post.val.density, ncol=1,
                                        labels=c("D", "E"),
                                        heights=c(3,1))

SUper.plot <- ggarrange( final.pre.validation.plot,final.post.validation.plot , ncol=2 )

ggsave("/Users/sm3466/YSE Dropbox/Sparkle Malone/Research/FluxGradient/lterwg-flux-gradient-eval/Figures/Validation.Harmonization.png",
       plot =  SUper.plot, width = 8, height = 7, units = "in")



canopy.adj2 <- SITES_One2One_canopy_model %>%
  reframe( .by= c(Site, gas), 
           canopyHeight_m = mean(canopyHeight_m),
           LAI.mean = mean(LAI.mean),
           LAI.sd = mean(LAI.sd),
           count = mean(count),
           predicted = max(predicted %>% as.numeric),
           CCC.GF = mean(CCC.max, na.rm = T))


validation <- post.validation %>% left_join(canopy.adj2, by=c("Site", "gas") ) %>% left_join( pre.val.SHP %>% reframe( .by=c("Site", "gas"), CCC.SHP=mean( CCC.GF, na.rm=T)), by=c("Site", "gas") )

validation %>% ggplot() + geom_point( aes(y=CCC.EC , x=CCC.SHP)) + ylim(-1,1) + xlim(-1,1)

cor.val <- cor(validation[, c( 6:7, 9:13) ], method = "pearson", use = "complete.obs")

library(corrplot)
corrplot(cor.val , type = "upper", order = "hclust", 
         tl.col = "black", tl.srt = 45)

lm( data=validation,CCC.EC ~ count  ) %>% summary
lm( data=validation,CCC.EC ~ CCC.SHP  ) %>% summary

