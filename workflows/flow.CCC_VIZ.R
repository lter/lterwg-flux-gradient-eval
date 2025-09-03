# CCC Viz and Evaluation:
rm(list=ls())

library(dplyr)
library(ggplot2)
library(ggridges)

localdir <- '/Volumes/MaloneLab/Research/FluxGradient/FluxData'
load(fs::path(localdir,paste0("SITES_One2One.Rdata")))

canopy <- read.csv(file.path(paste(localdir, "canopy_commbined.csv", sep="/"))) %>% distinct

Highest.CCC <- SITES_One2One %>% reframe(.by= c(Site, gas, Approach), CCC.max = max(CCC, na.rm=T))

SITES_One2One_canopy <- SITES_One2One %>% full_join(canopy, by=c("Site", "dLevelsAminusB" ) ) %>% 
  full_join( Highest.CCC,by = c("Site", "gas", "Approach")) %>% mutate(Approach = factor(Approach, levels = c("MBR", "AE", "WP") ),
                                                                       Good.CCC = case_when( CCC >= 0.5 ~ 1,
                                                                                             CCC <0.5 ~ 0),
                                                                       Canopy_L1 = factor(Canopy_L1, levels = c("AA", "AW", "WW") ))


SITES_One2One_canopy.Highest <- SITES_One2One_canopy %>% filter( CCC== CCC.max)


SITES_One2One_canopy.Highest.100 <- SITES_One2One_canopy %>% filter( count > 100) %>% filter( CCC== CCC.max) 

# Non-parametric test for differences between groups:
# Approach:
kruskal.test(CCC ~ Approach, data = SITES_One2One_canopy %>% filter(gas=="CO2")) 
kruskal.test(CCC ~ Approach, data = SITES_One2One_canopy %>% filter(gas=="H2O")) 

SITES_One2One_canopy.subCO2 <- SITES_One2One_canopy %>% filter(gas=="CO2")
SITES_One2One_canopy.subH2O <- SITES_One2One_canopy %>% filter(gas=="H2O")

 pairwise.wilcox.test( SITES_One2One_canopy.subCO2$CCC , SITES_One2One_canopy.subCO2$Approach,
                     p.adjust.method = "BH")

 library("ggpubr")

 
# Canopy Levels:
 kruskal.test(CCC ~ Canopy_L2, data = SITES_One2One_canopy %>% filter(gas=="CO2")) 
 kruskal.test(CCC ~ Canopy_L2, data = SITES_One2One_canopy %>% filter(gas=="H2O")) 
 
 
 pairwise.wilcox.test( SITES_One2One_canopy.subCO2$CCC , SITES_One2One_canopy.subCO2$Canopy_L1,
                       p.adjust.method = "BH")
 
 pairwise.wilcox.test( SITES_One2One_canopy.subH2O$CCC , SITES_One2One_canopy.subH2O$Canopy_L1,
                       p.adjust.method = "BH")
 
 
 plot.pairwise.co2.canopy <- SITES_One2One_canopy %>% filter(gas == "CO2") %>% ggplot( aes( x= Canopy_L1, y = CCC , col= Canopy_L1)) +
   geom_boxplot() + scale_color_manual(values=c("black", "black", "black"),
                                       name= "Canopy") +theme_bw() + xlab("Canopy Level") + theme(legend.position="none")
 
 plot.pairwise.co2.approach <- SITES_One2One_canopy %>% filter(gas == "CO2") %>% 
   ggplot( aes( x= Approach, y = CCC , col= Approach)) +
   geom_boxplot() + scale_color_manual(values=c("goldenrod", "aquamarine4", "darkmagenta"),
                                       name= "Approach") +theme_bw() + theme(legend.position="none")
 
 
 plot.pairwise.h2o.canopy <- SITES_One2One_canopy %>% filter(gas == "H2O") %>% ggplot( aes( x= Canopy_L1, y = CCC , col= Canopy_L1)) +
   geom_boxplot() + scale_color_manual(values=c("black", "black", "black"),
                                       name= "Canopy") +theme_bw()+ xlab("Canopy Level")+ theme(legend.position="none")
 
 plot.pairwise.h2o.approach <- SITES_One2One_canopy %>% filter(gas == "H2O") %>% 
   ggplot( aes( x= Approach, y = CCC , col= Approach)) +
   geom_boxplot() + scale_color_manual(values=c("goldenrod", "aquamarine4", "darkmagenta"),
                                       name= "Approach") +theme_bw()+ theme(legend.position="none")
 

evaluation.ccc.plot <-  ggarrange(  plot.pairwise.co2.canopy ,  plot.pairwise.co2.approach ,
             plot.pairwise.h2o.canopy ,  plot.pairwise.h2o.approach ,ncol=2, nrow=2, common.legend = FALSE,
             labels = c("A", "B", "C", "D"))
 
 
evaluation.ccc.plot

library(ggridges)


ggridges.co2 <- SITES_One2One_canopy %>% filter(gas=="CO2") %>% ggplot( aes(x = CCC, y = Canopy_L1, fill = Approach)) + geom_density_ridges(alpha=0.5) + theme_ridges() +
  theme(legend.position = "none") +
  scale_fill_manual(name = "Approach",
                    values=c("goldenrod", "aquamarine4", "darkmagenta"),
                    labels = c("MBR", "AE", "WP")) + ylab("") +xlab("CCC")

ggridges.h2o <- SITES_One2One_canopy %>% filter(gas=="H2O") %>% ggplot( aes(x = CCC, y = Canopy_L1, fill = Approach)) + geom_density_ridges(alpha=0.5) + theme_ridges() +
  theme(legend.position = "none") +
  scale_fill_manual(name = "Approach",
                    values=c("goldenrod", "aquamarine4", "darkmagenta"),
                    labels = c("MBR", "AE", "WP")) + ylab("") +xlab("CCC")

evaluation.ggridges <-  ggarrange( ggridges.co2, ggridges.h2o,ncol=2,
                                    labels = c("E", "F"))


final.eval.ccc.plot <- ggarrange( evaluation.ccc.plot, evaluation.ggridges , ncol=1)

ggsave("/Users/sm3466/YSE Dropbox/Sparkle Malone/Research/FluxGradient/lterwg-flux-gradient-eval/Figures/final.eval.ccc.plot.png", plot = final.eval.ccc.plot, width = 8, height = 9, units = "in")


# CO2: #####
plot.CCC.CO2.all <- SITES_One2One_canopy %>% filter(gas=="CO2") %>%  ggplot() + geom_point( aes(x= CCC, y=Site, col=Approach ), alpha= 0.7, size = 3) + facet_grid(cols = vars(Canopy_L1)) + scale_color_manual(values=c("goldenrod", "aquamarine4", "darkmagenta")) + theme_bw() + ylab("") + geom_vline(xintercept= 0.5, linetype="dashed", color = "red")

plot.CCC.CO2.density <- SITES_One2One_canopy %>% filter(gas=="CO2") %>%  ggplot() + geom_density(aes(x=CCC, col= Approach)) + scale_color_manual(values=c("goldenrod", "aquamarine4", "darkmagenta")) + theme_bw() + facet_wrap(~Canopy_L2) + ylab("Density")

summary.ccc.table.L2 <- SITES_One2One_canopy %>% reframe( .by= c(Canopy_L2, gas, Approach), CCC.mean= mean(CCC)
                                  , CCC.min= min(CCC)
                                  , CCC.max= max(CCC))

summary.ccc.table.L1 <- SITES_One2One_canopy %>% reframe( .by= c(Canopy_L1, gas, Approach), CCC.mean= mean(CCC)
                                                          , CCC.min= min(CCC)
                                                          , CCC.max= max(CCC))

summary.ccc.table.approach <- SITES_One2One_canopy %>% reframe( .by= c(gas, Approach), CCC.mean= abs(mean(CCC)), CCC.min= min(CCC)
                                                          , CCC.max= max(CCC))

ggsave("Figures/CCC_plot_co2.png", plot = plot.CCC.CO2.all, width = 8, height = 9, units = "in")

plot.CCC.H2O.all <- SITES_One2One_canopy %>% filter(gas=="H2O") %>%  ggplot() + geom_point( aes(x= CCC, y=Site, col=Approach ), alpha= 0.7, size = 3) + facet_grid(cols = vars(Canopy_L1)) + scale_color_manual(values=c("goldenrod", "aquamarine4", "darkmagenta")) + theme_bw() + ylab("") + geom_vline(xintercept= 0.5, linetype="dashed", color = "red")


ggsave("/Users/sm3466/YSE Dropbox/Sparkle Malone/Research/FluxGradient/lterwg-flux-gradient-eval/Figures/CCC_plot_H2O.png", plot = plot.CCC.H2O.all, width = 8, height = 9, units = "in")



#H2O: #####

plot.CCC.H2O.all <- SITES_One2One_canopy %>% filter(gas=="H2O") %>%  ggplot() + geom_point( aes(x= CCC, y=Site, col=Approach ), alpha= 0.7, size = 3) + facet_grid(cols = vars(Canopy_L1)) + scale_color_manual(values=c("goldenrod", "aquamarine4", "darkmagenta")) + theme_bw() + ylab("") 

plot.CCC.H2O.t0.5 <- SITES_One2One_canopy %>% filter(gas=="H2O", CCC> 0.50) %>%  ggplot() + geom_point( aes(x= CCC, y=Site, col=Approach , shape= Canopy_L1), alpha= 0.7, size = 3) + scale_color_manual(values=c("goldenrod", "aquamarine4", "darkmagenta")) + theme_bw() + ylab("")


# Count Evaluation:

Summary.CCC.t0.5 <- SITES_One2One_canopy %>% filter( CCC> 0.50) %>% reframe( .by=c(Canopy_L2, gas),
                                                                             Count_ML= length(Canopy_L2),
                                                                             Count_Sites = length(Site %>% unique))

counts.CO2.CCC<- SITES_One2One_canopy %>% filter( CCC> 0.50, gas=="CO2") %>% ggplot(aes(x= Approach, y=Good.CCC)) + geom_bar(stat = "identity") + theme_bw() + ylab ( 'Measurement Levels (CCC > 0.5)')    

counts.CO2.CCC.approach <- SITES_One2One_canopy %>% filter( CCC> 0.50, gas=="CO2") %>% ggplot(aes(x= Approach, y=Good.CCC)) + geom_bar(stat = "identity") + theme_bw() + ylab ( 'Measurement Levels (CCC > 0.5)') + facet_wrap(~ Canopy_L2, ncol=4)

counts.H2O.CCC <- SITES_One2One_canopy %>% filter( CCC> 0.50, gas=="H2O") %>% ggplot(aes(x= Approach, y=Good.CCC)) + geom_bar(stat = "identity") + theme_bw() + ylab ( 'Measurement Levels (CCC > 0.5)') 

counts.H2O.CCC.approach <- SITES_One2One_canopy %>% filter( CCC> 0.50, gas=="H2O") %>% ggplot(aes(x= Approach, y=Good.CCC)) + geom_bar(stat = "identity") + theme_bw() + ylab ( 'Measurement Levels (CCC > 0.5)') + facet_wrap(~ Canopy_L2, ncol=4)

# Relationship betwee counts and CCC:
plot.countbyCCC <- SITES_One2One_canopy  %>% ggplot(aes( x=count, y = CCC)) + geom_point() + 
  theme_bw() + geom_smooth(method = "loess")

plot.countbyApproach <- SITES_One2One_canopy %>%  ggplot(aes( y=count, x = Approach)) + geom_violin() + 
  theme_bw() + geom_point(position = position_jitter(seed = 1, width = 0.2), alpha=0.1)  + geom_boxplot(width=0.1)

localdir <- '/Volumes/MaloneLab/Research/FluxGradient/FluxData'
setwd(localdir)
save(plot.CCC.CO2.all, plot.CCC.CO2.t0.5 ,
     plot.CCC.H2O.all, plot.CCC.H2O.t0.5 ,
     plot.countbyCCC, plot.countbyCCC,plot.countbyApproach,counts.CO2.CCC, counts.CO2.CCC.approach,counts.H2O.CCC, counts.H2O.CCC.approach,
     SITES_One2One_canopy, SITES_One2One_canopy.Highest, SITES_One2One_canopy.Highest.100,
      Summary.CCC.t0.5, file = "flow.CCC_VIZ.R" )
drive_url <- googledrive::as_id("https://drive.google.com/drive/folders/14Ga9sLRMlQVvorZdHBiYxCbUybiGwPNp")
fileSave <- file.path(paste(localdir, "flow.CCC_VIZ.R", sep="/"))
googledrive::drive_upload(media = fileSave, overwrite = T, path = drive_url)
