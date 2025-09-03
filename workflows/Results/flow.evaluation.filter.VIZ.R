#flow.evaluation.filter.Vizualization:
rm(list=ls())

# This script is developed to understand the following:
# 1. How much data is left?
# 2. How much was lost to different filters?
# 3. How well does the remaining data sample hours*months?
# 4. What are the potential biases of the remaining data?

library(tidyverse)

metadata <- read.csv('/Volumes/MaloneLab/Research/FluxGradient/Ameriflux_NEON field-sites.csv') # has a list of all the sites

# -------------------------------------------------------
site.list <- metadata$Site_Id.NEON %>% unique
localdir <- '/Volumes/MaloneLab/Research/FluxGradient/FluxData'
DirRepo <-"/Users/sm3466/YSE Dropbox/Sparkle Malone/Research/FluxGradient/lterwg-flux-gradient-eval"

setwd(DirRepo)

# Compiles Dataframes into one list:
source(fs::path(DirRepo,'workflows/flow.evaluation_SITELIST.R'))

canopy <- read.csv(file.path(paste(localdir, "canopy_commbined.csv", sep="/"))) %>% distinct  %>% mutate( site = Site)

total.report.CO2  <- filter.report.CO2  %>%  full_join(canopy,  by=c( 'dLevelsAminusB', 'site')) %>% mutate( approach = approach %>% as.factor,
                                                                                                             Canopy_L2 =  factor(Canopy_L2, levels = c( "AA", "AA+", "AW+" , "AW-" ,"AW+-", "AW", "WW", "WW-" ))) 

total.report.H2O <- filter.report.H2O  %>%  full_join(canopy,  by=c( 'dLevelsAminusB', 'site')) %>% mutate( approach = approach %>% as.factor,
                                                                                                             Canopy_L2 =  factor(Canopy_L2, levels = c( "AA", "AA+", "AW+" , "AW-" ,"AW+-", "AW", "WW", "WW-" ))) 


total.report.stability.CO2 <- filter.report.stability.CO2  %>%  full_join(canopy,  by=c( 'dLevelsAminusB', 'site')) %>% mutate( approach = approach %>% as.factor)

total.report.stability.H2O <- filter.report.stability.H2O   %>%  full_join(canopy,  by=c( 'dLevelsAminusB', 'site')) %>% mutate( approach = approach %>% as.factor)

# Change the order of the L2 factor:

total.report.CO2 %>% ggplot( aes( x= approach, y = flag.interaction.ALL)) + geom_violin() +
  facet_wrap(~ site) + xlab("") + ylab("Filtered (%)") + theme_bw() + ylim(0,100)

total.report.H2O %>% ggplot( aes( x= approach, y = flag.interaction.ALL)) + geom_violin() +
  facet_wrap(~ site) + xlab("") + ylab("Filtered (%)") + theme_bw() + ylim(0,100)

# How much data is lost by approach: 
total.report.CO2 %>% reframe( .by = c(approach) ,flag.interaction.ALL = mean(flag.interaction.ALL)) 
total.report.H2O %>% reframe( .by = c(approach) ,flag.interaction.ALL = mean(flag.interaction.ALL)) 

approach.violin.plot.CO2 <- total.report.CO2 %>% ggplot(aes( x= approach, y = flag.interaction.ALL) ) + geom_violin( ) + geom_point(position = position_jitter(seed = 1, width = 0.2), alpha=0.1)  + geom_boxplot(width=0.1)+ xlab("") + ylab("Filtered (%)") + ylim(0, 100) +
  theme(legend.position = "none") + theme_bw() +
  annotate("text", x = 1, y = 35, label = mean(total.report.CO2$flag.interaction.ALL[total.report.CO2$approach == "AE"]) %>% round(1), size = 4)+
  annotate("text", x = 2, y = 35, label = mean(total.report.CO2$flag.interaction.ALL[total.report.CO2$approach == "MBR"]) %>% round(1), size = 4)+
  annotate("text", x = 3, y = 35, label = mean(total.report.CO2$flag.interaction.ALL[total.report.CO2$approach == "WP"]) %>% round(1), size = 4)

approach.violin.plot.H2O <- total.report.H2O %>% ggplot(aes( x= approach, y = flag.interaction.ALL) ) + geom_violin( ) + geom_point(position = position_jitter(seed = 1, width = 0.2), alpha=0.1)  + geom_boxplot(width=0.1)+ xlab("") + ylab("Filtered (%)") + ylim(0, 100) +
  theme(legend.position = "none") + theme_bw() +
  annotate("text", x = 1, y = 15, label = mean(total.report.H2O $flag.interaction.ALL[total.report.H2O $approach == "AE"]) %>% round(1), size = 4)+
  annotate("text", x = 2, y = 15, label = mean(total.report.H2O $flag.interaction.ALL[total.report.H2O $approach == "MBR"]) %>% round(1), size = 4)+
  annotate("text", x = 3, y = 15, label = mean(total.report.H2O $flag.interaction.ALL[total.report.H2O $approach == "WP"]) %>% round(1), size = 4)

# How much data is lost by level:
total.report.CO2 %>% reframe( .by = c(Canopy_L2) ,flag.interaction.ALL = mean(flag.interaction.ALL)) 

total.report.H2O %>% reframe( .by = c(Canopy_L2) ,flag.interaction.ALL = mean(flag.interaction.ALL))

MLevels.violin.plot.CO2 <- total.report.CO2 %>%  ggplot(aes( x= Canopy_L2, y = flag.interaction.ALL) ) + geom_violin( ) + geom_point(position = position_jitter(seed = 1, width = 0.2), alpha=0.1)  + geom_boxplot(width=0.1)+ xlab("") + ylab("") + ylim(0, 100) + theme_bw() +  labs(col = "Canopy Level")

MLevels.violin.plot.H2O <- total.report.H2O %>%  ggplot(aes( x= Canopy_L2, y = flag.interaction.ALL) ) + geom_violin( ) + geom_point(position = position_jitter(seed = 1, width = 0.2), alpha=0.1)  + geom_boxplot(width=0.1)+ xlab("") + ylab("") + ylim(0, 100) + theme_bw() +  labs(col = "Canopy Level")


MLevels.approach.site.violin.plot.CO2 <- total.report.CO2 %>%  ggplot(aes( x= approach, y = flag.interaction.ALL) ) + geom_violin( ) + 
  ylab("Filtered (%)") + ylim(0, 100) +
  theme(legend.position = "none") + theme_bw() + facet_wrap(~Site)

MLevels.approach.site.violin.plot.H2O <- total.report.H2O %>%  ggplot(aes( x= approach, y = flag.interaction.ALL) ) + geom_violin( ) + 
  ylab("Filtered (%)") + ylim(0, 100) +
  theme(legend.position = "none") + theme_bw() + facet_wrap(~Site)


# Which filter led to the most loss?
p.all.ustar.CO2 <- total.report.CO2 %>%  ggplot(aes( x= Canopy_L2, y = flag.ustar_interp) ) + geom_violin( ) + geom_point(position = position_jitter(seed = 1, width = 0.2), alpha=0.1)  + 
  geom_boxplot(width=0.1)+ xlab("") + ylab("Filtered (%)") + ylim(0, 100) + guides(col = "none") + theme_bw()

p.all.ustar.H2O <- total.report.H2O %>%  ggplot(aes( x= Canopy_L2, y = flag.ustar_interp) ) + geom_violin( ) + geom_point(position = position_jitter(seed = 1, width = 0.2), alpha=0.1)  + 
  geom_boxplot(width=0.1)+ xlab("") + ylab("Filtered (%)") + ylim(0, 100) + guides(col = "none") + theme_bw()


p.all.snr.CO2  <- total.report.CO2  %>%  ggplot(aes( x= Canopy_L2, y = flag.dConcSNR) ) + geom_violin( ) + geom_point(position = position_jitter(seed = 1, width = 0.2), alpha=0.1)  + geom_boxplot(width=0.1)+ xlab("") + ylab("") + ylim(0, 100) + guides(col = "none") + theme_bw()

p.all.snr.H2O <- total.report.H2O %>%  ggplot(aes( x= Canopy_L2, y = flag.dConcSNR) ) + geom_violin( ) + geom_point(position = position_jitter(seed = 1, width = 0.2), alpha=0.1)  + geom_boxplot(width=0.1)+ xlab("") + ylab("") + ylim(0, 100) + guides(col = "none") + theme_bw()


p.all.tsnr.CO2 <- total.report.CO2 %>% filter(approach == "MBR") %>% ggplot(aes( x= Canopy_L2, y = flag.dConcTSNR) ) + geom_violin( ) + geom_point(position = position_jitter(seed = 1, width = 0.2), alpha=0.1)  + geom_boxplot(width=0.1) + xlab("") + ylab("") + ylim(0, 100) + theme_bw() + guides(col = "none")

p.all.tsnr.H2O <- total.report.H2O %>% filter(approach == "MBR") %>%  ggplot(aes( x= Canopy_L2, y = flag.dConcTSNR) ) + geom_violin( ) + geom_point(position = position_jitter(seed = 1, width = 0.2), alpha=0.1)  + geom_boxplot(width=0.1) + xlab("") + ylab("") + ylim(0, 100) + theme_bw() + guides(col = "none")


top.plot.CO2 <- ggpubr::ggarrange(approach.violin.plot.CO2, MLevels.violin.plot.CO2, nrow=1, ncol=2, common.legend = TRUE, labels=c("A", "B"))

bottom.plot.CO2 <- ggpubr::ggarrange(p.all.ustar.CO2 ,
                  p.all.snr.CO2,
                  p.all.tsnr.CO2, nrow=1, ncol=3, common.legend = TRUE, labels=c("C", "D", "E"))

filter.plot.CO2 <- ggpubr::ggarrange(top.plot.CO2 ,bottom.plot.CO2 ,  nrow=2, ncol=1, common.legend = TRUE)

ggsave("Figures/Filter_plot_CO2.png", plot = filter.plot.CO2, width = 9, height = 6, units = "in")



top.plot.H2O <- ggpubr::ggarrange(approach.violin.plot.H2O, MLevels.violin.plot.H2O, nrow=1, ncol=2, common.legend = TRUE, labels=c("A", "B"))

bottom.plot.H2O <- ggpubr::ggarrange(p.all.ustar.H2O ,
                                     p.all.snr.H2O,
                                     p.all.tsnr.H2O, nrow=1, ncol=3, common.legend = TRUE, labels=c("C", "D", "E"))

filter.plot.H2O <- ggpubr::ggarrange(top.plot.H2O ,bottom.plot.H2O ,  nrow=2, ncol=1, common.legend = TRUE)

ggsave("Figures/Filter_plot_H2O.png", plot = filter.plot.H2O, width = 9, height = 6, units = "in")

# Need to update below to consider the gas of interst

total.report.CO2$flag.ustar_interp %>% mean
total.report.CO2$flag.ustar_interp %>% sd
total.report.CO2$flag.dConcSNR %>% mean
total.report.CO2$flag.dConcSNR %>% sd

total.report.CO2$flag.dConcTSNR %>% mean
total.report.H2O$flag.dConcTSNR %>% mean


summary.apprach.CO2 <- total.report.CO2 %>% reframe( .by=approach,
                                             flag.ustar_interp = mean(flag.ustar_interp),
                                             flag.dConcSNR = mean(flag.dConcSNR),
                                             flag.dConcTSNR = mean(flag.dConcTSNR),
                                             flag.interaction.ALL = mean(flag.interaction.ALL))


summary.apprach.H2O <- total.report.H2O %>% reframe( .by=approach,
                                                     flag.ustar_interp = mean(flag.ustar_interp),
                                                     flag.dConcSNR = mean(flag.dConcSNR),
                                                     flag.dConcTSNR = mean(flag.dConcTSNR),
                                                     flag.interaction.ALL = mean(flag.interaction.ALL))

# Summarize Canopy:
# Same for both gases!
Canopy.summary<- total.report.CO2 %>% reframe(.by= c(Canopy_L2) , Sites = site %>% unique %>% length)

 
total.report.CO2 %>% ggplot( aes( Canopy_L2, col=Canopy_L2, fill=Canopy_L2)) + geom_bar() + theme_bw() + ylab('Measurement Levels') + xlab ("")

all.sites <-total.report.CO2 $site %>% unique 

listofsites <- total.report.CO2 %>%  filter( Canopy_L1 == "AA") %>% select( site) 
AA.sites <- listofsites$site%>% unique 

setdiff(all.sites, AA.sites)

