# Ecosystem Based Figures: 

library(tidyverse)
library(dplyr)
library(colorspace)
library(ggpubr)
library(ggplot2)

load( fs::path(localdir,paste0("SITES_One2One_AA_AW.Rdata")))
load( fs::path(localdir,paste0("SITE_DATA_FILTERED_CCC_AA_AW.Rdata")))
source(fs::path(DirRepo.eval,'workflows/flow.igbp.R'))

SITES_One2One_canopy <- SITES_One2One %>% left_join( metadata.igbp, by='Site') %>% 
  mutate(counter=1) %>% filter(CCC > 0.5, Canopy_L1 != "WW") 

Summary.approach.ecotype <- SITES_One2One_canopy %>% reframe( .by=c(EcoType, gas), 
                                  Total.counter = sum(counter)) %>% 
  left_join(SITES_One2One_canopy %>% 
              reframe( .by=c(EcoType, gas, Approach), 
                       Approach.counter = sum(counter)),
          by= c('EcoType', 'gas')) %>% mutate(percent =Approach.counter/Total.counter*100 ) %>% 
  ggplot() +
  geom_col( aes( x = EcoType, y = percent , fill = Approach))  +
  labs(x = "",
       y = "Reliable Sampling Height Pairs (%)",
       title = "",
       fill = "Approach")  +
  theme_bw()  + scale_fill_manual(values=c("goldenrod", "aquamarine4", "darkmagenta"))  +
  facet_wrap(~gas, scales = "free_x")+
  theme(axis.text.x = element_text(angle = 45, vjust = 1, hjust=1))+
  theme(strip.background = element_rect(fill = 'white'))




my_colors <- c("AA" = "violetred2" , "AW" = "salmon1")


# Canopy Level:

Summary.canopyL.ecotype <- SITES_One2One_canopy %>% reframe( .by=c(EcoType, gas), 
                                                              Total.counter = sum(counter)) %>% 
  left_join(SITES_One2One_canopy %>% 
              reframe( .by=c(EcoType, gas, Canopy_L1), 
                       Canopy_L1.counter = sum(counter)),
            by= c('EcoType', 'gas')) %>% mutate(percent =Canopy_L1.counter/Total.counter*100 ) %>% 
  ggplot() +
  geom_col( aes( x = EcoType, y = percent , fill = Canopy_L1))  +
  labs(x = "",
       y = "Reliable Sampling Height Pairs (%)",
       title = "",
       fill = "Canopy Level")  +
  theme_bw()  + scale_fill_manual(values=my_colors)  +
  facet_wrap(~gas, scales = "free_x")+
  theme(axis.text.x = element_text(angle = 45, vjust = 1, hjust=1))+
  theme(strip.background = element_rect(fill = 'white'))




 
RSHP.CCC.ecotype <- SITES_One2One_canopy %>% filter(CCC > 0.5) %>% ggplot() + 
  geom_boxplot( aes(x= EcoType, y= CCC)) + facet_wrap(~gas) + theme_bw() + 
  ylab("CCC (EC)") + xlab("")+
  theme(strip.background = element_rect(fill = 'white'))+
  theme(axis.text.x = element_text(angle = 45, vjust = 1, hjust=1))




total.RSHP <- SITES_One2One_canopy %>% filter(CCC > 0.5) %>% reframe( .by=c(Site, gas), 
                                                   Total.RSHP = sum(counter) %>% as.numeric) %>% left_join( SITES_One2One_canopy , by=c("Site", "gas")) %>% 
  reframe( .by=c( EcoType, gas), mean.rshp = mean(Total.RSHP, na.rm = T ),
           sd.rshp_p = sd(Total.RSHP, na.rm = T ) + mean.rshp,
           sd.rshp_m =  mean.rshp-sd(Total.RSHP, na.rm = T ) )

total.RSHP.plot <- total.RSHP %>% ggplot() + 
  geom_col(aes( x= EcoType, y=mean.rshp)) + 
  facet_wrap(~gas) + theme_bw() + ylab('Reliable Sampling Height Pairs (Site)') + 
  geom_errorbar(aes(x=EcoType, ymin=sd.rshp_m, ymax=sd.rshp_p),
                width=0.4, colour="orange", alpha=0.9, size=1.3) +
  theme(strip.background = element_rect(fill = 'white'))+
  theme(axis.text.x = element_text(angle = 45, vjust = 1, hjust=1))+ xlab("")


eco.plot.final <- ggarrange( Summary.approach.ecotype ,
                             Summary.canopyL.ecotype ,
                             RSHP.CCC.ecotype,
                             total.RSHP.plot, labels=c("A", "B", "C", "D"))

ggsave("/Users/sm3466/YSE Dropbox/Sparkle Malone/Research/FluxGradient/lterwg-flux-gradient-eval/Figures/WF_Version1/EcoType_Results_AA_AW.png", plot = eco.plot.final , width =8, height = 7, units = "in")


             
