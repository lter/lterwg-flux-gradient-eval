# This script explores the distribution of fluxes:
library(tidyverse)
library(dplyr)
library(colorspace)
library(ggpubr)
library(ggplot2)


load( fs::path(localdir,paste0("SITES_One2One.Rdata")))
load( fs::path(localdir,paste0("SITE_DATA_FILTERED_CCC.Rdata")))
source(fs::path(DirRepo.eval,'workflows/flow.igbp.R'))

# Explore Seasonal Distribution: ####

site.list <- SITE_DATA_FILTERED_CCC %>% names()

SITE_DATA_FLUX_COUNTS_SEASON <- data.frame()
SITE_DATA_FLUX_COUNTS_Approach <- data.frame()
SITE_DATA_FLUX_COUNTS_Total <- data.frame()
SITE_DATA_FLUX_COUNTS_HOUR <- data.frame()

for( site in site.list){
  print(site)
 
  df <-SITE_DATA_FILTERED_CCC[[site]] %>% filter(CCC >=0.5, gas != "CH4") %>% 
    mutate(month = format(timeEndA.local,'%m') %>% as.numeric,
           season = case_when(
             month %in% c(12, 1, 2) ~ "Winter",
             month %in% c(3, 4, 5) ~ "Spring",
             month %in% c(6, 7, 8) ~ "Summer",
             TRUE ~ "Autumn" # TRUE acts as the 'else' statement
           ),
           hour = format(timeEndA.local,'%H'),
           count= case_when( is.na(FG_mean) == FALSE ~ 1,
                            TRUE ~ 0)) %>% distinct
  
  df2 <-df %>% reframe(.by = c(gas, season), count.season = sum(count) ) %>% mutate( Site = site)
  df3 <-df %>% reframe(.by = c(gas, Approach), count.approach = sum(count) ) %>% mutate( Site = site)
  df4 <-df %>% reframe(.by = c(gas), count.total = sum(count) ) %>% mutate( Site = site)
  df5 <-df %>% reframe(.by = c(gas, hour, season), count.hour = sum(count) ) %>% mutate( Site = site)
  
  SITE_DATA_FLUX_COUNTS_SEASON <- rbind(SITE_DATA_FLUX_COUNTS_SEASON,   df2)
  SITE_DATA_FLUX_COUNTS_Total <- rbind(SITE_DATA_FLUX_COUNTS_Total,   df4)
  SITE_DATA_FLUX_COUNTS_Approach <- rbind(SITE_DATA_FLUX_COUNTS_Approach,   df3)
  SITE_DATA_FLUX_COUNTS_HOUR <- rbind(SITE_DATA_FLUX_COUNTS_HOUR,   df5)
  
  print('done')
}

SITE_DATA_FLUX_COUNTS_Approach_Total <- SITE_DATA_FLUX_COUNTS_Total %>% 
  full_join(SITE_DATA_FLUX_COUNTS_Approach, 
            by = c('Site', 'gas'),
            relationship = "many-to-many") %>% 
  mutate( percent = count.approach/count.total*100 %>% round(1))


SITE_DATA_FLUX_COUNTS_SEASON_Total <- SITE_DATA_FLUX_COUNTS_Total %>% 
  full_join(SITE_DATA_FLUX_COUNTS_SEASON,
            by = c('Site', 'gas'),
            relationship = "many-to-many")  %>%
  mutate( percent = count.season/count.total*100 %>% round(1),
          season = ordered(season, levels = c("Winter", "Spring", "Summer", "Autumn"))) %>% distinct

SITE_DATA_FLUX_COUNTS_Total_EcoType <-SITE_DATA_FLUX_COUNTS_Total %>% full_join(metadata.igbp , by="Site") %>% 
  reframe( .by= c(gas, EcoType), 
           counts = mean(count.total, na.rm=T),
           counts.SD = sd(count.total, na.rm=T),
           counts.min = counts - counts.SD, 
           counts.max = counts + counts.SD, )

# FLUX COUNTS BYECOTYPE: ####
plot.flux.count.ecotype <- SITE_DATA_FLUX_COUNTS_Total_EcoType %>%  
  na.omit() %>% 
  ggplot(aes( y = counts/10000, x = EcoType)) +
  geom_col( aes( y = counts/10000, x = EcoType), fill="transparent", col="black") + 
  geom_errorbar(aes(ymin = counts.min/10000, ymax = counts.max/10000), width = 0.2,
                col="red") +
  labs(y = expression(paste("Flux Measurements Site"^-1, "(Counts/10,000)")),
       x = "Ecosystem Type",
       title = "",
       fill = "")  +
  theme_bw() + facet_wrap(~gas, scales = "free_x")+ 
  theme(axis.text.x = element_text(angle = 45, vjust = 1, hjust=1),
        strip.background = element_rect(fill = "transparent"))


ggsave("/Users/sm3466/YSE Dropbox/Sparkle Malone/Research/FluxGradient/lterwg-flux-gradient-eval/Figures/SamplingFluxesEcotype.png", plot = ggarrange( plot.flux.count.ecotype, labels="D"), width = 8.5, height = 4, units = "in")

# FLUX COUNTS BY APPROACH: ####

plot.flux.count <- SITE_DATA_FLUX_COUNTS_Approach_Total %>%  
  ggplot() +
  geom_col( aes( x = count.total, y = Site, fill = Approach)) + 
  labs(x = "Flux Measurements (Counts)",
       y = "Sites",
       title = "",
       fill = "Approach")  +
  theme_bw()  + scale_fill_manual(values=c("goldenrod", "aquamarine4", "darkmagenta")) + facet_wrap(~gas, scales = "free_x")+ theme(axis.text.x = element_text(angle = 45, vjust = 1, hjust=1))

ggsave("/Users/sm3466/YSE Dropbox/Sparkle Malone/Research/FluxGradient/lterwg-flux-gradient-eval/Figures/Counts_Fluxes_Approach.png", plot = plot.flux.count , width =5, height = 6, units = "in")

# Summary for Approach: ####

SITE_DATA_FLUX_COUNTS_Approach_Total %>% reframe(.by=c(Approach, gas), 
                                                 count.mean = mean(count.total),
                                                 count.sd = sd(count.total))

# Percent 
plot.flux.count.percent <- SITE_DATA_FLUX_COUNTS_Approach_Total %>%  
  ggplot() +
  geom_col( aes( x = percent, y = Site, fill = Approach)) + 
  labs(x = "Flux Measurements (%)",
       y = "Sites",
       title = "",
       fill = "Approach")  +
  theme_bw()  + scale_fill_manual(values=c("goldenrod", "aquamarine4", "darkmagenta")) + facet_wrap(~gas, scales = "free_x")+ theme(axis.text.x = element_text(angle = 45, vjust = 1, hjust=1))

SITE_DATA_FLUX_COUNTS_Approach_Total %>% reframe(.by=c(Approach, gas), 
                                                 percent.mean = mean(percent),
                                                 percent.sd = sd(percent))

ggsave("/Users/sm3466/YSE Dropbox/Sparkle Malone/Research/FluxGradient/lterwg-flux-gradient-eval/Figures/Counts_Fluxes_Approach_Percent.png", plot = plot.flux.count.percent , width =5, height = 6, units = "in")


# Seasonal Analysis: #####

SITE_DATA_FLUX_COUNTS_SEASON_Total$season <- factor(SITE_DATA_FLUX_COUNTS_SEASON_Total$season, levels = c("Winter", "Spring", "Summer", "Autumn"))

# Hour

# Prepare the data: 
SITE_DATA_FLUX_COUNTS_HOUR_Season <- SITE_DATA_FLUX_COUNTS_SEASON %>% 
  full_join(SITE_DATA_FLUX_COUNTS_HOUR, 
            by = c('Site', 'gas', 'season'),
            relationship = "many-to-many") %>%
  mutate( percent = (count.hour/count.season*100) %>% round(1))

SITE_DATA_FLUX_COUNTS_HOUR_Season$season <- factor(SITE_DATA_FLUX_COUNTS_HOUR_Season$season, levels = c("Winter", "Spring", "Summer", "Autumn"))

# PLOTS for Season: ####

plot.counts.season <- SITE_DATA_FLUX_COUNTS_SEASON_Total %>% distinct %>%  ggplot(aes(x = percent, y = Site, fill = season)) +
  geom_col() + facet_wrap(~ gas) +
  theme_bw() +  scale_fill_discrete_qualitative(palette = "Harmonic") + theme(legend.position = "top") + labs(fill='Season') + xlab('Percent Coverage (%)')

plot.counts.season.hour.percent.h2o <- 
  SITE_DATA_FLUX_COUNTS_HOUR_Season  %>% filter(gas == "H2O") %>%
  reframe( .by= c(hour,season), percent.mean = mean(percent)) %>% 
  ggplot(aes( x = hour, y = percent.mean, col=season)) +
  geom_point( ) + geom_line(aes( x = hour, y = percent.mean, group=season)) + 
  geom_hline(yintercept = 4) +  scale_colour_discrete_qualitative(palette = "Harmonic") +
  theme_bw() + theme(legend.position = "none")+ ylab('Percent Coverage (%)') + xlab('Hour')

plot.counts.season.hour.percent.co2 <- SITE_DATA_FLUX_COUNTS_HOUR_Season  %>% filter(gas == "CO2") %>%
  reframe( .by= c(hour,season), percent.mean = mean(percent)) %>% 
  ggplot(aes( x = hour, y = percent.mean, col=season)) +
  geom_point( ) + geom_line(aes( x = hour, y = percent.mean, group=season)) + 
  geom_hline(yintercept = 4) +  scale_colour_discrete_qualitative(palette = "Harmonic") +
  theme_bw() + theme(legend.position = "none") + ylab('Percent Coverage (%)')+ xlab('Hour')

hour.plots <- ggarrange( plot.counts.season.hour.percent.co2,
                         plot.counts.season.hour.percent.h2o,  
                         nrow=2, labels= c('B', 'C'))

season.summary <- SITE_DATA_FLUX_COUNTS_SEASON_Total %>% reframe( .by=c(gas, season), percent.mean= mean(percent),  percent.sd= sd(percent) )

SITE_DATA_FLUX_COUNTS_SEASON_Total %>% reframe( .by=c(gas, season), percent.mean= mean(percent),  percent.sd= sd(percent) )


p_season <- ggplot(season.summary, aes(x = percent.mean, y = gas, fill = season))  + geom_col() +  scale_fill_discrete_qualitative(palette = "Harmonic")+ geom_text(aes(label = sprintf("%.0f%%", percent.mean)), position = position_stack(vjust = 0.5), size = 3) +
  labs(
    y = NULL,
    x = "Perent Coverage (%)",
    title = "",
    fill = "Canopy Level"
  )  + theme_bw()  + theme(legend.position = "none")

p_season_label <- ggarrange(p_season, labels="D")

final.season.hour.plots <- ggarrange( ggarrange( plot.counts.season, hour.plots, ncol=2,labels="A"), p_season_label ,ncol=1, nrow=2, heights = c(3, 1))

ggsave("/Users/sm3466/YSE Dropbox/Sparkle Malone/Research/FluxGradient/lterwg-flux-gradient-eval/Figures/Counts_Fluxes_Season-Hour_Percent.png", plot = final.season.hour.plots , width =9, height = 6.5, units = "in")

# Summary information for Season: ####
SITE_DATA_FLUX_COUNTS_SEASON_Total %>% names
SITE_DATA_FLUX_COUNTS_SEASON_Total %>% reframe( .by=c(gas, season), 
                                                percent.mean= mean(percent),  
                                                percent.sd= sd(percent))

# Hour Analysis: ####
SITE_DATA_FLUX_COUNTS_HOUR_Season <- SITE_DATA_FLUX_COUNTS_SEASON %>% 
  full_join(SITE_DATA_FLUX_COUNTS_HOUR, 
            by = c('Site', 'gas', 'season'),
            relationship = "many-to-many") %>%
  mutate( percent = (count.hour/count.season*100) %>% round(1)) %>% distinct

library(grDevices)

library(RColorBrewer)

sunset <- brewer.pal(9, "YlOrRd")

red_yellow_red_palette <- colorRampPalette(c("red", "Yellow", "Red"))
sunset <- red_yellow_red_palette(24)

plot.counts.season.hour.co2 <- SITE_DATA_FLUX_COUNTS_HOUR_Season  %>% filter(gas == "CO2") %>% 
  ggplot(aes(x = percent, y = Site, fill=hour)) +
  geom_col() + facet_wrap(~season, ncol=4) +
  theme_bw() + scale_fill_manual(values=sunset)+ labs(fill='Hour') + 
  theme(legend.position = 'top') + xlab('Percent Coverage (%)') + 
  guides(fill = guide_legend(nrow = 3)) + 
  theme( legend.text = element_text(size = 6),
    legend.title = element_text(size = 11, face = "bold"))

plot.counts.season.hour.h2o <- SITE_DATA_FLUX_COUNTS_HOUR_Season  %>% filter(gas == "H2O") %>% 
  ggplot(aes(x = percent, y = Site, fill=hour)) +
  geom_col() + facet_wrap(~season, ncol=4) +
  theme_bw() + scale_fill_manual(values=sunset)+ labs(fill='Hour') + 
  theme(legend.position = 'top') + xlab('Percent Coverage (%)') + 
  guides(fill = guide_legend(nrow = 3)) + 
  theme( legend.text = element_text(size = 6),
         legend.title = element_text(size = 11, face = "bold"))


ggsave("/Users/sm3466/YSE Dropbox/Sparkle Malone/Research/FluxGradient/lterwg-flux-gradient-eval/Figures/Counts_Fluxes_Season-Hour_Percent_CO2.png", plot = plot.counts.season.hour.co2  , width =6, height = 7, units = "in")

ggsave("/Users/sm3466/YSE Dropbox/Sparkle Malone/Research/FluxGradient/lterwg-flux-gradient-eval/Figures/Counts_Fluxes_Season-Hour_Percent_H2O.png", plot = plot.counts.season.hour.h2o  , width =6, height = 7, units = "in")



# Measure the overlap: #####
site.list <- SITE_DATA_FILTERED_CCC %>% names()

SITE_DATA_FLUX_COUNTS_Overlap <- data.frame()

for( site in site.list){
  print(site)
  
  df <-SITE_DATA_FILTERED_CCC[[site]] %>% filter(CCC >=0.5 , gas != "CH4") %>% 
    mutate(month = format(timeEndA.local,'%m') %>% as.numeric,
           season = case_when(
             month %in% c(12, 1, 2) ~ "Winter",
             month %in% c(3, 4, 5) ~ "Spring",
             month %in% c(6, 7, 8) ~ "Summer",
             TRUE ~ "Autumn" # TRUE acts as the 'else' statement
           ),
           hour = format(timeEndA.local,'%H'),
           count= case_when( is.na(FG_mean) == FALSE ~ 1,
                             TRUE ~ 0)) %>% distinct

  df.MBR <-df %>% filter( Approach == 'MBR') %>% rename(MBR.count = count) %>% select(timeEndA.local,MBR.count, gas)
  df.AE <-df %>% filter( Approach == 'AE') %>% rename(AE.count = count) %>% select(timeEndA.local,AE.count, gas)
  df.WP <-df %>% filter( Approach == 'WP') %>% rename(WP.count = count) %>% select(timeEndA.local, WP.count, gas)
  
  df.approach <- df.MBR %>% full_join( df.AE, by=c('timeEndA.local', 'gas')) %>% 
    full_join( df.WP, by=c('timeEndA.local', 'gas')) %>%   mutate(across(everything(), ~replace_na(.x, 0))) %>% mutate(OL.MBR.AE = MBR.count + AE.count,
                                                      OL.MBR.WP = case_when(MBR.count + WP.count >= 2 ~ 1, TRUE ~0) ,
                                                      OL.MBR.AE = case_when(MBR.count + AE.count  >= 2~ 1, TRUE ~0),
                                                      OL.AE.WP = case_when(WP.count + AE.count >= 2 ~ 1, TRUE ~0) ,
                                                      OL.MBR.AE.WP = case_when(MBR.count + AE.count + WP.count >= 3~ 1, TRUE ~0),
                                                      count = 1) %>% 
    reframe( .by= gas,
      OL.MBR.WP = sum(OL.MBR.WP),
             OL.MBR.AE = sum(OL.MBR.AE),
             OL.AE.WP = sum(OL.AE.WP),
             OL.MBR.AE.WP = sum(OL.MBR.AE.WP),
             total = sum(count)) %>% mutate( OL.MBR.WP.pt = ( OL.MBR.WP-OL.MBR.AE.WP)/total *100,
                                             OL.MBR.AE.pt = (OL.MBR.AE-OL.MBR.AE.WP)/total *100,
                                                        OL.AE.WP.pt = (OL.AE.WP-OL.MBR.AE.WP)/total *100,
                                                        OL.MBR.AE.WP.pt = OL.MBR.AE.WP/total *100,
                                                        Site = site) 
  
  
  SITE_DATA_FLUX_COUNTS_Overlap <- rbind(SITE_DATA_FLUX_COUNTS_Overlap, df.approach)
  
  print('done')
}


# Make changes to the Dataframe to make the figure:
SITE_DATA_FLUX_COUNTS_Overlap %>% names

df.1 <- SITE_DATA_FLUX_COUNTS_Overlap %>% 
  select(OL.MBR.AE.WP.pt, Site, gas) %>% 
  mutate( Overlap = "MBR+AE+WP") %>% 
  rename( Percent = OL.MBR.AE.WP.pt) %>% 
  select(Site, Percent, Overlap , gas)

df.2 <- SITE_DATA_FLUX_COUNTS_Overlap %>% 
  select(OL.AE.WP.pt, Site, gas) %>% 
  mutate( Overlap = "AE+WP") %>% 
  rename( Percent = OL.AE.WP.pt) %>% 
  select(Site, Percent, Overlap, gas )

df.3 <- SITE_DATA_FLUX_COUNTS_Overlap %>% 
  select(OL.MBR.AE.pt, Site, gas) %>% 
  mutate( Overlap = "MBR+AE") %>% 
  rename( Percent = OL.MBR.AE.pt) %>% 
  select(Site, Percent, Overlap, gas )

df.4 <- SITE_DATA_FLUX_COUNTS_Overlap %>% 
  select(OL.MBR.WP.pt, Site, gas) %>% 
  mutate( Overlap = "MBR+WP") %>% 
  rename( Percent = OL.MBR.WP.pt) %>% 
  select(Site, Percent, Overlap, gas )

SITE_DATA_FLUX_COUNTS_Overlap_Long <- rbind( df.1, df.2, df.3, df.4) %>% fortify()
SITE_DATA_FLUX_COUNTS_Overlap_Long$Overlap %>% unique
SITE_DATA_FLUX_COUNTS_Overlap_Long$Overlap <- as.factor( SITE_DATA_FLUX_COUNTS_Overlap_Long$Overlap)
SITE_DATA_FLUX_COUNTS_Overlap_Long$Overlap <- factor(SITE_DATA_FLUX_COUNTS_Overlap_Long$Overlap, levels = c("MBR+AE", "MBR+WP", "AE+WP", "MBR+AE+WP"))


# Make one dataset with an index for the type of overlap and do the stacked bar charts

plot.overlap.1 <- SITE_DATA_FLUX_COUNTS_Overlap_Long %>% 
  ggplot(aes(x = Percent, y = Site, fill=Overlap)) +
  geom_col() + facet_wrap(~ gas, ncol=2) +
  theme_bw() +  scale_fill_discrete_sequential(palette = "Mint", nmax = 4, order = c(1, 2, 3, 4)) + labs(fill='') + 
  theme(legend.position = 'top') + xlab('Percent Overlap (%)') + 
  guides(fill = guide_legend(nrow = 1)) + 
  theme( legend.text = element_text(size = 6),
         legend.title = element_text(size = 11, face = "bold"),
         strip.background = element_rect(fill = "transparent", size = 0.5)) + xlab('Temporal Overlap (%)')

SITE_DATA_FLUX_COUNTS_Overlap_Long_summary <- SITE_DATA_FLUX_COUNTS_Overlap_Long %>% 
  reframe( .by=c(gas, Overlap), mean.percent = mean(Percent)) %>% 
  arrange(gas)

plot.overlap.2 <- SITE_DATA_FLUX_COUNTS_Overlap_Long_summary %>% 
  ggplot(aes(x = mean.percent, y = gas, fill=Overlap)) +
  geom_col() +
  theme_bw() +  scale_fill_discrete_sequential(palette = "Mint", nmax = 4, order = c(1, 2, 3, 4)) + 
  xlab('Percent Overlap (%)') + 
  theme( legend.text = element_text(size = 6),
         legend.title = element_text(size = 11, face = "bold"),
         strip.background = element_rect(fill = "transparent", size = 0.5),
         legend.position = "none")+ ylab("")


final.overlap.plot <- ggarrange(plot.overlap.1, plot.overlap.2, 
          ncol=1, nrow=2, heights = c(3, 0.5), labels=c("A", "B"))


ggsave("/Users/sm3466/YSE Dropbox/Sparkle Malone/Research/FluxGradient/lterwg-flux-gradient-eval/Figures/Counts_Fluxes_Overlap.png", plot = final.overlap.plot, width = 6, height = 7, units = "in")


SITE_DATA_FLUX_COUNTS_Overlap_Long %>% 
  reframe( .by=c(gas, Overlap), mean.percent = mean(Percent), min.percent = min(Percent), max.percent = max(Percent)) %>% 
  arrange(gas)

# Save the files: ####
data.dir <- '/Volumes/MaloneLab/Research/FluxGradient' # Where do you want to save the plots
save(
  SITES_One2One, 
  SITE_DATA_FLUX_COUNTS_SEASON, 
  SITE_DATA_FLUX_COUNTS_Approach ,
  SITE_DATA_FLUX_COUNTS_Total,
  SITE_DATA_FLUX_COUNTS_HOUR ,
  SITE_DATA_FLUX_COUNTS_Approach_Total,
  SITE_DATA_FLUX_COUNTS_SEASON_Total, 
  SITE_DATA_FLUX_COUNTS_HOUR_Season ,
  SITE_DATA_FLUX_COUNTS_SEASON_Total,
  SITE_DATA_FLUX_COUNTS_HOUR_Season,
  SITE_DATA_FLUX_COUNTS_Overlap ,
  SITE_DATA_FLUX_COUNTS_Overlap_Long,
  SITE_DATA_FLUX_COUNTS_Overlap_Long_summary, file= paste(data.dir,"/flow.flux.counts.R", sep=""))

