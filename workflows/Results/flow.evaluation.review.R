
library(tidyverse)
localdir <- '/Volumes/MaloneLab/Research/FluxGradient/FluxData'

fileSave <- fs::path(localdir,paste0("SITE_DATA_FILTERED.Rdata"))
load(file=fileSave)

# Determine Sample sizes:

site.list <- names(SITE_DATA_FILTERED)

sample.data.total <- data.frame()
sample.data.canopy <- data.frame()
sample.data.month <- data.frame()

for ( site in site.list){
  print(site)
  
  # Add season to the file:
  
  sub.data.total <- SITE_DATA_FILTERED[[site]] %>% filter(gas == "CO2") %>% 
    reframe(.by=c(Approach), 
            FG_mean = length(na.omit(FG_mean)), 
            FC_turb_interp = length(na.omit(FC_turb_interp))) %>% mutate( SITE = site) 
  
  sample.data.total <- rbind( sample.data.total, sub.data.total)
  
  sub.data.canopy <- SITE_DATA_FILTERED[[site]] %>% filter(gas == "CO2") %>% 
    reframe(.by=c(Approach, Canopy_L1), 
            FG_mean = length(na.omit(FG_mean)), 
            FC_turb_interp = length(na.omit(FC_turb_interp))) %>% mutate( SITE = site)
  
  sample.data.canopy <- rbind( sample.data.canopy, sub.data.canopy)
  
  sub.data.month <- SITE_DATA_FILTERED[[site]] %>% filter(gas == "CO2") %>% 
    mutate( Month = timeEndA.local %>% format("%m") %>% as.numeric,
           Season = case_when(
             Month %in% c(12, 1, 2) ~ "DJF",
             Month %in% c(3, 4, 5) ~ "MAM",
             Month %in% c(6, 7, 8) ~ "JJA",
             Month %in% c(9, 10, 11) ~ "SON",
             TRUE ~ NA_character_ # Handle any unexpected cases
           )) %>% 
    reframe(.by=c(Approach, Season), 
            FG_mean = length(na.omit(FG_mean)), 
            FC_turb_interp = length(na.omit(FC_turb_interp))) %>% mutate( SITE = site)
  
  sample.data.month <- rbind( sample.data.month, sub.data.month)
  
  print('done')

                                                                              }

sample.data.total %>%  ggplot(aes(x=  Approach, y= FG_mean)) + 
  geom_boxplot(aes(x=  Approach, y= FG_mean, col=Approach)) + facet_wrap(~SITE) +
  ylab('Sample Size') + xlab('')

sample.data.canopy %>%  ggplot(aes()) + 
  geom_boxplot(aes(y= SITE, x= FG_mean,))  + 
  facet_wrap(~Canopy_L1,ncol=3) + xlab('Sample Size') + theme_bw()

sample.data.month %>%  ggplot(aes(y= SITE, x= FG_mean)) + 
  geom_boxplot(aes()) + facet_wrap(~Season,ncol=4) + 
  xlab('Sample Size') + theme_bw()

sample.data.total %>% ggplot( aes(x="", y=FG_mean, fill=Approach)) +
  geom_bar(stat="identity", color="white") +
  coord_polar("y", start= 0) +  theme_void() + facet_wrap(~SITE) +
  ylab('Sample Size') + xlab('')+ ylim(0, 17520)

sample.data.canopy %>% ggplot( aes(x="", y=FG_mean, fill= Canopy_L1)) +
  geom_bar(stat="identity") +
  coord_polar("y", start= 0) +  theme_void() + facet_wrap(~SITE) +
  ylab('Sample Size') + xlab('') + ylim(0, 17520)

# Stacked Histogram Approach
SITE_DATA_FILTERED$HARV %>% filter(gas=="CO2") %>% as.data.frame() %>% ggplot( aes(FG_mean)) + 
  geom_histogram(aes(fill = Approach), binwidth = 0.1, position = "fill",
                 na.rm = TRUE) 

# Linear plot
library(ggpubr) 

Linear_plots_approach <- list()

for (site in site.list){
  print(site)
  Linear_plots_approach[[site]] <- SITE_DATA_FILTERED[[site]] %>% filter(gas=="CO2") %>% 
    ggplot( aes( x= FG_mean, y = FC_turb_interp, col=Approach)) + 
    geom_point(size=0.1, alpha=0.05) + theme_bw() + xlim(-40, 40) + ylim(-40, 40) +
    geom_abline(intercept = 0, slope = 1, color = "black", linetype = "dashed") +
    geom_smooth(method = "lm", se = FALSE, size=0.5) +
    #stat_regline_equation(aes(label = paste(..eq.label.., ..rr.label.., sep = "~`,`~")), label.x = 2, label.y = c(40, 35, 30)) + xlab('GF') + ylab('EC') + 
    ggtitle(site) + ylab("GF") + ylab("EC")
  
}


ggarrange(plotlist =Linear_plots_approach[1:30], common.legend = TRUE)
  

Linear_plots <- list()

for (site in site.list){
  print(site)
  Linear_plots[[site]] <- SITE_DATA_FILTERED[[site]] %>% filter(gas=="CO2") %>% 
    ggplot( aes( x= FG_mean, y = FC_turb_interp)) + 
    geom_point(size=0.1, col="grey") + theme_bw() + xlim(-40, 40) + ylim(-40, 40) +
    geom_abline(intercept = 0, slope = 1, color = "black", linetype = "dashed") +
    geom_smooth(method = "lm", se = FALSE, size=0.5) +
    #stat_regline_equation(aes(label = paste(..eq.label.., ..rr.label.., sep = "~`,`~")), label.x = 2, label.y = c(40, 35, 30)) + xlab('GF') + ylab('EC') + 
    ggtitle(site) + ylab("GF") + ylab("EC")
  
}

ggarrange(plotlist =Linear_plots[1:30], common.legend = TRUE, align = "hv")


