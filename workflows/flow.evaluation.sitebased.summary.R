# Site based example: 


localdir <- '/Volumes/MaloneLab/Research/FluxGradient/FluxData'
DirRepo <-"/Users/sm3466/YSE Dropbox/Sparkle Malone/Research/FluxGradient/lterwg-flux-gradient-eval"
load( fs::path(localdir,paste0("SITES_One2One.Rdata")))
load( fs::path(localdir,paste0("SITE_DATA_FILTERED.Rdata")))

# Build the dataset for canopy Information: ####
canopy <- read.csv(file.path(paste(localdir, "canopy_commbined.csv", sep="/"))) %>% distinct

Highest.CCC <- SITES_One2One %>% reframe(.by= c(Site, gas, Approach), CCC.max = max(CCC, na.rm=T), R2.max = max(R2, na.rm=T) )

Highest.CCC %>% ggplot( aes( x= CCC.max, y= R2.max)) + geom_point()

SITES_One2One_canopy <- SITES_One2One %>% full_join(canopy, by=c("Site", "dLevelsAminusB" ) ) %>% 
  full_join( Highest.CCC,by = c("Site", "gas", "Approach")) %>%
  mutate(Approach = factor(Approach, levels = c("MBR", "AE", "WP") ),
         Good.CCC = case_when(  CCC >= 0.5 ~ 1, CCC < 0.5 ~ 0) %>% as.factor,
         RelativeDistB = MeasurementHeight_m_B - CanopyHeight ) %>% distinct

aoi <- c( 'JORN', 'KONZ', 'GUAN', 'HARV')


# Methods Panel ####
summarize.method.pairs <- function( site){
  
  df_method <- SITES_One2One_canopy %>% filter( CCC >= 0.5, Site == site ) %>%  mutate( count = 1) %>% reframe(.by= c(gas, Approach), n_pairs = sum(count)) |>
    group_by(gas) |>
    mutate(percent = 100 * n_pairs / sum(n_pairs)) |>
    ungroup()
  
  return( df_method)
  
}

df_method.JORN <- summarize.method.pairs(site ="JORN")
df_method.KONZ <- summarize.method.pairs(site ="KONZ")
df_method.GUAN <- summarize.method.pairs(site ="GUAN")
df_method.HARV <- summarize.method.pairs(site ="HARV")

## ======================
summarize.method.pairs.plot <- function( df_method){
  
  
  totalpairs = df_method %>% reframe( .by=gas, n = sum(n_pairs))
  
  p_method <- ggplot(df_method , aes(x = gas, y = percent, fill = Approach),alpha=0.05) +
    geom_col() +
    geom_text(aes(label = sprintf("%.0f%%", percent)),
              position = position_stack(vjust = 0.5), size = 3) +
    labs(x = NULL,
         y = "Sampling Height Pairs (%)",
         title = "",
         fill = "Approach")  +
    theme_bw()  + scale_fill_manual(values=c("goldenrod", "aquamarine4", "darkmagenta")) +
    theme( legend.position = "none")
  
  return(p_method )
}

plot.method.pairs.JORN  <- summarize.method.pairs.plot(df_method =  df_method.JORN)
plot.method.pairs.KONZ  <- summarize.method.pairs.plot(df_method =  df_method.KONZ)
plot.method.pairs.GUAN  <- summarize.method.pairs.plot(df_method =  df_method.GUAN)
plot.method.pairs.HARV  <- summarize.method.pairs.plot(df_method =  df_method.HARV)
## ======================
method.panel <- ggarrange(plot.method.pairs.JORN,plot.method.pairs.KONZ, plot.method.pairs.GUAN, plot.method.pairs.HARV ,
          ncol=4)


# Canopy Level Panel :  ####

summarize.canopy.pairs <- function( site){
  
  df_method <- SITES_One2One_canopy %>% filter( CCC >= 0.5, Site == site) %>%  mutate( count.sh = 1) %>% reframe(.by= c(gas, Canopy_L1), n_pairs = sum(count.sh)) |>
    group_by(gas) |>
    mutate(percent = 100 * n_pairs / sum(n_pairs)) |>
    ungroup() 
  
  return( df_method)
  
}

df_canopy.JORN <- summarize.canopy.pairs(site ="JORN")
df_canopy.KONZ <- summarize.canopy.pairs(site ="KONZ")
df_canopy.GUAN <- summarize.canopy.pairs(site ="GUAN")
df_canopy.HARV <- summarize.canopy.pairs(site ="HARV")

# NOTE: if you revise the counts, you only need to edit df_canopy above.

summarize.canopy.pairs.plot <- function( df_canopy){
  p_canopy <- ggplot(df_canopy, aes(x = gas, y = percent, fill = Canopy_L1)) +
    geom_col() +
    geom_text(aes(label = sprintf("%.0f%%", percent)),
              position = position_stack(vjust = 0.5), size = 3) +
    labs(
      x = NULL,
      y = "Sampling Height Pairs (%)",
      title = "",
      fill = "Canopy Level"
    )  +
    theme_bw() +    scale_fill_discrete_sequential(palette = "OrYel") + theme( legend.position = 'none')
  
  
  return(p_canopy )
}

plot.canopy.pairs.JORN  <- summarize.canopy.pairs.plot(df_canopy =  df_canopy.JORN)
plot.canopy.pairs.KONZ  <- summarize.canopy.pairs.plot(df_canopy =  df_canopy.KONZ)
plot.canopy.pairs.GUAN  <- summarize.canopy.pairs.plot(df_canopy =  df_canopy.GUAN)
plot.canopy.pairs.HARV  <- summarize.canopy.pairs.plot(df_canopy =  df_canopy.HARV)
## ======================
canopy.panel <- ggarrange(plot.canopy.pairs.JORN,plot.canopy.pairs.KONZ, plot.canopy.pairs.GUAN, plot.canopy.pairs.HARV ,
                          ncol=4)


#Number of sampling level pairs :
mean.counts.co2 <- SITES_One2One_canopy %>% filter(gas == "CO2", CCC >= 0.5, Site %in% aoi) %>% mutate( count.sh = 1) %>% reframe(.by= c(Site), Mean.count.co2 = sum(count.sh)) 

mean.counts.h20 <- SITES_One2One_canopy %>% filter(gas == "H2O", CCC >= 0.5, Site %in% aoi) %>% mutate( count.sh = 1) %>% reframe(.by= c(Site), Mean.count.h20 = sum(count.sh)) 

# Season Sampling Panel:  #####

data.dir <- '/Volumes/MaloneLab/Research/FluxGradient' # Where do you want to save the plots
load(paste(data.dir,"/flow.flux.counts.R", sep=""))

SITE_DATA_FLUX_COUNTS_SEASON_Total.aoi <- SITE_DATA_FLUX_COUNTS_SEASON_Total %>% filter(Site %in% aoi)
SITE_DATA_FLUX_COUNTS_HOUR_Season.aoi  <-  SITE_DATA_FLUX_COUNTS_HOUR_Season %>% filter(Site %in% aoi) 

plot.counts.season <- SITE_DATA_FLUX_COUNTS_SEASON_Total.aoi %>% distinct %>%  ggplot(aes(x = percent, y = fct_relevel(Site,  "HARV","GUAN" ,"KONZ","JORN" ) , fill = season)) +
  geom_col() + facet_wrap(~ gas) +
  theme_bw() +  scale_fill_discrete_qualitative(palette = "Harmonic") + 
  theme(legend.position = "none",
        strip.background = element_rect(fill = "transparent", size = 0.5)) + labs(fill='Season') + xlab('Percent Coverage (%)') + ylab('')

plot.counts.season.hour.percent.h2o <- SITE_DATA_FLUX_COUNTS_HOUR_Season.aoi  %>% filter(gas == "H2O") %>%
  reframe( .by= c(hour,season), percent.mean = mean(percent)) %>% 
  ggplot(aes( x = hour, y = percent.mean, col=season)) +
  geom_point( ) + geom_line(aes( x = hour, y = percent.mean, group=season)) + 
  geom_hline(yintercept = 4) +  scale_colour_discrete_qualitative(palette = "Harmonic") +
  theme_bw() + theme(legend.position = "none")+ ylab('Percent Coverage (%)') + xlab('Hour')

plot.counts.season.hour.percent.co2 <- SITE_DATA_FLUX_COUNTS_HOUR_Season.aoi  %>% filter(gas == "CO2") %>%
  reframe( .by= c(hour,season), percent.mean = mean(percent)) %>% 
  ggplot(aes( x = hour, y = percent.mean, col=season)) +
  geom_point( ) + geom_line(aes( x = hour, y = percent.mean, group=season)) + 
  geom_hline(yintercept = 4) +  scale_colour_discrete_qualitative(palette = "Harmonic") +
  theme_bw() + theme(legend.position = "none") + ylab('Percent Coverage (%)')+ xlab('Hour')

 
season.plots <- ggarrange( plot.counts.season, plot.counts.season.hour.percent.co2, 
                           plot.counts.season.hour.percent.h2o, ncol=3, common.legend = T)

# Hour Panel:  #####

red_yellow_red_palette_func <- colorRampPalette(c("#FF0000", "#FFFF00", "#FF0000"))

plot.counts.season.hour.co2.aoi <- SITE_DATA_FLUX_COUNTS_HOUR_Season.aoi  %>% filter(gas == "CO2") %>% 
  ggplot(aes(x = percent, y = fct_relevel(Site,  "HARV","GUAN" ,"KONZ","JORN" ), fill=hour)) +
  geom_col() + facet_wrap(~season, ncol=4) +
  theme_bw() + scale_fill_manual(values=sunset)+ labs(fill='Hour') + 
  theme(legend.position = 'top') + xlab('') + ylab("")+
  guides(fill = guide_legend(nrow = 3)) + 
  theme( legend.position = 'none',
    legend.text = element_text(size = 6),
    strip.background = element_rect(fill = "transparent", size = 0.5),
         legend.title = element_text(size = 11, face = "bold"))

plot.counts.season.hour.h2o.aoi <- SITE_DATA_FLUX_COUNTS_HOUR_Season.aoi  %>% filter(gas == "H2O") %>% 
  ggplot(aes(x = percent, y = fct_relevel(Site,  "HARV","GUAN" ,"KONZ","JORN" ), fill=hour)) +
  geom_col() + facet_wrap(~season, ncol=4) +
  theme_bw() + scale_fill_manual(values=sunset)+ labs(fill='Hour') + 
  theme(legend.position = 'top',strip.background = element_rect(fill = "transparent", size = 0.5)) + xlab('Percent Coverage (%)') + ylab("")+
  guides(fill = guide_legend(nrow = 3)) + 
  theme( legend.position = 'none')

hour.plots <- ggarrange( plot.counts.season.hour.co2.aoi , plot.counts.season.hour.h2o.aoi, ncol=1)



# Temporal Overlap : ####
SITE_DATA_FLUX_COUNTS_Overlap_Long.aoi <-  SITE_DATA_FLUX_COUNTS_Overlap_Long %>% filter( Site %in% aoi)

plot.overlap.1.aoi <- SITE_DATA_FLUX_COUNTS_Overlap_Long.aoi %>% 
  ggplot(aes(x = Percent, y = fct_relevel(Site,  "HARV","GUAN" ,"KONZ","JORN" ), fill=Overlap)) +
  geom_col() + facet_wrap(~ gas, ncol=2) +
  theme_bw() +  scale_fill_discrete_sequential(palette = "Mint", nmax = 4, order = c(1, 2, 3, 4)) + labs(fill='') + 
  theme(legend.position = 'top') + xlab('Percent Overlap (%)') + 
  guides(fill = guide_legend(nrow = 1)) + 
  theme( legend.text = element_text(size = 6),
         legend.title = element_text(size = 11, face = "bold"),
         strip.background = element_rect(fill = "transparent", size = 0.5)) + 
  xlab('Temporal Overlap (%)') + ylab('')


# Harmonized Linear Relationship: ####

Harmonized.data <- fs::path(localdir,paste0("SITE_DATA_Harmonized.Rdata"))
load(file=Harmonized.data)

linear.aoi.co2 <- ggarrange(  Linear_Harmonized_plots_CO2$JORN,
            Linear_Harmonized_plots_CO2$KONZ,
            Linear_Harmonized_plots_CO2$GUAN,
            Linear_Harmonized_plots_CO2$HARV, ncol=4)

linear.aoi.h2o <- ggarrange(Linear_Harmonized_plots_H2O$JORN,
          Linear_Harmonized_plots_H2O$KONZ,
          Linear_Harmonized_plots_H2O$GUAN,
          Linear_Harmonized_plots_H2O$HARV, ncol=4)

linear.aoi <- ggarrange(linear.aoi.co2 , linear.aoi.h2o , ncol=1)

# DIEL Plots:####

load(file='/Volumes/MaloneLab/Research/FluxGradient/DIEL_SUMMARY_Harmonized.RDATA')


plot.diel.gas.season.regression.aoi <-Harmonized_DIELS  %>% filter(site %in% aoi) %>%   ggplot(aes(x = FG , y = EC, col=season))+
  geom_point() +
  geom_smooth(method = "lm", se = FALSE, size=0.5, col='black') +
  stat_regline_equation(aes(label = paste(..rr.label.., sep = "~`,`~")), col='black') +
  scale_colour_discrete_qualitative(palette = "Harmonic") +
  theme_bw() + 
  facet_wrap(~ gas+ season, , scales = "free", ncol=4) + 
  theme(legend.position = "none", 
        strip.background = element_rect(fill = "transparent", size = 0.5),
        legend.title = element_blank()) + ylab(expression(paste( "EC (g m"^2, ")")))  + xlab(expression(paste( "GF (g m"^2, ")"))) 

diel.plot <- function(DF, Site, label, Gas ){

  plot.diels <- DF %>% filter(season == 'Summer', gas==Gas, site == Site) %>% ggplot()+ 
    stat_smooth(aes(x = Hour , y = FG), col="black") + 
    stat_smooth(aes(x = Hour , y = EC), col="black", linetype="dashed") +
    geom_ribbon(aes(x=Hour, ymin = FG, ymax = EC), fill = "red", alpha = 0.2) +
    theme_bw() + ggtitle(Site)+ylab('Flux') 
  
  return(plot.diels )
}

plot.diels.co2.JORN <-diel.plot(DF =  Harmonized_DIELS, Site = 'JORN', 
          label= expression(paste( "CO"[2]," (g m"^2, ")")) , Gas = "CO2" )

plot.diels.co2.KONZ <-diel.plot(DF =  Harmonized_DIELS, Site = 'KONZ', 
                                label= expression(paste( "CO"[2]," (g m"^2, ")")) , Gas = "CO2" )
plot.diels.co2.GUAN <-diel.plot(DF =  Harmonized_DIELS, Site = 'GUAN', 
                                label= expression(paste( "CO"[2]," (g m"^2, ")")) , Gas = "CO2" )
plot.diels.co2.HARV <-diel.plot(DF =  Harmonized_DIELS, Site = 'HARV', 
                                label= expression(paste( "CO"[2]," (g m"^2, ")")) , Gas = "CO2" )

plot.diels.h2o.JORN <-diel.plot(DF =  Harmonized_DIELS, Site = 'JORN', 
                                label= expression(paste( "CO"[2]," (g m"^2, ")")) , Gas = "H2O" )

plot.diels.h2o.KONZ <-diel.plot(DF =  Harmonized_DIELS, Site = 'KONZ', 
                                label= expression(paste( "CO"[2]," (g m"^2, ")")) , Gas = "H2O" )
plot.diels.h2o.GUAN <-diel.plot(DF =  Harmonized_DIELS, Site = 'GUAN', 
                                label= expression(paste( "CO"[2]," (g m"^2, ")")) , Gas = "H2O" )
plot.diels.h2o.HARV <-diel.plot(DF =  Harmonized_DIELS, Site = 'HARV', 
                                label= expression(paste( "CO"[2]," (g m"^2, ")")) , Gas = "H2O" )


diel.plots.aoi <- ggarrange( plot.diels.co2.JORN,
           plot.diels.co2.KONZ,
           plot.diels.co2.GUAN,
           plot.diels.co2.HARV,
           plot.diels.h2o.JORN,
           plot.diels.h2o.KONZ,
           plot.diels.h2o.GUAN,
           plot.diels.h2o.HARV, ncol=4, nrow= 2)


# Compile the filgure: ####

ggarrange( method.panel ,
           canopy.panel,
           season.plots,
           ncol=1)
hour.plots 
plot.overlap.1.aoi
ggarrange(linear.aoi,
           ncol=1)

ggarrange( plot.diel.gas.season.regression.aoi,
           diel.plots.aoi ,
           ncol=1)

