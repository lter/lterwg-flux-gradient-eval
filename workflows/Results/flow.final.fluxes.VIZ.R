# Final diel:
rm(list=ls())

# The Diel analysis is currently set up by CCC threshold:
library(ggpubr)
library(ggplot2)
library(tidyverse)
library(gtools)

DirRepo <-"/Users/sm3466/YSE Dropbox/Sparkle Malone/Research/FluxGradient/lterwg-flux-gradient-eval"
localdir <- '/Volumes/MaloneLab/Research/FluxGradient/FluxData'
source(fs::path(DirRepo,'functions/calc.diel.R' ))
load(fs::path(localdir,paste0("SITES_One2One.Rdata"))) # Import CCC results
metadata <- read.csv('/Volumes/MaloneLab/Research/FluxGradient/Ameriflux_NEON field-sites.csv') 
site.list <- metadata$Site_Id.NEON %>% unique

# Calculate Diels by site: ####
SITE_DIEL_FINAL <- data.frame() # Save all the data here:
for( site in site.list){
  
  print(paste("Working on " ,site))
  
  message( paste("Importing the data for ", site))
  localdir.site <- paste(localdir,"/", site, sep = "")
  
  load(paste(localdir.site, "/", site, "_FILTER.Rdata", sep=""))
  
  
  canopy.sub <- canopy %>% filter( Site == site) %>% select(Site, Canopy_L1, dLevelsAminusB)
  
  SITES_One2One_sub <- SITES_One2One %>% 
    mutate(Approach = factor(Approach, levels = c("MBR", "AE", "WP") ),
           Good.CCC = case_when( gas == 'CO2' & CCC >= 0.5 & Approach == "MBR" ~ 1,
                                 gas == 'CO2' &CCC >= 0.75 & Approach == "WP" ~ 1,
                                 gas == 'CO2' &CCC >= 0.7 & Approach == "AE" ~ 1,
                                 gas == 'CO2' &CCC < 0.5 & Approach == "MBR" ~ 0,
                                 gas == 'CO2' & CCC < 0.75 & Approach == "WP" ~ 0,
                                 gas == 'CO2' &CCC < 0.7 & Approach == "AE" ~ 0,
                                 
                                 gas == 'H2O' & CCC < 0.5  ~ 0,
                                 gas == 'H2O' & CCC > 0.5  ~ 1) %>% as.factor) %>% 
    select( Site, Good.CCC, dLevelsAminusB, Approach) %>% filter(Site == site) %>% left_join(canopy.sub , by= c('Site', 'dLevelsAminusB'))  %>% filter(Good.CCC == 1 , Canopy_L1 != "WW")
  
  
  
  MBR_9min_FILTER_CCC <- SITES_One2One_sub %>% filter( Approach == "MBR") %>% full_join( MBR_9min_FILTER , by = c('dLevelsAminusB'))  %>% filter(Good.CCC == 1 , Canopy_L1 != "WW") %>% 
    select( timeEndA.local,FG_mean ,Good.CCC , Approach, Canopy_L1, gas, TowerPosition_A, TowerPosition_B,  FC_turb_interp)
  
  WP_9min_FILTER_CCC <- SITES_One2One_sub %>% filter( Approach == "WP") %>% full_join( WP_9min_FILTER , by = c('dLevelsAminusB'))  %>% filter(Good.CCC == 1 , Canopy_L1 != "WW") %>% 
    select( timeEndA.local,FG_mean ,Good.CCC , Approach, Canopy_L1, gas, TowerPosition_A, TowerPosition_B, FC_turb_interp)
  AE_9min_FILTER_CCC <- SITES_One2One_sub %>% filter( Approach == "AE") %>% full_join( AE_9min_FILTER , by = c('dLevelsAminusB'))  %>% filter(Good.CCC == 1 , Canopy_L1 != "WW") %>% 
    select( timeEndA.local,FG_mean ,Good.CCC , Approach, Canopy_L1, gas, TowerPosition_A, TowerPosition_B, FC_turb_interp)
  
  
  Data <- rbind(MBR_9min_FILTER_CCC,  AE_9min_FILTER_CCC, WP_9min_FILTER_CCC) %>% as.data.frame() %>% filter( Good.CCC == 1)
  
  # Use the original functions to have GF and EC...
  
  DIEL.CO2 = try( DIEL.COMPILE.FINAL( dataframe = Data, 
                                      FG_flux = 'FG_mean', 
                                      EC_flux = 'FC_turb_interp',
                                      Gas = "CO2") %>% mutate( gas="CO2", Site = site), silent = TRUE)
  
  DIEL.H2O =   try( DIEL.COMPILE.FINAL( dataframe = Data, 
                                        FG_flux = 'FG_mean', 
                                        EC_flux = 'FC_turb_interp',
                                        Gas = "H2O") %>% mutate( gas="H2O", Site = site), silent = TRUE)
  
  
  if (is.data.frame(DIEL.CO2)){
    
    DIEL.DATA <- rbind( DIEL.CO2, DIEL.H2O )
    
    SITE_DIEL_FINAL <- rbind(SITE_DIEL_FINAL, DIEL.DATA)
    
    rm( DIEL.CO2, DIEL.H2O,  DIEL.DATA)
    
  }
  
}

SITE_DIEL_FINAL_Daily <- SITE_DIEL_FINAL %>% reframe(.by = c(Year, Site, gas), 
                                                     FG = sum(FG),
                                                     FG.SE = sum(FG.SE),
                                                     FG.count = mean(count.FG),
                                                     EC = sum(EC),
                                                     EC.SE = sum(EC.SE) ) %>% mutate(
                                                       FG.gC = case_when(gas == "CO2" ~ (FG/1000000)*44.01*1800,
                                                                         gas == "H2O" ~ (FG/1000000)*18.05*1800),
                                                       FG.SE.gC = case_when(gas == "CO2" ~ (FG.SE/1000000)*44.01*1800,
                                                                            gas == "H2O" ~ (FG.SE/1000000)*18.05*1800),
                                                       EC.gC = case_when(gas == "CO2" ~ (EC/1000000)*44.01*1800,
                                                                         gas == "H2O" ~ (EC/1000000)*18.05*1800),
                                                       EC.SE.gC = case_when(gas == "CO2" ~ (EC.SE/1000000)*44.01*1800,
                                                                            gas == "H2O" ~ (EC.SE/1000000)*18.05*1800))


# Canopy Information #####
canopy <- read.csv(file.path(paste(localdir, "canopy_commbined.csv", sep="/"))) %>% distinct
canopy %>% names
canopy.summary <- canopy %>% reframe( .by=Site, canopyHeight_m = mean(canopyHeight_m), EVI.mean= mean(EVI.mean),EVI.sd= mean(EVI.sd), NDVI.mean= mean(NDVI.mean), NDVI.sd= mean(NDVI.sd), LAI.mean= mean(LAI.sd), LAI.sd= mean(LAI.sd), CHM.sd = mean(CHM.sd), SDSDH.mean = mean(Cutoff05.SDSDH), TopRugosity= mean(Cutoff05.TopRugosity))

# Flux Rates:
SITE_DIEL_FINAL_Daily_C <- SITE_DIEL_FINAL_Daily %>% full_join(canopy.summary, by="Site") %>% mutate(DIFF = FG.gC- EC.gC)

# Reduce flux file to match EC data availability for a coparison:

SITE_DIEL_FINAL_Daily_C_same <- SITE_DIEL_FINAL_Daily_C %>% filter(!is.na(FG.gC) & !is.na(EC.gC)) 

canopy.site <- data.frame( Site = canopy$Site %>% unique())

uptake.days <- SITE_DIEL_FINAL_Daily_C  %>% filter(!is.na(FG.gC) & !is.na(EC.gC)) %>%  
  filter(EC.gC < 0, gas== "CO2")  %>% full_join(canopy.site ) %>%  mutate(Site = fct_explicit_na(Site, na_level = "(Missing)"))

respiration.days <- SITE_DIEL_FINAL_Daily_C %>% filter(!is.na(FG.gC) & !is.na(EC.gC)) %>% filter(EC.gC >= 0, gas== "CO2")  %>% full_join(canopy.site ) %>% mutate(Site = fct_explicit_na(Site, na_level = "(Missing)"))

plot.uptake <- uptake.days %>% ggplot( ) +  geom_boxplot( aes(y = Site, x= EC.gC), col="grey50", fill="grey80") +
  geom_boxplot( aes(y = Site, x= FG.gC, col= canopyHeight_m), fill='transparent') +
  xlab( expression(paste('Flux (g CO'[2],' m'^-2,  ' day'^-1,')' ))) + ylab ('') + scale_color_gradient2(midpoint=10, low="goldenrod", mid="darkgreen", high="springgreen", name="Canopy Height (m)" ) +  theme_bw() 

plot.resp <-respiration.days  %>% ggplot( ) +  geom_boxplot( aes(y = Site, x= EC.gC), col="grey50", fill="grey80") +
  geom_boxplot( aes(y = Site, x= FG.gC, col= canopyHeight_m), fill='transparent') +
  xlab( expression(paste('Flux (g CO'[2],' m'^-2,  ' day'^-1,')' ))) + ylab ('') + scale_color_gradient2(midpoint=10, low="goldenrod", mid="darkgreen", high="springgreen", name="Canopy Height (m)" ) +  theme_bw() 



final.flux.plot.co2 <- SITE_DIEL_FINAL_Daily_C_same %>% filter(!is.na(FG.gC) & !is.na(EC.gC)) %>% filter(gas== "CO2") %>% ggplot( ) +  geom_boxplot( aes(y = Site, x= EC.gC), col="grey50", fill="grey80") +
  geom_boxplot( aes(y = Site, x= FG.gC, col= canopyHeight_m), fill='transparent') +
  xlab( expression(paste('Flux (g CO'[2],' m'^-2,  ' day'^-1,')' ))) + ylab ('') + scale_color_gradient2(midpoint=10, low="goldenrod", mid="darkgreen", high="springgreen", name="Canopy Height (m)" ) +  theme_bw() + xlim(-25, 60)

final.flux.plot.h2o <- SITE_DIEL_FINAL_Daily_C_same %>% filter(!is.na(FG.gC) & !is.na(EC.gC)) %>% filter(gas== "H2O") %>% ggplot( ) +  geom_boxplot( aes(y = Site, x= EC.gC), col="grey50", fill="grey80")+
  geom_boxplot( aes(y = Site, x= FG.gC ,col= canopyHeight_m), fill = 'transparent') + 
  theme_bw() + xlab( expression(paste('Flux (g H'[2],'O  m'^-2,  ' day'^-1,')' ))) + ylab ('')+ scale_color_gradient2(midpoint=10, low="goldenrod", mid="darkgreen", high="springgreen", name="Canopy Height (m)" ) + xlim(-20, 50)

final.flux.plot.co2.h2o <- ggarrange(final.flux.plot.co2,final.flux.plot.h2o, labels=c("A", "B"), 
                                 common.legend = TRUE, ncol=2)

ggsave("Figures/Flux_plot_CO2.png", plot = final.flux.plot.co2.h2o, width = 5, height = 6, units = "in")

save( SITE_DIEL_FINAL, SITE_DIEL_FINAL_Daily_C,  SITE_DIEL_FINAL_Daily_C_same,
      file= "/Volumes/MaloneLab/Research/FluxGradient/SITE_DIEL_FINAL.RDATA")

# Results:
SITE_DIEL_FINAL_Daily_C_same %>% 
  filter(FG.gC < 80) %>%  reframe(.by="gas", 
                                  FG.min = min(FG.gC),
                                  FG.max = max(FG.gC),
                                  FG.mean = mean(FG.gC), 
                                  EC.min = min(EC.gC),
                                  EC.max = max(EC.gC),
                                  EC.mean = mean(EC.gC)) 
                                                                
# Non-linear Model for Diff corrections:
nls_model.co2 <- nls(DIFF ~ a * exp(b * canopyHeight_m) + exp(d * SDSDH.mean) * TopRugosity, 
                            data = SITE_DIEL_FINAL_Daily_C_same %>% filter( gas == "CO2", EC.gC < 0),
                            start = list(a = 1, b = 0.1, d = 0.19))

SITE_DIEL_FINAL_Daily_C_same_gs <- SITE_DIEL_FINAL_Daily_C_same %>% filter( EC.gC <= 0)

SITE_DIEL_FINAL_Daily_C_same_gs$correction.co2 <- predict( nls_model.co2 , SITE_DIEL_FINAL_Daily_C_same_gs)


diff.plot.ch.co2.gs <- SITE_DIEL_FINAL_Daily_C_same_gs %>% filter( gas == "CO2") %>% ggplot(aes(x = canopyHeight_m, y= DIFF)) + geom_point(col="grey80", size = 1) + geom_smooth(aes(x= canopyHeight_m, y = correction.co2 ), col="black") + ylab("Difference") + xlab("Canopy Height (m)") + theme_bw() 


# H2O

nls_model.h2o <- nls(DIFF ~ a * exp(b * canopyHeight_m) + exp(d * SDSDH.mean) * TopRugosity, 
                     data = SITE_DIEL_FINAL_Daily_C_same %>% filter( gas == "H2O", EC.gC < 0),
                     start = list(a = 1, b = 0.1, d = 0.19))

nls_model.co2 %>% summary
nls_model.h2o%>% summary

SITE_DIEL_FINAL_Daily_C_same_gs$correction.h2o <- predict( nls_model.h2o , SITE_DIEL_FINAL_Daily_C_same_gs)

diff.plot.ch.h2o.gs <- SITE_DIEL_FINAL_Daily_C_same_gs %>% filter( gas == "H2O") %>% ggplot(aes(x = canopyHeight_m, y= DIFF)) + geom_point(col="grey80", size=1) + geom_smooth(aes(x= canopyHeight_m, y = correction.h2o ), col="black") + ylab("Difference") + xlab("Canopy Height (m)") + theme_bw() 


final.correction <- ggarrange( diff.plot.ch.co2.gs,diff.plot.ch.h2o.gs,
                               ncol=2, nrow=1, labels = c("A", "B"))

ggsave("Figures/Correction_CO2.png", plot = final.correction, width = 4, height = 2.5, units = "in")


# Adjust daily fluxes ...

SITE_DIEL_FINAL_Daily_C_adj <- SITE_DIEL_FINAL_Daily_C_same %>% mutate(
  adj.ch  = case_when( gas == "H2O" & EC.gC <=0  ~ 1.19 * exp(0.07 * canopyHeight_m) + exp(-1.63 * SDSDH.mean* TopRugosity),
                       gas == "H2O" & EC.gC > 0  ~ 0,
                       gas == "CO2"& EC.gC <=0 ~ 3.41 * exp(0.06 * canopyHeight_m) + exp(-1.51 * SDSDH.mean* TopRugosity),
                       gas == "CO2" & EC.gC > 0  ~ 0),
  
  FG.gC.adj.ch = FG.gC - adj.ch,
  DIFF.adj = FG.gC.adj.ch - EC.gC) 

SITE_DIEL_FINAL_Daily_C_adj$FG.gC.adj.ch

SITE_DIEL_FINAL_Daily_C_adj %>% filter(gas== "CO2") %>% ggplot( ) +  geom_boxplot( aes(y = Site, x= EC.gC), col="grey50", fill="grey80") +
  geom_boxplot( aes(y = Site, x= FG.gC.adj.ch, col= canopyHeight_m), fill='transparent') +
  xlab( expression(paste('Flux (g CO'[2],' m'^-2,  ' day'^-1,')' ))) + ylab ('') + scale_color_gradient2(midpoint=10, low="goldenrod", mid="darkgreen", high="springgreen", name="Canopy Height (m)" ) +  theme_bw() + xlim(-25, 40)

SITE_DIEL_FINAL_Daily_C_adj %>% filter(!is.na(FG.gC) & !is.na(EC.gC)) %>% filter(gas== "H2O") %>% ggplot( ) +  geom_boxplot( aes(y = Site, x= EC.gC), col="grey50", fill="grey80")+
  geom_boxplot( aes(y = Site, x= FG.gC.adj.ch ,col= canopyHeight_m), fill = 'transparent') + 
  theme_bw() + xlab( expression(paste('Flux (g H'[2],'O  m'^-2,  ' day'^-1,')' ))) + ylab ('')+ scale_color_gradient2(midpoint=10, low="goldenrod", mid="darkgreen", high="springgreen", name="Canopy Height (m)" ) + xlim(-15, 15)


SITE_DIEL_FINAL_Daily_C_adj %>% filter( gas== "CO2") %>% select(DIFF) %>% summary
SITE_DIEL_FINAL_Daily_C_adj %>% filter( gas== "H2O") %>% select(DIFF) %>% summary

SITE_DIEL_FINAL_Daily_C_adj %>% filter( gas== "CO2") %>% select(DIFF.adj) %>% summary
SITE_DIEL_FINAL_Daily_C_adj %>% filter( gas== "H2O") %>% select(DIFF.adj) %>% summary

SITE_DIEL_FINAL_Daily_C_adj %>% filter(gas == "CO2") %>% ggplot( aes(x = EC.gC,  y= FG.gC.adj.ch)) + geom_point( col="blue4", size = 1, alpha=0.1) + geom_smooth (method = "lm", col="black") + ylim(-100, 100) + xlim(-30, 30) + geom_abline(intercept = 0, slope = 1, col="red") + ylab("Gradient Flux") + xlab("Eddy Covariance") + theme_bw()

metadata <- read.csv('/Volumes/MaloneLab/Research/FluxGradient/Ameriflux_NEON field-sites.csv') %>% mutate( Site = Site_Id.NEON, IGBP = Vegetation.Abbreviation..IGBP., Koppen = Climate.Class.Abbreviation..Koeppen.) %>%  select( Site, IGBP, Koppen)

SITE_DIEL_FINAL_Daily_C_adj_attr <- SITE_DIEL_FINAL_Daily_C_adj %>% full_join(metadata, by = "Site")



library(ggplot2)
library(ggpmisc)

plot.IGBP.fluxes <- SITE_DIEL_FINAL_Daily_C_adj_attr %>% filter(FG.gC <50) %>% ggplot( aes(x = EC.gC,  y= FG.gC.adj.ch,col=canopyHeight_m)) + geom_point(size = 1, alpha=0.5) +
  stat_poly_line() +
  stat_poly_eq() + geom_abline(intercept = 0, slope = 1, col="grey") + ylab(expression(paste(' Gradiet Flux (g m'^-2,  ' day'^-1,')' ))) + xlab(expression(paste('Eddy Covariance (g  m'^-2,  ' day'^-1,')' ))) + theme_bw()  + scale_color_gradient2(midpoint=10, low="goldenrod", mid="darkgreen", high="springgreen", name="Canopy Height (m)" )  + facet_wrap(~IGBP) +theme(
    strip.text.x = element_text(size = 10, color = "white"),
    strip.background = element_rect(fill = "black"))

ggsave("Figures/IGBP.CO2.H2O.FINAL.png", plot = plot.IGBP.fluxes, width = 7, height = 5, units = "in")


boxplot.co2.gs <- SITE_DIEL_FINAL_Daily_C_adj_attr %>% filter (gas == "CO2", FG.gC.adj.ch < 0)  %>% ggplot( aes(y = IGBP,  x= FG.gC.adj.ch)) + geom_boxplot( outlier.shape = NA)  + theme_bw() + xlab(expression(paste(' Productivity-Dominant (g CO'[2], 'm'^-2,  ' day'^-1,')' )))


boxplot.co2.ngs <-SITE_DIEL_FINAL_Daily_C_adj_attr %>% filter(gas == "CO2", FG.gC.adj.ch > 0 ) %>% ggplot( aes(y = IGBP,  x= FG.gC.adj.ch)) + geom_boxplot( outlier.shape = NA) + theme_bw() + xlim(0, 10) + xlab(expression(paste(' Respiration-Dominant (g CO'[2], 'm'^-2,  ' day'^-1,')' )))

boxplot.h2o.gs <- SITE_DIEL_FINAL_Daily_C_adj_attr %>% filter (gas == "H2O", FG.gC.adj.ch < 0)  %>% ggplot( aes(y = IGBP,  x= FG.gC.adj.ch)) + geom_boxplot( outlier.shape = NA)  + theme_bw() + xlab(expression(paste(' Productivity-Dominant (g H'[2], 'O m'^-2,  ' day'^-1,')' )))


boxplot.h2o.ngs <-SITE_DIEL_FINAL_Daily_C_adj_attr %>% filter(gas == "H2O", FG.gC.adj.ch > 0 ) %>% ggplot( aes(y = IGBP,  x= FG.gC.adj.ch)) + geom_boxplot( outlier.shape = NA) + theme_bw() + xlab(expression(paste(' Respiration-Dominant (g H'[2], 'O m'^-2,  ' day'^-1,')' )))+ xlim(-2, 2)



final.boxplot <- ggarrange( boxplot.co2.gs , boxplot.co2.ngs ,
                            boxplot.h2o.gs , boxplot.h2o.ngs ,
                               ncol=2, nrow=2, labels = c("A", "B", "C", "D"))

ggsave("Figures/Box_plot.png", plot = final.boxplot, width = 6, height = 5, units = "in")

