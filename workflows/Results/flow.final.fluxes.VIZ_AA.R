# Final diel:
rm(list=ls())

# The Diel analysis is currently set up by CCC threshold:
library(ggpubr)
library(ggplot2)
library(tidyverse)
library(gtools)
library(ggplot2)
library(ggpmisc)

DirRepo <-"/Users/sm3466/YSE Dropbox/Sparkle Malone/Research/FluxGradient/lterwg-flux-gradient-eval"
localdir <- '/Volumes/MaloneLab/Research/FluxGradient/FluxData'
source(fs::path(DirRepo,'functions/calc.diel.R' ))
load(fs::path(localdir,paste0("SITES_One2One.Rdata"))) # Import CCC results
metadata <- read.csv('/Volumes/MaloneLab/Research/FluxGradient/Ameriflux_NEON field-sites.csv') 
site.list <- metadata$Site_Id.NEON %>% unique
canopy <- read.csv(file.path(paste(localdir, "canopy_commbined.csv", sep="/"))) %>% distinct

# Data Prep:
Highest.CCC <- SITES_One2One %>% reframe(.by= c(Site, gas, Approach), CCC.max = max(CCC, na.rm=T))

SITES_One2One_canopy <- SITES_One2One %>% full_join(canopy, by=c("Site", "dLevelsAminusB" ) ) %>% 
  full_join( Highest.CCC,by = c("Site", "gas", "Approach")) %>% mutate(Approach = factor(Approach, levels = c("MBR", "AE", "WP") ),
                                                                       Good.CCC = case_when( gas == 'CO2' & CCC >= 0.5 & Approach == "MBR" ~ 1,
                                                                                             gas == 'CO2' &CCC >= 0.75 & Approach == "WP" ~ 1,
                                                                                             gas == 'CO2' &CCC >= 0.7 & Approach == "AE" ~ 1,
                                                                                             gas == 'CO2' &CCC < 0.5 & Approach == "MBR" ~ 0,
                                                                                             gas == 'CO2' & CCC < 0.75 & Approach == "WP" ~ 0,
                                                                                             gas == 'CO2' &CCC < 0.7 & Approach == "AE" ~ 0,
                                                                                             
                                                                                             gas == 'H2O' & CCC < 0.5  ~ 0,
                                                                                             gas == 'H2O' & CCC > 0.5  ~ 1) %>% as.factor,
                                                                       RelativeDistB = MeasurementHeight_m_B - CanopyHeight, 
                                                                       RelativeDistA = MeasurementHeight_m_A - CanopyHeight, 
                                                                       MeasurementDist = MeasurementHeight_m_A - MeasurementHeight_m_A)

# Calculate Diels by site: ####
SITE_DIEL_FINAL <- data.frame() # Save all the data here:

for( site in site.list){
  
  print(paste("Working on " ,site))
  
  message( paste("Importing the data for ", site))
  
  localdir.site <- paste(localdir,"/", site, sep = "")
  load(paste(localdir.site, "/", site, "_FILTER.Rdata", sep=""))
  
  canopy.sub <- canopy %>% filter( Site == site) %>% select(Site, Canopy_L1, dLevelsAminusB)
  
  
  SITES_One2One_sub <- SITES_One2One_canopy %>% 
    select( Site, Good.CCC, dLevelsAminusB, Approach) %>% filter(Site == site) %>% left_join(canopy.sub , by= c('Site', 'dLevelsAminusB'))  %>% filter(Good.CCC == 1)
  
  MBR_9min_FILTER_CCC <- SITES_One2One_sub %>% filter( Approach == "MBR") %>% full_join( MBR_9min_FILTER , by = c('dLevelsAminusB'))  %>% filter(Good.CCC == 1 , cross_grad_flag == 0, Canopy_L1 != "WW") %>% 
    select( timeEndA.local,FG_mean ,Good.CCC , Approach, Canopy_L1, gas, TowerPosition_A, TowerPosition_B,  FC_turb_interp, cross_grad_flag)
  
  WP_9min_FILTER_CCC <- SITES_One2One_sub %>% filter( Approach == "WP") %>% full_join( WP_9min_FILTER , by = c('dLevelsAminusB'))  %>% filter(Good.CCC == 1 ,  cross_grad_flag == 0, Canopy_L1 != "WW") %>% 
    select( timeEndA.local,FG_mean ,Good.CCC , Approach, Canopy_L1, gas, TowerPosition_A, TowerPosition_B, FC_turb_interp, cross_grad_flag)
  
  AE_9min_FILTER_CCC <- SITES_One2One_sub %>% filter( Approach == "AE") %>% full_join( AE_9min_FILTER , by = c('dLevelsAminusB'))  %>% filter(Good.CCC == 1 ,  cross_grad_flag == 0 , Canopy_L1 != "WW") %>% 
    select( timeEndA.local,FG_mean ,Good.CCC , Approach, Canopy_L1, gas, TowerPosition_A, TowerPosition_B, FC_turb_interp, cross_grad_flag)
  
  
  Data <- rbind(MBR_9min_FILTER_CCC,  AE_9min_FILTER_CCC, WP_9min_FILTER_CCC) %>% as.data.frame() %>% filter( Good.CCC == 1, cross_grad_flag != 1)
  
  Data$Canopy_L1 %>% unique
  
  # Fit DIEL Curves
  
  DIEL.CO2.all = try( DIEL.COMPILE.FINAL( dataframe = Data, 
                                      FG_flux = 'FG_mean', 
                                      EC_flux = 'FC_turb_interp',
                                      Gas = "CO2") %>% mutate( gas="CO2", Site = site, Canopy_L1 = "AA + AW"), silent = TRUE)
  
  DIEL.H2O.all =   try( DIEL.COMPILE.FINAL( dataframe = Data, 
                                        FG_flux = 'FG_mean', 
                                        EC_flux = 'FC_turb_interp',
                                        Gas = "H2O") %>% mutate( gas="H2O", Site = site,
                                                                 Canopy_L1 = "AA + AW"), silent = TRUE)
  
  
  if (is.data.frame(DIEL.CO2.all)){
    
    DIEL.DATA.all <- rbind( DIEL.CO2.all, DIEL.H2O.all )
    SITE_DIEL_FINAL <- rbind(SITE_DIEL_FINAL, DIEL.DATA.all)
    rm( DIEL.CO2.all, DIEL.H2O.all,  DIEL.DATA.all)
    
  }

  
  if( Data %>% filter( Canopy_L1 == "AA") %>% length > 1 ){
    
    print("AA present")
    
    DIEL.CO2.aa = try( DIEL.COMPILE.FINAL( dataframe = Data %>% filter(Canopy_L1 == "AA"), 
                                           FG_flux = 'FG_mean', 
                                           EC_flux = 'FC_turb_interp',
                                           Gas = "CO2") %>% mutate( gas="CO2", Site = site, Canopy_L1 = "AA"), silent = TRUE)
    
    DIEL.H2O.aa =   try( DIEL.COMPILE.FINAL( dataframe = Data %>% filter(Canopy_L1 == "AA"), 
                                             FG_flux = 'FG_mean', 
                                             EC_flux = 'FC_turb_interp',
                                             Gas = "H2O") %>% mutate( gas="H2O", Site = site,
                                                                      Canopy_L1 = "AA"), silent = TRUE)
    
    if (is.data.frame(DIEL.CO2.aa)){
      
      DIEL.DATA.aa <- rbind( DIEL.CO2.aa, DIEL.H2O.aa )
      SITE_DIEL_FINAL <- rbind(SITE_DIEL_FINAL, DIEL.DATA.aa)
      rm( DIEL.CO2.aa, DIEL.H2O.aa,  DIEL.DATA.aa)
      
    }}
  
  if( Data %>% filter( Canopy_L1 == "AW") %>% length > 1 ){
    print('AW is present')
    
    DIEL.CO2.aw = try( DIEL.COMPILE.FINAL( dataframe = Data %>% filter(Canopy_L1 == "AW"), 
                                           FG_flux = 'FG_mean', 
                                           EC_flux = 'FC_turb_interp',
                                           Gas = "CO2") %>% mutate( gas="CO2", Site = site, Canopy_L1 = "AW"), silent = TRUE)
    
    DIEL.H2O.aw =   try( DIEL.COMPILE.FINAL( dataframe = Data %>% filter(Canopy_L1 == "AW"), 
                                             FG_flux = 'FG_mean', 
                                             EC_flux = 'FC_turb_interp',
                                             Gas = "H2O") %>% mutate( gas="H2O", Site = site,
                                                                      Canopy_L1 = "AW"), silent = TRUE)
    
    if (is.data.frame(DIEL.CO2.aw)){
      
      DIEL.DATA.aw <- rbind( DIEL.CO2.aw, DIEL.H2O.aw )
      SITE_DIEL_FINAL <- rbind(SITE_DIEL_FINAL, DIEL.DATA.aw)
      rm( DIEL.CO2.aw, DIEL.H2O.aw,  DIEL.DATA.aw)
      
    }
  }  
   
  }
  
# Unit Changes  and daily flux calculation
SITE_DIEL_FINAL_Daily <- SITE_DIEL_FINAL %>% reframe(.by = c(Year, Site, gas, Canopy_L1), 
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
canopy.summary <- canopy %>% reframe( .by=Site, canopyHeight_m = mean(canopyHeight_m), EVI.mean= mean(EVI.mean),EVI.sd= mean(EVI.sd), NDVI.mean= mean(NDVI.mean), NDVI.sd= mean(NDVI.sd), LAI.mean= mean(LAI.sd), LAI.sd= mean(LAI.sd), CHM.sd = mean(CHM.sd), SDH.mean = mean(Cutoff05.SDH), TopRugosity= mean(Cutoff05.TopRugosity),MeasB_RelCaonopy = mean(MeasurementHeight_m_B - canopyHeight_m),DistZaxsDisp = mean(DistZaxsDisp),DistZaxsGrndOfst = mean(DistZaxsGrndOfst), RelativeDist_B = MeasurementHeight_m_B - canopyHeight_m, RelativeDist_A = MeasurementHeight_m_A - canopyHeight_m)

# Flux Rates:

SITE_DIEL_FINAL_Daily_C <- SITE_DIEL_FINAL_Daily %>% full_join(canopy.summary, by="Site", relationship = "many-to-many") %>% mutate(DIFF = FG.gC- EC.gC)

# Reduce flux file to match EC data availability for a coparison:
SITE_DIEL_FINAL_Daily_C_same <- SITE_DIEL_FINAL_Daily_C %>% filter(!is.na(FG.gC) & !is.na(EC.gC)) 

save( SITE_DIEL_FINAL, SITE_DIEL_FINAL_Daily_C,  SITE_DIEL_FINAL_Daily_C_same,
      file= "/Volumes/MaloneLab/Research/FluxGradient/SITE_DIEL_FINAL.RDATA")

# Daily Fluxes : ####

load(file= "/Volumes/MaloneLab/Research/FluxGradient/SITE_DIEL_FINAL.RDATA")
canopy.site <- data.frame( Site = canopy$Site %>% unique()) # This ensures all sites will be plotted regardless of data availability

uptake.days <- SITE_DIEL_FINAL_Daily_C_same  %>% filter(!is.na(FG.gC) & !is.na(EC.gC)) %>%  
  filter(EC.gC < 0, gas== "CO2")  %>% full_join(canopy.site, relationship = "many-to-many") %>%  mutate(Site = fct_explicit_na(Site, na_level = "(Missing)"))

respiration.days <- SITE_DIEL_FINAL_Daily_C_same %>% filter(!is.na(FG.gC) & !is.na(EC.gC)) %>% filter(EC.gC >= 0, gas== "CO2")  %>% full_join(canopy.site, relationship = "many-to-many" ) %>% mutate(Site = fct_explicit_na(Site, na_level = "(Missing)"))

plot.uptake <- uptake.days %>% filter(Canopy_L1 == "AA + AW")  %>% ggplot( ) +  geom_boxplot( aes(y = Site, x= EC.gC), col="grey50", fill="grey80") +
  geom_boxplot( aes(y = Site, x= FG.gC, col= canopyHeight_m), fill='transparent') +
  xlab( expression(paste('Flux (g CO'[2],' m'^-2,  ' day'^-1,')' ))) + ylab ('') + scale_color_gradient2(midpoint=10, low="goldenrod", mid="darkgreen", high="springgreen", name="Canopy Height (m)" ) +  theme_bw() 

plot.resp <-respiration.days %>% filter(Canopy_L1 == "AA + AW") %>% ggplot( ) +  geom_boxplot( aes(y = Site, x= EC.gC), col="grey50", fill="grey80") +
  geom_boxplot( aes(y = Site, x= FG.gC, col= canopyHeight_m), fill='transparent') +
  xlab( expression(paste('Flux (g CO'[2],' m'^-2,  ' day'^-1,')' ))) + ylab ('') + scale_color_gradient2(midpoint=10, low="goldenrod", mid="darkgreen", high="springgreen", name="Canopy Height (m)" ) +  theme_bw() 

final.flux.plot.co2 <- SITE_DIEL_FINAL_Daily_C_same  %>% filter(gas== "CO2") %>% ggplot( ) +  geom_boxplot( aes(y = Site, x= EC.gC), col="grey50", fill="grey80") +
  geom_boxplot( aes(y = Site, x= FG.gC, col= canopyHeight_m), fill='transparent') +
  xlab( expression(paste('Flux (g CO'[2],' m'^-2,  ' day'^-1,')' ))) + ylab ('') + scale_color_gradient2(midpoint=10, low="goldenrod", mid="darkgreen", high="springgreen", name="Canopy Height (m)" ) +  theme_bw() + xlim(-25, 60)

final.flux.plot.h2o <- SITE_DIEL_FINAL_Daily_C_same %>% filter(gas== "H2O") %>% ggplot( ) +  geom_boxplot( aes(y = Site, x= EC.gC), col="grey50", fill="grey80")+
  geom_boxplot( aes(y = Site, x= FG.gC ,col= canopyHeight_m), fill = 'transparent') + 
  theme_bw() + xlab( expression(paste('Flux (g H'[2],'O  m'^-2,  ' day'^-1,')' ))) + ylab ('')+ scale_color_gradient2(midpoint=10, low="goldenrod", mid="darkgreen", high="springgreen", name="Canopy Height (m)" ) + xlim(-20, 50)

final.flux.plot.co2.h2o <- ggarrange(final.flux.plot.co2,final.flux.plot.h2o, labels=c("A", "B"), 
                                     common.legend = TRUE, ncol=2)

ggsave("Figures/Flux_plot_CO2_AA.png", plot = final.flux.plot.co2.h2o, width = 5, height = 6, units = "in")

# Results:
SITE_DIEL_FINAL_Daily_C_same %>% 
  filter( Canopy_L1 == "AA + AW",FG.gC <50 ) %>%  reframe(.by="gas", 
                                  FG.min = min(FG.gC),
                                  FG.max = max(FG.gC),
                                  FG.mean = mean(FG.gC), 
                                  EC.min = min(EC.gC),
                                  EC.max = max(EC.gC),
                                  EC.mean = mean(EC.gC),
                                  Mean.DIFF = mean(FG.gC- EC.gC) )

# Correction: ####

load(file= "/Volumes/MaloneLab/Research/FluxGradient/SITE_DIEL_FINAL.RDATA")
metadata <- read.csv('/Volumes/MaloneLab/Research/FluxGradient/Ameriflux_NEON field-sites.csv') %>% mutate( Site = Site_Id.NEON, IGBP = Vegetation.Abbreviation..IGBP., Koppen = Climate.Class.Abbreviation..Koeppen.) %>%  select( Site, IGBP, Koppen)

SITE_DIEL_FINAL_Daily_C_same_adj_attr <- SITE_DIEL_FINAL_Daily_C_same  %>% full_join(metadata, by = "Site", relationship = "many-to-many")

# Non-linear Model for Diff corrections:

nls_model.all <- nls(EC.gC ~ FG.gC + a * exp(b * canopyHeight_m) +  c*RelativeDist_B + d*LAI.sd , 
                 data = SITE_DIEL_FINAL_Daily_C_same_adj_attr %>% filter(Canopy_L1 == "AA + AW"),
                 start = list(a = 1, b = 0.1, c= 0.01, d= 0.01))

nls_model.aa <- nls(EC.gC ~ FG.gC + a * exp(b * canopyHeight_m) +  c*RelativeDist_B + d*LAI.sd , 
                     data = SITE_DIEL_FINAL_Daily_C_same_adj_attr %>% filter(Canopy_L1 == "AA"),
                     start = list(a = 1, b = 0.1, c= 0.01, d= 0.01))

nls_model.aw <- nls(EC.gC ~ FG.gC + a * exp(b * canopyHeight_m) +  c*RelativeDist_B + d*LAI.sd , 
                    data = SITE_DIEL_FINAL_Daily_C_same_adj_attr %>% filter(Canopy_L1 == "AW"),
                    start = list(a = 1, b = 0.1, c= 0.01, d= 0.01))

nls_model.all %>% summary
nls_model.aa %>% summary
nls_model.aw %>% summary


SITE_DIEL_FINAL_Daily_C_same_adj_attr$nls.correction.all <- predict( nls_model.all , SITE_DIEL_FINAL_Daily_C_same_adj_attr)

SITE_DIEL_FINAL_Daily_C_same_adj_attr$nls.correction.aa <- predict( nls_model.aa , SITE_DIEL_FINAL_Daily_C_same_adj_attr)

SITE_DIEL_FINAL_Daily_C_same_adj_attr$nls.correction.aw <- predict( nls_model.aw , SITE_DIEL_FINAL_Daily_C_same_adj_attr)

plot.IGBP.fluxes <-  ggplot() + 
  geom_abline(intercept = 0, slope = 1, col="grey") +
  geom_point(data= SITE_DIEL_FINAL_Daily_C_same_adj_attr %>% filter(Canopy_L1 == 'AA + AW') , aes(x = nls.correction.all, y= EC.gC), size = 1, alpha=0.015, col="red") + 
    stat_poly_line(data= SITE_DIEL_FINAL_Daily_C_same_adj_attr %>% filter(Canopy_L1 == 'AA + AW') , aes(x = nls.correction.all, y= EC.gC),col="red" , linewidth =1, fullrange = TRUE) +
    stat_poly_eq(data= SITE_DIEL_FINAL_Daily_C_same_adj_attr %>% filter(Canopy_L1 == 'AA + AW') , aes(x = nls.correction.all, y= EC.gC), col="red", label.x = 0.1, label.y = 0.97) + 
    geom_point(data= SITE_DIEL_FINAL_Daily_C_same_adj_attr %>% filter(Canopy_L1 == 'AA') , aes(x = nls.correction.aa, y= EC.gC), size = 1, alpha=0.015, col="cornflowerblue") + 
    stat_poly_line(data= SITE_DIEL_FINAL_Daily_C_same_adj_attr %>% filter(Canopy_L1 == 'AA') , aes(x = nls.correction.all, y= EC.gC),col="cornflowerblue", linewidth =1, fullrange = TRUE, linetype = "dotdash") +
    stat_poly_eq(data= SITE_DIEL_FINAL_Daily_C_same_adj_attr %>% filter(Canopy_L1 == 'AA') , aes(x = nls.correction.all, y= EC.gC), col="cornflowerblue", label.x = 0.1, label.y = 0.87) +
    geom_point(data= SITE_DIEL_FINAL_Daily_C_same_adj_attr %>% filter(Canopy_L1 == 'AW') , aes(x = nls.correction.aw, y= EC.gC), size = 1, alpha=0.015, col="darkblue") + 
      stat_poly_line(data= SITE_DIEL_FINAL_Daily_C_same_adj_attr %>% filter(Canopy_L1 == 'AW') , aes(x = nls.correction.all, y= EC.gC),col="darkblue", linetype='dashed', fullrange = TRUE) +
      stat_poly_eq(data= SITE_DIEL_FINAL_Daily_C_same_adj_attr %>% filter(Canopy_L1 == 'AW') , aes(x = nls.correction.all, y= EC.gC), col="darkblue", label.x = 0.1, label.y = 0.77) + theme_bw() +
  theme(legend.title = element_blank()) +theme(
    strip.text.x = element_text(size = 10, color = "white"),
    strip.background = element_rect(fill = "black")) + xlim(-40, 40) + ylim(-40,40) +
  facet_wrap( ~IGBP) + xlab("Gradient Flux") + ylab("Eddy Covariance")

ggsave("Figures/IGBP.CO2.H2O.FINAL_AA.png", plot = plot.IGBP.fluxes, width = 7, height = 5, units = "in")


# Update this
SITE_DIEL_FINAL_Daily_C_same_adj_attr %>% filter( gas== "CO2", Canopy_L1 == "AA + AW") %>% mutate( diff.cor = nls.correction.all- EC.gC) %>% select(diff.cor) %>% summary


SITE_DIEL_FINAL_Daily_C_same_adj_attr %>% filter( gas== "H2O", Canopy_L1 == "AA + AW") %>% mutate( diff.cor = nls.correction.all- EC.gC) %>% select(diff.cor) %>% summary

season.AA <- SITE_DIEL_FINAL_Daily_C_same_adj_attr %>% filter(Canopy_L1 == "AA") %>% 
  separate(Year, into = c("year", "month"), sep = "-", remove = FALSE) %>% mutate( month = as.numeric(month),
    season= case_when(month == 1 ~ "Winter",
                      month == 2 ~ "Winter",
                      month == 3 ~ "Spring",
                      month == 4 ~ "Spring",
                      month == 5 ~ "Spring",
                      month == 6 ~ "Summer",
                      month == 7 ~ "Summer",
                      month == 8 ~ "Summer",
                      month == 9 ~ "Fall",
                      month == 10 ~ "Fall",
                      month == 11 ~ "Fall",
                      month == 12 ~ "Winter"))  %>% full_join( canopy.site, relationship = "many-to-many") 



# Create a season indicator:

boxplot.season.CO2 <- season.AA %>% filter( gas == "CO2") %>% ggplot( )  +
  geom_vline(xintercept = 0, color = "red", size=0.5)+
  geom_boxplot( aes(y = Site, x= EC.gC), col="grey50", fill="grey80") +
  geom_boxplot( aes(y = Site, x=nls.correction.aa, col=IGBP), fill='transparent') +
  xlab( expression(paste('Flux (g CO'[2],' m'^-2,  ' day'^-1,')' ))) + ylab ('')  +  theme_bw() + facet_wrap(~season, nrow=1) + xlim(-20,20) 

boxplot.season.H2O <- season.AA %>% filter( gas == "H2O") %>% ggplot( )  +
  geom_vline(xintercept = 0, color = "red", size=0.5)+
  geom_boxplot( aes(y = Site, x= EC.gC), col="grey50", fill="grey80") +
  geom_boxplot( aes(y = Site, x=nls.correction.aa, col=IGBP), fill='transparent') +
  xlab( expression(paste('Flux (g H'[2],'O m'^-2,  ' day'^-1,')' ))) + ylab ('')  +  theme_bw() + facet_wrap(~season, nrow=1) + xlim(-10,10) 

ggsave("Figures/Box_plot_CO2_AA.png", plot = boxplot.season.CO2, width = 8, height = 7, units = "in")


ggsave("Figures/Box_plot_H2O_AA.png", plot = boxplot.season.H2O, width = 8, height = 7, units = "in")



boxplot.season.CO2.igbp <-  season.AA %>% filter(Canopy_L1 == "AA", gas == "CO2") %>% ggplot( )  +
  geom_vline(xintercept = 0, color = "red", size=0.5)+
  geom_boxplot( aes(y = IGBP, x= EC.gC), col="grey50", fill="grey80") +
  geom_boxplot( aes(y = IGBP, x=nls.correction.aa, col=IGBP), fill='transparent') +
  xlab( expression(paste('Flux (g CO'[2],' m'^-2,  ' day'^-1,')' ))) + ylab ('')  +  theme_bw() + facet_wrap(~season, nrow=1) + xlim(-20,20) 


boxplot.season.H2O.igbp <-  season.AA%>% filter( gas == "H2O") %>% ggplot( )  +
  geom_vline(xintercept = 0, color = "red", size=0.5)+
  geom_boxplot( aes(y = IGBP, x= EC.gC), col="grey50", fill="grey80") +
  geom_boxplot( aes(y = IGBP, x=nls.correction.aa, col=IGBP), fill='transparent') +
  xlab( expression(paste('Flux (g CO'[2],' m'^-2,  ' day'^-1,')' ))) + ylab ('')  +  theme_bw() + facet_wrap(~season, nrow=1) + xlim(-10,10) 





ggsave("Figures/Box_plot_AA.png", plot = final.boxplot, width = 8, height = 5, units = "in")

