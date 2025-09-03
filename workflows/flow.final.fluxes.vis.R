# Final diel:

# The Diel analysis is currently set up by CCC threshold:
library(ggpubr)
library(ggplot2)

localdir <- '/Volumes/MaloneLab/Research/FluxGradient/FluxData'
source(fs::path(DirRepo,'functions/calc.diel.R' ))
load(fs::path(localdir,paste0("SITES_One2One.Rdata"))) # Import CCC results

message(" Currently the diel is fit for the peak growing season for each year using all measurement heights. Good data is where the CCC > 0.5, bad data is where the CCC is < 0.5")

# Calculate Diels by site:
for( site in site.list){
    
    print(paste("Working on " ,site))
    
    message( paste("Importing the data for ", site))
    localdir.site <- paste(localdir,"/", site, sep = "")
    
    load(paste(localdir.site, "/", site, "_FILTER.Rdata", sep=""))
    
    SITES_One2One_sub <- SITES_One2One %>% mutate(Approach = factor(Approach, levels = c("MBR", "AE", "WP") ),
                                                  Good.CCC = case_when( gas == 'CO2' & CCC >= 0.5 & Approach == "MBR" ~ 1,
                                                                        gas == 'CO2' &CCC >= 0.75 & Approach == "WP" ~ 1,
                                                                        gas == 'CO2' &CCC >= 0.7 & Approach == "AE" ~ 1,
                                                                        gas == 'CO2' &CCC < 0.5 & Approach == "MBR" ~ 0,
                                                                        gas == 'CO2' & CCC < 0.75 & Approach == "WP" ~ 0,
                                                                        gas == 'CO2' &CCC < 0.7 & Approach == "AE" ~ 0,
                                                                        
                                                                        gas == 'H2O' & CCC < 0.5  ~ 0,
                                                                        gas == 'H2O' & CCC > 0.5  ~ 1) %>% as.factor) %>% 
      select( Site, Good.CCC, dLevelsAminusB, Approach) %>% filter(Site == site) %>% filter(Good.CCC == 1 )
    
    
    MBR_9min_FILTER_CCC <- SITES_One2One_sub %>% filter( Approach == "MBR") %>% full_join( MBR_9min_FILTER , by = c('dLevelsAminusB'))
    WP_9min_FILTER_CCC <- SITES_One2One_sub %>% filter( Approach == "WP") %>% full_join( WP_9min_FILTER , by = c('dLevelsAminusB'))
    AE_9min_FILTER_CCC <- SITES_One2One_sub %>% filter( Approach == "AE") %>% full_join( AE_9min_FILTER , by = c('dLevelsAminusB'))
  
    library(gtools)
    Data <- smartbind(MBR_9min_FILTER_CCC,  AE_9min_FILTER_CCC, WP_9min_FILTER_CCC) %>% as.data.frame() %>% filter( Good.CCC == 1)

    Data %>% ggplot( ) + geom_point(aes( x=hour.local, y=FG_mean))
    
    DIEL.CO2 <-  DIEL.COMPILE.FINAL( dataframe = Data, 
                                  FG_flux = 'FG_mean', 
                                  Gas = "CO2")
    
    
    DIEL.CO2 <- rbind(  DIEL.MBR.CO2, DIEL.WP.CO2, DIEL.AE.CO2 )
    
    DIEL.CO2 %>% filter(data=="good") %>%  ggplot() + geom_point(aes(x=Hour, y = FG, col=Year))+ geom_point(aes(x=Hour, y = EC), col="black") + facet_wrap( ~ TYP)
    
    message( paste("Running DIEL functions- H2O for ", site, "CCC threshold = ", ccc))
    # Diel for H2O
    
    DIEL.MBR.H2O <- DIEL.COMPILE( dataframe = MBR_9min_FILTER_CCC, 
                                  FG_flux = 'FG_mean', 
                                  EC_flux = 'FC_turb_interp',
                                  Gas = "H2O") %>% mutate(TYP = "MBR", gas= "H2O")
    
    DIEL.WP.H2O <- DIEL.COMPILE( dataframe =  WP_9min_FILTER_CCC, 
                                 FG_flux = 'FG_mean', 
                                 EC_flux = 'FC_turb_interp', 
                                 Gas = "H2O") %>% mutate(TYP = "WP", gas= "H2O")
    
    DIEL.AE.H2O <-DIEL.COMPILE( dataframe = AE_9min_FILTER_CCC, 
                                FG_flux = 'FG_mean', 
                                EC_flux = 'FC_turb_interp', 
                                Gas = "H2O") %>% mutate(TYP = "AE", gas= "H2O")
    
    
    DIEL.H2O <- rbind(  DIEL.MBR.H2O, DIEL.WP.H2O, DIEL.AE.H2O ) %>% mutate( TYP = factor( TYP, levels= c('MBR', 'AE', 'WP')))
    
    DIEL.H2O %>% filter(data=="good") %>%  ggplot() + geom_point(aes(x=Hour, y = FG, col=Year))+ geom_point(aes(x=Hour, y = EC), col="black") + facet_wrap( ~ TYP)
    
    final.DIEL <- rbind(   DIEL.CO2,   DIEL.H2O)
    message( paste("Saving data for", site))
    save(final.DIEL, file=paste(localdir.site, "/", site, "_ccc",ccc,"_DIEL.Rdata", sep=""))
  }
  
}

# Calculate diels by Canopy_L2: ####
# 40 has an issue so it is skipped
for( site in site.list){
  
  # Import Canopy_L2 and subset it by the site:
  canopy <- read.csv(file.path(paste(localdir, "canopy_commbined.csv", sep="/"))) %>% distinct
  canopy %>% names()
  
  canopy.site <- canopy %>% select( Site, Canopy_L2, dLevelsAminusB) %>% filter( Site == site)  %>% distinct %>% select(Canopy_L2, dLevelsAminusB )
  
  DIEL.MBR.CL2 <- data.frame()
  DIEL.AE.CL2 <- data.frame()
  DIEL.WP.CL2 <- data.frame()
  
  for(ccc in ccc.thresholds){
    
    print(paste(site, "CCC threshold =", ccc))
    
    message( paste("Importing the data for ", site))
    localdir.site <- paste(localdir,"/", site, sep = "")
    
    load(paste(localdir.site, "/", site, "_FILTER.Rdata", sep=""))
    
    SITES_One2One_sub <- SITES_One2One %>% mutate(Approach = factor(Approach, levels = c("MBR", "AE", "WP") ),
                                                  Good.CCC = case_when( CCC >= ccc ~ 1,
                                                                        CCC < ccc ~ 0)) %>% 
      select( Site, Good.CCC, dLevelsAminusB, Approach) %>% filter(Site == site)
    
    
    MBR_9min_FILTER_CCC <- SITES_One2One_sub %>% filter( Approach == "MBR") %>% full_join( MBR_9min_FILTER , by = c('dLevelsAminusB')) %>% left_join( canopy.site , by = c('dLevelsAminusB')) 
    WP_9min_FILTER_CCC <- SITES_One2One_sub %>% filter( Approach == "WP") %>% full_join( WP_9min_FILTER , by = c('dLevelsAminusB')) %>% left_join( canopy.site , by = c('dLevelsAminusB')) 
    AE_9min_FILTER_CCC <- SITES_One2One_sub %>% filter( Approach == "AE") %>% full_join( AE_9min_FILTER , by = c('dLevelsAminusB')) %>% left_join( canopy.site , by = c('dLevelsAminusB')) 
    
    for( cl2 in MBR_9min_FILTER_CCC$Canopy_L2 %>% unique){
      
      message( paste("Running DIEL functions- CO2 for ", site, cl2))
      
      
      if( MBR_9min_FILTER_CCC %>% filter( Canopy_L2 ==  cl2 ) %>% select(  Good.CCC) %>% sum > 24) {
        
        DIEL.MBR.CO2 <- DIEL.COMPILE( dataframe = MBR_9min_FILTER_CCC %>% filter( Canopy_L2 ==  cl2 ), 
                                      FG_flux = 'FG_mean', 
                                      EC_flux = 'FC_turb_interp',
                                      Gas = "CO2") %>% mutate(TYP = "MBR", gas= "CO2",  Canopy_L2 =  cl2)
        
        DIEL.MBR.H2O <- DIEL.COMPILE( dataframe = MBR_9min_FILTER_CCC%>% filter( Canopy_L2 ==  cl2 ), 
                                      FG_flux = 'FG_mean', 
                                      EC_flux = 'FC_turb_interp',
                                      Gas = "H2O") %>% mutate(TYP = "MBR", gas= "H2O",  Canopy_L2 =  cl2)
        
        DIEL.MBR.CL2 <- rbind( DIEL.MBR.CL2,  DIEL.MBR.CO2, DIEL.MBR.H2O )
        rm( DIEL.MBR.CO2, DIEL.MBR.H2O)
      }
      # Calculate Diurnal Patterns by Year-month:
      
      
      
      
    }
    
    for( cl2 in AE_9min_FILTER_CCC$Canopy_L2 %>% unique){
      
      message( paste("Running DIEL functions- CO2 for ", site, cl2))
      
      if( AE_9min_FILTER_CCC %>% filter( Canopy_L2 ==  cl2 ) %>% select(  Good.CCC) %>% sum > 24) {
        # Calculate Diurnal Patterns by Year-month:
        DIEL.AE.CO2 <- DIEL.COMPILE( dataframe = AE_9min_FILTER_CCC %>% filter( Canopy_L2 ==  cl2 ), 
                                     FG_flux = 'FG_mean', 
                                     EC_flux = 'FC_turb_interp',
                                     Gas = "CO2") %>% mutate(TYP = "AE", gas= "CO2",  Canopy_L2 =  cl2)
        
        DIEL.AE.H2O <- DIEL.COMPILE( dataframe = AE_9min_FILTER_CCC%>% filter( Canopy_L2 ==  cl2 ), 
                                     FG_flux = 'FG_mean', 
                                     EC_flux = 'FC_turb_interp',
                                     Gas = "H2O") %>% mutate(TYP = "AE", gas= "H2O",  Canopy_L2 =  cl2)
        
        DIEL.AE.CL2<- rbind( DIEL.AE.CL2,  DIEL.AE.CO2, DIEL.AE.H2O ) } 
      rm( DIEL.AE.CO2, DIEL.AE.H2O)
      
    }
    
    for( cl2 in WP_9min_FILTER_CCC$Canopy_L2 %>% unique){
      
      message( paste("Running DIEL functions- CO2 for ", site, cl2))
      
      if(  WP_9min_FILTER_CCC %>% filter( Canopy_L2 ==  cl2 ) %>% select(  Good.CCC) %>% sum > 24) {
        # Calculate Diurnal Patterns by Year-month:
        DIEL.WP.CO2 <- DIEL.COMPILE( dataframe = WP_9min_FILTER_CCC %>% filter( Canopy_L2 ==  cl2 ), 
                                     FG_flux = 'FG_mean', 
                                     EC_flux = 'FC_turb_interp',
                                     Gas = "CO2") %>% mutate(TYP = "WP", gas= "CO2",  Canopy_L2 =  cl2)
        
        DIEL.WP.H2O <- DIEL.COMPILE( dataframe = WP_9min_FILTER_CCC%>% filter( Canopy_L2 ==  cl2 ), 
                                     FG_flux = 'FG_mean', 
                                     EC_flux = 'FC_turb_interp',
                                     Gas = "H2O") %>% mutate(TYP = "WP", gas= "H2O",  Canopy_L2 =  cl2)
        
        DIEL.WP.CL2 <- rbind( DIEL.WP.CL2,  DIEL.WP.CO2, DIEL.WP.H2O )
        rm( DIEL.WP.CO2, DIEL.WP.H2O)
      }
      
    }
    
    final.DIEL <- rbind(DIEL.MBR.CL2,  DIEL.AE.CL2, DIEL.WP.CL2)
    
    message( paste("Saving data for", site))
    save(final.DIEL, file=paste(localdir.site, "/", site, "_ccc",ccc,"_CL2_DIEL.Rdata", sep=""))
    
    rm(   MBR_9min_FILTER_CCC, WP_9min_FILTER_CCC,  AE_9min_FILTER_CCC)
  }
  
}


# Plots : ####
for ( site in site.list){
  for(ccc in ccc.thresholds){
    
    print(paste(site, "CCC threshold =", ccc))
    
    # Import the data: 
    localdir.site <- paste(localdir,"/", site, sep = "")
    load(file=paste(localdir.site, "/", site, "_ccc",ccc,"_DIEL.Rdata", sep=""))
    
    message(paste("Lets make some plots for", site))
    
    
    try({
      
      p1 <- ggplot( data = final.DIEL %>% filter(TYP=="MBR", gas == "CO2")) + stat_smooth(aes(x = Hour , y = FG), col="black") + stat_smooth(aes(x = Hour , y = EC), col="black", linetype="dashed") + theme_bw() + ylab("MBR") +facet_wrap(~data)
      
      p2 <- ggplot( data =  final.DIEL %>% filter(TYP=="AE", gas == "CO2")) + stat_smooth(aes(x = Hour , y = FG), col="black") + stat_smooth(aes(x = Hour , y = EC), col="black", linetype="dashed") + theme_bw() + ylab("AE") +facet_wrap(~data)
      
      p3 <-ggplot( data =  final.DIEL %>% filter(TYP=="WP", gas == "CO2")) + stat_smooth(aes(x = Hour , y = FG), col="black") + stat_smooth(aes(x = Hour , y = EC), col="black", linetype="dashed") + theme_bw() + ylab("WP") +facet_wrap(~data)
    })
    
    print(ggarrange( p1, p2, p3, nrow=3))
    
    setwd(dir.diel)
    
    png(paste("Diel_CO2_", site,"_", ccc,".png", sep=""), width=6, 
        height=6, units="in", res=1200)
    
    print(ggarrange( p1, p2, p3, nrow=3))
    dev.off()
    
    message("done with CO2 Diel")       
    
    try({
      p1 <- ggplot( data = final.DIEL %>% filter(TYP=="MBR", gas == "H2O")) + stat_smooth(aes(x = Hour , y = FG), col="black") + stat_smooth(aes(x = Hour , y = EC), col="black", linetype="dashed") + theme_bw() + ylab("MBR") +facet_wrap(~data)
      
      p2 <- ggplot( data =  final.DIEL %>% filter(TYP=="AE", gas == "H2O")) + stat_smooth(aes(x = Hour , y = FG), col="black") + stat_smooth(aes(x = Hour , y = EC), col="black", linetype="dashed") + theme_bw() + ylab("AE") +facet_wrap(~data)
      
      p3 <-ggplot( data =  final.DIEL %>% filter(TYP=="WP", gas == "H2O")) + stat_smooth(aes(x = Hour , y = FG), col="black") + stat_smooth(aes(x = Hour , y = EC), col="black", linetype="dashed") + theme_bw() + ylab("WP") +facet_wrap(~data)
    })
    
    print(ggarrange( p1, p2, p3, nrow=3))
    
    png(paste("Diel_H2O_", site,"_", ccc,".png", sep=""), width=6, 
        height=5, units="in", res=1200)
    print(ggarrange( p1, p2, p3, nrow=3))
    dev.off()
    
    print("done with H2O diel")   
    
  }}

# Build Summary files by CCC ####
# Summary of diel fit by threshold:
diel.summary.year.gas <- data.frame(Year = as.character(),
                                    gas = as.character(),
                                    Site = as.character(),
                                    ccc = as.numeric(),
                                    count = as.numeric(),
                                    FG.total = as.numeric(),
                                    FG.SE = as.numeric(), 
                                    EC.total = as.numeric(), 
                                    EC.SE = as.numeric(),
                                    FG.min = as.numeric(),
                                    FG.max = as.numeric(),
                                    FG.mean = as.numeric(), 
                                    FG.SE = as.numeric(),
                                    EC.min = as.numeric(), 
                                    EC.max = as.numeric(), 
                                    EC.mean = as.numeric(), 
                                    EC.SE = as.numeric(),
                                    DIFF = as.numeric())

diel.summary.year.gas.typ <- data.frame(Year = as.character(),
                                        gas = as.character(),
                                        TYP = as.character(),
                                        Site = as.character(),
                                        ccc = as.numeric(),
                                        count = as.numeric(),
                                        FG.total = as.numeric(),
                                        FG.SE = as.numeric(), 
                                        EC.total = as.numeric(), 
                                        EC.SE = as.numeric(),
                                        FG.min = as.numeric(),
                                        FG.max = as.numeric(),
                                        FG.mean = as.numeric(), 
                                        FG.SE = as.numeric(),
                                        EC.min = as.numeric(), 
                                        EC.max = as.numeric(), 
                                        EC.mean = as.numeric(), 
                                        EC.SE = as.numeric(),
                                        DIFF = as.numeric() )



for( site in site.list){
  for(ccc in ccc.thresholds){
    
    localdir.site = paste( localdir ,"/", site, sep="")
    print(paste(site, "CCC threshold =", ccc))
    load(file=paste(localdir.site, "/", site, "_ccc",ccc,"_DIEL.Rdata", sep=""))
    
    # Add the sit and CCC to the file
    final.DIEL$Site <- site
    final.DIEL$ccc.threshold <- ccc
    
    # Summarize results
    sub.year.gas <- final.DIEL %>% filter( data== "good") %>% reframe(.by = c(Year, gas),
                                                                      Site = site,
                                                                      ccc=ccc,
                                                                      count = mean(count),
                                                                      FG.total = sum(abs(FG), na.rm=T), 
                                                                      FG.SE = sum(abs(FG.SE), na.rm=T), 
                                                                      EC.total = sum(abs(EC), na.rm=T), 
                                                                      EC.SE = sum(abs(EC.SE), na.rm=T),
                                                                      FG.min = min(FG, na.rm=T),
                                                                      FG.max = max(FG, na.rm=T),
                                                                      FG.mean = mean(FG, na.rm=T), 
                                                                      FG.SE = var(FG, na.rm=T)/sqrt(length(FG)),
                                                                      EC.min = min(EC, na.rm=T), 
                                                                      EC.max = max(EC, na.rm=T), 
                                                                      EC.mean = mean(EC, na.rm=T), 
                                                                      EC.SE = var(EC, na.rm=T)/sqrt(length(EC)),
                                                                      DIFF=  mean( DIFF.DIEL, na.rm=T))
    
    sub.year.gas.typ <- final.DIEL %>% filter( data== "good") %>% reframe(.by = c(Year, gas,TYP),
                                                                          Site = site,
                                                                          ccc=ccc,
                                                                          count = mean(count),
                                                                          FG.total = sum(abs(FG), na.rm=T), 
                                                                          FG.SE = sum(abs(FG.SE), na.rm=T), 
                                                                          EC.total = sum(abs(EC), na.rm=T), 
                                                                          EC.SE = sum(abs(EC.SE), na.rm=T),
                                                                          FG.min = min(FG, na.rm=T),
                                                                          FG.max = max(FG, na.rm=T),
                                                                          FG.mean = mean(FG, na.rm=T), 
                                                                          FG.SE = var(FG, na.rm=T)/sqrt(length(FG)),
                                                                          EC.min = min(EC, na.rm=T), 
                                                                          EC.max = max(EC, na.rm=T), 
                                                                          EC.mean = mean(EC, na.rm=T), 
                                                                          EC.SE = var(EC, na.rm=T)/sqrt(length(EC)),
                                                                          DIFF=  mean( DIFF.DIEL, na.rm=T))
    
    diel.summary.year.gas <- rbind( diel.summary.year.gas,  sub.year.gas)
    diel.summary.year.gas.typ <- rbind( diel.summary.year.gas.typ,  sub.year.gas.typ)
    
    rm(final.DIEL,sub.year.gas,   sub.year.gas.typ)
    
  }}

# Build Summary files by CCC and Canopy_L2####

# Summary of diel fit by threshold and CL2:
diel.summary.year.gas.cl2 <- data.frame(Year = as.character(),
                                        gas = as.character(),
                                        Canopy_L2 = as.character(), 
                                        Site = as.character(),
                                        ccc = as.numeric(),
                                        Canopy_L2 = as.character(), 
                                        count = as.numeric(),
                                        FG.total = as.numeric(),
                                        FG.SE = as.numeric(), 
                                        EC.total = as.numeric(), 
                                        EC.SE = as.numeric(),
                                        FG.min = as.numeric(),
                                        FG.max = as.numeric(),
                                        FG.mean = as.numeric(), 
                                        FG.SE = as.numeric(),
                                        EC.min = as.numeric(), 
                                        EC.max = as.numeric(), 
                                        EC.mean = as.numeric(), 
                                        EC.SE = as.numeric(),
                                        DIFF = as.numeric())

diel.summary.year.gas.cl2.typ <- data.frame(Year = as.character(),
                                            gas = as.character(),
                                            Canopy_L2 = as.character(), 
                                            TYP = as.character(),
                                            Site = as.character(),
                                            ccc = as.numeric(),
                                            count = as.numeric(),
                                            FG.total = as.numeric(),
                                            FG.SE = as.numeric(), 
                                            EC.total = as.numeric(), 
                                            EC.SE = as.numeric(),
                                            FG.min = as.numeric(),
                                            FG.max = as.numeric(),
                                            FG.mean = as.numeric(), 
                                            FG.SE = as.numeric(),
                                            EC.min = as.numeric(), 
                                            EC.max = as.numeric(), 
                                            EC.mean = as.numeric(), 
                                            EC.SE = as.numeric(),
                                            DIFF = as.numeric() )



for( site in site.list){
  for(ccc in ccc.thresholds){
    
    localdir.site = paste( localdir ,"/", site, sep="")
    print(paste(site, "CCC threshold =", ccc))
    file_path=paste(localdir.site, "/", site, "_ccc",ccc,"_CL2_DIEL.Rdata", sep="")
    
    if (file.exists(file_path)) {
      load(file=paste(localdir.site, "/", site, "_ccc",ccc,"_CL2_DIEL.Rdata", sep="")) 
      if( length(final.DIEL) >0){
        # Add the sit and CCC to the file
        final.DIEL$Site <- site
        final.DIEL$ccc.threshold <- ccc
        # Summarize results
        sub.year.gas <- final.DIEL %>% filter( data== "good") %>% reframe(.by = c(Year, gas,  Canopy_L2),
                                                                          Site = site,
                                                                          ccc=ccc,
                                                                          count = mean(count),
                                                                          FG.total = sum(abs(FG), na.rm=T), 
                                                                          FG.SE = sum(abs(FG.SE), na.rm=T), 
                                                                          EC.total = sum(abs(EC), na.rm=T), 
                                                                          EC.SE = sum(abs(EC.SE), na.rm=T),
                                                                          FG.min = min(FG, na.rm=T),
                                                                          FG.max = max(FG, na.rm=T),
                                                                          FG.mean = mean(FG, na.rm=T), 
                                                                          FG.SE = var(FG, na.rm=T)/sqrt(length(FG)),
                                                                          EC.min = min(EC, na.rm=T), 
                                                                          EC.max = max(EC, na.rm=T), 
                                                                          EC.mean = mean(EC, na.rm=T), 
                                                                          EC.SE = var(EC, na.rm=T)/sqrt(length(EC)),
                                                                          DIFF=  mean( DIFF.DIEL, na.rm=T))
        
        sub.year.gas.typ <- final.DIEL %>% filter( data== "good") %>% reframe(.by = c(Year, gas,Canopy_L2, TYP),
                                                                              Site = site,
                                                                              ccc=ccc,
                                                                              count = mean(count),
                                                                              FG.total = sum(abs(FG), na.rm=T), 
                                                                              FG.SE = sum(abs(FG.SE), na.rm=T), 
                                                                              EC.total = sum(abs(EC), na.rm=T), 
                                                                              EC.SE = sum(abs(EC.SE), na.rm=T),
                                                                              FG.min = min(FG, na.rm=T),
                                                                              FG.max = max(FG, na.rm=T),
                                                                              FG.mean = mean(FG, na.rm=T), 
                                                                              FG.SE = var(FG, na.rm=T)/sqrt(length(FG)),
                                                                              EC.min = min(EC, na.rm=T), 
                                                                              EC.max = max(EC, na.rm=T), 
                                                                              EC.mean = mean(EC, na.rm=T), 
                                                                              EC.SE = var(EC, na.rm=T)/sqrt(length(EC)),
                                                                              DIFF=  mean( DIFF.DIEL, na.rm=T))
        
        diel.summary.year.gas.cl2 <- rbind( diel.summary.year.gas.cl2,  sub.year.gas)
        diel.summary.year.gas.cl2.typ <- rbind( diel.summary.year.gas.cl2.typ,  sub.year.gas.typ)
        
        rm(final.DIEL,sub.year.gas,   sub.year.gas.typ)
        
      }
    }}}


# Create Summary Plots by Canopy_L2

# Adjuest the order of factors for plotting:
diel.summary.year.gas.cl2.typ$TYP <- factor( diel.summary.year.gas.cl2.typ$TYP , levels = c("MBR", "AE", "WP"))
diel.summary.year.gas.cl2.typ$Canopy_L2 <- factor( diel.summary.year.gas.cl2.typ$Canopy_L2 , levels = c("AA+" , "AA", "AW+-", "AW+", "AW-" , "AW",   "WW-", "WW" ))


diel.summary.mean.line.co2.cl2.typ <- diel.summary.year.gas.cl2.typ %>% filter(gas=="CO2") %>% reframe(.by=c( ccc , Canopy_L2, TYP) ,DIFF.total = mean(DIFF),
                                                                                                       DIFF.max = mean(FG.max - EC.max),
                                                                                                       DIFF.min = mean(FG.min - EC.min)) %>% fortify()

diel.summary.mean.line.h2o.cl2.typ <- diel.summary.year.gas.cl2.typ %>% filter(gas=="H2O") %>% reframe(.by=c( ccc , Canopy_L2, TYP) ,DIFF.total = mean(DIFF),
                                                                                                       DIFF.max = mean(FG.max - EC.max),
                                                                                                       DIFF.min = mean(FG.min - EC.min)) %>% fortify()

save(diel.summary.mean.line.h2o.cl2.typ,  diel.summary.mean.line.co2.cl2.typ,
     diel.summary.year.gas.cl2.typ, diel.summary.year.gas,diel.summary.year.gas.typ ,
     file='/Volumes/MaloneLab/Research/FluxGradient/DIEL_SUMMARY.RDATA')
