

# Functions use the filtered data and provide both CCC and R2:

source(fs::path(DirRepo,"./functions/calc.linear.terms.R"))


validation.canopy <- function( threshold){
  
  site.list <- SITE_DATA_FILTERED %>% names()
  Linear_CanopyLevel_Stats <- data.frame()
  
  for( site in site.list[-c(31, 42)]){
    
    print(site)
    
    df <-SITE_DATA_FILTERED[[site]] %>% filter(gas != "CH4") %>% 
      mutate(month = format(timeEndA.local,'%m') %>% as.numeric,
             season = case_when(
               month %in% c(12, 1, 2) ~ "Winter",
               month %in% c(3, 4, 5) ~ "Spring",
               month %in% c(6, 7, 8) ~ "Summer",
               TRUE ~ "Autumn"),
             hour = format(timeEndA.local,'%H'),
             count= case_when( is.na(FG_mean) == FALSE ~ 1,
                               TRUE ~ 0)) %>% distinct
    
    
    canopy.sub <- SITES_One2One_canopy_model %>% select( Site, dLevelsAminusB, gas, Approach, rf_model ) %>% filter( Site == site)
    
    if( threshold == "rf_model.1"){
      
      print( "Using the threshold for rf_model = 1")
      df_canopy <- df %>% left_join(canopy.sub , by=c("dLevelsAminusB", "gas", "Approach" ), relationship = "many-to-many") %>% mutate( time.rounded = timeEndA.local %>% round_date( unit = "30 minutes") ) %>% 
        filter(predicted == '1') %>% #THRESHOLD
        reframe( .by= c(dLevelsAminusB, gas, Approach, time.rounded), 
                 EC_mean = mean(EC_mean, na.rm=T), 
                 FG_mean = mean(FG_mean, na.rm=T))
    } 
    
    if( threshold == "rf_model.all"){
      print( "Using  no threshold for rf_model")
      df_canopy <- df %>% left_join(canopy.sub , by=c("dLevelsAminusB", "gas", "Approach" ), relationship = "many-to-many") %>% mutate( time.rounded = timeEndA.local %>% round_date( unit = "30 minutes") ) %>% 
        #filter(predicted == '1') %>% #THRESHOLD
        reframe( .by= c(dLevelsAminusB, gas, Approach, time.rounded), 
                 EC_mean = mean(EC_mean, na.rm=T), 
                 FG_mean = mean(FG_mean, na.rm=T))
    } 
    
    if( threshold == "CCC.0.5"){
      print( "Using the threshold for CCC >= 0.5")
      df_canopy <- df %>% left_join(canopy.sub , by=c("dLevelsAminusB", "gas", "Approach" ), relationship = "many-to-many") %>% mutate( time.rounded = timeEndA.local %>% round_date( unit = "30 minutes") ) %>% 
        filter(CCC >= 0.5) %>% #THRESHOLD
        reframe( .by= c(dLevelsAminusB, gas, Approach, time.rounded), 
                 EC_mean = mean(EC_mean, na.rm=T), 
                 FG_mean = mean(FG_mean, na.rm=T))
    }
    
    # CO2 : ####
    df_canopy_CO2 <- df_canopy %>% filter( gas == "CO2")
    
    if(df_canopy_CO2$dLevelsAminusB %>% unique %>% length > 1){
      
      df_canopy_level_wide <- df_canopy_CO2 %>% select(dLevelsAminusB, FG_mean, time.rounded, Approach) %>% 
        pivot_wider(names_from = dLevelsAminusB, values_from = FG_mean) %>% as.data.frame() 
      
      canopy_level.list.co2 <- df_canopy_CO2$dLevelsAminusB %>% unique
      
      combinations.canopy_level.list.co2 <- combn(canopy_level.list.co2,2)
      
      for(i in 1:ncol(combinations.canopy_level.list.co2)){
        print( paste( "Working on combination", combinations.canopy_level.list.co2[1, i], combinations.canopy_level.list.co2[2, i], sep = " "))
        
        new.data <- linear_terms(  df = df_canopy_level_wide,
                                   x_col=combinations.canopy_level.list.co2[1, i],
                                   y_col=combinations.canopy_level.list.co2[2, i] ,
                                   site= site,
                                   var1= combinations.canopy_level.list.co2[1, i],
                                   var2=combinations.canopy_level.list.co2[2, i]) %>% 
          mutate( gas = "CO2")
        
        Linear_CanopyLevel_Stats <- rbind(Linear_CanopyLevel_Stats, new.data)
      }
      
      
    }
    
    # H2O : ####
    df_canopy_H2O <- df_canopy %>% filter( gas == "H2O")
    
    if(df_canopy_H2O$dLevelsAminusB %>% unique %>% length > 1){
      
      df_canopy_level_wide <- df_canopy_H2O %>% select(dLevelsAminusB, FG_mean, time.rounded, Approach) %>% 
        pivot_wider(names_from = dLevelsAminusB, values_from = FG_mean) %>% distinct %>% unnest(cols = c(time.rounded)) %>% as.data.frame() 
      
      canopy_level.list.h2o <- df_canopy_H2O$dLevelsAminusB %>% unique
      
      combinations.canopy_level.list.h2o <- combn(canopy_level.list.h2o,2)
      
      for(i in 1:ncol(combinations.canopy_level.list.h2o)){
        print( paste( "Working on combination", combinations.canopy_level.list.h2o[1, i], combinations.canopy_level.list.h2o[2, i], sep = " "))
        
        new.data <- linear_terms(  df = df_canopy_level_wide,
                                   x_col=combinations.canopy_level.list.h2o[1, i],
                                   y_col=combinations.canopy_level.list.h2o[2, i],
                                   site = site,
                                   var1= combinations.canopy_level.list.h2o[1, i],
                                   var2=combinations.canopy_level.list.h2o[2, i])%>% 
          mutate( gas = "H2O")
        
        Linear_CanopyLevel_Stats <- rbind(Linear_CanopyLevel_Stats, new.data)
      }
      
    }
    
  }
  
  return(  Linear_CanopyLevel_Stats)
}
Compare.SamplingHeightPairs <- function( threshold){
  
  site.list <- SITE_DATA_FILTERED %>% names()
  Linear_CanopyLevel_Stats <- data.frame()
  
  for( site in site.list[-c(31, 42)]){
    
    print(site)
    
    df <-SITE_DATA_FILTERED[[site]] %>% filter(gas != "CH4") %>% 
      mutate(month = format(timeEndA.local,'%m') %>% as.numeric,
             season = case_when(
               month %in% c(12, 1, 2) ~ "Winter",
               month %in% c(3, 4, 5) ~ "Spring",
               month %in% c(6, 7, 8) ~ "Summer",
               TRUE ~ "Autumn"),
             hour = format(timeEndA.local,'%H'),
             count= case_when( is.na(FG_mean) == FALSE ~ 1,
                               TRUE ~ 0)) %>% distinct
    
    
    canopy.sub <- SITES_One2One_canopy_model %>% select( Site, dLevelsAminusB, gas, Approach, rf_model ) %>% filter( Site == site)
    
      print( "Using  no threshold for rf_model")
      
      df_canopy <- df %>% left_join(canopy.sub , by=c("dLevelsAminusB", "gas", "Approach" ), relationship = "many-to-many") %>% mutate( time.rounded = timeEndA.local %>% round_date( unit = "30 minutes") ) %>% 
        #filter(rf_model == '1') %>% #THRESHOLD
        reframe( .by= c(dLevelsAminusB, gas, Approach, time.rounded), 
                 EC_mean = mean(EC_mean, na.rm=T), 
                 FG_mean = mean(FG_mean, na.rm=T)) %>% mutate( levels = paste(Approach,dLevelsAminusB, sep="-"))
    
    
    # CO2 : ####
    df_canopy_CO2 <- df_canopy %>% filter( gas == "CO2") 

    if(df_canopy_CO2$levels %>% unique %>% length > 1){
      df_canopy_CO2 %>% names
      
      df_canopy_level_wide <- df_canopy_CO2 %>% select(levels, time.rounded, FG_mean ) %>% 
        pivot_wider(names_from = levels, values_from = c(FG_mean)) %>% as.data.frame() 
      
      canopy_level.list.co2 <- df_canopy_CO2$levels %>% unique
      
      combinations.canopy_level.list.co2 <- combn(canopy_level.list.co2,2)
      
      for(i in 1:ncol(combinations.canopy_level.list.co2)){
        print( paste( "Working on combination", combinations.canopy_level.list.co2[1, i], combinations.canopy_level.list.co2[2, i], sep = " "))
        
        new.data <- linear_terms(  df = df_canopy_level_wide,
                                   x_col=combinations.canopy_level.list.co2[1, i],
                                   y_col= combinations.canopy_level.list.co2[2, i],
                                   site= site,
                                   var1= combinations.canopy_level.list.co2[1, i],
                                   var2=combinations.canopy_level.list.co2[2, i])%>% 
          mutate( gas = "CO2")
        
        Linear_CanopyLevel_Stats <- rbind(Linear_CanopyLevel_Stats, new.data)
      }
      
      
    }
    
    # H2O : ####
    df_canopy_H2O <- df_canopy %>% filter( gas == "H2O")
    
    if(df_canopy_H2O$levels %>% unique %>% length > 1){
      
      df_canopy_level_wide <- df_canopy_H2O %>% select(levels, time.rounded, FG_mean ) %>% 
        pivot_wider(names_from = levels, values_from = c(FG_mean)) %>% as.data.frame() 
      
      canopy_level.list.h2o <- df_canopy_H2O$levels %>% unique
      
      combinations.canopy_level.list.h2o <- combn(canopy_level.list.h2o,2)
      
      for(i in 1:ncol(combinations.canopy_level.list.h2o)){
        print( paste( "Working on combination", combinations.canopy_level.list.h2o[1, i], combinations.canopy_level.list.h2o[2, i], sep = " "))
        
        new.data <- linear_terms(  df = df_canopy_level_wide,
                                   x_col= combinations.canopy_level.list.h2o[1, i],
                                   y_col= combinations.canopy_level.list.h2o[2, i],
                                   site = site,
                                   var1= combinations.canopy_level.list.h2o[1, i],
                                   var2=combinations.canopy_level.list.h2o[2, i])%>% 
          mutate( gas = "H2O")
        
        Linear_CanopyLevel_Stats <- rbind(Linear_CanopyLevel_Stats, new.data)
      }
      
    }
    
  }
  
  return(  Linear_CanopyLevel_Stats)
}
validation.approach <- function(index.df){
  
  site.list <- SITE_DATA_FILTERED %>% names()
  Linear_Approach_Stats <- data.frame()
  
  for( site in site.list[-c(31, 42)]){
    
    print(site)
    
    df <-SITE_DATA_FILTERED[[site]] %>% filter(gas != "CH4") %>% 
      mutate(month = format(timeEndA.local,'%m') %>% as.numeric,
             season = case_when(
               month %in% c(12, 1, 2) ~ "Winter",
               month %in% c(3, 4, 5) ~ "Spring",
               month %in% c(6, 7, 8) ~ "Summer",
               TRUE ~ "Autumn"),
             hour = format(timeEndA.local,'%H'),
             count= case_when( is.na(FG_mean) == FALSE ~ 1,
                               TRUE ~ 0)) %>% distinct
    
    
    canopy.sub <- index.df %>% select( Site, dLevelsAminusB, gas, Approach, Good.CCC.EC.FG ) %>% filter( Site == site)
    
    df_canopy <- df %>% left_join(canopy.sub , by=c("dLevelsAminusB", "gas", "Approach" ), relationship = "many-to-many") %>% mutate( time.rounded = timeEndA.local %>% round_date( unit = "30 minutes") ) %>% 
      filter( Good.CCC.EC.FG  == '1') %>%
      reframe( .by= c( gas, Approach, time.rounded), 
               EC_mean = mean(EC_mean, na.rm=T), 
               FG_mean = mean(FG_mean, na.rm=T)) 
      
    # CO2 : ####
    df_canopy_CO2 <- df_canopy %>% filter( gas == "CO2")
    
    if(df_canopy_CO2$Approach %>% unique %>% length > 1){
      
      df_canopy_approach_wide <- df_canopy_CO2 %>% select(Approach,  FG_mean, time.rounded) %>% 
        pivot_wider(names_from = Approach, values_from = FG_mean, values_fn =mean) %>% as.data.frame() 
      
      # Make a list of the condition in the df:
      approach.list.co2 <- df_canopy_CO2$Approach %>% unique
      
      combinations.approach.list.co2 <- combn( approach.list.co2,2)
      
      for(i in 1:ncol( combinations.approach.list.co2)){
        print( paste( "Working on combination",  combinations.approach.list.co2[1, i],  combinations.approach.list.co2[2, i], sep = " "))
        
        new.data <- linear_terms2(  df = df_canopy_approach_wide,
                                   x_col=combinations.approach.list.co2[1, i],
                                   y_col= combinations.approach.list.co2[2, i],
                                   site = site ) %>% 
          mutate( gas = "CO2")
        
        Linear_Approach_Stats <- rbind(Linear_Approach_Stats, new.data)
        
       
      }
      rm(combinations.approach.list.co2)
    }
    
    # H2O : ####
    df_canopy_H2O <- df_canopy %>% filter( gas == "H2O")
    
    if(df_canopy_H2O$Approach %>% unique %>% length > 1){
      
      df_canopy_approach_wide <- df_canopy_H2O %>% select(Approach,  FG_mean, time.rounded) %>% 
        pivot_wider(names_from = Approach, values_from = FG_mean, values_fn =mean) %>% as.data.frame() 
      
      # Make a list of the condition in the df:
      approach.list.H2O <- df_canopy_H2O$Approach %>% unique
      
      combinations.approach.list.H2O <- combn(approach.list.H2O ,2)
      
      for(i in 1:ncol(combinations.approach.list.H2O)){
        print( paste( "Working on combination", combinations.approach.list.H2O[1, i], combinations.approach.list.H2O[2, i], sep = " "))
        
        new.data <- linear_terms2(  df = df_canopy_approach_wide,
                                   x_col= combinations.approach.list.H2O[1, i],
                                   y_col= combinations.approach.list.H2O[2, i],
                                   site = site) %>% 
          mutate( gas = "H2O")
        
        Linear_Approach_Stats <- rbind(Linear_Approach_Stats, new.data)
        
        
      }
      
    }
    
  }
  
  Finaldata <- Linear_Approach_Stats
  return(Finaldata )
}
validation.canopy.l1 <- function(index.df){
  
  site.list <- SITE_DATA_FILTERED %>% names()
  Linear_Canopy_L1_Stats <- data.frame()
  
  for( site in site.list[-c(31, 42)]){
    
    print(site)
    
    df <-SITE_DATA_FILTERED[[site]] %>% filter(gas != "CH4") %>% 
      mutate(month = format(timeEndA.local,'%m') %>% as.numeric,
             season = case_when(
               month %in% c(12, 1, 2) ~ "Winter",
               month %in% c(3, 4, 5) ~ "Spring",
               month %in% c(6, 7, 8) ~ "Summer",
               TRUE ~ "Autumn"),
             hour = format(timeEndA.local,'%H'),
             count= case_when( is.na(FG_mean) == FALSE ~ 1,
                               TRUE ~ 0)) %>% distinct
    
    
    canopy.sub <- index.df %>% select( Site, dLevelsAminusB, gas, Approach, Good.CCC.EC.FG) %>% filter( Site == site)
    
    df_canopy <- df %>% left_join(canopy.sub , by=c("dLevelsAminusB", "gas", "Approach" ), relationship = "many-to-many") %>% mutate( time.rounded = timeEndA.local %>% round_date( unit = "30 minutes") ) %>% 
      filter( Good.CCC.EC.FG  == '1') %>%
      reframe( .by= c( gas, Canopy_L1, time.rounded), 
               EC_mean = mean(EC_mean, na.rm=T), 
               FG_mean = mean(FG_mean, na.rm=T)) 
    
    # CO2 : ####
    df_canopy_CO2 <- df_canopy %>% filter( gas == "CO2")
    
    if(df_canopy_CO2$Canopy_L1 %>% unique %>% length > 1){
      
      df_canopy_approach_wide <- df_canopy_CO2 %>% select(Canopy_L1,  FG_mean, time.rounded) %>% 
        pivot_wider(names_from = Canopy_L1, values_from = FG_mean, values_fn =mean) %>% as.data.frame() 
      
      # Make a list of the condition in the df:
      approach.list.co2 <- df_canopy_CO2$Canopy_L1 %>% unique
      
      combinations.approach.list.co2 <- combn( approach.list.co2,2)
      
      for(i in 1:ncol( combinations.approach.list.co2)){
        print( paste( "Working on combination",  combinations.approach.list.co2[1, i],  combinations.approach.list.co2[2, i], sep = " "))
        
        new.data <- linear_terms2(  df = df_canopy_approach_wide,
                                    x_col=combinations.approach.list.co2[1, i],
                                    y_col= combinations.approach.list.co2[2, i],
                                    site = site ) %>% 
          mutate( gas = "CO2")
        
        Linear_Canopy_L1_Stats <- rbind(Linear_Canopy_L1_Stats, new.data)
        
        
      }
      rm(combinations.approach.list.co2)
    }
    
    # H2O : ####
    df_canopy_H2O <- df_canopy %>% filter( gas == "H2O")
    
    if(df_canopy_H2O$Canopy_L1 %>% unique %>% length > 1){
      
      df_canopy_approach_wide <- df_canopy_H2O %>% select(Canopy_L1,  FG_mean, time.rounded) %>% 
        pivot_wider(names_from = Canopy_L1, values_from = FG_mean, values_fn =mean) %>% as.data.frame() 
      
      # Make a list of the condition in the df:
      approach.list.H2O <- df_canopy_H2O$Canopy_L1 %>% unique
      
      combinations.approach.list.H2O <- combn(approach.list.H2O ,2)
      
      for(i in 1:ncol(combinations.approach.list.H2O)){
        print( paste( "Working on combination", combinations.approach.list.H2O[1, i], combinations.approach.list.H2O[2, i], sep = " "))
        
        new.data <- linear_terms2(  df = df_canopy_approach_wide,
                                    x_col= combinations.approach.list.H2O[1, i],
                                    y_col= combinations.approach.list.H2O[2, i],
                                    site = site) %>% 
          mutate( gas = "H2O")
        
        Linear_Canopy_L1_Stats <- rbind(Linear_Canopy_L1_Stats, new.data)
        
        
      }
      
    }
    
  }
  
  Finaldata <-  Linear_Canopy_L1_Stats
  return(Finaldata )
}
validation.SamplingHeightPairs <- function( index.df){
  
  site.list <- SITE_DATA_FILTERED %>% names()
  Linear_CanopyLevel_Stats <- data.frame()
  
  for( site in site.list[-c(31, 42)]){
    
    print(site)
    
    df <-SITE_DATA_FILTERED[[site]] %>% filter(gas != "CH4") %>% 
      mutate(month = format(timeEndA.local,'%m') %>% as.numeric,
             season = case_when(
               month %in% c(12, 1, 2) ~ "Winter",
               month %in% c(3, 4, 5) ~ "Spring",
               month %in% c(6, 7, 8) ~ "Summer",
               TRUE ~ "Autumn"),
             hour = format(timeEndA.local,'%H'),
             count= case_when( is.na(FG_mean) == FALSE ~ 1,
                               TRUE ~ 0)) %>% distinct
    
    
    canopy.sub <- index.df %>% select( Site, dLevelsAminusB, gas, Approach, Good.CCC.EC.FG) %>% filter( Site == site)
    
    df_canopy <- df %>% left_join(canopy.sub , by=c("dLevelsAminusB", "gas", "Approach" ), 
                                  relationship = "many-to-many") %>% 
      mutate( time.rounded = timeEndA.local %>% round_date( unit = "30 minutes") ) %>% 
      filter( Good.CCC.EC.FG  == '1') %>%
      reframe( .by= c( dLevelsAminusB, gas, Approach, time.rounded), 
               EC_mean = mean(EC_mean, na.rm=T), 
               FG_mean = mean(FG_mean, na.rm=T)) %>% mutate( levels = paste(Approach,dLevelsAminusB, sep="-"))

    
    # CO2 : ####
    df_canopy_CO2 <- df_canopy %>% filter( gas == "CO2") 
    
    if(df_canopy_CO2$levels %>% unique %>% length > 1){
      df_canopy_CO2 %>% names
      
      df_canopy_level_wide <- df_canopy_CO2 %>% select(levels, time.rounded, FG_mean ) %>% 
        pivot_wider(names_from = levels, values_from = c(FG_mean)) %>% as.data.frame() 
      
      canopy_level.list.co2 <- df_canopy_CO2$levels %>% unique
      
      combinations.canopy_level.list.co2 <- combn(canopy_level.list.co2,2)
      
      for(i in 1:ncol(combinations.canopy_level.list.co2)){
        print( paste( "Working on combination", combinations.canopy_level.list.co2[1, i], combinations.canopy_level.list.co2[2, i], sep = " "))
        
        new.data <- linear_terms(  df = df_canopy_level_wide,
                                   x_col=combinations.canopy_level.list.co2[1, i],
                                   y_col= combinations.canopy_level.list.co2[2, i],
                                   site= site,
                                   var1= combinations.canopy_level.list.co2[1, i],
                                   var2=combinations.canopy_level.list.co2[2, i])%>% 
          mutate( gas = "CO2")
        
        Linear_CanopyLevel_Stats <- rbind(Linear_CanopyLevel_Stats, new.data)
      }
      
      
    }
    
    # H2O : ####
    df_canopy_H2O <- df_canopy %>% filter( gas == "H2O")
    
    if(df_canopy_H2O$levels %>% unique %>% length > 1){
      
      df_canopy_level_wide <- df_canopy_H2O %>% select(levels, time.rounded, FG_mean ) %>% 
        pivot_wider(names_from = levels, values_from = c(FG_mean)) %>% as.data.frame() 
      
      canopy_level.list.h2o <- df_canopy_H2O$levels %>% unique
      
      combinations.canopy_level.list.h2o <- combn(canopy_level.list.h2o,2)
      
      for(i in 1:ncol(combinations.canopy_level.list.h2o)){
        print( paste( "Working on combination", combinations.canopy_level.list.h2o[1, i], combinations.canopy_level.list.h2o[2, i], sep = " "))
        
        new.data <- linear_terms(  df = df_canopy_level_wide,
                                   x_col= combinations.canopy_level.list.h2o[1, i],
                                   y_col= combinations.canopy_level.list.h2o[2, i],
                                   site = site,
                                   var1= combinations.canopy_level.list.h2o[1, i],
                                   var2=combinations.canopy_level.list.h2o[2, i])%>% 
          mutate( gas = "H2O")
        
        Linear_CanopyLevel_Stats <- rbind(Linear_CanopyLevel_Stats, new.data)
      }
      
    }
    
  }
  
  return(  Linear_CanopyLevel_Stats)
}
validation <- function( index.df){
  
  site.list <- SITE_DATA_FILTERED %>% names()
  Linear_Stats <- data.frame()
  
  for( site in site.list[-c(31, 42)]){
    
    print(site)
    
    df <-SITE_DATA_FILTERED[[site]] %>% filter(gas != "CH4") %>% 
      mutate(month = format(timeEndA.local,'%m') %>% as.numeric,
             season = case_when(
               month %in% c(12, 1, 2) ~ "Winter",
               month %in% c(3, 4, 5) ~ "Spring",
               month %in% c(6, 7, 8) ~ "Summer",
               TRUE ~ "Autumn"),
             hour = format(timeEndA.local,'%H'),
             count= case_when( is.na(FG_mean) == FALSE ~ 1,
                               TRUE ~ 0)) %>% distinct
    
    
    canopy.sub <- index.df %>% select( Site, dLevelsAminusB, gas, Approach, Good.CCC.EC.FG) %>% filter( Site == site)
  
    df_canopy <- df %>% left_join(canopy.sub , by=c("dLevelsAminusB", "gas", "Approach" ), 
                                  relationship = "many-to-many") %>% 
      mutate( time.rounded = timeEndA.local %>% round_date( unit = "30 minutes") ) %>% 
      filter( Good.CCC.EC.FG  == '1') %>%
      reframe( .by= c( gas, time.rounded), 
               EC_mean = mean(EC_mean, na.rm=T), 
               FG_mean = mean(FG_mean, na.rm=T))
    
    
    # CO2 : ####
    df_canopy_CO2 <- df_canopy %>% filter( gas == "CO2") 
    
           new.data <- linear_terms2(  df = df_canopy_CO2,
                                   x_col= 'FG_mean',
                                   y_col= 'EC_mean',
                                   site= site)%>% 
          mutate( gas = "CO2")
        
        Linear_Stats <- rbind(Linear_Stats, new.data)
    
    # H2O : ####
    df_canopy_H2O <- df_canopy %>% filter( gas == "H2O")
   
        new.data <- linear_terms2(   df = df_canopy_H2O,
                                    x_col= 'FG_mean',
                                    y_col= 'EC_mean',
                                    site= site)%>% 
          mutate( gas = "H2O")
        
        Linear_Stats <- rbind(Linear_Stats, new.data)
}
  
  return(  Linear_Stats)
}
 
