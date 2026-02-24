drive_url <- googledrive::as_id("https://drive.google.com/drive/folders/1Q99CT77DnqMl2mrUtuikcY47BFpckKw3") # The Data 

source(fs::path(DirRepo.eval, 'functions/calc.One2One.CCC_testing.R'))

load(fs::path(localdir,paste0("SITES_MBR_9min_FILTER.Rdata")))
load(fs::path(localdir,paste0("SITES_AE_9min_FILTER.Rdata")))
load(fs::path(localdir,paste0("SITES_WP_9min_FILTER.Rdata")))

# Calculate CCC parameters for CO2
SITES_CCC_CO2 <- ccc.parms.site(MBR.tibble = SITES_MBR_9min_FILTER,
                                AE.tibble = SITES_AE_9min_FILTER,
                                WP.tibble = SITES_WP_9min_FILTER, 
                                gas = "CO2")

# Calculate CCC parameters for H2O
SITES_CCC_H2O <- ccc.parms.site(MBR.tibble = SITES_MBR_9min_FILTER,
                                AE.tibble = SITES_AE_9min_FILTER,
                                WP.tibble = SITES_WP_9min_FILTER, 
                                gas = "H2O")

# Combine all CCC data with gas indicator
SITES_One2One_gas <- SITES_CCC_CO2 %>% 
  mutate(gas = "CO2") %>% 
  rbind(
    SITES_CCC_H2O %>% 
      mutate(gas = "H2O"))

canopy <- read.csv(file.path(paste(localdir, "canopy_commbined.csv", sep="/"))) %>% distinct
canopy$Canopy_L1 %>% unique

# Data Prep:
Highest.CCC <- SITES_One2One_gas %>% reframe(.by= c(Site, gas, Approach), CCC.max = max(CCC, na.rm=T))

SITES_One2One <- SITES_One2One_gas %>% full_join(canopy, by=c("Site", "dLevelsAminusB" ) ) %>% 
  full_join( Highest.CCC,by = c("Site", "gas", "Approach")) %>% 
  mutate(Approach = factor(Approach, levels = c("MBR", "AE", "WP") ),
         RelativeDistB = MeasurementHeight_m_B - CanopyHeight, 
         RelativeDistA = MeasurementHeight_m_A - CanopyHeight, 
         MeasurementDist = MeasurementHeight_m_A - MeasurementHeight_m_A,
         Good.CCC = case_when(  CCC >= 0.5 ~ 1, CCC < 0.5 ~ 0) %>% as.factor)