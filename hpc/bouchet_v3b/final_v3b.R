# final_v3b.R: last part of flow.evaluation.resume_v3b.R (after the supplement scripts), which the
# 30872741 job did not finish (bus error after REVIEW_R8). All output files of the earlier steps already exist.
suppressPackageStartupMessages(library(tidyverse))
localdir <- '/nfs/roberts/project/pi_sm3466/sm3466/FluxGradient'
run.tag <- '_AA_AW_2021_2026_v3b'
load(file.path(localdir, "NEON_GradientFlux_Data_Filter", paste0("SITES_One2One", run.tag, ".RData")))
sink(file.path(localdir, "logs_v3b", paste0("RESULTS_SECTION1", run.tag, ".txt")), split = TRUE)
# Results Section 1: ####
SITES_One2One$CCC %>% summary


SITES_One2One %>% reframe(.by=c(gas, Approach), R2 = mean(R2), CCC = mean(CCC))
SITES_One2One %>% reframe(.by=c(gas, Canopy_L1), R2 = mean(R2), CCC = mean(CCC))
SITES_One2One %>% filter(CCC >= 0.5) %>% reframe(.by=c(gas, Canopy_L1), R2 = mean(R2), CCC = mean(CCC))


CO2.sites <- SITES_One2One %>% filter(CCC >= 0.5, gas == "CO2") 
H2O.sites <- SITES_One2One %>% filter(CCC >= 0.5, gas == "H2O") 

RSHP.CO2 <- CO2.sites$Site %>% unique
RSHP.H2O <- H2O.sites$Site %>% unique

# Number of Sites with RSHPs: 
length(RSHP.CO2)
length(RSHP.H2O)

# Both:
RSHP.H2O  %in% RSHP.CO2 

SITES_One2One <- SITES_One2One %>% mutate(Counter = 1)

# Total counts
SITES_One2One.summary.gas <- SITES_One2One %>% 
  reframe( .by=gas, Total = sum(Counter)) %>% 
  left_join(by ="gas", 
            SITES_One2One %>% filter(CCC > 0.5) %>% 
              reframe( .by=gas, Count.RSHP = sum(Counter))) %>% 
  mutate(percent =Count.RSHP/Total *100 )

SITES_One2One.summary.gas.site <- SITES_One2One %>% filter(CCC > 0.5) %>% 
  reframe( .by=c(gas,Site), Count.RSHP = sum(Counter)) %>% reframe(
    .by=c(gas), mean.Count.RSHP = mean( Count.RSHP ),
    sd.Count.RSHP = sd( Count.RSHP ))


SITES_One2One.summary.approach <-SITES_One2One %>% filter( CCC > 0.5) %>% 
  reframe( .by= gas, Total = sum(Counter)) %>% 
  left_join(by =c('gas'), 
            SITES_One2One %>% filter(CCC > 0.5) %>% 
              reframe( .by=c(gas, Approach), Count.RSHP = sum(Counter))) %>% 
  mutate(percent =Count.RSHP/Total *100 )

SITES_One2One.summary.canopy <-SITES_One2One %>% filter( CCC > 0.5) %>% 
  reframe( .by= gas, Total = sum(Counter)) %>% 
  left_join(by =c('gas'), 
            SITES_One2One %>% filter(CCC > 0.5) %>% 
              reframe( .by=c(gas, Canopy_L1), Count.RSHP = sum(Counter))) %>% 
  mutate(percent =Count.RSHP/Total *100 )

SITES_One2One %>% filter( CCC > 0.5) %>% 
  reframe( .by= c(gas, Canopy_L1), Mean_CCC = mean(CCC),
           Mean_R2 = mean(R2) )


# Section....
# Next go to flow.CCC.VIX for results
# Next go to flow.flux.counts for results
# Next go to flow.results.ecotype for results
# flow.results.summary
# Good Drivers 
load( file=paste0("/nfs/roberts/project/pi_sm3466/sm3466/FluxGradient/NEON_GradientFlux_Ensemble/DIEL_SUMMARY_ENSEMBLE_V1", run.tag, ".RData"))

sink()
