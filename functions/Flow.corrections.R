


# Correction: Y = mx + B ####
val.approach.canopy.rf_model.all %>% names
val.approach.canopy.rf_model.all %>% ggplot( ) + geom_density( aes( x=slope, col = Approach))

# Potential for a methods based correction:

threshold.workflow ='rf_model.1'
val.approach.rf_model.1 <- validation.approach(threshold = threshold.workflow ) %>% mutate(
  var3 = paste(var1, var2, sep="-"))

val.approach.rf_model.1 %>% ggplot() + geom_density( aes( x = slope, col= var3)) + facet_wrap(~gas)
val.approach.rf_model.1 %>% ggplot() + geom_density( aes( x = slope, col= var3))+ facet_wrap(~gas)


val.approach.rf_model.1 %>% ggplot() + geom_density( aes( x = intercept, col= var3))+ facet_wrap(~gas)
val.approach.rf_model.1 %>% ggplot() + geom_density( aes( x = intercept, col= var3))+ facet_wrap(~gas)

val.approach.rf_model.1 %>% reframe( .by=c(Site, var3, gas), 
                                     slope.mean = mean(slope, na.rm=T),
                                     slope.sd = sd(slope, na.rm=T),
                                     intercept.mean = mean(intercept, na.rm=T),
                                     intercept.mean = sd(intercept, na.rm=T)) %>% 
  ggplot() + geom_point( aes( x= intercept.mean, y = Site, col = var3)) + facet_wrap( ~gas)

threshold.workflow ='CCC.0.5'
val.canopy.CCC.0.5 <- validation.canopy(threshold = threshold.workflow )
val.approach.CCC.0.5 <- validation.approach(threshold = threshold.workflow )

# Save these validation files: ####

fileSave <- fs::path(localdir,paste0("Validation_SITE_DATA_FILTERED.Rdata"))

save(val.canopy.rf_model.all,val.approach.rf_model.all,val.canopy.rf_model.1,val.approach.rf_model.1 , val.canopy.CCC.0.5 ,  val.approach.CCC.0.5 ,file=fileSave)

googledrive::drive_upload(media = fileSave, overwrite = T, path = drive_url)

# Plots for validation: ####
rf_model %>% summary
val.approach.rf_model.all %>% mutate(Combination = paste(x, y, sep="-")) %>% 
  ggplot() + geom_point( aes( x= CCC, y = Site, col=Combination)) + facet_wrap(~gas + Combination) + 
  geom_vline( xintercept=0.5, col="red") 


val.canopy.rf_model.all %>% mutate(Combination = paste(x, y, sep="-")) %>% 
  ggplot() + geom_boxplot( aes( x= CCC, y = Site)) + facet_wrap(~gas) + 
  geom_vline( xintercept=0.5, col="red") 

# Percentage of levels with a high CCC.


# Need to check how the filter for CCC.GF was applie to ensure it is correct before moving on with this!