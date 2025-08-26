# Creates the list of dataframes used by One2One, Diel, and Cparms. This also compiles the filter reports for all sites.  

SITES_MBR_9min_FILTER <- list()
SITES_AE_9min_FILTER <- list()
SITES_WP_9min_FILTER <- list()
filter.report.CO2 <- data.frame()
filter.report.stability.CO2 <- data.frame()

filter.report.H2O <- data.frame()
filter.report.stability.H2O <- data.frame()
# Site List DF and filter report
for( site in site.list){
  
  print( site)
  
  # Load the files:
  localdir.site <- paste(localdir,"/", site, sep = "")
  
  files <- paste(site, "_FILTER.Rdata", sep = "")
  
  load(paste(localdir.site, "/", files, sep=""))
  
  
  SITES_MBR_9min_FILTER[[site]] <- MBR_9min_FILTER 
  SITES_AE_9min_FILTER[[site]] <- AE_9min_FILTER
  SITES_WP_9min_FILTER[[site]] <- WP_9min_FILTER
  
  files.CO2 <- paste(site, "_9min.report.CO2.csv", sep = "")
  
  report.CO2 <- read.csv( paste( localdir.site,"/", files.CO2, sep="" ))

  filter.report.CO2 <- rbind(filter.report.CO2,  report.CO2 )
  
  files.H2O <- paste(site, "_9min.report.H2O.csv", sep = "")
  
  report.H2O <- read.csv( paste( localdir.site,"/", files.H2O, sep="" ))
  
  filter.report.H2O <- rbind(filter.report.H2O,  report.H2O )
  
  message("Done with ", site)
}

for( site in site.list){
  
  print( site)
  
  # Load the files:
  localdir.site <- paste(localdir,"/", site, sep = "")

  files.CO2 <- paste(site, "_9min.report.stability.CO2.csv", sep = "")
  report.CO2 <- read.csv( paste( localdir.site,"/", files.CO2, sep="" ))
  filter.report.stability.CO2 <- rbind(filter.report.stability.CO2,  report.CO2 )
  
  files.H2O <- paste(site, "_9min.report.stability.H2O.csv", sep = "")
  report.H2O <- read.csv( paste( localdir.site,"/", files.H2O, sep="" ))
  filter.report.stability.H2O <- rbind(filter.report.stability.H2O,  report.H2O )
  
  message("Done with ", site)
}