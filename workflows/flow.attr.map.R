# View site attributes and create a site visualization:
library(tidyverse)
library(sf)
library(AOI)

metadata <- read.csv('/Volumes/MaloneLab/Research/FluxGradient/Ameriflux_NEON field-sites.csv') # has a list of all 

site.att.sf <- st_as_sf(x = metadata,                         
               coords = c("Longitude..degrees.", "Latitude..degrees."),
               crs = 4326)

site.att.sf$geometry %>% plot

aoi.usa <- aoi_get(country = c('PR', 'USA'))

ggplot() + geom_sf(data = aoi.usa, fill='white', color="navy", lwd=1) + geom_sf(data = site.att.sf, size=2, color = 'goldenrod') + theme_bw()

# Attribute and canopy file:
canopy <- read.csv(file.path(paste(localdir, "canopy_commbined.csv", sep="/"))) %>% distinct
metadata %>% names

metadata$Vegetation.Abbreviation..IGBP. %>% as.factor %>% summary

# Get climate classes and eco-domains for the location
