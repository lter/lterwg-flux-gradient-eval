# View site attributes and create a site visualization:

library(tidyverse)
library(sf)
library(AOI)

metadata <- read.csv('/Volumes/MaloneLab/Research/FluxGradient/Ameriflux_NEON field-sites.csv') # has a list of all 

site.att.sf <- st_as_sf(x = metadata,                         
               coords = c("Longitude..degrees.", "Latitude..degrees."),
               crs = 4326) %>% mutate( Site = Site_Id.NEON)

summary.igbp <-  metadata %>% reframe( .by = Vegetation.Abbreviation..IGBP., towers = length(Vegetation.Abbreviation..IGBP.)) %>% 
  mutate( EcoType = case_when( Vegetation.Abbreviation..IGBP. == 'ENF' ~ 'Forest',
                              Vegetation.Abbreviation..IGBP. == 'DBF' ~ 'Forest', 
                              Vegetation.Abbreviation..IGBP. == 'MF' ~ 'Forest',
                              Vegetation.Abbreviation..IGBP. == 'EBF' ~ 'Forest',
                              Vegetation.Abbreviation..IGBP. == 'SAV' ~ 'Forest',
                              Vegetation.Abbreviation..IGBP. == 'WET' ~ 'Wetland',
                              
                              Vegetation.Abbreviation..IGBP. == 'GRA' ~ 'Grassland',
                              
                              Vegetation.Abbreviation..IGBP. == 'CVM' ~ 'Cropland',
                              Vegetation.Abbreviation..IGBP. == 'CRO' ~ 'Cropland',
                              Vegetation.Abbreviation..IGBP. == 'OSH' ~ 'Shrubland')) %>% 
  reframe( .by = EcoType, towers = sum(towers))

canopy <- read.csv(file.path(paste(localdir, "canopy_commbined.csv", sep="/"))) %>% distinct

canopy.Ht <- canopy %>% reframe( .by= Site,
                         canopyHeight_m = mean(canopyHeight_m))

canopy.Ht$canopyHeight_m %>% range

tower.counts <- ggplot(data=site.att.sf, aes(x=Vegetation.Abbreviation..IGBP.)) +
  geom_bar(stat="count", width=0.7, fill='grey50') + theme_bw() + ylab('Towers') +
  xlab('IGBP') + scale_y_continuous(breaks = scales::breaks_pretty(n = 5)) 

site.att.sf.ht <- site.att.sf %>% left_join(canopy.Ht, by = 'Site')

N.America <- aoi.usa <- aoi_get(country = 'North America')
aoi.usa <- aoi_get(country = c('PR', 'USA'))

map <- ggplot() + geom_sf(data = N.America) + 
  geom_sf(data = site.att.sf.ht,  aes( color = canopyHeight_m )) + theme_bw() + scale_color_gradient2(midpoint=10, low="goldenrod", mid="darkgreen", high="springgreen", name="Canopy Height (m)" ) +theme(legend.position="top")
  
library(ggpubr)
ggarrange(map, tower.counts, labels=c("A", "B"), ncol=1)

metadata$Vegetation.Abbreviation..IGBP. %>% as.factor %>% summary

# Measurement Levels:

canopy %>% names

canopy.ML <- canopy %>% mutate( Canopy_L1 = as.factor(Canopy_L1)) %>% reframe( .by=Canopy_L1,
                                 Count = length(Canopy_L1))

canopy.ML2 <- canopy %>% mutate( Canopy_L2 = as.factor(Canopy_L2)) %>% reframe( .by=Canopy_L2,
                                                                               Levels = length(Canopy_L2),  
                                                                               Towers = length(Site %>% unique),)

canopy.summary <- canopy %>% reframe( .by=Site, canopyHeight_m = mean(canopyHeight_m), EVI.mean= mean(EVI.mean),EVI.sd= mean(EVI.sd), NDVI.mean= mean(NDVI.mean), NDVI.sd= mean(NDVI.sd), LAI.mean= mean(LAI.sd), LAI.sd= mean(LAI.sd), CHM.sd = mean(CHM.sd), SDSDH.mean = mean(Cutoff05.SDSDH), TopRugosity= mean(Cutoff05.TopRugosity))

canopy.plot <- canopy.summary  %>% ggplot() + geom_point( aes(x= canopyHeight_m, y = TopRugosity, col = SDSDH.mean )) + theme_bw()  + ylab("Rugosity") + xlab("Height (m)") +   theme(legend.position = "top")+ labs(color = "Height Variance")

top.plot <- ggarrange(map, labels=c("A", "B"), ncol=2)
bottom.plot <-ggarrange(tower.counts,canopy.plot,  labels=c("C", "D"), ncol=2)

final.plot <- ggarrange(top.plot , bottom.plot, ncol=1)

ggsave("Figures/Map_plot.png", plot = final.plot, width = 7, height = 5, units = "in")

