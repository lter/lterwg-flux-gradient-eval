# Creates a file of the sites and IGBP class to merge with anyfile:
library(sf)

metadata <- read.csv('/Volumes/MaloneLab/Research/FluxGradient/Ameriflux_NEON field-sites.csv') # has a list of all 

site.att.sf <- st_as_sf(x = metadata,                         
                        coords = c("Longitude..degrees.", "Latitude..degrees."),
                        crs = 4326) %>% rename( Site = Site_Id.NEON)

metadata.igbp <- metadata %>% select(Site_Id.NEON, Vegetation.Abbreviation..IGBP.) %>% rename( IGBP = Vegetation.Abbreviation..IGBP., Site = Site_Id.NEON) %>% 
  mutate( EcoType = case_when( IGBP == 'ENF' ~ 'Forest',
                               IGBP == 'DBF' ~ 'Forest', 
                               IGBP == 'MF' ~ 'Forest',
                               IGBP == 'EBF' ~ 'Forest',
                               IGBP == 'SAV' ~ 'Forest',
                               IGBP == 'WET' ~ 'Wetland',
                               
                               IGBP == 'GRA' ~ 'Grassland',
                               
                               IGBP == 'CVM' ~ 'Cropland',
                               IGBP == 'CRO' ~ 'Cropland',
                               IGBP == 'OSH' ~ 'Shrubland'))
  
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