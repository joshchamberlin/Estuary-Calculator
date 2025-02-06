#Catalina
# This script is for practicing geocomputation. 
#Last edited: 2/5

library(tidyverse)
library(sf)
library(leaflet)


# Load Data -------------------------------
SnoDelta <- read_sf("data/SnoDelta.shp")

st_crs(SnoDelta) #check CRS


restoration <- read_sf("data/ProjectFootprints_SNO.shp")

outlets <- read_sf("data/tc_dist_outlets_SNO.shp")

current_hab <- read_sf("data/update_SNO.shp")

# Manipulate Data -------------------------
data_map <- st_transform(SnoDelta, crs = '+proj=longlat +datum=WGS84')


# Create Map -------------------------------

map <-leaflet() %>% 
  addTiles(group = "Street") %>% #basemap
  addProviderTiles(providers$Esri.WorldImagery,
                   group = "Satellite") %>% #satelite base map
  setView(lng =-122.214, lat = 48.024, zoom = 10) %>% 
  addPolygons(data = data_map,
              popup = "Snohomish Delta", group = "Snohomish Delta") %>% 
  addLayersControl(baseGroups = c("Satelite", "Street"),
                   overlayGroups = c("Snohomish Delta"),
                   options = layersControlOptions(collapsed = F)) %>% 
  addScaleBar()


map
