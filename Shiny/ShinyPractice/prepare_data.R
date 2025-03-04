#Catalina
# This script is for practicing geocomputation. It's mostly junk code.
#Last edited: 2/5

library(tidyverse)
library(sf)
library(leaflet)
library(units)
library(terra) #raster data


# Load Data -------------------------------
lidar <-rast("./data/Lidar-20250213T181242Z-001/Lidar/lidar/2009snohomish_river_estuary.gdb",
            lyrs = "snohomishriverestuary_be")

st_bbox(lidar)
st_bbox(cedar_grove)
crs(lidar, describe = T)
st_crs(cedar_grove)

test <- crop(x = lidar, y = SnoDelta)

lidar.df <-as.data.frame(lidar, xy= T)
st_crs(lidar)
lidar.df$snohomishriverestuary_be
ggplot() +
  geom_raster(data = lidar.df, aes(x=x, y=y, fill = snohomishriverestuary_be))

#function to read in data and transform into a consistent crs
read_data <- function(folder) {
  read_sf(folder) %>% 
    st_transform(crs = '+proj=longlat +datum=WGS84')
}

#Polygon of the Snohomish Delta
SnoDelta <- read_data("data/SnoDelta")

#Cedar Grove Mitigation project footprint
cedar_grove <- read_data("data/CedarGroveMitigation") #%>% 
  st_transform(crs = 2286)
st_crs(cedar_grove)
  
  read_data("data/CedarGroveMitigation")

#Current Habitat Conditions
outlets <- read_data("data/current_outlets_SNO")
current_hab <- read_data("data/current_hab_SNO")

#Historic Habitat
Hveg <- read_data("data/Historic_Veg")

#Tidal Connectivity
Tconnect <- read_data("data/Connectivity")



# Manipulate Data -------------------------

#calculate the current habitat area

#Fix value for new mitigation bank
Tconnect$Feat_Tidal[Tconnect$fid_ == 3086] <- "Unrestricted"

#Remove the two polygons that were mislabeled
Tconnect$Feat_Tidal[Tconnect$fid_ == 2965] <- "Mislabeled"
Tconnect$Feat_Tidal[Tconnect$fid_ == 3218] <- "Mislabeled"

#simplify the data frame down to just unrestricted and partially restricted wetlands
wetlands <- Tconnect %>% 
  select(fid_, Feat_Tidal, TidalFeat_) %>% 
  filter(Feat_Tidal == "Unrestricted" | Feat_Tidal == "Partial") %>% 
  filter(TidalFeat_ == "Wetland") 

#Troubleshooting issues with spherical geometries
st_is_valid(wetlands)
wetlands <- st_make_valid(wetlands)

st_is_valid(Hveg)
Hveg <- st_make_valid(Hveg)

#Simplify data
Hveg <- Hveg %>% 
  distinct(Veg, geometry)

#Clipping wetlands to the Hveg layer  
test <- st_intersection(wetlands, Hveg)
veg <- st_intersection(cedar_grove, Hveg)
#Map to look at data
leaflet() %>%
  addProviderTiles(providers$Esri.WorldImagery,
                   group = "Satellite") %>% 
  setView(lng =-122.214, lat = 48.024, zoom = 10) %>% 
  
  #User uploaded footprint
  addPolygons(data = test)

#Notes on Map:
  #There are some wierd polygons like the wetlands at the mouth of the river


#Calculating Habitat Area
#Current
current_area <- test %>%
  group_by(Veg) %>% 
  mutate(area = st_area(geometry)) %>% 
  mutate(area = sum(area)) %>% 
  distinct(Veg, area) %>% 
  mutate(time = "present")

#Historic
historic_area <- Hveg %>% 
  group_by(Veg) %>% 
  mutate(area = st_area(geometry)) %>% 
  mutate(area = sum(area)) %>% 
  distinct(Veg, area) %>% 
  mutate(time = "historic")

hab_area<- full_join(current_area, historic_area) %>% 
  filter(Veg != "Shore")

df <- tibble(
  Veg = "Emergent",
  area = 40,
  time = "restoration")
hab_area[nrow(hab_area)+1, 1] = "Emergent"
hab_area[nrow(hab_area), 2] = st_area(cedar_grove)
hab_area[nrow(hab_area), 3] = "Restoration"

hab_area$acres <- set_units(hab_area$area, "acres")

ggplot(hab_area, aes(fill = time, y = area, x = Veg))+
  geom_bar(position = 'stack', stat = 'identity')+
  theme_classic()

sum(historic_area$area)

Present <- sum(st_area(test))

Historic <- sum(st_area(Hveg))

Present/Historic



