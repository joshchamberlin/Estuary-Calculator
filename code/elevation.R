#Elevation Layer
#Last edited 3/31 by Catalina
#NOTES: I cropped the lidar outside of the app to get it to fit the Github size requirement

#load libraries
library(tidyverse)
library(sf)
library(leaflet)
library(terra)

#TEMPORARY method: load data
#elevation <- rast("data/elevation/cgelevation.tif")
lidar <- rast("data/large_files_ignore/2009snohomish_river_estuary.gdb",
              lyrs = "snohomishriverestuary_be")

footprint <- read_sf("data/CedarGroveMitigation")
footprint <- st_transform(site, crs = st_crs(lidar))

elevation <- crop(lidar, footprint, mask = T)

elevation_df <- as.data.frame(elevation, xy = T)




elevation_df <- as.data.frame(elevation, xy = T)

#rename column
elevation_df <- elevation_df %>% 
  rename_at("snohomishriverestuary_be", ~"bare_earth")

#PERMANENT method: once we figure out how to use large files we should
  #load the lidar for the whole delta and then crop to the project footprint
  #within the app.
# lidar <- rast("")
# cropping will have to happen in the app.
# Arianna suggested that we could crop tiles instead of the whole dataset
  # to save on computation

f_elevation <- function(elevation_df){
  iqr <- IQR(elevation_df$bare_earth)
  quan <- quantile(elevation_df$bare_earth)
  upper <- quan[4] + (1.5*iqr)
  lower <- quan[2]- (1.5* iqr)
  elevation_metric <- elevation_df %>% 
    filter(bare_earth < upper & bare_earth > lower)
  elevation_metric
}

#test function
elevation_metric <- f_elevation(elevation_df)

ggplot() +
  geom_raster(data = elevation_metric, 
              aes(fill = bare_earth, x = x, y = y))
