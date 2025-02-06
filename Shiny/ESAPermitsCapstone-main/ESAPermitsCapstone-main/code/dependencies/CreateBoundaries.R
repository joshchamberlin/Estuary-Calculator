# =================================================
# Rory Spurr
# Script #3 to be run before the Shiny app

# Summary:
# Creates large polygons that describe all of the 
# different HUC8's where you could potentially find a given ESU or DPS
# =================================================


# =================================================
# Filter the species we want from data set describing where 
# these species can be found
ESUBasins <- ESUBasins %>%
  filter(Species %in% c("Eulachon", "Salmon, Chinook", "Salmon, chum",
                     "Salmon, coho", "Salmon, sockeye", "Steelhead",
                     "Sturgeon, green")) %>%
  mutate(HUC8 = as.double(HUC8)) %>% # Did not read in as correct data type - fixed here
  mutate(Species = recode(Species,
                          "Salmon, coho" = "coho salmon",
                          "Steelhead" = "steelhead", 
                          "Eulachon" = "eulachon",
                          "Salmon, Chinook" = "Chinook salmon",
                          "Salmon, chum" = "chum salmon",
                          "Salmon, sockeye" = "sockeye salmon",
                          "Sturgeon, green" = "green sturgeon",
                          "Rockfish, Canary" = "canary rockfish",
                          "Rockfish, Bocaccio" = "bocaccio",
                          "Rockfish, Yelloweye" = "yelloweye rockfish")) # naming consistency with permit data
  

ESUBasinsSpatial <- left_join(ESUBasins, wbd.hucs, by = c("HUC8" = "huc8")) %>% # join with spatial data
  st_as_sf() %>% # converts to sf object
  mutate(DPS = paste(`Population/Stock`, Species, sep = " ")) # create ESU/DPS column (same unique values 
                                                              # as permit data)

DPSs <- unique(ESUBasinsSpatial$DPS) # says DPS here, but also encompasses ESUs
nDPS <- length(DPSs)
sf_use_s2(use_s2 = F) # have to shut this off so loop works (due to some vertices being used in multiple shapes)

temp <- ESUBasinsSpatial %>% filter(DPS == DPSs[1])
temp <- st_union(temp)
esuBound <- temp

# loop to combine all polygons for each ESU/DPS -> creates ESU boundary polygons
for (i in 2:nDPS){
  temp <- ESUBasinsSpatial %>% filter(DPS == DPSs[i])
  temp <- st_union(temp)
  esuBound <- rbind(esuBound, temp)
  rm(temp)
} 


# This chunk re-formats, provides informative column names 
# and sets up spatial data
esuBound <- cbind(DPSs, esuBound)
esuBound <- as.data.frame(esuBound)
names(esuBound) <- c("DPS", "geometry")
esuBound$geometry <- st_as_sfc(esuBound$geometry)
esuBound <- st_as_sf(esuBound)
st_crs(esuBound) <- 4326 # CRS for playing nice with Leaflet


