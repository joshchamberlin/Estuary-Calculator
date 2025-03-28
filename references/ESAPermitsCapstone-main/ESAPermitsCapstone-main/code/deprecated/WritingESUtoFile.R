# Rory Spurr
# This script contains code that was used to write a shapefile containing
# the ESU boundaries found from NOAA's geodatabase 
# (URL: https://www.fisheries.noaa.gov/resource/map/species-ranges-salmon-and-steelhead-west-coast-region)

# st_layers(dsn = "data/WCR_Salmon_Steelhead_gdb_2015/WCR_Salmon_Steelhead_gdb_2015.gdb") # note that the GDB
# # was removed from GitHub in order to push it
# esuBound <- read_sf(dsn = "data/WCR_Salmon_Steelhead_gdb_2015/WCR_Salmon_Steelhead_gdb_2015.gdb",
#                     layer = "fish")
# 
# esuBound2 <- esuBound %>%
#   select(hydrologic_OBJECTID, DPS, SHAPE) # maybe need to change what 
# # columns we pull based on joining needs
# 
# st_write(esuBound2, dsn = "data/esu_boundaries.shp",
#          delete_layer = T)

# esuBound2 <- read_sf("data/esu_boundaries.shp") # works, but the file is too large, and we really just want 
# intersections with HUC 8's

# test <- esuBound %>% filter(DPS == "Salmon, coho (Oregon Coast ESU)")
# test2 <- st_union(test)

# test2 <- as.data.frame(test2)
# ggplot()+
#   geom_sf(data = test2, fill = "blue") 
#   geom_sf(data = test, fill = "red")

# Now lets write a loop to do this
DPSs <- unique(esuBound$DPS)
nDPS <- length(DPSs)
esuDissolve <- st_sfc(crs = 4269)
sf_use_s2(use_s2 = F)
for (i in 1:nDPS){
  temp <- esuBound %>% filter(DPS == DPSs[i])
  temp <- st_union(temp)
  esuDissolve[i] <- temp[1]
  rm(temp)
}
esuDissolve <- cbind(DPSs, esuDissolve)
names(esuDissolve) <- c("DPS", "geometry")
esuDissolve <- st_sf(esuDissolve, sf_column_name = geometry)
esuDissolve_sf <- st_sf(esuDissolve, agr = "identity", crs = 4269, 
                        sf_column_name = geometry)
