library(here)
library(here)
library(dplyr)
library(tidyverse)
library(here)
library(dplyr)
library(tidyverse)
library(MazamaCoreUtils)
library(rnaturalearth)
library(terra)
library(leaflet)
#install.packages ("leaflet")
sf_use_s2(FALSE)
#-----
#Reading in Data
wcr <- read.csv(here("data", "WCR.csv"))
wcr <- read.csv(here("WCRpermitBiOp_allregns_all_years__7Jan2022.xlsx - WCRpermitBiOp_allregns_all_year.csv"))
#-----
#Filtering Data 
wcr_rev <- wcr %>% 
  filter(PermitStatus == "Issued")
filter(PermitStatus == "Issued") #Issued Permits
wcr_rev1 <- wcr_rev %>% 
  filter(DateIssued >"2012-01-01")
filter(DateIssued >"2012-01-01") #Permits from last 10 years
wcr_rev2 <- wcr_rev1 %>% 
  filter(DateExpired >= Sys.Date()) #DateField >= Sys.Date() puts it to the date of the system
filter(DateExpired >= Sys.Date()) #DateField >= Sys.Date() puts it to the date of the system #Current Permits
wcr_rev3 <- wcr_rev2 %>% 
  filter(ResultCode == c("NMFS 10a1A Salmon","4d", "NMFS BiOp DTA", "Tribal 4d"))
filter(ResultCode == c("NMFS 10a1A Salmon","4d", "NMFS BiOp DTA", "Tribal 4d")) #Filtering for relevant permit type
#-----
#Recoding Data
wcr_rev4 <- wcr_rev3 %>%
  mutate(LifeStage = recode(LifeStage,
                            "Smolt" = "Juvenile",
                            "Fry" = "Juvenile",
                            "Larvae" = "Juvenile",
                            "Subadult" = "Adult"))
"Subadult" = "Adult")) # 

wcr_rev5 <- wcr_rev4 %>% 
  mutate(CommonName = recode(CommonName,
                             @@ -36,29 +45,116 @@ wcr_rev5 <- wcr_rev4 %>%
                               "Rockfish, Bocaccio" = "bocaccio",
                             "Rockfish, Yelloweye" = "yelloweye rockfish"))

#------
# Splitting up datasets 
adults <- wcr_rev5 %>% 
  filter(LifeStage == "Adult")
filter(LifeStage == "Adult") #Adult Dataset
juvenile <- wcr_rev5 %>% 
  filter(LifeStage == "Juvenile")
filter(LifeStage == "Juvenile") # Juvenile Dataset
#------
#Splitting species by run
unique(wcr_spatial$Population)
wcr_spatial %>% 
  group_by(Population) %>% 
  mutate( Sp_Run = CommonName, by = Population)
#------
#Practice mapping with leaflet

states <- geojsonio::geojson_read("https://rstudio.github.io/leaflet/json/us-states.geojson", what = "sp")
m <- leaflet(states) %>%
  setView(-96, 37.8, 4) %>%
  addProviderTiles("MapBox", options = providerTileOptions(
    id = "mapbox.light",
    accessToken = Sys.getenv('MAPBOX_ACCESS_TOKEN')))
bins <- c(0, 10, 20, 50, 100, 200, 500, 1000, Inf)
pal <- colorBin("YlOrRd", domain = states$density, bins = bins)


m %>% addPolygons(fillColor = ~pal(density),
                  weight = 2,
                  opacity = 1,
                  color = "white",
                  dashArray = "3",
                  fillOpacity = 0.7,
                  highlightOptions = highlightOptions(
                    weight = 5,
                    color = "#666",
                    dashArray = "",
                    fillOpacity = 0.7,
                    bringToFront = TRUE))

outline <- quakes[chull(quakes$long, quakes$lat),]

map <- leaflet(quakes) %>%
  # Base groups
  addTiles(group = "OSM (default)") %>%
  addProviderTiles(providers$Stamen.Toner, group = "Toner") %>%
  addProviderTiles(providers$Stamen.TonerLite, group = "Toner Lite") %>%
  # Overlay groups
  addCircles(~long, ~lat, ~10^mag/5, stroke = F, group = "Quakes") %>%
  addPolygons(data = outline, lng = ~long, lat = ~lat,
              fill = F, weight = 2, color = "#FFFFCC", group = "Outline") %>%
  # Layers control
  addLayersControl(
    baseGroups = c("OSM (default)", "Toner", "Toner Lite"),
    overlayGroups = c("Quakes", "Outline"),
    options = layersControlOptions(collapsed = FALSE)
  )
map
#------
#Actual Mapping with leaflet
#outline <- wcr_spatial[chull(wcr_spatial$long, wcr_spatial$lat),]
# map1 <- leaflet(wcr_spatial) %>%
#   # Base groups
#   addTiles(group = "PermitStatus(default)") %>%
#   addProviderTiles(providers$CommonName, group = "Species") %>%
#   addProviderTiles(providers$ResultCode, group = "Permit Type") %>%
#   # Overlay groups
#   addCircles(~longitude, ~latitude, ~10^mag/5, stroke = F, group = "Species") %>%
#   addPolygons(data = outline, lng = ~long, lat = ~lat,
#               fill = F, weight = 2, color = "#FFFFCC", group = "Outline") %>%
#   # Layers control
#   addLayersControl(
#     baseGroups = c("PermitStatus(default)", "Species", "Permit Type"),
#     overlayGroups = c("Species", "Permit Type"),
#     options = layersControlOptions(collapsed = FALSE)
#   )
#^^ WIP DOES NOT WORK

#Following code shows leaflet map of active permits WIP !!!
wcr_leaf <- leaflet(wcr_spatial) %>%
  addTiles(group = "PermitStatus(default)") %>% 
  setView(-96, 37.8, 4) %>%
  addProviderTiles("MapBox", options = providerTileOptions(
    id = "mapbox.light",
    accessToken = Sys.getenv('MAPBOX_ACCESS_TOKEN')))
wcr_leaf %>% addPolygons()
#Density of active permits reflected by darker polygons
#------
#Mapping Ind Mort

st_layers(dsn = here("data//WBD_National_GDB", "WBD_National_GDB.gdb"))
st_layers(dsn = here("data", "WBD_National_GDB", "WBD_National_GDB.gdb"))

wbd.hucs <- read_sf(dsn = here("data/WBD_National_GDB", "WBD_National_GDB.gdb"), layer = "WBDHU8")
wbd.hucs <- read_sf(dsn = here("data", "WBD_National_GDB", "WBD_National_GDB.gdb"), layer = "WBDHU8")

wbd.hucs$huc8 <- as.double(wbd.hucs$huc8)

state.bound <- read_sf(here("data/cb_2018_us_state_20m", "cb_2018_us_state_20m.shp"))
state.bound <- read_sf(here("data", "cb_2018_us_state_20m", "cb_2018_us_state_20m.shp"))

wcr.bound <- state.bound %>%
  filter(NAME == "Washington" | NAME == "Oregon" |
           NAME == "California" | NAME == "Idaho")


wcr_spatial <- right_join(x = wbd.hucs, y = adults, by = c("huc8" = "HUCNumber")) #always have to pick adults or juveniles, never both on same page

wcr_spatialJ <-right_join(x = wbd.hucs, y = juvenile, by = c("huc8" = "HUCNumber"))
#unique(wcr_spatial$CommonName)





# all adults 
#sockeye
spatial_sockeye <- wcr_spatial %>% 
  filter(CommonName == "sockeye salmon") %>%
  filter(huc8 != c(99999999, NA))

sockeye_mort <- spatial_sockeye %>%
  group_by(huc8) %>%
  summarize(sum = sum(IndMort, na.rm = T))

WA.hucs <- wbd.hucs %>%
  filter(states %in% "WA")
hucs <- wbd.hucs %>%
  filter(states %in% c("WA", "ID", "CA", "OR", "CA,OR"))

sockeye_plot <- ggplot() +
  geom_sf(data = wcr.bound, fill = "ivory") +
  geom_sf(data = sockeye_mort, aes(fill = sum)) +
  theme_void()
ggsave(device = "pdf", here("output", "sockeye_plot.pdf"))




#coho
spatial_coho <- wcr_spatial %>% 
  filter(CommonName == "coho salmon") %>%
  filter(huc8 != c(99999999, NA))

coho_mort <- spatial_coho %>%
  group_by(huc8) %>%
  summarize(sum = sum(IndMort, na.rm = T))

coho_plot <- ggplot() +
  geom_sf(data = wcr.bound, fill = "ivory") +
  geom_sf(data = coho_mort, aes(fill = sum)) +
  theme_void()
ggsave(device = "pdf", here("output", "coho_plot.pdf"))


#steelhead
spatial_steelhead <- wcr_spatial %>% 
  filter(CommonName == "steelhead") %>%
  filter(huc8 != c(99999999, NA))

steelhead_mort <- spatial_steelhead %>%
  group_by(huc8) %>%
  summarize(sum = sum(IndMort, na.rm = T))

steelhead_plot <- ggplot() +
  geom_sf(data = wcr.bound, fill = "ivory") +
  geom_sf(data = steelhead_mort, aes(fill = sum)) +
  theme_void()
ggsave(device = "pdf", here("output", "steelhead_plot.pdf"))

#chinook
spatial_chinook <- wcr_spatial %>% 
  filter(CommonName == "Chinook salmon") %>%
  filter(huc8 != c(99999999, NA))

chinook_mort <- spatial_chinook %>%
  group_by(huc8) %>%
  summarize(sum = sum(IndMort, na.rm = T))

chinook_plot <- ggplot() +
  geom_sf(data = wcr.bound, fill = "ivory") +
  geom_sf(data = chinook_mort, aes(fill = sum)) +
  theme_void()
ggsave(device = "pdf", here("output", "chinook_plot.pdf"))

#canary
spatial_canary <- wcr_spatial %>% 
  filter(CommonName == "canary rockfish") %>%
  filter(huc8 != c(99999999, NA))

canary_mort <- spatial_canary %>%
  group_by(huc8) %>%
  summarize(sum = sum(IndMort, na.rm = T))

canary_plot <- ggplot() +
  geom_sf(data = wcr.bound, fill = "ivory") +
  geom_sf(data = canary_mort, aes(fill = sum)) +
  theme_void()
ggsave(device = "pdf", here("output", "canary_plot.pdf"))

#boccacio
spatial_bocc <- wcr_spatial %>% 
  filter(CommonName == "bocaccio") %>%
  filter(huc8 != c(99999999, NA))

bocc_mort <- spatial_bocc %>%
  group_by(huc8) %>%
  summarize(sum = sum(IndMort, na.rm = T))

bocc_plot <- ggplot() +
  geom_sf(data = wcr.bound, fill = "ivory") +
  geom_sf(data = bocc_mort, aes(fill = sum)) +
  theme_void()
ggsave(device = "pdf", here("output", "bocc_plot.pdf"))

#yelloweye
spatial_yelloweye <- wcr_spatial %>% 
  filter(CommonName == "yelloweye rockfish") 
#filter(huc8 != c(99999999, NA))

yelloweye_mort <- spatial_yelloweye %>%
  group_by(huc8) %>%
  summarize(sum = sum(IndMort, na.rm = T))

WA.hucs <- wbd.hucs %>%
  filter(states %in% "WA")

yelloweye_plot <- ggplot() +
  geom_sf(data = wcr.bound, fill = "ivory") +
  geom_sf(data = yelloweye_mort, aes(fill = sum)) +
  theme_void()
ggsave(device = "pdf", here("output", "yelloweye_plot.pdf"))

#green sturgeon
spatial_sturgeon <- wcr_spatial %>% 
  filter(CommonName == "green sturgeon") %>%
  filter(huc8 != c(99999999, NA))

sturgeon_mort <- spatial_sturgeon %>%
  group_by(huc8) %>%
  summarize(sum = sum(IndMort, na.rm = T))

WA.hucs <- wbd.hucs %>%
  filter(states %in% "WA")

sturgeon_plot <- ggplot() +
  geom_sf(data = wcr.bound, fill = "ivory") +
  geom_sf(data = sturgeon_mort, aes(fill = sum)) +
  theme_void()
ggsave(device = "pdf", here("output", "sturegon_plot.pdf"))

#eulachon
spatial_eulachon <- wcr_spatial %>% 
  filter(CommonName == "eulachon") %>%
  filter(huc8 != c(99999999, NA))

eulachon_mort <- spatial_eulachon %>%
  group_by(huc8) %>%
  summarize(sum = sum(IndMort, na.rm = T))

WA.hucs <- wbd.hucs %>%
  filter(states %in% "WA")

eulachon_plot <- ggplot() +
  geom_sf(data = wcr.bound, fill = "ivory") +
  geom_sf(data = eulachon_mort, aes(fill = sum)) +
  theme_void()
ggsave(device = "pdf", here("output", "eulachon_plot.pdf"))



#chum
spatial_chum <- wcr_spatial %>% 
  filter(CommonName == "chum salmon") %>%
  filter(huc8 != c(99999999, NA))

chum_mort <- spatial_chum %>%
  group_by(huc8) %>%
  summarize(sum = sum(IndMort, na.rm = T))

chum_plot <- ggplot() +
  geom_sf(data = wcr.bound, fill = "ivory") +
  geom_sf(data = chum_mort, aes(fill = sum)) +
  theme_void()
ggsave(device = "pdf", here("output", "chum_plot.pdf"))


#------
#Mapping Juveniles 

#chinook j
spatial_chinookJ <- wcr_spatialJ %>% 
  filter(CommonName == "Chinook salmon") %>%
  filter(huc8 != c(99999999, NA))

chinookJ_mort <- spatial_chinookJ %>%
  group_by(huc8) %>%
  summarize(sum = sum(IndMort, na.rm = T))

chinookJ_plot <- ggplot() +
  geom_sf(data = wcr.bound, fill = "ivory") +
  geom_sf(data = chinookJ_mort, aes(fill = sum)) +
  theme_void()
ggsave(device = "pdf", here("output", "chinookJ_plot.pdf"))


#get_root_filenum <- function(file_list){
#return(unique(str_extract(file_list, "[^- | //s]+")))
#}

#us state map with different geometries of the states 
#huc data

#------
# Name points / Geom Points

wa<- data.frame(long = -120.7401, lat = 47.7511, city = "Washington")

ca <- data.frame(long = -119.4179, lat = 36.7783, city = "California")

id <- data.frame(long = -114.7420, lat = 44.0682, city = "Idaho")

or <- data.frame(long =-120.5542, lat = 43.8041, city = "Oregon")
