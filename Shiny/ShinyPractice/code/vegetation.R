#VEGETATION LAYER
#Last edited 3/24 by Catalina
#NOTES: 

# Load libraries
library(tidyverse)
library(sf)

#Load Data ----------------
#site
site <- read_sf("data/CedarGroveMitigation")

#Historic Veg
Hveg <- read_sf("data/Historic_Veg")

#Tidal Connectivity
Tconnect <- read_sf("data/Connectivity")

#Match CRS
st_crs(Hveg)
st_crs(Tconnect)
st_crs(site)

Hveg <- st_transform(Hveg, crs = st_crs(site)) %>% 
  st_make_valid()
Tconnect <- st_transform(Tconnect, crs = st_crs(site)) %>% 
  st_make_valid()

# QA / QC ---------------------------
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

#remove shore from historic veg
Hveg <- Hveg %>% 
  filter(Veg != "Shore")

#Calculate Habitat Areas -------------------
Hveg_area <- Hveg %>% 
  mutate(area = st_area(geometry)) %>% 
  group_by(Veg) %>% 
  summarise(Area = sum(area)) %>% 
  st_drop_geometry() %>% 
  mutate(Area = set_units(Area, km^2))

#Match current with historic habitat
wetlands <- st_intersection(wetlands, Hveg)

Cveg_area <- wetlands %>% 
  mutate(area = st_area(geometry)) %>% 
  group_by(Veg) %>% 
  summarise(Area = sum(area)) %>% 
  st_drop_geometry() %>% 
  mutate(Area = set_units(Area, km^2))


#Plot change in habitat

habitat_change_plot <- ggplot()+
  geom_col(data = Hveg_area, aes(x = Veg, y = Area, color = "Historic"), fill = "lightblue")+
  geom_col(data = Cveg_area, aes(x = Veg, y = Area, color = "Current"), fill = "darkblue") +
  theme_classic() +
  scale_x_discrete(expand = c(0,0))+
  scale_y_units(expand = c(0,0), breaks = c(0,5,10,15,20,25,30)) +
  labs(x = "Habitat Type", title = "Change in Habitat from Historic to Present")+
  scale_color_manual(name = "Period",
                     breaks = c("Historic", "Current"),
                     values = c("Historic" = "lightblue", "Current" = "darkblue"))
  


