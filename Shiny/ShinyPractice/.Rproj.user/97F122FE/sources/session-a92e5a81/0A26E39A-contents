#LANDSCAPE CONNECTIVITY LAYER
#Last edited 3/3 by Catalina

# Load libraries
library(tidyverse)
library(sf)

# Function to calculate connectivity score ----------------------
connectivity <- function(l_connect, i1, c1){
  #create a df to house connectivity scores
  df <- data.frame(matrix(ncol = 2, nrow = 20)) 
  colnames(df) <- c("ivalue", "cvalue")
  #insert starting values from project site
  df[1,1] <- i1 
  df[1,2] <- c1
  
  #loop for calculating upstream path connectivity score
  for (x in 1:nrow(df)) {
  line <- l_connect %>% 
    filter(j == df[x,1]) %>% 
    mutate(c = lngth_m*bi_order)
  
  if(nrow(line) == 0){ #this will break the loop if you don't get a matching i and j value (when you hit the furthest upstream segment)
    break}
  
  line <- line %>%  #this selects the path with the lower (better) connectivity score if you have multiple upstream paths
    filter(c == min(c))
  
  df[x+1,1] <- line$i
  df[x+1,2] <- line$c
  }
  
  df <- na.omit(df) #remove nas
  score <- 1 / sum(df$cvalue) #calculate the connectivity score
  score
}











# OLD CODE USED FOR FUNCTION DEVELOPMENT --------------------------

# Load Data -----------
l_connect <- read_sf("data/Landscape_Connectivity")
site <- read_sf("data/CedarGroveMitigation")
st_crs(site)
l_connect <- st_transform(l_connect, crs = st_crs(site))
site <- st_transform(site, crs = st_crs(l_connect))

#Prepping the project site -------------
#Produce a line between the shortest distance between the site and the connectivity line
#requires unionizing the landscape connectivity line
line <- st_nearest_points(site, st_union(l_connect))
#Turn the line into a point at the intersection
point <- st_intersection(l_connect, line) %>%
  st_as_sf()
#check that it intersects
st_intersects(point, l_connect, sparse = F)
#split the line by the intersect
startingline <- st_collection_extract(st_split(l_connect, point), "LINESTRING") %>%
  filter(fid == point$fid) %>% #This filters for the line segement that we sliced above
  slice(2) #this drops one of the segments... but I'm not sure if this will work 100% of the time?
#------------
#Extracting the data needed to run the function
#first line connectivity score
c1 <- startingline$lngth_m * startingline$bi_order

#ID the first i value
i1 <- startingline$i


# Test Function ----------------------
connectivity(l_connect, i1, c1)

# Plot check --------------------
#ggplot() +
#  geom_sf(data = point)+
#  geom_sf(data = site)+
#  geom_sf(data = startingline)




