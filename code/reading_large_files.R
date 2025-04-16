# Reading Large Files into Environment
# Last Edited: 4/15 Catalina
# Notes: 

# Install arcgis metapackage
#install.packages(
#  "arcgis",
#  repos = c("https://r-arcgis.r-universe.dev", "https://cloud.r-project.org")
#)

# References
# This link is useful https://developers.arcgis.com/r-bridge/layers/read-layers/

# Load Libraries
library(arcgis)
library(tidyverse)

# Practice

test <- arc_open("https://services.arcgis.com/P3ePLMYs2RVChkJx/ArcGIS/rest/services/USA_Counties_Generalized_Boundaries/FeatureServer/0")

test2 <- arc_select(test)


# Test plot
ggplot() +
  geom_sf(data = test2)

# josh comment  #