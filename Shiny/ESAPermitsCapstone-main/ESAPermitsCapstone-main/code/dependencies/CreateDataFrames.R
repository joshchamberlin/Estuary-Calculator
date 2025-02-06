# =================================================
# Rory Spurr
# Script #2 to be run before the Shiny app

# Summary:
# This script creates the tables that summarize both lethal and non-lethal take data. 
# Also creates the labels that are used in Leaflet popups, and joins the tables to spatial 
# data for Hydrologic Unit Codes
# =================================================


# =================================================
# Load packages
library(shiny)
library(leaflet)
library(leaflet.extras)
library(leaflet.providers)
library(htmlwidgets)
# =================================================


# =================================================
# Assigning HUCs to some marine areas in WA
LocGroup1 <- c("Admiralty Inlet", "North Puget Sound", "South Puget Sound",
               "Whidbey Basin", "Puget Sound")
LocGroup2 <- "Hood Canal"
LocGroup3 <- "Strait of Juan de Fuca"


N <- length(wcr$Location)
for (i in 1:N){
  if (wcr$Location[i] %in% LocGroup1) {wcr$HUCNumber[i] <- 17110019} 
  if (wcr$Location[i] == "Hood Canal") {wcr$HUCNumber[i] <- 17110018} 
  if (wcr$Location[i] == "Strait of Juan de Fuca") {wcr$HUCNumber[i] <- 17110021} # also another one given (17110020)
}
# =================================================

# =================================================
# Aggregation and joining
# End result is two separate tables, one summarizing total take and one summarizing lethal take, with
# spatial data attached.

# TOTAL TAKE
ESUs <- unique(wcr$Species)

ESUdf <- aggregate(wcr$ExpTake, 
                   by = list(wcr$HUCNumber, wcr$Species, wcr$LifeStage, wcr$Prod),
                   FUN = sum) # aggregate total expected take by HUC

names(ESUdf) <- c("huc8", "ESU", "LifeStage", "Production", "theData") # rename columns
ESU_spatialTotal <- right_join(wbd.hucs, ESUdf, by = "huc8") # join with spatial data
ESU_spatialTotal <- ESU_spatialTotal %>% 
  st_transform(crs = 4326) %>% # set crs to play nice with Leaflet
  filter(huc8 != 99999999) %>% # filter out messy HUCs from map
  filter(!is.na(huc8)) #no NA HUCs

# create labels for Leaflet map popups
ESU_spatialTotal$labels <- paste0(
  "<strong> Name: </strong>",
  ESU_spatialTotal$name, "<br/> ",
  "<strong> HUC 8: </strong>",
  ESU_spatialTotal$huc8, "<br/> ",
  "<strong> Authorized Take (# of fish): </strong> ",
  ESU_spatialTotal$theData, "<br/> "
) %>%
  lapply(htmltools::HTML)


# LETHAL TAKE (same steps as total take, just different data)
ESUdfMort <- aggregate(wcr$TotalMorts,
                       by = list(wcr$HUCNumber, wcr$Species, wcr$LifeStage, wcr$Prod),
                       FUN = sum) # aggregate total lethal take by HUC, species, life stage and production

names(ESUdfMort) <- c("huc8", "ESU", "LifeStage", "Production", "theData") # rename columns
ESU_spatialMort <- right_join(wbd.hucs, ESUdfMort, by = "huc8") 
ESU_spatialMort <- 
  st_transform(ESU_spatialMort, crs = 4326) %>%
  filter(huc8 != 99999999) %>%
  filter(!is.na(huc8))

ESU_spatialMort$labels <- paste0(
  "<strong> Name: </strong>",
  ESU_spatialMort$name, "<br/> ",
  "<strong> HUC 8: </strong>",
  ESU_spatialMort$huc8, "<br/> ",
  "<strong> Lethal Take (# of fish): </strong> ",
  ESU_spatialMort$theData, "<br/> "
) %>%
  lapply(htmltools::HTML) # Create labels for Leaflet map popups





