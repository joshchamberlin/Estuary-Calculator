# =================================================
# Rory Spurr and Alana Santana
# Script to read in and filter data

#Summary: 
# The below script filters data from the NMFS' WCR APPS database 
# into relevant and usable data for the purpose of this app. 
# Includes filters on permit data and reporting data. 
# Also includes filtering for spatial data. 
# =================================================

# =================================================
# Read in Packages
library(shiny)
library(ggplot2)
library(tidyr)
library(sf)
library(dplyr)
library(tidyverse)
library(leaflet)
library(NMFSResPermits)
library(plotly)
library(viridis)
sf_use_s2(FALSE)

# =================================================================================
# Permit Data
# =================================================================================
# West Coast region read in
wcr.init <- read_csv("data_raw/WCRpermits_demo_20221129.csv")
#Filters for condensing to relevant data
wcr <- wcr.init %>% 
  filter(PermitStatus == "Issued") %>% # only permits actually issued
  filter(DateIssued >"2012-01-01") %>% # permits issued within last 10 years
  filter(DateExpired > Sys.Date()) %>% #DateField >= Sys.Date() puts it to the date of the system
  filter(ResultCode %in% c("NMFS 10a1A Salmon","4d", "NMFS BiOp DTA", "Tribal 4d")) %>% # only relevant permit types
  mutate(LifeStage = recode(LifeStage,
                            "Smolt" = "Juvenile",
                            "Fry" = "Juvenile",
                            "Larvae" = "Juvenile",
                            "Subadult" = "Adult")) %>% # simplifying life stage
  mutate(CommonName = recode(CommonName,
                             "Salmon, coho" = "coho salmon",
                             "Steelhead" = "steelhead", 
                             "Eulachon" = "eulachon",
                             "Salmon, Chinook" = "Chinook salmon",
                             "Salmon, chum" = "chum salmon",
                             "Salmon, sockeye" = "sockeye salmon",
                             "Sturgeon, green" = "green sturgeon",
                             "Rockfish, Canary" = "canary rockfish",
                             "Rockfish, Bocaccio" = "bocaccio",
                             "Rockfish, Yelloweye" = "yelloweye rockfish")) %>% # simplifying species names
  mutate(HUCNumber = recode(HUCNumber,
                            `18020103` = 18020156,
                            `18020109` = 18020163,
                            `18020112` = 18020154,
                            `18020118` = 18020154,
                            `18040005` = 18040012,
                            `18060001` = 18060015,
                            `18060012` = 18060006)) %>% # recoded HUCs based on historical reorganizing by USGS
                                                        # see metadata for details
  mutate(Species = paste(Population, CommonName, sep = " ")) %>% #creating ESU/DPS names
  mutate(Prod = recode(Production, 
                       "Natural" = "Natural", 
                       "Listed Hatchery" = "Hatchery", 
                       "Listed Hatchery, Clipped and Intact" = "Hatchery",  
                       "Listed Hatchery Intact Adipose" = "Hatchery", 
                       "Listed Hatchery Adipose Clip" = "Hatchery", 
                       "Unlisted Hatchery" = "Unlisted Hatchery")) %>% # simplifying species origin data
  filter(Prod != "Unlisted Hatchery") %>%   # Omitting non-invasive take actions
  filter(Prod != "All") %>% 
  filter(TakeAction != "Observe/Harass") %>%
  filter(TakeAction != "Observe/Sample Tissue Dead Animal") %>%
  filter(TakeAction != "Unknown")

# The same filters (more or less) were applied to reported take data
wcr_act <- read_csv("data_raw/WCRpermit_reports_demo_20221129.csv")
wcr_act <- wcr_act %>%
  filter(ResultCode %in% c("NMFS 10a1A Salmon","4d", "NMFS BiOp DTA", "Tribal 4d")) %>% 
  mutate(LifeStage = recode(LifeStage,
                            "Smolt" = "Juvenile",
                            "Kelt" = "Juvenile",
                            "Yearling" = "Juvenile",
                            "Sub-Yearling" = "Juvenile",
                            "Egg" = "Juvenile",
                            "Fry" = "Juvenile",
                            "Larvae" = "Juvenile",
                            "Subadult" = "Adult",
                            "Spawned Adult/ Carcass" = "Adult")) %>%
  mutate(CommonName = recode(CommonName,
                             "Salmon, coho" = "coho salmon",
                             "Steelhead" = "steelhead",
                             "Eulachon" = "eulachon",
                             "Salmon, Chinook" = "Chinook salmon",
                             "Salmon, chum" = "chum salmon",
                             "Salmon, sockeye" = "sockeye salmon",
                             "Sturgeon, green" = "green sturgeon",
                             "Rockfish, Canary" = "canary rockfish",
                             "Rockfish, Bocaccio" = "bocaccio",
                             "Rockfish, Yelloweye" = "yelloweye rockfish")) %>%
  mutate(Species = paste(Population, CommonName, sep = " ")) %>% 
  mutate(Prod = recode(Production, 
                       "Natural" = "Natural", 
                       "Listed Hatchery" = "Hatchery", 
                       "Listed Hatchery, Clipped and Intact" = "Hatchery",  
                       "Listed Hatchery Intact Adipose" = "Hatchery", 
                       "Listed Hatchery Adipose Clip" = "Hatchery",
                       "Unlisted Hatchery" = "Unlisted Hatchery")) %>%
  filter(Prod != "Unlisted Hatchery") %>% 
  filter(TakeAction != "Observe/Harass") %>%
  filter(TakeAction != "Observe/Sample Tissue Dead Animal")


# =================================================================================
# Check notes below:
# recoding HUCs to account for HUCs that were altered or moved
# broken hucs are 18020103, 18020109, 18020112, 18020118,
# 18040005, 18060001, 18060012 -> they are broken as they don't have spatial data info in the USGS WBD

# Notes on what to change them to:
# 18020103 = 18020156 # very certain
# 18020109 = 18020163 # very certain
# 18020112 = 18020154 # very certain based on location descriptions
# 18020118 = 18020154 # very certain based on location descriptions
# 18040005 = 18040012 # very certain based on location descriptions
# 18060001 = 18060015 # split between 18050006 as well, arbitrarily picked
# 18060012 = 18060006 # chose this over Monterrey Bay as population is South-Central Cal Coast

# =================================================================================
# Spatial Data
# =================================================================================
# Reading in HUC 8 shape file
wbd.hucs <- read_sf("data_raw/WCR_HUC8/WCR_HUC8.shp")
wbd.hucs$huc8 <- as.double(wbd.hucs$huc8)

# =================================================================================
# ESU Species with Basins -> for creation of ESU Boundaries
# =================================================================================
ESUBasins <- read_csv("data_raw/APPS_HUCassignments_11Feb23.csv")
ESUBasins <- ESUBasins %>%
  filter(Species %in% c("Eulachon", "Salmon, Chinook", "Salmon, chum",
                        "Salmon, coho", "Salmon, sockeye", "Steelhead",
                        "Sturgeon, green"))
