# =================================================
# Rory Spurr                                      
# Script #1 to run before Shiny App               

# Summary:
# Creates a data table for display under the Leaflet map in the Shiny app.
# Table only includes fields that are of interest, while omitting those that aren't 
# Creates a water type column for the table as well
# =================================================


# =================================================
# Load in packages
library(DT) # package that helps with data table manipulation
library(naniar) # helps with replacing NA values

# Package made by Diana Dishman at NOAA, new users will have to download the package from Github
# by un-commenting and running lines 17:18
# library(devtools)
# devtools::install_github("DianaDishman-NOAA/NMFSResPermits")
library(NMFSResPermits)
# =================================================


# =================================================
# Use functions from NMFSResPermits package to organize data and create columns
wcr <- wcr %>%
  rename_population() %>%
  create_totalmorts() %>%
  order_table()
# =================================================


# =================================================
# Creating the location field for the table. This is based on a hierarchical sort 
# of different location based fields from the raw data. When researchers apply
# for permits they often use different fields to describe their location.
# The highest level (largest in terms of geographic scope) is used unless it
# is an NA value, than the next highest non-NA value is used.

# replaces "N/A" character values with NA
wcr <- wcr %>%
  replace_with_na(
    replace = list(WaterbodyName = "N/A",
                   BasinName = "N/A")
  ) 

# Each loop checks each value, if it returns NA it replaces it with the next value in the 
# hierarchy. 
Location <- wcr$WaterbodyName
for (i in 1:nrow(wcr)){
  if(is.na(Location[i] == T)){
    Location[i] <- wcr$BasinName[i]
  }
}

for (i in 1:nrow(wcr)){
  if(is.na(Location[i] == T)){
    Location[i] <- wcr$StreamName[i]
  }
}

wcr <- cbind(wcr, Location) # bind the locations to the table

wcr <- wcr %>%
  replace_with_na(replace = list(Location = ".")) # replaces "." values with NA

# adds the last available location based column if all other values have been NA
# LocationDescription is (usually) the longest/most detailed, hence why it is at the 
# bottom of the hierarchy
for (i in 1:nrow(wcr)){
  if(is.na(wcr$Location[i] == T)){
    wcr$Location[i] <- wcr$LocationDescription[i]
  }
} 


# ===================================================
# Creating a water type column that uses keywords to place locations into
# a freshwater or saltwater category.

Locations <- wcr$Location # grab locations

# Create list of keywords used to identify a place as saltwater or freshwater
SW_strings <- c("Sound", "Bay", "Ocean", "Strait", "Admiralty",
                "Whidbey", "Canal", "shelf", "Cape", "estuary", 
                "Estuary","lagoon", "Lagoon", "Delta",
                "estuarine")
FW_strings <- c("Lake", "stream", "freshwaters", "Columbia")

# This function assigns water types based on matching the keywords specified above
assignWaterType <- function(x, strings1 = SW_strings, strings2 = FW_strings){
  result <- "empty"
    ifelse(str_detect(x, paste(strings1, collapse = "|")) & str_detect(x, paste(strings2, collapse = "|")),
         result <- "SW/FW",
         ifelse(str_detect(x, paste(strings1, collapse = "|")), result <- "SW",
            result <- "FW"))
  return(result)
}
assignWaterType(x = Locations[41]) # testing the function

#create the column
SW_FW <- sapply(Locations, assignWaterType)

# only binds the column if it doesn't already exist
if (length(wcr$SW_FW) > 0){
  wcr <- wcr[,1:31]
  wcr <- cbind(wcr, SW_FW)
} else {wcr <- cbind(wcr, SW_FW)}

# Selecting the columns we want for final display in the app
wcr4App <- wcr %>%
  select(FileNumber, # File Number
         ResultCode, # Permit Type
         Organization, # Organization
         HUCNumber, # HUC 8
         Location, # Location
         SW_FW, # water type
         TakeAction:ExpTake, 
         TotalMorts,
         LifeStage,
         Species,
         Prod) %>%
  mutate(HUCNumber = as.character(HUCNumber)) %>%
  replace_na(list(HUCNumber = "No Data"))

# table for checking values to ensure the sorting function works
# check <- table(wcr4App$Location, wcr4App$SW_FW)
# write.csv(check, "docs/waterTypeCheck.csv") 



