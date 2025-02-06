# # =================================================================================
# # Helpful functions and data organization/aggregation for ESA permits capstone project
# # Rory Spurr
# # July 5th 2022
# # =================================================================================
# 
# # Aggregating the number of apps per Species
# # returns a 2 column tibble with the species and the number of applications currently 
# # active per species, sorted in ascending order
# species.num.apps <- function(df = wcr){ # I put this in a function to reduce global variables
#   # get species names
#   species <- unique(df$CommonName)
#   #create tibble as species and the number of apps
#   nApps <- table(df$CommonName)
#   SpeciesDist <- as_tibble(cbind(species, nApps))
#   SpeciesDist <- SpeciesDist %>% 
#     mutate(nApps = as.numeric(nApps)) %>%
#     arrange(nApps) # sort in ascending order
#   return(SpeciesDist)
# }
# 
# # =================================================================================
# # Creating spatial data frames for each species as elements of a list
# # helpful for pre-filtering data to increase R shiny speed
# create.species.dats <- function(df = wcr, df_spatial = wbd.hucs){
#   species <- unique(df$CommonName)
#   nspecies <- length(species)
#   species.dats <- list(NULL)
#   
#   for (i in 1:(nspecies)){
#     temp <- df %>%
#       filter(CommonName == species[i])
#     species.dats[[i]] <- temp
#   }
#   
#   for (i in 1:(nspecies)){
#     temp <- right_join(x = df_spatial, y = species.dats[[i]], by = c("huc8" = "HUCNumber")) 
#     temp <- filter(temp, huc8 != c(99999999, NA))
#     species.dats[[i]] <- temp
#   }
#   return(species.dats)
# }
# species.dats <- create.species.dats()
# 
# 
# # =================================================================================
# # Spatial Sum functions
# # =================================================================================
# # Input is a data frame with the huc8 spatial component. These functions will add a 
# # column displaying and summarizing the total sum of whatever metric by species
# # and huc8 to that data frame. Can use the lapply() to with the species data frame
# # list from above to quickly find sum metrics for each species. Can then use subset 
# # to look at particular species data
# 
# # IMPORTANT: For now have to switch s2 geometry off in order for below functions to work
# # need to research implications of this, or if there is a way around it
# 
# # sf::sf_use_s2(FALSE) # switches s2 geometry off
# 
# mort.sum <- function(df){
#   df <- df %>%
#     group_by(huc8) %>%
#     summarize(sum = sum(IndMort, na.rm = T))
# }
# # # Example usage
# # species.IndMort <- lapply(species.dats, mort.sum)
# # names(species.dats) <- unique(wcr$CommonName)
# 
# take.sum <- function(df){
#   df <- df %>%
#     group_by(huc8) %>%
#     summarize(sum = sum(ExpTake, na.rm = T))
# }
# # # Example usage
# # species.ExpTake <- lapply(species.dats, take.sum)
# # names(species.dats) <- unique(wcr$CommonName)


