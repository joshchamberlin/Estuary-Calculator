########################################################################################################
### SCRIPT METADATA ###                                                                             
# Title: permitFunctions.R                                                                 
# Subject: Common functions for scientific research permit analyses              
# Author: E.B. Smith                                                                                
# First Created: 10/7/2020                                                                            
#                                                                                                    
### SCRIPT HISTORY ###                                                                               
# Script Modification Log                                                                            
# Date          Staff         Comment                                                                  
# 10/27/20      D. Dishman    Added function to order tables by species/prod/lifestage, finished create TotalMorts function by 
#                               adding conventions (not df-specific) and added error warning if used before renaming TakeAction,
#                               corrected LifeStage recode missing capitalization and recode TakeAction order (old = new)
# 2/12/21       D. Dishman    Updated colformat_num to colformat_double per flextable package update
########################################################################################################

# *** get_root_filenum ---------
get_root_filenum <- function(file_list){
  return(unique(str_extract(file_list, "[^- | //s]+")))
}


# *** rename_population ---------
# ***** takes a dataframe that already has the Population variable and updates the wording
# ***** for Population based on whatever equalities are listed in the recode() portion.
rename_population <- function(df){
  df <- df %>%
    mutate(Population = recode(Population,
                               "Deschutes River Steelhead NEP" = "Middle Columbia River"))
  return(df)
}

# *** rename_commonname ---------
# ***** takes a dataframe that already has the CommonName variable and updates the
# ***** wording FROM the left hand side of the ~ TO the right hand side of the ~
rename_commonname <- function(df){
  df <- df %>%
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
                               "Rockfish, Yelloweye" = "yelloweye rockfish"))
  return(df)
}

# *** consolidate_lifestage ---------
# ***** takes a dataframe that already has the LifeStage variable and updates the wording
# ***** for LifeStagen based on whatever equalities are listed in the recode() portion.
# ***** the ifelse statement makes it so that changes to Adults are only done on salmonids
coordinate_lifestage <- function(df){
  df <- df %>%
    mutate(LifeStage = recode(LifeStage,
                              "Smolt" = "Juvenile",
                              "Fry" = "Juvenile",
                              "Yearling" = "Juvenile",
                              "Sub-Yearling" = "Juvenile",
                              "Parr" = "Juvenile")) %>%
    mutate(LifeStage = ifelse(grepl(c("salmon|steelhead"), Species),
                              recode(LifeStage,
                                     "Subadult" = "Adult",
                                     "Jack" = "Adult"),
                              LifeStage)) %>%
    filter(!(grepl(c("salmon|steelhead"), Species)==TRUE & LifeStage == "Egg"))
  return(df)
}


# *** rename_takeaction ---------
# ***** takes a dataframe that already has the TakeAction variable and updates the wording
# ***** for TakeAction based on whatever equalities are listed in the recode() portion.
rename_takeaction <- function(df){
  df <- df %>%
    mutate(TakeAction= recode(TakeAction,
                              "IM" = "Intentional \\(Directed\\) Mortality" ,
                              "C/H/R" = "Capture/Handle/Release Fish",
                              "C/M, T, ST/R" = "Capture/Mark, Tag, Sample Tissue/Release Live Animal",
                              "O/H" = "Observe/Harass",
                              "O/ST D" = "Observe/Sample Tissue Dead Animal",
                              "C,S,T" = "Collect, Sample, and Transport Live Animal")) 
}

# *** rename_speciesvar ---------
# ***** combines Population and CommonName variables to create the Species variable
create_speciesvar <- function(df){
  df <- df %>%
    mutate(Species = paste(Population, CommonName, sep = " "))
  return(df)
}

# *** create_total_morts ---------
# *** this isn't often needed, as it renames TakeAction to APPS standard output
# *** this function MUST be called after renaming TakeAction, if it is other than default 
create_totalmorts <- function(df){
  try(if('Intentional (Directed) Mortality' %in% df$TakeAction==FALSE) stop("haven't renamed TakeAction yet!", call. = FALSE))
  
  df <- df %>%
    mutate(TotalMorts = ifelse(
      TakeAction=='Intentional (Directed) Mortality',
      ExpTake + IndMort, IndMort))
  return(df)
}

# *** rename_production ---------
# ***** takes a dataframe that already has the Production variable and updates
# ***** the wording based on whatever equalities are listed in the recode() portion
rename_production <- function(df){
  df <- df %>%
    mutate(Production = recode(Production,
                               "LHAC" = "Listed Hatchery Adipose Clip",
                               "LHIA" = "Listed Hatchery Intact Adipose",
                               "LHAC_LHIA" = "Listed Hatchery, Clipped and Intact",
                               "LHAC_LHIA_NOR" = "Listed Hatchery and Natural Origin"))
  return(df)
}

# *** rename_coordination_fx ---------
# ***** takes a dataframe and boolean variables stating whether or not changes for each category
# ***** are wanted. It then calls the appropriate functions to make those changes, and returns
# ***** the updated dataframe
rename_coordination_fx <- function(df, population, commonname, takeaction, speciesvar, production, lifestage){
  if(population == TRUE){
    df <- rename_population(df)}
  if (commonname == TRUE){
    df <- rename_commonname(df)}
  if(takeaction == TRUE){
    df <- rename_takeaction(df)}
  if(speciesvar == TRUE){
    df <- create_speciesvar(df)}
  if(production==TRUE){
    df <- rename_production(df)}
  if(lifestage==TRUE){
    df <- coordinate_lifestage(df)
  }
  return(df)
}

# *** order_table ----------
# ***** takes a tibble/df and orders it by Species, LifeStage, and Production (source) columns for pretty printing
# ***** currently set up to create ordered factors out of these columns first, then order all at once
# ***** this arrangement is set in a North to South species pattern, which matches the current BiOp template

# ** Species order: first setting order we want species to appear in tables (we want these available as object)
sp.order <- c(
  "Puget Sound Chinook salmon",
  "Puget Sound steelhead",
  "Puget Sound/Georgia Basin DPS bocaccio", 
  "Puget Sound/Georgia Basin DPS yelloweye rockfish", 
  "Hood Canal summer-run chum salmon",
  "Ozette Lake sockeye salmon", 
  "Upper Columbia River spring-run Chinook salmon", 
  "Upper Columbia River spring-run NEP Chinook salmon",
  "Upper Columbia River steelhead", 
  "Middle Columbia River steelhead",
  "Snake River spring/summer-run Chinook salmon",
  "Snake River fall-run Chinook salmon", 
  "Snake River Basin steelhead", 
  "Snake River sockeye salmon", 
  "Deschutes River steelhead NEP",
  "Lower Columbia River Chinook salmon",
  "Lower Columbia River coho salmon", 
  "Lower Columbia River steelhead",
  "Columbia River chum salmon", 
  "Upper Willamette River Chinook salmon", 
  "Upper Willamette River steelhead", 
  "Oregon Coast coho salmon",
  "Southern Oregon/Northern California Coast coho salmon",
  "Northern California steelhead",
  "California Coastal Chinook salmon",
  "Sacramento River winter-run Chinook salmon",
  "Central Valley spring-run Chinook salmon",
  "California Central Valley steelhead",
  "Central California Coast coho salmon",  
  "Central California Coast steelhead",
  "South-Central California Coast steelhead",
  "Southern California steelhead",
  "Southern DPS eulachon",
  "Southern DPS green sturgeon")                                                                            

# ** Lifestage order
ls.order <- c("Adult", "Kelt", "Spawned Adult/ Carcass", "Subadult", "Smolt", 
              "Yearling", "Juvenile", "Parr", "Sub-Yearling", "Fry", "Larvae", "Egg")                                                                     

# ** Production or origin order      
pr.order <- c("Natural", 
              "Listed Hatchery", 
              "Listed Hatchery, Clipped and Intact", 
              "Listed Hatchery Intact Adipose", 
              "Listed Hatchery Adipose Clip",
              "Listed Hatchery and Natural Origin", 
              "Unlisted Hatchery")

# ** Species order: actually order a tibble by how we want species to appear in tables
order_table <- function(df){
  df <- df %>%
    mutate(Species = factor(Species, sp.order)) %>%
    mutate(LifeStage = factor(LifeStage, ls.order)) %>%
    mutate(Production = factor(Production, pr.order)) %>%
    arrange(Species, LifeStage, Production)
  return(df)
}

# *** perc_ESU_DPS ----------
# ***** Combining take tables with abundance is tricky because we don't have abundance data
# ***** for all life stages and HOR/NOR components individually

perc_ESU_DPS <- function(df, abund){
  #Bring in ESU/DPS abundance data for percent of ESU/DPS taken and killed columns
  #merge baseline take and abundance tables
  df_abund <- merge(df, abund, by = c("Species", "LifeStage", "Production"), all = TRUE)
  
  #Calculate take sums as perc of ESU/DPS using adult abundance only
  #For rockfish and eulachon
  df_by_sp_adults <- df_abund %>%
    group_by(Species) %>%
    replace_na(list(ExpTake = 0, TotalMorts = 0, abundance = 0)) %>%
    summarise(ExpTake = sum(ExpTake), TotalMorts = sum(TotalMorts), abundance = sum(abundance)) %>%
    mutate(PercESUtake = (ExpTake/abundance)*100) %>%
    mutate(PercESUkill = (TotalMorts/abundance)*100)
  
  df_sp <- df %>%
    filter(Species %in% c("Puget Sound/Georgia Basin DPS bocaccio",
                          "Puget Sound/Georgia Basin DPS yelloweye rockfish",
                          "Southern DPS eulachon")) %>%
    left_join(select(df_by_sp_adults, c(Species, abundance, PercESUtake, PercESUkill)), 
              by = "Species")
  
  #Calculate take sums as perc of ESU/DPS by species and lifestage
  #For species where you can't distinguish hatchery and natural origin adults
  df_by_sp_ls <- df_abund %>%
    group_by(Species, LifeStage) %>%
    replace_na(list(ExpTake = 0, TotalMorts = 0, abundance = 0)) %>%
    summarise(ExpTake = sum(ExpTake), TotalMorts = sum(TotalMorts), abundance = sum(abundance)) %>%
    mutate(PercESUtake = (ExpTake/abundance)*100) %>%
    mutate(PercESUkill = (TotalMorts/abundance)*100) %>%
    filter(!abundance == 0)
  
  df_ls_list <- unique(abund[abund$Production == "Listed Hatchery and Natural Origin", "Species"]) 
  
  df_ls <- df %>%
    filter(Species %in% unlist(df_ls_list)) %>%
    filter(LifeStage == "Adult") %>%
    left_join(select(df_by_sp_ls, c(Species, LifeStage, abundance, PercESUtake, PercESUkill)), 
              by = c("Species", "LifeStage"))
  
  #Calculate take sums as perc of ESU/DPS by species, lifestage, and hatchery origin
  #For species where you can't distinguish clipped and unclipped hatchery fish
  df_by_sp_ls_HOR <- df_abund %>%
    mutate(Production = str_remove(Production, c(" Intact Adipose| Adipose Clip"))) %>%
    group_by(Species, LifeStage, Production) %>%
    replace_na(list(ExpTake = 0, TotalMorts = 0, abundance = 0)) %>%
    summarise(ExpTake = sum(ExpTake), TotalMorts = sum(TotalMorts), abundance = sum(abundance)) %>%
    mutate(PercESUtake = (ExpTake/abundance)*100) %>%
    mutate(PercESUkill = (TotalMorts/abundance)*100) %>%
    filter(!Production == "Natural")
  
  df_HOR_list <- unique(abund[abund$Production == "Listed Hatchery", "Species"]) 
  
  df_HOR <- df %>%
    filter(Species %in% unlist(df_HOR_list)) %>%
    filter(LifeStage == "Adult") %>%
    filter(!Production == "Natural") %>%
    left_join(select(df_by_sp_ls_HOR, c(Species, LifeStage, abundance, PercESUtake, PercESUkill)), 
              by = c("Species", "LifeStage"))
  
  
  #Calculate take sums as perc of ESU/DPS when you have all component information
  #For salmonid juveniles and most salmonid adults, green sturgeon juvs, subadults, and adults
  df_by_all <- df_abund %>%
    group_by(Species, LifeStage, Production) %>%
    replace_na(list(ExpTake = 0, TotalMorts = 0, abundance = 0)) %>%
    summarise(ExpTake = sum(ExpTake), TotalMorts = sum(TotalMorts), abundance = sum(abundance)) %>%
    mutate(PercESUtake = (ExpTake/abundance)*100) %>%
    mutate(PercESUkill = (TotalMorts/abundance)*100) %>%
    filter(!abundance == 0)
  #filter(!ExpTake == 0) 
  
  df_sturg <- df %>%
    filter(Species == "Southern DPS green sturgeon") %>%
    left_join(select(df_by_all, c(Species, LifeStage, Production, abundance, PercESUtake, PercESUkill)), 
              by = c("Species", "LifeStage", "Production"))
  
  
  df_NOR <- df %>%
    filter(LifeStage == "Adult" & Production == "Natural") %>%
    filter(!Species %in% c("Southern DPS green sturgeon",
                           "Puget Sound/Georgia Basin DPS bocaccio",
                           "Puget Sound/Georgia Basin DPS yelloweye rockfish",
                           "Southern DPS eulachon")) %>%
    left_join(select(df_by_all, c(Species, LifeStage, Production, abundance, PercESUtake, PercESUkill)), 
              by = c("Species", "LifeStage", "Production")) %>% 
    filter(!abundance == 0)
  
  df_juvs <- df %>%
    filter(LifeStage == "Juvenile") %>%
    filter(!Species %in% c("Southern DPS green sturgeon",
                           "Puget Sound/Georgia Basin DPS bocaccio",
                           "Puget Sound/Georgia Basin DPS yelloweye rockfish",
                           "Southern DPS eulachon")) %>%
    left_join(select(df_by_all, c(Species, LifeStage, Production, abundance, PercESUtake, PercESUkill)), 
              by = c("Species", "LifeStage", "Production"))
  
  perc_ESU_DPS_table <- bind_rows(df_ls, df_sp, df_HOR, df_NOR, df_sturg, df_juvs) %>%
    relocate(LifeStage, .after = Species) %>%
    select(!abundance) %>%
    mutate(PercESUtake = case_when(
      PercESUtake == 0 | PercESUtake >=0.001 ~ scales::number(PercESUtake, big.mark = "", accuracy = .001),
      PercESUtake >0 & PercESUtake <0.001 ~ scales::number(PercESUtake, big.mark = "", accuracy = .0001))) %>%
    mutate(PercESUtake = as.character(PercESUtake)) %>%
    mutate(PercESUtake = str_replace_all(PercESUtake, c(
      "0.0000" = "<0.001",
      "0.0001" = "<0.001",
      "0.0002" = "<0.001",
      "0.0003" = "<0.001",
      "0.0004" = "<0.001",
      "0.0005" = "<0.001",
      "0.0006" = "<0.001",
      "0.0007" = "<0.001",
      "0.0008" = "<0.001",
      "0.0009" = "<0.001"))) %>%
    mutate(PercESUkill = case_when(
      PercESUkill == 0 | PercESUkill >=0.001 ~ scales::number(PercESUkill, big.mark = "", accuracy = .001),
      PercESUkill >0 & PercESUkill <0.001 ~ scales::number(PercESUkill, big.mark = "", accuracy = .0001))) %>%
    mutate(PercESUkill = as.character(PercESUkill)) %>%
    mutate(PercESUkill = str_replace_all(PercESUkill, c(
      "0.0000" = "<0.001",
      "0.0001" = "<0.001",
      "0.0002" = "<0.001",
      "0.0003" = "<0.001",
      "0.0004" = "<0.001",
      "0.0005" = "<0.001",
      "0.0006" = "<0.001",
      "0.0007" = "<0.001",
      "0.0008" = "<0.001",
      "0.0009" = "<0.001"))) %>%
    order_table()
  
  return(perc_ESU_DPS_table)
}

# *** pretty_flextable_fx ---------
# ***** takes a tibble, creates pretty, formatted flextable for export into word doc
# ***** note a preview can be printed if desired
# ***** this only FORMATS the flextable - you must open a docx, add flextables to the body
# ***** and then PRINT the docx file to export

pretty_flextable <- function(df, ftname){
  
  if("TakeAction" %in% colnames(df)){
    df <- df %>%
      mutate(TakeAction= recode(TakeAction,
                                "Intentional \\(Directed\\) Mortality" = "IM",
                                "Capture/Handle/Release Fish" = "C/H/R",
                                "Capture/Handle/Release Animal" = "C/H/R",
                                "Capture/Mark, Tag, Sample Tissue/Release Live Animal" = "C/M, T, ST/R",
                                "Observe/Harass" = "O/H",
                                "Observe/Sample Tissue Dead Animal" = "O/ST D",
                                "Collect, Sample, and Transport Live Animal" = "C,S,T"))}
  
  if("Production" %in% colnames(df)){
    df <- df %>%
      mutate(Production = recode(Production,
                                 "Listed Hatchery Adipose Clip" = "LHAC",
                                 "Listed Hatchery Intact Adipose" = "LHIA",
                                 "Listed Hatchery, Clipped and Intact" = "LHAC & LHIA",
                                 "Listed Hatchery and Natural Origin" = "LHAC, LHIA & NOR"))}
  
  ft <- flextable(df) 
  
  ft <- autofit(ft) %>%
    font(fontname = "Times", part = "all") %>% 
    align(align = "center", part = "header") %>%
    align(align = "center", part = "body") %>%
    align(align = "left", j = 1, part = "all") %>%
    add_header_lines(values = ftname) 
  
  if("Species" %in% colnames(df)){
    ft <- ft %>%
      width(j= "Species", 1) %>%
      width(j= "LifeStage", 0.9) %>%
      width(j= "Production", 0.8) %>%
      border_inner_h(part="all", border = officer::fp_border(color="gray")) %>%                       
      merge_v(j = c("Species", "LifeStage")) %>%
      set_header_labels(Production = "Origin", LifeStage = "Life Stage")
  }
  
  if("abundance" %in% colnames(df)){
    ft <- ft %>%
      set_header_labels(abundance = "Abundance") %>%
      colformat_num(j = "abundance", big.mark=",", na_str = "-")
  }
  
  if("ExpTake" %in% colnames(df)){
    ft <- ft %>%
      width(j= ~ExpTake + TotalMorts, 0.8) %>%
      set_header_labels(ExpTake = "Requested Take", TotalMorts = "Lethal Take") %>%
      colformat_double(j = c("ExpTake", "TotalMorts"), big.mark=",", digits = 0, na_str = "-")
  }
  
  if("PercESUtake" %in% colnames(df)){
    ft <- ft %>%
      width(j= ~PercESUtake + PercESUkill, 0.9) %>%
      merge_v(j = c("PercESUtake", "PercESUkill")) %>%
      set_header_labels(PercESUtake = "Percent of ESU/DPS taken", 
                        PercESUkill = "Percent of ESU/DPS killed") %>%
      colformat_char(j = c("PercESUtake", "PercESUkill"), na_str = "-")
  }  
  
  if("TakeAction" %in% colnames(df)){
    ft <- ft %>%
      width(j= "TakeAction", 1.1) %>%
      set_header_labels(TakeAction = "Take Action")
  }
  
  if("PriorExpTake" %in% colnames(df)){
    ft <- ft %>%
      width(j= ~PriorExpTake + PriorTotalMorts, 0.8) %>%
      set_header_labels(PriorExpTake = "Prior Total Take", PriorTotalMorts = "Prior Lethal Take") %>%
      colformat_double(j = c("PriorExpTake", "PriorTotalMorts"), big.mark=",", digits = 0, na_str = "-")
  }
  return(ft)
  #print(ft, preview = "docx")                                                                     #print test view if desired
}
