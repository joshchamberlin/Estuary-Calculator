# Alana Santana 
# Script to run before Shiny App
# Summary: This script runs the filters and math for the time series plots and data table. 
# =================================================================================
# Load in packages
#install.packages("plotly")
library(DT)
library(RColorBrewer)
# source(paste(getwd(), "/code/dependencies/ReadingFiltering.R", sep = ""))
# =================================================================================
#Setting up data - Changing time factor
a<-as.factor(wcr_act$DateReportPeriodEnd) #identifies the column we'll use for time factor and makes object
b<-strptime(a,format="%Y-%m-%d") # reformat date column
Year <- format(as.Date(b, format ="%Y-%m-%d" ), "%Y") # creates new column just for the year
Year <- as.data.frame(Year) #reassigns year column as object and own data
wcr.v <- cbind(wcr_act, Year) #merges back to the original data frame
#==============================================================
#Setting up data - Creating totalmorts and QC NA values
wcr.v <-wcr.v %>% 
  create_totalmorts() %>%
  order_table() %>% 
  replace_na(list(ExpTake = 0, ActTake = 0, TotalMorts = 0, ActMort = 0))
#==================================================================================
############################### Stacked bar plot code #############################
# =================================================================================
#Aggregating Authorized Take 
df <- aggregate(wcr.v$ExpTake, 
                by = list(wcr.v$CommonName, wcr.v$ResultCode, wcr.v$ActMort, wcr.v$ActTake, wcr.v$TakeAction, wcr.v$Species, wcr.v$LifeStage, wcr.v$Prod, 
                          wcr.v$Year, wcr.v$TotalMorts), FUN = sum) 

names(df) <- c("CommonName", "ResultCode", "ActMort", "ActTake", "TakeAction", "ESU", "LifeStage", "Production", "Year", "TotalMorts", "ExpTake") #renaming columns
#==============================================================
#Summing each variable by year and grouping by ESU, Production, and Lifestage
YT <-df %>%
  group_by(Year, ESU, Production, LifeStage) %>% # Actual Take
  summarise(Used = sum(ActTake))
YM <- df %>%
  group_by(Year, ESU, Production, LifeStage) %>% # Actual Mortality
  summarise(Used = sum(ActMort))
TM <- df %>%
  group_by(Year, ESU, Production, LifeStage) %>% # Authorized Mortality
  summarise(Authorized_Mortality = sum(TotalMorts))
ET <-df %>%
  group_by(Year, ESU, Production, LifeStage) %>%
  summarise(Authorized_Take = sum(as.numeric(ExpTake))) #Authorized Take 
#==============================================================
#Merging data sets
Take <- merge(YT, ET, by = c("Year", "ESU", "Production", "LifeStage")) #grouping authorized and expected take together
Mort <- merge(YM, TM, by = c("Year", "ESU", "Production", "LifeStage")) #grouping auth mortality and expected mortality together
#==============================================================
#Replacing NaN or NA or Inf with 0
Take[is.na(Take)] <- 0
Mort[is.na(Mort)] <- 0
#==============================================================
#Math for determining unused take
Take %>%
  mutate(Unused = Authorized_Take - Used) -> t
Mort %>% 
  mutate(Unused = Authorized_Mortality - Used) -> m
#==============================================================
#Delineating columns we want to include within plot data
df1 <- t %>%
  gather("Take_Type","N", 5:7) 
df2 <- m %>% 
  gather("Take_Type","N", 5:7) 
#==============================================================
#Creating plot data for further filtering 
df1 %>%
  filter(Take_Type %in% c("Used","Unused")) -> df_plot
df2 %>%
  filter(Take_Type %in% c("Used","Unused")) -> df_plot2
#==================================================================================
################################### Table code ##################################
# =================================================================================
#Aggregating Authorized Take 
dt <- aggregate(wcr.v$ExpTake, 
                by = list(wcr.v$FileNumber, wcr.v$CommonName, wcr.v$ResultCode, wcr.v$ActMort, wcr.v$ActTake, wcr.v$TakeAction, wcr.v$Species, wcr.v$LifeStage, wcr.v$Prod, 
                          wcr.v$Year, wcr.v$TotalMorts, wcr.v$CaptureMethod, wcr.v$ReportID), FUN = sum) 

names(dt) <- c("FileNumber","CommonName", "ResultCode", "ActMort", "ActTake", 
               "TakeAction", "ESU", "LifeStage", "Production", "Year", "TotalMorts",
               "CaptureMethod", "ReportID","ExpTake")

#==============================================================
#Summing each variable by year
YT <-dt %>%
  group_by(Year, ESU, Production, LifeStage, FileNumber, CaptureMethod, ReportID, ResultCode) %>%
  summarise(Reported_Take = sum(ActTake))
YM <- dt %>%
  group_by(Year, ESU, Production, LifeStage,FileNumber, CaptureMethod, ReportID, ResultCode) %>%
  summarise(Reported_Mortality = sum(ActMort))
TM <- dt %>%
  group_by(Year, ESU, Production, LifeStage, FileNumber, CaptureMethod, ReportID, ResultCode) %>%
  summarise(Authorized_Mortality = sum(TotalMorts))
ET <-dt %>%
  group_by(Year, ESU, Production, LifeStage, FileNumber, CaptureMethod, ReportID, ResultCode) %>% 
  summarise(Authorized_Take = sum(as.numeric(ExpTake)))
#==============================================================
#Merging data sets back together
Take <- merge(YT, ET, by = c("Year", "ESU", "Production", "LifeStage", "FileNumber", "CaptureMethod", "ReportID", "ResultCode"))
Mort <- merge(YM, TM, by = c("Year", "ESU", "Production", "LifeStage", "FileNumber", "CaptureMethod", "ReportID", "ResultCode"))
dt <- merge(Take, Mort, by = c("Year", "ESU", "Production", "LifeStage", "FileNumber", "CaptureMethod", "ReportID", "ResultCode"))
#==============================================================
#Math for determining the Unused Take/Mortality 
dt %>%
  mutate(Authorized_Take_Unused = Authorized_Take - Reported_Take) %>%
  mutate(Authorized_Mortality_Unused = Authorized_Mortality - Reported_Mortality) -> dt
#==============================================================
################################### Extras ####################
#==============================================================
#Creating custom color palette 
mycols <- colors()[c(461, 142, 525, 87)]

