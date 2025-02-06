# Alana Santana and Rory Spurr
# Script to run prior to running time series app - Table codes
#install.packages("plotly")
source(paste(getwd(), "/code/dependencies/Reading and Filtering.R", sep = ""))

# =================================================================================
#Setting up data - Changing time factor
a<-as.factor(wcr_act$DateReportPeriodEnd)
b<-strptime(a,format="%Y-%m-%d") 
Year <- format(as.Date(b, format ="%Y-%m-%d" ), "%Y")
Year <- as.data.frame(Year)
wcr.v <- cbind(wcr_act, Year) #look into annual time start vs annual start end
#==============================================================
#Setting up data - Creating totalmorts and QC NA values
wcr.v <-wcr.v %>% 
  create_totalmorts() %>%
  order_table() %>% 
  replace_na(list(ExpTake = 0, ActTake = 0, TotalMorts = 0, ActMort = 0))
#==============================================================
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
  summarise(Authorized_Take = sum(ExpTake))
# #==============================================================
# #Merging data sets
Take <- merge(YT, ET, by = c("Year", "ESU", "Production", "LifeStage", "FileNumber", "CaptureMethod", "ReportID", "ResultCode"))
Mort <- merge(YM, TM, by = c("Year", "ESU", "Production", "LifeStage", "FileNumber", "CaptureMethod", "ReportID", "ResultCode"))
dt <- merge(Take, Mort, by = c("Year", "ESU", "Production", "LifeStage", "FileNumber", "CaptureMethod", "ReportID", "ResultCode"))
#==============================================================
dt %>%
  mutate(Authorized_Take_Unused = Authorized_Take - Reported_Take) %>%
  mutate(Authorized_Mortality_Unused = Authorized_Mortality - Reported_Mortality) -> dt
#==============================================================
#Reordering columns
#dt <- colReorder(c("FileNumber", "ResultCode", "ReportID","Year", "ESU", 
                  # "Production", "LifeStage", "CaptureMethod", "Authorized_Take", "Reported_Take", 
                  # "Authorized_Mortality", "Reported_Mortality", 
                  # "Authorized_Take_Unused", "Authorized_Mortality_Unused"))
