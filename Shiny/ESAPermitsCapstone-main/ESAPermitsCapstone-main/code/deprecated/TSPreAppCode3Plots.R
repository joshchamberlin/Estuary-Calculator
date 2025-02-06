# Alana Santana and Rory Spurr
# Script to run prior to running time series app - plot codes 
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
df <- aggregate(wcr.v$ExpTake, 
                by = list(wcr.v$CommonName, wcr.v$ResultCode, wcr.v$ActMort, wcr.v$ActTake, wcr.v$TakeAction, wcr.v$Species, wcr.v$LifeStage, wcr.v$Prod, 
                          wcr.v$Year, wcr.v$TotalMorts), FUN = sum) 

names(df) <- c("CommonName", "ResultCode", "ActMort", "ActTake", "TakeAction", "ESU", "LifeStage", "Production", "Year", "TotalMorts", "ExpTake")
#df <- df %>%  filter(LifeStage == "Adult", ESU == "Puget Sound Chinook salmon", Production == "Natural") # comment out for real app code
#==============================================================
#Summing each variable by year
YT <-df %>%
  group_by(Year, ESU, Production, LifeStage) %>%
  summarise(Reported_Take = sum(ActTake))
YM <- df %>%
  group_by(Year, ESU, Production, LifeStage) %>%
  summarise(Reported_Mortality = sum(ActMort))
TM <- df %>%
  group_by(Year, ESU, Production, LifeStage) %>%
  summarise(Authorized_Mortality = sum(TotalMorts))
ET <-df %>%
  group_by(Year, ESU, Production, LifeStage) %>%
  summarise(Authorized_Take = sum(ExpTake))
# #==============================================================
# #Merging data sets
Take <- merge(YT, ET, by = c("Year", "ESU", "Production", "LifeStage"))
Mort <- merge(YM, TM, by = c("Year", "ESU", "Production", "LifeStage"))
df1 <- merge(Take, Mort, by = c("Year", "ESU", "Production", "LifeStage"))
#==============================================================
#Replacing NaN or NA or Inf with 0
df1[is.na(df1)] <- 0
#==============================================================
#Creating custom color palette 
mycols <- colors()[c(461, 142, 525, 87)]

#==================================================================================
############################### Stacked bar plot code #############################
# =================================================================================
df1 %>%
  mutate(Authorized_Take_Unused = Authorized_Take - Reported_Take) %>%
  mutate(Authorized_Mortality_Unused = Authorized_Mortality - Reported_Mortality) -> df2

df_TM <- df2 %>%
  gather("Take_Type","N", 5:10) 
#==============================================================
df_TM %>%
  filter(Take_Type %in% c("Reported_Take","Authorized_Take_Unused")) -> df_plot
df_TM %>%
  filter(Take_Type %in% c("Reported_Mortality","Authorized_Mortality_Unused")) -> df_plot2
#==============================================================

# =================================================================================
############################ Percentage bar plot code #############################
# =================================================================================
dp <- df1 %>%
  mutate(Take = (df1$Reported_Take/df1$Authorized_Take)*100) %>%
  mutate(Lethal_Take = (df1$Reported_Mortality/df1$Authorized_Mortality)*100)
#==============================================================
#Merging columns
df_TM1 <- pivot_longer(dp, Lethal_Take:Take, names_to = "Take_Type", values_to = "Percentage")
#==============================================================
df_TM1 %>%
  filter(Take_Type %in% c("Take")) -> dfplot
df_TM1 %>%
  filter(Take_Type %in% "Lethal_Take") -> dfplot2
# =================================================================================
############################ Line graph code #####################################
# =================================================================================
df_l <- df1 %>%
  gather("Take","N", 5:8) 
# #==============================================================
df_l %>%
  filter(Take %in% c("Reported_Take", "Authorized_Take")) -> plot
df_l %>%
  filter(Take %in% c("Reported_Mortality", "Authorized_Mortality")) -> plot2