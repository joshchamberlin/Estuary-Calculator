# Alana Santana and Rory Spurr
# Script to run prior to running time series app
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
df <- df %>%  filter(LifeStage == "Adult", ESU == "Puget Sound Chinook salmon", Production == "Natural") # comment out for real app code
#==============================================================
#Summing each variable by year
YT <-df %>%
  group_by(Year, ESU, Production, LifeStage) %>%
  summarise(Yearly_Take = sum(ActTake))
YM <- df %>%
  group_by(Year, ESU, Production, LifeStage) %>%
  summarise(Yearly_Mort = sum(ActMort))
TM <- df %>%
  group_by(Year, ESU, Production, LifeStage) %>%
  summarise(Yearly_TM = sum(TotalMorts))
ET <-df %>%
  group_by(Year, ESU, Production, LifeStage) %>%
  summarise(Yearly_ET = sum(ExpTake))
# #==============================================================
# #Merging data sets
Take <- merge(YT, ET, by = c("Year", "ESU", "Production", "LifeStage"))
Mort <- merge(YM, TM, by = c("Year", "ESU", "Production", "LifeStage"))
df2 <- merge(Take, Mort, by = c("Year", "ESU", "Production", "LifeStage"))
#df <- merge(df, df2, by = c("Year", "ESU", "Production", "LifeStage"))
#==============================================================
# Add columns for authorized minus reported take and total mortality minus yearly mortality
  # side note - I don't really get what 'total mortality' and 'yearly mortality' represent...
  # is this the authorized lethal take vs. actual lethal take? If so, labeling needs to be fixed.
df2 %>%
  mutate(AuthTakeMinusRepTake = Yearly_ET - Yearly_Take) %>%
  mutate(AuthMortMinusRepMort = Yearly_TM - Yearly_Mort) -> df2
#==============================================================
#Reshape table from wide to long
#df %>%
#  gather("Take_Type","Proportion",9:12) -> df_TM
#==============================================================
df2 %>%
  gather("Take_Type","N",5:10) -> df_TM2
#==============================================================
#df_TM %>%
#  filter(Take_Type %in% c("PropTake","PropNoTake")) -> df_plot

#ggplot(data = df_plot, aes (y = Proportion, x = Year, fill = Take_Type ) ) +
#  geom_bar(stat = "identity", position = "fill")+
#  scale_fill_viridis(discrete = T) +
#  labs(x = "Year", y = "Proportion", title = "Total Authorized Take vs Reported Take over Time")
#ggplotly(tooltip = c("y", "x")) #comment out for real app code
#==============================================================
df_TM2 %>%
  filter(Take_Type %in% c("Yearly_Take","AuthTakeMinusRepTake")) -> df_plot

ggplot(data = df_plot, aes (y = N, x = Year, fill = Take_Type )) +
  geom_bar(stat = "identity", position = "stack")+
  scale_fill_viridis(discrete = T, name = "Take Type", labels = c("Unused Authorized Take", "Reported Take")) +
  labs(x = "Year", y = "Number of fish")
#ggplotly(tooltip = c("y", "x")) 
#==============================================================
df_TM2 %>%
  filter(Take_Type %in% c("Yearly_Mort","AuthMortMinusRepMort")) -> df_plot2

ggplot(data = df_plot2, aes (y = N, x = Year, fill = Take_Type )) +
  geom_bar(stat = "identity", position = "stack")+
  scale_fill_viridis(discrete = T, name = "Take Type", labels = c("Unused Authorized Mortality", "Reported Mortality")) +
  labs(x = "Year", y = "Number of fish")
