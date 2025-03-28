

LocGroup1 <- c("Admiralty Inlet", "North Puget Sound", "South Puget Sound",
               "Whidbey Basin", "Puget Sound")

LocGroup2 <- "Hood Canal"

LocGroup3 <- "Strait of Juan de Fuca"
# AllLocs <- c(LocGroup1, LocGroup2, LocGroup3)

N <- length(wcr$Location)
for (i in 1:N){
  if (wcr$Location[i] %in% LocGroup1) {wcr$HUCNumber[i] <- 17110019} 
  if (wcr$Location[i] == "Hood Canal") {wcr$HUCNumber[i] <- 17110018} 
  if (wcr$Location[i] == "Strait of Juan de Fuca") {wcr$HUCNumber[i] <- 17110021} # also another one given (17110020)
}

test <- wcr %>% filter(Location %in% AllLocs)

test2 <- wcr %>% filter(HUCNumber == 99999999)
unique(test2$FileNumber)
