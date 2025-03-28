yr <- juveniles %>% filter(CommonName == "yelloweye rockfish")
yr$WaterbodyName


bocaccio <- juveniles %>% filter(CommonName == "bocaccio")
bocaccio$WaterbodyName

# seems like waterbody name would be easiest to use/incorporate into a map

theNAjuv <- juveniles %>% filter(is.na(HUCNumber) == T)
the89juv <- juveniles %>% filter(HUCNumber == 99999999)

theNAadult <- adults %>% filter(is.na(HUCNumber) == T)
the89adult <- adults %>% filter(HUCNumber == 99999999)


# Change the "N/A" character string values to NA for computing ease
the89adult <- the89adult %>%
  mutate(across(c(WaterbodyName, StreamName), na_if, "N/A"))

the89juv <- the89juv %>%
  mutate(across(c(WaterbodyName, StreamName), na_if, "N/A"))


# create table displaying all the different water body/stream combinations for 
# 9999999 hucs
waterbody.tab.adult <- table(the89adult$WaterbodyName, the89adult$StreamName, useNA = "always",
                       dnn = c("WaterbodyName", "StreamName"))
waterbody.tab.juv <- table(the89juv$WaterbodyName, the89juv$StreamName, useNA = "always",
                           dnn = c("WaterbodyName", "StreamName"))

waterbody.tab.juv

write.csv(waterbody.tab.adult, "data/Adults_table.csv")
write.csv(waterbody.tab.juv, "data/Juvs_table.csv")

# Need CSVs for messy HUCs
juv.messy.huc <- juveniles %>% 
  filter(HUCNumber == 99999999 | is.na(HUCNumber) == T)
write.csv(juv.messy.huc, "data/juv_messy_huc.csv")

adult.messy.huc <- adults %>%
  filter(HUCNumber == 99999999 | is.na(HUCNumber) == T)
write.csv(adult.messy.huc, "data/adult_messy_huc.csv")



snake <- wbd.hucs %>% filter(huc8 == 10160008) # snake may have changed HUC 8
salmon <- wbd.hucs %>% filter(huc8 == 18010210) # salmon river basin may have its own HUC 8
LC <- wbd.hucs %>% filter(huc8 == 17080006) # lower Columbia has a HUC
clear <- wbd.hucs %>% filter(huc8 == 17060306)

ggplot() +
  geom_sf(data = wcr.bound) +
  geom_sf(data = LC, aes(fill = "red"))


# Making a table to send to Diana
messyHUCtable <- wcr %>% filter(HUCNumber == 99999999 | is.na(HUCNumber) == T)
write.csv(messyHUCtable, "data/all_messy_hucs.csv")





