# Rory Spurr
#Pre app code for shiny/leaflet map displaying abundance data

wcr_act <- replace_na(wcr_act, replace = list(ActTake = 0, ActMort = 0))

reported <- aggregate(wcr_act$ActMort,
  by = list(wcr_act$Species, wcr_act$LifeStage, wcr_act$Prod),
  FUN = sum)
names(reported) <- c("Species", "LifeStage", "Production", "ActMort")

abund_all <- aggregate(abund$abundance,
                       by = list(abund$Species, abund$LifeStage),
                       FUN = sum)
all_vec <- rep("All", times = nrow(abund_all))
abund_all <- cbind(abund_all, all_vec)
names(abund_all) <- c("Species", "LifeStage", "abundance", "Production")

abund <- rbind(abund, abund_all)

ActAbund <- right_join(x = abund, y = reported, by = c("Species", "LifeStage", "Production"))
ActAbund <- ActAbund %>%
  mutate(ratio = ActMort/abundance)

quantile(ActAbund$ratio, na.rm = T)



