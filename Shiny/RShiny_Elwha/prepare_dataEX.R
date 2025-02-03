# Prepare compressed pre-loaded data for the app

library(dplyr)
library(sf)

# GEO data ----
# # load in drainage basin shapefile
elwha_wbd <- st_read("../data/geo/WBD_Elwha.shp") %>%
  st_transform('+proj=longlat +datum=WGS84')

elwha_streams <- st_read("../data/shapefiles/elwha_streams_nhd2_vat.shp") %>%
   st_transform('+proj=longlat +datum=WGS84')
 elwha_streams <- st_zm(elwha_streams)
 elwha_df <- st_as_sf(elwha_streams)

elwha_sites <- st_read('../data/shapefiles/elwha_sites_nhd2.shp') %>%
  st_transform('+proj=longlat +datum=WGS84')
elwha_sites <- st_zm(elwha_sites)
elwha_sites$Site[elwha_sites$Site %in% 'MS16-5'] <- "MS16_5"

elwha_boundaries <- st_read("../data/shapefiles/elwha_boundary.shp") %>%
  st_transform('+proj=longlat +datum=WGS84')
elwha_boundaries <- st_zm(elwha_boundaries)

colnames(elwha_sites)[5] <- "FW_Site"
colnames(elwha_sites)[4] <- "Site_2"
colnames(elwha_sites)[3] <- "Site"

sites_df <- as.data.frame(elwha_sites)[,-which(colnames(elwha_sites) %in% "geometry")]
sites_df$FW_Site[sites_df$FW_Site %in% "EMMSEJ"] <- "ELMSEJ"
sites_df$SECTION[sites_df$FW_Site %in% "ELMSEJ"] <-  "Lower River"
sites_df$LAT[sites_df$FW_Site %in% "ELMSEJ"] <- 48.126080
sites_df$LONG[sites_df$FW_Site %in% "ELMSEJ"] <- -123.557560
sites_df$FW_Site[sites_df$FW_Site == "EMMSFH"] <- "ELMSFH"
sites_df$SECTION[sites_df$FW_Site == "ELMSFH"] <-  "Lower River"
sites_df$FW_Site[sites_df$FW_Site %in% "EMMSFW"] <- "ELMSFW"
sites_df$SECTION[sites_df$FW_Site %in% "ELMSFW"] <-  "Lower River"
sites_df$LAT[sites_df$FW_Site %in% "ELMSFW"] <- 48.104153
sites_df$LONG[sites_df$FW_Site %in% "ELMSFW"] <- -123.553309

save(elwha_streams, elwha_sites, elwha_boundaries, sites_df, file = "data/elwha_geo.RData")

site_names <- unique(elwha_sites$Site)
fwsite_names <- unique(elwha_sites$FW_Site)


# Empirical tmperature data & sites ----

stdata_raw <- data.table::fread("../data/elwha.wt.2000-2024.csv")
stdata_raw$Date <- as.Date(stdata_raw$DateTime)

stdata_daily <- stdata_raw %>%
  dplyr::group_by(Date, Site) %>%
  dplyr::summarise(Temp = mean(Temp, na.rm = T)) 

st_wide <- stdata_daily %>% tidyr::pivot_wider(id_cols = "Date", names_from = "Site", values_from = "Temp", values_fill = NA)
st_wide <- as.data.frame(st_wide)

stdata_daily_fw <- stdata_raw %>%
  dplyr::group_by(Date, FW_Site) %>%
  dplyr::summarise(Temp = mean(Temp, na.rm = T)) 
st_wide_fw <- stdata_daily_fw %>% tidyr::pivot_wider(id_cols = "Date", names_from = "FW_Site", values_from = "Temp", values_fill = NA)
st_wide_fw <- as.data.frame(st_wide_fw)
st_wide_fw <- st_wide_fw[,-which(colnames(st_wide_fw) %in% "NA")]


# add COMID
si <- as.data.frame(elwha_sites[, c("Site", "FW_Site", "COMID", "LAT", "LONG", "SECTION", "HABITAT", "ELEVATION", "RKM", "TotDASqKM")])[,1:10]
sd <- as.data.frame(stdata_daily)
sf <- as.data.frame(stdata_daily_fw)
merged_data <- dplyr::left_join(sd, si, by = "Site")
merged_data_fw <- dplyr::left_join(sf, si, by = "FW_Site")
merged_data_fw <- merged_data_fw[!is.na(merged_data_fw$FW_Site),]


# Predicted stream temperature data ----
predst <- data.table::fread("../data/st_pred_171100/st_pred_1711002005.csv")
predst <- predst[, c("tim.date", "COMID", "prd.stream_temp")]
predst <- predst %>%
  dplyr::group_by(tim.date, COMID,) %>%
  dplyr::summarise("prd.stream_temp" = mean(prd.stream_temp, na.rm = T)) 
predst$prd.stream_temp <- round(predst$prd.stream_temp, 2)
colnames(predst)[1] <- "Date"
predst$Date <- as.Date(predst$Date)
predst <- as.data.frame(predst)

# merge empirical and predictions
intersect(unique(merged_data$COMID), unique(predst$COMID))
setdiff(unique(merged_data$COMID), unique(predst$COMID))

data_obs_prd <- dplyr::left_join(merged_data, predst, by = c("COMID", "Date"))

cids <- sort(unique(data_obs_prd$COMID))
data_prd <- predst[predst$COMID %in% cids,]
data_prd_obs <- dplyr::left_join(data_prd, si, by = "COMID")
data_prd_obs <- dplyr::left_join(data_prd_obs, merged_data[, c("Site", "Date", "Temp")], by = c("Site", "Date"))
data_prd_obs <- data_prd_obs[,colnames(data_obs_prd)]
merged_data <- unique(rbind(data_obs_prd, data_prd_obs))

# save elwha data
save(merged_data, st_wide, st_wide_fw, site_names, fwsite_names, file = "data/stemp_data.RData")


