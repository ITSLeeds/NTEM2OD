library(UK2GTFS)
library(nngeo)
library(tmap)
library(opentripplanner)
library(sf)
library(dplyr)

# SOmething odd about newcastle metro

gtfs <- gtfs_read("D:/OneDrive - University of Leeds/Data/opentripplanner/graphs/great-britain-NTEM/NE_20211012.zip")

routes <- gtfs$routes
routes <- routes[routes$route_type == 1,]

trips <- gtfs$trips
trips <- trips[trips$route_id %in% routes$route_id,]

gtfs_rail <- gtfs_split_ids(gtfs, trip_ids = trips$trip_id)
gtfs_rail <- gtfs_rail$true

foo <- UK2GTFS:::gtfs_trips_sf(gtfs_rail)
qtm(foo)

stop_times <- gtfs_rail$stop_times
stops <- UK2GTFS:::gtfs_stops_sf(gtfs_rail)


ntem_cents = st_read("data/NTEM/NTEM_centroids_mod.geojson")

nn <- st_nn(stops, ntem_cents, k = 1)
ntem_rail <- ntem_cents[unlist(nn),]
ntem_rail <- ntem_rail[!duplicated(ntem_rail$Zone_Code),]

main_stops <- read_sf("data/NTEM/NTEM_centroids_rail_to.geojson")
main_stops <- main_stops %>%
  group_by(Zone_Code, rural_urban, region) %>%
  summarise(geometry = geometry[1],
    n = n())
main_stops <- main_stops[main_stops$n > 2000,]
qtm(main_stops)

fromPlace <- ntem_rail[rep(1:nrow(ntem_rail), times = nrow(ntem_rail)),]
toPlace <- ntem_rail[rep(1:nrow(ntem_rail), each = nrow(ntem_rail)),]

fromPlace2 <- ntem_rail[rep(1:nrow(ntem_rail), times = nrow(main_stops)),]
toPlace2 <- main_stops[rep(1:nrow(main_stops), each = nrow(ntem_rail)),]

toPlace2$n <- NULL

fromPlace <- rbind(fromPlace, fromPlace2)
toPlace <- rbind(toPlace, toPlace2)
nrow(fromPlace) == nrow(toPlace)

for(j in 1:100){
  otpcon <- try(otp_connect(router = "great-britain-NTEM"), silent = TRUE)
  if(class(otpcon)[1] == "try-error"){
    message("Round ",j," sleeping for another minute")
    Sys.sleep(60)
  } else {
    break
  }
}

message(Sys.time()," Starting routing")

routes <- otp_plan(otpcon, 
                   fromPlace = fromPlace,
                   toPlace = toPlace,
                   fromID = fromPlace$Zone_Code,
                   toID = toPlace$Zone_Code, 
                   date_time = lubridate::ymd_hms("2021/10/28 08:00:00"),
                   mode = c("WALK","TRAM","RAIL","SUBWAY","FERRY"),
                   maxWalkDistance = 5000,
                   ncores = 30,
                   distance_balance = TRUE,
                   get_geometry = TRUE)

routes <- st_drop_geometry(routes)

saveRDS(routes, paste0("data/ttmatrix/routes_rail_newcastle.Rds"))

qtm(routes[1:500,], lines.col = "mode", lines.lwd = 3)


