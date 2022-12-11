library(sf)
library(opentripplanner)
library(tmap)
library(dplyr)
tmap_mode("view")

cities <- st_read("data/example_cities.geojson")

path_data = "D:/OneDrive - University of Leeds/Data/opentripplanner"
path_opt = "D:/OneDrive - University of Leeds/Data/opentripplanner/otp-1.5.0-shaded.jar"


log2 = otp_setup(path_opt,
                 path_data,
                 memory = 200011,
                 router = "great-britain-NTEM",
                 quiet = TRUE,
                 securePort = 8082,
                 open_browser = FALSE,
                 wait = FALSE)

message(Sys.time()," Sleeping during OTP setup")

toPlace   = cities[rep(seq(1, nrow(cities)), times = nrow(cities)),]
fromPlace = cities[rep(seq(1, nrow(cities)), each  = nrow(cities)),]

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
                   fromID = fromPlace$query,
                   toID = toPlace$query, 
                   date_time = lubridate::ymd_hms("2021/10/28 08:00:00"),
                   mode = c("WALK","TRANSIT"),
                   maxWalkDistance = 10000,
                   ncores = 10,
                   distance_balance = TRUE,
                   get_geometry = TRUE)
  
message(Sys.time()," Saving Results")
  
saveRDS(routes, paste0("data/routes_transit_example_cities.Rds"))

  
