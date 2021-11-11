library(sf)
library(opentripplanner)
library(tmap)
library(dplyr)
tmap_mode("view")

ntem_cluster <- st_read("data/NTEM/NTEM_centroids_clustered.geojson")

path_data = "D:/OneDrive - University of Leeds/Data/opentripplanner"
path_opt = "D:/OneDrive - University of Leeds/Data/opentripplanner/otp-1.5.0-shaded.jar"


log2 = otp_setup(path_opt,
                 path_data,
                 memory = 120011,
                 router = "great-britain-NTEM",
                 quiet = TRUE,
                 securePort = 8082,
                 open_browser = FALSE,
                 wait = FALSE)

toPlace   = ntem_cluster[rep(seq(1, nrow(ntem_cluster)), times = nrow(ntem_cluster)),]
fromPlace = ntem_cluster[rep(seq(1, nrow(ntem_cluster)), each  = nrow(ntem_cluster)),]

message(Sys.time()," Sleeping during OTP setup")

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
                          mode = c("WALK","BUS","FERRY"),
                          maxWalkDistance = 20000,
                          ncores = 32,
                          distance_balance = TRUE,
                          get_geometry = FALSE)

message(Sys.time()," Killing OTP")

otp_stop(warn = FALSE)

message(Sys.time()," Saving Results")

saveRDS(routes, paste0("data/ttmatrix/routes_bus.Rds"))
