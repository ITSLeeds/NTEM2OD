library(sf)
library(opentripplanner)
library(tmap)
library(dplyr)
tmap_mode("view")

ntem_cluster <- st_read("data/NTEM/NTEM_centroids_clustered.geojson")
ntem <- st_read("data/NTEM/NTEM_centroids_mod.geojson")

# Places driving fails

car_fail = c("E02006781", "E02003597", "E02003596", "E02003595", "E02003598", "E02003592",
             "E02003593", "E02003589", "E02003591", "E02003584",
             "E02003594", "E02003590", "E02003588", "E02003587", "E02003586", "E02003585",
             "E02003583", "E02003582", "E02003581", "S99900028",
             "S99900279", "S99900328", "S99900498", "S99900499", "S99900207", "S99900125",
             "S99900123", "S99900163", "S99900164", "S99900180",
             "S99900179")

car_fail = ntem[ntem$Zone_Code %in% car_fail,]

path_data = "D:/OneDrive - University of Leeds/Data/opentripplanner"
path_opt = "D:/OneDrive - University of Leeds/Data/opentripplanner/otp-1.5.0-shaded.jar"


log2 = otp_setup(path_opt,
                 path_data,
                 memory = 120011,
                 router = "great-britain-NTEM-drive",
                 quiet = TRUE,
                 securePort = 8082,
                 open_browser = FALSE,
                 wait = FALSE)

message(Sys.time()," Sleeping during OTP setup")

toPlace   = ntem_cluster[rep(seq(1, nrow(ntem_cluster)), times = nrow(car_fail)),]
fromPlace = car_fail[rep(seq(1, nrow(car_fail)), each  = nrow(ntem_cluster)),]
toPlace$cluster <- NULL

toPlace2 <- rbind(toPlace, fromPlace)
fromPlace2 <- rbind(fromPlace, toPlace)

toPlace  <- toPlace2
fromPlace <- fromPlace2

chunks <- split(1:nrow(toPlace), ceiling(seq_along(1:nrow(toPlace))/(2000)))



for(j in 1:100){
  otpcon <- try(otp_connect(router = "great-britain-NTEM-drive"), silent = TRUE)
  if(class(otpcon)[1] == "try-error"){
    message("Round ",j," sleeping for another minute")
    Sys.sleep(60)
  } else {
    break
  }
}

message(Sys.time()," Starting routing")

for(i in 1:length(chunks)){
  chunk_sub <- chunks[[i]]
  
  message(Sys.time()," Stage ", i," from ",min(chunk_sub)," to ",max(chunk_sub))
  
  toPlace_sub <- toPlace[chunk_sub, ]
  fromPlace_sub <- fromPlace[chunk_sub, ]
  
  routes <- otp_plan(otpcon, 
                     fromPlace = fromPlace_sub,
                     toPlace = toPlace_sub,
                     fromID = fromPlace_sub$Zone_Code,
                     toID = toPlace_sub$Zone_Code, 
                     date_time = lubridate::ymd_hms("2021/10/28 08:00:00"),
                     mode = c("CAR"),
                     maxWalkDistance = 10000,
                     ncores = 25,
                     distance_balance = TRUE,
                     get_geometry = FALSE)
  
  message(Sys.time()," Saving Results")
  
  saveRDS(routes, paste0("data/ttmatrix/routes_car_chunk_",i,"_from_",min(chunk_sub),"_to_",max(chunk_sub),".Rds"))
  rm(routes, toPlace_sub, fromPlace_sub)
  
}


  
