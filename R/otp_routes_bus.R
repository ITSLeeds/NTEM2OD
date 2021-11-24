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

message(Sys.time()," Sleeping during OTP setup")

toPlace   = ntem_cluster[rep(seq(1, nrow(ntem_cluster)), times = nrow(ntem_cluster)),]
fromPlace = ntem_cluster[rep(seq(1, nrow(ntem_cluster)), each  = nrow(ntem_cluster)),]

chunks <- split(1:nrow(toPlace), ceiling(seq_along(1:nrow(toPlace))/(2000)))



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

for(i in 59:length(chunks)){
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
                     mode = c("WALK","BUS","FERRY"),
                     maxWalkDistance = 10000,
                     ncores = 25,
                     distance_balance = TRUE,
                     get_geometry = FALSE)
  
  message(Sys.time()," Saving Results")
  
  saveRDS(routes, paste0("data/ttmatrix/routes_bus_v2_chunk_",i,"_from_",min(chunk_sub),"_to_",max(chunk_sub),".Rds"))
  rm(routes, toPlace_sub, fromPlace_sub)
  
}




message(Sys.time()," Killing OTP")

otp_stop(warn = FALSE)


# Read in routes
files <- list.files("data/ttmatrix/", pattern = "routes_bus_v2_chunk_", full.names = TRUE)

res <- list()
for(i in 1:length(files)){
  sub <- readRDS(files[i])
  sub <- sub %>% 
    group_by(fromPlace, toPlace) %>%
    summarise(duration = min(duration))
  res[[i]] <- sub
}
res <- bind_rows(res)

mat <- stplanr::od_to_odmatrix(res)
mat <- t(mat)

cents2 <- left_join(ntem_cluster, res[res$fromPlace == "E02004004", ], by = c("Zone_Code" = "toPlace"))
cents2$duration <- cents2$duration / 3600

tm_shape(cents2) +
  tm_dots(col = "duration") +
  tm_shape(cents2[cents2$Zone_Code == "E02004004", ]) +
  tm_dots(col = "red")


# Count matches 
rcount <- rowSums(!is.na(mat))
rcount <- data.frame(Zone_Code = names(rcount), rcount = rcount)
cents2 <- left_join(ntem_cluster, rcount, by = c("Zone_Code"))
tm_shape(cents2) +
  tm_dots(col = "rcount")

ccount <- colSums(!is.na(mat))
ccount <- data.frame(Zone_Code = names(ccount), ccount = ccount)
cents2 <- left_join(ntem_cluster, ccount, by = c("Zone_Code"))
tm_shape(cents2) +
  tm_dots(col = "ccount")

