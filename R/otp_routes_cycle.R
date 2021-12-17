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
                 memory = 200001,
                 router = "great-britain-NTEM",
                 quiet = FALSE,
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
                     mode = c("WALK","BICYCLE"),
                     maxWalkDistance = 20000,
                     ncores = 30,
                     distance_balance = TRUE,
                     get_geometry = FALSE)
  
  routes <- as.data.frame(routes)
  
  if(nrow(routes) < nrow(fromPlace_sub)){
    # Check for ferry missing
    check <- data.frame(fromPlace = fromPlace_sub$Zone_Code, toPlace = toPlace_sub$Zone_Code)
    check <- left_join(check, routes, by = c("fromPlace","toPlace"))
    toPlace_sub <- toPlace_sub[!is.na(check$duration),]
    fromPlace_sub <- fromPlace_sub[!is.na(check$duration),]
    
    routes2 <- otp_plan(otpcon, 
                        fromPlace = fromPlace_sub,
                        toPlace = toPlace_sub,
                        fromID = fromPlace_sub$Zone_Code,
                        toID = toPlace_sub$Zone_Code, 
                        date_time = lubridate::ymd_hms("2021/10/28 08:00:00"),
                        mode = c("WALK","BICYCLE","FERRY"),
                        maxWalkDistance = 90000,
                        ncores = 30,
                        distance_balance = TRUE,
                        get_geometry = FALSE)
    
    if(!is.null(nrow(routes2))){
      routes2 <- as.data.frame(routes2)
      routes2 <- routes2[,names(routes)]
      routes <- rbind(routes,routes2)
    }
    
  }
  
  message(Sys.time()," Saving Results")
  
  saveRDS(routes, paste0("data/ttmatrix/routes_bike_chunk_",i,"_from_",min(chunk_sub),"_to_",max(chunk_sub),".Rds"))
  rm(routes, toPlace_sub, fromPlace_sub)
  
}


# Bonus Routes

fromPlace <- read_sf("data/NTEM/NTEM_centroids_cycle_from.geojson")
toPlace <- read_sf("data/NTEM/NTEM_centroids_cycle_to.geojson")

routes3 <- otp_plan(otpcon, 
                    fromPlace = fromPlace,
                    toPlace = toPlace,
                    fromID = fromPlace$Zone_Code,
                    toID = toPlace$Zone_Code, 
                    date_time = lubridate::ymd_hms("2021/10/28 08:00:00"),
                    mode = c("BICYCLE"),
                    maxWalkDistance = 90000,
                    ncores = 30,
                    distance_balance = TRUE,
                    get_geometry = FALSE)

check <- data.frame(fromPlace = fromPlace$Zone_Code, toPlace = toPlace$Zone_Code)
check <- left_join(check, routes3, by = c("fromPlace","toPlace"))
toPlace <- toPlace[!is.na(check$duration),]
fromPlace <- fromPlace[!is.na(check$duration),]

routes4 <- otp_plan(otpcon, 
                    fromPlace = fromPlace,
                    toPlace = toPlace,
                    fromID = fromPlace$Zone_Code,
                    toID = toPlace$Zone_Code, 
                    date_time = lubridate::ymd_hms("2021/10/28 08:00:00"),
                    mode = c("BICYCLE","FERRY"),
                    maxWalkDistance = 90000,
                    ncores = 30,
                    distance_balance = TRUE,
                    get_geometry = FALSE)

if(!is.null(nrow(routes4))){
  routes4 <- as.data.frame(routes4)
  routes4 <- routes4[,names(routes3)]
  routes3 <- rbind(routes3,routes4)
}


saveRDS(routes3, paste0("data/ttmatrix/routes_cycle_bonus.Rds"))




message(Sys.time()," Killing OTP")

otp_stop(warn = FALSE)


# Read in routes
files <- list.files("data/ttmatrix/", pattern = "routes_bike_chunk_", full.names = TRUE)

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

