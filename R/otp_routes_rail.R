library(sf)
library(opentripplanner)
library(tmap)
library(dplyr)
tmap_mode("view")

ntem_cluster <- st_read("data/NTEM/NTEM_centroids_clustered_rail.geojson")

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
                     mode = c("WALK","TRAM","RAIL","SUBWAY","FERRY"),
                     maxWalkDistance = 10000,
                     ncores = 25,
                     distance_balance = TRUE,
                     get_geometry = FALSE)
  
  message(Sys.time()," Saving Results")
  
  saveRDS(routes, paste0("data/ttmatrix/routes_rail_chunk_",i,"_from_",min(chunk_sub),"_to_",max(chunk_sub),".Rds"))
  rm(routes, toPlace_sub, fromPlace_sub)
  
}


message(Sys.time()," Killing OTP")

otp_stop(warn = FALSE)


# Read in routes
files <- list.files("data/ttmatrix/", pattern = "routes_rail_chunk_", full.names = TRUE)

res <- list()
for(i in 1:length(files)){
  sub <- readRDS(files[i])
  sub <- sub %>% 
    group_by(fromPlace, toPlace) %>%
    summarise(duration = min(duration))
  res[[i]] <- sub
}
res <- bind_rows(res)

mat_route <- data.frame(fromPlace = rep(unique(res$fromPlace), each = length(unique(res$toPlace))),
                        toPlace = rep(unique(res$toPlace), times = length(unique(res$fromPlace))))
mat_route <- left_join(mat_route, res, by = c("fromPlace","toPlace"))
mat_route <- matrix(mat_route$duration, ncol = length(unique(res$fromPlace)),
                    dimnames = list(unique(mat_route$toPlace), unique(mat_route$fromPlace)))

#mat_route[56,87, drop = FALSE]
#res[res$fromPlace == "S99900452" & res$toPlace == "E02002702",]
#mat_route[mat_route$fromPlace == "W02000117" & mat_route$toPlace == "E02000059",]

# mat_route <- stplanr::od_to_odmatrix(res)
# mat_route <- t(mat_route)

# Read tin ttmatrix
files <- list.files("data/ttmatrix/", pattern = "ttmatrix_rail_chunk_", full.names = TRUE)

res <- list()
for(i in 1:length(files)){
  res[[i]] <- readRDS(files[i])
}

mat_tt <- res[[1]]
for(i in 2:length(res)){
  mat_tt <- cbind(mat_tt, res[[i]])
}
mat_tt <- as.matrix(mat_tt)
# Merge matrixes

mat_na <- matrix(NA, nrow = nrow(mat_route), ncol = ncol(mat_tt) - ncol(mat_route))
colnames(mat_na) <- colnames(mat_tt)[!colnames(mat_tt) %in% colnames(mat_route)]

mat_route <- cbind(mat_route, mat_na)

mat_na <- matrix(NA, ncol = ncol(mat_route), nrow = nrow(mat_tt) - nrow(mat_route))
rownames(mat_na) <- rownames(mat_tt)[!rownames(mat_tt) %in% rownames(mat_route)]

mat_route <- rbind(mat_route, mat_na)

summary(as.numeric(mat_route))
summary(as.numeric(mat_tt))

mat_route <- mat_route[rownames(mat_tt), colnames(mat_tt)]
summary(rownames(mat_tt) == rownames(mat_route))
summary(colnames(mat_tt) == colnames(mat_route))
mat <- ifelse(is.na(mat_tt), mat_route, mat_tt)
summary(as.numeric(mat))

newmat <- odmatrix_interpolate(mat, 1, 10) # 4h + long distance 2 hours




cents <- read_sf("data/NTEM/NTEM_centroids_mod.geojson")
col = "E02000655"

times <- mat[col,]
times <- data.frame(Zone_Code = names(times), duration = times)
cents2 <- left_join(cents, times, by = c("Zone_Code"))
tm_shape(cents2) +
  tm_dots(col = "duration") +
  tm_shape(cents2[cents2$Zone_Code == col, ]) +
  tm_dots(col = "red")

cents2$newtimes <- combined
tm_shape(cents2) +
  tm_dots(col = "newtimes") +
  tm_shape(cents2[cents2$Zone_Code == col, ]) +
  tm_dots(col = "red")



cents2 <- left_join(ntem_cluster, res[res$fromPlace == "W02000265", ], by = c("Zone_Code" = "toPlace"))
cents2$duration <- cents2$duration / 3600

tm_shape(cents2) +
  tm_dots(col = "duration") +
  tm_shape(cents2[cents2$Zone_Code == "W02000265", ]) +
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




