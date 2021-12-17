# Inputs
library(dplyr)
library(tmap)
library(sf)
tmap_mode("view")


ttmat_text = "ttmatrix_cycle_chunk_"
routes_text = "routes_cycle_"
out_text = "data/ttmatrix/final/cycle.csv"
threshold = 1
with_routes = TRUE

if(with_routes){
  # Read in routes
  files <- list.files("data/ttmatrix/", pattern = routes_text, full.names = TRUE)
  
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
  mat_route <- t(stplanr::od_to_odmatrix(mat_route))
  
  # mat_route <- matrix(mat_route$duration, ncol = length(unique(res$fromPlace)),
  #                     dimnames = list(unique(mat_route$toPlace), unique(mat_route$fromPlace)))
}

# Read tin ttmatrix
files <- list.files("data/ttmatrix/", pattern = ttmat_text, full.names = TRUE)

res <- list()
for(i in 1:length(files)){
  res[[i]] <- readRDS(files[i])
}

mat_tt <- res[[1]]
for(i in 2:length(res)){
  mat_tt_sub <- res[[i]]
  mat_tt_sub <- mat_tt_sub[rownames(mat_tt),]
  mat_tt <- cbind(mat_tt, mat_tt_sub)
}
mat_tt <- as.matrix(mat_tt)

if(with_routes){
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
} else {
  mat = mat_tt
}



source("R/batch_check_travel_times.R")

ntem <- st_read("data/NTEM/NTEM_centroids_mod.geojson")
ntem <- ntem[match(rownames(mat), ntem$Zone_Code),]
ntem$times <- mat[,"E02003367"]
qtm(ntem, dots.col = "times")

newmat <- odmatrix_interpolate(mat, threshold) # 4h
newmat <- odmatrix_interpolate(newmat, threshold) # 8h
newmat <- odmatrix_interpolate(newmat, threshold) # 16h
newmat <- odmatrix_interpolate(newmat, threshold) # 32h
newmat <- odmatrix_interpolate(newmat, threshold) # 64h
newmat <- odmatrix_interpolate(newmat, threshold) # 128h
newmat <- odmatrix_interpolate(newmat, threshold) # 256h
newmat4 <- newmat
nmna <- as.logical(is.na(newmat4))
message(sum(nmna)/length(nmna))

ntem <- st_read("data/NTEM/NTEM_centroids_mod.geojson")
ntem <- ntem[match(rownames(newmat4), ntem$Zone_Code),]
csum <- colSums(is.na(newmat4))
ntem$csum <- csum
ntem <- ntem[ntem$csum > min(ntem$csum, na.rm = TRUE),]
qtm(ntem, dots.col = "csum")

ntem <- st_read("data/NTEM/NTEM_centroids_mod.geojson")
ntem <- ntem[match(rownames(newmat4), ntem$Zone_Code),]
ntem$times <- newmat4[,"S99900325"]
qtm(ntem, dots.col = "times") + qtm(ntem[ntem$Zone_Code == "S99900325",], dots.col = "red")

# Fixes for the Islands
ferry_fix <- function(old, new, extra){
  col <- newmat4[,old]
  col <- ifelse(is.na(col), newmat4[,new] + extra, col)
  col
}

# Isle of Scilly
#newmat4[,"E02006781"] <- ferry_fix("E02006781","E02003949", 9900)
#newmat4[,"E02003949"] <- ferry_fix("E02003949","E02006781", 9900)

# Outer Hebrides
#newmat4[,"S99900328"] <- ferry_fix("S99900328","S99900119", 8400)
newmat4[,"S99900498"] <- ferry_fix("S99900498","S99900119", 19380)
newmat4[,"S99900499"] <- ferry_fix("S99900499","S99900119", 20820)
newmat4[,"S99900207"] <- ferry_fix("S99900207","S99900119", 20820)

# newmat4[,"S99900119"] <- ferry_fix("S99900119","S99900328", 8400)
# newmat4[,"S99900119"] <- ferry_fix("S99900119","S99900498", 19380)
# newmat4[,"S99900119"] <- ferry_fix("S99900119","S99900499", 20820)
# newmat4[,"S99900119"] <- ferry_fix("S99900119","S99900207", 20820)
# 
# newmat4[,"S99900141"] <- ferry_fix("S99900141","S99900207", 5400)
# newmat4[,"S99900027"] <- ferry_fix("S99900027","S99900126", 5400)

#Shetland
# newmat4[,"S99900179"] <- ferry_fix("S99900179","S99900180", 1560)
# newmat4[,"S99900163"] <- ferry_fix("S99900163","S99900123", 9000)
# newmat4[,"S99900164"] <- ferry_fix("S99900164","S99900123", 9000)
# newmat4[,"S99900180"] <- ferry_fix("S99900180","S99900289", 46800)

newmat5 <- odmatrix_interpolate(newmat4, 1) # Final Pass
nmna <- as.logical(is.na(newmat5))
message(sum(nmna)/length(nmna))

ntem <- st_read("data/NTEM/NTEM_centroids_mod.geojson")
ntem <- ntem[match(rownames(newmat5), ntem$Zone_Code),]
csum <- colSums(is.na(newmat5))
ntem$csum <- csum
ntem <- ntem[ntem$csum > min(ntem$csum, na.rm = TRUE),]
qtm(ntem, dots.col = "csum")


ntem <- st_read("data/NTEM/NTEM_centroids_mod.geojson")
ntem <- ntem[match(rownames(newmat5), ntem$Zone_Code),]
ntem$times <- newmat5[,"E02004199"]
qtm(ntem, dots.col = "times")


# fill in with reverse direction

mat_t <- t(newmat5)
newmat6 <- ifelse(is.na(newmat5), mat_t, newmat5)

ntem <- st_read("data/NTEM/NTEM_centroids_mod.geojson")
ntem <- ntem[match(rownames(newmat6), ntem$Zone_Code),]
csum <- colSums(is.na(newmat6))
ntem$csum <- csum
ntem <- ntem[ntem$csum > min(ntem$csum, na.rm = TRUE),]
qtm(ntem, dots.col = "csum")

ntem <- st_read("data/NTEM/NTEM_centroids_mod.geojson")
ntem <- ntem[match(rownames(newmat6), ntem$Zone_Code),]
ntem$times <- newmat6[,"S99900487"]
qtm(ntem, dots.col = "times")

newmat7 <- odmatrix_interpolate(newmat6, 1) 

ntem <- st_read("data/NTEM/NTEM_centroids_mod.geojson")
ntem <- ntem[match(rownames(newmat7), ntem$Zone_Code),]
csum <- colSums(is.na(newmat7))
ntem$csum <- csum
ntem <- ntem[ntem$csum > min(ntem$csum, na.rm = TRUE),]
qtm(ntem, dots.col = "csum")

ntem <- st_read("data/NTEM/NTEM_centroids_mod.geojson")
ntem <- ntem[match(rownames(newmat7), ntem$Zone_Code),]
ntem$times <- newmat6[,"S99900487"]
qtm(ntem, dots.col = "times")

write.csv(newmat7,out_text)
# 
# # fill in with reverse direction
# 
# mat_t <- t(newmat5)
# newmat6 <- ifelse(is.na(newmat5), mat_t, newmat5)
# 
# # ferry_fix <- function(old, new, extra){
# #   col <- newmat6[,old]
# #   col <- ifelse(is.na(col), newmat6[,new] + extra, col)
# #   col
# # }
# # 
# # nmna <- as.logical(is.na(newmat6))
# # message(sum(nmna)/length(nmna))
# # csum <- colSums(is.na(newmat6))
# # csum[csum > 2]
# # ntem <- st_read("data/NTEM/NTEM_centroids_mod.geojson")
# # ntem <- ntem[match(rownames(newmat6), ntem$Zone_Code),]
# # ntem$csum <- csum
# # ntem <- ntem[ntem$csum > 2,]
# # qtm(ntem, dots.col = "csum")
# # 
# # newmat6[,"S99900179"] <- ferry_fix("S99900179","S99900180", 1560)
# # 
# # mat_t <- t(newmat6)
# # newmat7 <- ifelse(is.na(newmat6), mat_t, newmat6)
# newmat8 <- odmatrix_interpolate(newmat6, 1) # Final Pass
# 
# 
# ntem <- st_read("data/NTEM/NTEM_centroids_mod.geojson")
# ntem <- ntem[match(rownames(newmat8), ntem$Zone_Code),]
# csum <- colSums(is.na(newmat8))
# ntem$csum <- csum
# ntem <- ntem[ntem$csum > min(ntem$csum, na.rm = TRUE),]
# qtm(ntem, dots.col = "csum")
# 
# 
# ntem <- st_read("data/NTEM/NTEM_centroids_mod.geojson")
# ntem <- ntem[match(rownames(newmat8), ntem$Zone_Code),]
# ntem$times <- newmat8[,"E02003941"]
# qtm(ntem, dots.col = "times") +
#   qtm(ntem[ntem$Zone_Code == "E02003941",], dots.col = "red")
# 
# write.csv(newmat8,out_text)
# 
# 
# nmna <- as.logical(is.na(newmat7))
# message(sum(nmna)/length(nmna))
# 
# csum <- colSums(is.na(newmat7))
# 
# write.csv(newmat7,out_text)
# 
# 
# # check for missing
# ntem <- st_read("data/NTEM/NTEM_centroids_mod.geojson")
# ntem <- ntem[match(rownames(newmat7), ntem$Zone_Code),]
# 
# 
# ntem$times <- newmat8[,"E02006298"]
# qtm(ntem, dots.col = "times")
# 
# csum <- colSums(is.na(newmat7))
# ntem$csum <- csum[match(ntem$Zone_Code, names(csum))]
# 
# rsum <- rowSums(is.na(newmat8))
# ntem$rsum <- rsum[match(ntem$Zone_Code, names(rsum))]
# 
# tm_shape(ntem) +
# tm_dots(col = "rsum", style = "fixed", breaks = c(0,33,50,100,7699,7700))
# 
# 
# rsum <- rowSums(is.na(newmat7))
# rsum <- rsum[rsum > 0] 
# qtm(ntem[ntem$Zone_Code %in% names(rsum),])
