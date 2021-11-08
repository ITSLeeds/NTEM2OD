files = list.files("data/ttmatrix", pattern = "ttmatrix_car_chunk_", full.names = TRUE)

res <- list()
for(i in 1:length(files)){
  res[[i]] <- readRDS(files[i])
}

mat <- res[[1]]
for(i in 2:length(res)){
  mat <- cbind(mat, res[[i]])
}

mat <- mat[rownames(mat) %in% colnames(mat),]
mat <- as.matrix(mat)
mat <- mat[colnames(mat),]

odmatrix_interpolate <- function(mat, 
                                 ncores = 30,
                                 threshold = 10){
  
  cl <- parallel::makeCluster(ncores)
  newmat <- pbapply::pblapply(1:ncol(mat), 
                              mat_inter2, 
                              mat = mat,
                              threshold = threshold,
                              cl = cl)
  parallel::stopCluster(cl)
  
  newmat <- unlist(newmat)
  newmat <- matrix(newmat, ncol = ncol(mat))
  rownames(newmat) <- rownames(mat)
  colnames(newmat) <- colnames(mat)
  return(newmat)
}

mat_inter2 <- function(col, mat, threshold){
  # Select on column
  times_to_midpoints <- mat[col, ]
  times_to_midpoints_isna <- is.na(times_to_midpoints)
  
  # Subset matrix
  with_times <- times_to_midpoints[!times_to_midpoints_isna]
  mat_sub <- mat[,!times_to_midpoints_isna, drop = FALSE]
  
  # To have confidence in estimate want to pick from multiple options
  fun1 <- function(x, threshold){
    sum(!is.na(x)) >= threshold
  }
  
  rsum_count <- apply(mat_sub, 1, 
                      fun1, 
                      threshold = threshold)
  
  mat_sub = mat_sub[rsum_count, , drop = FALSE]
  
  if(length(mat_sub) == 0){
    return(times_to_midpoints)
  } else {
    # Add travel time to midpoints to overall travel times
    mat_sub = sweep(mat_sub, 2, with_times, "+")
    # Find shortest travel time
    rsum <- apply(mat_sub, 1, min, na.rm = TRUE)
    rsum[is.infinite(rsum)] <- NA
    
    rsum <- rsum[match(names(times_to_midpoints), names(rsum))]
    
    combined <- ifelse(times_to_midpoints_isna, rsum, times_to_midpoints)
    combined <- unname(combined)
    return(combined)
  }

}

newmat <- odmatrix_interpolate(mat, 30, 10) # 4h
newmat2 <- odmatrix_interpolate(newmat, 30, 10) # 8h
newmat3 <- odmatrix_interpolate(newmat2, 30, 10) # 16h
newmat4 <- odmatrix_interpolate(newmat3, 30, 10) # 32h
newmat5 <- odmatrix_interpolate(newmat4, 30, 2) # Low accucarcy pass for hard to reach places

saveRDS(newmat5,"data/ttmatrix/ttmatrix_car_full.Rds")

summary(is.na(as.numeric(newmat4)))

foo <- pbapply::pblapply(7494, 
                            mat_inter2, 
                            mat = mat,
                            threshold = 10)


# newmat <- list()
# # Loop over each column
# for(col in  1:ncol(mat)){
#   if(col %% 20 == 0){
#     message("col ",col)
#   }
#   
#   # Select on column
#   times_to_midpoints <- mat[col, ]
#   times_to_midpoints_isna <- is.na(times_to_midpoints)
#   
#   # Subset matrix
#   with_times <- times_to_midpoints[!times_to_midpoints_isna]
#   mat_sub <- mat[,!times_to_midpoints_isna, drop = FALSE]
#   
#   # Add travel time to midpoints to overall travel times
#   mat_sub = sweep(mat_sub, 2, with_times, "+")
#   # Find shortest travel time
#   rsum <- apply(mat_sub, 1, min, na.rm = TRUE)
#   
#   # To have confidence in estimate want to pick from multiple options
#   fun1 <- function(x){
#     sum(!is.na(x))
#   }
#   
#   rsum_count <- apply(mat_sub, 1, fun1)
#   rsum[rsum_count <= 10] <- NA
#   rsum[is.infinite(rsum)] <- NA
#   
#   combined <- ifelse(times_to_midpoints_isna, rsum, times_to_midpoints)
#   newmat[[col]] <- combined
# }
# 
# 
# 
# # Alt function, for larger columsn
# mat_inter <- function(col, mat, threshold){
#   # Select on column
#   times_to_midpoints <- mat[col, ]
#   times_to_midpoints_isna <- is.na(times_to_midpoints)
#   
#   # Subset matrix
#   with_times <- times_to_midpoints[!times_to_midpoints_isna]
#   mat_sub <- mat[,!times_to_midpoints_isna, drop = FALSE]
#   
#   # Add travel time to midpoints to overall travel times
#   mat_sub = sweep(mat_sub, 2, with_times, "+")
#   # Find shortest travel time
#   rsum <- apply(mat_sub, 1, min, na.rm = TRUE)
#   
#   # To have confidence in estimate want to pick from multiple options
#   fun1 <- function(x){
#     sum(!is.na(x))
#   }
#   
#   rsum_count <- apply(mat_sub, 1, fun1)
#   rsum[rsum_count <= threshold] <- NA
#   rsum[is.infinite(rsum)] <- NA
#   
#   combined <- ifelse(times_to_midpoints_isna, rsum, times_to_midpoints)
#   combined <- unname(combined)
#   return(combined)
# }
# 
# identical(as.integer(mat_inter(530, mat, 10)), mat_inter2(530, mat, 10))
# 
# # new function about 10% faster
# bench::mark(f1 = mat_inter(530, mat, 10), 
# f2 = mat_inter2(530, mat, 10), check = FALSE)
# 
# foo = data.frame(f1 = f1, f2 = f2)
# rownames(foo) = rownames(mat)
# foo$diff <- f1 - f2




# qtm(cents[cents$Zone_Code %in% colnames(mat)[col],])
# qtm(cents[cents$Zone_Code %in% names(with_times),])
# summary(as.numeric(mat_sub))
# 
# 
# summary(times_to_midpoints)
# summary(rsum)
# 
# qtm(cents[cents$Zone_Code %in% names(rsum)[!is.na(rsum)],])
# 
# summary(names(rsum) == names(times_to_midpoints))
# 
# combined <- ifelse(times_to_midpoints_isna, rsum, times_to_midpoints)
# 
# combined <- data.frame(Zone_Code = names(combined),
#                        combined = combined,
#                        before = times_to_midpoints,
#                        after = rsum)
library(dplyr)
library(sf)
library(tmap)
tmap_mode("view")
col = 5000
ttsum <- data.frame(Zone_Code = rownames(mat),
                    before = mat[,col],
                    after = newmat5[,col])
ttsum$hours <- ttsum$after / 3600

cents <- read_sf("data/NTEM/NTEM_centroids_mod.geojson")
cents2 <- left_join(cents, ttsum, by = "Zone_Code")


tm_shape(cents2) +
  tm_dots(col = "hours",
          palette = "Spectral", 
          n = 14) +
  tm_shape(cents[cents$Zone_Code %in% colnames(mat)[col],]) +
  tm_dots(col = "black")
  
  qtm(cents2, dots.col = "before") +
  qtm(cents2[], dots.col = "after") +
  qtm(cents[cents$Zone_Code %in% colnames(mat)[col],], dots.col = "red") 
#qtm(cents[!cents$Zone_Code %in% colnames(mat),], dots.col = "blue")


mat_sub <- mat[,with_times]
mat_sub <- mat_sub[rownames(mat_sub) %in% without_times,]
