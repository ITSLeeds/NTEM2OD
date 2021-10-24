# Make NTEM Cents
library(sf)
library(opentripplanner)
library(tmap)
library(nngeo)

# Validate that centroids are routeable

ntem_cents = st_read("data/NTEM/NTEM_centroids_mod.geojson")
ntem_cents <- cbind(ntem_cents, data.frame(st_coordinates(ntem_cents)))
ntem_cents <- ntem_cents[order(round(ntem_cents$Y, 1), round(ntem_cents$X, 1)),]
#ntem_cents$chunk <- ceiling(seq_along(1:nrow(ntem_cents))/385)

# tm_shape(ntem_cents) +
#   tm_dots(col = "chunk", n = 20, palette = "Spectral")

ntem_cents <- ntem_cents[,"Zone_Code"]



path_data = "D:/OneDrive - University of Leeds/Data/opentripplanner"
path_opt = "D:/OneDrive - University of Leeds/Data/opentripplanner/otp-1.5.0-shaded.jar"

nchunks = 38

chunks <- split(1:nrow(ntem_cents), ceiling(seq_along(1:nrow(ntem_cents))/(7700/nchunks)))



for(i in 1:nchunks){
  chunk_sub <- chunks[[i]]
  
  message(Sys.time()," Stage ", i," from ",min(chunk_sub)," to ",max(chunk_sub))
  
  
  
  log2 = otp_setup(path_opt,
                   path_data,
                   memory = 115011,
                   router = "great-britain-NTEM",
                   quiet = TRUE,
                   securePort = 8082, 
                   pointsets = TRUE,
                   analyst = TRUE)
  
  fromPlace <- ntem_cents[chunk_sub, ]
  toPlace <- ntem_cents
  
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
  
  mat_sub <- otp_traveltime(otpcon, 
                            path_data = path_data,
                            fromPlace = fromPlace,
                            toPlace = toPlace,
                            fromID = fromPlace$Zone_Code,
                            toID = toPlace$Zone_Code,
                            mode = "CAR",
                            ncores = 34)
  
  message(Sys.time()," Killing OTP")
  
  otp_stop(warn = FALSE)
  
  message(Sys.time()," Saving Results")
  
  #res[[i]] <- mat_sub
  saveRDS(mat_sub, paste0("data/ttmatrix_drive_chunk_",i,"_from_",min(chunk_sub),"_to_",max(chunk_sub),".Rds"))
  rm(otpcon, mat_sub)
  
  
}





# matrix_drive = otp_traveltime(otpcon, 
#                               path_data = path_data,
#                               fromPlace = ntem_cents,
#                               toPlace = ntem_cents,
#                               fromID = ntem_cents$Zone_Code,
#                               toID = ntem_cents$Zone_Code,
#                               mode = "CAR",
#                               ncores = 34)
# saveRDS(matrix_drive,"data/ttmatrix_drive.Rds")



# near = st_nn(ntem_cents, ntem_cents, k = 3, parallel = 20)
# near <- sapply(near, function(x){x[3]})
# 
# fromPlace = ntem_cents
# toPlace = ntem_cents[near, ]
# 
# #qtm(fromPlace[1,]) + qtm(toPlace[1,], dots.col = "red")
# 
# 
# 
# routes <- otp_plan(otpcon, 
#                    fromPlace = fromPlace, 
#                    toPlace = toPlace,
#                    fromID = fromPlace$Zone_Code,
#                    toID = toPlace$Zone_Code,
#                    ncores = 30,
#                    distance_balance = TRUE,
#                    get_geometry = FALSE)
# 
# missing <- ntem_cents[!ntem_cents$Zone_Code %in% routes$fromPlace,]
# missing <- missing[!missing$Zone_Code %in% routes$toPlace,]
# qtm(missing)


#Doing everything take ages
# do some tests

# toPlace = ntem_cents
# ids <- c("S99900321",
#          "E02005761",
#          "E02000968")
# 
# res <- list()
# 
# dists <- st_distance(ntem_cents)
# rownames(dists) <- ntem_cents$Zone_Code
# colnames(dists) <- ntem_cents$Zone_Code
# 
# 
# for(i in 1:3){
#   fromPlace <- ntem_cents[ntem_cents$Zone_Code == ids[i], ]
#   message(ids[i])
#   tstarts <- Sys.time()
#   
#   res[[i]] = otp_traveltime(otpcon, 
#                                 path_data = path_data,
#                                 fromPlace = fromPlace,
#                                 toPlace = toPlace,
#                                 fromID = fromPlace$Zone_Code,
#                                 toID = toPlace$Zone_Code,
#                                 mode = "CAR",
#                                 ncores = 1)
#   
#   tend <- Sys.time()
#   print(tend - tstarts)
#   
# }
# 
# 
# for(i in 1:3){
#   fromPlace <- ntem_cents[ntem_cents$Zone_Code == ids[i], ]
#   message(ids[i])
#   
#   dsub <- dists[,colnames(dists) == ids[i]]
#   dsub <- dsub[as.numeric(dsub) < 402336] # 250 miles
#   
#   toPlace <- ntem_cents[ntem_cents$Zone_Code %in% names(dsub), ]
#   
#   tstarts <- Sys.time()
#   
#   res[[i]] = otp_traveltime(otpcon, 
#                             path_data = path_data,
#                             fromPlace = fromPlace,
#                             toPlace = toPlace,
#                             fromID = fromPlace$Zone_Code,
#                             toID = toPlace$Zone_Code,
#                             mode = "CAR",
#                             ncores = 1)
#   
#   tend <- Sys.time()
#   print(tend - tstarts)
#   
# }
# 
# 
# 
# qtm(ntem_cents) + qtm(ntem_cents[ntem_cents$Zone_Code %in% rownames(foo)[is.na(foo[,1])], ], dots.col = "red")
# 
# 
# 
