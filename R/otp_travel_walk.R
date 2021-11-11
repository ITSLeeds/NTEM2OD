# Make NTEM Cents
library(sf)
library(opentripplanner)
library(tmap)
library(nngeo)

# Validate that centroids are routeable

ntem_cents = st_read("data/NTEM/NTEM_centroids_mod.geojson")
ntem_cents <- cbind(ntem_cents, data.frame(st_coordinates(ntem_cents)))
ntem_cents <- ntem_cents[order(round(ntem_cents$Y, 1), round(ntem_cents$X, 1)),]
ntem_cents <- ntem_cents[,"Zone_Code"]

path_data = "D:/OneDrive - University of Leeds/Data/opentripplanner"
path_opt = "D:/OneDrive - University of Leeds/Data/opentripplanner/otp-1.5.0-shaded.jar"

chunks <- split(1:nrow(ntem_cents), ceiling(seq_along(1:nrow(ntem_cents))/(1000)))

for(i in 1:length(chunks)){
  chunk_sub <- chunks[[i]]
  
  message(Sys.time()," Stage ", i," from ",min(chunk_sub)," to ",max(chunk_sub))
  
  
  
  log2 = otp_setup(path_opt,
                   path_data,
                   memory = 120011,
                   router = "great-britain-NTEM",
                   quiet = TRUE,
                   securePort = 8082, 
                   pointsets = TRUE,
                   analyst = TRUE,
                   open_browser = FALSE)
  
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
                            mode = c("WALK","FERRY"),
                            maxWalkDistance = 90000,
                            ncores = 12)
  
  message(Sys.time()," Checking for failed routes")
  
  fromPlace2 <- fromPlace[!fromPlace$Zone_Code %in% colnames(mat_sub),]
  
  if(nrow(fromPlace2) > 0 ){
    mat_sub2 <- otp_traveltime(otpcon, 
                               path_data = path_data,
                               fromPlace = fromPlace2,
                               toPlace = toPlace,
                               fromID = fromPlace2$Zone_Code,
                               toID = toPlace$Zone_Code,
                               mode = c("WALK","FERRY"),
                               maxWalkDistance = 90000,
                               ncores = 1)
    
    mat_sub <- cbind(mat_sub, mat_sub2)
    
  }
  
  message(Sys.time()," Killing OTP")
  
  otp_stop(warn = FALSE)
  
  message(Sys.time()," Saving Results")
  
  saveRDS(mat_sub, paste0("data/ttmatrix/ttmatrix_walk_chunk_",i,"_from_",min(chunk_sub),"_to_",max(chunk_sub),".Rds"))
  rm(otpcon, mat_sub)
  
}


files <- list.files("data/ttmatrix", 
                    full.names = TRUE, 
                    pattern = "ttmatrix_walk_chunk_")

res <- list()
for(i in 1:length(files)){
  res[[i]] <- readRDS(files[i])
}

mat <- res[[1]]
for(i in 2:length(res)){
  mat <- cbind(mat, res[[i]])
}

matna <- as.numeric(!is.na(mat))
matna <- matrix(matna, ncol = ncol(mat))
colnames(matna) <- colnames(mat)
csums <- colSums(matna)
summary(csums)

csums <- data.frame(Zone_Code = names(csums), matches = csums)
cents2 <- left_join(ntem_cents, csums, by = "Zone_Code")
cents2$matches[is.na(cents2$matches)] <- -1
tm_shape(cents2[cents2$matches < 15,]) +
  tm_dots(col = "matches", style = "fixed", palette = c("black","red","orange","yellow","lightblue"),
          breaks = c(-1,0,2,5,10,400),
          scale = 2)
