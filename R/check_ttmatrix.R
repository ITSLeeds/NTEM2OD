library(sf)
library(tmap)
library(dplyr)
tmap_mode("view")

cents <- read_sf("data/NTEM/NTEM_centroids_mod.geojson")
cents <- cbind(cents, data.frame(st_coordinates(cents)))
cents <- cents[order(round(cents$Y, 1), round(cents$X, 1)),]
cents$chunk <- ceiling(seq_along(1:nrow(cents))/(7700/38))
cents$id <- 1:7700



# dists <- st_distance(cents)
# 
# d2 <- as.numeric(dists)
# d2 <- d2 < 100000
# d2 <- as.numeric(d2)
# d2 <- matrix(d2, ncol = 7700)
# 
# near <- rowSums(d2)
# 
# cents$near <- near
# cents$chunk <- as.character(cents$chunk)
# 
# tm_shape(cents) +
#   tm_dots(col = "id", n = 20, palette = "Spectral")
# 
# 
# c2 <- st_drop_geometry(cents)
# c3 <- c2 %>%
#   group_by(chunk) %>%
#   summarise(total = sum(near),
#             mean = mean(near),
#             median = median(near),
#             max = max(near),
#             min = min(near))


#dir.create("tmp")
#unzip("data/ttmatrix_drive_sample.zip", exdir = "tmp")
files <- list.files("data/ttmatrix", full.names = TRUE, pattern = "ttmatrix_car_chunk_")

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

cents2 <- left_join(cents, csums, by = "Zone_Code")

cents2$matches[is.na(cents2$matches)] <- -1

tm_shape(cents2) +
  tm_dots(col = "matches", style = "fixed", palette = c("black","red","orange","yellow","lightblue"),
          breaks = c(-1,0,2,5,100,4000),
          scale = 1.5          )

mat <- mat[,rownames(mat)]
mat <- as.matrix(mat)
matna <- !is.na(mat)
csums <- colSums(matna)
rsums <- rowSums(matna)

csums <- data.frame(Zone_Code = names(csums), cols = csums, rows = rsums)
csums$diff <- (csums$cols - csums$rows) / csums$rows
hist(csums$diff)
summary(csums$diff)
cents2 <- left_join(cents, csums, by = "Zone_Code")

tm_shape(cents2) +
  tm_dots(col = "diff", 
          palette =  "Spectral",
          style = "fixed",
          breaks = c(-400,-1,-0.5,-0.1,0.1,0.5,1,400),
          scale = 1.5 )


library(opentripplanner)

path_data = "D:/OneDrive - University of Leeds/Data/opentripplanner"
path_opt = "D:/OneDrive - University of Leeds/Data/opentripplanner/otp-1.5.0-shaded.jar"

# log2 = otp_setup(path_opt,
#                  path_data,
#                  memory = 120011,
#                  router = "great-britain-NTEM-drive",
#                  quiet = FALSE,
#                  securePort = 8082, 
#                  pointsets = TRUE,
#                  analyst = TRUE)

otpcon <- otp_connect(router = "great-britain-NTEM-drive")

r1 <- otp_plan(otpcon, 
              fromPlace = c(-1.95145, 50.67737), 
              toPlace = c(-1.94849, 50.68371), 
              mode = c("CAR","FERRY"))

r2 <- otp_plan(otpcon, 
               fromPlace = c(-1.95145, 50.67737), 
               toPlace = c(-1.94849, 50.68371), 
               mode = c("CAR"))


cents_redo <- cents2[cents2$matches < 30,]
qtm(cents_redo, dots.col = "matches")


chunks <- split(1:nrow(cents_redo), ceiling(seq_along(1:nrow(cents_redo))/(100)))



for(i in 1:length(chunks)){
  chunk_sub <- chunks[[i]]
  
  message(Sys.time()," Stage ", i," from ",min(chunk_sub)," to ",max(chunk_sub))
  
  
  
  log2 = otp_setup(path_opt,
                   path_data,
                   memory = 120011,
                   router = "great-britain-NTEM-drive",
                   quiet = TRUE,
                   securePort = 8082,
                   pointsets = TRUE,
                   analyst = TRUE)
  
  fromPlace <- cents_redo[chunk_sub, ]
  toPlace <- cents
  
  message(Sys.time()," Sleeping during OTP setup")
  
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
  
  mat_sub <- otp_traveltime(otpcon, 
                            path_data = path_data,
                            fromPlace = fromPlace,
                            toPlace = toPlace,
                            fromID = fromPlace$Zone_Code,
                            toID = toPlace$Zone_Code,
                            mode = c("CAR","FERRY"),
                            maxWalkDistance = 10000,
                            ncores = 12)
  
  message(Sys.time()," Killing OTP")
  
  otp_stop(warn = FALSE)
  
  message(Sys.time()," Saving Results")
  
  #res[[i]] <- mat_sub
  saveRDS(mat_sub, paste0("data/ttmatrix_drive_retest_chunk_",i,"_from_",min(chunk_sub),"_to_",max(chunk_sub),".Rds"))
  rm(otpcon, mat_sub)
  
  
}

files = list.files("data", pattern = "ttmatrix_drive_retest_chunk_", full.names = TRUE)

res <- list()
for(i in 1:length(files)){
  res[[i]] <- readRDS(files[i])
}

newmat <- res[[1]]
for(i in 2:length(res)){
  newmat <- cbind(newmat, res[[i]])
}


newmatna <- as.numeric(!is.na(newmat))
newmatna <- matrix(newmatna, ncol = ncol(newmat))
colnames(newmatna) <- colnames(newmat)

csums <- colSums(newmatna)
summary(csums)

csums <- data.frame(Zone_Code = names(csums), matches = csums)

cents3 <- left_join(cents, csums, by = "Zone_Code")
cents3 <- cents3[cents3$Zone_Code %in% cents_redo$Zone_Code,]

cents3$matches[is.na(cents3$matches)] <- -1

tm_shape(cents3) +
  tm_dots(col = "matches", style = "fixed", palette = c("black","red","orange","yellow","lightblue"),
          breaks = c(-1,0,2,5,100,4000),
          scale = 2          )



cents_redo <- cents3[cents3$matches < 30,]
qtm(cents_redo, dots.col = "matches")

r3 <- otp_plan(otpcon, 
               fromPlace = c(-1.28658, 50.75696), 
               toPlace = c(-1.41380, 50.91256), maxWalkDistance = 5000,
               mode = c("CAR","FERRY"))



fromPlace <- cents_redo
toPlace <- cents

mat_tryagain <- otp_traveltime(otpcon, 
                          path_data = path_data,
                          fromPlace = fromPlace,
                          toPlace = toPlace,
                          fromID = fromPlace$Zone_Code,
                          toID = toPlace$Zone_Code,
                          mode = c("CAR","FERRY"),
                          maxWalkDistance = 10000,
                          ncores = 12)


newmatna <- as.numeric(!is.na(mat_tryagain))
newmatna <- matrix(newmatna, ncol = ncol(mat_tryagain))
colnames(newmatna) <- colnames(mat_tryagain)

csums <- colSums(newmatna)
summary(csums)


csums <- data.frame(Zone_Code = names(csums), matches = csums)

cents4 <- left_join(cents, csums, by = "Zone_Code")
cents4 <- cents4[cents4$Zone_Code %in% cents_redo$Zone_Code,]

cents4$matches[is.na(cents4$matches)] <- -1

tm_shape(cents3) +
  tm_dots(col = "matches", style = "fixed", palette = c("black","red","orange","yellow","lightblue"),
          breaks = c(-1,0,2,5,100,4000),
          scale = 2          )


fromPlace  <- cents4[cents4$matches < 30,]
toPlace <- cents

tm_shape(fromPlace) +
  tm_dots(col = "matches", style = "fixed", palette = c("black","red","orange","yellow","lightblue"),
          breaks = c(-1,0,2,5,100,4000),
          scale = 2          )


mat_1 <- otp_traveltime(otpcon, 
                               path_data = path_data,
                               fromPlace = fromPlace,
                               toPlace = toPlace,
                               fromID = fromPlace$Zone_Code,
                               toID = toPlace$Zone_Code,
                               mode = c("CAR","FERRY"),
                               maxWalkDistance = 10000,
                               ncores = 12)


newmatna <- as.numeric(!is.na(mat_1))
newmatna <- matrix(newmatna, ncol = ncol(mat_1))
colnames(newmatna) <- colnames(mat_1)

csums <- colSums(newmatna)
summary(csums)

