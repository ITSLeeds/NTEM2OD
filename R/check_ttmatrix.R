library(sf)
library(tmap)
library(dplyr)
tmap_mode("view")

cents <- read_sf("data/NTEM/NTEM_centroids_mod.geojson")
cents <- cbind(cents, data.frame(st_coordinates(cents)))
cents <- cents[order(round(cents$Y, 1), round(cents$X, 1)),]
cents$chunk <- ceiling(seq_along(1:nrow(cents))/(7700/38))
cents$id <- 1:7700



dists <- st_distance(cents)

d2 <- as.numeric(dists)
d2 <- d2 < 100000
d2 <- as.numeric(d2)
d2 <- matrix(d2, ncol = 7700)

near <- rowSums(d2)

cents$near <- near
cents$chunk <- as.character(cents$chunk)

tm_shape(cents) +
  tm_dots(col = "id", n = 20, palette = "Spectral")


c2 <- st_drop_geometry(cents)
c3 <- c2 %>%
  group_by(chunk) %>%
  summarise(total = sum(near),
            mean = mean(near),
            median = median(near),
            max = max(near),
            min = min(near))


dir.create("tmp")
unzip("data/ttmatrix_drive_sample.zip", exdir = "tmp")
files <- list.files("tmp", full.names = TRUE)

res <- list()
for(i in 1:length(files)){
  res[[i]] <- readRDS(files[i])
}
unlink("tmp", recursive = TRUE)


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
          scale = 2          )


tm_shape(cents2[cents2$matches > 1,]) +
  tm_dots(col = "matches", palette = "Spectral",  n = 20)
