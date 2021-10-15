library(sf)
dir.create("tmp")
unzip("data/NTEM/shp-format-version-7-december-2016.zip",
      exdir = "tmp")
bounds <- read_sf("tmp/SHP/GB_70_region.shp")
unlink("tmp", recursive = TRUE)
bounds <- bounds["Zone_Code"]
bounds <- st_transform(bounds, 27700)
write_sf(bounds,"data/NTEM_bounds.gpkg")


dir.create("tmp")
unzip("D:/OneDrive - University of Leeds/Data/OA Bounadries/SG_IntermediateZoneCent_2011.zip",
      exdir = "tmp")
scotcents <- read_sf("tmp/SG_IntermediateZone_Cent_2011.shp")
unlink("tmp", recursive = TRUE)


ntemcents <- st_centroid(bounds)

msoacents <- pct::get_centroids_ew()

ntemcents <- ntemcents[!ntemcents$Zone_Code %in% msoacents$msoa11cd,]

msoacents$msoa11nm <- NULL
names(msoacents) <- names(ntemcents)
ntemcents <- st_transform(ntemcents, 27700)

ntemcents <- rbind(ntemcents, msoacents)
qtm(ntemcents)

write_sf(ntemcents,"data/NTEM_centroids.gpkg")

