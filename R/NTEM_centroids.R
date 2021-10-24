# Make NTEM Cents
library(sf)
library(opentripplanner)
library(tmap)

tmap_mode("view")

msoa_cents = st_read("data/MSOA/MSOA_2011_Centroids.geojson")
ntem_cents <- read_sf("data/NTEM_bounds.gpkg")
ntem_cents <- st_centroid(ntem_cents)

ntem_cents <- ntem_cents[!ntem_cents$Zone_Code %in% msoa_cents$msoa11cd,]

#qtm(msoa_cents) + qtm(ntem_cents, dots.col = "red")

msoa_cents <- msoa_cents[,c("msoa11cd")]
names(msoa_cents) <- c("Zone_Code","geometry")

msoa_cents <- st_transform(msoa_cents, 4326)
ntem_cents <- st_transform(ntem_cents, 4326)

names(ntem_cents) <- names(msoa_cents)
st_geometry(ntem_cents)= "geometry"

ntem_cents <- rbind(ntem_cents, msoa_cents)

st_write(ntem_cents, "data/NTEM/NTEM_centroids_raw.geojson")


