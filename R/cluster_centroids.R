# CLuster centroid for long distance transit routing
library(sf)
library(tmap)
library(dplyr)
library(geosphere)
library(nngeo)

tmap_mode("view")

ntem_cents = st_read("data/NTEM/NTEM_centroids_mod.geojson")
dist <- st_distance(ntem_cents)
dist <- as.numeric(dist)
dist <- matrix(dist, ncol = 7700)

# cluster all points using a hierarchical clustering approach
hc <- hclust(as.dist(dist), method="complete")
ntem_cents$cluster <- cutree(hc, h = 30000) # Clusters of 30 km

#qtm(ntem_cents, dots.col = "cluster")

# FInd the centroid of the cluster
clusts <- ntem_cents %>%
  group_by(cluster) %>%
  summarise() %>%
  st_centroid()

# qtm(clusts, dots.col = "cluster") +
#   qtm(ntem_cents)


nn <- st_nn(clusts, ntem_cents, k = 1)

ntem_cluster <- ntem_cents[unlist(nn),]

# qtm(ntem_cluster, dots.col = "cluster") +
#   qtm(ntem_cents)


st_write(ntem_cluster, "data/NTEM/NTEM_centroids_clustered.geojson", delete_dsn = TRUE)


# Cluster Centroids for rail
dir.create("tmp")
unzip("D:/OneDrive - University of Leeds/Data/opentripplanner/graphs/great-britain-NTEM/rail_20211009.zip",
      exdir = "tmp")
rail_stops <- read.csv("tmp/stops.txt")
rail_stop_times <- read.csv("tmp/stop_times.txt")
unlink("tmp")
rail_stops <- st_as_sf(rail_stops, coords = c("stop_lon","stop_lat"), crs = 4326)

rail_stop_freq <- rail_stop_times %>%
  group_by(stop_id) %>%
  summarise(n_stops = n())

rail_stops <- left_join(rail_stops, rail_stop_freq, by = "stop_id")
rail_stops_top <- rail_stops[rail_stops$n_stops > 10000,]
tm_shape(rail_stops_top) +
  tm_dots(col = "n_stops", style = "jenks")


nn <- st_nn(rail_stops, ntem_cents, k = 1)
nn <- unlist(nn)
ntem_rail <- ntem_cents[nn,]
ntem_rail <- ntem_rail[!duplicated(ntem_rail$Zone_Code),]

qtm(rail_stops) +
  qtm(ntem_rail, dots.col = "red")


dist <- st_distance(ntem_rail)
dist <- as.numeric(dist)
dist <- matrix(dist, ncol = nrow(ntem_rail))

# cluster all points using a hierarchical clustering approach
hc <- hclust(as.dist(dist), method="complete")
ntem_rail$cluster <- cutree(hc, h = 20000) # Clusters of 20 km

qtm(ntem_rail, dots.col = "cluster")

# FInd the centroid closes to a station for each cluster
nn <- st_nn(ntem_rail, rail_stops, k = 1, returnDist = TRUE)

ntem_rail$dist <- unlist(nn$dist)
ntem_rail$top <- rail_stops$n_stops[unlist(nn$nn)] > 3000

qtm(ntem_rail, dots.col = "top")



clusts <- ntem_rail %>%
  group_by(cluster, top) %>%
  summarise(Zone_Code = Zone_Code[dist == min(dist)],
            geometry = geometry[dist == min(dist)])

# qtm(clusts, dots.col = "cluster") +
#   qtm(ntem_cents)
clusts <- clusts[,c("Zone_Code")]
clusts <- clusts[!duplicated(clusts$Zone_Code),]

qtm(clusts)


st_write(clusts, "data/NTEM/NTEM_centroids_clustered_rail.geojson", delete_dsn = TRUE)
