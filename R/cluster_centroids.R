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
ntem_cents$cluster <- cutree(hc, h = 20000) # Clusters of 20 km

qtm(ntem_cents, dots.col = "cluster")

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


st_write(ntem_cluster, "data/NTEM/NTEM_centroids_clustered.geojson")


