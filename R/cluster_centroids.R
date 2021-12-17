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



# Cluster Bus

# cluster all points using a hierarchical clustering approach
hc <- hclust(as.dist(dist), method="complete")
ntem_cents$cluster <- cutree(hc, h = 20000) # Clusters of 20 km

# FInd the centroid of the cluster
clusts_bus <- ntem_cents %>%
  group_by(cluster) %>%
  summarise() %>%
  st_centroid()

nn <- st_nn(clusts_bus, ntem_cents, k = 1)
ntem_cluster_bus <- ntem_cents[unlist(nn),]
ntem_cluster_bus <- ntem_cluster_bus[!ntem_cluster_bus$Zone_Code %in% ntem_cluster$Zone_Code,]

# qtm(ntem_cluster, dots.col = "cluster") +
#   qtm(ntem_cents)


st_write(ntem_cluster_bus, "data/NTEM/NTEM_centroids_clustered_bus.geojson", delete_dsn = TRUE)





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
rail_stops_top <- rail_stops[rail_stops$n_stops > 4000,]
tm_shape(rail_stops_top) +
  tm_dots(col = "n_stops", style = "jenks")


nn <- st_nn(rail_stops, ntem_cents, k = 1)
nn <- unlist(nn)
ntem_rail <- ntem_cents[nn,]
ntem_rail <- ntem_rail[!duplicated(ntem_rail$Zone_Code),]

dist <- st_distance(rail_stops_top)
dist <- as.numeric(dist)
dist <- matrix(dist, ncol = nrow(rail_stops_top))

# cluster all points using a hierarchical clustering approach
hc <- hclust(as.dist(dist), method="complete")
rail_stops_top$cluster <- cutree(hc, h = 4000) # Clusters of 4 km

qtm(rail_stops_top, dots.col = "cluster")

rail_stops_top <- rail_stops_top %>%
  st_drop_geometry() %>%
  group_by(cluster) %>%
  summarise(stop_id = stop_id[n_stops == max(n_stops)][1])

rail_stops_top <- rail_stops[rail_stops$stop_id %in% rail_stops_top$stop_id,]
qtm(rail_stops_top)

nn <- st_nn(rail_stops_top, ntem_cents, k = 1)
nn <- unlist(nn)
ntem_rail_top <- ntem_cents[nn,]

# Get the nearst 20 rail stops 
nn <- st_nn(ntem_rail, ntem_rail, k = 20)
nn <- unlist(nn)

fromPlace <- ntem_rail[rep(1:nrow(ntem_rail), each = 20),]
toPlace <- ntem_rail[nn,]

fromPlace2 <- ntem_rail[rep(1:nrow(ntem_rail), each = nrow(ntem_rail_top)),]
toPlace2 <- ntem_rail_top[rep(1:nrow(ntem_rail_top), times = nrow(ntem_rail)),]

fromPlace <- rbind(fromPlace, fromPlace2)
toPlace <- rbind(toPlace, toPlace2)


st_write(fromPlace, "data/NTEM/NTEM_centroids_rail_from.geojson", delete_dsn = TRUE)
st_write(toPlace, "data/NTEM/NTEM_centroids_rail_to.geojson", delete_dsn = TRUE)

qtm(rail_stops) +
  qtm(ntem_rail, dots.col = "red")


# dist <- st_distance(ntem_rail)
# dist <- as.numeric(dist)
# dist <- matrix(dist, ncol = nrow(ntem_rail))
# 
# # cluster all points using a hierarchical clustering approach
# hc <- hclust(as.dist(dist), method="complete")
# ntem_rail$cluster <- cutree(hc, h = 20000) # Clusters of 20 km
# 
# qtm(ntem_rail, dots.col = "cluster")
# 
# # FInd the centroid closes to a station for each cluster
# nn <- st_nn(ntem_rail, rail_stops, k = 1, returnDist = TRUE)
# 
# ntem_rail$dist <- unlist(nn$dist)
# ntem_rail$top <- rail_stops$n_stops[unlist(nn$nn)] > 3000
# 
# qtm(ntem_rail, dots.col = "top")
# 
# 
# 
# clusts <- ntem_rail %>%
#   group_by(cluster, top) %>%
#   summarise(Zone_Code = Zone_Code[dist == min(dist)],
#             geometry = geometry[dist == min(dist)])
# 
# # qtm(clusts, dots.col = "cluster") +
# #   qtm(ntem_cents)
# clusts <- clusts[,c("Zone_Code")]
# clusts <- clusts[!duplicated(clusts$Zone_Code),]
# 
# qtm(clusts)
# 
# 
# st_write(clusts, "data/NTEM/NTEM_centroids_clustered_rail.geojson", delete_dsn = TRUE)

# CLuster walking centroids
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

csum <- colSums(is.na(mat))
csum <- csum[csum > 7696]
csum <- c(names(csum),"E02004416","E02004420","E02006049","S99900027",
          "S99900141","S99900498","S99900207","S99900164",
          "S99900180","E02006298","E02002723","E02002689","E02005936",
          "E02006781", "E02003950", "E02004200", "E02004198", "E02004199",
          "E02004822", "E02006053", "E02006050", "E02006049", "E02006047",
          "E02004883", "E02004878",
          "E02005628", "E02005476", "E02002714", "E02002722", "E02005322",
          "E02005327", "E02005324", "E02005325", "S99900141", "S99900027",
          "S99900279", "S99900498","S99900499", "S99900207", "S99900163",
          "S99900164", "S99900180", "S99900179")
csum <- unique(csum)

ntem_walk <- ntem_cents[ntem_cents$Zone_Code %in% csum,]

nn <- st_nn(ntem_walk, ntem_cents, k = 10)
nn <- unlist(nn)

walk_from <- ntem_walk[rep(1:nrow(ntem_walk), each = 10),]
walk_to <- ntem_cents[nn,]

st_write(walk_from, "data/NTEM/NTEM_centroids_walk_from.geojson", delete_dsn = TRUE)
st_write(walk_to, "data/NTEM/NTEM_centroids_walk_to.geojson", delete_dsn = TRUE)



# CLuster cycleing centroids
files <- list.files("data/ttmatrix", 
                    full.names = TRUE, 
                    pattern = "ttmatrix_cycle_chunk_")

res <- list()
for(i in 1:length(files)){
  res[[i]] <- readRDS(files[i])
}

mat <- res[[1]]
for(i in 2:length(res)){
  mat <- cbind(mat, res[[i]])
}

csum <- colSums(is.na(mat))
csum <- csum[csum > 7696]
csum <- c(names(csum),"E02004416","E02004420","E02006049","S99900027",
          "S99900141","S99900498","S99900207","S99900164",
          "S99900180","E02006298","E02002723","E02002689",
          "E02006781", "E02005727", "S99900028", "S99900141",
          "S99900027", "S99900404", "S99900403", "S99900029",
          "S99900279", "S99900026", "S99900402", "S99900119",
          "S99900320", "S99900302", "S99900325", "S99900452",
          "S99900328", "S99900392", "S99900498", "S99900323",
          "S99900322", "S99900499", "S99900207", "S99900321",
          "S99900125", "S99900123", "S99900163", "S99900164",
          "S99900180", "S99900179", "E02006117")
csum <- unique(csum)

ntem_cycle <- ntem_cents[ntem_cents$Zone_Code %in% csum,]

nn <- st_nn(ntem_cycle, ntem_cents, k = 10)
nn <- unlist(nn)

cycle_from <- ntem_cycle[rep(1:nrow(ntem_cycle), each = 10),]
cycle_to <- ntem_cents[nn,]

st_write(cycle_from, "data/NTEM/NTEM_centroids_cycle_from.geojson", delete_dsn = TRUE)
st_write(cycle_to, "data/NTEM/NTEM_centroids_cycle_to.geojson", delete_dsn = TRUE)

# Cluster centorid for newcastle
