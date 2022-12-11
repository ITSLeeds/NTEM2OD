library(sf)
library(geodist)
library(tidyr)
library(ggplot2)
library(seriation)
library(dplyr)


cities <- st_read("data/example_cities.geojson")
cities$name <- gsub(", United Kingdom","",cities$query)
cities = cities[!duplicated(cities$name),]
cities = cities[cities$name != "Gloucester",]

dists <- geodist(x = st_coordinates(cities),
                measure = "geodesic")

rownames(dists) <- cities$name
colnames(dists) <- cities$name

dists_df <- as.data.frame(dists)
dists_df$from <- rownames(dists)

dists_long <- pivot_longer(dists_df, 
                           cols = London:Hereford,
                           names_to = "to",
                           values_to = "distance")

dists_long$distance_miles <- dists_long$distance / 1609.34


# Get travel time

routes <- readRDS("data/routes_transit_example_cities.Rds")

# Remove glouster due to bug
routes <- routes[routes$fromPlace != "Gloucester, United Kingdom",]
routes <- routes[routes$toPlace != "Gloucester, United Kingdom",]

# Pick the fastest options
rs <- st_drop_geometry(routes)
rs <- rs[,c("fromPlace","toPlace","duration","route_option")]
rs <- rs %>%
  group_by(fromPlace, toPlace) %>%
  summarise(duration_min = min(duration))
routes <- left_join(routes, rs, by = c("fromPlace","toPlace"))
routes <- routes[routes$duration == routes$duration_min, ]
routes <- routes[,c("fromPlace","toPlace","duration","distance","route_option","mode")]

rs <- routes %>%
  st_drop_geometry() %>%
  group_by(fromPlace, toPlace) %>%
  summarise(route_option_min = min(route_option))

routes <- left_join(routes, rs, by = c("fromPlace","toPlace"))
routes <- routes[routes$route_option == routes$route_option_min, ]

routes <- routes[,c("fromPlace","toPlace","duration","distance","route_option","mode")]

# foo <- routes[routes$fromPlace == "Newcastle, United Kingdom",]
# foo <- foo[foo$toPlace == "Peterborough, United Kingdom",]

routes_summary <- st_drop_geometry(routes)
routes_summary <- routes_summary %>%
  group_by(fromPlace, toPlace) %>%
  summarise(duration = min(duration),
            distance = sum(distance))

routes_summary$fromPlace <- gsub(", United Kingdom","",routes_summary$fromPlace)
routes_summary$toPlace <- gsub(", United Kingdom","",routes_summary$toPlace)

routes_summary$hours <- routes_summary$duration / 3600
routes_summary$distance_network <- routes_summary$distance / 1609.34
routes_summary$distance <- NULL
names(routes_summary) <- c("from","to","duration","hours","distance_network")


dists_long2 <- left_join(dists_long, routes_summary,
                 by = c("from","to"))

dists_long2$mph_straight <- dists_long2$distance_miles / dists_long2$hours
dists_long2$mph_network <- dists_long2$distance_network / dists_long2$hours

summary(dists_long2$mph_straight)
summary(dists_long2$mph_network)


speed_mat <- dists_long2[,c("from","to","mph_network")]
speed_mat <- pivot_wider(speed_mat, names_from = "to", values_from = "mph_network")
speed_mat <- as.data.frame(speed_mat)
nms <- speed_mat$from
speed_mat$from <- NULL
speed_mat <- as.matrix(speed_mat)
rownames(speed_mat) <- nms
colnames(speed_mat) <- nms
speed_mat[is.na(speed_mat)] <- 0

set.seed(4)
o <- seriate(speed_mat, method="BEA_TSP")

#with the same longData then earlier
dists_long2$from2 <- factor(dists_long2$from, levels=names(unlist(o[[1]][]))) 
dists_long2$to2 <- factor(dists_long2$to, levels=names(unlist(o[[2]][])))
#levels must be names


# ggplot(dists_long2, aes(x = from, y = to)) + 
#   geom_raster(aes(fill=mph)) + 
#   scale_fill_binned(breaks = c(30,40,50,60,70),
#                     type = "viridis") +
#   labs(x="From", y="To", title="Distance") +
#   theme_bw() + theme(axis.text.x=element_text(size=9, angle=90, vjust=0.3),
#                      axis.text.y=element_text(size=9),
#                      plot.title=element_text(size=11))
# 
# ggplot(dists_long2, aes(x = from, y = to)) + 
#   geom_raster(aes(fill=mph)) + 
#   scale_fill_stepsn(colours=c("red","orange","yellow","green","blue","black"),
#                     breaks= c(30,40,50,60,70),
#                     limits=c(0,75),
#                     values = scales::rescale(c(0.1, 0.3, 0.5, 0.7,0.9), 
#                                              from = c(0, 75))) + 
#   labs(x="From", y="To", title="Distance") +
#   theme_bw() + theme(axis.text.x=element_text(size=9, angle=90, vjust=0.3),
#                      axis.text.y=element_text(size=9),
#                      plot.title=element_text(size=11))


ggplot(dists_long2, aes(x = from, y = to)) + 
  geom_raster(aes(fill=mph_network)) + 
  labs(x="From", y="To", title="Average speed (mph) of public transport between cities") +
  theme_bw() + theme(axis.text.x=element_text(size=9, angle=90, hjust=0.95,vjust=0.2),
                     axis.text.y=element_text(size=9),
                     plot.title=element_text(size=11))+
  binned_scale("fill",
                 "foo",
                 ggplot2:::binned_pal(scales::manual_pal(c("#d53e4f","#fc8d59","#fee08b","#e6f598","#99d594","#3288bd"))),
                 guide="coloursteps",
                 breaks=c(30,40,50,60,70),
                 limits=c(0,75),
                 show.limits=TRUE,
                 name = "mph") 
  
ggsave("plots/speed_matrix.png", dpi = 300, width = 7, height = 6.5)


# Dist to London

dist_lond <- geodist(x = st_coordinates(cities), y = st_coordinates(cities[cities$name == "London",]),
                     measure = "geodesic")

cities$dist_lond <- dist_lond[,1]
cities <- cities[order(cities$dist_lond, decreasing = TRUE),]

dists_long2$from2 <- factor(dists_long2$from, levels=cities$name) 
dists_long2$to2 <- factor(dists_long2$to, levels=cities$name)


ggplot(dists_long2, aes(x = from2, y = to2)) + 
  geom_raster(aes(fill=mph_network)) + 
  labs(x="From", y="To", title="Average speed (mph) of public transport between cities") +
  theme_bw() + theme(axis.text.x=element_text(size=9, angle=90, hjust=0.95,vjust=0.2),
                     axis.text.y=element_text(size=9),
                     plot.title=element_text(size=11))+
  binned_scale("fill",
               "foo",
               ggplot2:::binned_pal(scales::manual_pal(c("#d53e4f","#fc8d59","#fee08b","#e6f598","#99d594","#3288bd"))),
               guide="coloursteps",
               breaks=c(30,40,50,60,70),
               limits=c(0,75),
               show.limits=TRUE,
               name = "mph") 

ggsave("plots/speed_matrix_from_london.png", dpi = 300, width = 7, height = 6.5)

# scale_fill_stepsn(breaks = c(0,60,70,100,200,500),
#                   nice.breaks = FALSE,
#                   midpoint = 60,
#                   colors = c("red","orange","yellow","green")) +
