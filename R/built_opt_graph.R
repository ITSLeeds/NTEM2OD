remotes::install_github("ropensci/opentripplanner")
library(opentripplanner)

path_data = "D:/OneDrive - University of Leeds/Data/opentripplanner"
path_opt = "D:/OneDrive - University of Leeds/Data/opentripplanner/otp-1.5.0-shaded.jar"

# log1 = otp_build_graph(path_opt,
#                        path_data,
#                        memory = 90000,
#                        router = "great-britain-NTEM",
#                        quiet = FALSE)

log2 = otp_setup(path_opt,
                 path_data,
                 memory = 80000,
                 router = "great-britain-NTEM",
                 quiet = FALSE,
                 securePort = 8082, 
                 pointsets = TRUE,
                 analyst = TRUE)

#java -Xmx100000M -d64 -jar "D:/OneDrive - University of Leeds/Data/opentripplanner/otp-1.5.0-shaded.jar" --router great-britain-NTEM --graphs "D:/OneDrive - University of Leeds/Data/opentripplanner/graphs" --server --port 8080 --securePort 8082 --analyst --pointSets "D:/OneDrive - University of Leeds/Data/opentripplanner/pointsets"


# Remove cycleways dro driving graph
library(OSMtools)


osmt_convert(file = "D:/OneDrive - University of Leeds/Data/opentripplanner/graphs/great-britain-NTEM-drive/great-britain-latest.osm.pbf", format_out = "o5m")
osmt_filter(file = "D:/OneDrive - University of Leeds/Data/opentripplanner/graphs/great-britain-NTEM-drive/great-britain-latest.osm.o5m", 
            drop_tags = "cycleway",
            path_out = "D:/OneDrive - University of Leeds/Data/opentripplanner/graphs/great-britain-NTEM-drive/great-britain-nocycleway.osm.o5m")

osmt_convert(file = "D:/OneDrive - University of Leeds/Data/opentripplanner/graphs/great-britain-NTEM-drive/great-britain-nocycleway.osm.o5m", format_out = "pbf")

# Find ferrys
library(UK2GTFS)
files <- list.files("D:/OneDrive - University of Leeds/Data/opentripplanner/graphs/great-britain-NTEM/", 
                    pattern = ".zip", full.names = TRUE)
gtfs <- list()
for(i in 1:length(files)){
  sub <- gtfs_read(files[i])
  routes <- sub$routes
  routes <- routes[routes$route_type == 4,]
  if(nrow(routes) > 0){
    trips <- sub$trips$trip_id[sub$trips$route_id %in% routes$route_id]
    sub <- gtfs_split_ids(sub, trips)
    gtfs[[i]] <- sub$true
  }
}


gtfs2 <- gtfs_merge(gtfs)

# Select Car Ferry  only
#st_write(tps,"data/ferrys.geojson")
tps2 <- st_read("data/ferrys.geojson")
tps2 <- tps2$trip_id

trips <- gtfs2$trips
trips <- trips[trips$trip_id %in% tps2,]
routes <- gtfs2$routes                                       
routes <- routes[routes$route_id %in% trips$route_id,]
trips <- gtfs2$trips$trip_id[gtfs2$trips$route_id %in% routes$route_id]

gtfs_ferry <- gtfs_split_ids(gtfs2, trips)
gtfs_ferry <- gtfs_ferry$true

gtfs_write(gtfs_ferry, "D:/OneDrive - University of Leeds/Data/opentripplanner/graphs/great-britain-NTEM-drive/",
           "car_ferry")

log1 = otp_build_graph(path_opt,
                        path_data,
                        memory = 80000,
                        router = "great-britain-NTEM-drive",
                        quiet = FALSE)


dir.create("foo")
unlink("foo", recursive = TRUE)

