library(sf)
dir.create("tmp")
unzip("data/TTWA/Travel_to_Work_Areas_(December_2011)_Full_Clipped_Boundaries_in_United_Kingdom.zip",
      exdir = "tmp")
bounds <- read_sf("tmp/Travel_to_Work_Areas_(December_2011)_Full_Clipped_Boundaries_in_United_Kingdom.shp")
unlink("tmp", recursive = TRUE)

bounds <- bounds[,c("TTWA11CD")]
bounds <- bounds[substr(bounds$TTWA11CD,1,1) != "N",]

st_write(bounds,"data/TTWA/TTWA_GB.gpkg")
