library(sf)
library(dplyr)

ru <- read.csv("data/MSOA/Rural_Urban_Classification_(2011)_of_Middle_Layer_Super_Output_Areas_in_England_and_Wales.csv")
names(ru) = c("Zone_Code","nm","rucode","rural_urban","FID")
ru <- ru[,c("Zone_Code","rural_urban")]

cents <- read_sf("data/NTEM/NTEM_centroids_mod.geojson")

dir.create("tmp")
unzip("data/MSOA/settlements2016boundaries.zip", exdir = "tmp")

urb <- read_sf("tmp/Settlements2016_MHW.shp")
urb <- st_transform(urb, 4326)
urb <- urb[urb$Shape_Area > 2e6,]

cent_urb <- cents[urb,]

ru$rural_urban <- ifelse(ru$rural_urban %in% c("Urban major conurbation",
                                               "Urban city and town",
                                               "Urban minor conurbation",
                                               "Urban city and town in a sparse setting"),
                                               "Urban",
                                               "Rural")
cents <- left_join(cents, ru, by = "Zone_Code")
cents$scot_urban  <- cents$Zone_Code %in% cent_urb$Zone_Code
cents$scot_urban <- ifelse(cents$scot_urban,"Urban","Rural")

cents$rural_urban <- ifelse(is.na(cents$rural_urban),cents$scot_urban,cents$rural_urban)

cents$scot_urban <- NULL

regions <- read_sf("data/MSOA/regions.gpkg")
regions <- regions[,"nuts118nm"]
names(regions)[1] <- "region"

cents2 <- st_join(cents, regions)
cents2$region <- gsub(" (England)","",cents2$region,fixed = TRUE)
table(cents2$region)
summary(is.na(cents2$region))

write_sf(cents2, "data/NTEM/NTEM_centroids_mod.geojson", delete_layer = TRUE)

qtm(cents, dots.col = "rural_urban")
