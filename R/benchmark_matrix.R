# Benchmark matrix for accuracy
library(sf)
library(opentripplanner)
library(tmap)
library(dplyr)
tmap_mode("view")

#ntem_cluster <- st_read("data/NTEM/NTEM_centroids_clustered_rail.geojson")

path_data = "D:/OneDrive - University of Leeds/Data/opentripplanner"
path_opt = "D:/OneDrive - University of Leeds/Data/opentripplanner/otp-1.5.0-shaded.jar"


log2 = otp_setup(path_opt,
                 path_data,
                 memory = 220011,
                 router = "great-britain-NTEM",
                 quiet = TRUE,
                 securePort = 8082,
                 open_browser = FALSE,
                 wait = FALSE)

message(Sys.time()," Sleeping during OTP setup")

# Produce random od examples

ntem_cents <- read_sf("data/NTEM/NTEM_centroids_mod.geojson")

# set.seed(1234)
# 
# fromPlace <- ntem_cents[sample(1:nrow(ntem_cents), 1000),]
# toPlace <- ntem_cents[sample(1:nrow(ntem_cents), 1000),]
# 
# st_write(fromPlace, "data/NTEM/NTEM_benchmark_from.geojson")
# st_write(toPlace, "data/NTEM/NTEM_benchmark_to.geojson")

fromPlace <- read_sf("data/NTEM/NTEM_benchmark_from.geojson")
toPlace <- read_sf("data/NTEM/NTEM_benchmark_to.geojson")


foo <- list()
for(i in 1:1000){
  foo[[i]] <- st_cast(st_combine(c(fromPlace$geometry[[i]], toPlace$geometry[[i]])), "LINESTRING")
}
foo <- st_multilinestring(do.call("rbind", foo))
foo <- st_sfc(list(foo))
foo <- st_cast(foo, "LINESTRING")
qtm(foo)


for(j in 1:100){
  otpcon <- try(otp_connect(router = "great-britain-NTEM"), silent = TRUE)
  if(class(otpcon)[1] == "try-error"){
    message("Round ",j," sleeping for another minute")
    Sys.sleep(60)
  } else {
    break
  }
}

routes_walk  <- otp_plan(otpcon, 
                       fromPlace = fromPlace,
                       toPlace = toPlace,
                       fromID = fromPlace$Zone_Code,
                       toID = toPlace$Zone_Code, 
                       date_time = lubridate::ymd_hms("2021/10/28 08:00:00"),
                       mode = c("WALK"),
                       maxWalkDistance = 5000,
                       ncores = 20,
                       distance_balance = TRUE,
                       get_geometry = FALSE)

routes_cycle  <- otp_plan(otpcon, 
                         fromPlace = fromPlace,
                         toPlace = toPlace,
                         fromID = fromPlace$Zone_Code,
                         toID = toPlace$Zone_Code, 
                         date_time = lubridate::ymd_hms("2021/10/28 08:00:00"),
                         mode = c("BICYCLE"),
                         maxWalkDistance = 5000,
                         ncores = 20,
                         distance_balance = TRUE,
                         get_geometry = FALSE)

routes_drive  <- otp_plan(otpcon, 
                          fromPlace = fromPlace,
                          toPlace = toPlace,
                          fromID = fromPlace$Zone_Code,
                          toID = toPlace$Zone_Code, 
                          date_time = lubridate::ymd_hms("2021/10/28 08:00:00"),
                          mode = c("CAR"),
                          maxWalkDistance = 5000,
                          ncores = 20,
                          distance_balance = TRUE,
                          get_geometry = FALSE)

routes_bus  <- otp_plan(otpcon, 
                          fromPlace = fromPlace,
                          toPlace = toPlace,
                          fromID = fromPlace$Zone_Code,
                          toID = toPlace$Zone_Code, 
                          date_time = lubridate::ymd_hms("2021/10/28 08:00:00"),
                          mode = c("WALK","BUS","FERRY"),
                          maxWalkDistance = 10000,
                          ncores = 20,
                          distance_balance = TRUE,
                          get_geometry = FALSE)

routes_rail  <- otp_plan(otpcon, 
                        fromPlace = fromPlace,
                        toPlace = toPlace,
                        fromID = fromPlace$Zone_Code,
                        toID = toPlace$Zone_Code, 
                        date_time = lubridate::ymd_hms("2021/10/28 08:00:00"),
                        mode = c("WALK","RAIL","TRAM","SUBWAY","FERRY"),
                        maxWalkDistance = 10000,
                        ncores = 25,
                        distance_balance = TRUE,
                        get_geometry = TRUE)

saveRDS(routes_rail, "data/ttmatrix/route_benchmark_rail.Rds")
saveRDS(routes_bus, "data/ttmatrix/route_benchmark_bus.Rds")
saveRDS(routes_drive, "data/ttmatrix/route_benchmark_car.Rds")
saveRDS(routes_walk, "data/ttmatrix/route_benchmark_walk.Rds")
saveRDS(routes_cycle, "data/ttmatrix/route_benchmark_cycle.Rds")

routes_rail <- readRDS("data/ttmatrix/route_benchmark_rail.Rds")
routes_bus <- readRDS("data/ttmatrix/route_benchmark_bus.Rds")
routes_drive <- readRDS("data/ttmatrix/route_benchmark_car.Rds")
routes_walk <- readRDS("data/ttmatrix/route_benchmark_walk.Rds")
routes_cycle <- readRDS("data/ttmatrix/route_benchmark_cycle.Rds")

routes_rail <- routes_rail %>%
  group_by(fromPlace, toPlace) %>%
  summarise(duration = min(duration, na.rm = TRUE))

routes_bus <- routes_bus %>%
  group_by(fromPlace, toPlace) %>%
  summarise(duration = min(duration, na.rm = TRUE))

routes_drive <- routes_drive %>%
  group_by(fromPlace, toPlace) %>%
  summarise(duration = min(duration, na.rm = TRUE))

routes_walk <- routes_walk %>%
  group_by(fromPlace, toPlace) %>%
  summarise(duration = min(duration, na.rm = TRUE))

routes_cycle <- routes_cycle %>%
  group_by(fromPlace, toPlace) %>%
  summarise(duration = min(duration, na.rm = TRUE))

times_all <- data.frame(fromPlace = fromPlace$Zone_Code, toPlace = toPlace$Zone_Code)
times_all <- left_join(times_all, routes_rail, by = c("fromPlace","toPlace"))
times_all <- left_join(times_all, routes_bus, by = c("fromPlace","toPlace"))
times_all <- left_join(times_all, routes_drive, by = c("fromPlace","toPlace"))
times_all <- left_join(times_all, routes_walk, by = c("fromPlace","toPlace"))
times_all <- left_join(times_all, routes_cycle, by = c("fromPlace","toPlace"))
names(times_all) <- c("fromPlace","toPlace","rail","bus","car", "walk", "cycle")

# Try again for missign routes

routes_walk2  <- otp_plan(otpcon, 
                         fromPlace = fromPlace[is.na(times_all$walk),],
                         toPlace = toPlace[is.na(times_all$walk),],
                         fromID = fromPlace$Zone_Code[is.na(times_all$walk)],
                         toID = toPlace$Zone_Code[is.na(times_all$walk)], 
                         date_time = lubridate::ymd_hms("2021/10/28 08:00:00"),
                         mode = c("WALK"),
                         maxWalkDistance = 5000,
                         ncores = 20,
                         distance_balance = TRUE,
                         get_geometry = FALSE)

routes_cycle2  <- otp_plan(otpcon, 
                          fromPlace = fromPlace[is.na(times_all$cycle),],
                          toPlace = toPlace[is.na(times_all$cycle),],
                          fromID = fromPlace$Zone_Code[is.na(times_all$cycle)],
                          toID = toPlace$Zone_Code[is.na(times_all$cycle)], 
                          date_time = lubridate::ymd_hms("2021/10/28 08:00:00"),
                          mode = c("BICYCLE"),
                          maxWalkDistance = 5000,
                          ncores = 20,
                          distance_balance = TRUE,
                          get_geometry = FALSE)

routes_drive2  <- otp_plan(otpcon, 
                          fromPlace = fromPlace[is.na(times_all$car),],
                          toPlace = toPlace[is.na(times_all$car),],
                          fromID = fromPlace$Zone_Code[is.na(times_all$car)],
                          toID = toPlace$Zone_Code[is.na(times_all$car)], 
                          date_time = lubridate::ymd_hms("2021/10/28 08:00:00"),
                          mode = c("CAR"),
                          maxWalkDistance = 5000,
                          ncores = 20,
                          distance_balance = TRUE,
                          get_geometry = FALSE)

routes_bus2  <- otp_plan(otpcon, 
                          fromPlace = fromPlace[is.na(times_all$bus),],
                          toPlace = toPlace[is.na(times_all$bus),],
                          fromID = fromPlace$Zone_Code[is.na(times_all$bus)],
                          toID = toPlace$Zone_Code[is.na(times_all$bus)], 
                          date_time = lubridate::ymd_hms("2021/10/28 08:00:00"),
                          mode = c("WALK","BUS","FERRY"),
                          maxWalkDistance = 10000,
                          ncores = 20,
                          distance_balance = TRUE,
                          get_geometry = FALSE)

routes_rail2  <- otp_plan(otpcon, 
                          fromPlace = fromPlace[is.na(times_all$rail),],
                          toPlace = toPlace[is.na(times_all$rail),],
                          fromID = fromPlace$Zone_Code[is.na(times_all$rail)],
                          toID = toPlace$Zone_Code[is.na(times_all$rail)], 
                          date_time = lubridate::ymd_hms("2021/10/28 08:00:00"),
                          mode = c("WALK","RAIL","TRAM","SUBWAY","FERRY"),
                          maxWalkDistance = 10000,
                          ncores = 20,
                          distance_balance = TRUE,
                          get_geometry = FALSE)

saveRDS(routes_rail2, "data/ttmatrix/route_benchmark_rail2.Rds")
saveRDS(routes_bus2, "data/ttmatrix/route_benchmark_bus2.Rds")
saveRDS(routes_drive2, "data/ttmatrix/route_benchmark_car2.Rds")
saveRDS(routes_walk2, "data/ttmatrix/route_benchmark_walk2.Rds")
saveRDS(routes_cycle2, "data/ttmatrix/route_benchmark_cycle2.Rds")

routes_rail2 <- readRDS("data/ttmatrix/route_benchmark_rail2.Rds")
routes_bus2 <- readRDS("data/ttmatrix/route_benchmark_bus2.Rds")
routes_drive2 <- readRDS("data/ttmatrix/route_benchmark_car2.Rds")
routes_walk2 <- readRDS("data/ttmatrix/route_benchmark_walk2.Rds")
routes_cycle2 <- readRDS("data/ttmatrix/route_benchmark_cycle2.Rds")

routes_rail2 <- routes_rail2 %>%
  group_by(fromPlace, toPlace) %>%
  summarise(duration = min(duration, na.rm = TRUE))

routes_bus2 <- routes_bus2 %>%
  group_by(fromPlace, toPlace) %>%
  summarise(duration = min(duration, na.rm = TRUE))

routes_drive2 <- routes_drive2 %>%
  group_by(fromPlace, toPlace) %>%
  summarise(duration = min(duration, na.rm = TRUE))

routes_walk2 <- routes_walk2 %>%
  group_by(fromPlace, toPlace) %>%
  summarise(duration = min(duration, na.rm = TRUE))

routes_cycle2 <- routes_cycle2 %>%
  group_by(fromPlace, toPlace) %>%
  summarise(duration = min(duration, na.rm = TRUE))


routes_rail <- rbind(routes_rail, routes_rail2)
routes_bus <- rbind(routes_bus, routes_bus2)
routes_drive <- rbind(routes_drive, routes_drive2)
routes_walk <- rbind(routes_walk, routes_walk2)
routes_cycle <- rbind(routes_cycle, routes_cycle2)

times_all <- data.frame(fromPlace = fromPlace$Zone_Code, toPlace = toPlace$Zone_Code)
times_all <- left_join(times_all, routes_rail, by = c("fromPlace","toPlace"))
times_all <- left_join(times_all, routes_bus, by = c("fromPlace","toPlace"))
times_all <- left_join(times_all, routes_drive, by = c("fromPlace","toPlace"))
times_all <- left_join(times_all, routes_walk, by = c("fromPlace","toPlace"))
times_all <- left_join(times_all, routes_cycle, by = c("fromPlace","toPlace"))
names(times_all) <- c("fromPlace","toPlace","rail","bus","car", "walk", "cycle")

qtm(foo[is.na(times_all$cycle)])

# tt matrix
# car
mat_car <- read.csv("data/ttmatrix/final/car_with_routes.csv")
rownames(mat_car) <- mat_car[,1]
mat_car$X <- NULL
mat_car <- as.matrix(mat_car)


# bus
mat_bus <- read.csv("data/ttmatrix/final/bus.csv")
rownames(mat_bus) <- mat_bus[,1]
mat_bus$X <- NULL
mat_bus <- as.matrix(mat_bus)


# rail
mat_rail <- read.csv("data/ttmatrix/final/rail.csv") 
rownames(mat_rail) <- mat_rail[,1]
mat_rail$X <- NULL
mat_rail <- as.matrix(mat_rail)


# walk
mat_walk <- read.csv("data/ttmatrix/final/walk.csv") 
rownames(mat_walk) <- mat_walk[,1]
mat_walk$X <- NULL
mat_walk <- as.matrix(mat_walk)


# cycle
mat_cycle <- read.csv("data/ttmatrix/final/cycle.csv") 
rownames(mat_cycle) <- mat_cycle[,1]
mat_cycle$X <- NULL
mat_cycle <- as.matrix(mat_cycle)

mat_bus = mat_bus[rownames(mat_car), colnames(mat_car)]
mat_cycle = mat_cycle[rownames(mat_car), colnames(mat_car)]
mat_walk = mat_walk[rownames(mat_car), colnames(mat_car)]
mat_rail = mat_rail[rownames(mat_car), colnames(mat_car)]



dur <- list()
for(i in 1:1000){
  dur[[i]] <- mat_car[times_all$toPlace[i],times_all$fromPlace[i]]
}
times_all$car_mat <- unlist(dur)
plot(times_all$car/3600, times_all$car_mat/3600)
abline(0,1, col = "red")

dur <- list()
for(i in 1:1000){
  dur[[i]] <- mat_walk[times_all$toPlace[i],times_all$fromPlace[i]]
}
times_all$walk_mat <- unlist(dur)
plot(times_all$walk/3600, times_all$walk_mat/3600)
abline(0,1, col = "red")

dur <- list()
for(i in 1:1000){
  dur[[i]] <- mat_cycle[times_all$toPlace[i],times_all$fromPlace[i]]
}
times_all$cycle_mat <- unlist(dur)
plot(times_all$cycle/3600, times_all$cycle_mat/3600)
abline(0,1, col = "red")

dur <- list()
for(i in 1:1000){
  dur[[i]] <- mat_bus[times_all$toPlace[i],times_all$fromPlace[i]]
}
times_all$bus_mat <- unlist(dur)
plot(times_all$bus/3600, times_all$bus_mat/3600)
abline(0,1, col = "red")

dur <- list()
for(i in 1:1000){
  dur[[i]] <- mat_rail[times_all$toPlace[i],times_all$fromPlace[i]]
}
times_all$rail_mat <- unlist(dur)
plot(times_all$rail/3600, times_all$rail_mat/3600)
abline(0,1, col = "red")


times_all$rail_diff <- times_all$rail_mat - times_all$rail
times_all$bus_diff <- times_all$bus_mat - times_all$bus


ntem <- st_read("data/NTEM/NTEM_centroids_mod.geojson")
ntem <- ntem[match(rownames(mat_rail), ntem$Zone_Code),]
id <- "S99900125"
id2 <- "E02003951"
ntem$times <- mat_rail[,id]
ntem$times <- ntem$times / 3600
tm_shape(ntem) +
  tm_dots(col = "times", n = 20, palette = "Spectral") +
  tm_shape(ntem[ntem$Zone_Code == id,]) +
  tm_dots() +
  tm_shape(ntem[ntem$Zone_Code == id2,]) +
  tm_dots()

