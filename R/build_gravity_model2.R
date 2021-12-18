library(sf)
library(ggplot2)
library(tmap)
library(tidyr)
library(dplyr)
library(stplanr)
tmap_mode("view")

#TODO: I 61 is 0% sucessf may be cuase of missing rail
# NTEM
ntem <- readRDS("data/TEMPRO/GB-OA-Day-Baseline.Rds")
ntem_cents <- st_read("data/NTEM/NTEM_centroids_mod.geojson")
ntem_cents$region[ntem_cents$region != "London"] <- "Not London"

curves <- readRDS("data/NTS/decay_curves_v2.Rds")

curves$purpose <- gsub("Not Home Based ","NHB_",curves$purpose)
#curves$purpose <- gsub("Not HomeBased ","NHB_",curves$purpose)
curves$purpose <- gsub("Home Based ","HB_",curves$purpose)
#curves$purpose <- gsub("HomeBased ","HB_",curves$purpose)

curves$purpose <- gsub("Education","education",curves$purpose)
curves$purpose <- gsub("Employers Business","business",curves$purpose)
curves$purpose <- gsub("Holiday/Day Trip","holiday",curves$purpose)
curves$purpose <- gsub("Personal Business","personal",curves$purpose)
curves$purpose <- gsub("Recreation/Social","recreation",curves$purpose)
curves$purpose <- gsub("Visiting Friends and Relatives","friends",curves$purpose)
curves$purpose <- gsub("Shopping","shopping",curves$purpose)
curves$purpose <- gsub("Work","work",curves$purpose)

# Tempaory Fix #TODO: Reaplce with Ian fixed data
#curves <- curves[curves$purpose != "NHB_friends",]
curves_extra <- curves[curves$purpose == "HB_work",]
curves_extra$purpose = "NHB_work"

curves <- rbind(curves, curves_extra)
# Combine some 
# ntem$HB_holiday_origin = ntem$HB_holiday_origin + ntem$HB_recreation_origin + ntem$HB_friends_origin
# ntem$NHB_holiday_origin = ntem$NHB_holiday_origin + ntem$NHB_recreation_origin #+ ntem$NHB_friends_origin
# ntem$HB_holiday_destination = ntem$HB_holiday_destination + ntem$HB_recreation_destination + ntem$HB_friends_destination
# ntem$NHB_holiday_destination = ntem$NHB_holiday_destination + ntem$NHB_recreation_destination #+ ntem$NHB_friends_destination

# tt matrix
# car
mat_car <- read.csv("data/ttmatrix/final/car_with_routes.csv")
rownames(mat_car) <- mat_car[,1]
mat_car$X <- NULL
mat_car <- as.matrix(mat_car)
mat_car <- mat_car/ 60

# bus
mat_bus <- read.csv("data/ttmatrix/final/bus.csv")
rownames(mat_bus) <- mat_bus[,1]
mat_bus$X <- NULL
mat_bus <- as.matrix(mat_bus)
mat_bus <- mat_bus/ 60

# rail
mat_rail <- read.csv("data/ttmatrix/final/rail.csv") 
rownames(mat_rail) <- mat_rail[,1]
mat_rail$X <- NULL
mat_rail <- as.matrix(mat_rail)
mat_rail <- mat_rail/ 60

# walk
mat_walk <- read.csv("data/ttmatrix/final/walk.csv") 
rownames(mat_walk) <- mat_walk[,1]
mat_walk$X <- NULL
mat_walk <- as.matrix(mat_walk)
mat_walk <- mat_walk/ 60

# cycle
mat_cycle <- read.csv("data/ttmatrix/final/cycle.csv") 
rownames(mat_cycle) <- mat_cycle[,1]
mat_cycle$X <- NULL
mat_cycle <- as.matrix(mat_cycle)
mat_cycle <- mat_cycle/ 60

#Standardise the matrix orders
mat_car <- mat_car[match(ntem_cents$Zone_Code,rownames(mat_car)),]
mat_car <- mat_car[,match(ntem_cents$Zone_Code,colnames(mat_car))]

mat_bus <- mat_bus[match(ntem_cents$Zone_Code,rownames(mat_bus)),]
mat_bus <- mat_bus[,match(ntem_cents$Zone_Code,colnames(mat_bus))]

mat_rail <- mat_rail[match(ntem_cents$Zone_Code,rownames(mat_rail)),]
mat_rail <- mat_rail[,match(ntem_cents$Zone_Code,colnames(mat_rail))]

mat_walk <- mat_walk[match(ntem_cents$Zone_Code,rownames(mat_walk)),]
mat_walk <- mat_walk[,match(ntem_cents$Zone_Code,colnames(mat_walk))]

mat_cycle <- mat_cycle[match(ntem_cents$Zone_Code,rownames(mat_cycle)),]
mat_cycle <- mat_cycle[,match(ntem_cents$Zone_Code,colnames(mat_cycle))]


summary(rownames(mat_car) == rownames(mat_bus))
summary(rownames(mat_car) == rownames(mat_rail))
summary(rownames(mat_car) == rownames(mat_walk))
summary(rownames(mat_car) == rownames(mat_cycle))
        
summary(colnames(mat_car) == colnames(mat_bus))
summary(colnames(mat_car) == colnames(mat_rail))
summary(colnames(mat_car) == colnames(mat_walk))
summary(colnames(mat_car) == colnames(mat_cycle))

# Curve from Ian's work
prob_curve <- function(x, a = 0.06756, b = 5.29572, c = 1){
  c * a * b * exp(-a * x) * ((1/(exp(-a * x) + 1))^(b+1))
}

od_to_flow <- function(mat){
  data.frame(from = rep(colnames(mat), each = nrow(mat)),
             to = rep(rownames(mat), times = ncol(mat)),
             flow = as.numeric(mat))
}

# Furness method balancing
furness <- function(mat, rsum, csum, n = 3){
  
  rname <- rownames(mat)
  cname <- colnames(mat)
  
  # Get scale about right
  mat <- mat / (sum(mat, na.rm = TRUE) / sum(rsum))
  #mat_orig = mat
  
  bal_func <- function(mat, rsum, csum){
    # Find ratio of rows
    mat_rsum <- rowSums(mat, na.rm = TRUE)
    mat_rratio <- rsum / mat_rsum
    mat_rratio[is.nan(mat_rratio)] <- 0
    
    mat <- mat * mat_rratio
    
    # Find ratio of rows
    mat_csum <- colSums(mat, na.rm = TRUE)
    mat_cratio <- csum / mat_csum
    mat_cratio[is.nan(mat_cratio)] <- 0
    
    mat <- sweep(mat, MARGIN=2, mat_cratio, `*`)
    mat[is.nan(mat)] <- 0 
    return(mat)
  }
  
  for(i in seq_len(n)){
    mat <- bal_func(mat, rsum, csum)
    if(i == 1){
      message("First pass")
      print(summary(rowSums(mat, na.rm = TRUE) - rsum))
    }
    if(i == n){
      message("Last pass")
      print(summary(rowSums(mat, na.rm = TRUE) - rsum))
    }
  }
  
  
  rownames(mat) <- rname
  colnames(mat) <- cname
  
  return(mat)
}



# Build a list of weighting matrixes
mode <- unique(curves$mode)
purpose <- unique(curves$purpose)

mat_types <- crossing(mode = mode,
         purpose = purpose)


list_weights <- list()

for(i in 1:nrow(mat_types)){
  mode_sub <- mat_types$mode[i]
  purpose_sub <- mat_types$purpose[i]
  
  message(i,"/",nrow(mat_types)," ",mode_sub," ",purpose_sub," ",Sys.time())
  curves_sub <- curves[curves$mode == mode_sub & curves$purpose == purpose_sub,]
  if(nrow(curves_sub) != 4){
    stop("Wrong number of options")
  }
                                                    
  # Select the right matrix to use
  if(mode_sub == "Bicycle" ){
    mat_sub <- mat_cycle
  } else if (mode_sub == "bus") {
    mat_sub <- mat_bus
  } else if (mode_sub == "Car / van driver") {
    mat_sub <- mat_car
  } else if (mode_sub == "Car / van passenger") {
    mat_sub <- mat_car
  } else if (mode_sub == "rail") {
    mat_sub <- mat_rail
  } else if (mode_sub == "Walk") {
    mat_sub <- mat_walk
  } else {
    stop("Unknown mode")
  }
  
  mat_sub_urb_lon <- prob_curve(mat_sub, 
                                a = curves_sub$a[curves_sub$rural_urb == "Urban" & curves_sub$region == "London"], 
                                b = curves_sub$b[curves_sub$rural_urb == "Urban" & curves_sub$region == "London"],
                                c = curves_sub$c[curves_sub$rural_urb == "Urban" & curves_sub$region == "London"])
  mat_sub_rur_lon <- prob_curve(mat_sub, 
                                a = curves_sub$a[curves_sub$rural_urb == "Rural" & curves_sub$region == "London"], 
                                b = curves_sub$b[curves_sub$rural_urb == "Rural" & curves_sub$region == "London"],
                                c = curves_sub$c[curves_sub$rural_urb == "Rural" & curves_sub$region == "London"])
  mat_sub_urb_nlon <- prob_curve(mat_sub, 
                                 a = curves_sub$a[curves_sub$rural_urb == "Urban" & curves_sub$region == "Not London"], 
                                 b = curves_sub$b[curves_sub$rural_urb == "Urban" & curves_sub$region == "Not London"],
                                 c = curves_sub$c[curves_sub$rural_urb == "Urban" & curves_sub$region == "Not London"])
  mat_sub_rur_nlon <- prob_curve(mat_sub, 
                                 a = curves_sub$a[curves_sub$rural_urb == "Rural" & curves_sub$region == "Not London"], 
                                 b = curves_sub$b[curves_sub$rural_urb == "Rural" & curves_sub$region == "Not London"],
                                 c = curves_sub$c[curves_sub$rural_urb == "Rural" & curves_sub$region == "Not London"])
  
  mat_fin <- mat_sub_urb_nlon
  mat_fin[,ntem_cents$rural_urban == "Urban" & ntem_cents$region == "London"]     <- mat_sub_urb_lon[,ntem_cents$rural_urban == "Urban" & ntem_cents$region == "London"]
  mat_fin[,ntem_cents$rural_urban == "Rural" & ntem_cents$region == "Not London"] <- mat_sub_rur_nlon[,ntem_cents$rural_urban == "Rural" & ntem_cents$region == "Not London"]
  mat_fin[,ntem_cents$rural_urban == "Rural" & ntem_cents$region == "London"]     <- mat_sub_rur_lon[,ntem_cents$rural_urban == "Rural" & ntem_cents$region == "London"]
  
  if(anyNA(mat_fin)){
    message("NAs introduced")
  }
  
  list_weights[[i]] <- mat_fin
  
}

list_flow <- list()

for(i in 1:nrow(mat_types)){
  mode_sub <- mat_types$mode[i]
  purpose_sub <- mat_types$purpose[i]
  
  message(i,"/",nrow(mat_types)," ",mode_sub," ",purpose_sub," ",Sys.time())
  
  mat_sub <- list_weights[[i]]
  
  if(mode_sub == "Bicycle" ){
    md = "cycle"
  } else if (mode_sub == "bus") {
    md = "bus"
  } else if (mode_sub == "Car / van driver") {
    md = "drive"
  } else if (mode_sub == "Car / van passenger") {
    md = "passenger"
  } else if (mode_sub == "rail") {
    md = "rail"
  } else if (mode_sub == "Walk") {
    md <- "walk"
  } else {
    stop("Unknown mode")
  }
  
  ntem_sub <- ntem[ntem$mode == md,]
  
  ntem_sub <- ntem_sub[match(colnames(mat_sub), ntem_sub$msoa),]
  ntem_sub <- as.data.frame(ntem_sub)
  summary(ntem_sub$msoa == colnames(mat_sub))
  
  origins <- ntem_sub[,paste0(purpose_sub,"_origin")]
  destinations <- ntem_sub[,paste0(purpose_sub,"_destination")]
  
  #Convert Int to Float
  origins <- as.numeric(origins)
  destinations <- as.numeric(destinations)
  
  names(origins) <- ntem_sub$msoa
  names(destinations) <- ntem_sub$msoa
  
  # Make the Home Based Matrix
  mat_hb <- matrix(destinations, ncol = 1) %*% matrix(origins, ncol = 7700)
  mat_hb <- mat_hb * mat_sub
  mat_hb <- round(mat_hb, 10)
  mat_hb[is.infinite(mat_hb)] <- 0
  
  mat_hb = furness(mat_hb, destinations, origins, 20)
  
  # Check totals
  total_mat = sum(mat_hb, na.rm = TRUE)
  total_orig = sum(origins)
  total_dest = sum(destinations)
  
  message("Captured ",round(total_mat/total_orig * 100,4),"% of origins")
  message("Captured ",round(total_mat/total_dest * 100,4),"% of destinations")
  
  # Convert matrix to flow
  mat_hb <- od_to_flow(mat_hb)

  mat_hb$flow <- round(mat_hb$flow)
  mat_hb$flow[is.na(mat_hb$flow)] <- 0
  mat_hb <- mat_hb[mat_hb$flow > 0,]
  
  
  names(mat_hb) <- c("from", "to",paste0(purpose_sub,"_",md))

  list_flow[[i]] <- mat_hb
  
}



# rm(mat_hb, mat_nhb, curves, curves_sub, mat_bus, mat_car, mat_cycle, mat_fin, mat_rail,
#    mat_sub, mat_sub_rur_lon, mat_sub_rur_nlon, mat_sub_urb_lon, mat_sub_urb_nlon,
#    mat_walk, list_weights)

flow_all = list_flow[[1]]
flow_all = flow_all[!is.na(flow_all$HB_cycle_business),]
flow_all = flow_all[flow_all$HB_cycle_business > 0,]

for(i in 2:length(list_flow)){
  message(Sys.time()," ",i," nflows = ",nrow(flow_all))
  flow_hb <- list_flow[[i]]
  # flow_hb <- flow_hb[!is.na(flow_hb[,3]),]
  # flow_hb <- flow_hb[flow_hb[,3] > 0,]
  message(nrow(flow_hb))
  flow_all <- full_join(flow_all, flow_hb, by = c("from","to"))
}

flow_all <- flow_all[rowSums(flow_all[,3:ncol(flow_all)], na.rm = TRUE) > 0,]
flow_all$cycle <- rowSums(flow_all[,grepl("cycle",names(flow_all))], na.rm = TRUE)
flow_all$drive <- rowSums(flow_all[,grepl("drive",names(flow_all))], na.rm = TRUE)
flow_all$passenger <- rowSums(flow_all[,grepl("passenger",names(flow_all))], na.rm = TRUE)
flow_all$walk <- rowSums(flow_all[,grepl("walk",names(flow_all))], na.rm = TRUE)
flow_all$rail <- rowSums(flow_all[,grepl("rail",names(flow_all))], na.rm = TRUE)
flow_all$bus <- rowSums(flow_all[,grepl("bus",names(flow_all))], na.rm = TRUE)

flow_all <- flow_all[,c("from","to","cycle","drive","passenger","walk","rail","bus")]
flow_all$lgv <- 0
flow_all$hgv <- 0

flow_all$key = od_id_szudzik(flow_all$from, flow_all$to)

summary(flow_all)
saveRDS(flow_all,"data/NTEM/NTEM_flows_mode.Rds")


flow_oneway <- flow_all %>% 
  group_by(key) %>%
  summarise(from = from[1],
            to = to[1],
            cycle = sum(cycle, na.rm = TRUE),
            drive = sum(drive, na.rm = TRUE),
            passenger = sum(passenger, na.rm = TRUE),
            walk = sum(walk, na.rm = TRUE),
            rail = sum(rail, na.rm = TRUE),
            bus = sum(bus, na.rm = TRUE),
            lgv = sum(lgv, na.rm = TRUE),
            hgv = sum(hgv, na.rm = TRUE))

flow_oneway$key <- NULL

# Overall Benchmars

ntem_totals = ntem %>%
  select(-msoa) %>%
  select(-msoa_name) %>%
  select(-contains("origin")) %>%
  select(-contains("recreation")) %>%
  select(-contains("friends")) %>%
  group_by(mode) %>%
  summarise_all(sum, na.rm = TRUE)
ntem_totals$total <- rowSums(ntem_totals[,2:ncol(ntem_totals)])
ntem_totals <- ntem_totals[,c("mode","total")]

flow_totals = flow_all
flow_totals$key <- 1
flow_totals = flow_totals %>%
  select(-from) %>%
  select(-to) %>%
  group_by(key) %>%
  summarise_all(sum, na.rm = T)
flow_totals$key = NULL
flow_totals = tidyr::pivot_longer(flow_totals, cols = names(flow_totals))
names(flow_totals) = c("mode","flow")
ntem_totals <- left_join(ntem_totals, flow_totals, by = "mode")
ntem_totals$diff <- ntem_totals$total - ntem_totals$flow
ntem_totals$percent <- ntem_totals$total / ntem_totals$flow
ntem_totals

flow_sf <- od2line(flow_oneway,ntem_cents)

# Weight flows to make ntem original mode share
flow_sf$cycle <- round(flow_sf$cycle * ntem_totals$percent[ntem_totals$mode == "cycle"])
flow_sf$drive <- round(flow_sf$drive * ntem_totals$percent[ntem_totals$mode == "drive"])
flow_sf$passenger <- round(flow_sf$passenger * ntem_totals$percent[ntem_totals$mode == "passenger"])
flow_sf$walk <- round(flow_sf$walk * ntem_totals$percent[ntem_totals$mode == "walk"])
flow_sf$rail <- round(flow_sf$rail * ntem_totals$percent[ntem_totals$mode == "rail"])
flow_sf$bus <- round(flow_sf$bus * ntem_totals$percent[ntem_totals$mode == "bus"])

flow_sf$all <- rowSums(st_drop_geometry(flow_sf[,c("cycle", "drive", "passenger", "walk", "rail", "bus", "lgv", "hgv")]))

write_sf(flow_sf,"data/NTEM/NTEM_desire_lines.geojson", delete_dsn = TRUE)



summary(flow_all)

saveRDS(flow_all,"data/NTEM/NTEM_flows_mode.Rds")

Mypal <- c("#67001f",
  "#b2182b",
  "#d6604d",
  "#f4a582",
  "#fddbc7",
  "#d1e5f0",
  "#92c5de",
  "#4393c3",
  "#2166ac",
  "#053061")

foo <- flow_sf[flow_sf$all > 500,]
tm_shape(foo) +
  tm_lines(lwd = "all", 
           col = "all", 
           palette = Mypal,
           style = "fixed",
           scale = 10,
           breaks = c(0,500,600,700,800,900,1000,5000,10000,20000,40000))

bar = flow_sf[flow_sf$from == "E02003250" | flow_sf$to == "E02003250",]
tm_shape(bar) +
  tm_lines(lwd = "rail", 
           col = "rail", 
           palette = Mypal,
           n = 10,
           scale = 4)
