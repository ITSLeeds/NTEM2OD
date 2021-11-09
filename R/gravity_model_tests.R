library(sf)
library(ggplot2)
library(tmap)
library(dplyr)
tmap_mode("view")

# NTEM
ntem <- readRDS("data/TEMPRO/GB-OA-Day-Baseline.Rds")
ntem_zone <- read_sf("data/NTEM_bounds.gpkg")
ntem_cents <- st_centroid(ntem_zone)

#TTWA
ttwa <- read_sf("data/TTWA/TTWA_GB.gpkg")

# Centroids
cents <- read_sf("data/NTEM/NTEM_centroids_mod.geojson")
dists <- st_distance(cents)
dim(dists)
dists <- as.numeric(dists)
dists <- dists / 1000 # convert to km
dists <- matrix(dists, ncol = 7700)
row.names(dists) <- cents$Zone_Code
colnames(dists) <- cents$Zone_Code

# Get curve from Census 
census <- pct::get_od()
census <- census[,c("geo_code1", "geo_code2","car_driver" )]
census <- census[census$geo_code2 %in% cents$Zone_Code,]
census <- stplanr::od2line(census, cents)
census$length <- as.numeric(st_length(census)) / 1000 
census <- st_drop_geometry(census)


# Produce Density Histogram
m <- ggplot(census, aes(length, y =..density.. , weight = car_driver)) + 
  geom_histogram(binwidth = 1) +
  geom_density()
p <- ggplot_build(m)

head(p$data[[1]])

x = p$data[[1]]$x
y = p$data[[1]]$density

y = y / max(y)

#write.csv(data.frame(x = x, y = y), "commute_curve.csv")


y[1] = 0.4

# decay_func <- function(x,a = 0.2,b = 0.2,c = 1, d = 0){
#   c * (x^a) * exp(b * -x) + d
# }

decay_func <- function(x,a = 0.22,b = 0.1214){
  out <- (x^a) * exp(b * -x)
  #out[x == 0] <- 0.9
  return(out)
}

model <- nls(y ~ decay_func(x,a,b), start=list(a=0.2,b=0.2))
model

plot(x,y,xlim = c(0,100)) +
  lines(sapply(x, decay_func, a = 0.22, b = 0.1214), col = "red") +
  lines(sapply(x, decay_func, a = 0.55804 , b = 0.20016 , c = 0.80191 ,d=0.08261), col = "green")

bins = data.frame(x = x, y = y)

# x = 0:60
# y = (x^0.5) * exp(-0.1*x) 
# 
# plot(x, exp(-x)) +
#   lines(exp(-0.5*x), col = "red") +
#   lines(exp(-0.25*x), col = "blue")  +
#   lines(exp(-0.1*x), col = "green")  +
#   
#   plot(x, y)


#60 min in car at 50 mph / 80 kph is 80000m, 1333 metres per min
# costs <- dists
# costs[1:10,1:10]
# summary(as.numeric(costs))

# weight_distances <- function(dists, bins){
#   dr <- round(dists)
#   dr <- bins$y[match(dr, bins$x)]
#   dr[is.na(dr)] <- 0
#   dr <- matrix(dr, ncol = 7700)
#   return(dr)
# }



weights <- decay_func(dists, 0.22, 0.1214)
#weights <- weight_distances(dists, bins)

weights[1:10,1:10]
#weights_old[1:10,1:10]
summary(as.numeric(weights))

ntem_drive <- ntem[ntem$mode == "drive",]
ntem_drive <- ntem_drive[match(ntem_drive$msoa, cents$Zone_Code),]
origins <- ntem_drive$HB_work_origin
destinations <- ntem_drive$HB_work_destination
names(origins) <- cents$Zone_Code
names(destinations) <- cents$Zone_Code


res <- list()
# Fill the matrix
#res_ave <- (sum(origins) + sum(destinations))
for(col in 1:7700){
  for(row in 1:7700){
    i <- (col - 1) * 7700 + row
    res[[i]] <- (origins[row] * destinations[col]) * (weights[row,col])
    if(i %% 10000000 == 0){
      message(Sys.time()," ",i)
    }
  }
}

res <- unlist(res)
res <- matrix(res, ncol = 7700)
res[is.infinite(res)] <- 0

row.names(res) <- cents$Zone_Code
colnames(res) <- cents$Zone_Code

# Furness method balancing
furness <- function(mat, rsum, csum, n = 3){
  
  #if(sum(rsum) != sum(csum)){
   # stop("rsum != csum")
  #}
  
  # Get scale about right
  mat <- mat / (sum(mat) / sum(rsum))
  mat_orig = mat
  
  bal_func <- function(mat, rsum, csum){
    # Find ratio of rows
    mat_rsum <- rowSums(mat)
    mat_rratio <- rsum / mat_rsum
    
    mat <- mat * mat_rratio
    
    # Find ratio of rows
    mat_csum <- colSums(mat)
    mat_cratio <- csum / mat_csum
    
    mat <- sweep(mat, MARGIN=2, mat_cratio, `*`)
    return(mat)
  }
  
  for(i in seq_len(n)){
    mat <- bal_func(mat, rsum, csum)
    print(summary(rowSums(mat) / rsum))
  }
  
  return(mat)
}





foo = furness(res, origins, destinations, 20)
row.names(foo) = cents$Zone_Code
colnames(foo) = cents$Zone_Code

sum(foo[row.names(foo) == "E02003156",])
origins[names(origins) == "E02003156"]

sum(foo[,colnames(foo) == "E02003156" ])
destinations[names(destinations) == "E02003156"]

summary(rowSums(foo) - origins)


fizz = cents$Zone_Code[(colSums(foo) - origins) == max((colSums(foo) - origins))]
res_weighted = foo

#summary(as.numeric(res))
#res_weighted <- res / (sum(res) / sum(c(origins, destinations)))
#sum(res_weighted)

res_weighted <- round(res_weighted)
quantile(as.numeric(res_weighted), probs = seq(0,1,0.1))

#row.names(res_weighted) <- cents$Zone_Code
#colnames(res_weighted) <- cents$Zone_Code

res_weighted[res_weighted == 0] <- NA

resod <- stplanr::odmatrix_to_od(res_weighted)
resod <- resod[resod$orig != resod$dest, ]
resod <- stplanr::od2line(resod, cents)
saveRDS(resod,"data/test_desire_lines.Rds")

sub <- resod

sub$id <- paste0(sub$orig," ",sub$dest)
census$id <- paste0(census$geo_code1," ",census$geo_code2)
sub <- left_join(sub, census, by = "id")
sub$car_driver[is.na(sub$car_driver)] <- 0
sub <- sub[sub$car_driver > 100 | sub$flow > 100,]

sub$ratio <- sub$flow / sub$car_driver
sub <- sub[,c("id","ratio","flow","car_driver")]
sub <- sub[substr(sub$id,1,1) != "S",]

#re_colsum <- colSums(res_weighted, na.rm= TRUE)

#foo = data.frame(orig = origins, new = re_colsum)

tm_shape(sub) +
  tm_lines(lwd = "flow",
           scale = 20,
           col = "ratio",
           style = "fixed",
           breaks = c(0,0.5,0.9,1.1,2,10,Inf),
           midpoint = 2, 
           palette = c("red","orange","green","lightblue","darkblue","black"),
           popup.vars = c("ratio","flow","car_driver"))

# sub2 <- resod[resod$orig == "E02005403" | resod$dest == "E02005403",]
# qtm(sub2, lines.lwd = "flow")
