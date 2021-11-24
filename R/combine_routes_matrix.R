# Inputs

ttmat_text = "ttmatrix_cycle_chunk_"
routes_text = ""
out_text = "data/ttmatrix/final/cycle.csv"
threshold = 5
with_routes = FALSE

if(with_routes){
  # Read in routes
  files <- list.files("data/ttmatrix/", pattern = routes_text, full.names = TRUE)
  
  res <- list()
  for(i in 1:length(files)){
    sub <- readRDS(files[i])
    sub <- sub %>% 
      group_by(fromPlace, toPlace) %>%
      summarise(duration = min(duration))
    res[[i]] <- sub
  }
  res <- bind_rows(res)
  
  mat_route <- data.frame(fromPlace = rep(unique(res$fromPlace), each = length(unique(res$toPlace))),
                          toPlace = rep(unique(res$toPlace), times = length(unique(res$fromPlace))))
  mat_route <- left_join(mat_route, res, by = c("fromPlace","toPlace"))
  mat_route <- matrix(mat_route$duration, ncol = length(unique(res$fromPlace)),
                      dimnames = list(unique(mat_route$toPlace), unique(mat_route$fromPlace)))
}

# Read tin ttmatrix
files <- list.files("data/ttmatrix/", pattern = ttmat_text, full.names = TRUE)

res <- list()
for(i in 1:length(files)){
  res[[i]] <- readRDS(files[i])
}

mat_tt <- res[[1]]
for(i in 2:length(res)){
  mat_tt_sub <- res[[i]]
  mat_tt_sub <- mat_tt_sub[rownames(mat_tt),]
  mat_tt <- cbind(mat_tt, mat_tt_sub)
}
mat_tt <- as.matrix(mat_tt)

if(with_routes){
  # Merge matrixes
  mat_na <- matrix(NA, nrow = nrow(mat_route), ncol = ncol(mat_tt) - ncol(mat_route))
  colnames(mat_na) <- colnames(mat_tt)[!colnames(mat_tt) %in% colnames(mat_route)]
  
  mat_route <- cbind(mat_route, mat_na)
  
  mat_na <- matrix(NA, ncol = ncol(mat_route), nrow = nrow(mat_tt) - nrow(mat_route))
  rownames(mat_na) <- rownames(mat_tt)[!rownames(mat_tt) %in% rownames(mat_route)]
  
  mat_route <- rbind(mat_route, mat_na)
  
  summary(as.numeric(mat_route))
  summary(as.numeric(mat_tt))
  
  mat_route <- mat_route[rownames(mat_tt), colnames(mat_tt)]
  summary(rownames(mat_tt) == rownames(mat_route))
  summary(colnames(mat_tt) == colnames(mat_route))
  mat <- ifelse(is.na(mat_tt), mat_route, mat_tt)
  summary(as.numeric(mat))
} else {
  mat = mat_tt
}



source("R/batch_check_travel_times.R")

newmat <- odmatrix_interpolate(mat, threshold) # 4h
newmat2 <- odmatrix_interpolate(newmat, threshold) # 8h
newmat3 <- odmatrix_interpolate(newmat2, threshold) # 16h
newmat4 <- odmatrix_interpolate(newmat3, threshold) # 32h
nmna <- as.logical(is.na(newmat4))
message(sum(nmna)/length(nmna))

saveRDS(newmat4,out_text)
