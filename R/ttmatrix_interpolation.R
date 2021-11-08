# Testing matrix interpolation

#mat <- mat[rownames(mat) %in% colnames(mat), ]

mat <- as.matrix(mat)
mat_orig <- mat
# Loop over each cell in the matrix 

# Reused variaibles
mat_colnames <- colnames(mat)
mat_rownames <- rownames(mat)


for(row in 1:nrow(mat)){
  if(row %% 10 == 0){
    message("row ",row)
  }
  for(col in 1:ncol(mat)){
    i <- mat[row, col]
    if(is.na(i)){
      to = mat_rownames[row]
      from = mat_colnames[col]
      
      inter1 <- mat_colnames[!is.na(mat[to,])]
      inter2 <- mat_rownames[!is.na(mat[,from])]
      
      midpoints <- inter1[inter1 %in% inter2]
      if(length(midpoints) > 0){
        lths <- list()
        for(j in seq_len(length(midpoints))){
          lths[[j]] <- mat[to, midpoints[j]] + mat[midpoints[j], from]
        }
        lths <- unlist(lths)
        minres <- min(lths, na.rm = TRUE)
        
        if(is.infinite(minres)){
          message("check ",row," ",col)
        }
        mat[row, col] <- minres
        
      }
      
     
    }
  }
}

# Ideas 
# go though by column
# functionalise - would work on orginal matrix? so need muliple passes?


interploate_times <- function(n, mat, dim = 7700){
  col <- (n-1)%/%dim + 1
  row <- n - (dim * col) + dim

  i <- mat[row, col]
  if(is.na(i)){
    mat_colnames <- colnames(mat)
    mat_rownames <- rownames(mat)
    
    to = mat_rownames[row]
    from = mat_colnames[col]
    
    inter1 <- mat_colnames[!is.na(mat[to,])]
    inter2 <- mat_rownames[!is.na(mat[,from])]
    
    midpoints <- inter1[inter1 %in% inter2]
    if(length(midpoints) > 0){
      lths <- list()
      for(j in seq_len(length(midpoints))){
        lths[[j]] <- mat[to, midpoints[j]] + mat[midpoints[j], from]
      }
      lths <- unlist(lths)
      minres <- min(lths, na.rm = TRUE)
      
      if(is.infinite(minres)){
        message("check ",row," ",col)
      }
      return(minres)
    } else {
      return(NA)
    }
  } else {
    return(i)
  }
}


cl <- parallel::makeCluster(36)
res <- pbapply::pblapply(1:(7700 * 7700), 
                         interploate_times, 
                         mat = mat,
                         dim = 7700,
                         cl = cl)
parallel::stopCluster(cl)
res <- matrix(unlist(res), ncol = 7700)
rownames(res) <- rownames(mat)
colnames(res) <- colnames(mat)

# Took 2.5 hours for first iteration
# from 56713472 NAs to 49503479 
# Increase travel time to 4 hours will need 3-4 interations for cover 24 hours

cl <- parallel::makeCluster(36)
res2 <- pbapply::pblapply(1:(7700 * 7700), 
                         interploate_times, 
                         mat = res,
                         dim = 7700,
                         cl = cl)
parallel::stopCluster(cl)
res2 <- matrix(unlist(res2), ncol = 7700)
rownames(res2) <- rownames(mat)
colnames(res2) <- colnames(mat)


foo = mat[,"E02003950", drop=FALSE]
foo = data.frame(Zone_Code = rownames(foo), dists = foo$E02003950)

foo = left_join(ntem_cents[ntem_cents$Zone_Code %in% rownames(mat),], foo, by = "Zone_Code" )
foo$dists <- foo$dists / (60 * 60)
qtm(foo, dots.col = "dists")



qtm(ntem_cents[ntem_cents$Zone_Code %in% inter1,], dots.col = "yellow") +
  qtm(ntem_cents[ntem_cents$Zone_Code %in% inter2,], dots.col = "black") +
qtm(ntem_cents[ntem_cents$Zone_Code == from,], dots.col = "red") +
  qtm(ntem_cents[ntem_cents$Zone_Code == to,], dots.col = "green")
  


