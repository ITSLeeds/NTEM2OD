mat <- mat[rownames(mat) %in% colnames(mat),]
mat <- as.matrix(mat)
mat <- mat[colnames(mat),]

col = 4700
qtm(cents[cents$Zone_Code %in% colnames(mat)[col],])

times_to_midpoints <- mat[col, ]
times_to_midpoints_isna <- is.na(times_to_midpoints)

with_times <- times_to_midpoints[!times_to_midpoints_isna]
qtm(cents[cents$Zone_Code %in% names(with_times),])


mat_sub <- mat[,!times_to_midpoints_isna]

summary(as.numeric(mat_sub))
mat_sub = sweep(mat_sub, 2, with_times, "+")
summary(as.numeric(mat_sub))


rsum <- apply(mat_sub, 1, min, na.rm = TRUE)

# TO have conficence in estimate want to pick from multiple options
fun1 <- function(x){
  sum(!is.na(x))
}

rsum_count <- apply(mat_sub, 1, fun1)
rsum[rsum_count <= 10] <- NA
rsum[is.infinite(rsum)] <- NA


summary(times_to_midpoints)
summary(rsum)

qtm(cents[cents$Zone_Code %in% names(rsum)[!is.na(rsum)],])

summary(names(rsum) == names(times_to_midpoints))

combined <- ifelse(times_to_midpoints_isna, rsum, times_to_midpoints)

combined <- data.frame(Zone_Code = names(combined),
                       combined = combined,
                       before = times_to_midpoints,
                       after = rsum)

cents2 <- left_join(cents, combined, by = "Zone_Code")
qtm(cents2, dots.col = "combined") +
  qtm(cents[cents$Zone_Code %in% names(with_times),]) +
  qtm(cents[cents$Zone_Code %in% colnames(mat)[col],], dots.col = "red")





mat_sub <- mat[,with_times]
mat_sub <- mat_sub[rownames(mat_sub) %in% without_times,]
