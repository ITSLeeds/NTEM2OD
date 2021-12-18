# Ingest and summarise Ian's work
library(tidyr)
library(dplyr)


dat = read.csv("../../SDCA-tool/sdca-NTS/data/output/results_NTS_curves_funcs_NTEM_mode_purp_lon_not_Rural_urb.csv")
#dat <- dat[!is.na(dat$rural_urb),]

perms = crossing(mode = unique(dat$mode),
                 purpose = unique(dat$purpose),
                 region = unique(dat$region),
                 rural_urb = unique(dat$rural_urb))
nobs <- dat %>%
  group_by(mode, purpose, region, rural_urb) %>%
  summarise(n_obs = n())

perms = left_join(perms, nobs, by = c("mode", "purpose", "region", "rural_urb"))

perms <- perms[order(perms$mode, perms$purpose,perms$rural_urb, perms$region),]


# Limit to NTEM modes
allowed_modes = c("Walk","Bicycle","Car / van driver","Car / van passenger","rail","bus")
perms <- perms[perms$mode %in% allowed_modes,]

# Limit to NTEM journey purpose
# allowed_purpose = c("Business","Commuting","Education / escort education",
#                     "Leisure","Personal business","Shopping")
# perms <- perms[perms$purpose %in% allowed_purpose,]


# Function for a curve
prob_curve <- function(x, a = 0.06756, b = 5.29572, c = 5){
  c * a * b * exp(-a * x) * ((1/(exp(-a * x) + 1))^(b+1))
}

cum_curve <- function(x,a,b){
   (1/(1+exp(-a*x)))^b
}


res <- list()
for(i in 1:nrow(perms)){
  per_sub <- perms[i, ]
  dat_sub <- dat[dat$mode == per_sub$mode,]
  dat_sub <- dat_sub[dat_sub$purpose == per_sub$purpose,]
  dat_sub <- dat_sub[dat_sub$region  == per_sub$region,]
  dat_sub <- dat_sub[dat_sub$rural_urb  == per_sub$rural_urb,]
  
  if(nrow(dat_sub) > 0){
    if(any(duplicated(dat_sub$bin5mins))){
      message(i," Duplicated time bins for: ",paste(unlist(per_sub), collapse = " "))
      dat_sub <- dat_sub[!duplicated(dat_sub$bin5mins),]
    }
    if(length(unique(dat_sub$bin_proportion)) < 3){
      message(i," Insuffcient data for: ",paste(unlist(per_sub), collapse = " "))
      per_sub$a = NA
      per_sub$b = NA
      per_sub$c = NA
      per_sub$error_main <- NA
      per_sub$error_backup <- NA
      
    } else {
      dat_sub$time <- dat_sub$bin5mins * 5 - 2.5
      
      plot(dat_sub$time, dat_sub$bin_proportion)
      lines(dat_sub$time, prob_curve(dat_sub$time))
      lines(dat_sub$time, dat_sub$pred_bin_propn)
      plot(dat_sub$time, dat_sub$cumulative_proportion)
      
      
      nlc <- nls.control(maxiter = 1000, warnOnly = FALSE)
      model_prop <- try(nls(bin_proportion ~ prob_curve(time,a,b,c), 
                        data = dat_sub,
                        control = nlc,
                        start=list(a=0.06756,b=5.29572,c = 5)), silent = TRUE)
      # First fail try some different start values
      if("try-error" %in% class(model_prop)){
        model_prop <- try(nls(bin_proportion ~ prob_curve(time,a,b,c), 
                              data = dat_sub,
                              control = nlc,
                              start=list(a=0.02603,b=9.79523,c = 4.56921)), silent = TRUE)
      }
      
      model_cum <- try(nls(pred_bin_propn ~ prob_curve(time,a,b,c),
                       data = dat_sub,
                       control = nlc,
                       start=list(a=0.06756,b=5.29572,c = 5)), silent = TRUE)
      
      if("try-error" %in% class(model_prop)){
        message("failed to build prop model for ",paste(unlist(per_sub), collapse = " "))
        err_prop = 99999999999
      } else {
        err_prop <- sum(abs(dat_sub$bin_proportion - prob_curve(dat_sub$time, 
                                                                coefficients(model_prop)["a"],
                                                                coefficients(model_prop)["b"],
                                                                coefficients(model_prop)["c"])))
      }
      if("try-error" %in% class(model_cum)){
        message("failed to build cum model for ",paste(unlist(per_sub), collapse = " "))
        err_cum = 99999999999
      } else {
        err_cum <- sum(abs(dat_sub$bin_proportion - prob_curve(dat_sub$time, 
                                                               coefficients(model_cum)["a"],
                                                               coefficients(model_cum)["b"],
                                                               coefficients(model_cum)["c"])))
      }
      
      if(err_prop == 99999999999 & err_cum == 99999999999){
        message("Bad data for: ",paste(unlist(per_sub), collapse = " "))
        per_sub$a = NA
        per_sub$b = NA
        per_sub$c = NA
        per_sub$error_main <- NA
        per_sub$error_backup <- NA
        
      } else {
        if(err_prop < err_cum){
          #message("Using main model for: ",paste(unlist(per_sub), collapse = ", ")," Error is ",round(err_prop * 100,4),"%")
          model <- model_prop
        } else {
          #message("Using backup model for: ",paste(unlist(per_sub), collapse = ", ")," Error is ",round(err_cum * 100,4),"%")
          model <- model_cum
        }
        
        # if(err > 0.1){
        #   message("Model error for: ",paste(unlist(per_sub), collapse = ", ")," is ",round(sum(residuals(model)) * 100, 3),"%")
        # }
        
        per_sub$a = coefficients(model)["a"]
        per_sub$b = coefficients(model)["b"]
        per_sub$c = coefficients(model)["c"]
        per_sub$error_main <- round(err_prop * 100, 5)
        per_sub$error_backup <- round(err_cum * 100, 5)
      }
      
      
      
      
    }
    
    
    
    
    
    
  } else {
    message("No data for: ",paste(unlist(per_sub), collapse = ", "))
    
    per_sub$a = NA
    per_sub$b = NA
    per_sub$c = NA
    per_sub$error_main <- NA
    per_sub$error_backup <- NA
    
  }
  
  res[[i]] <- per_sub
  rm(model_cum, model_prop, model, per_sub, dat_sub)
  
}
res <- bind_rows(res)

# Fill in London Rural with London Urban when missing

res_missing <- res[is.na(res$a),]
res <- res[!is.na(res$a),]

for(i in 1:nrow(res_missing)){
  a <- res$a[res$mode == res_missing$mode[i] &
               res$purpose == res_missing$purpose[i] &
               res$region == "London" &
               res$rural_urb == "Urban"]
  b <- res$b[res$mode == res_missing$mode[i] &
               res$purpose == res_missing$purpose[i] &
               res$region == "London" &
               res$rural_urb == "Urban"]
  c <- res$c[res$mode == res_missing$mode[i] &
               res$purpose == res_missing$purpose[i] &
               res$region == "London" &
               res$rural_urb == "Urban"]
  if(length(a) == 1){
    res_missing$a[i] <- a
    res_missing$b[i] <- b
    res_missing$c[i] <- c
  } else {
    message("No london alternative for ",i)
  }
  
  
}
res <- rbind(res, res_missing)


# res_missing <- res[is.na(res$a),]
# res <- res[!is.na(res$a),]
# 
# # Fill in any other missing with same mode
# 
# for(i in 1:nrow(res_missing)){
#   alt_values <- res[res$mode == res_missing$mode[i] &
#                       res$purpose == res_missing$purpose[i],]
#   
#   if(nrow(alt_values) == 0){
#     alt_values <- res[res$mode == res_missing$mode[i],]
#   }
#   
#   alt_values <- alt_values[!is.na(alt_values$error_main),]
#   alt_values <- alt_values[alt_values$error_main == min(alt_values$error_main, na.rm = TRUE),]
#   alt_values <- alt_values[1,]
#   
#   a <- alt_values$a
#   b <- alt_values$b
#   c <- alt_values$c
#   
#   if(length(a) == 1){
#     res_missing$a[i] <- a
#     res_missing$b[i] <- b
#     res_missing$c[i] <- c
#   } else {
#     message("No alternative for ",i)
#   }
#   
#   
# }
# 
# res <- rbind(res, res_missing)

saveRDS(res,"data/NTS/decay_curves_v2.Rds")

# plot all the curves
plot(1:300, prob_curve(1:300, res$a[1], res$b[1], res$c[1]), type = "l")

for(i in 2:nrow(res)){
  mode = res$mode[i]
  if(mode == "Bicycle"){
    col = "green"
  } else if(mode == "bus"){
    col = "blue"
  } else if(mode == "Car / van driver"){
    col = "red"
  } else if(mode == "Car / van passenger"){
    col = "orange"
  } else if(mode == "rail"){
    col = "purple"
  } else if(mode == "Walk"){
    col = "grey"
  }
  
  lines(1:300, prob_curve(1:300, res$a[i], res$b[i], res$c[i]), col = col)
}






# Plot some examples
# Low Error
mode = "Car / van passenger"
purpose = "Home Based Education"
region = "Not London"
rural_urb = "Urban"


dat_sub = dat[dat$mode == mode &
                dat$purpose == purpose &
                dat$region == region &
                dat$rural_urb == rural_urb
                ,]
dat_sub$time <- dat_sub$bin5mins * 5 - 2.5
res_sub <- res[res$mode == mode &
                 res$purpose == purpose &
                 res$region == region &
                 res$rural_urb == rural_urb,]
plot(dat_sub$time, dat_sub$bin_proportion)
  lines(dat_sub$time, prob_curve(dat_sub$time, 
                                 a = res_sub$a,
                                 b = res_sub$b,
                                 c = res_sub$c))

# high error
mode = "Walk"
purpose = "Not Home Based Recreation/Social"
region = "London"
rural_urb = "Rural"


dat_sub = dat[dat$mode == mode &
              dat$purpose == purpose &
              dat$region == region &
              dat$rural_urb == rural_urb
            ,]
dat_sub$time <- dat_sub$bin5mins * 5 - 2.5
res_sub <- res[res$mode == mode &
               res$purpose == purpose &
               res$region == region &
               res$rural_urb == rural_urb,]
plot(dat_sub$time, dat_sub$bin_proportion)
lines(dat_sub$time, prob_curve(dat_sub$time, 
                             a = res_sub$a,
                             b = res_sub$b,
                             c = res_sub$c))


# med error
mode = "rail"
purpose = "Home Based Shopping"
region = "Not London"
rural_urb = "Urban"


dat_sub = dat[dat$mode == mode &
                dat$purpose == purpose &
                dat$region == region &
                dat$rural_urb == rural_urb
              ,]
dat_sub$time <- dat_sub$bin5mins * 5 - 2.5
res_sub <- res[res$mode == mode &
                 res$purpose == purpose &
                 res$region == region &
                 res$rural_urb == rural_urb,]
plot(dat_sub$time, dat_sub$bin_proportion)
lines(dat_sub$time, prob_curve(dat_sub$time, 
                               a = res_sub$a,
                               b = res_sub$b,
                               c = res_sub$c))
  
  
# missing
mode = "rail"
purpose = "Home Based Personal Business"
region = "London"
rural_urb = "Urban"


dat_sub = dat[dat$mode == mode &
                dat$purpose == purpose &
                dat$region == region &
                dat$rural_urb == rural_urb
              ,]
dat_sub$time <- dat_sub$bin5mins * 5 - 2.5
res_sub <- res[res$mode == mode &
                 res$purpose == purpose &
                 res$region == region &
                 res$rural_urb == rural_urb,]
plot(dat_sub$time, dat_sub$bin_proportion)
lines(dat_sub$time, prob_curve(dat_sub$time, 
                               a = res_sub$a,
                               b = res_sub$b,
                               c = res_sub$c))

















plot(1:100, prob_curve(1:100, res$a[1], res$b[1], res$c[1]), type = "l", ylim = c(0,0.3))
lines(1:100, prob_curve(1:100, res$a[2], res$b[2], res$c[2]), col = "red")
lines(1:100, prob_curve(1:100, res$a[3], res$b[3], res$c[3]), col = "orange")
lines(1:100, prob_curve(1:100, res$a[4], res$b[4], res$c[4]), col = "yellow")
lines(1:100, prob_curve(1:100, res$a[5], res$b[5], res$c[5]), col = "green")
lines(1:100, prob_curve(1:100, res$a[6], res$b[6], res$c[6]), col = "blue")
lines(1:100, prob_curve(1:100, res$a[7], res$b[7], res$c[7]), col = "pink")
lines(1:100, prob_curve(1:100, res$a[8], res$b[8], res$c[8]), col = "violet")



plot(dat_sub$time, dat_sub$pred_bin_propn)
lines(dat_sub$time, prob_curve(dat_sub$time))

plot(dat_sub$time, dat_sub$cumulative_proportion)
lines(dat_sub$time, dat_sub$pred_cumulative_propn)
lines(dat_sub$time, cum_curve(dat_sub$time, 
                                      coefficients(model_cum)["a"],
                                      coefficients(model_cum)["b"]),
      col = "red")

plot(dat_sub$time, dat_sub$bin_proportion)
lines(dat_sub$time, prob_curve(dat_sub$time, 
                               coefficients(model_prop)["a"],
                               coefficients(model_prop)["b"],
                               coefficients(model_prop)["c"]))
lines(dat_sub$time, prob_curve(dat_sub$time, 
                               coefficients(model_cum)["a"],
                               coefficients(model_cum)["b"],
                               coefficients(model_cum)["c"]),
      col = "red")
#lines(dat_sub$time, dat_sub$pred_bin_propn, col = "blue")
