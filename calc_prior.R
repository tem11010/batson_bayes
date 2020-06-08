### assume dat0 is all the historical data we have

library(dplyr)

dat0 <- readRDS("jury_data_cleaned.RDS")

extract_seq <- function(dat){
  
  dat$strike_num1 <- as.numeric(dat$strike_num)
  skipped <- as.numeric(union(setdiff(unique(dat$strike_num),seq(min(as.numeric(dat$strike_num)),max(as.numeric(dat$strike_num)))),setdiff(seq(min(as.numeric(dat$strike_num)),max(as.numeric(dat$strike_num))),unique(dat$strike_num))))
  if(length(skipped)>0){
    for(i in 1:length(skipped)){
      dat$strike_num1[which(dat$strike_num1>skipped[i])] = dat$strike_num1[which(dat$strike_num1>skipped[i])]-1
    }
  }
  ### find number of strikes by both PP and P
  
  n0 = length(dat$strike_num1)-sum(dat$strike_num1==0)-length(unique(dat$strike_num1))+1
  idx = intersect(which(dat$strike_num1<=n0),which(dat$strike_num1>0))
  
  ### find the second index of duplicated strike numbers
  second_index = intersect(which(duplicated(dat$strike_num1)),which(dat$strike_num1>0))
  
  dat$strike_seq[idx] = as.numeric(dat$strike_num1[idx])*2-1
  dat$strike_seq[second_index] = as.numeric(dat$strike_num1[second_index])*2
  dat$strike_seq[which(as.numeric(dat$strike_num1)>n0)] = as.numeric(dat$strike_num1[which(as.numeric(dat$strike_num1)>n0)])+n0
  return(dat)
}




extract_atny <- function(atny_name,pp,dat){
  if(pp){
    idx = sapply(1:nrow(dat),function(x) grepl(atny_name, dat$P_atty_l[x], fixed=TRUE))
    return(dat[which(idx),])
  }
  else{
    idx = sapply(1:nrow(dat),function(x) grepl(atny_name, dat$D_atty_l[x], fixed=TRUE))
    return(dat[which(idx),])
  }
} 

# extract_atny <- function(pp_name,pd_name,dat){
#   idx1 = sapply(1:nrow(dat),function(x) grepl(pp_name, dat$P_atty_l[x], fixed=TRUE))
#   idx2 = sapply(1:nrow(dat),function(x) grepl(pd_name, dat$D_atty_l[x], fixed=TRUE))
#   idx = sapply(1:nrow(dat),function(x) any(idx1[x],idx2[x]))
#   return(dat[which(idx),])
# }


##### test  ####
# dat1 = dat0 %>% group_by(ID)
# extract_atny('Brian P. Leaming',dat1) -> dat2


### if attorney name is from PP, set pp as TRUE
### if attorney name is from PD, set pp as FALSE

calc_prior <- function(atny_name,pp,dat){
  dat_sub <- extract_atny(atny_name,pp,dat)
  trials <- unique(dat_sub$ID)
  
  dat_sub0 <- dat_sub[which(dat_sub$ID==trials[1]),]
  dat_sub0 <- extract_seq(dat_sub0)
  dat_strikes <- dat_sub0 %>% filter(!is.na(strike_seq))
  dat_strikes <- dat_strikes[order(dat_strikes$strike_seq),]
  dat_strikes$sexb <- ifelse(dat_strikes$sex=="F", 1, 0)
  num_female <- sum(dat_strikes$sexb)
  num_t <- nrow(dat_strikes)
  subs <- data.frame(round = c(1:num_t), 
                          num_female = NA, 
                          total = NA)
  for ( j in 2:num_t){
    num_female = num_female-dat_strikes$sexb[j-1]
    num_total = num_t - dat_strikes$strike_seq[j-1]
    subs$num_female[j] <- num_female
    # number of females struck in this and previous rounds
    subs$total[j] <- num_total
  }
  subs$num_female[1] <- num_female
  subs$total[1] <- num_t
  subs <- cbind(subs,dat_strikes$sexb)
  colnames(subs)[4] = 'cognizable'
  
  if(length(trials)>1){
    for(i in trials){
      dat_sub0 <- dat_sub[which(dat_sub$ID==i),]
      dat_sub0 <- extract_seq(dat_sub0)
      dat_strikes <- dat_sub0 %>% filter(!is.na(strike_seq))
      dat_strikes$sexb <- ifelse(dat_strikes$sex=="F", 1, 0)
      dat_strikes <- dat_strikes[order(dat_strikes$strike_seq),]
      num_female <- sum(dat_strikes$sexb)
      num_t <- nrow(dat_strikes)
      subsnew <- data.frame(round = c(1:num_t),
                              num_female = NA,
                              total = NA)
      for ( j in 2:num_t){
        num_female = num_female-dat_strikes$sexb[j-1]
        num_total = num_t - dat_strikes$strike_seq[j-1]
        subsnew$num_female[j] <- num_female
        # number of females struck in this and previous rounds
        subsnew$total[j] <- num_total
      }
      subsnew$num_female[1] <- num_female
      subsnew$total[1] <- num_t
      subsnew <- cbind(subsnew,dat_strikes$sexb)
      colnames(subsnew)[4] = 'cognizable'
      subs <- rbind(subs,subsnew)
    }
  }
  
  if(pp){
    df_m <- as.matrix(subs[which(dat_strikes$Disp=='PP'),])
  }else{
    df_m <- as.matrix(subs[which(dat_strikes$Disp=='PD'),])
  }
  
  
  out <- make_posterior(x = df_m, niter = 110000, theta_start_val = 0,theta_proposal_sd =.5,prior_mean = 0,prior_sd=2)
  d1 <- out$theta[1001:11000]
  return(list(prior_mean = mean(d1),prior_sd = sd(d1)))
}

##### test #####
test_prior = calc_prior('Brian P. Leaming',TRUE,dat0)

