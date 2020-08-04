### assume dat0 is all the historical data we have

library(dplyr)
library(mcmc)
library(Rcpp)
Rcpp::sourceCpp(here::here("prototype","mh_sampler.cpp"))

dat0 <- readRDS("jury_data_cleaned_new.RDS")


#### extract_atny() function extracts the subset of jury_data_cleaned that corresponds to 
#### the attorney name inputted in 'atny_name' parameter. 
#### if attorney name is from PP, set 'pp' parameter as TRUE
#### if attorney name is from PD, set 'pp' parameter as FALSE
#### parameter 'pp' is the same for the following functions, used to control whether the case is from PP or PD

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

#### function organize_input will return a matrix with four columns:
#### round, num of cog_class, total number, and whether the strike is from cognizable class
#### the return matrix is of the form of the input of make_posterior function
#### input of cog should be either 'gender' or 'race'

organize_input <- function(dat,pp,cog){
  dat_strikes <- dat %>% filter(!is.na(strike_seq)) 
  if(cog=='gender'){
    dat_strikes <- dat_strikes %>% filter(!is.na(sex))
  }else{
    dat_strikes <- dat_strikes %>% filter(!is.na(race))
  }
  
  dat_strikes <- dat_strikes[order(dat_strikes$strike_seq),]
  if(cog=='gender'){
    dat_strikes$cogb <- ifelse(dat_strikes$sex=="F", 1, 0)
  }else{
    dat_strikes$cogb <- ifelse(dat_strikes$race !=3, 1, 0)
  }
  
  num_cog <- sum(dat_strikes$cogb)
  num_t <- nrow(dat_strikes)
  subs <- data.frame(round = c(1:num_t), 
                     num_cog = NA, 
                     total = NA)
  subs$num_cog[1] <- num_cog
  subs$total[1] <- num_t
  for ( j in 2:num_t){
    num_cog = num_cog-dat_strikes$cogb[j-1]
    num_total = num_t - dat_strikes$strike_seq[j-1]
    subs$num_cog[j] <- num_cog
    # number of females struck in this and previous rounds
    subs$total[j] <- num_total
  }
  subs <- cbind(subs,dat_strikes$cogb)
  colnames(subs)[4] = 'cognizable'
  if(pp){
    df_m <- as.matrix(subs[which(dat_strikes$Disp=='PP'),])
  }else{
    df_m <- as.matrix(subs[which(dat_strikes$Disp=='PD'),])
  }
  return(df_m)
}

### function calc_prior will return prior mean and sd using the subset data containing the attorney name
### to get the prior mean and sd corresponding to a prosecutor attorney, we need to input 
### attorney name of the prosecutor to parameter 'atny_name', and TRUE for 'pp' and jury_data_cleaned_new (which is the one with strike_seq) for 'dat'
### to get the prior mean and sd corresponding to a defense attorney, we put the according name and set 'pp' as FALSE.

subset <- function(atny_name,pp,dat,cog){
  dat_sub <- extract_atny(atny_name,pp,dat)
  sub1 <- dat_sub %>% group_split(ID)
  sub1_l <- lapply(1:length(sub1), function(x) organize_input(sub1[[x]],pp,cog))
  df_m = do.call(rbind.data.frame,sub1_l)
  df_m <- as.matrix(df_m)
}

calc_prior <- function(atny_name,pp,dat,cog){
  df_m <- subset(atny_name,pp,dat,cog)
  out <- make_posterior(x = df_m, niter = 110000, theta_start_val = 0,theta_proposal_sd =.5,prior_mean = 0,prior_sd=2)
  d1 <- out$theta[1001:11000]
  return(list(prior_mean = mean(d1),prior_sd = sd(d1)))
}

##### test whether the function works #####
#test_prior = calc_prior('Brian P. Leaming',TRUE,dat0)

