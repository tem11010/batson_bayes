setwd('/Users/xiaomengli/Documents/consulting/2020spring/bayesian_project/Test0/prototype/')
library(Rcpp)
source("calc_prior.R")
sourceCpp("mh_sampler.cpp")
sourceCpp("mh_sampler_pp.cpp")
library(ggplot2)

#### simulate input table ####
df0 <- data.frame(round = c(1:9), 
                  num_cog = NA, 
                  total = NA,cog=NA)
df0[,'num_cog']=c(3,4,4,3,2,2,2,2,2)
df0[,'total']=rep(9,9)
df0[,'cog']=c(0,1,1,1,1,0,0,0,1)
df0_m <- as.matrix(df0)

### normal prior with calculated posterior mean and sd ###

prior1 = calc_prior('Brian P. Leaming',TRUE,dat0,'race')
post1 <- make_posterior(x = df0_m, niter = 11000, 
                        theta_start_val = 0, theta_proposal_sd =.5, 
                        prior_mean = prior1$prior_mean, prior_sd = prior1$prior_sd)

### use default vague prior ###
post4 <- make_posterior(x = df0_m, niter = 11000, 
                        theta_start_val = 0, theta_proposal_sd =.5, 
                        prior_mean = 0, prior_sd = 2)


### power prior ####
subset <- function(atny_name,pp,dat,cog){
  dat_sub <- extract_atny(atny_name,pp,dat)
  sub1 <- dat_sub %>% group_split(ID)
  sub1_l <- lapply(1:length(sub1), function(x) organize_input(sub1[[x]],pp,cog))
  df_m = do.call(rbind.data.frame,sub1_l)
  df_m <- as.matrix(df_m)
}

### with a0=1 ###
sub1 <- subset('Brian P. Leaming',TRUE,dat0,'race')
post2 <- make_posterior_p(x = df0_m,x_p=sub1,a0=1, niter = 11000, 
                        theta_start_val = 0, theta_proposal_sd =.5, 
                        prior_mean = 0, prior_sd = 2)

### with a0=0 which should be the same with the vague normal prior ###
post3 <- make_posterior_p(x = df0_m,x_p=sub1,a0=0, niter = 11000, 
                          theta_start_val = 0, theta_proposal_sd =.5, 
                          prior_mean = 0, prior_sd = 2)


### create density plot to compare posterior density with different priors ###
d1 <- density(post1$theta[1001:11000])
d2 <- density(post2$theta[1001:11000])
d3 <- density(post3$theta[1001:11000])
d4 <- density(post4$theta[1001:11000])
plot(d1,ylim=c(0,max(c(d1$y,d2$y))))
lines(d2,col='red')
lines(d3,col='green')
lines(d4,col='blue')

