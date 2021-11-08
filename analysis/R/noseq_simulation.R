library(Rcpp)
library(ggplot2)
library(dplyr)
sourceCpp('R/mh_sampler.cpp')
sourceCpp('R/mh_sampler_pp.cpp')
source('R/sim_data.R')

### simulation repeated 1000 times ###
### round = 30
### total = 64
### number of cognizable class = 20
sim_result1 = expand.grid(d = -3,d_h = c(-3,-2,-1,0,1,2,3),h = c(1,2,3),a = c(0.1,0.3,0.5,0.7,1))
sim_result2 = expand.grid(d = -2,d_h = c(-3,-2,-1,0,1,2,3),h = c(1,2,3),a = c(0.1,0.3,0.5,0.7,1))
sim_result3 = expand.grid(d = -1,d_h = c(-3,-2,-1,0,1,2,3),h = c(1,2,3),a = c(0.1,0.3,0.5,0.7,1))
sim_result4 = expand.grid(d = 0,d_h = c(-3,-2,-1,0,1,2,3),h = c(1,2,3),a = c(0.1,0.3,0.5,0.7,1))
sim_result5 = expand.grid(d = 1,d_h = c(-3,-2,-1,0,1,2,3),h = c(1,2,3),a = c(0.1,0.3,0.5,0.7,1))
sim_result6 = expand.grid(d = 2,d_h = c(-3,-2,-1,0,1,2,3),h = c(1,2,3),a = c(0.1,0.3,0.5,0.7,1))
sim_result7 = expand.grid(d = 3,d_h = c(-3,-2,-1,0,1,2,3),h = c(1,2,3),a = c(0.1,0.3,0.5,0.7,1))
sim_result8 = expand.grid(d = c(-3,-2,-1,0,1,2,3))

### modify the sim_result to sim_result1/2/.../8 
### running time is long, so submit 8 jobs to cluster to run it
sim_result$d_hat = sim_result$C80 = sim_result$C90 = sim_result$C95 = 
  sim_result$mean_p = sim_result$mean_h = sim_result$mean_diff = NA

for(j in 1:nrow(sim_result)){
  theta1 <- data.frame(sim = c(1:1000),thetahat=NA,LCL90 = NA,UCL90 = NA,
                       LCL95=NA,UCL95=NA,LCL80=NA,UCL80=NA,mean_p = NA,mean_h = NA,mean_diff = NA)
  cnt = sim_result$h[j]
  set.seed(3*cnt+10)
  hist_data = sim_data(sim_result$d_h[j],30,64,20)
  cnt = cnt-1
  while(cnt>0){
    set.seed(3*cnt+10)
    hist_data = rbind(hist_data,sim_data(sim_result$d_h[j],30,64,20))
    cnt = cnt-1
  }
  for(i in 1:1000){
    set.seed(100+i*5)
    df1 <- sim_data(sim_result$d[j],30,64,20)
    row_odd <- seq_len(nrow(df1)) %% 2 
    pp <- df1[row_odd==1,]
    row_odd_h <- seq_len(nrow(hist_data)) %% 2 
    pp_h <- hist_data[row_odd_h==1,]
    outp <- make_posterior(x = as.matrix(pp),niter = 15000, theta_start_val = 0, 
                           theta_proposal_sd = .5,prior_mean = 0,prior_sd=2)
    outh <- make_posterior(x = as.matrix(pp_h),niter = 15000, theta_start_val = 0, 
                           theta_proposal_sd = .5,prior_mean = 0,prior_sd=2)
    out1 <- make_posterior_p(x = as.matrix(pp),x_p = as.matrix(pp_h),a0=sim_result$a[j],niter = 15000, theta_start_val = 0, 
                             theta_proposal_sd = .5,prior_mean = 0,prior_sd=2)
    theta = out1$theta[5001:15000]
    theta1$thetahat[i] = mean(theta)
    theta1$LCL90[i] = quantile(theta,0.05) 
    theta1$LCL95[i] = quantile(theta,0.025) 
    theta1$UCL90[i] = quantile(theta,0.95) 
    theta1$UCL95[i] = quantile(theta,0.975) 
    theta1$LCL80[i] = quantile(theta,0.1) 
    theta1$UCL80[i] = quantile(theta,0.9) 
    thetap <- outp$theta[5001:15000]
    thetah <- outh$theta[5001:15000]
    theta1$mean_p[i] = mean(thetap)
    theta1$mean_h[i] = mean(thetah)
    theta1$mean_diff[i] = theta1$mean_p[i]-theta1$mean_h[i]
    
  }
  sim_result$d_hat[j] = mean(theta1$thetahat)
  sim_result$C80[j] = calc_c(theta1$LCL80,theta1$UCL80,sim_result$d[j])
  sim_result$C90[j] = calc_c(theta1$LCL90,theta1$UCL90,sim_result$d[j])
  sim_result$C95[j] = calc_c(theta1$LCL95,theta1$UCL95,sim_result$d[j])
  sim_result$mean_p[j] = mean(theta1$mean_p)
  sim_result$mean_h[j] = mean(theta1$mean_h)
  sim_result$mean_diff[j] = mean(abs(theta1$mean_diff))
}

#### with no historical data ####
sim_result$d_hat = sim_result$C80 = sim_result$C90 = sim_result$C95 = NA
for(j in 1:nrow(sim_result)){
  theta1 <- data.frame(sim = c(1:500),thetahat=NA,LCL90 = NA,UCL90 = NA,LCL95=NA,UCL95=NA,LCL80=NA,UCL80=NA)
  for(i in 1:500){
    set.seed(100+i*5)
    df1 <- sim_data(sim_result$d[j],30,64,20)
    row_odd <- seq_len(nrow(df1)) %% 2 
    pp <- df1[row_odd==1,]
    out1 <- make_posterior(x = as.matrix(pp),niter = 15000, theta_start_val = 0, 
                           theta_proposal_sd = .5,prior_mean = 0,prior_sd=2)
    theta = out1$theta[5001:15000]
    theta1$thetahat[i] = mean(theta)
    theta1$LCL90[i] = quantile(theta,0.05) 
    theta1$LCL95[i] = quantile(theta,0.025) 
    theta1$UCL90[i] = quantile(theta,0.95) 
    theta1$UCL95[i] = quantile(theta,0.975) 
    theta1$LCL80[i] = quantile(theta,0.1) 
    theta1$UCL80[i] = quantile(theta,0.9) 
  }
  sim_result$d_hat[j] = mean(theta1$thetahat)
  sim_result$C80[j] = calc_c(theta1$LCL80,theta1$UCL80,sim_result$d[j])
  sim_result$C90[j] = calc_c(theta1$LCL90,theta1$UCL90,sim_result$d[j])
  sim_result$C95[j] = calc_c(theta1$LCL95,theta1$UCL95,sim_result$d[j])
}
