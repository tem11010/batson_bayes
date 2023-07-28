library(Rcpp)
sourceCpp('mh_sampler.cpp')
sourceCpp('mh_sampler_pp.cpp')

sim_data <- function(d_p,d_d,round_p,round_d,total,num_cog){
  df0 <- data.frame(round = c(1:(round_p+round_d)), 
                    num_cog = NA, 
                    total = NA,
                    cog = NA,
                    party = NA)
  df0$num_cog[1] = num_cog
  df0$total[1] = total
  p_cog <- num_cog*exp(d_p)/(num_cog*exp(d_p)+total-num_cog)
  df0$cog[1] = rbinom(1,1,p_cog)
  df0$party[1] = 'PP'
  df0$num_cog[2] = df0$num_cog[1]-df0$cog[1]
  df0$total[2] = df0$total[1]-1
  p_cog <- df0$num_cog[2]*exp(d_d)/(df0$num_cog[2]*exp(d_d)+df0$total[2]-
                                      df0$num_cog[2])
  df0$cog[2] = rbinom(1,1,p_cog)
  df0$party[2] = 'PD'
  for(i in 2:min(round_p,round_d)){
    df0$num_cog[2*i-1] = df0$num_cog[2*i-2]-df0$cog[2*i-2]
    df0$total[2*i-1] = df0$total[2*i-2]-1
    p_cog <- df0$num_cog[2*i-1]*exp(d_p)/(df0$num_cog[2*i-1]*exp(d_p)+
                                            df0$total[2*i-1]-df0$num_cog[2*i-1])
    df0$cog[2*i-1] = rbinom(1,1,p_cog)
    df0$party[2*i-1] = 'PP'
    
    df0$num_cog[2*i] = df0$num_cog[2*i-1]-df0$cog[2*i-1]
    df0$total[2*i] = df0$total[2*i-1]-1
    p_cog <- df0$num_cog[2*i]*exp(d_d)/(df0$num_cog[2*i]*exp(d_d)+
                                          df0$total[2*i]-df0$num_cog[2*i])
    df0$cog[2*i] = rbinom(1,1,p_cog)
    df0$party[2*i] = 'PD'
  }
  if(round_p>round_d){
    for(i in (2*round_d+1):(round_p+round_d)){
      df0$num_cog[i] = df0$num_cog[i-1]-df0$cog[i-1]
      df0$total[i] = df0$total[i-1]-1
      p_cog <- df0$num_cog[i]*exp(d_p)/(df0$num_cog[i]*exp(d_p)+
                                          df0$total[i]-df0$num_cog[i])
      df0$cog[i] = rbinom(1,1,p_cog)
      df0$party[i] = 'PP'
    }
  }
  if(round_p<round_d){
    for(i in (2*round_p+1):(round_p+round_d)){
      df0$num_cog[i] = df0$num_cog[i-1]-df0$cog[i-1]
      df0$total[i] = df0$total[i-1]-1
      p_cog <- df0$num_cog[i]*exp(d_d)/(df0$num_cog[i]*exp(d_d)+
                                          df0$total[i]-df0$num_cog[i])
      df0$cog[i] = rbinom(1,1,p_cog)
      df0$party[i] = 'PD'
    }
  }
  return(df0)
}

calc_c <- function(L,U,d){
  if (d<0){
    return(sum(L<0 & U<0)/500)
  }
  if(d==0){
    return(sum(L<= 0 & U>=0)/500)
  }
  if(d>0){
    return(sum(L>0 & U>0)/500)
  }
}

#sim_result = expand.grid(d = -3,d_h = c(-3),h = c(1),a = c(0.1))
sim_result = expand.grid(d = -3,d_h = c(-3,-2,-1,0,1,2,3),h = c(1,2,3,4),a = c(0.1,0.3,0.5,0.7,1))
sim_result$d_hat = sim_result$C80 = sim_result$C90 = sim_result$C95 = sim_result$L80 = sim_result$U80 = 
sim_result$L90 = sim_result$U90 = sim_result$L95 = sim_result$U95 =  
sim_result$mean_p = sim_result$mean_h = sim_result$mean_diff = NA

for(j in 1:nrow(sim_result)){
  theta1 <- data.frame(sim = c(1:500),thetahat=NA,LCL90 = NA,UCL90 = NA,LCL95=NA,UCL95=NA,LCL80=NA,UCL80=NA)
    cnt = sim_result$h[j]
    set.seed(3*cnt+10)
    hist_data = sim_data(0,sim_result$d_h[j],6,10,28,9)
    cnt = cnt-1
    while(cnt>0){
      set.seed(3*cnt+10)
      hist_data = rbind(hist_data,sim_data(0,sim_result$d_h[j],6,10,28,9))
      cnt = cnt-1
    }
    for(i in 1:500){
      set.seed(100+i*5)
      df1 <- sim_data(0,sim_result$d_h[j],6,10,28,9)
      pp <- df1[df1$party=='PD',]
      pp = pp[,1:4]
      pp_h <- hist_data[hist_data$party=='PD',]
      pp_h = pp_h[,1:4]
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
    sim_result$L80[j] = mean(theta1$LCL80)
    sim_result$U80[j] =	mean(theta1$UCL80)
    sim_result$L90[j] = mean(theta1$LCL90)
    sim_result$U90[j] = mean(theta1$UCL90)
    sim_result$L95[j] = mean(theta1$LCL95)
    sim_result$U95[j] = mean(theta1$UCL95)
    sim_result$C80[j] = calc_c(theta1$LCL80,theta1$UCL80,sim_result$d[j])
    sim_result$C90[j] = calc_c(theta1$LCL90,theta1$UCL90,sim_result$d[j])
    sim_result$C95[j] = calc_c(theta1$LCL95,theta1$UCL95,sim_result$d[j])
    sim_result$mean_p[j] = mean(theta1$mean_p)
    sim_result$mean_h[j] = mean(theta1$mean_h)
    sim_result$mean_diff[j] = mean(abs(theta1$mean_diff))
}

write.csv(sim_result,'sim_result1.csv')

