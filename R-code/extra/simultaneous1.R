library(Rcpp)
sourceCpp('R/mh_sampler.cpp')
source('R/sim_data.R')
##### b = 1  #####
a1 <- sim_data(1,15,32,10)
n_1 = sum(a1$cog)
a_simultns <- list()
for(i in 1:50){
  a_simultns[[i]] = a1
  a_simultns[[i]]$cog = 0
  idx = sample(1:15,n_1,replace=FALSE)
  a_simultns[[i]]$cog[idx]=1
  for(j in 2:15){
    a_simultns[[i]]$num_cog[j] = a_simultns[[i]]$num_cog[j-1]-a_simultns[[i]]$cog[j-1]
  }
}

out_s = list()
for(i in 1:50){
  out_s[[i]] = make_posterior(x = as.matrix(a_simultns[[i]]),niter = 15000, theta_start_val = 0, 
                              theta_proposal_sd = .5,prior_mean = 0,prior_sd=2)
}
 b_s = sapply(1:50, function(x) mean(out_s[[x]]$theta[5001:15000]))
 hist(b_s)
 ### 95% credible interval 
bs_l = sapply(1:50, function(x) quantile(out_s[[x]]$theta[5001:15000],c(0.025,0.975)))
idx1 = bs_l[1,]<3
idx2 = bs_l[2,]>3
sum(idx1&idx2)/50
b_s1 <- rbind(b_s,bs_l)
save_path_b1 <- here::here("data-sim","b1.csv")
write.csv(b_s1, save_path_b1)

##### b=2 #####
a1 <- sim_data(2,15,32,10)
n_1 = sum(a1$cog)
a_simultns <- list()
for(i in 1:50){
  a_simultns[[i]] = a1
  a_simultns[[i]]$cog = 0
  idx = sample(1:15,n_1,replace=FALSE)
  a_simultns[[i]]$cog[idx]=1
  for(j in 2:15){
    a_simultns[[i]]$num_cog[j] = a_simultns[[i]]$num_cog[j-1]-a_simultns[[i]]$cog[j-1]
  }
}

out_s = list()
for(i in 1:50){
  out_s[[i]] = make_posterior(x = as.matrix(a_simultns[[i]]),niter = 15000, theta_start_val = 0, 
                              theta_proposal_sd = .5,prior_mean = 0,prior_sd=2)
}
b_s = sapply(1:50, function(x) mean(out_s[[x]]$theta[5001:15000]))
hist(b_s)
### 95% credible interval 
bs_l = sapply(1:50, function(x) quantile(out_s[[x]]$theta[5001:15000],c(0.025,0.975)))
idx1 = bs_l[1,]<3
idx2 = bs_l[2,]>3
sum(idx1&idx2)/50
b_s1 <- rbind(b_s,bs_l)
save_path_b2 <- here::here("data-sim","b2.csv")
write.csv(b_s1, save_path_b2)

###### b=3  ######
a1 <- sim_data(3,15,32,10)
n_1 = sum(a1$cog)
a_simultns <- list()
for(i in 1:50){
  a_simultns[[i]] = a1
  a_simultns[[i]]$cog = 0
  idx = sample(1:15,n_1,replace=FALSE)
  a_simultns[[i]]$cog[idx]=1
  for(j in 2:15){
    a_simultns[[i]]$num_cog[j] = a_simultns[[i]]$num_cog[j-1]-a_simultns[[i]]$cog[j-1]
  }
}

out_s = list()
for(i in 1:50){
  out_s[[i]] = make_posterior(x = as.matrix(a_simultns[[i]]),niter = 15000, theta_start_val = 0, 
                              theta_proposal_sd = .5,prior_mean = 0,prior_sd=2)
}
b_s = sapply(1:50, function(x) mean(out_s[[x]]$theta[5001:15000]))
hist(b_s)
### 95% credible interval 
bs_l = sapply(1:50, function(x) quantile(out_s[[x]]$theta[5001:15000],c(0.025,0.975)))
idx1 = bs_l[1,]<3
idx2 = bs_l[2,]>3
sum(idx1&idx2)/50
b_s1 <- rbind(b_s,bs_l)
save_path_b3 <- here::here("data-sim","b3.csv")
write.csv(b_s1,save_path_b3 )
