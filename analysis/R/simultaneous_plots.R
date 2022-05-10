##Purpose: Code to simulate application of simultaneous strikes 

#Libraries to use
library(rstan)
library(dplyr)

# Each row in the resulting data set is a strike
###Function: sim_data######
#d_p: bias in prosecution
#d_d: bias in defense
#round_p: the number round of strikes for prosecution
#round_d: the number round fo strikes for defense
#total: total number of jurors 
#num_cog: number of cognizeable people


sim_data <- function(d_p,d_d,round_p,round_d,total,num_cog){
  #df0: empty data frame
  df0 <- data.frame(round = c(1:(round_p+round_d)),
                    num_cog = NA,
                    total = NA,
                    cog = NA,
                    party = NA)
  df0$num_cog[1] = num_cog
  df0$total[1] = total
  #p_cog=probablity strike a cognizeable class member
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



#dat1=example for trial run 
#dat1=sim_data(d_p=.3,d_d=0,round_p=5,round_d=5,total=10,num_cog=5)

## Function: get_stan_dat#################################################
#d_p: bias in prosecution
#d_d: bias in defense
#round_p: the number round of strikes for prosecution
#round_d: the number round fo strikes for defense
#total: total number of jurors 
#num_cog: number of cognizeable people
#trial_name
############################################################################

get_stan_dat<-function(d_p,d_d,round_p,round_d,total,num_cog,trial_name)
{
  dat1=sim_data(d_p,d_d,round_p,round_d,total,num_cog)
  dat1$trial=trial_name
  dat1$m=dat1$total-dat1$num_cog
  # pp_dat = dat1 %>% dplyr::filter(party=="PP") to permute both and then subset
  pp_dat=dat1
  return(pp_dat)
}

#current trial 
#c_dat=get_stan_dat(d_p=.3,d_d=0,round_p=5,round_d=5,total=10,num_cog=5,trial_name=1)


## Function: get_historical#################################################
# NOTE: Pooling historical trials 
#n_t=number of historical trials
#d_p: bias in prosecution (Vector of size n_t)
#d_d: bias in defense (Vector of size n_t)
#round_p: the number round of strikes for prosecution (Vector of size n_t)
#round_d: the number round fo strikes for defense (Vector of size n_t)
#total: total number of jurors initial  (Vector of size n_t)
#num_cog: number of cognizable people initial  (Vector of size n_t)
############################################################################

#trial name=1 for current trial 
#trial name =2...n_t+1 for historical trials 
get_historical<-function(n_t,d_p,d_d,round_p,round_d,total,num_cog)
{
  #inital dat to then rbind the other historical trials
  dat=get_stan_dat(d_p=d_p[1],d_d=d_d[1],round_p=round_p[1],round_d=round_d[1]
                   ,total=total[1],num_cog=num_cog[1],trial_name = 2)
  for(i in 2:n_t)
  {
    temp_dat=get_stan_dat(d_p=d_p[i],d_d=d_d[i],round_p=round_p[i],round_d=round_d[i]
                          ,total=total[i],num_cog=num_cog[i],trial_name=i+1)
    dat=rbind(dat,temp_dat)
  }
  return(dat)
  
}
#h_dat=historical trials: 
# h_dat=get_historical(n_t=3,d_p=c(.3,.3,.3),d_d=c(.3,.3,.3),round_p=c(rep(10,3)),
#                    round_d=c(rep(10,3)),total=rep(20,3),num_cog=rep(5,3))


data_create<-function(reps,bias_p_c,bias_d_c,strikes_p_c, strikes_d_c, total_strikes_c,num_cog_c,
                      n_t,bias_p_h,bias_d_h,strikes_p_h, strikes_d_h, total_strikes_h,num_cog_h)
{
  #list of current data sets
  c_dats<- vector(mode = "list", length = reps)
  #list of current data sets
  h_dats<- vector(mode = "list", length = reps)
  for(l in 1:reps)
  {
    c_dats[[l]] = get_stan_dat(d_p=bias_p_c,d_d=bias_d_c,round_p=strikes_p_c,
                               round_d=strikes_d_c,total=total_strikes_c,num_cog=num_cog_c,trial_name=1)
    if(n_t>1)
    {
      h_dats[[l]]=get_historical(n_t,d_p=c(rep(bias_p_h,n_t)),d_d=c(rep(bias_d_h,n_t)),
                                 round_p=c(rep(strikes_p_h,n_t)),round_d=c(rep(strikes_d_h,n_t)),
                                 total=c(rep(total_strikes_h,n_t)),num_cog=c(rep(num_cog_h,n_t)))
    }
    else
    {
      h_dats[[l]] = get_stan_dat(d_p=bias_p_h,d_d=bias_d_h,round_p=strikes_p_h,
                                 round_d=strikes_d_h,total=total_strikes_h,num_cog=num_cog_h,trial_name=2)
    }
    
  }
  dats=list("c_dats"=c_dats,"h_dats"=h_dats)
  return(dats)
}


# Create Data to Permutate -------------------------------------------------------------------------------------------


#Create one replicated of current data and historical data for 3 scenarios 

#d1: bias in historical trial and current trial is 1 
d1=data_create(reps=1,bias_p_c=1,bias_d_c=0,strikes_p_c=15, strikes_d_c=15, 
               total_strikes_c=30,num_cog_c=15,
               n_t=1,bias_p_h=1,bias_d_h=0,strikes_p_h=15, 
               strikes_d_h=15, total_strikes_h=30,num_cog_h=15)

#pp<- d1[[1]] %>% filter(party=="PP")


#d2: bias in historical trial and current trial is 2
d2=data_create(reps=1,bias_p_c=2,bias_d_c=0,strikes_p_c=15, strikes_d_c=15, 
               total_strikes_c=30,num_cog_c=15,
               n_t=1,bias_p_h=2,bias_d_h=0,strikes_p_h=15, 
               strikes_d_h=15, total_strikes_h=30,num_cog_h=15)


#d3: bias in historical trial and current trial is 3
d3=data_create(reps=1,bias_p_c=3,bias_d_c=0,strikes_p_c=15, strikes_d_c=15, 
               total_strikes_c=30,num_cog_c=15,
               n_t=1,bias_p_h=3,bias_d_h=0,strikes_p_h=15, 
               strikes_d_h=15, total_strikes_h=30,num_cog_h=15)


# Create Permuted Datasets -------------------------------------------------------------------------------------------

#Only permuted the strike data of party=PP (Prosecution - should not matter in terms of simulation)

## Function: get_perm_dat      
##dat: data of one replicate of data_create
##reps: number of times to reshuffle
get_perm_dat<-function(dats,reps)
{
  c_dats_p<- vector(mode = "list", length = reps)
  h_dats_p<- vector(mode = "list", length = reps)
  c_dat_temp = dats$c_dats[[1]] #[[1]] because only one data set
  h_dat_temp = dats$h_dats[[1]]
  for(r in 1:reps)
  {
    c_dats_p[[r]]=c_dat_temp[sample(c(1:nrow(c_dat_temp)),nrow(c_dat_temp),replace=F),] %>% dplyr::filter(party=="PP") 
    h_dats_p[[r]]=h_dat_temp[sample(c(1:nrow(h_dat_temp)),nrow(h_dat_temp),replace=F),] %>% dplyr::filter(party=="PP") 
  }
  #rename to work with the stan code
  dat=list("c_dats"=c_dats_p,"h_dats"=h_dats_p)
  return(dat)
}

#permuted data sets

d1_p=get_perm_dat(dat=d1,reps=50)
d2_p=get_perm_dat(dat=d2,reps=50)
d3_p=get_perm_dat(dat=d3,reps=50)


# Model --------------------------------------------------------------------------------------------------------------

#stan_mod only one discounting coefficient
stan_mod<-'
data
{
// [Current trial]
//Number of rounds with a strike  
int<lower=1> N_c;
//vector cognizeable =1 or not (strikes)
int cog_c[N_c];
// vector number of cognizeables
int c_c[N_c];
// vector number non-cognizeables 
int m_c[N_c];
// [Historical Trials(pooled as one data set)]
//Number of rounds with a strike  (H)
int<lower=1> N_h;
//vector cognizeable =1 or not (strikes)
int cog_h[N_h];
// vector number of cognizeables (H)
int c_h[N_h];
// vector number non-cognizeables (H)
int m_h[N_h];
// discounting coefficient 
real<lower=0,upper=1> alpha;  
//inital prior 
real mean_init;
real sd_init;
}
parameters
{
//b=bias
real b; 
}
model
{
// priors 
//initial prior 
target += normal_lpdf(b|mean_init,sd_init);
// power prior 
for(j in 1:N_h)
{
target+=alpha*bernoulli_lpmf(cog_h[j]| 1.0*exp(b)*c_h[j] / (1.0*exp(b)*c_h[j]+m_h[j]));
}
// likelihood
for (i in 1:N_c)
{
target+=bernoulli_lpmf(cog_c[i]| 1.0*exp(b)*c_c[i] / (1.0*exp(b)*c_c[i]+m_c[i]));
}
}
'

fit<- stan_model(model_code=stan_mod) 




## Function: pp_sims#################################################
#@dats: list of data sets current and historical (list of lists)
#@alpha: discounting coefficient in power prior
#@mean_init: initial prior in power prior's mean 
#@sd_init: initial prior in power prior's sd
#@true_bias: the true bias in the current trial (fixed in data generation)
#@P : if =1 then will print the rep
#@num_trials: number of historical trials 
############################################################################

pp_sims<-function(dats,alpha,mean_init,sd_init,true_bias,P,num_htrials)
{
  ## Results to index
  #bias_b=bias of the bias-parameter
  #sd_b= posterior SD of the bias-parameter
  #ind_bias: indicator if bias is detected or not (with percent region)
  #ind_par: indicator if parameter is recovered or not (with percent region)
  # LB_CI_95/UB_CI_95: upper and lower bound of the credible interval (95%)
  res=data.frame("bias_b"=rep(NA,length(dats$c_dats)),
                 "mean_b"=rep(NA,length(dats$c_dats)),
                 "sd_b"=rep(NA,length(dats$c_dats)),
                 "ind_bias_95"=rep(NA,length(dats$c_dats)),
                 "ind_par_95"=rep(NA,length(dats$c_dats)),
                 "LB_CI_95"=rep(NA,length(dats$c_dats)),
                 "UB_CI_95"=rep(NA,length(dats$c_dats)))
  
  for(r in 1:length(dats$c_dats))
  {
    
    c_dat = dats$c_dats[[r]]
    h_dat = dats$h_dats[[r]]
    fit_mod <- sampling(
      fit,
      data = list(
        N_c = nrow(c_dat),
        cog_c = c_dat$cog,
        c_c = c_dat$num_cog,
        m_c = c_dat$m,
        N_h = nrow(h_dat),
        cog_h = h_dat$cog,
        c_h = h_dat$num_cog,
        m_h = h_dat$m,
        alpha = alpha,
        mean_init = mean_init,
        sd_init = sd_init
      ),
      chains = 1,
      warmup = 1000,
      iter = 10000
    )
    
    b = extract(fit_mod)$b
    
    
    res[r, "bias_b"] <- mean(b) - true_bias
    res[r,"mean_b"]<-mean(b)
    res[r, "sd_b"] <- sd(b)
    
    
    ci_95 <- as.numeric(quantile(b,probs=c(0.025,.975)))
    res[r,"LB_CI_95"]=ci_95[1]
    res[r,"UB_CI_95"]=ci_95[2]
    res[r, "ind_bias_95"] = !(ci_95[1] <= 0 & 0 <= ci_95[2])
    res[r, "ind_par_95"] = ci_95[1] <= true_bias & true_bias <= ci_95[2]
    
    
    #print iter
    if(P==1)
    {
      print(r)
    }
  }
  
  
  
  return(list(fit_mod,res))
}

#example=pp_sims(dats=temp,alpha=c(.1),mean_init=0,sd_init=2,true_bias=3,P=1,num_htrials=3)


simul_1=pp_sims(dats=d1_p,alpha=1,mean_init=0,sd_init=2,true_bias=1,P=1,num_htrials=1)

simul_2=pp_sims(dats=d2_p,alpha=1,mean_init=0,sd_init=2,true_bias=2,P=1,num_htrials=1)

simul_3=pp_sims(dats=d3_p,alpha=1,mean_init=0,sd_init=2,true_bias=3,P=1,num_htrials=1)





