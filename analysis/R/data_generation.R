
# Purpose of script ------------------------------------------------------------------------------------------------------------
# To generate data for simulations for the paper. 

# Libraries required 
library(rstan)
library(dplyr)


# sim_data function --------------------------------------------------------------------------------------------------

#sim_data: Function to generate simulated data sets
#@d_p: bias in prosecution
#@d_d: bias in defense
#@round_p: the number round of strikes for prosecution
#@round_d: the number round fo strikes for defense
#@total: total number of jurors  
#@num_cog: number of cognizable people
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
  df0$cog[1] = rbinom(1,1,p_cog)  #generate one decision of strike or not strike 
  df0$party[1] = 'PP' #initial party (PP=prosecution party)
  df0$num_cog[2] = df0$num_cog[1]-df0$cog[1] #decrease the number of cognizable jurors if strike is 1. 
  df0$total[2] = df0$total[1]-1 # decrease the number of potential jurors 
  p_cog <- df0$num_cog[2]*exp(d_d)/(df0$num_cog[2]*exp(d_d)+df0$total[2]-
                                      df0$num_cog[2]) #re calculate probability of striking a cognizable juror 
  df0$cog[2] = rbinom(1,1,p_cog) # repeat for initial strike for party PD (PD=party defense)
  df0$party[2] = 'PD' 
  #now loop through to populate the rest of the data frame
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


# get_stan_dat -------------------------------------------------------------------------------------------------------

#get_stan_dat: Function to get data into RStan for party of interest (i.e. PP)
#@d_p: bias in prosecution
#@d_d: bias in defense
#@round_p: the number round of strikes for prosecution
#@round_d: the number round fo strikes for defense
#@total: total number of jurors  
#@num_cog: number of cognizable people
#@trial_name: Name of trial (e.g. current data or historical data)

get_stan_dat<-function(d_p,d_d,round_p,round_d,total,num_cog,trial_name)
{
  dat1=sim_data(d_p,d_d,round_p,round_d,total,num_cog) # use to generate data frame of strikes
  dat1$trial=trial_name #trial_name 
  dat1$m=dat1$total-dat1$num_cog #number of non-cognizable jurors
  pp_dat = dat1 %>% dplyr::filter(party=="PP") #filter to only prosecution 
  return(pp_dat)
}


# get_historical -----------------------------------------------------------------------------------------------------


#get_historical: Funciton that calls get_stan_dat to create 'n_t' historical data files 
#@n_t: number of historical trials to generate 
#@d_p: bias in prosecution
#@d_d: bias in defense
#@round_p: the number round of strikes for prosecution
#@round_d: the number round fo strikes for defense
#@total: total number of jurors  
#@num_cog: number of cognizable people

#trial_name=1 for current and 2,..,n_t+1 for historical trials 
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


# data_create --------------------------------------------------------------------------------------------------------

#data_create: Function which returns lists of current dataset and historical dataset(s)
#@reps: Number of replicates
#@bias_p_c: bias of prosecution in current trial
#@bias_d_c: bias of defense in current trial (set at zero for paper results)
#@strikes_p_c: Number of strikes for prosecution in current trial
#@strikes_d_c: Number of strikes for defense in current trial
#@total_strikes_c: Total number of strikes used by both parties in current trial
#@num_cog_c: number of cognizable class members in current trial 
#@n_t: Number of historical trials in each replicate
#@bias_p_h: bias of prosecution in historical trial(s)
#@bias_d_h: bias of defense in historical trial(s) (set at zero for paper results)
#@strikes_p_h: Number of strikes for prosecution in historical trial(s)
#@strikes_d_h: Number of strikes for defense in historical trial(s)
#@total_strikes_h: Total number of strikes used by both parties in historical trial 
#@num_cog_h: Number of cognizable class members in historical trial(s)

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


# Dataset generation code for paper simulation study ----------------------------------------------------------------------------

# The following code generates the data sets under different scenarios for the model.R 
#@reps: Number of replicates (used 1000 for paper simulations)
#@bias_p_c: bias of prosecution in current trial (use values c(seq(0,3,.5)))
#@bias_d_c: bias of defense in current trial (set at zero for paper results) (assumed it's zero)
#@strikes_p_c: Number of strikes for prosecution in current trial (used 6,10,15)
#@strikes_d_c: Number of strikes for defense in current trial (used 6,10,15)
#@total_strikes_c: Total number of individuals who could be struck
# Used 28 when strikes_p_c=strikes_d_c= 6 or 10.
# Used 30 when strikes_p_c=strikes_d_c= 15.
#@num_cog_c: number of cognizable class members in current trial 
#@n_t: Number of historical trials in each replicate
#@bias_p_h: bias of prosecution in historical trial(s)
#@bias_d_h: bias of defense in historical trial(s) (set at zero for paper results)
#@strikes_p_h: Number of strikes for prosecution in historical trial(s)
#@strikes_d_h: Number of strikes for defense in historical trial(s)
#@total_strikes_h: Total number of strikes used by both parties in historical trial 
#@num_cog_h: Number of cognizable class members in historical trial(s)

# dataset labeling 
#dab_c meaning 
#a= bias in historical data: (1,2,3,4,5,6,7) corresponds to (0,.5,1,1.5,2,2.5,3)
#b = number of historical trials(1,2,3) corresponds to (1,2,3)
#c = number of strikes
# e.g. d13_6 = bias_h=0, number of h_trials=3, number of strike 6 
d11_6=data_create(reps=1000,bias_p_c=0,bias_d_c=0,strikes_p_c=6, strikes_d_c=6, 
                  total_strikes_c=28,num_cog_c=14,
                  n_t=1,bias_p_h=0,bias_d_h=0,strikes_p_h=6, 
                  strikes_d_h=6, total_strikes_h=28,num_cog_h=14)

d21_6=data_create(reps=1000,bias_p_c=0,bias_d_c=0,strikes_p_c=6, strikes_d_c=6, 
                  total_strikes_c=28,num_cog_c=14,
                  n_t=1,bias_p_h=.5,bias_d_h=0,strikes_p_h=6, 
                  strikes_d_h=6, total_strikes_h=28,num_cog_h=14)

d31_6=data_create(reps=1000,bias_p_c=0,bias_d_c=0,strikes_p_c=6, strikes_d_c=6, 
                  total_strikes_c=28,num_cog_c=14,
                  n_t=1,bias_p_h=1,bias_d_h=0,strikes_p_h=6, 
                  strikes_d_h=6, total_strikes_h=28,num_cog_h=14)

d41_6=data_create(reps=1000,bias_p_c=0,bias_d_c=0,strikes_p_c=6, strikes_d_c=6, 
                  total_strikes_c=28,num_cog_c=14,
                  n_t=1,bias_p_h=1.5,bias_d_h=0,strikes_p_h=6, 
                  strikes_d_h=6, total_strikes_h=28,num_cog_h=14)

d51_6=data_create(reps=1000,bias_p_c=0,bias_d_c=0,strikes_p_c=6, strikes_d_c=6, 
                  total_strikes_c=28,num_cog_c=14,
                  n_t=1,bias_p_h=2,bias_d_h=0,strikes_p_h=6, 
                  strikes_d_h=6, total_strikes_h=28,num_cog_h=14)

d61_6=data_create(reps=1000,bias_p_c=0,bias_d_c=0,strikes_p_c=6, strikes_d_c=6, 
                  total_strikes_c=28,num_cog_c=14,
                  n_t=1,bias_p_h=2.5,bias_d_h=0,strikes_p_h=6, 
                  strikes_d_h=6, total_strikes_h=28,num_cog_h=14)

d71_6=data_create(reps=1000,bias_p_c=0,bias_d_c=0,strikes_p_c=6, strikes_d_c=6, 
                  total_strikes_c=28,num_cog_c=14,
                  n_t=1,bias_p_h=3,bias_d_h=0,strikes_p_h=6, 
                  strikes_d_h=6, total_strikes_h=28,num_cog_h=14)



d12_6=data_create(reps=1000,bias_p_c=0,bias_d_c=0,strikes_p_c=6, strikes_d_c=6, 
                  total_strikes_c=28,num_cog_c=14,
                  n_t=2,bias_p_h=0,bias_d_h=0,strikes_p_h=6, 
                  strikes_d_h=6, total_strikes_h=28,num_cog_h=14)

d22_6=data_create(reps=1000,bias_p_c=0,bias_d_c=0,strikes_p_c=6, strikes_d_c=6, 
                  total_strikes_c=28,num_cog_c=14,
                  n_t=2,bias_p_h=.5,bias_d_h=0,strikes_p_h=6, 
                  strikes_d_h=6, total_strikes_h=28,num_cog_h=14)

d32_6=data_create(reps=1000,bias_p_c=0,bias_d_c=0,strikes_p_c=6, strikes_d_c=6, 
                  total_strikes_c=28,num_cog_c=14,
                  n_t=2,bias_p_h=1,bias_d_h=0,strikes_p_h=6, 
                  strikes_d_h=6, total_strikes_h=28,num_cog_h=14)

d42_6=data_create(reps=1000,bias_p_c=0,bias_d_c=0,strikes_p_c=6, strikes_d_c=6, 
                  total_strikes_c=28,num_cog_c=14,
                  n_t=2,bias_p_h=1.5,bias_d_h=0,strikes_p_h=6, 
                  strikes_d_h=6, total_strikes_h=28,num_cog_h=14)

d52_6=data_create(reps=1000,bias_p_c=0,bias_d_c=0,strikes_p_c=6, strikes_d_c=6, 
                  total_strikes_c=28,num_cog_c=14,
                  n_t=2,bias_p_h=2,bias_d_h=0,strikes_p_h=6, 
                  strikes_d_h=6, total_strikes_h=28,num_cog_h=14)

d62_6=data_create(reps=1000,bias_p_c=0,bias_d_c=0,strikes_p_c=6, strikes_d_c=6, 
                  total_strikes_c=28,num_cog_c=14,
                  n_t=2,bias_p_h=2.5,bias_d_h=0,strikes_p_h=6, 
                  strikes_d_h=6, total_strikes_h=28,num_cog_h=14)

d72_6=data_create(reps=1000,bias_p_c=0,bias_d_c=0,strikes_p_c=6, strikes_d_c=6, 
                  total_strikes_c=28,num_cog_c=14,
                  n_t=2,bias_p_h=3,bias_d_h=0,strikes_p_h=6, 
                  strikes_d_h=6, total_strikes_h=28,num_cog_h=14)


d13_6=data_create(reps=1000,bias_p_c=0,bias_d_c=0,strikes_p_c=6, strikes_d_c=6, 
                  total_strikes_c=28,num_cog_c=14,
                  n_t=3,bias_p_h=0,bias_d_h=0,strikes_p_h=6, 
                  strikes_d_h=6, total_strikes_h=28,num_cog_h=14)

d23_6=data_create(reps=1000,bias_p_c=0,bias_d_c=0,strikes_p_c=6, strikes_d_c=6, 
                  total_strikes_c=28,num_cog_c=14,
                  n_t=3,bias_p_h=.5,bias_d_h=0,strikes_p_h=6, 
                  strikes_d_h=6, total_strikes_h=28,num_cog_h=14)

d33_6=data_create(reps=1000,bias_p_c=0,bias_d_c=0,strikes_p_c=6, strikes_d_c=6, 
                  total_strikes_c=28,num_cog_c=14,
                  n_t=3,bias_p_h=1,bias_d_h=0,strikes_p_h=6, 
                  strikes_d_h=6, total_strikes_h=28,num_cog_h=14)

d43_6=data_create(reps=1000,bias_p_c=0,bias_d_c=0,strikes_p_c=6, strikes_d_c=6, 
                  total_strikes_c=28,num_cog_c=14,
                  n_t=3,bias_p_h=1.5,bias_d_h=0,strikes_p_h=6, 
                  strikes_d_h=6, total_strikes_h=28,num_cog_h=14)

d53_6=data_create(reps=1000,bias_p_c=0,bias_d_c=0,strikes_p_c=6, strikes_d_c=6, 
                  total_strikes_c=28,num_cog_c=14,
                  n_t=3,bias_p_h=2,bias_d_h=0,strikes_p_h=6, 
                  strikes_d_h=6, total_strikes_h=28,num_cog_h=14)

d63_6=data_create(reps=1000,bias_p_c=0,bias_d_c=0,strikes_p_c=6, strikes_d_c=6, 
                  total_strikes_c=28,num_cog_c=14,
                  n_t=3,bias_p_h=2.5,bias_d_h=0,strikes_p_h=6, 
                  strikes_d_h=6, total_strikes_h=28,num_cog_h=14)

d73_6=data_create(reps=1000,bias_p_c=0,bias_d_c=0,strikes_p_c=6, strikes_d_c=6, 
                  total_strikes_c=28,num_cog_c=14,
                  n_t=3,bias_p_h=3,bias_d_h=0,strikes_p_h=6, 
                  strikes_d_h=6, total_strikes_h=28,num_cog_h=14)

d11_10=data_create(reps=1000,bias_p_c=0,bias_d_c=0,strikes_p_c=10, strikes_d_c=10, 
                   total_strikes_c=28,num_cog_c=14,
                   n_t=1,bias_p_h=0,bias_d_h=0,strikes_p_h=10, 
                   strikes_d_h=10, total_strikes_h=28,num_cog_h=14)

d21_10=data_create(reps=1000,bias_p_c=0,bias_d_c=0,strikes_p_c=10, strikes_d_c=10, 
                   total_strikes_c=28,num_cog_c=14,
                   n_t=1,bias_p_h=.5,bias_d_h=0,strikes_p_h=10, 
                   strikes_d_h=10, total_strikes_h=28,num_cog_h=14)

d31_10=data_create(reps=1000,bias_p_c=0,bias_d_c=0,strikes_p_c=10, strikes_d_c=10, 
                   total_strikes_c=28,num_cog_c=14,
                   n_t=1,bias_p_h=1,bias_d_h=0,strikes_p_h=10, 
                   strikes_d_h=10, total_strikes_h=28,num_cog_h=14)

d41_10=data_create(reps=1000,bias_p_c=0,bias_d_c=0,strikes_p_c=10, strikes_d_c=10, 
                   total_strikes_c=28,num_cog_c=14,
                   n_t=1,bias_p_h=1.5,bias_d_h=0,strikes_p_h=10, 
                   strikes_d_h=10, total_strikes_h=28,num_cog_h=14)

d51_10=data_create(reps=1000,bias_p_c=0,bias_d_c=0,strikes_p_c=10, strikes_d_c=10, 
                   total_strikes_c=28,num_cog_c=14,
                   n_t=1,bias_p_h=2,bias_d_h=0,strikes_p_h=10, 
                   strikes_d_h=10, total_strikes_h=28,num_cog_h=14)

d61_10=data_create(reps=1000,bias_p_c=0,bias_d_c=0,strikes_p_c=10, strikes_d_c=10, 
                   total_strikes_c=28,num_cog_c=14,
                   n_t=1,bias_p_h=2.5,bias_d_h=0,strikes_p_h=10, 
                   strikes_d_h=10, total_strikes_h=28,num_cog_h=14)

d71_10=data_create(reps=1000,bias_p_c=0,bias_d_c=0,strikes_p_c=10, strikes_d_c=10, 
                   total_strikes_c=28,num_cog_c=14,
                   n_t=1,bias_p_h=3,bias_d_h=0,strikes_p_h=10, 
                   strikes_d_h=10, total_strikes_h=28,num_cog_h=14)



d12_10=data_create(reps=1000,bias_p_c=0,bias_d_c=0,strikes_p_c=10, strikes_d_c=10, 
                   total_strikes_c=28,num_cog_c=14,
                   n_t=2,bias_p_h=0,bias_d_h=0,strikes_p_h=10, 
                   strikes_d_h=10, total_strikes_h=28,num_cog_h=14)

d22_10=data_create(reps=1000,bias_p_c=0,bias_d_c=0,strikes_p_c=10, strikes_d_c=10, 
                   total_strikes_c=28,num_cog_c=14,
                   n_t=2,bias_p_h=.5,bias_d_h=0,strikes_p_h=10, 
                   strikes_d_h=10, total_strikes_h=28,num_cog_h=14)

d32_10=data_create(reps=1000,bias_p_c=0,bias_d_c=0,strikes_p_c=10, strikes_d_c=10, 
                   total_strikes_c=28,num_cog_c=14,
                   n_t=2,bias_p_h=1,bias_d_h=0,strikes_p_h=10, 
                   strikes_d_h=10, total_strikes_h=28,num_cog_h=14)

d42_10=data_create(reps=1000,bias_p_c=0,bias_d_c=0,strikes_p_c=10, strikes_d_c=10, 
                   total_strikes_c=28,num_cog_c=14,
                   n_t=2,bias_p_h=1.5,bias_d_h=0,strikes_p_h=10, 
                   strikes_d_h=10, total_strikes_h=28,num_cog_h=14)

d52_10=data_create(reps=1000,bias_p_c=0,bias_d_c=0,strikes_p_c=10, strikes_d_c=10, 
                   total_strikes_c=28,num_cog_c=14,
                   n_t=2,bias_p_h=2,bias_d_h=0,strikes_p_h=10, 
                   strikes_d_h=10, total_strikes_h=28,num_cog_h=14)

d62_10=data_create(reps=1000,bias_p_c=0,bias_d_c=0,strikes_p_c=10, strikes_d_c=10, 
                   total_strikes_c=28,num_cog_c=14,
                   n_t=2,bias_p_h=2.5,bias_d_h=0,strikes_p_h=10, 
                   strikes_d_h=10, total_strikes_h=28,num_cog_h=14)

d72_10=data_create(reps=1000,bias_p_c=0,bias_d_c=0,strikes_p_c=10, strikes_d_c=10, 
                   total_strikes_c=28,num_cog_c=14,
                   n_t=2,bias_p_h=3,bias_d_h=0,strikes_p_h=10, 
                   strikes_d_h=10, total_strikes_h=28,num_cog_h=14)


d13_10=data_create(reps=1000,bias_p_c=0,bias_d_c=0,strikes_p_c=10, strikes_d_c=10, 
                   total_strikes_c=28,num_cog_c=14,
                   n_t=3,bias_p_h=0,bias_d_h=0,strikes_p_h=10, 
                   strikes_d_h=10, total_strikes_h=28,num_cog_h=14)

d23_10=data_create(reps=1000,bias_p_c=0,bias_d_c=0,strikes_p_c=10, strikes_d_c=10, 
                   total_strikes_c=28,num_cog_c=14,
                   n_t=3,bias_p_h=.5,bias_d_h=0,strikes_p_h=10, 
                   strikes_d_h=10, total_strikes_h=28,num_cog_h=14)

d33_10=data_create(reps=1000,bias_p_c=0,bias_d_c=0,strikes_p_c=10, strikes_d_c=10, 
                   total_strikes_c=28,num_cog_c=14,
                   n_t=3,bias_p_h=1,bias_d_h=0,strikes_p_h=10, 
                   strikes_d_h=10, total_strikes_h=28,num_cog_h=14)

d43_10=data_create(reps=1000,bias_p_c=0,bias_d_c=0,strikes_p_c=10, strikes_d_c=10, 
                   total_strikes_c=28,num_cog_c=14,
                   n_t=3,bias_p_h=1.5,bias_d_h=0,strikes_p_h=10, 
                   strikes_d_h=10, total_strikes_h=28,num_cog_h=14)

d53_10=data_create(reps=1000,bias_p_c=0,bias_d_c=0,strikes_p_c=10, strikes_d_c=10, 
                   total_strikes_c=28,num_cog_c=14,
                   n_t=3,bias_p_h=2,bias_d_h=0,strikes_p_h=10, 
                   strikes_d_h=10, total_strikes_h=28,num_cog_h=14)

d63_10=data_create(reps=1000,bias_p_c=0,bias_d_c=0,strikes_p_c=10, strikes_d_c=10, 
                   total_strikes_c=28,num_cog_c=14,
                   n_t=3,bias_p_h=2.5,bias_d_h=0,strikes_p_h=10, 
                   strikes_d_h=10, total_strikes_h=28,num_cog_h=14)

d73_10=data_create(reps=1000,bias_p_c=0,bias_d_c=0,strikes_p_c=10, strikes_d_c=10, 
                   total_strikes_c=28,num_cog_c=14,
                   n_t=3,bias_p_h=3,bias_d_h=0,strikes_p_h=10, 
                   strikes_d_h=10, total_strikes_h=28,num_cog_h=14)



d11_15=data_create(reps=1000,bias_p_c=0,bias_d_c=0,strikes_p_c=15, strikes_d_c=15, 
                   total_strikes_c=30,num_cog_c=15,
                   n_t=1,bias_p_h=0,bias_d_h=0,strikes_p_h=15, 
                   strikes_d_h=15, total_strikes_h=30,num_cog_h=15)

d21_15=data_create(reps=1000,bias_p_c=0,bias_d_c=0,strikes_p_c=15, strikes_d_c=15, 
                   total_strikes_c=30,num_cog_c=15,
                   n_t=1,bias_p_h=.5,bias_d_h=0,strikes_p_h=15, 
                   strikes_d_h=15, total_strikes_h=30,num_cog_h=15)

d31_15=data_create(reps=1000,bias_p_c=0,bias_d_c=0,strikes_p_c=15, strikes_d_c=15, 
                   total_strikes_c=30,num_cog_c=15,
                   n_t=1,bias_p_h=1,bias_d_h=0,strikes_p_h=15, 
                   strikes_d_h=15, total_strikes_h=30,num_cog_h=15)

d41_15=data_create(reps=1000,bias_p_c=0,bias_d_c=0,strikes_p_c=15, strikes_d_c=15, 
                   total_strikes_c=30,num_cog_c=15,
                   n_t=1,bias_p_h=1.5,bias_d_h=0,strikes_p_h=15, 
                   strikes_d_h=15, total_strikes_h=30,num_cog_h=15)

d51_15=data_create(reps=1000,bias_p_c=0,bias_d_c=0,strikes_p_c=15, strikes_d_c=15, 
                   total_strikes_c=30,num_cog_c=15,
                   n_t=1,bias_p_h=2,bias_d_h=0,strikes_p_h=15, 
                   strikes_d_h=15, total_strikes_h=30,num_cog_h=15)

d61_15=data_create(reps=1000,bias_p_c=0,bias_d_c=0,strikes_p_c=15, strikes_d_c=15, 
                   total_strikes_c=30,num_cog_c=15,
                   n_t=1,bias_p_h=2.5,bias_d_h=0,strikes_p_h=15, 
                   strikes_d_h=15, total_strikes_h=30,num_cog_h=15)

d71_15=data_create(reps=1000,bias_p_c=0,bias_d_c=0,strikes_p_c=15, strikes_d_c=15, 
                   total_strikes_c=30,num_cog_c=15,
                   n_t=1,bias_p_h=3,bias_d_h=0,strikes_p_h=15, 
                   strikes_d_h=15, total_strikes_h=30,num_cog_h=15)



d12_15=data_create(reps=1000,bias_p_c=0,bias_d_c=0,strikes_p_c=15, strikes_d_c=15, 
                   total_strikes_c=30,num_cog_c=15,
                   n_t=2,bias_p_h=0,bias_d_h=0,strikes_p_h=15, 
                   strikes_d_h=15, total_strikes_h=30,num_cog_h=15)

d22_15=data_create(reps=1000,bias_p_c=0,bias_d_c=0,strikes_p_c=15, strikes_d_c=15, 
                   total_strikes_c=30,num_cog_c=15,
                   n_t=2,bias_p_h=.5,bias_d_h=0,strikes_p_h=15, 
                   strikes_d_h=15, total_strikes_h=30,num_cog_h=15)

d32_15=data_create(reps=1000,bias_p_c=0,bias_d_c=0,strikes_p_c=15, strikes_d_c=15, 
                   total_strikes_c=30,num_cog_c=15,
                   n_t=2,bias_p_h=1,bias_d_h=0,strikes_p_h=15, 
                   strikes_d_h=15, total_strikes_h=30,num_cog_h=15)

d42_15=data_create(reps=1000,bias_p_c=0,bias_d_c=0,strikes_p_c=15, strikes_d_c=15, 
                   total_strikes_c=30,num_cog_c=15,
                   n_t=2,bias_p_h=1.5,bias_d_h=0,strikes_p_h=15, 
                   strikes_d_h=15, total_strikes_h=30,num_cog_h=15)

d52_15=data_create(reps=1000,bias_p_c=0,bias_d_c=0,strikes_p_c=15, strikes_d_c=15, 
                   total_strikes_c=30,num_cog_c=15,
                   n_t=2,bias_p_h=2,bias_d_h=0,strikes_p_h=15, 
                   strikes_d_h=15, total_strikes_h=30,num_cog_h=15)

d62_15=data_create(reps=1000,bias_p_c=0,bias_d_c=0,strikes_p_c=15, strikes_d_c=15, 
                   total_strikes_c=30,num_cog_c=15,
                   n_t=2,bias_p_h=2.5,bias_d_h=0,strikes_p_h=15, 
                   strikes_d_h=15, total_strikes_h=30,num_cog_h=15)

d72_15=data_create(reps=1000,bias_p_c=0,bias_d_c=0,strikes_p_c=15, strikes_d_c=15, 
                   total_strikes_c=30,num_cog_c=15,
                   n_t=2,bias_p_h=3,bias_d_h=0,strikes_p_h=15, 
                   strikes_d_h=15, total_strikes_h=30,num_cog_h=15)


d13_15=data_create(reps=1000,bias_p_c=0,bias_d_c=0,strikes_p_c=15, strikes_d_c=15, 
                   total_strikes_c=30,num_cog_c=15,
                   n_t=3,bias_p_h=0,bias_d_h=0,strikes_p_h=15, 
                   strikes_d_h=15, total_strikes_h=30,num_cog_h=15)

d23_15=data_create(reps=1000,bias_p_c=0,bias_d_c=0,strikes_p_c=15, strikes_d_c=15, 
                   total_strikes_c=30,num_cog_c=15,
                   n_t=3,bias_p_h=.5,bias_d_h=0,strikes_p_h=15, 
                   strikes_d_h=15, total_strikes_h=30,num_cog_h=15)

d33_15=data_create(reps=1000,bias_p_c=0,bias_d_c=0,strikes_p_c=15, strikes_d_c=15, 
                   total_strikes_c=30,num_cog_c=15,
                   n_t=3,bias_p_h=1,bias_d_h=0,strikes_p_h=15, 
                   strikes_d_h=15, total_strikes_h=30,num_cog_h=15)

d43_15=data_create(reps=1000,bias_p_c=0,bias_d_c=0,strikes_p_c=15, strikes_d_c=15, 
                   total_strikes_c=30,num_cog_c=15,
                   n_t=3,bias_p_h=1.5,bias_d_h=0,strikes_p_h=15, 
                   strikes_d_h=15, total_strikes_h=30,num_cog_h=15)

d53_15=data_create(reps=1000,bias_p_c=0,bias_d_c=0,strikes_p_c=15, strikes_d_c=15, 
                   total_strikes_c=30,num_cog_c=15,
                   n_t=3,bias_p_h=2,bias_d_h=0,strikes_p_h=15, 
                   strikes_d_h=15, total_strikes_h=30,num_cog_h=15)

d63_15=data_create(reps=1000,bias_p_c=0,bias_d_c=0,strikes_p_c=15, strikes_d_c=15, 
                   total_strikes_c=30,num_cog_c=15,
                   n_t=3,bias_p_h=2.5,bias_d_h=0,strikes_p_h=15, 
                   strikes_d_h=15, total_strikes_h=30,num_cog_h=15)

d73_15=data_create(reps=1000,bias_p_c=0,bias_d_c=0,strikes_p_c=15, strikes_d_c=15, 
                   total_strikes_c=30,num_cog_c=15,
                   n_t=3,bias_p_h=3,bias_d_h=0,strikes_p_h=15, 
                   strikes_d_h=15, total_strikes_h=30,num_cog_h=15)

save.image("/dats0.RData")


#bias_p_c=.5
d11_6=data_create(reps=1000,bias_p_c=.5,bias_d_c=0,strikes_p_c=6, strikes_d_c=6, 
                  total_strikes_c=28,num_cog_c=14,
                  n_t=1,bias_p_h=0,bias_d_h=0,strikes_p_h=6, 
                  strikes_d_h=6, total_strikes_h=28,num_cog_h=14)

d21_6=data_create(reps=1000,bias_p_c=.5,bias_d_c=0,strikes_p_c=6, strikes_d_c=6, 
                  total_strikes_c=28,num_cog_c=14,
                  n_t=1,bias_p_h=.5,bias_d_h=0,strikes_p_h=6, 
                  strikes_d_h=6, total_strikes_h=28,num_cog_h=14)

d31_6=data_create(reps=1000,bias_p_c=.5,bias_d_c=0,strikes_p_c=6, strikes_d_c=6, 
                  total_strikes_c=28,num_cog_c=14,
                  n_t=1,bias_p_h=1,bias_d_h=0,strikes_p_h=6, 
                  strikes_d_h=6, total_strikes_h=28,num_cog_h=14)

d41_6=data_create(reps=1000,bias_p_c=.5,bias_d_c=0,strikes_p_c=6, strikes_d_c=6, 
                  total_strikes_c=28,num_cog_c=14,
                  n_t=1,bias_p_h=1.5,bias_d_h=0,strikes_p_h=6, 
                  strikes_d_h=6, total_strikes_h=28,num_cog_h=14)

d51_6=data_create(reps=1000,bias_p_c=.5,bias_d_c=0,strikes_p_c=6, strikes_d_c=6, 
                  total_strikes_c=28,num_cog_c=14,
                  n_t=1,bias_p_h=2,bias_d_h=0,strikes_p_h=6, 
                  strikes_d_h=6, total_strikes_h=28,num_cog_h=14)

d61_6=data_create(reps=1000,bias_p_c=.5,bias_d_c=0,strikes_p_c=6, strikes_d_c=6, 
                  total_strikes_c=28,num_cog_c=14,
                  n_t=1,bias_p_h=2.5,bias_d_h=0,strikes_p_h=6, 
                  strikes_d_h=6, total_strikes_h=28,num_cog_h=14)

d71_6=data_create(reps=1000,bias_p_c=.5,bias_d_c=0,strikes_p_c=6, strikes_d_c=6, 
                  total_strikes_c=28,num_cog_c=14,
                  n_t=1,bias_p_h=3,bias_d_h=0,strikes_p_h=6, 
                  strikes_d_h=6, total_strikes_h=28,num_cog_h=14)



d12_6=data_create(reps=1000,bias_p_c=.5,bias_d_c=0,strikes_p_c=6, strikes_d_c=6, 
                  total_strikes_c=28,num_cog_c=14,
                  n_t=2,bias_p_h=0,bias_d_h=0,strikes_p_h=6, 
                  strikes_d_h=6, total_strikes_h=28,num_cog_h=14)

d22_6=data_create(reps=1000,bias_p_c=.5,bias_d_c=0,strikes_p_c=6, strikes_d_c=6, 
                  total_strikes_c=28,num_cog_c=14,
                  n_t=2,bias_p_h=.5,bias_d_h=0,strikes_p_h=6, 
                  strikes_d_h=6, total_strikes_h=28,num_cog_h=14)

d32_6=data_create(reps=1000,bias_p_c=.5,bias_d_c=0,strikes_p_c=6, strikes_d_c=6, 
                  total_strikes_c=28,num_cog_c=14,
                  n_t=2,bias_p_h=1,bias_d_h=0,strikes_p_h=6, 
                  strikes_d_h=6, total_strikes_h=28,num_cog_h=14)

d42_6=data_create(reps=1000,bias_p_c=.5,bias_d_c=0,strikes_p_c=6, strikes_d_c=6, 
                  total_strikes_c=28,num_cog_c=14,
                  n_t=2,bias_p_h=1.5,bias_d_h=0,strikes_p_h=6, 
                  strikes_d_h=6, total_strikes_h=28,num_cog_h=14)

d52_6=data_create(reps=1000,bias_p_c=.5,bias_d_c=0,strikes_p_c=6, strikes_d_c=6, 
                  total_strikes_c=28,num_cog_c=14,
                  n_t=2,bias_p_h=2,bias_d_h=0,strikes_p_h=6, 
                  strikes_d_h=6, total_strikes_h=28,num_cog_h=14)

d62_6=data_create(reps=1000,bias_p_c=.5,bias_d_c=0,strikes_p_c=6, strikes_d_c=6, 
                  total_strikes_c=28,num_cog_c=14,
                  n_t=2,bias_p_h=2.5,bias_d_h=0,strikes_p_h=6, 
                  strikes_d_h=6, total_strikes_h=28,num_cog_h=14)

d72_6=data_create(reps=1000,bias_p_c=.5,bias_d_c=0,strikes_p_c=6, strikes_d_c=6, 
                  total_strikes_c=28,num_cog_c=14,
                  n_t=2,bias_p_h=3,bias_d_h=0,strikes_p_h=6, 
                  strikes_d_h=6, total_strikes_h=28,num_cog_h=14)


d13_6=data_create(reps=1000,bias_p_c=.5,bias_d_c=0,strikes_p_c=6, strikes_d_c=6, 
                  total_strikes_c=28,num_cog_c=14,
                  n_t=3,bias_p_h=0,bias_d_h=0,strikes_p_h=6, 
                  strikes_d_h=6, total_strikes_h=28,num_cog_h=14)

d23_6=data_create(reps=1000,bias_p_c=.5,bias_d_c=0,strikes_p_c=6, strikes_d_c=6, 
                  total_strikes_c=28,num_cog_c=14,
                  n_t=3,bias_p_h=.5,bias_d_h=0,strikes_p_h=6, 
                  strikes_d_h=6, total_strikes_h=28,num_cog_h=14)

d33_6=data_create(reps=1000,bias_p_c=.5,bias_d_c=0,strikes_p_c=6, strikes_d_c=6, 
                  total_strikes_c=28,num_cog_c=14,
                  n_t=3,bias_p_h=1,bias_d_h=0,strikes_p_h=6, 
                  strikes_d_h=6, total_strikes_h=28,num_cog_h=14)

d43_6=data_create(reps=1000,bias_p_c=.5,bias_d_c=0,strikes_p_c=6, strikes_d_c=6, 
                  total_strikes_c=28,num_cog_c=14,
                  n_t=3,bias_p_h=1.5,bias_d_h=0,strikes_p_h=6, 
                  strikes_d_h=6, total_strikes_h=28,num_cog_h=14)

d53_6=data_create(reps=1000,bias_p_c=.5,bias_d_c=0,strikes_p_c=6, strikes_d_c=6, 
                  total_strikes_c=28,num_cog_c=14,
                  n_t=3,bias_p_h=2,bias_d_h=0,strikes_p_h=6, 
                  strikes_d_h=6, total_strikes_h=28,num_cog_h=14)

d63_6=data_create(reps=1000,bias_p_c=.5,bias_d_c=0,strikes_p_c=6, strikes_d_c=6, 
                  total_strikes_c=28,num_cog_c=14,
                  n_t=3,bias_p_h=2.5,bias_d_h=0,strikes_p_h=6, 
                  strikes_d_h=6, total_strikes_h=28,num_cog_h=14)

d73_6=data_create(reps=1000,bias_p_c=.5,bias_d_c=0,strikes_p_c=6, strikes_d_c=6, 
                  total_strikes_c=28,num_cog_c=14,
                  n_t=3,bias_p_h=3,bias_d_h=0,strikes_p_h=6, 
                  strikes_d_h=6, total_strikes_h=28,num_cog_h=14)

d11_10=data_create(reps=1000,bias_p_c=.5,bias_d_c=0,strikes_p_c=10, strikes_d_c=10, 
                   total_strikes_c=28,num_cog_c=14,
                   n_t=1,bias_p_h=0,bias_d_h=0,strikes_p_h=10, 
                   strikes_d_h=10, total_strikes_h=28,num_cog_h=14)

d21_10=data_create(reps=1000,bias_p_c=.5,bias_d_c=0,strikes_p_c=10, strikes_d_c=10, 
                   total_strikes_c=28,num_cog_c=14,
                   n_t=1,bias_p_h=.5,bias_d_h=0,strikes_p_h=10, 
                   strikes_d_h=10, total_strikes_h=28,num_cog_h=14)

d31_10=data_create(reps=1000,bias_p_c=.5,bias_d_c=0,strikes_p_c=10, strikes_d_c=10, 
                   total_strikes_c=28,num_cog_c=14,
                   n_t=1,bias_p_h=1,bias_d_h=0,strikes_p_h=10, 
                   strikes_d_h=10, total_strikes_h=28,num_cog_h=14)

d41_10=data_create(reps=1000,bias_p_c=.5,bias_d_c=0,strikes_p_c=10, strikes_d_c=10, 
                   total_strikes_c=28,num_cog_c=14,
                   n_t=1,bias_p_h=1.5,bias_d_h=0,strikes_p_h=10, 
                   strikes_d_h=10, total_strikes_h=28,num_cog_h=14)

d51_10=data_create(reps=1000,bias_p_c=.5,bias_d_c=0,strikes_p_c=10, strikes_d_c=10, 
                   total_strikes_c=28,num_cog_c=14,
                   n_t=1,bias_p_h=2,bias_d_h=0,strikes_p_h=10, 
                   strikes_d_h=10, total_strikes_h=28,num_cog_h=14)

d61_10=data_create(reps=1000,bias_p_c=.5,bias_d_c=0,strikes_p_c=10, strikes_d_c=10, 
                   total_strikes_c=28,num_cog_c=14,
                   n_t=1,bias_p_h=2.5,bias_d_h=0,strikes_p_h=10, 
                   strikes_d_h=10, total_strikes_h=28,num_cog_h=14)

d71_10=data_create(reps=1000,bias_p_c=.5,bias_d_c=0,strikes_p_c=10, strikes_d_c=10, 
                   total_strikes_c=28,num_cog_c=14,
                   n_t=1,bias_p_h=3,bias_d_h=0,strikes_p_h=10, 
                   strikes_d_h=10, total_strikes_h=28,num_cog_h=14)



d12_10=data_create(reps=1000,bias_p_c=.5,bias_d_c=0,strikes_p_c=10, strikes_d_c=10, 
                   total_strikes_c=28,num_cog_c=14,
                   n_t=2,bias_p_h=0,bias_d_h=0,strikes_p_h=10, 
                   strikes_d_h=10, total_strikes_h=28,num_cog_h=14)

d22_10=data_create(reps=1000,bias_p_c=.5,bias_d_c=0,strikes_p_c=10, strikes_d_c=10, 
                   total_strikes_c=28,num_cog_c=14,
                   n_t=2,bias_p_h=.5,bias_d_h=0,strikes_p_h=10, 
                   strikes_d_h=10, total_strikes_h=28,num_cog_h=14)

d32_10=data_create(reps=1000,bias_p_c=.5,bias_d_c=0,strikes_p_c=10, strikes_d_c=10, 
                   total_strikes_c=28,num_cog_c=14,
                   n_t=2,bias_p_h=1,bias_d_h=0,strikes_p_h=10, 
                   strikes_d_h=10, total_strikes_h=28,num_cog_h=14)

d42_10=data_create(reps=1000,bias_p_c=.5,bias_d_c=0,strikes_p_c=10, strikes_d_c=10, 
                   total_strikes_c=28,num_cog_c=14,
                   n_t=2,bias_p_h=1.5,bias_d_h=0,strikes_p_h=10, 
                   strikes_d_h=10, total_strikes_h=28,num_cog_h=14)

d52_10=data_create(reps=1000,bias_p_c=.5,bias_d_c=0,strikes_p_c=10, strikes_d_c=10, 
                   total_strikes_c=28,num_cog_c=14,
                   n_t=2,bias_p_h=2,bias_d_h=0,strikes_p_h=10, 
                   strikes_d_h=10, total_strikes_h=28,num_cog_h=14)

d62_10=data_create(reps=1000,bias_p_c=.5,bias_d_c=0,strikes_p_c=10, strikes_d_c=10, 
                   total_strikes_c=28,num_cog_c=14,
                   n_t=2,bias_p_h=2.5,bias_d_h=0,strikes_p_h=10, 
                   strikes_d_h=10, total_strikes_h=28,num_cog_h=14)

d72_10=data_create(reps=1000,bias_p_c=.5,bias_d_c=0,strikes_p_c=10, strikes_d_c=10, 
                   total_strikes_c=28,num_cog_c=14,
                   n_t=2,bias_p_h=3,bias_d_h=0,strikes_p_h=10, 
                   strikes_d_h=10, total_strikes_h=28,num_cog_h=14)


d13_10=data_create(reps=1000,bias_p_c=.5,bias_d_c=0,strikes_p_c=10, strikes_d_c=10, 
                   total_strikes_c=28,num_cog_c=14,
                   n_t=3,bias_p_h=0,bias_d_h=0,strikes_p_h=10, 
                   strikes_d_h=10, total_strikes_h=28,num_cog_h=14)

d23_10=data_create(reps=1000,bias_p_c=.5,bias_d_c=0,strikes_p_c=10, strikes_d_c=10, 
                   total_strikes_c=28,num_cog_c=14,
                   n_t=3,bias_p_h=.5,bias_d_h=0,strikes_p_h=10, 
                   strikes_d_h=10, total_strikes_h=28,num_cog_h=14)

d33_10=data_create(reps=1000,bias_p_c=.5,bias_d_c=0,strikes_p_c=10, strikes_d_c=10, 
                   total_strikes_c=28,num_cog_c=14,
                   n_t=3,bias_p_h=1,bias_d_h=0,strikes_p_h=10, 
                   strikes_d_h=10, total_strikes_h=28,num_cog_h=14)

d43_10=data_create(reps=1000,bias_p_c=.5,bias_d_c=0,strikes_p_c=10, strikes_d_c=10, 
                   total_strikes_c=28,num_cog_c=14,
                   n_t=3,bias_p_h=1.5,bias_d_h=0,strikes_p_h=10, 
                   strikes_d_h=10, total_strikes_h=28,num_cog_h=14)

d53_10=data_create(reps=1000,bias_p_c=.5,bias_d_c=0,strikes_p_c=10, strikes_d_c=10, 
                   total_strikes_c=28,num_cog_c=14,
                   n_t=3,bias_p_h=2,bias_d_h=0,strikes_p_h=10, 
                   strikes_d_h=10, total_strikes_h=28,num_cog_h=14)

d63_10=data_create(reps=1000,bias_p_c=.5,bias_d_c=0,strikes_p_c=10, strikes_d_c=10, 
                   total_strikes_c=28,num_cog_c=14,
                   n_t=3,bias_p_h=2.5,bias_d_h=0,strikes_p_h=10, 
                   strikes_d_h=10, total_strikes_h=28,num_cog_h=14)

d73_10=data_create(reps=1000,bias_p_c=.5,bias_d_c=0,strikes_p_c=10, strikes_d_c=10, 
                   total_strikes_c=28,num_cog_c=14,
                   n_t=3,bias_p_h=3,bias_d_h=0,strikes_p_h=10, 
                   strikes_d_h=10, total_strikes_h=28,num_cog_h=14)



d11_15=data_create(reps=1000,bias_p_c=.5,bias_d_c=0,strikes_p_c=15, strikes_d_c=15, 
                   total_strikes_c=30,num_cog_c=15,
                   n_t=1,bias_p_h=0,bias_d_h=0,strikes_p_h=15, 
                   strikes_d_h=15, total_strikes_h=30,num_cog_h=15)

d21_15=data_create(reps=1000,bias_p_c=.5,bias_d_c=0,strikes_p_c=15, strikes_d_c=15, 
                   total_strikes_c=30,num_cog_c=15,
                   n_t=1,bias_p_h=.5,bias_d_h=0,strikes_p_h=15, 
                   strikes_d_h=15, total_strikes_h=30,num_cog_h=15)

d31_15=data_create(reps=1000,bias_p_c=.5,bias_d_c=0,strikes_p_c=15, strikes_d_c=15, 
                   total_strikes_c=30,num_cog_c=15,
                   n_t=1,bias_p_h=1,bias_d_h=0,strikes_p_h=15, 
                   strikes_d_h=15, total_strikes_h=30,num_cog_h=15)

d41_15=data_create(reps=1000,bias_p_c=.5,bias_d_c=0,strikes_p_c=15, strikes_d_c=15, 
                   total_strikes_c=30,num_cog_c=15,
                   n_t=1,bias_p_h=1.5,bias_d_h=0,strikes_p_h=15, 
                   strikes_d_h=15, total_strikes_h=30,num_cog_h=15)

d51_15=data_create(reps=1000,bias_p_c=.5,bias_d_c=0,strikes_p_c=15, strikes_d_c=15, 
                   total_strikes_c=30,num_cog_c=15,
                   n_t=1,bias_p_h=2,bias_d_h=0,strikes_p_h=15, 
                   strikes_d_h=15, total_strikes_h=30,num_cog_h=15)

d61_15=data_create(reps=1000,bias_p_c=.5,bias_d_c=0,strikes_p_c=15, strikes_d_c=15, 
                   total_strikes_c=30,num_cog_c=15,
                   n_t=1,bias_p_h=2.5,bias_d_h=0,strikes_p_h=15, 
                   strikes_d_h=15, total_strikes_h=30,num_cog_h=15)

d71_15=data_create(reps=1000,bias_p_c=.5,bias_d_c=0,strikes_p_c=15, strikes_d_c=15, 
                   total_strikes_c=30,num_cog_c=15,
                   n_t=1,bias_p_h=3,bias_d_h=0,strikes_p_h=15, 
                   strikes_d_h=15, total_strikes_h=30,num_cog_h=15)



d12_15=data_create(reps=1000,bias_p_c=.5,bias_d_c=0,strikes_p_c=15, strikes_d_c=15, 
                   total_strikes_c=30,num_cog_c=15,
                   n_t=2,bias_p_h=0,bias_d_h=0,strikes_p_h=15, 
                   strikes_d_h=15, total_strikes_h=30,num_cog_h=15)

d22_15=data_create(reps=1000,bias_p_c=.5,bias_d_c=0,strikes_p_c=15, strikes_d_c=15, 
                   total_strikes_c=30,num_cog_c=15,
                   n_t=2,bias_p_h=.5,bias_d_h=0,strikes_p_h=15, 
                   strikes_d_h=15, total_strikes_h=30,num_cog_h=15)

d32_15=data_create(reps=1000,bias_p_c=.5,bias_d_c=0,strikes_p_c=15, strikes_d_c=15, 
                   total_strikes_c=30,num_cog_c=15,
                   n_t=2,bias_p_h=1,bias_d_h=0,strikes_p_h=15, 
                   strikes_d_h=15, total_strikes_h=30,num_cog_h=15)

d42_15=data_create(reps=1000,bias_p_c=.5,bias_d_c=0,strikes_p_c=15, strikes_d_c=15, 
                   total_strikes_c=30,num_cog_c=15,
                   n_t=2,bias_p_h=1.5,bias_d_h=0,strikes_p_h=15, 
                   strikes_d_h=15, total_strikes_h=30,num_cog_h=15)

d52_15=data_create(reps=1000,bias_p_c=.5,bias_d_c=0,strikes_p_c=15, strikes_d_c=15, 
                   total_strikes_c=30,num_cog_c=15,
                   n_t=2,bias_p_h=2,bias_d_h=0,strikes_p_h=15, 
                   strikes_d_h=15, total_strikes_h=30,num_cog_h=15)

d62_15=data_create(reps=1000,bias_p_c=.5,bias_d_c=0,strikes_p_c=15, strikes_d_c=15, 
                   total_strikes_c=30,num_cog_c=15,
                   n_t=2,bias_p_h=2.5,bias_d_h=0,strikes_p_h=15, 
                   strikes_d_h=15, total_strikes_h=30,num_cog_h=15)

d72_15=data_create(reps=1000,bias_p_c=.5,bias_d_c=0,strikes_p_c=15, strikes_d_c=15, 
                   total_strikes_c=30,num_cog_c=15,
                   n_t=2,bias_p_h=3,bias_d_h=0,strikes_p_h=15, 
                   strikes_d_h=15, total_strikes_h=30,num_cog_h=15)


d13_15=data_create(reps=1000,bias_p_c=.5,bias_d_c=0,strikes_p_c=15, strikes_d_c=15, 
                   total_strikes_c=30,num_cog_c=15,
                   n_t=3,bias_p_h=0,bias_d_h=0,strikes_p_h=15, 
                   strikes_d_h=15, total_strikes_h=30,num_cog_h=15)

d23_15=data_create(reps=1000,bias_p_c=.5,bias_d_c=0,strikes_p_c=15, strikes_d_c=15, 
                   total_strikes_c=30,num_cog_c=15,
                   n_t=3,bias_p_h=.5,bias_d_h=0,strikes_p_h=15, 
                   strikes_d_h=15, total_strikes_h=30,num_cog_h=15)

d33_15=data_create(reps=1000,bias_p_c=.5,bias_d_c=0,strikes_p_c=15, strikes_d_c=15, 
                   total_strikes_c=30,num_cog_c=15,
                   n_t=3,bias_p_h=1,bias_d_h=0,strikes_p_h=15, 
                   strikes_d_h=15, total_strikes_h=30,num_cog_h=15)

d43_15=data_create(reps=1000,bias_p_c=.5,bias_d_c=0,strikes_p_c=15, strikes_d_c=15, 
                   total_strikes_c=30,num_cog_c=15,
                   n_t=3,bias_p_h=1.5,bias_d_h=0,strikes_p_h=15, 
                   strikes_d_h=15, total_strikes_h=30,num_cog_h=15)

d53_15=data_create(reps=1000,bias_p_c=.5,bias_d_c=0,strikes_p_c=15, strikes_d_c=15, 
                   total_strikes_c=30,num_cog_c=15,
                   n_t=3,bias_p_h=2,bias_d_h=0,strikes_p_h=15, 
                   strikes_d_h=15, total_strikes_h=30,num_cog_h=15)

d63_15=data_create(reps=1000,bias_p_c=.5,bias_d_c=0,strikes_p_c=15, strikes_d_c=15, 
                   total_strikes_c=30,num_cog_c=15,
                   n_t=3,bias_p_h=2.5,bias_d_h=0,strikes_p_h=15, 
                   strikes_d_h=15, total_strikes_h=30,num_cog_h=15)

d73_15=data_create(reps=1000,bias_p_c=.5,bias_d_c=0,strikes_p_c=15, strikes_d_c=15, 
                   total_strikes_c=30,num_cog_c=15,
                   n_t=3,bias_p_h=3,bias_d_h=0,strikes_p_h=15, 
                   strikes_d_h=15, total_strikes_h=30,num_cog_h=15)

save.image("/dats.5.RData")


#bias_p_c=1
d11_6=data_create(reps=1000,bias_p_c=1,bias_d_c=0,strikes_p_c=6, strikes_d_c=6, 
                  total_strikes_c=28,num_cog_c=14,
                  n_t=1,bias_p_h=0,bias_d_h=0,strikes_p_h=6, 
                  strikes_d_h=6, total_strikes_h=28,num_cog_h=14)

d21_6=data_create(reps=1000,bias_p_c=1,bias_d_c=0,strikes_p_c=6, strikes_d_c=6, 
                  total_strikes_c=28,num_cog_c=14,
                  n_t=1,bias_p_h=.5,bias_d_h=0,strikes_p_h=6, 
                  strikes_d_h=6, total_strikes_h=28,num_cog_h=14)

d31_6=data_create(reps=1000,bias_p_c=1,bias_d_c=0,strikes_p_c=6, strikes_d_c=6, 
                  total_strikes_c=28,num_cog_c=14,
                  n_t=1,bias_p_h=1,bias_d_h=0,strikes_p_h=6, 
                  strikes_d_h=6, total_strikes_h=28,num_cog_h=14)

d41_6=data_create(reps=1000,bias_p_c=1,bias_d_c=0,strikes_p_c=6, strikes_d_c=6, 
                  total_strikes_c=28,num_cog_c=14,
                  n_t=1,bias_p_h=1.5,bias_d_h=0,strikes_p_h=6, 
                  strikes_d_h=6, total_strikes_h=28,num_cog_h=14)

d51_6=data_create(reps=1000,bias_p_c=1,bias_d_c=0,strikes_p_c=6, strikes_d_c=6, 
                  total_strikes_c=28,num_cog_c=14,
                  n_t=1,bias_p_h=2,bias_d_h=0,strikes_p_h=6, 
                  strikes_d_h=6, total_strikes_h=28,num_cog_h=14)

d61_6=data_create(reps=1000,bias_p_c=1,bias_d_c=0,strikes_p_c=6, strikes_d_c=6, 
                  total_strikes_c=28,num_cog_c=14,
                  n_t=1,bias_p_h=2.5,bias_d_h=0,strikes_p_h=6, 
                  strikes_d_h=6, total_strikes_h=28,num_cog_h=14)

d71_6=data_create(reps=1000,bias_p_c=1,bias_d_c=0,strikes_p_c=6, strikes_d_c=6, 
                  total_strikes_c=28,num_cog_c=14,
                  n_t=1,bias_p_h=3,bias_d_h=0,strikes_p_h=6, 
                  strikes_d_h=6, total_strikes_h=28,num_cog_h=14)



d12_6=data_create(reps=1000,bias_p_c=1,bias_d_c=0,strikes_p_c=6, strikes_d_c=6, 
                  total_strikes_c=28,num_cog_c=14,
                  n_t=2,bias_p_h=0,bias_d_h=0,strikes_p_h=6, 
                  strikes_d_h=6, total_strikes_h=28,num_cog_h=14)

d22_6=data_create(reps=1000,bias_p_c=1,bias_d_c=0,strikes_p_c=6, strikes_d_c=6, 
                  total_strikes_c=28,num_cog_c=14,
                  n_t=2,bias_p_h=.5,bias_d_h=0,strikes_p_h=6, 
                  strikes_d_h=6, total_strikes_h=28,num_cog_h=14)

d32_6=data_create(reps=1000,bias_p_c=1,bias_d_c=0,strikes_p_c=6, strikes_d_c=6, 
                  total_strikes_c=28,num_cog_c=14,
                  n_t=2,bias_p_h=1,bias_d_h=0,strikes_p_h=6, 
                  strikes_d_h=6, total_strikes_h=28,num_cog_h=14)

d42_6=data_create(reps=1000,bias_p_c=1,bias_d_c=0,strikes_p_c=6, strikes_d_c=6, 
                  total_strikes_c=28,num_cog_c=14,
                  n_t=2,bias_p_h=1.5,bias_d_h=0,strikes_p_h=6, 
                  strikes_d_h=6, total_strikes_h=28,num_cog_h=14)

d52_6=data_create(reps=1000,bias_p_c=1,bias_d_c=0,strikes_p_c=6, strikes_d_c=6, 
                  total_strikes_c=28,num_cog_c=14,
                  n_t=2,bias_p_h=2,bias_d_h=0,strikes_p_h=6, 
                  strikes_d_h=6, total_strikes_h=28,num_cog_h=14)

d62_6=data_create(reps=1000,bias_p_c=1,bias_d_c=0,strikes_p_c=6, strikes_d_c=6, 
                  total_strikes_c=28,num_cog_c=14,
                  n_t=2,bias_p_h=2.5,bias_d_h=0,strikes_p_h=6, 
                  strikes_d_h=6, total_strikes_h=28,num_cog_h=14)

d72_6=data_create(reps=1000,bias_p_c=1,bias_d_c=0,strikes_p_c=6, strikes_d_c=6, 
                  total_strikes_c=28,num_cog_c=14,
                  n_t=2,bias_p_h=3,bias_d_h=0,strikes_p_h=6, 
                  strikes_d_h=6, total_strikes_h=28,num_cog_h=14)


d13_6=data_create(reps=1000,bias_p_c=1,bias_d_c=0,strikes_p_c=6, strikes_d_c=6, 
                  total_strikes_c=28,num_cog_c=14,
                  n_t=3,bias_p_h=0,bias_d_h=0,strikes_p_h=6, 
                  strikes_d_h=6, total_strikes_h=28,num_cog_h=14)

d23_6=data_create(reps=1000,bias_p_c=1,bias_d_c=0,strikes_p_c=6, strikes_d_c=6, 
                  total_strikes_c=28,num_cog_c=14,
                  n_t=3,bias_p_h=.5,bias_d_h=0,strikes_p_h=6, 
                  strikes_d_h=6, total_strikes_h=28,num_cog_h=14)

d33_6=data_create(reps=1000,bias_p_c=1,bias_d_c=0,strikes_p_c=6, strikes_d_c=6, 
                  total_strikes_c=28,num_cog_c=14,
                  n_t=3,bias_p_h=1,bias_d_h=0,strikes_p_h=6, 
                  strikes_d_h=6, total_strikes_h=28,num_cog_h=14)

d43_6=data_create(reps=1000,bias_p_c=1,bias_d_c=0,strikes_p_c=6, strikes_d_c=6, 
                  total_strikes_c=28,num_cog_c=14,
                  n_t=3,bias_p_h=1.5,bias_d_h=0,strikes_p_h=6, 
                  strikes_d_h=6, total_strikes_h=28,num_cog_h=14)

d53_6=data_create(reps=1000,bias_p_c=1,bias_d_c=0,strikes_p_c=6, strikes_d_c=6, 
                  total_strikes_c=28,num_cog_c=14,
                  n_t=3,bias_p_h=2,bias_d_h=0,strikes_p_h=6, 
                  strikes_d_h=6, total_strikes_h=28,num_cog_h=14)

d63_6=data_create(reps=1000,bias_p_c=1,bias_d_c=0,strikes_p_c=6, strikes_d_c=6, 
                  total_strikes_c=28,num_cog_c=14,
                  n_t=3,bias_p_h=2.5,bias_d_h=0,strikes_p_h=6, 
                  strikes_d_h=6, total_strikes_h=28,num_cog_h=14)

d73_6=data_create(reps=1000,bias_p_c=1,bias_d_c=0,strikes_p_c=6, strikes_d_c=6, 
                  total_strikes_c=28,num_cog_c=14,
                  n_t=3,bias_p_h=3,bias_d_h=0,strikes_p_h=6, 
                  strikes_d_h=6, total_strikes_h=28,num_cog_h=14)

d11_10=data_create(reps=1000,bias_p_c=1,bias_d_c=0,strikes_p_c=10, strikes_d_c=10, 
                   total_strikes_c=28,num_cog_c=14,
                   n_t=1,bias_p_h=0,bias_d_h=0,strikes_p_h=10, 
                   strikes_d_h=10, total_strikes_h=28,num_cog_h=14)

d21_10=data_create(reps=1000,bias_p_c=1,bias_d_c=0,strikes_p_c=10, strikes_d_c=10, 
                   total_strikes_c=28,num_cog_c=14,
                   n_t=1,bias_p_h=.5,bias_d_h=0,strikes_p_h=10, 
                   strikes_d_h=10, total_strikes_h=28,num_cog_h=14)

d31_10=data_create(reps=1000,bias_p_c=1,bias_d_c=0,strikes_p_c=10, strikes_d_c=10, 
                   total_strikes_c=28,num_cog_c=14,
                   n_t=1,bias_p_h=1,bias_d_h=0,strikes_p_h=10, 
                   strikes_d_h=10, total_strikes_h=28,num_cog_h=14)

d41_10=data_create(reps=1000,bias_p_c=1,bias_d_c=0,strikes_p_c=10, strikes_d_c=10, 
                   total_strikes_c=28,num_cog_c=14,
                   n_t=1,bias_p_h=1.5,bias_d_h=0,strikes_p_h=10, 
                   strikes_d_h=10, total_strikes_h=28,num_cog_h=14)

d51_10=data_create(reps=1000,bias_p_c=1,bias_d_c=0,strikes_p_c=10, strikes_d_c=10, 
                   total_strikes_c=28,num_cog_c=14,
                   n_t=1,bias_p_h=2,bias_d_h=0,strikes_p_h=10, 
                   strikes_d_h=10, total_strikes_h=28,num_cog_h=14)

d61_10=data_create(reps=1000,bias_p_c=1,bias_d_c=0,strikes_p_c=10, strikes_d_c=10, 
                   total_strikes_c=28,num_cog_c=14,
                   n_t=1,bias_p_h=2.5,bias_d_h=0,strikes_p_h=10, 
                   strikes_d_h=10, total_strikes_h=28,num_cog_h=14)

d71_10=data_create(reps=1000,bias_p_c=1,bias_d_c=0,strikes_p_c=10, strikes_d_c=10, 
                   total_strikes_c=28,num_cog_c=14,
                   n_t=1,bias_p_h=3,bias_d_h=0,strikes_p_h=10, 
                   strikes_d_h=10, total_strikes_h=28,num_cog_h=14)



d12_10=data_create(reps=1000,bias_p_c=1,bias_d_c=0,strikes_p_c=10, strikes_d_c=10, 
                   total_strikes_c=28,num_cog_c=14,
                   n_t=2,bias_p_h=0,bias_d_h=0,strikes_p_h=10, 
                   strikes_d_h=10, total_strikes_h=28,num_cog_h=14)

d22_10=data_create(reps=1000,bias_p_c=1,bias_d_c=0,strikes_p_c=10, strikes_d_c=10, 
                   total_strikes_c=28,num_cog_c=14,
                   n_t=2,bias_p_h=.5,bias_d_h=0,strikes_p_h=10, 
                   strikes_d_h=10, total_strikes_h=28,num_cog_h=14)

d32_10=data_create(reps=1000,bias_p_c=1,bias_d_c=0,strikes_p_c=10, strikes_d_c=10, 
                   total_strikes_c=28,num_cog_c=14,
                   n_t=2,bias_p_h=1,bias_d_h=0,strikes_p_h=10, 
                   strikes_d_h=10, total_strikes_h=28,num_cog_h=14)

d42_10=data_create(reps=1000,bias_p_c=1,bias_d_c=0,strikes_p_c=10, strikes_d_c=10, 
                   total_strikes_c=28,num_cog_c=14,
                   n_t=2,bias_p_h=1.5,bias_d_h=0,strikes_p_h=10, 
                   strikes_d_h=10, total_strikes_h=28,num_cog_h=14)

d52_10=data_create(reps=1000,bias_p_c=1,bias_d_c=0,strikes_p_c=10, strikes_d_c=10, 
                   total_strikes_c=28,num_cog_c=14,
                   n_t=2,bias_p_h=2,bias_d_h=0,strikes_p_h=10, 
                   strikes_d_h=10, total_strikes_h=28,num_cog_h=14)

d62_10=data_create(reps=1000,bias_p_c=1,bias_d_c=0,strikes_p_c=10, strikes_d_c=10, 
                   total_strikes_c=28,num_cog_c=14,
                   n_t=2,bias_p_h=2.5,bias_d_h=0,strikes_p_h=10, 
                   strikes_d_h=10, total_strikes_h=28,num_cog_h=14)

d72_10=data_create(reps=1000,bias_p_c=1,bias_d_c=0,strikes_p_c=10, strikes_d_c=10, 
                   total_strikes_c=28,num_cog_c=14,
                   n_t=2,bias_p_h=3,bias_d_h=0,strikes_p_h=10, 
                   strikes_d_h=10, total_strikes_h=28,num_cog_h=14)


d13_10=data_create(reps=1000,bias_p_c=1,bias_d_c=0,strikes_p_c=10, strikes_d_c=10, 
                   total_strikes_c=28,num_cog_c=14,
                   n_t=3,bias_p_h=0,bias_d_h=0,strikes_p_h=10, 
                   strikes_d_h=10, total_strikes_h=28,num_cog_h=14)

d23_10=data_create(reps=1000,bias_p_c=1,bias_d_c=0,strikes_p_c=10, strikes_d_c=10, 
                   total_strikes_c=28,num_cog_c=14,
                   n_t=3,bias_p_h=.5,bias_d_h=0,strikes_p_h=10, 
                   strikes_d_h=10, total_strikes_h=28,num_cog_h=14)

d33_10=data_create(reps=1000,bias_p_c=1,bias_d_c=0,strikes_p_c=10, strikes_d_c=10, 
                   total_strikes_c=28,num_cog_c=14,
                   n_t=3,bias_p_h=1,bias_d_h=0,strikes_p_h=10, 
                   strikes_d_h=10, total_strikes_h=28,num_cog_h=14)

d43_10=data_create(reps=1000,bias_p_c=1,bias_d_c=0,strikes_p_c=10, strikes_d_c=10, 
                   total_strikes_c=28,num_cog_c=14,
                   n_t=3,bias_p_h=1.5,bias_d_h=0,strikes_p_h=10, 
                   strikes_d_h=10, total_strikes_h=28,num_cog_h=14)

d53_10=data_create(reps=1000,bias_p_c=1,bias_d_c=0,strikes_p_c=10, strikes_d_c=10, 
                   total_strikes_c=28,num_cog_c=14,
                   n_t=3,bias_p_h=2,bias_d_h=0,strikes_p_h=10, 
                   strikes_d_h=10, total_strikes_h=28,num_cog_h=14)

d63_10=data_create(reps=1000,bias_p_c=1,bias_d_c=0,strikes_p_c=10, strikes_d_c=10, 
                   total_strikes_c=28,num_cog_c=14,
                   n_t=3,bias_p_h=2.5,bias_d_h=0,strikes_p_h=10, 
                   strikes_d_h=10, total_strikes_h=28,num_cog_h=14)

d73_10=data_create(reps=1000,bias_p_c=1,bias_d_c=0,strikes_p_c=10, strikes_d_c=10, 
                   total_strikes_c=28,num_cog_c=14,
                   n_t=3,bias_p_h=3,bias_d_h=0,strikes_p_h=10, 
                   strikes_d_h=10, total_strikes_h=28,num_cog_h=14)



d11_15=data_create(reps=1000,bias_p_c=1,bias_d_c=0,strikes_p_c=15, strikes_d_c=15, 
                   total_strikes_c=30,num_cog_c=15,
                   n_t=1,bias_p_h=0,bias_d_h=0,strikes_p_h=15, 
                   strikes_d_h=15, total_strikes_h=30,num_cog_h=15)

d21_15=data_create(reps=1000,bias_p_c=1,bias_d_c=0,strikes_p_c=15, strikes_d_c=15, 
                   total_strikes_c=30,num_cog_c=15,
                   n_t=1,bias_p_h=.5,bias_d_h=0,strikes_p_h=15, 
                   strikes_d_h=15, total_strikes_h=30,num_cog_h=15)

d31_15=data_create(reps=1000,bias_p_c=1,bias_d_c=0,strikes_p_c=15, strikes_d_c=15, 
                   total_strikes_c=30,num_cog_c=15,
                   n_t=1,bias_p_h=1,bias_d_h=0,strikes_p_h=15, 
                   strikes_d_h=15, total_strikes_h=30,num_cog_h=15)

d41_15=data_create(reps=1000,bias_p_c=1,bias_d_c=0,strikes_p_c=15, strikes_d_c=15, 
                   total_strikes_c=30,num_cog_c=15,
                   n_t=1,bias_p_h=1.5,bias_d_h=0,strikes_p_h=15, 
                   strikes_d_h=15, total_strikes_h=30,num_cog_h=15)

d51_15=data_create(reps=1000,bias_p_c=1,bias_d_c=0,strikes_p_c=15, strikes_d_c=15, 
                   total_strikes_c=30,num_cog_c=15,
                   n_t=1,bias_p_h=2,bias_d_h=0,strikes_p_h=15, 
                   strikes_d_h=15, total_strikes_h=30,num_cog_h=15)

d61_15=data_create(reps=1000,bias_p_c=1,bias_d_c=0,strikes_p_c=15, strikes_d_c=15, 
                   total_strikes_c=30,num_cog_c=15,
                   n_t=1,bias_p_h=2.5,bias_d_h=0,strikes_p_h=15, 
                   strikes_d_h=15, total_strikes_h=30,num_cog_h=15)

d71_15=data_create(reps=1000,bias_p_c=1,bias_d_c=0,strikes_p_c=15, strikes_d_c=15, 
                   total_strikes_c=30,num_cog_c=15,
                   n_t=1,bias_p_h=3,bias_d_h=0,strikes_p_h=15, 
                   strikes_d_h=15, total_strikes_h=30,num_cog_h=15)



d12_15=data_create(reps=1000,bias_p_c=1,bias_d_c=0,strikes_p_c=15, strikes_d_c=15, 
                   total_strikes_c=30,num_cog_c=15,
                   n_t=2,bias_p_h=0,bias_d_h=0,strikes_p_h=15, 
                   strikes_d_h=15, total_strikes_h=30,num_cog_h=15)

d22_15=data_create(reps=1000,bias_p_c=1,bias_d_c=0,strikes_p_c=15, strikes_d_c=15, 
                   total_strikes_c=30,num_cog_c=15,
                   n_t=2,bias_p_h=.5,bias_d_h=0,strikes_p_h=15, 
                   strikes_d_h=15, total_strikes_h=30,num_cog_h=15)

d32_15=data_create(reps=1000,bias_p_c=1,bias_d_c=0,strikes_p_c=15, strikes_d_c=15, 
                   total_strikes_c=30,num_cog_c=15,
                   n_t=2,bias_p_h=1,bias_d_h=0,strikes_p_h=15, 
                   strikes_d_h=15, total_strikes_h=30,num_cog_h=15)

d42_15=data_create(reps=1000,bias_p_c=1,bias_d_c=0,strikes_p_c=15, strikes_d_c=15, 
                   total_strikes_c=30,num_cog_c=15,
                   n_t=2,bias_p_h=1.5,bias_d_h=0,strikes_p_h=15, 
                   strikes_d_h=15, total_strikes_h=30,num_cog_h=15)

d52_15=data_create(reps=1000,bias_p_c=1,bias_d_c=0,strikes_p_c=15, strikes_d_c=15, 
                   total_strikes_c=30,num_cog_c=15,
                   n_t=2,bias_p_h=2,bias_d_h=0,strikes_p_h=15, 
                   strikes_d_h=15, total_strikes_h=30,num_cog_h=15)

d62_15=data_create(reps=1000,bias_p_c=1,bias_d_c=0,strikes_p_c=15, strikes_d_c=15, 
                   total_strikes_c=30,num_cog_c=15,
                   n_t=2,bias_p_h=2.5,bias_d_h=0,strikes_p_h=15, 
                   strikes_d_h=15, total_strikes_h=30,num_cog_h=15)

d72_15=data_create(reps=1000,bias_p_c=1,bias_d_c=0,strikes_p_c=15, strikes_d_c=15, 
                   total_strikes_c=30,num_cog_c=15,
                   n_t=2,bias_p_h=3,bias_d_h=0,strikes_p_h=15, 
                   strikes_d_h=15, total_strikes_h=30,num_cog_h=15)


d13_15=data_create(reps=1000,bias_p_c=1,bias_d_c=0,strikes_p_c=15, strikes_d_c=15, 
                   total_strikes_c=30,num_cog_c=15,
                   n_t=3,bias_p_h=0,bias_d_h=0,strikes_p_h=15, 
                   strikes_d_h=15, total_strikes_h=30,num_cog_h=15)

d23_15=data_create(reps=1000,bias_p_c=1,bias_d_c=0,strikes_p_c=15, strikes_d_c=15, 
                   total_strikes_c=30,num_cog_c=15,
                   n_t=3,bias_p_h=.5,bias_d_h=0,strikes_p_h=15, 
                   strikes_d_h=15, total_strikes_h=30,num_cog_h=15)

d33_15=data_create(reps=1000,bias_p_c=1,bias_d_c=0,strikes_p_c=15, strikes_d_c=15, 
                   total_strikes_c=30,num_cog_c=15,
                   n_t=3,bias_p_h=1,bias_d_h=0,strikes_p_h=15, 
                   strikes_d_h=15, total_strikes_h=30,num_cog_h=15)

d43_15=data_create(reps=1000,bias_p_c=1,bias_d_c=0,strikes_p_c=15, strikes_d_c=15, 
                   total_strikes_c=30,num_cog_c=15,
                   n_t=3,bias_p_h=1.5,bias_d_h=0,strikes_p_h=15, 
                   strikes_d_h=15, total_strikes_h=30,num_cog_h=15)

d53_15=data_create(reps=1000,bias_p_c=1,bias_d_c=0,strikes_p_c=15, strikes_d_c=15, 
                   total_strikes_c=30,num_cog_c=15,
                   n_t=3,bias_p_h=2,bias_d_h=0,strikes_p_h=15, 
                   strikes_d_h=15, total_strikes_h=30,num_cog_h=15)

d63_15=data_create(reps=1000,bias_p_c=1,bias_d_c=0,strikes_p_c=15, strikes_d_c=15, 
                   total_strikes_c=30,num_cog_c=15,
                   n_t=3,bias_p_h=2.5,bias_d_h=0,strikes_p_h=15, 
                   strikes_d_h=15, total_strikes_h=30,num_cog_h=15)

d73_15=data_create(reps=1000,bias_p_c=1,bias_d_c=0,strikes_p_c=15, strikes_d_c=15, 
                   total_strikes_c=30,num_cog_c=15,
                   n_t=3,bias_p_h=3,bias_d_h=0,strikes_p_h=15, 
                   strikes_d_h=15, total_strikes_h=30,num_cog_h=15)

save.image("/dats1.RData")




#bias_p_c=1.5
d11_6=data_create(reps=1000,bias_p_c=1.5,bias_d_c=0,strikes_p_c=6, strikes_d_c=6, 
                  total_strikes_c=28,num_cog_c=14,
                  n_t=1,bias_p_h=0,bias_d_h=0,strikes_p_h=6, 
                  strikes_d_h=6, total_strikes_h=28,num_cog_h=14)

d21_6=data_create(reps=1000,bias_p_c=1.5,bias_d_c=0,strikes_p_c=6, strikes_d_c=6, 
                  total_strikes_c=28,num_cog_c=14,
                  n_t=1,bias_p_h=.5,bias_d_h=0,strikes_p_h=6, 
                  strikes_d_h=6, total_strikes_h=28,num_cog_h=14)

d31_6=data_create(reps=1000,bias_p_c=1.5,bias_d_c=0,strikes_p_c=6, strikes_d_c=6, 
                  total_strikes_c=28,num_cog_c=14,
                  n_t=1,bias_p_h=1,bias_d_h=0,strikes_p_h=6, 
                  strikes_d_h=6, total_strikes_h=28,num_cog_h=14)

d41_6=data_create(reps=1000,bias_p_c=1.5,bias_d_c=0,strikes_p_c=6, strikes_d_c=6, 
                  total_strikes_c=28,num_cog_c=14,
                  n_t=1,bias_p_h=1.5,bias_d_h=0,strikes_p_h=6, 
                  strikes_d_h=6, total_strikes_h=28,num_cog_h=14)

d51_6=data_create(reps=1000,bias_p_c=1.5,bias_d_c=0,strikes_p_c=6, strikes_d_c=6, 
                  total_strikes_c=28,num_cog_c=14,
                  n_t=1,bias_p_h=2,bias_d_h=0,strikes_p_h=6, 
                  strikes_d_h=6, total_strikes_h=28,num_cog_h=14)

d61_6=data_create(reps=1000,bias_p_c=1.5,bias_d_c=0,strikes_p_c=6, strikes_d_c=6, 
                  total_strikes_c=28,num_cog_c=14,
                  n_t=1,bias_p_h=2.5,bias_d_h=0,strikes_p_h=6, 
                  strikes_d_h=6, total_strikes_h=28,num_cog_h=14)

d71_6=data_create(reps=1000,bias_p_c=1.5,bias_d_c=0,strikes_p_c=6, strikes_d_c=6, 
                  total_strikes_c=28,num_cog_c=14,
                  n_t=1,bias_p_h=3,bias_d_h=0,strikes_p_h=6, 
                  strikes_d_h=6, total_strikes_h=28,num_cog_h=14)



d12_6=data_create(reps=1000,bias_p_c=1.5,bias_d_c=0,strikes_p_c=6, strikes_d_c=6, 
                  total_strikes_c=28,num_cog_c=14,
                  n_t=2,bias_p_h=0,bias_d_h=0,strikes_p_h=6, 
                  strikes_d_h=6, total_strikes_h=28,num_cog_h=14)

d22_6=data_create(reps=1000,bias_p_c=1.5,bias_d_c=0,strikes_p_c=6, strikes_d_c=6, 
                  total_strikes_c=28,num_cog_c=14,
                  n_t=2,bias_p_h=.5,bias_d_h=0,strikes_p_h=6, 
                  strikes_d_h=6, total_strikes_h=28,num_cog_h=14)

d32_6=data_create(reps=1000,bias_p_c=1.5,bias_d_c=0,strikes_p_c=6, strikes_d_c=6, 
                  total_strikes_c=28,num_cog_c=14,
                  n_t=2,bias_p_h=1,bias_d_h=0,strikes_p_h=6, 
                  strikes_d_h=6, total_strikes_h=28,num_cog_h=14)

d42_6=data_create(reps=1000,bias_p_c=1.5,bias_d_c=0,strikes_p_c=6, strikes_d_c=6, 
                  total_strikes_c=28,num_cog_c=14,
                  n_t=2,bias_p_h=1.5,bias_d_h=0,strikes_p_h=6, 
                  strikes_d_h=6, total_strikes_h=28,num_cog_h=14)

d52_6=data_create(reps=1000,bias_p_c=1.5,bias_d_c=0,strikes_p_c=6, strikes_d_c=6, 
                  total_strikes_c=28,num_cog_c=14,
                  n_t=2,bias_p_h=2,bias_d_h=0,strikes_p_h=6, 
                  strikes_d_h=6, total_strikes_h=28,num_cog_h=14)

d62_6=data_create(reps=1000,bias_p_c=1.5,bias_d_c=0,strikes_p_c=6, strikes_d_c=6, 
                  total_strikes_c=28,num_cog_c=14,
                  n_t=2,bias_p_h=2.5,bias_d_h=0,strikes_p_h=6, 
                  strikes_d_h=6, total_strikes_h=28,num_cog_h=14)

d72_6=data_create(reps=1000,bias_p_c=1.5,bias_d_c=0,strikes_p_c=6, strikes_d_c=6, 
                  total_strikes_c=28,num_cog_c=14,
                  n_t=2,bias_p_h=3,bias_d_h=0,strikes_p_h=6, 
                  strikes_d_h=6, total_strikes_h=28,num_cog_h=14)


d13_6=data_create(reps=1000,bias_p_c=1.5,bias_d_c=0,strikes_p_c=6, strikes_d_c=6, 
                  total_strikes_c=28,num_cog_c=14,
                  n_t=3,bias_p_h=0,bias_d_h=0,strikes_p_h=6, 
                  strikes_d_h=6, total_strikes_h=28,num_cog_h=14)

d23_6=data_create(reps=1000,bias_p_c=1.5,bias_d_c=0,strikes_p_c=6, strikes_d_c=6, 
                  total_strikes_c=28,num_cog_c=14,
                  n_t=3,bias_p_h=.5,bias_d_h=0,strikes_p_h=6, 
                  strikes_d_h=6, total_strikes_h=28,num_cog_h=14)

d33_6=data_create(reps=1000,bias_p_c=1.5,bias_d_c=0,strikes_p_c=6, strikes_d_c=6, 
                  total_strikes_c=28,num_cog_c=14,
                  n_t=3,bias_p_h=1,bias_d_h=0,strikes_p_h=6, 
                  strikes_d_h=6, total_strikes_h=28,num_cog_h=14)

d43_6=data_create(reps=1000,bias_p_c=1.5,bias_d_c=0,strikes_p_c=6, strikes_d_c=6, 
                  total_strikes_c=28,num_cog_c=14,
                  n_t=3,bias_p_h=1.5,bias_d_h=0,strikes_p_h=6, 
                  strikes_d_h=6, total_strikes_h=28,num_cog_h=14)

d53_6=data_create(reps=1000,bias_p_c=1.5,bias_d_c=0,strikes_p_c=6, strikes_d_c=6, 
                  total_strikes_c=28,num_cog_c=14,
                  n_t=3,bias_p_h=2,bias_d_h=0,strikes_p_h=6, 
                  strikes_d_h=6, total_strikes_h=28,num_cog_h=14)

d63_6=data_create(reps=1000,bias_p_c=1.5,bias_d_c=0,strikes_p_c=6, strikes_d_c=6, 
                  total_strikes_c=28,num_cog_c=14,
                  n_t=3,bias_p_h=2.5,bias_d_h=0,strikes_p_h=6, 
                  strikes_d_h=6, total_strikes_h=28,num_cog_h=14)

d73_6=data_create(reps=1000,bias_p_c=1.5,bias_d_c=0,strikes_p_c=6, strikes_d_c=6, 
                  total_strikes_c=28,num_cog_c=14,
                  n_t=3,bias_p_h=3,bias_d_h=0,strikes_p_h=6, 
                  strikes_d_h=6, total_strikes_h=28,num_cog_h=14)

d11_10=data_create(reps=1000,bias_p_c=1.5,bias_d_c=0,strikes_p_c=10, strikes_d_c=10, 
                   total_strikes_c=28,num_cog_c=14,
                   n_t=1,bias_p_h=0,bias_d_h=0,strikes_p_h=10, 
                   strikes_d_h=10, total_strikes_h=28,num_cog_h=14)

d21_10=data_create(reps=1000,bias_p_c=1.5,bias_d_c=0,strikes_p_c=10, strikes_d_c=10, 
                   total_strikes_c=28,num_cog_c=14,
                   n_t=1,bias_p_h=.5,bias_d_h=0,strikes_p_h=10, 
                   strikes_d_h=10, total_strikes_h=28,num_cog_h=14)

d31_10=data_create(reps=1000,bias_p_c=1.5,bias_d_c=0,strikes_p_c=10, strikes_d_c=10, 
                   total_strikes_c=28,num_cog_c=14,
                   n_t=1,bias_p_h=1,bias_d_h=0,strikes_p_h=10, 
                   strikes_d_h=10, total_strikes_h=28,num_cog_h=14)

d41_10=data_create(reps=1000,bias_p_c=1.5,bias_d_c=0,strikes_p_c=10, strikes_d_c=10, 
                   total_strikes_c=28,num_cog_c=14,
                   n_t=1,bias_p_h=1.5,bias_d_h=0,strikes_p_h=10, 
                   strikes_d_h=10, total_strikes_h=28,num_cog_h=14)

d51_10=data_create(reps=1000,bias_p_c=1.5,bias_d_c=0,strikes_p_c=10, strikes_d_c=10, 
                   total_strikes_c=28,num_cog_c=14,
                   n_t=1,bias_p_h=2,bias_d_h=0,strikes_p_h=10, 
                   strikes_d_h=10, total_strikes_h=28,num_cog_h=14)

d61_10=data_create(reps=1000,bias_p_c=1.5,bias_d_c=0,strikes_p_c=10, strikes_d_c=10, 
                   total_strikes_c=28,num_cog_c=14,
                   n_t=1,bias_p_h=2.5,bias_d_h=0,strikes_p_h=10, 
                   strikes_d_h=10, total_strikes_h=28,num_cog_h=14)

d71_10=data_create(reps=1000,bias_p_c=1.5,bias_d_c=0,strikes_p_c=10, strikes_d_c=10, 
                   total_strikes_c=28,num_cog_c=14,
                   n_t=1,bias_p_h=3,bias_d_h=0,strikes_p_h=10, 
                   strikes_d_h=10, total_strikes_h=28,num_cog_h=14)



d12_10=data_create(reps=1000,bias_p_c=1.5,bias_d_c=0,strikes_p_c=10, strikes_d_c=10, 
                   total_strikes_c=28,num_cog_c=14,
                   n_t=2,bias_p_h=0,bias_d_h=0,strikes_p_h=10, 
                   strikes_d_h=10, total_strikes_h=28,num_cog_h=14)

d22_10=data_create(reps=1000,bias_p_c=1.5,bias_d_c=0,strikes_p_c=10, strikes_d_c=10, 
                   total_strikes_c=28,num_cog_c=14,
                   n_t=2,bias_p_h=.5,bias_d_h=0,strikes_p_h=10, 
                   strikes_d_h=10, total_strikes_h=28,num_cog_h=14)

d32_10=data_create(reps=1000,bias_p_c=1.5,bias_d_c=0,strikes_p_c=10, strikes_d_c=10, 
                   total_strikes_c=28,num_cog_c=14,
                   n_t=2,bias_p_h=1,bias_d_h=0,strikes_p_h=10, 
                   strikes_d_h=10, total_strikes_h=28,num_cog_h=14)

d42_10=data_create(reps=1000,bias_p_c=1.5,bias_d_c=0,strikes_p_c=10, strikes_d_c=10, 
                   total_strikes_c=28,num_cog_c=14,
                   n_t=2,bias_p_h=1.5,bias_d_h=0,strikes_p_h=10, 
                   strikes_d_h=10, total_strikes_h=28,num_cog_h=14)

d52_10=data_create(reps=1000,bias_p_c=1.5,bias_d_c=0,strikes_p_c=10, strikes_d_c=10, 
                   total_strikes_c=28,num_cog_c=14,
                   n_t=2,bias_p_h=2,bias_d_h=0,strikes_p_h=10, 
                   strikes_d_h=10, total_strikes_h=28,num_cog_h=14)

d62_10=data_create(reps=1000,bias_p_c=1.5,bias_d_c=0,strikes_p_c=10, strikes_d_c=10, 
                   total_strikes_c=28,num_cog_c=14,
                   n_t=2,bias_p_h=2.5,bias_d_h=0,strikes_p_h=10, 
                   strikes_d_h=10, total_strikes_h=28,num_cog_h=14)

d72_10=data_create(reps=1000,bias_p_c=1.5,bias_d_c=0,strikes_p_c=10, strikes_d_c=10, 
                   total_strikes_c=28,num_cog_c=14,
                   n_t=2,bias_p_h=3,bias_d_h=0,strikes_p_h=10, 
                   strikes_d_h=10, total_strikes_h=28,num_cog_h=14)


d13_10=data_create(reps=1000,bias_p_c=1.5,bias_d_c=0,strikes_p_c=10, strikes_d_c=10, 
                   total_strikes_c=28,num_cog_c=14,
                   n_t=3,bias_p_h=0,bias_d_h=0,strikes_p_h=10, 
                   strikes_d_h=10, total_strikes_h=28,num_cog_h=14)

d23_10=data_create(reps=1000,bias_p_c=1.5,bias_d_c=0,strikes_p_c=10, strikes_d_c=10, 
                   total_strikes_c=28,num_cog_c=14,
                   n_t=3,bias_p_h=.5,bias_d_h=0,strikes_p_h=10, 
                   strikes_d_h=10, total_strikes_h=28,num_cog_h=14)

d33_10=data_create(reps=1000,bias_p_c=1.5,bias_d_c=0,strikes_p_c=10, strikes_d_c=10, 
                   total_strikes_c=28,num_cog_c=14,
                   n_t=3,bias_p_h=1,bias_d_h=0,strikes_p_h=10, 
                   strikes_d_h=10, total_strikes_h=28,num_cog_h=14)

d43_10=data_create(reps=1000,bias_p_c=1.5,bias_d_c=0,strikes_p_c=10, strikes_d_c=10, 
                   total_strikes_c=28,num_cog_c=14,
                   n_t=3,bias_p_h=1.5,bias_d_h=0,strikes_p_h=10, 
                   strikes_d_h=10, total_strikes_h=28,num_cog_h=14)

d53_10=data_create(reps=1000,bias_p_c=1.5,bias_d_c=0,strikes_p_c=10, strikes_d_c=10, 
                   total_strikes_c=28,num_cog_c=14,
                   n_t=3,bias_p_h=2,bias_d_h=0,strikes_p_h=10, 
                   strikes_d_h=10, total_strikes_h=28,num_cog_h=14)

d63_10=data_create(reps=1000,bias_p_c=1.5,bias_d_c=0,strikes_p_c=10, strikes_d_c=10, 
                   total_strikes_c=28,num_cog_c=14,
                   n_t=3,bias_p_h=2.5,bias_d_h=0,strikes_p_h=10, 
                   strikes_d_h=10, total_strikes_h=28,num_cog_h=14)

d73_10=data_create(reps=1000,bias_p_c=1.5,bias_d_c=0,strikes_p_c=10, strikes_d_c=10, 
                   total_strikes_c=28,num_cog_c=14,
                   n_t=3,bias_p_h=3,bias_d_h=0,strikes_p_h=10, 
                   strikes_d_h=10, total_strikes_h=28,num_cog_h=14)



d11_15=data_create(reps=1000,bias_p_c=1.5,bias_d_c=0,strikes_p_c=15, strikes_d_c=15, 
                   total_strikes_c=30,num_cog_c=15,
                   n_t=1,bias_p_h=0,bias_d_h=0,strikes_p_h=15, 
                   strikes_d_h=15, total_strikes_h=30,num_cog_h=15)

d21_15=data_create(reps=1000,bias_p_c=1.5,bias_d_c=0,strikes_p_c=15, strikes_d_c=15, 
                   total_strikes_c=30,num_cog_c=15,
                   n_t=1,bias_p_h=.5,bias_d_h=0,strikes_p_h=15, 
                   strikes_d_h=15, total_strikes_h=30,num_cog_h=15)

d31_15=data_create(reps=1000,bias_p_c=1.5,bias_d_c=0,strikes_p_c=15, strikes_d_c=15, 
                   total_strikes_c=30,num_cog_c=15,
                   n_t=1,bias_p_h=1,bias_d_h=0,strikes_p_h=15, 
                   strikes_d_h=15, total_strikes_h=30,num_cog_h=15)

d41_15=data_create(reps=1000,bias_p_c=1.5,bias_d_c=0,strikes_p_c=15, strikes_d_c=15, 
                   total_strikes_c=30,num_cog_c=15,
                   n_t=1,bias_p_h=1.5,bias_d_h=0,strikes_p_h=15, 
                   strikes_d_h=15, total_strikes_h=30,num_cog_h=15)

d51_15=data_create(reps=1000,bias_p_c=1.5,bias_d_c=0,strikes_p_c=15, strikes_d_c=15, 
                   total_strikes_c=30,num_cog_c=15,
                   n_t=1,bias_p_h=2,bias_d_h=0,strikes_p_h=15, 
                   strikes_d_h=15, total_strikes_h=30,num_cog_h=15)

d61_15=data_create(reps=1000,bias_p_c=1.5,bias_d_c=0,strikes_p_c=15, strikes_d_c=15, 
                   total_strikes_c=30,num_cog_c=15,
                   n_t=1,bias_p_h=2.5,bias_d_h=0,strikes_p_h=15, 
                   strikes_d_h=15, total_strikes_h=30,num_cog_h=15)

d71_15=data_create(reps=1000,bias_p_c=1.5,bias_d_c=0,strikes_p_c=15, strikes_d_c=15, 
                   total_strikes_c=30,num_cog_c=15,
                   n_t=1,bias_p_h=3,bias_d_h=0,strikes_p_h=15, 
                   strikes_d_h=15, total_strikes_h=30,num_cog_h=15)



d12_15=data_create(reps=1000,bias_p_c=1.5,bias_d_c=0,strikes_p_c=15, strikes_d_c=15, 
                   total_strikes_c=30,num_cog_c=15,
                   n_t=2,bias_p_h=0,bias_d_h=0,strikes_p_h=15, 
                   strikes_d_h=15, total_strikes_h=30,num_cog_h=15)

d22_15=data_create(reps=1000,bias_p_c=1.5,bias_d_c=0,strikes_p_c=15, strikes_d_c=15, 
                   total_strikes_c=30,num_cog_c=15,
                   n_t=2,bias_p_h=.5,bias_d_h=0,strikes_p_h=15, 
                   strikes_d_h=15, total_strikes_h=30,num_cog_h=15)

d32_15=data_create(reps=1000,bias_p_c=1.5,bias_d_c=0,strikes_p_c=15, strikes_d_c=15, 
                   total_strikes_c=30,num_cog_c=15,
                   n_t=2,bias_p_h=1,bias_d_h=0,strikes_p_h=15, 
                   strikes_d_h=15, total_strikes_h=30,num_cog_h=15)

d42_15=data_create(reps=1000,bias_p_c=1.5,bias_d_c=0,strikes_p_c=15, strikes_d_c=15, 
                   total_strikes_c=30,num_cog_c=15,
                   n_t=2,bias_p_h=1.5,bias_d_h=0,strikes_p_h=15, 
                   strikes_d_h=15, total_strikes_h=30,num_cog_h=15)

d52_15=data_create(reps=1000,bias_p_c=1.5,bias_d_c=0,strikes_p_c=15, strikes_d_c=15, 
                   total_strikes_c=30,num_cog_c=15,
                   n_t=2,bias_p_h=2,bias_d_h=0,strikes_p_h=15, 
                   strikes_d_h=15, total_strikes_h=30,num_cog_h=15)

d62_15=data_create(reps=1000,bias_p_c=1.5,bias_d_c=0,strikes_p_c=15, strikes_d_c=15, 
                   total_strikes_c=30,num_cog_c=15,
                   n_t=2,bias_p_h=2.5,bias_d_h=0,strikes_p_h=15, 
                   strikes_d_h=15, total_strikes_h=30,num_cog_h=15)

d72_15=data_create(reps=1000,bias_p_c=1.5,bias_d_c=0,strikes_p_c=15, strikes_d_c=15, 
                   total_strikes_c=30,num_cog_c=15,
                   n_t=2,bias_p_h=3,bias_d_h=0,strikes_p_h=15, 
                   strikes_d_h=15, total_strikes_h=30,num_cog_h=15)


d13_15=data_create(reps=1000,bias_p_c=1.5,bias_d_c=0,strikes_p_c=15, strikes_d_c=15, 
                   total_strikes_c=30,num_cog_c=15,
                   n_t=3,bias_p_h=0,bias_d_h=0,strikes_p_h=15, 
                   strikes_d_h=15, total_strikes_h=30,num_cog_h=15)

d23_15=data_create(reps=1000,bias_p_c=1.5,bias_d_c=0,strikes_p_c=15, strikes_d_c=15, 
                   total_strikes_c=30,num_cog_c=15,
                   n_t=3,bias_p_h=.5,bias_d_h=0,strikes_p_h=15, 
                   strikes_d_h=15, total_strikes_h=30,num_cog_h=15)

d33_15=data_create(reps=1000,bias_p_c=1.5,bias_d_c=0,strikes_p_c=15, strikes_d_c=15, 
                   total_strikes_c=30,num_cog_c=15,
                   n_t=3,bias_p_h=1,bias_d_h=0,strikes_p_h=15, 
                   strikes_d_h=15, total_strikes_h=30,num_cog_h=15)

d43_15=data_create(reps=1000,bias_p_c=1.5,bias_d_c=0,strikes_p_c=15, strikes_d_c=15, 
                   total_strikes_c=30,num_cog_c=15,
                   n_t=3,bias_p_h=1.5,bias_d_h=0,strikes_p_h=15, 
                   strikes_d_h=15, total_strikes_h=30,num_cog_h=15)

d53_15=data_create(reps=1000,bias_p_c=1.5,bias_d_c=0,strikes_p_c=15, strikes_d_c=15, 
                   total_strikes_c=30,num_cog_c=15,
                   n_t=3,bias_p_h=2,bias_d_h=0,strikes_p_h=15, 
                   strikes_d_h=15, total_strikes_h=30,num_cog_h=15)

d63_15=data_create(reps=1000,bias_p_c=1.5,bias_d_c=0,strikes_p_c=15, strikes_d_c=15, 
                   total_strikes_c=30,num_cog_c=15,
                   n_t=3,bias_p_h=2.5,bias_d_h=0,strikes_p_h=15, 
                   strikes_d_h=15, total_strikes_h=30,num_cog_h=15)

d73_15=data_create(reps=1000,bias_p_c=1.5,bias_d_c=0,strikes_p_c=15, strikes_d_c=15, 
                   total_strikes_c=30,num_cog_c=15,
                   n_t=3,bias_p_h=3,bias_d_h=0,strikes_p_h=15, 
                   strikes_d_h=15, total_strikes_h=30,num_cog_h=15)

save.image("/dats1.5.RData")



#bias_p_c=2
d11_6=data_create(reps=1000,bias_p_c=2,bias_d_c=0,strikes_p_c=6, strikes_d_c=6, 
                  total_strikes_c=28,num_cog_c=14,
                  n_t=1,bias_p_h=0,bias_d_h=0,strikes_p_h=6, 
                  strikes_d_h=6, total_strikes_h=28,num_cog_h=14)

d21_6=data_create(reps=1000,bias_p_c=2,bias_d_c=0,strikes_p_c=6, strikes_d_c=6, 
                  total_strikes_c=28,num_cog_c=14,
                  n_t=1,bias_p_h=.5,bias_d_h=0,strikes_p_h=6, 
                  strikes_d_h=6, total_strikes_h=28,num_cog_h=14)

d31_6=data_create(reps=1000,bias_p_c=2,bias_d_c=0,strikes_p_c=6, strikes_d_c=6, 
                  total_strikes_c=28,num_cog_c=14,
                  n_t=1,bias_p_h=1,bias_d_h=0,strikes_p_h=6, 
                  strikes_d_h=6, total_strikes_h=28,num_cog_h=14)

d41_6=data_create(reps=1000,bias_p_c=2,bias_d_c=0,strikes_p_c=6, strikes_d_c=6, 
                  total_strikes_c=28,num_cog_c=14,
                  n_t=1,bias_p_h=1.5,bias_d_h=0,strikes_p_h=6, 
                  strikes_d_h=6, total_strikes_h=28,num_cog_h=14)

d51_6=data_create(reps=1000,bias_p_c=2,bias_d_c=0,strikes_p_c=6, strikes_d_c=6, 
                  total_strikes_c=28,num_cog_c=14,
                  n_t=1,bias_p_h=2,bias_d_h=0,strikes_p_h=6, 
                  strikes_d_h=6, total_strikes_h=28,num_cog_h=14)

d61_6=data_create(reps=1000,bias_p_c=2,bias_d_c=0,strikes_p_c=6, strikes_d_c=6, 
                  total_strikes_c=28,num_cog_c=14,
                  n_t=1,bias_p_h=2.5,bias_d_h=0,strikes_p_h=6, 
                  strikes_d_h=6, total_strikes_h=28,num_cog_h=14)

d71_6=data_create(reps=1000,bias_p_c=2,bias_d_c=0,strikes_p_c=6, strikes_d_c=6, 
                  total_strikes_c=28,num_cog_c=14,
                  n_t=1,bias_p_h=3,bias_d_h=0,strikes_p_h=6, 
                  strikes_d_h=6, total_strikes_h=28,num_cog_h=14)



d12_6=data_create(reps=1000,bias_p_c=2,bias_d_c=0,strikes_p_c=6, strikes_d_c=6, 
                  total_strikes_c=28,num_cog_c=14,
                  n_t=2,bias_p_h=0,bias_d_h=0,strikes_p_h=6, 
                  strikes_d_h=6, total_strikes_h=28,num_cog_h=14)

d22_6=data_create(reps=1000,bias_p_c=2,bias_d_c=0,strikes_p_c=6, strikes_d_c=6, 
                  total_strikes_c=28,num_cog_c=14,
                  n_t=2,bias_p_h=.5,bias_d_h=0,strikes_p_h=6, 
                  strikes_d_h=6, total_strikes_h=28,num_cog_h=14)

d32_6=data_create(reps=1000,bias_p_c=2,bias_d_c=0,strikes_p_c=6, strikes_d_c=6, 
                  total_strikes_c=28,num_cog_c=14,
                  n_t=2,bias_p_h=1,bias_d_h=0,strikes_p_h=6, 
                  strikes_d_h=6, total_strikes_h=28,num_cog_h=14)

d42_6=data_create(reps=1000,bias_p_c=2,bias_d_c=0,strikes_p_c=6, strikes_d_c=6, 
                  total_strikes_c=28,num_cog_c=14,
                  n_t=2,bias_p_h=1.5,bias_d_h=0,strikes_p_h=6, 
                  strikes_d_h=6, total_strikes_h=28,num_cog_h=14)

d52_6=data_create(reps=1000,bias_p_c=2,bias_d_c=0,strikes_p_c=6, strikes_d_c=6, 
                  total_strikes_c=28,num_cog_c=14,
                  n_t=2,bias_p_h=2,bias_d_h=0,strikes_p_h=6, 
                  strikes_d_h=6, total_strikes_h=28,num_cog_h=14)

d62_6=data_create(reps=1000,bias_p_c=2,bias_d_c=0,strikes_p_c=6, strikes_d_c=6, 
                  total_strikes_c=28,num_cog_c=14,
                  n_t=2,bias_p_h=2.5,bias_d_h=0,strikes_p_h=6, 
                  strikes_d_h=6, total_strikes_h=28,num_cog_h=14)

d72_6=data_create(reps=1000,bias_p_c=2,bias_d_c=0,strikes_p_c=6, strikes_d_c=6, 
                  total_strikes_c=28,num_cog_c=14,
                  n_t=2,bias_p_h=3,bias_d_h=0,strikes_p_h=6, 
                  strikes_d_h=6, total_strikes_h=28,num_cog_h=14)


d13_6=data_create(reps=1000,bias_p_c=2,bias_d_c=0,strikes_p_c=6, strikes_d_c=6, 
                  total_strikes_c=28,num_cog_c=14,
                  n_t=3,bias_p_h=0,bias_d_h=0,strikes_p_h=6, 
                  strikes_d_h=6, total_strikes_h=28,num_cog_h=14)

d23_6=data_create(reps=1000,bias_p_c=2,bias_d_c=0,strikes_p_c=6, strikes_d_c=6, 
                  total_strikes_c=28,num_cog_c=14,
                  n_t=3,bias_p_h=.5,bias_d_h=0,strikes_p_h=6, 
                  strikes_d_h=6, total_strikes_h=28,num_cog_h=14)

d33_6=data_create(reps=1000,bias_p_c=2,bias_d_c=0,strikes_p_c=6, strikes_d_c=6, 
                  total_strikes_c=28,num_cog_c=14,
                  n_t=3,bias_p_h=1,bias_d_h=0,strikes_p_h=6, 
                  strikes_d_h=6, total_strikes_h=28,num_cog_h=14)

d43_6=data_create(reps=1000,bias_p_c=2,bias_d_c=0,strikes_p_c=6, strikes_d_c=6, 
                  total_strikes_c=28,num_cog_c=14,
                  n_t=3,bias_p_h=1.5,bias_d_h=0,strikes_p_h=6, 
                  strikes_d_h=6, total_strikes_h=28,num_cog_h=14)

d53_6=data_create(reps=1000,bias_p_c=2,bias_d_c=0,strikes_p_c=6, strikes_d_c=6, 
                  total_strikes_c=28,num_cog_c=14,
                  n_t=3,bias_p_h=2,bias_d_h=0,strikes_p_h=6, 
                  strikes_d_h=6, total_strikes_h=28,num_cog_h=14)

d63_6=data_create(reps=1000,bias_p_c=2,bias_d_c=0,strikes_p_c=6, strikes_d_c=6, 
                  total_strikes_c=28,num_cog_c=14,
                  n_t=3,bias_p_h=2.5,bias_d_h=0,strikes_p_h=6, 
                  strikes_d_h=6, total_strikes_h=28,num_cog_h=14)

d73_6=data_create(reps=1000,bias_p_c=2,bias_d_c=0,strikes_p_c=6, strikes_d_c=6, 
                  total_strikes_c=28,num_cog_c=14,
                  n_t=3,bias_p_h=3,bias_d_h=0,strikes_p_h=6, 
                  strikes_d_h=6, total_strikes_h=28,num_cog_h=14)

d11_10=data_create(reps=1000,bias_p_c=2,bias_d_c=0,strikes_p_c=10, strikes_d_c=10, 
                   total_strikes_c=28,num_cog_c=14,
                   n_t=1,bias_p_h=0,bias_d_h=0,strikes_p_h=10, 
                   strikes_d_h=10, total_strikes_h=28,num_cog_h=14)

d21_10=data_create(reps=1000,bias_p_c=2,bias_d_c=0,strikes_p_c=10, strikes_d_c=10, 
                   total_strikes_c=28,num_cog_c=14,
                   n_t=1,bias_p_h=.5,bias_d_h=0,strikes_p_h=10, 
                   strikes_d_h=10, total_strikes_h=28,num_cog_h=14)

d31_10=data_create(reps=1000,bias_p_c=2,bias_d_c=0,strikes_p_c=10, strikes_d_c=10, 
                   total_strikes_c=28,num_cog_c=14,
                   n_t=1,bias_p_h=1,bias_d_h=0,strikes_p_h=10, 
                   strikes_d_h=10, total_strikes_h=28,num_cog_h=14)

d41_10=data_create(reps=1000,bias_p_c=2,bias_d_c=0,strikes_p_c=10, strikes_d_c=10, 
                   total_strikes_c=28,num_cog_c=14,
                   n_t=1,bias_p_h=1.5,bias_d_h=0,strikes_p_h=10, 
                   strikes_d_h=10, total_strikes_h=28,num_cog_h=14)

d51_10=data_create(reps=1000,bias_p_c=2,bias_d_c=0,strikes_p_c=10, strikes_d_c=10, 
                   total_strikes_c=28,num_cog_c=14,
                   n_t=1,bias_p_h=2,bias_d_h=0,strikes_p_h=10, 
                   strikes_d_h=10, total_strikes_h=28,num_cog_h=14)

d61_10=data_create(reps=1000,bias_p_c=2,bias_d_c=0,strikes_p_c=10, strikes_d_c=10, 
                   total_strikes_c=28,num_cog_c=14,
                   n_t=1,bias_p_h=2.5,bias_d_h=0,strikes_p_h=10, 
                   strikes_d_h=10, total_strikes_h=28,num_cog_h=14)

d71_10=data_create(reps=1000,bias_p_c=2,bias_d_c=0,strikes_p_c=10, strikes_d_c=10, 
                   total_strikes_c=28,num_cog_c=14,
                   n_t=1,bias_p_h=3,bias_d_h=0,strikes_p_h=10, 
                   strikes_d_h=10, total_strikes_h=28,num_cog_h=14)



d12_10=data_create(reps=1000,bias_p_c=2,bias_d_c=0,strikes_p_c=10, strikes_d_c=10, 
                   total_strikes_c=28,num_cog_c=14,
                   n_t=2,bias_p_h=0,bias_d_h=0,strikes_p_h=10, 
                   strikes_d_h=10, total_strikes_h=28,num_cog_h=14)

d22_10=data_create(reps=1000,bias_p_c=2,bias_d_c=0,strikes_p_c=10, strikes_d_c=10, 
                   total_strikes_c=28,num_cog_c=14,
                   n_t=2,bias_p_h=.5,bias_d_h=0,strikes_p_h=10, 
                   strikes_d_h=10, total_strikes_h=28,num_cog_h=14)

d32_10=data_create(reps=1000,bias_p_c=2,bias_d_c=0,strikes_p_c=10, strikes_d_c=10, 
                   total_strikes_c=28,num_cog_c=14,
                   n_t=2,bias_p_h=1,bias_d_h=0,strikes_p_h=10, 
                   strikes_d_h=10, total_strikes_h=28,num_cog_h=14)

d42_10=data_create(reps=1000,bias_p_c=2,bias_d_c=0,strikes_p_c=10, strikes_d_c=10, 
                   total_strikes_c=28,num_cog_c=14,
                   n_t=2,bias_p_h=1.5,bias_d_h=0,strikes_p_h=10, 
                   strikes_d_h=10, total_strikes_h=28,num_cog_h=14)

d52_10=data_create(reps=1000,bias_p_c=2,bias_d_c=0,strikes_p_c=10, strikes_d_c=10, 
                   total_strikes_c=28,num_cog_c=14,
                   n_t=2,bias_p_h=2,bias_d_h=0,strikes_p_h=10, 
                   strikes_d_h=10, total_strikes_h=28,num_cog_h=14)

d62_10=data_create(reps=1000,bias_p_c=2,bias_d_c=0,strikes_p_c=10, strikes_d_c=10, 
                   total_strikes_c=28,num_cog_c=14,
                   n_t=2,bias_p_h=2.5,bias_d_h=0,strikes_p_h=10, 
                   strikes_d_h=10, total_strikes_h=28,num_cog_h=14)

d72_10=data_create(reps=1000,bias_p_c=2,bias_d_c=0,strikes_p_c=10, strikes_d_c=10, 
                   total_strikes_c=28,num_cog_c=14,
                   n_t=2,bias_p_h=3,bias_d_h=0,strikes_p_h=10, 
                   strikes_d_h=10, total_strikes_h=28,num_cog_h=14)


d13_10=data_create(reps=1000,bias_p_c=2,bias_d_c=0,strikes_p_c=10, strikes_d_c=10, 
                   total_strikes_c=28,num_cog_c=14,
                   n_t=3,bias_p_h=0,bias_d_h=0,strikes_p_h=10, 
                   strikes_d_h=10, total_strikes_h=28,num_cog_h=14)

d23_10=data_create(reps=1000,bias_p_c=2,bias_d_c=0,strikes_p_c=10, strikes_d_c=10, 
                   total_strikes_c=28,num_cog_c=14,
                   n_t=3,bias_p_h=.5,bias_d_h=0,strikes_p_h=10, 
                   strikes_d_h=10, total_strikes_h=28,num_cog_h=14)

d33_10=data_create(reps=1000,bias_p_c=2,bias_d_c=0,strikes_p_c=10, strikes_d_c=10, 
                   total_strikes_c=28,num_cog_c=14,
                   n_t=3,bias_p_h=1,bias_d_h=0,strikes_p_h=10, 
                   strikes_d_h=10, total_strikes_h=28,num_cog_h=14)

d43_10=data_create(reps=1000,bias_p_c=2,bias_d_c=0,strikes_p_c=10, strikes_d_c=10, 
                   total_strikes_c=28,num_cog_c=14,
                   n_t=3,bias_p_h=1.5,bias_d_h=0,strikes_p_h=10, 
                   strikes_d_h=10, total_strikes_h=28,num_cog_h=14)

d53_10=data_create(reps=1000,bias_p_c=2,bias_d_c=0,strikes_p_c=10, strikes_d_c=10, 
                   total_strikes_c=28,num_cog_c=14,
                   n_t=3,bias_p_h=2,bias_d_h=0,strikes_p_h=10, 
                   strikes_d_h=10, total_strikes_h=28,num_cog_h=14)

d63_10=data_create(reps=1000,bias_p_c=2,bias_d_c=0,strikes_p_c=10, strikes_d_c=10, 
                   total_strikes_c=28,num_cog_c=14,
                   n_t=3,bias_p_h=2.5,bias_d_h=0,strikes_p_h=10, 
                   strikes_d_h=10, total_strikes_h=28,num_cog_h=14)

d73_10=data_create(reps=1000,bias_p_c=2,bias_d_c=0,strikes_p_c=10, strikes_d_c=10, 
                   total_strikes_c=28,num_cog_c=14,
                   n_t=3,bias_p_h=3,bias_d_h=0,strikes_p_h=10, 
                   strikes_d_h=10, total_strikes_h=28,num_cog_h=14)



d11_15=data_create(reps=1000,bias_p_c=2,bias_d_c=0,strikes_p_c=15, strikes_d_c=15, 
                   total_strikes_c=30,num_cog_c=15,
                   n_t=1,bias_p_h=0,bias_d_h=0,strikes_p_h=15, 
                   strikes_d_h=15, total_strikes_h=30,num_cog_h=15)

d21_15=data_create(reps=1000,bias_p_c=2,bias_d_c=0,strikes_p_c=15, strikes_d_c=15, 
                   total_strikes_c=30,num_cog_c=15,
                   n_t=1,bias_p_h=.5,bias_d_h=0,strikes_p_h=15, 
                   strikes_d_h=15, total_strikes_h=30,num_cog_h=15)

d31_15=data_create(reps=1000,bias_p_c=2,bias_d_c=0,strikes_p_c=15, strikes_d_c=15, 
                   total_strikes_c=30,num_cog_c=15,
                   n_t=1,bias_p_h=1,bias_d_h=0,strikes_p_h=15, 
                   strikes_d_h=15, total_strikes_h=30,num_cog_h=15)

d41_15=data_create(reps=1000,bias_p_c=2,bias_d_c=0,strikes_p_c=15, strikes_d_c=15, 
                   total_strikes_c=30,num_cog_c=15,
                   n_t=1,bias_p_h=1.5,bias_d_h=0,strikes_p_h=15, 
                   strikes_d_h=15, total_strikes_h=30,num_cog_h=15)

d51_15=data_create(reps=1000,bias_p_c=2,bias_d_c=0,strikes_p_c=15, strikes_d_c=15, 
                   total_strikes_c=30,num_cog_c=15,
                   n_t=1,bias_p_h=2,bias_d_h=0,strikes_p_h=15, 
                   strikes_d_h=15, total_strikes_h=30,num_cog_h=15)

d61_15=data_create(reps=1000,bias_p_c=2,bias_d_c=0,strikes_p_c=15, strikes_d_c=15, 
                   total_strikes_c=30,num_cog_c=15,
                   n_t=1,bias_p_h=2.5,bias_d_h=0,strikes_p_h=15, 
                   strikes_d_h=15, total_strikes_h=30,num_cog_h=15)

d71_15=data_create(reps=1000,bias_p_c=2,bias_d_c=0,strikes_p_c=15, strikes_d_c=15, 
                   total_strikes_c=30,num_cog_c=15,
                   n_t=1,bias_p_h=3,bias_d_h=0,strikes_p_h=15, 
                   strikes_d_h=15, total_strikes_h=30,num_cog_h=15)



d12_15=data_create(reps=1000,bias_p_c=2,bias_d_c=0,strikes_p_c=15, strikes_d_c=15, 
                   total_strikes_c=30,num_cog_c=15,
                   n_t=2,bias_p_h=0,bias_d_h=0,strikes_p_h=15, 
                   strikes_d_h=15, total_strikes_h=30,num_cog_h=15)

d22_15=data_create(reps=1000,bias_p_c=2,bias_d_c=0,strikes_p_c=15, strikes_d_c=15, 
                   total_strikes_c=30,num_cog_c=15,
                   n_t=2,bias_p_h=.5,bias_d_h=0,strikes_p_h=15, 
                   strikes_d_h=15, total_strikes_h=30,num_cog_h=15)

d32_15=data_create(reps=1000,bias_p_c=2,bias_d_c=0,strikes_p_c=15, strikes_d_c=15, 
                   total_strikes_c=30,num_cog_c=15,
                   n_t=2,bias_p_h=1,bias_d_h=0,strikes_p_h=15, 
                   strikes_d_h=15, total_strikes_h=30,num_cog_h=15)

d42_15=data_create(reps=1000,bias_p_c=2,bias_d_c=0,strikes_p_c=15, strikes_d_c=15, 
                   total_strikes_c=30,num_cog_c=15,
                   n_t=2,bias_p_h=1.5,bias_d_h=0,strikes_p_h=15, 
                   strikes_d_h=15, total_strikes_h=30,num_cog_h=15)

d52_15=data_create(reps=1000,bias_p_c=2,bias_d_c=0,strikes_p_c=15, strikes_d_c=15, 
                   total_strikes_c=30,num_cog_c=15,
                   n_t=2,bias_p_h=2,bias_d_h=0,strikes_p_h=15, 
                   strikes_d_h=15, total_strikes_h=30,num_cog_h=15)

d62_15=data_create(reps=1000,bias_p_c=2,bias_d_c=0,strikes_p_c=15, strikes_d_c=15, 
                   total_strikes_c=30,num_cog_c=15,
                   n_t=2,bias_p_h=2.5,bias_d_h=0,strikes_p_h=15, 
                   strikes_d_h=15, total_strikes_h=30,num_cog_h=15)

d72_15=data_create(reps=1000,bias_p_c=2,bias_d_c=0,strikes_p_c=15, strikes_d_c=15, 
                   total_strikes_c=30,num_cog_c=15,
                   n_t=2,bias_p_h=3,bias_d_h=0,strikes_p_h=15, 
                   strikes_d_h=15, total_strikes_h=30,num_cog_h=15)


d13_15=data_create(reps=1000,bias_p_c=2,bias_d_c=0,strikes_p_c=15, strikes_d_c=15, 
                   total_strikes_c=30,num_cog_c=15,
                   n_t=3,bias_p_h=0,bias_d_h=0,strikes_p_h=15, 
                   strikes_d_h=15, total_strikes_h=30,num_cog_h=15)

d23_15=data_create(reps=1000,bias_p_c=2,bias_d_c=0,strikes_p_c=15, strikes_d_c=15, 
                   total_strikes_c=30,num_cog_c=15,
                   n_t=3,bias_p_h=.5,bias_d_h=0,strikes_p_h=15, 
                   strikes_d_h=15, total_strikes_h=30,num_cog_h=15)

d33_15=data_create(reps=1000,bias_p_c=2,bias_d_c=0,strikes_p_c=15, strikes_d_c=15, 
                   total_strikes_c=30,num_cog_c=15,
                   n_t=3,bias_p_h=1,bias_d_h=0,strikes_p_h=15, 
                   strikes_d_h=15, total_strikes_h=30,num_cog_h=15)

d43_15=data_create(reps=1000,bias_p_c=2,bias_d_c=0,strikes_p_c=15, strikes_d_c=15, 
                   total_strikes_c=30,num_cog_c=15,
                   n_t=3,bias_p_h=1.5,bias_d_h=0,strikes_p_h=15, 
                   strikes_d_h=15, total_strikes_h=30,num_cog_h=15)

d53_15=data_create(reps=1000,bias_p_c=2,bias_d_c=0,strikes_p_c=15, strikes_d_c=15, 
                   total_strikes_c=30,num_cog_c=15,
                   n_t=3,bias_p_h=2,bias_d_h=0,strikes_p_h=15, 
                   strikes_d_h=15, total_strikes_h=30,num_cog_h=15)

d63_15=data_create(reps=1000,bias_p_c=2,bias_d_c=0,strikes_p_c=15, strikes_d_c=15, 
                   total_strikes_c=30,num_cog_c=15,
                   n_t=3,bias_p_h=2.5,bias_d_h=0,strikes_p_h=15, 
                   strikes_d_h=15, total_strikes_h=30,num_cog_h=15)

d73_15=data_create(reps=1000,bias_p_c=2,bias_d_c=0,strikes_p_c=15, strikes_d_c=15, 
                   total_strikes_c=30,num_cog_c=15,
                   n_t=3,bias_p_h=3,bias_d_h=0,strikes_p_h=15, 
                   strikes_d_h=15, total_strikes_h=30,num_cog_h=15)

save.image("/dats2.RData")


#bias_p_c=2.5
d11_6=data_create(reps=1000,bias_p_c=2.5,bias_d_c=0,strikes_p_c=6, strikes_d_c=6, 
                  total_strikes_c=28,num_cog_c=14,
                  n_t=1,bias_p_h=0,bias_d_h=0,strikes_p_h=6, 
                  strikes_d_h=6, total_strikes_h=28,num_cog_h=14)

d21_6=data_create(reps=1000,bias_p_c=2.5,bias_d_c=0,strikes_p_c=6, strikes_d_c=6, 
                  total_strikes_c=28,num_cog_c=14,
                  n_t=1,bias_p_h=.5,bias_d_h=0,strikes_p_h=6, 
                  strikes_d_h=6, total_strikes_h=28,num_cog_h=14)

d31_6=data_create(reps=1000,bias_p_c=2.5,bias_d_c=0,strikes_p_c=6, strikes_d_c=6, 
                  total_strikes_c=28,num_cog_c=14,
                  n_t=1,bias_p_h=1,bias_d_h=0,strikes_p_h=6, 
                  strikes_d_h=6, total_strikes_h=28,num_cog_h=14)

d41_6=data_create(reps=1000,bias_p_c=2.5,bias_d_c=0,strikes_p_c=6, strikes_d_c=6, 
                  total_strikes_c=28,num_cog_c=14,
                  n_t=1,bias_p_h=1.5,bias_d_h=0,strikes_p_h=6, 
                  strikes_d_h=6, total_strikes_h=28,num_cog_h=14)

d51_6=data_create(reps=1000,bias_p_c=2.5,bias_d_c=0,strikes_p_c=6, strikes_d_c=6, 
                  total_strikes_c=28,num_cog_c=14,
                  n_t=1,bias_p_h=2,bias_d_h=0,strikes_p_h=6, 
                  strikes_d_h=6, total_strikes_h=28,num_cog_h=14)

d61_6=data_create(reps=1000,bias_p_c=2.5,bias_d_c=0,strikes_p_c=6, strikes_d_c=6, 
                  total_strikes_c=28,num_cog_c=14,
                  n_t=1,bias_p_h=2.5,bias_d_h=0,strikes_p_h=6, 
                  strikes_d_h=6, total_strikes_h=28,num_cog_h=14)

d71_6=data_create(reps=1000,bias_p_c=2.5,bias_d_c=0,strikes_p_c=6, strikes_d_c=6, 
                  total_strikes_c=28,num_cog_c=14,
                  n_t=1,bias_p_h=3,bias_d_h=0,strikes_p_h=6, 
                  strikes_d_h=6, total_strikes_h=28,num_cog_h=14)



d12_6=data_create(reps=1000,bias_p_c=2.5,bias_d_c=0,strikes_p_c=6, strikes_d_c=6, 
                  total_strikes_c=28,num_cog_c=14,
                  n_t=2,bias_p_h=0,bias_d_h=0,strikes_p_h=6, 
                  strikes_d_h=6, total_strikes_h=28,num_cog_h=14)

d22_6=data_create(reps=1000,bias_p_c=2.5,bias_d_c=0,strikes_p_c=6, strikes_d_c=6, 
                  total_strikes_c=28,num_cog_c=14,
                  n_t=2,bias_p_h=.5,bias_d_h=0,strikes_p_h=6, 
                  strikes_d_h=6, total_strikes_h=28,num_cog_h=14)

d32_6=data_create(reps=1000,bias_p_c=2.5,bias_d_c=0,strikes_p_c=6, strikes_d_c=6, 
                  total_strikes_c=28,num_cog_c=14,
                  n_t=2,bias_p_h=1,bias_d_h=0,strikes_p_h=6, 
                  strikes_d_h=6, total_strikes_h=28,num_cog_h=14)

d42_6=data_create(reps=1000,bias_p_c=2.5,bias_d_c=0,strikes_p_c=6, strikes_d_c=6, 
                  total_strikes_c=28,num_cog_c=14,
                  n_t=2,bias_p_h=1.5,bias_d_h=0,strikes_p_h=6, 
                  strikes_d_h=6, total_strikes_h=28,num_cog_h=14)

d52_6=data_create(reps=1000,bias_p_c=2.5,bias_d_c=0,strikes_p_c=6, strikes_d_c=6, 
                  total_strikes_c=28,num_cog_c=14,
                  n_t=2,bias_p_h=2,bias_d_h=0,strikes_p_h=6, 
                  strikes_d_h=6, total_strikes_h=28,num_cog_h=14)

d62_6=data_create(reps=1000,bias_p_c=2.5,bias_d_c=0,strikes_p_c=6, strikes_d_c=6, 
                  total_strikes_c=28,num_cog_c=14,
                  n_t=2,bias_p_h=2.5,bias_d_h=0,strikes_p_h=6, 
                  strikes_d_h=6, total_strikes_h=28,num_cog_h=14)

d72_6=data_create(reps=1000,bias_p_c=2.5,bias_d_c=0,strikes_p_c=6, strikes_d_c=6, 
                  total_strikes_c=28,num_cog_c=14,
                  n_t=2,bias_p_h=3,bias_d_h=0,strikes_p_h=6, 
                  strikes_d_h=6, total_strikes_h=28,num_cog_h=14)


d13_6=data_create(reps=1000,bias_p_c=2.5,bias_d_c=0,strikes_p_c=6, strikes_d_c=6, 
                  total_strikes_c=28,num_cog_c=14,
                  n_t=3,bias_p_h=0,bias_d_h=0,strikes_p_h=6, 
                  strikes_d_h=6, total_strikes_h=28,num_cog_h=14)

d23_6=data_create(reps=1000,bias_p_c=2.5,bias_d_c=0,strikes_p_c=6, strikes_d_c=6, 
                  total_strikes_c=28,num_cog_c=14,
                  n_t=3,bias_p_h=.5,bias_d_h=0,strikes_p_h=6, 
                  strikes_d_h=6, total_strikes_h=28,num_cog_h=14)

d33_6=data_create(reps=1000,bias_p_c=2.5,bias_d_c=0,strikes_p_c=6, strikes_d_c=6, 
                  total_strikes_c=28,num_cog_c=14,
                  n_t=3,bias_p_h=1,bias_d_h=0,strikes_p_h=6, 
                  strikes_d_h=6, total_strikes_h=28,num_cog_h=14)

d43_6=data_create(reps=1000,bias_p_c=2.5,bias_d_c=0,strikes_p_c=6, strikes_d_c=6, 
                  total_strikes_c=28,num_cog_c=14,
                  n_t=3,bias_p_h=1.5,bias_d_h=0,strikes_p_h=6, 
                  strikes_d_h=6, total_strikes_h=28,num_cog_h=14)

d53_6=data_create(reps=1000,bias_p_c=2.5,bias_d_c=0,strikes_p_c=6, strikes_d_c=6, 
                  total_strikes_c=28,num_cog_c=14,
                  n_t=3,bias_p_h=2,bias_d_h=0,strikes_p_h=6, 
                  strikes_d_h=6, total_strikes_h=28,num_cog_h=14)

d63_6=data_create(reps=1000,bias_p_c=2.5,bias_d_c=0,strikes_p_c=6, strikes_d_c=6, 
                  total_strikes_c=28,num_cog_c=14,
                  n_t=3,bias_p_h=2.5,bias_d_h=0,strikes_p_h=6, 
                  strikes_d_h=6, total_strikes_h=28,num_cog_h=14)

d73_6=data_create(reps=1000,bias_p_c=2.5,bias_d_c=0,strikes_p_c=6, strikes_d_c=6, 
                  total_strikes_c=28,num_cog_c=14,
                  n_t=3,bias_p_h=3,bias_d_h=0,strikes_p_h=6, 
                  strikes_d_h=6, total_strikes_h=28,num_cog_h=14)

d11_10=data_create(reps=1000,bias_p_c=2.5,bias_d_c=0,strikes_p_c=10, strikes_d_c=10, 
                   total_strikes_c=28,num_cog_c=14,
                   n_t=1,bias_p_h=0,bias_d_h=0,strikes_p_h=10, 
                   strikes_d_h=10, total_strikes_h=28,num_cog_h=14)

d21_10=data_create(reps=1000,bias_p_c=2.5,bias_d_c=0,strikes_p_c=10, strikes_d_c=10, 
                   total_strikes_c=28,num_cog_c=14,
                   n_t=1,bias_p_h=.5,bias_d_h=0,strikes_p_h=10, 
                   strikes_d_h=10, total_strikes_h=28,num_cog_h=14)

d31_10=data_create(reps=1000,bias_p_c=2.5,bias_d_c=0,strikes_p_c=10, strikes_d_c=10, 
                   total_strikes_c=28,num_cog_c=14,
                   n_t=1,bias_p_h=1,bias_d_h=0,strikes_p_h=10, 
                   strikes_d_h=10, total_strikes_h=28,num_cog_h=14)

d41_10=data_create(reps=1000,bias_p_c=2.5,bias_d_c=0,strikes_p_c=10, strikes_d_c=10, 
                   total_strikes_c=28,num_cog_c=14,
                   n_t=1,bias_p_h=1.5,bias_d_h=0,strikes_p_h=10, 
                   strikes_d_h=10, total_strikes_h=28,num_cog_h=14)

d51_10=data_create(reps=1000,bias_p_c=2.5,bias_d_c=0,strikes_p_c=10, strikes_d_c=10, 
                   total_strikes_c=28,num_cog_c=14,
                   n_t=1,bias_p_h=2,bias_d_h=0,strikes_p_h=10, 
                   strikes_d_h=10, total_strikes_h=28,num_cog_h=14)

d61_10=data_create(reps=1000,bias_p_c=2.5,bias_d_c=0,strikes_p_c=10, strikes_d_c=10, 
                   total_strikes_c=28,num_cog_c=14,
                   n_t=1,bias_p_h=2.5,bias_d_h=0,strikes_p_h=10, 
                   strikes_d_h=10, total_strikes_h=28,num_cog_h=14)

d71_10=data_create(reps=1000,bias_p_c=2.5,bias_d_c=0,strikes_p_c=10, strikes_d_c=10, 
                   total_strikes_c=28,num_cog_c=14,
                   n_t=1,bias_p_h=3,bias_d_h=0,strikes_p_h=10, 
                   strikes_d_h=10, total_strikes_h=28,num_cog_h=14)



d12_10=data_create(reps=1000,bias_p_c=2.5,bias_d_c=0,strikes_p_c=10, strikes_d_c=10, 
                   total_strikes_c=28,num_cog_c=14,
                   n_t=2,bias_p_h=0,bias_d_h=0,strikes_p_h=10, 
                   strikes_d_h=10, total_strikes_h=28,num_cog_h=14)

d22_10=data_create(reps=1000,bias_p_c=2.5,bias_d_c=0,strikes_p_c=10, strikes_d_c=10, 
                   total_strikes_c=28,num_cog_c=14,
                   n_t=2,bias_p_h=.5,bias_d_h=0,strikes_p_h=10, 
                   strikes_d_h=10, total_strikes_h=28,num_cog_h=14)

d32_10=data_create(reps=1000,bias_p_c=2.5,bias_d_c=0,strikes_p_c=10, strikes_d_c=10, 
                   total_strikes_c=28,num_cog_c=14,
                   n_t=2,bias_p_h=1,bias_d_h=0,strikes_p_h=10, 
                   strikes_d_h=10, total_strikes_h=28,num_cog_h=14)

d42_10=data_create(reps=1000,bias_p_c=2.5,bias_d_c=0,strikes_p_c=10, strikes_d_c=10, 
                   total_strikes_c=28,num_cog_c=14,
                   n_t=2,bias_p_h=1.5,bias_d_h=0,strikes_p_h=10, 
                   strikes_d_h=10, total_strikes_h=28,num_cog_h=14)

d52_10=data_create(reps=1000,bias_p_c=2.5,bias_d_c=0,strikes_p_c=10, strikes_d_c=10, 
                   total_strikes_c=28,num_cog_c=14,
                   n_t=2,bias_p_h=2,bias_d_h=0,strikes_p_h=10, 
                   strikes_d_h=10, total_strikes_h=28,num_cog_h=14)

d62_10=data_create(reps=1000,bias_p_c=2.5,bias_d_c=0,strikes_p_c=10, strikes_d_c=10, 
                   total_strikes_c=28,num_cog_c=14,
                   n_t=2,bias_p_h=2.5,bias_d_h=0,strikes_p_h=10, 
                   strikes_d_h=10, total_strikes_h=28,num_cog_h=14)

d72_10=data_create(reps=1000,bias_p_c=2.5,bias_d_c=0,strikes_p_c=10, strikes_d_c=10, 
                   total_strikes_c=28,num_cog_c=14,
                   n_t=2,bias_p_h=3,bias_d_h=0,strikes_p_h=10, 
                   strikes_d_h=10, total_strikes_h=28,num_cog_h=14)


d13_10=data_create(reps=1000,bias_p_c=2.5,bias_d_c=0,strikes_p_c=10, strikes_d_c=10, 
                   total_strikes_c=28,num_cog_c=14,
                   n_t=3,bias_p_h=0,bias_d_h=0,strikes_p_h=10, 
                   strikes_d_h=10, total_strikes_h=28,num_cog_h=14)

d23_10=data_create(reps=1000,bias_p_c=2.5,bias_d_c=0,strikes_p_c=10, strikes_d_c=10, 
                   total_strikes_c=28,num_cog_c=14,
                   n_t=3,bias_p_h=.5,bias_d_h=0,strikes_p_h=10, 
                   strikes_d_h=10, total_strikes_h=28,num_cog_h=14)

d33_10=data_create(reps=1000,bias_p_c=2.5,bias_d_c=0,strikes_p_c=10, strikes_d_c=10, 
                   total_strikes_c=28,num_cog_c=14,
                   n_t=3,bias_p_h=1,bias_d_h=0,strikes_p_h=10, 
                   strikes_d_h=10, total_strikes_h=28,num_cog_h=14)

d43_10=data_create(reps=1000,bias_p_c=2.5,bias_d_c=0,strikes_p_c=10, strikes_d_c=10, 
                   total_strikes_c=28,num_cog_c=14,
                   n_t=3,bias_p_h=1.5,bias_d_h=0,strikes_p_h=10, 
                   strikes_d_h=10, total_strikes_h=28,num_cog_h=14)

d53_10=data_create(reps=1000,bias_p_c=2.5,bias_d_c=0,strikes_p_c=10, strikes_d_c=10, 
                   total_strikes_c=28,num_cog_c=14,
                   n_t=3,bias_p_h=2,bias_d_h=0,strikes_p_h=10, 
                   strikes_d_h=10, total_strikes_h=28,num_cog_h=14)

d63_10=data_create(reps=1000,bias_p_c=2.5,bias_d_c=0,strikes_p_c=10, strikes_d_c=10, 
                   total_strikes_c=28,num_cog_c=14,
                   n_t=3,bias_p_h=2.5,bias_d_h=0,strikes_p_h=10, 
                   strikes_d_h=10, total_strikes_h=28,num_cog_h=14)

d73_10=data_create(reps=1000,bias_p_c=2.5,bias_d_c=0,strikes_p_c=10, strikes_d_c=10, 
                   total_strikes_c=28,num_cog_c=14,
                   n_t=3,bias_p_h=3,bias_d_h=0,strikes_p_h=10, 
                   strikes_d_h=10, total_strikes_h=28,num_cog_h=14)



d11_15=data_create(reps=1000,bias_p_c=2.5,bias_d_c=0,strikes_p_c=15, strikes_d_c=15, 
                   total_strikes_c=30,num_cog_c=15,
                   n_t=1,bias_p_h=0,bias_d_h=0,strikes_p_h=15, 
                   strikes_d_h=15, total_strikes_h=30,num_cog_h=15)

d21_15=data_create(reps=1000,bias_p_c=2.5,bias_d_c=0,strikes_p_c=15, strikes_d_c=15, 
                   total_strikes_c=30,num_cog_c=15,
                   n_t=1,bias_p_h=.5,bias_d_h=0,strikes_p_h=15, 
                   strikes_d_h=15, total_strikes_h=30,num_cog_h=15)

d31_15=data_create(reps=1000,bias_p_c=2.5,bias_d_c=0,strikes_p_c=15, strikes_d_c=15, 
                   total_strikes_c=30,num_cog_c=15,
                   n_t=1,bias_p_h=1,bias_d_h=0,strikes_p_h=15, 
                   strikes_d_h=15, total_strikes_h=30,num_cog_h=15)

d41_15=data_create(reps=1000,bias_p_c=2.5,bias_d_c=0,strikes_p_c=15, strikes_d_c=15, 
                   total_strikes_c=30,num_cog_c=15,
                   n_t=1,bias_p_h=1.5,bias_d_h=0,strikes_p_h=15, 
                   strikes_d_h=15, total_strikes_h=30,num_cog_h=15)

d51_15=data_create(reps=1000,bias_p_c=2.5,bias_d_c=0,strikes_p_c=15, strikes_d_c=15, 
                   total_strikes_c=30,num_cog_c=15,
                   n_t=1,bias_p_h=2,bias_d_h=0,strikes_p_h=15, 
                   strikes_d_h=15, total_strikes_h=30,num_cog_h=15)

d61_15=data_create(reps=1000,bias_p_c=2.5,bias_d_c=0,strikes_p_c=15, strikes_d_c=15, 
                   total_strikes_c=30,num_cog_c=15,
                   n_t=1,bias_p_h=2.5,bias_d_h=0,strikes_p_h=15, 
                   strikes_d_h=15, total_strikes_h=30,num_cog_h=15)

d71_15=data_create(reps=1000,bias_p_c=2.5,bias_d_c=0,strikes_p_c=15, strikes_d_c=15, 
                   total_strikes_c=30,num_cog_c=15,
                   n_t=1,bias_p_h=3,bias_d_h=0,strikes_p_h=15, 
                   strikes_d_h=15, total_strikes_h=30,num_cog_h=15)



d12_15=data_create(reps=1000,bias_p_c=2.5,bias_d_c=0,strikes_p_c=15, strikes_d_c=15, 
                   total_strikes_c=30,num_cog_c=15,
                   n_t=2,bias_p_h=0,bias_d_h=0,strikes_p_h=15, 
                   strikes_d_h=15, total_strikes_h=30,num_cog_h=15)

d22_15=data_create(reps=1000,bias_p_c=2.5,bias_d_c=0,strikes_p_c=15, strikes_d_c=15, 
                   total_strikes_c=30,num_cog_c=15,
                   n_t=2,bias_p_h=.5,bias_d_h=0,strikes_p_h=15, 
                   strikes_d_h=15, total_strikes_h=30,num_cog_h=15)

d32_15=data_create(reps=1000,bias_p_c=2.5,bias_d_c=0,strikes_p_c=15, strikes_d_c=15, 
                   total_strikes_c=30,num_cog_c=15,
                   n_t=2,bias_p_h=1,bias_d_h=0,strikes_p_h=15, 
                   strikes_d_h=15, total_strikes_h=30,num_cog_h=15)

d42_15=data_create(reps=1000,bias_p_c=2.5,bias_d_c=0,strikes_p_c=15, strikes_d_c=15, 
                   total_strikes_c=30,num_cog_c=15,
                   n_t=2,bias_p_h=1.5,bias_d_h=0,strikes_p_h=15, 
                   strikes_d_h=15, total_strikes_h=30,num_cog_h=15)

d52_15=data_create(reps=1000,bias_p_c=2.5,bias_d_c=0,strikes_p_c=15, strikes_d_c=15, 
                   total_strikes_c=30,num_cog_c=15,
                   n_t=2,bias_p_h=2,bias_d_h=0,strikes_p_h=15, 
                   strikes_d_h=15, total_strikes_h=30,num_cog_h=15)

d62_15=data_create(reps=1000,bias_p_c=2.5,bias_d_c=0,strikes_p_c=15, strikes_d_c=15, 
                   total_strikes_c=30,num_cog_c=15,
                   n_t=2,bias_p_h=2.5,bias_d_h=0,strikes_p_h=15, 
                   strikes_d_h=15, total_strikes_h=30,num_cog_h=15)

d72_15=data_create(reps=1000,bias_p_c=2.5,bias_d_c=0,strikes_p_c=15, strikes_d_c=15, 
                   total_strikes_c=30,num_cog_c=15,
                   n_t=2,bias_p_h=3,bias_d_h=0,strikes_p_h=15, 
                   strikes_d_h=15, total_strikes_h=30,num_cog_h=15)


d13_15=data_create(reps=1000,bias_p_c=2.5,bias_d_c=0,strikes_p_c=15, strikes_d_c=15, 
                   total_strikes_c=30,num_cog_c=15,
                   n_t=3,bias_p_h=0,bias_d_h=0,strikes_p_h=15, 
                   strikes_d_h=15, total_strikes_h=30,num_cog_h=15)

d23_15=data_create(reps=1000,bias_p_c=2.5,bias_d_c=0,strikes_p_c=15, strikes_d_c=15, 
                   total_strikes_c=30,num_cog_c=15,
                   n_t=3,bias_p_h=.5,bias_d_h=0,strikes_p_h=15, 
                   strikes_d_h=15, total_strikes_h=30,num_cog_h=15)

d33_15=data_create(reps=1000,bias_p_c=2.5,bias_d_c=0,strikes_p_c=15, strikes_d_c=15, 
                   total_strikes_c=30,num_cog_c=15,
                   n_t=3,bias_p_h=1,bias_d_h=0,strikes_p_h=15, 
                   strikes_d_h=15, total_strikes_h=30,num_cog_h=15)

d43_15=data_create(reps=1000,bias_p_c=2.5,bias_d_c=0,strikes_p_c=15, strikes_d_c=15, 
                   total_strikes_c=30,num_cog_c=15,
                   n_t=3,bias_p_h=1.5,bias_d_h=0,strikes_p_h=15, 
                   strikes_d_h=15, total_strikes_h=30,num_cog_h=15)

d53_15=data_create(reps=1000,bias_p_c=2.5,bias_d_c=0,strikes_p_c=15, strikes_d_c=15, 
                   total_strikes_c=30,num_cog_c=15,
                   n_t=3,bias_p_h=2,bias_d_h=0,strikes_p_h=15, 
                   strikes_d_h=15, total_strikes_h=30,num_cog_h=15)

d63_15=data_create(reps=1000,bias_p_c=2.5,bias_d_c=0,strikes_p_c=15, strikes_d_c=15, 
                   total_strikes_c=30,num_cog_c=15,
                   n_t=3,bias_p_h=2.5,bias_d_h=0,strikes_p_h=15, 
                   strikes_d_h=15, total_strikes_h=30,num_cog_h=15)

d73_15=data_create(reps=1000,bias_p_c=2.5,bias_d_c=0,strikes_p_c=15, strikes_d_c=15, 
                   total_strikes_c=30,num_cog_c=15,
                   n_t=3,bias_p_h=3,bias_d_h=0,strikes_p_h=15, 
                   strikes_d_h=15, total_strikes_h=30,num_cog_h=15)

save.image("/dats2.5.RData")



#bias_p_c=3
d11_6=data_create(reps=1000,bias_p_c=3,bias_d_c=0,strikes_p_c=6, strikes_d_c=6, 
                  total_strikes_c=28,num_cog_c=14,
                  n_t=1,bias_p_h=0,bias_d_h=0,strikes_p_h=6, 
                  strikes_d_h=6, total_strikes_h=28,num_cog_h=14)

d21_6=data_create(reps=1000,bias_p_c=3,bias_d_c=0,strikes_p_c=6, strikes_d_c=6, 
                  total_strikes_c=28,num_cog_c=14,
                  n_t=1,bias_p_h=.5,bias_d_h=0,strikes_p_h=6, 
                  strikes_d_h=6, total_strikes_h=28,num_cog_h=14)

d31_6=data_create(reps=1000,bias_p_c=3,bias_d_c=0,strikes_p_c=6, strikes_d_c=6, 
                  total_strikes_c=28,num_cog_c=14,
                  n_t=1,bias_p_h=1,bias_d_h=0,strikes_p_h=6, 
                  strikes_d_h=6, total_strikes_h=28,num_cog_h=14)

d41_6=data_create(reps=1000,bias_p_c=3,bias_d_c=0,strikes_p_c=6, strikes_d_c=6, 
                  total_strikes_c=28,num_cog_c=14,
                  n_t=1,bias_p_h=1.5,bias_d_h=0,strikes_p_h=6, 
                  strikes_d_h=6, total_strikes_h=28,num_cog_h=14)

d51_6=data_create(reps=1000,bias_p_c=3,bias_d_c=0,strikes_p_c=6, strikes_d_c=6, 
                  total_strikes_c=28,num_cog_c=14,
                  n_t=1,bias_p_h=2,bias_d_h=0,strikes_p_h=6, 
                  strikes_d_h=6, total_strikes_h=28,num_cog_h=14)

d61_6=data_create(reps=1000,bias_p_c=3,bias_d_c=0,strikes_p_c=6, strikes_d_c=6, 
                  total_strikes_c=28,num_cog_c=14,
                  n_t=1,bias_p_h=2.5,bias_d_h=0,strikes_p_h=6, 
                  strikes_d_h=6, total_strikes_h=28,num_cog_h=14)

d71_6=data_create(reps=1000,bias_p_c=3,bias_d_c=0,strikes_p_c=6, strikes_d_c=6, 
                  total_strikes_c=28,num_cog_c=14,
                  n_t=1,bias_p_h=3,bias_d_h=0,strikes_p_h=6, 
                  strikes_d_h=6, total_strikes_h=28,num_cog_h=14)



d12_6=data_create(reps=1000,bias_p_c=3,bias_d_c=0,strikes_p_c=6, strikes_d_c=6, 
                  total_strikes_c=28,num_cog_c=14,
                  n_t=2,bias_p_h=0,bias_d_h=0,strikes_p_h=6, 
                  strikes_d_h=6, total_strikes_h=28,num_cog_h=14)

d22_6=data_create(reps=1000,bias_p_c=3,bias_d_c=0,strikes_p_c=6, strikes_d_c=6, 
                  total_strikes_c=28,num_cog_c=14,
                  n_t=2,bias_p_h=.5,bias_d_h=0,strikes_p_h=6, 
                  strikes_d_h=6, total_strikes_h=28,num_cog_h=14)

d32_6=data_create(reps=1000,bias_p_c=3,bias_d_c=0,strikes_p_c=6, strikes_d_c=6, 
                  total_strikes_c=28,num_cog_c=14,
                  n_t=2,bias_p_h=1,bias_d_h=0,strikes_p_h=6, 
                  strikes_d_h=6, total_strikes_h=28,num_cog_h=14)

d42_6=data_create(reps=1000,bias_p_c=3,bias_d_c=0,strikes_p_c=6, strikes_d_c=6, 
                  total_strikes_c=28,num_cog_c=14,
                  n_t=2,bias_p_h=1.5,bias_d_h=0,strikes_p_h=6, 
                  strikes_d_h=6, total_strikes_h=28,num_cog_h=14)

d52_6=data_create(reps=1000,bias_p_c=3,bias_d_c=0,strikes_p_c=6, strikes_d_c=6, 
                  total_strikes_c=28,num_cog_c=14,
                  n_t=2,bias_p_h=2,bias_d_h=0,strikes_p_h=6, 
                  strikes_d_h=6, total_strikes_h=28,num_cog_h=14)

d62_6=data_create(reps=1000,bias_p_c=3,bias_d_c=0,strikes_p_c=6, strikes_d_c=6, 
                  total_strikes_c=28,num_cog_c=14,
                  n_t=2,bias_p_h=2.5,bias_d_h=0,strikes_p_h=6, 
                  strikes_d_h=6, total_strikes_h=28,num_cog_h=14)

d72_6=data_create(reps=1000,bias_p_c=3,bias_d_c=0,strikes_p_c=6, strikes_d_c=6, 
                  total_strikes_c=28,num_cog_c=14,
                  n_t=2,bias_p_h=3,bias_d_h=0,strikes_p_h=6, 
                  strikes_d_h=6, total_strikes_h=28,num_cog_h=14)


d13_6=data_create(reps=1000,bias_p_c=3,bias_d_c=0,strikes_p_c=6, strikes_d_c=6, 
                  total_strikes_c=28,num_cog_c=14,
                  n_t=3,bias_p_h=0,bias_d_h=0,strikes_p_h=6, 
                  strikes_d_h=6, total_strikes_h=28,num_cog_h=14)

d23_6=data_create(reps=1000,bias_p_c=3,bias_d_c=0,strikes_p_c=6, strikes_d_c=6, 
                  total_strikes_c=28,num_cog_c=14,
                  n_t=3,bias_p_h=.5,bias_d_h=0,strikes_p_h=6, 
                  strikes_d_h=6, total_strikes_h=28,num_cog_h=14)

d33_6=data_create(reps=1000,bias_p_c=3,bias_d_c=0,strikes_p_c=6, strikes_d_c=6, 
                  total_strikes_c=28,num_cog_c=14,
                  n_t=3,bias_p_h=1,bias_d_h=0,strikes_p_h=6, 
                  strikes_d_h=6, total_strikes_h=28,num_cog_h=14)

d43_6=data_create(reps=1000,bias_p_c=3,bias_d_c=0,strikes_p_c=6, strikes_d_c=6, 
                  total_strikes_c=28,num_cog_c=14,
                  n_t=3,bias_p_h=1.5,bias_d_h=0,strikes_p_h=6, 
                  strikes_d_h=6, total_strikes_h=28,num_cog_h=14)

d53_6=data_create(reps=1000,bias_p_c=3,bias_d_c=0,strikes_p_c=6, strikes_d_c=6, 
                  total_strikes_c=28,num_cog_c=14,
                  n_t=3,bias_p_h=2,bias_d_h=0,strikes_p_h=6, 
                  strikes_d_h=6, total_strikes_h=28,num_cog_h=14)

d63_6=data_create(reps=1000,bias_p_c=3,bias_d_c=0,strikes_p_c=6, strikes_d_c=6, 
                  total_strikes_c=28,num_cog_c=14,
                  n_t=3,bias_p_h=2.5,bias_d_h=0,strikes_p_h=6, 
                  strikes_d_h=6, total_strikes_h=28,num_cog_h=14)

d73_6=data_create(reps=1000,bias_p_c=3,bias_d_c=0,strikes_p_c=6, strikes_d_c=6, 
                  total_strikes_c=28,num_cog_c=14,
                  n_t=3,bias_p_h=3,bias_d_h=0,strikes_p_h=6, 
                  strikes_d_h=6, total_strikes_h=28,num_cog_h=14)

d11_10=data_create(reps=1000,bias_p_c=3,bias_d_c=0,strikes_p_c=10, strikes_d_c=10, 
                   total_strikes_c=28,num_cog_c=14,
                   n_t=1,bias_p_h=0,bias_d_h=0,strikes_p_h=10, 
                   strikes_d_h=10, total_strikes_h=28,num_cog_h=14)

d21_10=data_create(reps=1000,bias_p_c=3,bias_d_c=0,strikes_p_c=10, strikes_d_c=10, 
                   total_strikes_c=28,num_cog_c=14,
                   n_t=1,bias_p_h=.5,bias_d_h=0,strikes_p_h=10, 
                   strikes_d_h=10, total_strikes_h=28,num_cog_h=14)

d31_10=data_create(reps=1000,bias_p_c=3,bias_d_c=0,strikes_p_c=10, strikes_d_c=10, 
                   total_strikes_c=28,num_cog_c=14,
                   n_t=1,bias_p_h=1,bias_d_h=0,strikes_p_h=10, 
                   strikes_d_h=10, total_strikes_h=28,num_cog_h=14)

d41_10=data_create(reps=1000,bias_p_c=3,bias_d_c=0,strikes_p_c=10, strikes_d_c=10, 
                   total_strikes_c=28,num_cog_c=14,
                   n_t=1,bias_p_h=1.5,bias_d_h=0,strikes_p_h=10, 
                   strikes_d_h=10, total_strikes_h=28,num_cog_h=14)

d51_10=data_create(reps=1000,bias_p_c=3,bias_d_c=0,strikes_p_c=10, strikes_d_c=10, 
                   total_strikes_c=28,num_cog_c=14,
                   n_t=1,bias_p_h=2,bias_d_h=0,strikes_p_h=10, 
                   strikes_d_h=10, total_strikes_h=28,num_cog_h=14)

d61_10=data_create(reps=1000,bias_p_c=3,bias_d_c=0,strikes_p_c=10, strikes_d_c=10, 
                   total_strikes_c=28,num_cog_c=14,
                   n_t=1,bias_p_h=2.5,bias_d_h=0,strikes_p_h=10, 
                   strikes_d_h=10, total_strikes_h=28,num_cog_h=14)

d71_10=data_create(reps=1000,bias_p_c=3,bias_d_c=0,strikes_p_c=10, strikes_d_c=10, 
                   total_strikes_c=28,num_cog_c=14,
                   n_t=1,bias_p_h=3,bias_d_h=0,strikes_p_h=10, 
                   strikes_d_h=10, total_strikes_h=28,num_cog_h=14)



d12_10=data_create(reps=1000,bias_p_c=3,bias_d_c=0,strikes_p_c=10, strikes_d_c=10, 
                   total_strikes_c=28,num_cog_c=14,
                   n_t=2,bias_p_h=0,bias_d_h=0,strikes_p_h=10, 
                   strikes_d_h=10, total_strikes_h=28,num_cog_h=14)

d22_10=data_create(reps=1000,bias_p_c=3,bias_d_c=0,strikes_p_c=10, strikes_d_c=10, 
                   total_strikes_c=28,num_cog_c=14,
                   n_t=2,bias_p_h=.5,bias_d_h=0,strikes_p_h=10, 
                   strikes_d_h=10, total_strikes_h=28,num_cog_h=14)

d32_10=data_create(reps=1000,bias_p_c=3,bias_d_c=0,strikes_p_c=10, strikes_d_c=10, 
                   total_strikes_c=28,num_cog_c=14,
                   n_t=2,bias_p_h=1,bias_d_h=0,strikes_p_h=10, 
                   strikes_d_h=10, total_strikes_h=28,num_cog_h=14)

d42_10=data_create(reps=1000,bias_p_c=3,bias_d_c=0,strikes_p_c=10, strikes_d_c=10, 
                   total_strikes_c=28,num_cog_c=14,
                   n_t=2,bias_p_h=1.5,bias_d_h=0,strikes_p_h=10, 
                   strikes_d_h=10, total_strikes_h=28,num_cog_h=14)

d52_10=data_create(reps=1000,bias_p_c=3,bias_d_c=0,strikes_p_c=10, strikes_d_c=10, 
                   total_strikes_c=28,num_cog_c=14,
                   n_t=2,bias_p_h=2,bias_d_h=0,strikes_p_h=10, 
                   strikes_d_h=10, total_strikes_h=28,num_cog_h=14)

d62_10=data_create(reps=1000,bias_p_c=3,bias_d_c=0,strikes_p_c=10, strikes_d_c=10, 
                   total_strikes_c=28,num_cog_c=14,
                   n_t=2,bias_p_h=2.5,bias_d_h=0,strikes_p_h=10, 
                   strikes_d_h=10, total_strikes_h=28,num_cog_h=14)

d72_10=data_create(reps=1000,bias_p_c=3,bias_d_c=0,strikes_p_c=10, strikes_d_c=10, 
                   total_strikes_c=28,num_cog_c=14,
                   n_t=2,bias_p_h=3,bias_d_h=0,strikes_p_h=10, 
                   strikes_d_h=10, total_strikes_h=28,num_cog_h=14)


d13_10=data_create(reps=1000,bias_p_c=3,bias_d_c=0,strikes_p_c=10, strikes_d_c=10, 
                   total_strikes_c=28,num_cog_c=14,
                   n_t=3,bias_p_h=0,bias_d_h=0,strikes_p_h=10, 
                   strikes_d_h=10, total_strikes_h=28,num_cog_h=14)

d23_10=data_create(reps=1000,bias_p_c=3,bias_d_c=0,strikes_p_c=10, strikes_d_c=10, 
                   total_strikes_c=28,num_cog_c=14,
                   n_t=3,bias_p_h=.5,bias_d_h=0,strikes_p_h=10, 
                   strikes_d_h=10, total_strikes_h=28,num_cog_h=14)

d33_10=data_create(reps=1000,bias_p_c=3,bias_d_c=0,strikes_p_c=10, strikes_d_c=10, 
                   total_strikes_c=28,num_cog_c=14,
                   n_t=3,bias_p_h=1,bias_d_h=0,strikes_p_h=10, 
                   strikes_d_h=10, total_strikes_h=28,num_cog_h=14)

d43_10=data_create(reps=1000,bias_p_c=3,bias_d_c=0,strikes_p_c=10, strikes_d_c=10, 
                   total_strikes_c=28,num_cog_c=14,
                   n_t=3,bias_p_h=1.5,bias_d_h=0,strikes_p_h=10, 
                   strikes_d_h=10, total_strikes_h=28,num_cog_h=14)

d53_10=data_create(reps=1000,bias_p_c=3,bias_d_c=0,strikes_p_c=10, strikes_d_c=10, 
                   total_strikes_c=28,num_cog_c=14,
                   n_t=3,bias_p_h=2,bias_d_h=0,strikes_p_h=10, 
                   strikes_d_h=10, total_strikes_h=28,num_cog_h=14)

d63_10=data_create(reps=1000,bias_p_c=3,bias_d_c=0,strikes_p_c=10, strikes_d_c=10, 
                   total_strikes_c=28,num_cog_c=14,
                   n_t=3,bias_p_h=2.5,bias_d_h=0,strikes_p_h=10, 
                   strikes_d_h=10, total_strikes_h=28,num_cog_h=14)

d73_10=data_create(reps=1000,bias_p_c=3,bias_d_c=0,strikes_p_c=10, strikes_d_c=10, 
                   total_strikes_c=28,num_cog_c=14,
                   n_t=3,bias_p_h=3,bias_d_h=0,strikes_p_h=10, 
                   strikes_d_h=10, total_strikes_h=28,num_cog_h=14)



d11_15=data_create(reps=1000,bias_p_c=3,bias_d_c=0,strikes_p_c=15, strikes_d_c=15, 
                   total_strikes_c=30,num_cog_c=15,
                   n_t=1,bias_p_h=0,bias_d_h=0,strikes_p_h=15, 
                   strikes_d_h=15, total_strikes_h=30,num_cog_h=15)

d21_15=data_create(reps=1000,bias_p_c=3,bias_d_c=0,strikes_p_c=15, strikes_d_c=15, 
                   total_strikes_c=30,num_cog_c=15,
                   n_t=1,bias_p_h=.5,bias_d_h=0,strikes_p_h=15, 
                   strikes_d_h=15, total_strikes_h=30,num_cog_h=15)

d31_15=data_create(reps=1000,bias_p_c=3,bias_d_c=0,strikes_p_c=15, strikes_d_c=15, 
                   total_strikes_c=30,num_cog_c=15,
                   n_t=1,bias_p_h=1,bias_d_h=0,strikes_p_h=15, 
                   strikes_d_h=15, total_strikes_h=30,num_cog_h=15)

d41_15=data_create(reps=1000,bias_p_c=3,bias_d_c=0,strikes_p_c=15, strikes_d_c=15, 
                   total_strikes_c=30,num_cog_c=15,
                   n_t=1,bias_p_h=1.5,bias_d_h=0,strikes_p_h=15, 
                   strikes_d_h=15, total_strikes_h=30,num_cog_h=15)

d51_15=data_create(reps=1000,bias_p_c=3,bias_d_c=0,strikes_p_c=15, strikes_d_c=15, 
                   total_strikes_c=30,num_cog_c=15,
                   n_t=1,bias_p_h=2,bias_d_h=0,strikes_p_h=15, 
                   strikes_d_h=15, total_strikes_h=30,num_cog_h=15)

d61_15=data_create(reps=1000,bias_p_c=3,bias_d_c=0,strikes_p_c=15, strikes_d_c=15, 
                   total_strikes_c=30,num_cog_c=15,
                   n_t=1,bias_p_h=2.5,bias_d_h=0,strikes_p_h=15, 
                   strikes_d_h=15, total_strikes_h=30,num_cog_h=15)

d71_15=data_create(reps=1000,bias_p_c=3,bias_d_c=0,strikes_p_c=15, strikes_d_c=15, 
                   total_strikes_c=30,num_cog_c=15,
                   n_t=1,bias_p_h=3,bias_d_h=0,strikes_p_h=15, 
                   strikes_d_h=15, total_strikes_h=30,num_cog_h=15)



d12_15=data_create(reps=1000,bias_p_c=3,bias_d_c=0,strikes_p_c=15, strikes_d_c=15, 
                   total_strikes_c=30,num_cog_c=15,
                   n_t=2,bias_p_h=0,bias_d_h=0,strikes_p_h=15, 
                   strikes_d_h=15, total_strikes_h=30,num_cog_h=15)

d22_15=data_create(reps=1000,bias_p_c=3,bias_d_c=0,strikes_p_c=15, strikes_d_c=15, 
                   total_strikes_c=30,num_cog_c=15,
                   n_t=2,bias_p_h=.5,bias_d_h=0,strikes_p_h=15, 
                   strikes_d_h=15, total_strikes_h=30,num_cog_h=15)

d32_15=data_create(reps=1000,bias_p_c=3,bias_d_c=0,strikes_p_c=15, strikes_d_c=15, 
                   total_strikes_c=30,num_cog_c=15,
                   n_t=2,bias_p_h=1,bias_d_h=0,strikes_p_h=15, 
                   strikes_d_h=15, total_strikes_h=30,num_cog_h=15)

d42_15=data_create(reps=1000,bias_p_c=3,bias_d_c=0,strikes_p_c=15, strikes_d_c=15, 
                   total_strikes_c=30,num_cog_c=15,
                   n_t=2,bias_p_h=1.5,bias_d_h=0,strikes_p_h=15, 
                   strikes_d_h=15, total_strikes_h=30,num_cog_h=15)

d52_15=data_create(reps=1000,bias_p_c=3,bias_d_c=0,strikes_p_c=15, strikes_d_c=15, 
                   total_strikes_c=30,num_cog_c=15,
                   n_t=2,bias_p_h=2,bias_d_h=0,strikes_p_h=15, 
                   strikes_d_h=15, total_strikes_h=30,num_cog_h=15)

d62_15=data_create(reps=1000,bias_p_c=3,bias_d_c=0,strikes_p_c=15, strikes_d_c=15, 
                   total_strikes_c=30,num_cog_c=15,
                   n_t=2,bias_p_h=2.5,bias_d_h=0,strikes_p_h=15, 
                   strikes_d_h=15, total_strikes_h=30,num_cog_h=15)

d72_15=data_create(reps=1000,bias_p_c=3,bias_d_c=0,strikes_p_c=15, strikes_d_c=15, 
                   total_strikes_c=30,num_cog_c=15,
                   n_t=2,bias_p_h=3,bias_d_h=0,strikes_p_h=15, 
                   strikes_d_h=15, total_strikes_h=30,num_cog_h=15)


d13_15=data_create(reps=1000,bias_p_c=3,bias_d_c=0,strikes_p_c=15, strikes_d_c=15, 
                   total_strikes_c=30,num_cog_c=15,
                   n_t=3,bias_p_h=0,bias_d_h=0,strikes_p_h=15, 
                   strikes_d_h=15, total_strikes_h=30,num_cog_h=15)

d23_15=data_create(reps=1000,bias_p_c=3,bias_d_c=0,strikes_p_c=15, strikes_d_c=15, 
                   total_strikes_c=30,num_cog_c=15,
                   n_t=3,bias_p_h=.5,bias_d_h=0,strikes_p_h=15, 
                   strikes_d_h=15, total_strikes_h=30,num_cog_h=15)

d33_15=data_create(reps=1000,bias_p_c=3,bias_d_c=0,strikes_p_c=15, strikes_d_c=15, 
                   total_strikes_c=30,num_cog_c=15,
                   n_t=3,bias_p_h=1,bias_d_h=0,strikes_p_h=15, 
                   strikes_d_h=15, total_strikes_h=30,num_cog_h=15)

d43_15=data_create(reps=1000,bias_p_c=3,bias_d_c=0,strikes_p_c=15, strikes_d_c=15, 
                   total_strikes_c=30,num_cog_c=15,
                   n_t=3,bias_p_h=1.5,bias_d_h=0,strikes_p_h=15, 
                   strikes_d_h=15, total_strikes_h=30,num_cog_h=15)

d53_15=data_create(reps=1000,bias_p_c=3,bias_d_c=0,strikes_p_c=15, strikes_d_c=15, 
                   total_strikes_c=30,num_cog_c=15,
                   n_t=3,bias_p_h=2,bias_d_h=0,strikes_p_h=15, 
                   strikes_d_h=15, total_strikes_h=30,num_cog_h=15)

d63_15=data_create(reps=1000,bias_p_c=3,bias_d_c=0,strikes_p_c=15, strikes_d_c=15, 
                   total_strikes_c=30,num_cog_c=15,
                   n_t=3,bias_p_h=2.5,bias_d_h=0,strikes_p_h=15, 
                   strikes_d_h=15, total_strikes_h=30,num_cog_h=15)

d73_15=data_create(reps=1000,bias_p_c=3,bias_d_c=0,strikes_p_c=15, strikes_d_c=15, 
                   total_strikes_c=30,num_cog_c=15,
                   n_t=3,bias_p_h=3,bias_d_h=0,strikes_p_h=15, 
                   strikes_d_h=15, total_strikes_h=30,num_cog_h=15)







































































