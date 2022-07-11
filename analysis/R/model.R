# Purpose of script ------------------------------------------------------------------------------------------------------------
# Includes model fit and build for stan code and code to run the model over 'R' number of replicates of the datasets

# Libraries required 
library(MASS)
library(rstan) 


# model --------------------------------------------------------------------------------------------------------------

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
#to compile model 
fit<- stan_model(model_code=stan_mod) 



# pp_sims ------------------------------------------------------------------------------------------------------------
# Fits the model on each of the datasets and then uses Monte Carlo average to estimate evaluation metrics 

#only one discounting coefficient (same for all historical trials)
pp_sims<-function(dats,alpha,mean_init,sd_init,true_bias,P,num_htrials)
{
  ## Results to index
  #bias_b=bias of the bias-parameter
  #sd_b= posterior SD of the bias-parameter
  #ind_bias: indicator if bias is detected or not (with percent region)
  #ind_par: indicator if parameter is recovered or not (with percent region)
  #b_c_hat: number cogs/number of strikes (Current trial)
  #b_h_hat: number cogs/ number of strikes ( historical trials-pooled )
  # b_diff_hat: | b_c_hat - b_h_hat | 
  res=data.frame("bias_b"=rep(NA,length(dats$c_dats)),
                 "mean_b"=rep(NA,length(dats$c_dats)),
                 "sd_b"=rep(NA,length(dats$c_dats)),
                 "ind_bias_80"=rep(NA,length(dats$c_dats)),
                 "ind_par_80"=rep(NA,length(dats$c_dats)),
                 "ind_bias_90"=rep(NA,length(dats$c_dats)),
                 "ind_par_90"=rep(NA,length(dats$c_dats)),
                 "ind_bias_95"=rep(NA,length(dats$c_dats)),
                 "ind_par_95"=rep(NA,length(dats$c_dats)),
                 "b_c_hat"=rep(NA,length(dats$c_dats)),
                 "b_h_hat"=rep(NA,length(dats$c_dats)),
                 "b_diff_hat"=rep(NA,length(dats$c_dats)))
  
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
    
    ci_80 <- as.numeric(quantile(b,probs=c(0.1,.9)))
    
    res[r, "ind_bias_80"] = !(ci_80[1] <= 0 & 0 <= ci_80[2]) # bias is if 0 is not included
    res[r, "ind_par_80"] = ci_80[1] <= true_bias & true_bias <= ci_80[2]
    ci_90 <- as.numeric(quantile(b,probs=c(0.05,.95)))
    res[r, "ind_bias_90"] = !(ci_90[1] <= 0 & 0 <= ci_90[2])
    res[r, "ind_par_90"] = ci_90[1] <= true_bias & true_bias <= ci_90[2]
    ci_95 <- as.numeric(quantile(b,probs=c(0.025,.975)))
    res[r, "ind_bias_95"] = !(ci_95[1] <= 0 & 0 <= ci_95[2])
    res[r, "ind_par_95"] = ci_95[1] <= true_bias & true_bias <= ci_95[2]
    
    #difference metric 
    res[r,"b_c_hat"]= mean(c_dat$cog)
    res[r,"b_h_hat"] = mean(h_dat$cog)
    res[r,"b_diff_hat"]= abs(res[r,"b_c_hat"]-res[r,"b_h_hat"])
    
    #print iter
    if(P==1)
    {
      print(r)
    }
  }
  
  results=colMeans(res)
  #simulation standard error:
  se=sd(res[,"mean_b"])
  final_results=c(results,se)
  names(final_results)=c("post_bias","post_mean","post_sd","post_bias_det80","cp_80",
                         "post_bias_det90","cp_90","post_bias_det95","cp_95","mean_bhat_c","mean_bhat_h",
                         "mean_bhat_diff","sim_se")
  
  return(list(final_results,fit_mod))
}



# get_res ------------------------------------------------------------------------------------------------------------

## get_res: do simulations for one scenario(data sets) looping over different alphas 
#@dats: list of data from data create
#@alpha_list: vector of alpha combinations to apply 
#@true_bias= the fixed true bias in the current trial 
#@num_htrials: number of historical trials 

alpha_list=c(0,.1,.2,.3,.4,.5,.6,.7,.8,.9,1) #selection of discounting coefficients
#only works for one discounting coefficient
get_res=function(dats,alpha_list,true_bias,num_htrials)
{
  res=list()
  for(i in 1:length(alpha_list))
  {
    res[[i]]=pp_sims(dats,alpha=alpha_list[i],mean_init=0,sd_init=2,true_bias=true_bias,
                     P=1,num_htrials)
  }
  return(res)
}

#p1=get_res(dats=d11_6,alpha_list=alpha_list,true_bias=0,num_htrials=1)

# Simulations --------------------------------------------------------------------------------------------------------

# The rest of the code is for running and extracting simulations for plotting

#######################################################################################  B_C=0

load("/dats0.RData")
p1=get_res(dats=d11_6,alpha_list=alpha_list,true_bias=0,num_htrials=1)
p2=get_res(dats=d21_6,alpha_list=alpha_list,true_bias=0,num_htrials=1)
p3=get_res(dats=d31_6,alpha_list=alpha_list,true_bias=0,num_htrials=1)
p4=get_res(dats=d41_6,alpha_list=alpha_list,true_bias=0,num_htrials=1)
p5=get_res(dats=d51_6,alpha_list=alpha_list,true_bias=0,num_htrials=1)
p6=get_res(dats=d61_6,alpha_list=alpha_list,true_bias=0,num_htrials=1)
p7=get_res(dats=d71_6,alpha_list=alpha_list,true_bias=0,num_htrials=1)

p8=get_res(dats=d12_6,alpha_list=alpha_list,true_bias=0,num_htrials=2)
p9=get_res(dats=d22_6,alpha_list=alpha_list,true_bias=0,num_htrials=2)
p10=get_res(dats=d32_6,alpha_list=alpha_list,true_bias=0,num_htrials=2)
p11=get_res(dats=d42_6,alpha_list=alpha_list,true_bias=0,num_htrials=2)
p12=get_res(dats=d52_6,alpha_list=alpha_list,true_bias=0,num_htrials=2)
p13=get_res(dats=d62_6,alpha_list=alpha_list,true_bias=0,num_htrials=2)
p14=get_res(dats=d72_6,alpha_list=alpha_list,true_bias=0,num_htrials=2)

p15=get_res(dats=d13_6,alpha_list=alpha_list,true_bias=0,num_htrials=3)
p16=get_res(dats=d23_6,alpha_list=alpha_list,true_bias=0,num_htrials=3)
p17=get_res(dats=d33_6,alpha_list=alpha_list,true_bias=0,num_htrials=3)
p18=get_res(dats=d43_6,alpha_list=alpha_list,true_bias=0,num_htrials=3)
p19=get_res(dats=d53_6,alpha_list=alpha_list,true_bias=0,num_htrials=3)
p20=get_res(dats=d63_6,alpha_list=alpha_list,true_bias=0,num_htrials=3)
p21=get_res(dats=d73_6,alpha_list=alpha_list,true_bias=0,num_htrials=3)

p22=get_res(dats=d11_10,alpha_list=alpha_list,true_bias=0,num_htrials=1)
p23=get_res(dats=d21_10,alpha_list=alpha_list,true_bias=0,num_htrials=1)
p24=get_res(dats=d31_10,alpha_list=alpha_list,true_bias=0,num_htrials=1)
p25=get_res(dats=d41_10,alpha_list=alpha_list,true_bias=0,num_htrials=1)
p26=get_res(dats=d51_10,alpha_list=alpha_list,true_bias=0,num_htrials=1)
p27=get_res(dats=d61_10,alpha_list=alpha_list,true_bias=0,num_htrials=1)
p28=get_res(dats=d71_10,alpha_list=alpha_list,true_bias=0,num_htrials=1)

p29=get_res(dats=d12_10,alpha_list=alpha_list,true_bias=0,num_htrials=2)
p30=get_res(dats=d22_10,alpha_list=alpha_list,true_bias=0,num_htrials=2)

remove(d11_10,d11_15,d11_6,d12_10,d12_15,d12_6,d13_10,d13_15,d13_6,d21_10,d21_15,d21_6,
       d22_10,d22_15,d22_6,d23_10,
       d23_15,d23_6, d31_10,d31_15,
       d31_6, d32_10,d32_15,d32_6,
       d33_10,d33_15,d33_6, d41_10,
       d41_15,d41_6, d42_10,d42_15,
       d42_6, d43_10,d43_15,d43_6,
       d51_10,d51_15,d51_6, d52_10,
       d52_15,d52_6, d53_10,d53_15,
       d53_6, d61_10,d61_15,d61_6,
       d62_10,d62_15,d62_6, d63_10,
       d63_15,d63_6, d71_10,d71_15,
       d71_6, d72_10,d72_15,d72_6,
       d73_10,d73_15,d73_6, data_create,
       get_historical, get_stan_dat,sim_data)

#01.RData: Bias_current, split the results into 1 and then 2.
save.image("/01.RData")

load("/dats0.RData")
p31=get_res(dats=d32_10,alpha_list=alpha_list,true_bias=0,num_htrials=2)
p32=get_res(dats=d42_10,alpha_list=alpha_list,true_bias=0,num_htrials=2)
p33=get_res(dats=d52_10,alpha_list=alpha_list,true_bias=0,num_htrials=2)
p34=get_res(dats=d62_10,alpha_list=alpha_list,true_bias=0,num_htrials=2)
p35=get_res(dats=d72_10,alpha_list=alpha_list,true_bias=0,num_htrials=2)

p36=get_res(dats=d13_10,alpha_list=alpha_list,true_bias=0,num_htrials=3)
p37=get_res(dats=d23_10,alpha_list=alpha_list,true_bias=0,num_htrials=3)
p38=get_res(dats=d33_10,alpha_list=alpha_list,true_bias=0,num_htrials=3)
p39=get_res(dats=d43_10,alpha_list=alpha_list,true_bias=0,num_htrials=3)
p40=get_res(dats=d53_10,alpha_list=alpha_list,true_bias=0,num_htrials=3)
p41=get_res(dats=d63_10,alpha_list=alpha_list,true_bias=0,num_htrials=3)
p42=get_res(dats=d73_10,alpha_list=alpha_list,true_bias=0,num_htrials=3)

p43=get_res(dats=d11_15,alpha_list=alpha_list,true_bias=0,num_htrials=1)
p44=get_res(dats=d21_15,alpha_list=alpha_list,true_bias=0,num_htrials=1)
p45=get_res(dats=d31_15,alpha_list=alpha_list,true_bias=0,num_htrials=1)
p46=get_res(dats=d41_15,alpha_list=alpha_list,true_bias=0,num_htrials=1)
p47=get_res(dats=d51_15,alpha_list=alpha_list,true_bias=0,num_htrials=1)
p48=get_res(dats=d61_15,alpha_list=alpha_list,true_bias=0,num_htrials=1)
p49=get_res(dats=d71_15,alpha_list=alpha_list,true_bias=0,num_htrials=1)

p50=get_res(dats=d12_15,alpha_list=alpha_list,true_bias=0,num_htrials=2)
p51=get_res(dats=d22_15,alpha_list=alpha_list,true_bias=0,num_htrials=2)
p52=get_res(dats=d32_15,alpha_list=alpha_list,true_bias=0,num_htrials=2)
p53=get_res(dats=d42_15,alpha_list=alpha_list,true_bias=0,num_htrials=2)
p54=get_res(dats=d52_15,alpha_list=alpha_list,true_bias=0,num_htrials=2)
p55=get_res(dats=d62_15,alpha_list=alpha_list,true_bias=0,num_htrials=2)
p56=get_res(dats=d72_15,alpha_list=alpha_list,true_bias=0,num_htrials=2)

p57=get_res(dats=d13_15,alpha_list=alpha_list,true_bias=0,num_htrials=3)
p58=get_res(dats=d23_15,alpha_list=alpha_list,true_bias=0,num_htrials=3)
p59=get_res(dats=d33_15,alpha_list=alpha_list,true_bias=0,num_htrials=3)
p60=get_res(dats=d43_15,alpha_list=alpha_list,true_bias=0,num_htrials=3)
p61=get_res(dats=d53_15,alpha_list=alpha_list,true_bias=0,num_htrials=3)
p62=get_res(dats=d63_15,alpha_list=alpha_list,true_bias=0,num_htrials=3)
p63=get_res(dats=d73_15,alpha_list=alpha_list,true_bias=0,num_htrials=3)

remove(d11_10,d11_15,d11_6,d12_10,d12_15,d12_6,d13_10,d13_15,d13_6,d21_10,d21_15,d21_6,
       d22_10,d22_15,d22_6,d23_10,
       d23_15,d23_6, d31_10,d31_15,
       d31_6, d32_10,d32_15,d32_6,
       d33_10,d33_15,d33_6, d41_10,
       d41_15,d41_6, d42_10,d42_15,
       d42_6, d43_10,d43_15,d43_6,
       d51_10,d51_15,d51_6, d52_10,
       d52_15,d52_6, d53_10,d53_15,
       d53_6, d61_10,d61_15,d61_6,
       d62_10,d62_15,d62_6, d63_10,
       d63_15,d63_6, d71_10,d71_15,
       d71_6, d72_10,d72_15,d72_6,
       d73_10,d73_15,d73_6, data_create,
       get_historical, get_stan_dat,sim_data)

save.image("/02.RData")

#######################################################################################  B_C=.5
load("/dats.5.RData")
p1=get_res(dats=d11_6,alpha_list=alpha_list,true_bias=.5,num_htrials=1)
p2=get_res(dats=d21_6,alpha_list=alpha_list,true_bias=.5,num_htrials=1)
p3=get_res(dats=d31_6,alpha_list=alpha_list,true_bias=.5,num_htrials=1)
p4=get_res(dats=d41_6,alpha_list=alpha_list,true_bias=.5,num_htrials=1)
p5=get_res(dats=d51_6,alpha_list=alpha_list,true_bias=.5,num_htrials=1)
p6=get_res(dats=d61_6,alpha_list=alpha_list,true_bias=.5,num_htrials=1)
p7=get_res(dats=d71_6,alpha_list=alpha_list,true_bias=.5,num_htrials=1)

p8=get_res(dats=d12_6,alpha_list=alpha_list,true_bias=.5,num_htrials=2)
p9=get_res(dats=d22_6,alpha_list=alpha_list,true_bias=.5,num_htrials=2)
p10=get_res(dats=d32_6,alpha_list=alpha_list,true_bias=.5,num_htrials=2)
p11=get_res(dats=d42_6,alpha_list=alpha_list,true_bias=.5,num_htrials=2)
p12=get_res(dats=d52_6,alpha_list=alpha_list,true_bias=.5,num_htrials=2)
p13=get_res(dats=d62_6,alpha_list=alpha_list,true_bias=.5,num_htrials=2)
p14=get_res(dats=d72_6,alpha_list=alpha_list,true_bias=.5,num_htrials=2)

p15=get_res(dats=d13_6,alpha_list=alpha_list,true_bias=.5,num_htrials=3)
p16=get_res(dats=d23_6,alpha_list=alpha_list,true_bias=.5,num_htrials=3)
p17=get_res(dats=d33_6,alpha_list=alpha_list,true_bias=.5,num_htrials=3)
p18=get_res(dats=d43_6,alpha_list=alpha_list,true_bias=.5,num_htrials=3)
p19=get_res(dats=d53_6,alpha_list=alpha_list,true_bias=.5,num_htrials=3)
p20=get_res(dats=d63_6,alpha_list=alpha_list,true_bias=.5,num_htrials=3)
p21=get_res(dats=d73_6,alpha_list=alpha_list,true_bias=.5,num_htrials=3)

p22=get_res(dats=d11_10,alpha_list=alpha_list,true_bias=.5,num_htrials=1)
p23=get_res(dats=d21_10,alpha_list=alpha_list,true_bias=.5,num_htrials=1)
p24=get_res(dats=d31_10,alpha_list=alpha_list,true_bias=.5,num_htrials=1)
p25=get_res(dats=d41_10,alpha_list=alpha_list,true_bias=.5,num_htrials=1)
p26=get_res(dats=d51_10,alpha_list=alpha_list,true_bias=.5,num_htrials=1)
p27=get_res(dats=d61_10,alpha_list=alpha_list,true_bias=.5,num_htrials=1)
p28=get_res(dats=d71_10,alpha_list=alpha_list,true_bias=.5,num_htrials=1)

p29=get_res(dats=d12_10,alpha_list=alpha_list,true_bias=.5,num_htrials=2)
p30=get_res(dats=d22_10,alpha_list=alpha_list,true_bias=.5,num_htrials=2)

remove(d11_10,d11_15,d11_6,d12_10,d12_15,d12_6,d13_10,d13_15,d13_6,d21_10,d21_15,d21_6,
       d22_10,d22_15,d22_6,d23_10,        
       d23_15,d23_6, d31_10,d31_15,        
       d31_6, d32_10,d32_15,d32_6,
       d33_10,d33_15,d33_6, d41_10,        
       d41_15,d41_6, d42_10,d42_15,        
       d42_6, d43_10,d43_15,d43_6,
       d51_10,d51_15,d51_6, d52_10,        
       d52_15,d52_6, d53_10,d53_15,        
       d53_6, d61_10,d61_15,d61_6,
       d62_10,d62_15,d62_6, d63_10,        
       d63_15,d63_6, d71_10,d71_15,        
       d71_6, d72_10,d72_15,d72_6,
       d73_10,d73_15,d73_6, data_create,   
       get_historical, get_stan_dat,sim_data)

#01.RData: Bias_current, split the results into 1 and then 2. 
save.image("/.5_1.RData")


load("/dats.5.RData")
p31=get_res(dats=d32_10,alpha_list=alpha_list,true_bias=.5,num_htrials=2)
p32=get_res(dats=d42_10,alpha_list=alpha_list,true_bias=.5,num_htrials=2)
p33=get_res(dats=d52_10,alpha_list=alpha_list,true_bias=.5,num_htrials=2)
p34=get_res(dats=d62_10,alpha_list=alpha_list,true_bias=.5,num_htrials=2)
p35=get_res(dats=d72_10,alpha_list=alpha_list,true_bias=.5,num_htrials=2)

p36=get_res(dats=d13_10,alpha_list=alpha_list,true_bias=.5,num_htrials=3)
p37=get_res(dats=d23_10,alpha_list=alpha_list,true_bias=.5,num_htrials=3)
p38=get_res(dats=d33_10,alpha_list=alpha_list,true_bias=.5,num_htrials=3)
p39=get_res(dats=d43_10,alpha_list=alpha_list,true_bias=.5,num_htrials=3)
p40=get_res(dats=d53_10,alpha_list=alpha_list,true_bias=.5,num_htrials=3)
p41=get_res(dats=d63_10,alpha_list=alpha_list,true_bias=.5,num_htrials=3)
p42=get_res(dats=d73_10,alpha_list=alpha_list,true_bias=.5,num_htrials=3)

p43=get_res(dats=d11_15,alpha_list=alpha_list,true_bias=.5,num_htrials=1)
p44=get_res(dats=d21_15,alpha_list=alpha_list,true_bias=.5,num_htrials=1)
p45=get_res(dats=d31_15,alpha_list=alpha_list,true_bias=.5,num_htrials=1)
p46=get_res(dats=d41_15,alpha_list=alpha_list,true_bias=.5,num_htrials=1)
p47=get_res(dats=d51_15,alpha_list=alpha_list,true_bias=.5,num_htrials=1)
p48=get_res(dats=d61_15,alpha_list=alpha_list,true_bias=.5,num_htrials=1)
p49=get_res(dats=d71_15,alpha_list=alpha_list,true_bias=.5,num_htrials=1)

p50=get_res(dats=d12_15,alpha_list=alpha_list,true_bias=.5,num_htrials=2)
p51=get_res(dats=d22_15,alpha_list=alpha_list,true_bias=.5,num_htrials=2)
p52=get_res(dats=d32_15,alpha_list=alpha_list,true_bias=.5,num_htrials=2)
p53=get_res(dats=d42_15,alpha_list=alpha_list,true_bias=.5,num_htrials=2)
p54=get_res(dats=d52_15,alpha_list=alpha_list,true_bias=.5,num_htrials=2)
p55=get_res(dats=d62_15,alpha_list=alpha_list,true_bias=.5,num_htrials=2)
p56=get_res(dats=d72_15,alpha_list=alpha_list,true_bias=.5,num_htrials=2)

p57=get_res(dats=d13_15,alpha_list=alpha_list,true_bias=.5,num_htrials=3)
p58=get_res(dats=d23_15,alpha_list=alpha_list,true_bias=.5,num_htrials=3)
p59=get_res(dats=d33_15,alpha_list=alpha_list,true_bias=.5,num_htrials=3)
p60=get_res(dats=d43_15,alpha_list=alpha_list,true_bias=.5,num_htrials=3)
p61=get_res(dats=d53_15,alpha_list=alpha_list,true_bias=.5,num_htrials=3)
p62=get_res(dats=d63_15,alpha_list=alpha_list,true_bias=.5,num_htrials=3)
p63=get_res(dats=d73_15,alpha_list=alpha_list,true_bias=.5,num_htrials=3)

remove(d11_10,d11_15,d11_6,d12_10,d12_15,d12_6,d13_10,d13_15,d13_6,d21_10,d21_15,d21_6,
       d22_10,d22_15,d22_6,d23_10,        
       d23_15,d23_6, d31_10,d31_15,        
       d31_6, d32_10,d32_15,d32_6,
       d33_10,d33_15,d33_6, d41_10,        
       d41_15,d41_6, d42_10,d42_15,        
       d42_6, d43_10,d43_15,d43_6,
       d51_10,d51_15,d51_6, d52_10,        
       d52_15,d52_6, d53_10,d53_15,        
       d53_6, d61_10,d61_15,d61_6,
       d62_10,d62_15,d62_6, d63_10,        
       d63_15,d63_6, d71_10,d71_15,        
       d71_6, d72_10,d72_15,d72_6,
       d73_10,d73_15,d73_6, data_create,   
       get_historical, get_stan_dat,sim_data)

save.image("/.5_2.RData")


#######################################################################################  B_C=1
load("/dats1.RData")
p1=get_res(dats=d11_6,alpha_list=alpha_list,true_bias=1,num_htrials=1)
p2=get_res(dats=d21_6,alpha_list=alpha_list,true_bias=1,num_htrials=1)
p3=get_res(dats=d31_6,alpha_list=alpha_list,true_bias=1,num_htrials=1)
p4=get_res(dats=d41_6,alpha_list=alpha_list,true_bias=1,num_htrials=1)
p5=get_res(dats=d51_6,alpha_list=alpha_list,true_bias=1,num_htrials=1)
p6=get_res(dats=d61_6,alpha_list=alpha_list,true_bias=1,num_htrials=1)
p7=get_res(dats=d71_6,alpha_list=alpha_list,true_bias=1,num_htrials=1)

p8=get_res(dats=d12_6,alpha_list=alpha_list,true_bias=1,num_htrials=2)
p9=get_res(dats=d22_6,alpha_list=alpha_list,true_bias=1,num_htrials=2)
p10=get_res(dats=d32_6,alpha_list=alpha_list,true_bias=1,num_htrials=2)
p11=get_res(dats=d42_6,alpha_list=alpha_list,true_bias=1,num_htrials=2)
p12=get_res(dats=d52_6,alpha_list=alpha_list,true_bias=1,num_htrials=2)
p13=get_res(dats=d62_6,alpha_list=alpha_list,true_bias=1,num_htrials=2)
p14=get_res(dats=d72_6,alpha_list=alpha_list,true_bias=1,num_htrials=2)

p15=get_res(dats=d13_6,alpha_list=alpha_list,true_bias=1,num_htrials=3)
p16=get_res(dats=d23_6,alpha_list=alpha_list,true_bias=1,num_htrials=3)
p17=get_res(dats=d33_6,alpha_list=alpha_list,true_bias=1,num_htrials=3)
p18=get_res(dats=d43_6,alpha_list=alpha_list,true_bias=1,num_htrials=3)
p19=get_res(dats=d53_6,alpha_list=alpha_list,true_bias=1,num_htrials=3)
p20=get_res(dats=d63_6,alpha_list=alpha_list,true_bias=1,num_htrials=3)
p21=get_res(dats=d73_6,alpha_list=alpha_list,true_bias=1,num_htrials=3)

p22=get_res(dats=d11_10,alpha_list=alpha_list,true_bias=1,num_htrials=1)
p23=get_res(dats=d21_10,alpha_list=alpha_list,true_bias=1,num_htrials=1)
p24=get_res(dats=d31_10,alpha_list=alpha_list,true_bias=1,num_htrials=1)
p25=get_res(dats=d41_10,alpha_list=alpha_list,true_bias=1,num_htrials=1)
p26=get_res(dats=d51_10,alpha_list=alpha_list,true_bias=1,num_htrials=1)
p27=get_res(dats=d61_10,alpha_list=alpha_list,true_bias=1,num_htrials=1)
p28=get_res(dats=d71_10,alpha_list=alpha_list,true_bias=1,num_htrials=1)

p29=get_res(dats=d12_10,alpha_list=alpha_list,true_bias=1,num_htrials=2)
p30=get_res(dats=d22_10,alpha_list=alpha_list,true_bias=1,num_htrials=2)

remove(d11_10,d11_15,d11_6,d12_10,d12_15,d12_6,d13_10,d13_15,d13_6,d21_10,d21_15,d21_6,
       d22_10,d22_15,d22_6,d23_10,        
       d23_15,d23_6, d31_10,d31_15,        
       d31_6, d32_10,d32_15,d32_6,
       d33_10,d33_15,d33_6, d41_10,        
       d41_15,d41_6, d42_10,d42_15,        
       d42_6, d43_10,d43_15,d43_6,
       d51_10,d51_15,d51_6, d52_10,        
       d52_15,d52_6, d53_10,d53_15,        
       d53_6, d61_10,d61_15,d61_6,
       d62_10,d62_15,d62_6, d63_10,        
       d63_15,d63_6, d71_10,d71_15,        
       d71_6, d72_10,d72_15,d72_6,
       d73_10,d73_15,d73_6, data_create,   
       get_historical, get_stan_dat,sim_data)

#01.RData: Bias_current, split the results into 1 and then 2. 
save.image("/1_1.RData")


load("/dats1.RData")
p31=get_res(dats=d32_10,alpha_list=alpha_list,true_bias=1,num_htrials=2)
p32=get_res(dats=d42_10,alpha_list=alpha_list,true_bias=1,num_htrials=2)
p33=get_res(dats=d52_10,alpha_list=alpha_list,true_bias=1,num_htrials=2)
p34=get_res(dats=d62_10,alpha_list=alpha_list,true_bias=1,num_htrials=2)
p35=get_res(dats=d72_10,alpha_list=alpha_list,true_bias=1,num_htrials=2)

p36=get_res(dats=d13_10,alpha_list=alpha_list,true_bias=1,num_htrials=3)
p37=get_res(dats=d23_10,alpha_list=alpha_list,true_bias=1,num_htrials=3)
p38=get_res(dats=d33_10,alpha_list=alpha_list,true_bias=1,num_htrials=3)
p39=get_res(dats=d43_10,alpha_list=alpha_list,true_bias=1,num_htrials=3)
p40=get_res(dats=d53_10,alpha_list=alpha_list,true_bias=1,num_htrials=3)
p41=get_res(dats=d63_10,alpha_list=alpha_list,true_bias=1,num_htrials=3)
p42=get_res(dats=d73_10,alpha_list=alpha_list,true_bias=1,num_htrials=3)

p43=get_res(dats=d11_15,alpha_list=alpha_list,true_bias=1,num_htrials=1)
p44=get_res(dats=d21_15,alpha_list=alpha_list,true_bias=1,num_htrials=1)
p45=get_res(dats=d31_15,alpha_list=alpha_list,true_bias=1,num_htrials=1)
p46=get_res(dats=d41_15,alpha_list=alpha_list,true_bias=1,num_htrials=1)
p47=get_res(dats=d51_15,alpha_list=alpha_list,true_bias=1,num_htrials=1)
p48=get_res(dats=d61_15,alpha_list=alpha_list,true_bias=1,num_htrials=1)
p49=get_res(dats=d71_15,alpha_list=alpha_list,true_bias=1,num_htrials=1)

p50=get_res(dats=d12_15,alpha_list=alpha_list,true_bias=1,num_htrials=2)
p51=get_res(dats=d22_15,alpha_list=alpha_list,true_bias=1,num_htrials=2)
p52=get_res(dats=d32_15,alpha_list=alpha_list,true_bias=1,num_htrials=2)
p53=get_res(dats=d42_15,alpha_list=alpha_list,true_bias=1,num_htrials=2)
p54=get_res(dats=d52_15,alpha_list=alpha_list,true_bias=1,num_htrials=2)
p55=get_res(dats=d62_15,alpha_list=alpha_list,true_bias=1,num_htrials=2)
p56=get_res(dats=d72_15,alpha_list=alpha_list,true_bias=1,num_htrials=2)

p57=get_res(dats=d13_15,alpha_list=alpha_list,true_bias=1,num_htrials=3)
p58=get_res(dats=d23_15,alpha_list=alpha_list,true_bias=1,num_htrials=3)
p59=get_res(dats=d33_15,alpha_list=alpha_list,true_bias=1,num_htrials=3)
p60=get_res(dats=d43_15,alpha_list=alpha_list,true_bias=1,num_htrials=3)
p61=get_res(dats=d53_15,alpha_list=alpha_list,true_bias=1,num_htrials=3)
p62=get_res(dats=d63_15,alpha_list=alpha_list,true_bias=1,num_htrials=3)
p63=get_res(dats=d73_15,alpha_list=alpha_list,true_bias=1,num_htrials=3)

remove(d11_10,d11_15,d11_6,d12_10,d12_15,d12_6,d13_10,d13_15,d13_6,d21_10,d21_15,d21_6,
       d22_10,d22_15,d22_6,d23_10,        
       d23_15,d23_6, d31_10,d31_15,        
       d31_6, d32_10,d32_15,d32_6,
       d33_10,d33_15,d33_6, d41_10,        
       d41_15,d41_6, d42_10,d42_15,        
       d42_6, d43_10,d43_15,d43_6,
       d51_10,d51_15,d51_6, d52_10,        
       d52_15,d52_6, d53_10,d53_15,        
       d53_6, d61_10,d61_15,d61_6,
       d62_10,d62_15,d62_6, d63_10,        
       d63_15,d63_6, d71_10,d71_15,        
       d71_6, d72_10,d72_15,d72_6,
       d73_10,d73_15,d73_6, data_create,   
       get_historical, get_stan_dat,sim_data)

save.image("/1_2.RData")


#######################################################################################  B_C=1
load("/dats1.5.RData")
p1=get_res(dats=d11_6,alpha_list=alpha_list,true_bias=1.5,num_htrials=1)
p2=get_res(dats=d21_6,alpha_list=alpha_list,true_bias=1.5,num_htrials=1)
p3=get_res(dats=d31_6,alpha_list=alpha_list,true_bias=1.5,num_htrials=1)
p4=get_res(dats=d41_6,alpha_list=alpha_list,true_bias=1.5,num_htrials=1)
p5=get_res(dats=d51_6,alpha_list=alpha_list,true_bias=1.5,num_htrials=1)
p6=get_res(dats=d61_6,alpha_list=alpha_list,true_bias=1.5,num_htrials=1)
p7=get_res(dats=d71_6,alpha_list=alpha_list,true_bias=1.5,num_htrials=1)

p8=get_res(dats=d12_6,alpha_list=alpha_list,true_bias=1.5,num_htrials=2)
p9=get_res(dats=d22_6,alpha_list=alpha_list,true_bias=1.5,num_htrials=2)
p10=get_res(dats=d32_6,alpha_list=alpha_list,true_bias=1.5,num_htrials=2)
p11=get_res(dats=d42_6,alpha_list=alpha_list,true_bias=1.5,num_htrials=2)
p12=get_res(dats=d52_6,alpha_list=alpha_list,true_bias=1.5,num_htrials=2)
p13=get_res(dats=d62_6,alpha_list=alpha_list,true_bias=1.5,num_htrials=2)
p14=get_res(dats=d72_6,alpha_list=alpha_list,true_bias=1.5,num_htrials=2)

p15=get_res(dats=d13_6,alpha_list=alpha_list,true_bias=1.5,num_htrials=3)
p16=get_res(dats=d23_6,alpha_list=alpha_list,true_bias=1.5,num_htrials=3)
p17=get_res(dats=d33_6,alpha_list=alpha_list,true_bias=1.5,num_htrials=3)
p18=get_res(dats=d43_6,alpha_list=alpha_list,true_bias=1.5,num_htrials=3)
p19=get_res(dats=d53_6,alpha_list=alpha_list,true_bias=1.5,num_htrials=3)
p20=get_res(dats=d63_6,alpha_list=alpha_list,true_bias=1.5,num_htrials=3)
p21=get_res(dats=d73_6,alpha_list=alpha_list,true_bias=1.5,num_htrials=3)

p22=get_res(dats=d11_10,alpha_list=alpha_list,true_bias=1.5,num_htrials=1)
p23=get_res(dats=d21_10,alpha_list=alpha_list,true_bias=1.5,num_htrials=1)
p24=get_res(dats=d31_10,alpha_list=alpha_list,true_bias=1.5,num_htrials=1)
p25=get_res(dats=d41_10,alpha_list=alpha_list,true_bias=1.5,num_htrials=1)
p26=get_res(dats=d51_10,alpha_list=alpha_list,true_bias=1.5,num_htrials=1)
p27=get_res(dats=d61_10,alpha_list=alpha_list,true_bias=1.5,num_htrials=1)
p28=get_res(dats=d71_10,alpha_list=alpha_list,true_bias=1.5,num_htrials=1)

p29=get_res(dats=d12_10,alpha_list=alpha_list,true_bias=1.5,num_htrials=2)
p30=get_res(dats=d22_10,alpha_list=alpha_list,true_bias=1.5,num_htrials=2)

remove(d11_10,d11_15,d11_6,d12_10,d12_15,d12_6,d13_10,d13_15,d13_6,d21_10,d21_15,d21_6,
       d22_10,d22_15,d22_6,d23_10,        
       d23_15,d23_6, d31_10,d31_15,        
       d31_6, d32_10,d32_15,d32_6,
       d33_10,d33_15,d33_6, d41_10,        
       d41_15,d41_6, d42_10,d42_15,        
       d42_6, d43_10,d43_15,d43_6,
       d51_10,d51_15,d51_6, d52_10,        
       d52_15,d52_6, d53_10,d53_15,        
       d53_6, d61_10,d61_15,d61_6,
       d62_10,d62_15,d62_6, d63_10,        
       d63_15,d63_6, d71_10,d71_15,        
       d71_6, d72_10,d72_15,d72_6,
       d73_10,d73_15,d73_6, data_create,   
       get_historical, get_stan_dat,sim_data)

#01.RData: Bias_current, split the results into 1 and then 2. 
save.image("/1.5_1.RData")


load("/dats1.5.RData")
p31=get_res(dats=d32_10,alpha_list=alpha_list,true_bias=1.5,num_htrials=2)
p32=get_res(dats=d42_10,alpha_list=alpha_list,true_bias=1.5,num_htrials=2)
p33=get_res(dats=d52_10,alpha_list=alpha_list,true_bias=1.5,num_htrials=2)
p34=get_res(dats=d62_10,alpha_list=alpha_list,true_bias=1.5,num_htrials=2)
p35=get_res(dats=d72_10,alpha_list=alpha_list,true_bias=1.5,num_htrials=2)

p36=get_res(dats=d13_10,alpha_list=alpha_list,true_bias=1.5,num_htrials=3)
p37=get_res(dats=d23_10,alpha_list=alpha_list,true_bias=1.5,num_htrials=3)
p38=get_res(dats=d33_10,alpha_list=alpha_list,true_bias=1.5,num_htrials=3)
p39=get_res(dats=d43_10,alpha_list=alpha_list,true_bias=1.5,num_htrials=3)
p40=get_res(dats=d53_10,alpha_list=alpha_list,true_bias=1.5,num_htrials=3)
p41=get_res(dats=d63_10,alpha_list=alpha_list,true_bias=1.5,num_htrials=3)
p42=get_res(dats=d73_10,alpha_list=alpha_list,true_bias=1.5,num_htrials=3)

p43=get_res(dats=d11_15,alpha_list=alpha_list,true_bias=1.5,num_htrials=1)
p44=get_res(dats=d21_15,alpha_list=alpha_list,true_bias=1.5,num_htrials=1)
p45=get_res(dats=d31_15,alpha_list=alpha_list,true_bias=1.5,num_htrials=1)
p46=get_res(dats=d41_15,alpha_list=alpha_list,true_bias=1.5,num_htrials=1)
p47=get_res(dats=d51_15,alpha_list=alpha_list,true_bias=1.5,num_htrials=1)
p48=get_res(dats=d61_15,alpha_list=alpha_list,true_bias=1.5,num_htrials=1)
p49=get_res(dats=d71_15,alpha_list=alpha_list,true_bias=1.5,num_htrials=1)

p50=get_res(dats=d12_15,alpha_list=alpha_list,true_bias=1.5,num_htrials=2)
p51=get_res(dats=d22_15,alpha_list=alpha_list,true_bias=1.5,num_htrials=2)
p52=get_res(dats=d32_15,alpha_list=alpha_list,true_bias=1.5,num_htrials=2)
p53=get_res(dats=d42_15,alpha_list=alpha_list,true_bias=1.5,num_htrials=2)
p54=get_res(dats=d52_15,alpha_list=alpha_list,true_bias=1.5,num_htrials=2)
p55=get_res(dats=d62_15,alpha_list=alpha_list,true_bias=1.5,num_htrials=2)
p56=get_res(dats=d72_15,alpha_list=alpha_list,true_bias=1.5,num_htrials=2)

p57=get_res(dats=d13_15,alpha_list=alpha_list,true_bias=1.5,num_htrials=3)
p58=get_res(dats=d23_15,alpha_list=alpha_list,true_bias=1.5,num_htrials=3)
p59=get_res(dats=d33_15,alpha_list=alpha_list,true_bias=1.5,num_htrials=3)
p60=get_res(dats=d43_15,alpha_list=alpha_list,true_bias=1.5,num_htrials=3)
p61=get_res(dats=d53_15,alpha_list=alpha_list,true_bias=1.5,num_htrials=3)
p62=get_res(dats=d63_15,alpha_list=alpha_list,true_bias=1.5,num_htrials=3)
p63=get_res(dats=d73_15,alpha_list=alpha_list,true_bias=1.5,num_htrials=3)

remove(d11_10,d11_15,d11_6,d12_10,d12_15,d12_6,d13_10,d13_15,d13_6,d21_10,d21_15,d21_6,
       d22_10,d22_15,d22_6,d23_10,        
       d23_15,d23_6, d31_10,d31_15,        
       d31_6, d32_10,d32_15,d32_6,
       d33_10,d33_15,d33_6, d41_10,        
       d41_15,d41_6, d42_10,d42_15,        
       d42_6, d43_10,d43_15,d43_6,
       d51_10,d51_15,d51_6, d52_10,        
       d52_15,d52_6, d53_10,d53_15,        
       d53_6, d61_10,d61_15,d61_6,
       d62_10,d62_15,d62_6, d63_10,        
       d63_15,d63_6, d71_10,d71_15,        
       d71_6, d72_10,d72_15,d72_6,
       d73_10,d73_15,d73_6, data_create,   
       get_historical, get_stan_dat,sim_data)

save.image("/1.5_2.RData")



#######################################################################################  B_C=1
load("/dats2.RData")
p1=get_res(dats=d11_6,alpha_list=alpha_list,true_bias=2,num_htrials=1)
p2=get_res(dats=d21_6,alpha_list=alpha_list,true_bias=2,num_htrials=1)
p3=get_res(dats=d31_6,alpha_list=alpha_list,true_bias=2,num_htrials=1)
p4=get_res(dats=d41_6,alpha_list=alpha_list,true_bias=2,num_htrials=1)
p5=get_res(dats=d51_6,alpha_list=alpha_list,true_bias=2,num_htrials=1)
p6=get_res(dats=d61_6,alpha_list=alpha_list,true_bias=2,num_htrials=1)
p7=get_res(dats=d71_6,alpha_list=alpha_list,true_bias=2,num_htrials=1)

p8=get_res(dats=d12_6,alpha_list=alpha_list,true_bias=2,num_htrials=2)
p9=get_res(dats=d22_6,alpha_list=alpha_list,true_bias=2,num_htrials=2)
p10=get_res(dats=d32_6,alpha_list=alpha_list,true_bias=2,num_htrials=2)
p11=get_res(dats=d42_6,alpha_list=alpha_list,true_bias=2,num_htrials=2)
p12=get_res(dats=d52_6,alpha_list=alpha_list,true_bias=2,num_htrials=2)
p13=get_res(dats=d62_6,alpha_list=alpha_list,true_bias=2,num_htrials=2)
p14=get_res(dats=d72_6,alpha_list=alpha_list,true_bias=2,num_htrials=2)

p15=get_res(dats=d13_6,alpha_list=alpha_list,true_bias=2,num_htrials=3)
p16=get_res(dats=d23_6,alpha_list=alpha_list,true_bias=2,num_htrials=3)
p17=get_res(dats=d33_6,alpha_list=alpha_list,true_bias=2,num_htrials=3)
p18=get_res(dats=d43_6,alpha_list=alpha_list,true_bias=2,num_htrials=3)
p19=get_res(dats=d53_6,alpha_list=alpha_list,true_bias=2,num_htrials=3)
p20=get_res(dats=d63_6,alpha_list=alpha_list,true_bias=2,num_htrials=3)
p21=get_res(dats=d73_6,alpha_list=alpha_list,true_bias=2,num_htrials=3)

p22=get_res(dats=d11_10,alpha_list=alpha_list,true_bias=2,num_htrials=1)
p23=get_res(dats=d21_10,alpha_list=alpha_list,true_bias=2,num_htrials=1)
p24=get_res(dats=d31_10,alpha_list=alpha_list,true_bias=2,num_htrials=1)
p25=get_res(dats=d41_10,alpha_list=alpha_list,true_bias=2,num_htrials=1)
p26=get_res(dats=d51_10,alpha_list=alpha_list,true_bias=2,num_htrials=1)
p27=get_res(dats=d61_10,alpha_list=alpha_list,true_bias=2,num_htrials=1)
p28=get_res(dats=d71_10,alpha_list=alpha_list,true_bias=2,num_htrials=1)

p29=get_res(dats=d12_10,alpha_list=alpha_list,true_bias=2,num_htrials=2)
p30=get_res(dats=d22_10,alpha_list=alpha_list,true_bias=2,num_htrials=2)

remove(d11_10,d11_15,d11_6,d12_10,d12_15,d12_6,d13_10,d13_15,d13_6,d21_10,d21_15,d21_6,
       d22_10,d22_15,d22_6,d23_10,        
       d23_15,d23_6, d31_10,d31_15,        
       d31_6, d32_10,d32_15,d32_6,
       d33_10,d33_15,d33_6, d41_10,        
       d41_15,d41_6, d42_10,d42_15,        
       d42_6, d43_10,d43_15,d43_6,
       d51_10,d51_15,d51_6, d52_10,        
       d52_15,d52_6, d53_10,d53_15,        
       d53_6, d61_10,d61_15,d61_6,
       d62_10,d62_15,d62_6, d63_10,        
       d63_15,d63_6, d71_10,d71_15,        
       d71_6, d72_10,d72_15,d72_6,
       d73_10,d73_15,d73_6, data_create,   
       get_historical, get_stan_dat,sim_data)

#01.RData: Bias_current, split the results into 1 and then 2. 
save.image("/2_1.RData")


load("/dats2.RData")
p31=get_res(dats=d32_10,alpha_list=alpha_list,true_bias=2,num_htrials=2)
p32=get_res(dats=d42_10,alpha_list=alpha_list,true_bias=2,num_htrials=2)
p33=get_res(dats=d52_10,alpha_list=alpha_list,true_bias=2,num_htrials=2)
p34=get_res(dats=d62_10,alpha_list=alpha_list,true_bias=2,num_htrials=2)
p35=get_res(dats=d72_10,alpha_list=alpha_list,true_bias=2,num_htrials=2)

p36=get_res(dats=d13_10,alpha_list=alpha_list,true_bias=2,num_htrials=3)
p37=get_res(dats=d23_10,alpha_list=alpha_list,true_bias=2,num_htrials=3)
p38=get_res(dats=d33_10,alpha_list=alpha_list,true_bias=2,num_htrials=3)
p39=get_res(dats=d43_10,alpha_list=alpha_list,true_bias=2,num_htrials=3)
p40=get_res(dats=d53_10,alpha_list=alpha_list,true_bias=2,num_htrials=3)
p41=get_res(dats=d63_10,alpha_list=alpha_list,true_bias=2,num_htrials=3)
p42=get_res(dats=d73_10,alpha_list=alpha_list,true_bias=2,num_htrials=3)

p43=get_res(dats=d11_15,alpha_list=alpha_list,true_bias=2,num_htrials=1)
p44=get_res(dats=d21_15,alpha_list=alpha_list,true_bias=2,num_htrials=1)
p45=get_res(dats=d31_15,alpha_list=alpha_list,true_bias=2,num_htrials=1)
p46=get_res(dats=d41_15,alpha_list=alpha_list,true_bias=2,num_htrials=1)
p47=get_res(dats=d51_15,alpha_list=alpha_list,true_bias=2,num_htrials=1)
p48=get_res(dats=d61_15,alpha_list=alpha_list,true_bias=2,num_htrials=1)
p49=get_res(dats=d71_15,alpha_list=alpha_list,true_bias=2,num_htrials=1)

p50=get_res(dats=d12_15,alpha_list=alpha_list,true_bias=2,num_htrials=2)
p51=get_res(dats=d22_15,alpha_list=alpha_list,true_bias=2,num_htrials=2)
p52=get_res(dats=d32_15,alpha_list=alpha_list,true_bias=2,num_htrials=2)
p53=get_res(dats=d42_15,alpha_list=alpha_list,true_bias=2,num_htrials=2)
p54=get_res(dats=d52_15,alpha_list=alpha_list,true_bias=2,num_htrials=2)
p55=get_res(dats=d62_15,alpha_list=alpha_list,true_bias=2,num_htrials=2)
p56=get_res(dats=d72_15,alpha_list=alpha_list,true_bias=2,num_htrials=2)

p57=get_res(dats=d13_15,alpha_list=alpha_list,true_bias=2,num_htrials=3)
p58=get_res(dats=d23_15,alpha_list=alpha_list,true_bias=2,num_htrials=3)
p59=get_res(dats=d33_15,alpha_list=alpha_list,true_bias=2,num_htrials=3)
p60=get_res(dats=d43_15,alpha_list=alpha_list,true_bias=2,num_htrials=3)
p61=get_res(dats=d53_15,alpha_list=alpha_list,true_bias=2,num_htrials=3)
p62=get_res(dats=d63_15,alpha_list=alpha_list,true_bias=2,num_htrials=3)
p63=get_res(dats=d73_15,alpha_list=alpha_list,true_bias=2,num_htrials=3)

remove(d11_10,d11_15,d11_6,d12_10,d12_15,d12_6,d13_10,d13_15,d13_6,d21_10,d21_15,d21_6,
       d22_10,d22_15,d22_6,d23_10,        
       d23_15,d23_6, d31_10,d31_15,        
       d31_6, d32_10,d32_15,d32_6,
       d33_10,d33_15,d33_6, d41_10,        
       d41_15,d41_6, d42_10,d42_15,        
       d42_6, d43_10,d43_15,d43_6,
       d51_10,d51_15,d51_6, d52_10,        
       d52_15,d52_6, d53_10,d53_15,        
       d53_6, d61_10,d61_15,d61_6,
       d62_10,d62_15,d62_6, d63_10,        
       d63_15,d63_6, d71_10,d71_15,        
       d71_6, d72_10,d72_15,d72_6,
       d73_10,d73_15,d73_6, data_create,   
       get_historical, get_stan_dat,sim_data)

save.image("/2_2.RData")



#######################################################################################  B_C=2.5
load("/dats2.5.RData")
p1=get_res(dats=d11_6,alpha_list=alpha_list,true_bias=2.5,num_htrials=1)
p2=get_res(dats=d21_6,alpha_list=alpha_list,true_bias=2.5,num_htrials=1)
p3=get_res(dats=d31_6,alpha_list=alpha_list,true_bias=2.5,num_htrials=1)
p4=get_res(dats=d41_6,alpha_list=alpha_list,true_bias=2.5,num_htrials=1)
p5=get_res(dats=d51_6,alpha_list=alpha_list,true_bias=2.5,num_htrials=1)
p6=get_res(dats=d61_6,alpha_list=alpha_list,true_bias=2.5,num_htrials=1)
p7=get_res(dats=d71_6,alpha_list=alpha_list,true_bias=2.5,num_htrials=1)

p8=get_res(dats=d12_6,alpha_list=alpha_list,true_bias=2.5,num_htrials=2)
p9=get_res(dats=d22_6,alpha_list=alpha_list,true_bias=2.5,num_htrials=2)
p10=get_res(dats=d32_6,alpha_list=alpha_list,true_bias=2.5,num_htrials=2)
p11=get_res(dats=d42_6,alpha_list=alpha_list,true_bias=2.5,num_htrials=2)
p12=get_res(dats=d52_6,alpha_list=alpha_list,true_bias=2.5,num_htrials=2)
p13=get_res(dats=d62_6,alpha_list=alpha_list,true_bias=2.5,num_htrials=2)
p14=get_res(dats=d72_6,alpha_list=alpha_list,true_bias=2.5,num_htrials=2)

p15=get_res(dats=d13_6,alpha_list=alpha_list,true_bias=2.5,num_htrials=3)
p16=get_res(dats=d23_6,alpha_list=alpha_list,true_bias=2.5,num_htrials=3)
p17=get_res(dats=d33_6,alpha_list=alpha_list,true_bias=2.5,num_htrials=3)
p18=get_res(dats=d43_6,alpha_list=alpha_list,true_bias=2.5,num_htrials=3)
p19=get_res(dats=d53_6,alpha_list=alpha_list,true_bias=2.5,num_htrials=3)
p20=get_res(dats=d63_6,alpha_list=alpha_list,true_bias=2.5,num_htrials=3)
p21=get_res(dats=d73_6,alpha_list=alpha_list,true_bias=2.5,num_htrials=3)

p22=get_res(dats=d11_10,alpha_list=alpha_list,true_bias=2.5,num_htrials=1)
p23=get_res(dats=d21_10,alpha_list=alpha_list,true_bias=2.5,num_htrials=1)
p24=get_res(dats=d31_10,alpha_list=alpha_list,true_bias=2.5,num_htrials=1)
p25=get_res(dats=d41_10,alpha_list=alpha_list,true_bias=2.5,num_htrials=1)
p26=get_res(dats=d51_10,alpha_list=alpha_list,true_bias=2.5,num_htrials=1)
p27=get_res(dats=d61_10,alpha_list=alpha_list,true_bias=2.5,num_htrials=1)
p28=get_res(dats=d71_10,alpha_list=alpha_list,true_bias=2.5,num_htrials=1)

p29=get_res(dats=d12_10,alpha_list=alpha_list,true_bias=2.5,num_htrials=2)
p30=get_res(dats=d22_10,alpha_list=alpha_list,true_bias=2.5,num_htrials=2)

remove(d11_10,d11_15,d11_6,d12_10,d12_15,d12_6,d13_10,d13_15,d13_6,d21_10,d21_15,d21_6,
       d22_10,d22_15,d22_6,d23_10,        
       d23_15,d23_6, d31_10,d31_15,        
       d31_6, d32_10,d32_15,d32_6,
       d33_10,d33_15,d33_6, d41_10,        
       d41_15,d41_6, d42_10,d42_15,        
       d42_6, d43_10,d43_15,d43_6,
       d51_10,d51_15,d51_6, d52_10,        
       d52_15,d52_6, d53_10,d53_15,        
       d53_6, d61_10,d61_15,d61_6,
       d62_10,d62_15,d62_6, d63_10,        
       d63_15,d63_6, d71_10,d71_15,        
       d71_6, d72_10,d72_15,d72_6,
       d73_10,d73_15,d73_6, data_create,   
       get_historical, get_stan_dat,sim_data)

#01.RData: Bias_current, split the results into 1 and then 2. 
save.image("/2.5_1.RData")


load("/dats2.5.RData")
p31=get_res(dats=d32_10,alpha_list=alpha_list,true_bias=2.5,num_htrials=2)
p32=get_res(dats=d42_10,alpha_list=alpha_list,true_bias=2.5,num_htrials=2)
p33=get_res(dats=d52_10,alpha_list=alpha_list,true_bias=2.5,num_htrials=2)
p34=get_res(dats=d62_10,alpha_list=alpha_list,true_bias=2.5,num_htrials=2)
p35=get_res(dats=d72_10,alpha_list=alpha_list,true_bias=2.5,num_htrials=2)

p36=get_res(dats=d13_10,alpha_list=alpha_list,true_bias=2.5,num_htrials=3)
p37=get_res(dats=d23_10,alpha_list=alpha_list,true_bias=2.5,num_htrials=3)
p38=get_res(dats=d33_10,alpha_list=alpha_list,true_bias=2.5,num_htrials=3)
p39=get_res(dats=d43_10,alpha_list=alpha_list,true_bias=2.5,num_htrials=3)
p40=get_res(dats=d53_10,alpha_list=alpha_list,true_bias=2.5,num_htrials=3)
p41=get_res(dats=d63_10,alpha_list=alpha_list,true_bias=2.5,num_htrials=3)
p42=get_res(dats=d73_10,alpha_list=alpha_list,true_bias=2.5,num_htrials=3)

p43=get_res(dats=d11_15,alpha_list=alpha_list,true_bias=2.5,num_htrials=1)
p44=get_res(dats=d21_15,alpha_list=alpha_list,true_bias=2.5,num_htrials=1)
p45=get_res(dats=d31_15,alpha_list=alpha_list,true_bias=2.5,num_htrials=1)
p46=get_res(dats=d41_15,alpha_list=alpha_list,true_bias=2.5,num_htrials=1)
p47=get_res(dats=d51_15,alpha_list=alpha_list,true_bias=2.5,num_htrials=1)
p48=get_res(dats=d61_15,alpha_list=alpha_list,true_bias=2.5,num_htrials=1)
p49=get_res(dats=d71_15,alpha_list=alpha_list,true_bias=2.5,num_htrials=1)

p50=get_res(dats=d12_15,alpha_list=alpha_list,true_bias=2.5,num_htrials=2)
p51=get_res(dats=d22_15,alpha_list=alpha_list,true_bias=2.5,num_htrials=2)
p52=get_res(dats=d32_15,alpha_list=alpha_list,true_bias=2.5,num_htrials=2)
p53=get_res(dats=d42_15,alpha_list=alpha_list,true_bias=2.5,num_htrials=2)
p54=get_res(dats=d52_15,alpha_list=alpha_list,true_bias=2.5,num_htrials=2)
p55=get_res(dats=d62_15,alpha_list=alpha_list,true_bias=2.5,num_htrials=2)
p56=get_res(dats=d72_15,alpha_list=alpha_list,true_bias=2.5,num_htrials=2)

p57=get_res(dats=d13_15,alpha_list=alpha_list,true_bias=2.5,num_htrials=3)
p58=get_res(dats=d23_15,alpha_list=alpha_list,true_bias=2.5,num_htrials=3)
p59=get_res(dats=d33_15,alpha_list=alpha_list,true_bias=2.5,num_htrials=3)
p60=get_res(dats=d43_15,alpha_list=alpha_list,true_bias=2.5,num_htrials=3)
p61=get_res(dats=d53_15,alpha_list=alpha_list,true_bias=2.5,num_htrials=3)
p62=get_res(dats=d63_15,alpha_list=alpha_list,true_bias=2.5,num_htrials=3)
p63=get_res(dats=d73_15,alpha_list=alpha_list,true_bias=2.5,num_htrials=3)

remove(d11_10,d11_15,d11_6,d12_10,d12_15,d12_6,d13_10,d13_15,d13_6,d21_10,d21_15,d21_6,
       d22_10,d22_15,d22_6,d23_10,        
       d23_15,d23_6, d31_10,d31_15,        
       d31_6, d32_10,d32_15,d32_6,
       d33_10,d33_15,d33_6, d41_10,        
       d41_15,d41_6, d42_10,d42_15,        
       d42_6, d43_10,d43_15,d43_6,
       d51_10,d51_15,d51_6, d52_10,        
       d52_15,d52_6, d53_10,d53_15,        
       d53_6, d61_10,d61_15,d61_6,
       d62_10,d62_15,d62_6, d63_10,        
       d63_15,d63_6, d71_10,d71_15,        
       d71_6, d72_10,d72_15,d72_6,
       d73_10,d73_15,d73_6, data_create,   
       get_historical, get_stan_dat,sim_data)

save.image("/2.5_2.RData")


#######################################################################################  B_C=2.5
load("/dats3.RData")
p1=get_res(dats=d11_6,alpha_list=alpha_list,true_bias=3,num_htrials=1)
p2=get_res(dats=d21_6,alpha_list=alpha_list,true_bias=3,num_htrials=1)
p3=get_res(dats=d31_6,alpha_list=alpha_list,true_bias=3,num_htrials=1)
p4=get_res(dats=d41_6,alpha_list=alpha_list,true_bias=3,num_htrials=1)
p5=get_res(dats=d51_6,alpha_list=alpha_list,true_bias=3,num_htrials=1)
p6=get_res(dats=d61_6,alpha_list=alpha_list,true_bias=3,num_htrials=1)
p7=get_res(dats=d71_6,alpha_list=alpha_list,true_bias=3,num_htrials=1)

p8=get_res(dats=d12_6,alpha_list=alpha_list,true_bias=3,num_htrials=2)
p9=get_res(dats=d22_6,alpha_list=alpha_list,true_bias=3,num_htrials=2)
p10=get_res(dats=d32_6,alpha_list=alpha_list,true_bias=3,num_htrials=2)
p11=get_res(dats=d42_6,alpha_list=alpha_list,true_bias=3,num_htrials=2)
p12=get_res(dats=d52_6,alpha_list=alpha_list,true_bias=3,num_htrials=2)
p13=get_res(dats=d62_6,alpha_list=alpha_list,true_bias=3,num_htrials=2)
p14=get_res(dats=d72_6,alpha_list=alpha_list,true_bias=3,num_htrials=2)

p15=get_res(dats=d13_6,alpha_list=alpha_list,true_bias=3,num_htrials=3)
p16=get_res(dats=d23_6,alpha_list=alpha_list,true_bias=3,num_htrials=3)
p17=get_res(dats=d33_6,alpha_list=alpha_list,true_bias=3,num_htrials=3)
p18=get_res(dats=d43_6,alpha_list=alpha_list,true_bias=3,num_htrials=3)
p19=get_res(dats=d53_6,alpha_list=alpha_list,true_bias=3,num_htrials=3)
p20=get_res(dats=d63_6,alpha_list=alpha_list,true_bias=3,num_htrials=3)
p21=get_res(dats=d73_6,alpha_list=alpha_list,true_bias=3,num_htrials=3)

p22=get_res(dats=d11_10,alpha_list=alpha_list,true_bias=3,num_htrials=1)
p23=get_res(dats=d21_10,alpha_list=alpha_list,true_bias=3,num_htrials=1)
p24=get_res(dats=d31_10,alpha_list=alpha_list,true_bias=3,num_htrials=1)
p25=get_res(dats=d41_10,alpha_list=alpha_list,true_bias=3,num_htrials=1)
p26=get_res(dats=d51_10,alpha_list=alpha_list,true_bias=3,num_htrials=1)
p27=get_res(dats=d61_10,alpha_list=alpha_list,true_bias=3,num_htrials=1)
p28=get_res(dats=d71_10,alpha_list=alpha_list,true_bias=3,num_htrials=1)

p29=get_res(dats=d12_10,alpha_list=alpha_list,true_bias=3,num_htrials=2)
p30=get_res(dats=d22_10,alpha_list=alpha_list,true_bias=3,num_htrials=2)

remove(d11_10,d11_15,d11_6,d12_10,d12_15,d12_6,d13_10,d13_15,d13_6,d21_10,d21_15,d21_6,
       d22_10,d22_15,d22_6,d23_10,        
       d23_15,d23_6, d31_10,d31_15,        
       d31_6, d32_10,d32_15,d32_6,
       d33_10,d33_15,d33_6, d41_10,        
       d41_15,d41_6, d42_10,d42_15,        
       d42_6, d43_10,d43_15,d43_6,
       d51_10,d51_15,d51_6, d52_10,        
       d52_15,d52_6, d53_10,d53_15,        
       d53_6, d61_10,d61_15,d61_6,
       d62_10,d62_15,d62_6, d63_10,        
       d63_15,d63_6, d71_10,d71_15,        
       d71_6, d72_10,d72_15,d72_6,
       d73_10,d73_15,d73_6, data_create,   
       get_historical, get_stan_dat,sim_data)

#01.RData: Bias_current, split the results into 1 and then 2. 
save.image("/3_1.RData")


load("/dats3.RData")
p31=get_res(dats=d32_10,alpha_list=alpha_list,true_bias=3,num_htrials=2)
p32=get_res(dats=d42_10,alpha_list=alpha_list,true_bias=3,num_htrials=2)
p33=get_res(dats=d52_10,alpha_list=alpha_list,true_bias=3,num_htrials=2)
p34=get_res(dats=d62_10,alpha_list=alpha_list,true_bias=3,num_htrials=2)
p35=get_res(dats=d72_10,alpha_list=alpha_list,true_bias=3,num_htrials=2)

p36=get_res(dats=d13_10,alpha_list=alpha_list,true_bias=3,num_htrials=3)
p37=get_res(dats=d23_10,alpha_list=alpha_list,true_bias=3,num_htrials=3)
p38=get_res(dats=d33_10,alpha_list=alpha_list,true_bias=3,num_htrials=3)
p39=get_res(dats=d43_10,alpha_list=alpha_list,true_bias=3,num_htrials=3)
p40=get_res(dats=d53_10,alpha_list=alpha_list,true_bias=3,num_htrials=3)
p41=get_res(dats=d63_10,alpha_list=alpha_list,true_bias=3,num_htrials=3)
p42=get_res(dats=d73_10,alpha_list=alpha_list,true_bias=3,num_htrials=3)

p43=get_res(dats=d11_15,alpha_list=alpha_list,true_bias=3,num_htrials=1)
p44=get_res(dats=d21_15,alpha_list=alpha_list,true_bias=3,num_htrials=1)
p45=get_res(dats=d31_15,alpha_list=alpha_list,true_bias=3,num_htrials=1)
p46=get_res(dats=d41_15,alpha_list=alpha_list,true_bias=3,num_htrials=1)
p47=get_res(dats=d51_15,alpha_list=alpha_list,true_bias=3,num_htrials=1)
p48=get_res(dats=d61_15,alpha_list=alpha_list,true_bias=3,num_htrials=1)
p49=get_res(dats=d71_15,alpha_list=alpha_list,true_bias=3,num_htrials=1)

p50=get_res(dats=d12_15,alpha_list=alpha_list,true_bias=3,num_htrials=2)
p51=get_res(dats=d22_15,alpha_list=alpha_list,true_bias=3,num_htrials=2)
p52=get_res(dats=d32_15,alpha_list=alpha_list,true_bias=3,num_htrials=2)
p53=get_res(dats=d42_15,alpha_list=alpha_list,true_bias=3,num_htrials=2)
p54=get_res(dats=d52_15,alpha_list=alpha_list,true_bias=3,num_htrials=2)
p55=get_res(dats=d62_15,alpha_list=alpha_list,true_bias=3,num_htrials=2)
p56=get_res(dats=d72_15,alpha_list=alpha_list,true_bias=3,num_htrials=2)

p57=get_res(dats=d13_15,alpha_list=alpha_list,true_bias=3,num_htrials=3)
p58=get_res(dats=d23_15,alpha_list=alpha_list,true_bias=3,num_htrials=3)
p59=get_res(dats=d33_15,alpha_list=alpha_list,true_bias=3,num_htrials=3)
p60=get_res(dats=d43_15,alpha_list=alpha_list,true_bias=3,num_htrials=3)
p61=get_res(dats=d53_15,alpha_list=alpha_list,true_bias=3,num_htrials=3)
p62=get_res(dats=d63_15,alpha_list=alpha_list,true_bias=3,num_htrials=3)
p63=get_res(dats=d73_15,alpha_list=alpha_list,true_bias=3,num_htrials=3)

remove(d11_10,d11_15,d11_6,d12_10,d12_15,d12_6,d13_10,d13_15,d13_6,d21_10,d21_15,d21_6,
       d22_10,d22_15,d22_6,d23_10,        
       d23_15,d23_6, d31_10,d31_15,        
       d31_6, d32_10,d32_15,d32_6,
       d33_10,d33_15,d33_6, d41_10,        
       d41_15,d41_6, d42_10,d42_15,        
       d42_6, d43_10,d43_15,d43_6,
       d51_10,d51_15,d51_6, d52_10,        
       d52_15,d52_6, d53_10,d53_15,        
       d53_6, d61_10,d61_15,d61_6,
       d62_10,d62_15,d62_6, d63_10,        
       d63_15,d63_6, d71_10,d71_15,        
       d71_6, d72_10,d72_15,d72_6,
       d73_10,d73_15,d73_6, data_create,   
       get_historical, get_stan_dat,sim_data)

save.image("/3_2.RData")













