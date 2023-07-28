sim_data <- function(d,round,total,num_cog){
  df0 <- data.frame(round = c(1:round), 
                    num_cog = NA, 
                    total = NA,
                    cog = NA)
  df0$num_cog[1] = num_cog
  df0$total[1] = total
  p_cog <- num_cog*exp(d)/(num_cog*exp(d)+total-num_cog)
  df0$cog[1] = rbinom(1,1,p_cog)
  for(i in 2:round){
    df0$num_cog[i] = df0$num_cog[i-1]-df0$cog[i-1]
    df0$total[i] = df0$total[i-1]-1
    p_cog <- df0$num_cog[i]*exp(d)/(df0$num_cog[i]*exp(d)+df0$total[i]-df0$num_cog[i])
    df0$cog[i] = rbinom(1,1,p_cog)
  }
  return(df0)
}
