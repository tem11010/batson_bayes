
#### simulation ####
#### p0 is the probability of striking non_cognizable class
#### pc is the probability of striking cognizable class
sim_seq <- function(num_seat,num_strikep,num_striked,p0,p_c){
  #### generate round, attorney, juror id, cognizable, strike, num_seats left, num_strike left p\d
  round = 1
  att <- c('PP','PD')
  att_idx <- 1
  jid <- 1
  ### jid1 used to consider the situation when the first party didn't strike
  jid1 <- 1
  cog <- rbinom(1,1,0.5)
  stk <- ifelse(cog==1,rbinom(1,1,p_c),rbinom(1,1,p0))
  nseat <- num_seat
  nstkp <- num_strikep
  nstkd <- num_striked
  dat0 <- c(round,att[att_idx],jid,cog,stk,nseat,nstkp,nstkd)
  nseat <- nseat
  nstkp <- nstkp - (2-att_idx)*stk
  nstkd <- nstkd + (1-att_idx)*stk
  jid1= (stk==0)
  while(nseat>=0 && (nstkp>0 || nstkd>0)){
    round = round+1
    if(stk==1){
      jid = jid+1
      att_idx <- 2-jid%%2
      cog = rbinom(1,1,0.5)
      if (att_idx==1 && nstkp==0){
        stk=0
      }else if(att_idx==2 && nstkd==0){
        stk=0
      }else{
        stk <- ifelse(cog==1,rbinom(1,1,p_c),rbinom(1,1,p0))
      }
     
      dat0 <- rbind(dat0,c(round,att[att_idx],jid,cog,stk,nseat,nstkp,nstkd))
      nseat <- nseat
      nstkp <- nstkp - (2-att_idx)*stk
      nstkd <- nstkd + (1-att_idx)*stk
      jid1= (stk==0)
      
    }else{
      if(jid1){
        jid = jid
        att_idx <- which(att!=att[att_idx])
        cog = cog
        if (att_idx==1 && nstkp==0){
          stk=0
        }else if(att_idx==2 && nstkd==0){
          stk=0
        }else{
          stk <- ifelse(cog==1,rbinom(1,1,p_c),rbinom(1,1,p0))
        }
        
        dat0 <- rbind(dat0,c(round,att[att_idx],jid,cog,stk,nseat,nstkp,nstkd))
        nseat <- nseat - (1-stk)
        nstkp <- nstkp - (2-att_idx)*stk
        nstkd <- nstkd + (1-att_idx)*stk
        jid1=FALSE
      }else{
        jid = jid+1
        att_idx <- 2-jid%%2
        cog = rbinom(1,1,0.5)
        if (att_idx==1 && nstkp==0){
          stk=0
        }else if(att_idx==2 && nstkd==0){
          stk=0
        }else{
          stk <- ifelse(cog==1,rbinom(1,1,p_c),rbinom(1,1,p0))
        }
        
        dat0 <- rbind(dat0,c(round,att[att_idx],jid,cog,stk,nseat,nstkp,nstkd))
        nseat <- nseat
        nstkp <- nstkp - (2-att_idx)*stk
        nstkd <- nstkd + (1-att_idx)*stk
        jid1= (stk==0)
      }
      
    }
  }
  colnames(dat0) <- c('round','att','juror_id','cognizable','strike','num_seat','num_strike_p','num_strike_d')
  
  return(dat0)
}


#### test  ###
#a = sim_seq(14,7,7,0.2,0.9)
### delete rows with 0 strikes left
#a = as.data.frame(a)
#if(min(which(a$num_strike_p==0))<nrow(a)){
  a1 = a[-which(a$num_strike_p==0 & a$att=='PP'),]
}else{
  a1 = a[-which(a$num_strike_d==0 & a$att=='PD'),]
}

#pp2 = a1[which(a1$att=='PP'),]
#pd2 <- a1[which(a1$att=='PD'),]

#b = bayesglm(strike~cognizable,data=pp2,family='binomial',prior.scale.for.intercept=2.5)
#summary(b)

### get the confidence interval of the estimated coefficient
#simulates <- coef(sim(b))
#hist(simulates[,1])
#quantile(simulates[,1],c(0.1,0.9))

