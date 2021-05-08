sim_seq <- function(num_seat,num_strikep,num_striked,p0,p_c){
  stopifnot(num_seat > 5, 
            num_strikep < 1,
            num_striked < 1, 
            p0 <=1,
            p0 >= 0, 
            p_c <=1,
            p_c >= 0,
            is.integer(num_seat),
            is.integer(num_strikep),
            is.integer(num_striked),
            is.numeric(p0),
            is.numeric(p_c)) # all must be TRUE
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
