cond.fn <- function(CI){
  CI.PP <- filter(CI, party=="PP")
    
  if(CI.PP$q1 < 0 & CI.PP$q2 < 0){
    print("Preference in PP") 
    
  }else if(CI.PP$q1 < 0 & CI.PP$q2 > 0) { 
    print("No bias or preference in PP")
    
  } else if (CI.PP$q1 > 0 & CI.PP$q2 > 0){
    print("Bias in PP")
    
  }
  
  CI.PD <- filter(CI, party=="PD")
  
  if(CI.PP$q1 < 0 & CI.PP$q2 < 0){
    print("Preference in PD") 
    
  }else if(CI.PP$q1 < 0 & CI.PP$q2 > 0) { 
    print("No bias or preference in PD")
    
  } else if (CI.PP$q1 > 0 & CI.PP$q2 > 0){
    print("Bias in PD")
    
  }
  
      
}

cond.fn(CI)
