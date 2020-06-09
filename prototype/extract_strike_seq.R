# Create Strike Sequence Variable (from strike_num) for each case (ID)

## Premise: PP starts first (one strike), then PD (two strikes), then PP ...
## until all strikes used or foregone (skip)

library(dplyr)

dat0 <- readRDS("jury_data_cleaned.RDS")

extract_seq <- function(dat){
  
  dat$strike_num1 <- as.numeric(dat$strike_num)
  skipped <- as.numeric(union(setdiff(unique(dat$strike_num),seq(min(as.numeric(dat$strike_num)),max(as.numeric(dat$strike_num)))),setdiff(seq(min(as.numeric(dat$strike_num)),max(as.numeric(dat$strike_num))),unique(dat$strike_num))))
  if(length(skipped)>0){
    for(i in 1:length(skipped)){
      dat$strike_num1[which(dat$strike_num1>skipped[i])] = dat$strike_num1[which(dat$strike_num1>skipped[i])]-1
    }
  }
  ### find number of strikes by both PP and P
  
  n0 = length(dat$strike_num1)-sum(dat$strike_num1==0)-length(unique(dat$strike_num1))+1
  idx = intersect(which(dat$strike_num1<=n0),which(dat$strike_num1>0))
  
  ### find the second index of duplicated strike numbers
  second_index = intersect(which(duplicated(dat$strike_num1)),which(dat$strike_num1>0))
  
  dat$strike_seq[idx] = as.numeric(dat$strike_num1[idx])*2-1
  dat$strike_seq[second_index] = as.numeric(dat$strike_num1[second_index])*2
  dat$strike_seq[which(as.numeric(dat$strike_num1)>n0)] = as.numeric(dat$strike_num1[which(as.numeric(dat$strike_num1)>n0)])+n0
  return(dat)
}

dat1 <- dat0 %>% group_split(ID)
test_l <- lapply(1:length(dat1), function(x) extract_seq(dat1[[x]]))
dat1 = do.call(rbind.data.frame,test_l)

saveRDS(dat1, file = "jury_data_cleaned_new.rds")


