# Create Strike Sequence Variable (from strike_num) for each case (ID)

## Premise: PP starts first (one strike), then PD (two strikes), then PP ...
## until all strikes used or foregone (skip)

library(dplyr)

dat0 <- readRDS("jury_data_cleaned.RDS")

dat1 <- dat0 %>% group_by(ID)

### create a column with skipped strike to test ####
# dat1$strike_num0[which(dat1$strike_num0>5)]=dat1$strike_num0[which(dat1$strike_num0>5)]+1


### create a copied strike_number column and check if any skipped strike_number
### if yes, modify the copied column as if no skip and use the modified column to calculate strike_seq
dat1$strike_num1 <- as.numeric(dat1$strike_num)
skipped <- as.numeric(union(setdiff(unique(dat1$strike_num),seq(min(as.numeric(dat1$strike_num)),max(as.numeric(dat1$strike_num)))),setdiff(seq(min(as.numeric(dat1$strike_num)),max(as.numeric(dat1$strike_num))),unique(dat1$strike_num))))

if(length(skipped)>0){
    for(i in 1:length(skipped)){
    dat1$strike_num1[which(dat1$strike_num1>skipped[i])] = dat1$strike_num1[which(dat1$strike_num1>skipped[i])]-1
  }
}

### find number of strikes by both PP and P

n0 = length(dat1$strike_num1)-sum(dat1$strike_num1==0)-length(unique(dat1$strike_num1))+1
idx = intersect(which(dat1$strike_num1<=n0),which(dat1$strike_num1>0))

### find the second index of duplicated strike numbers
second_index = intersect(which(duplicated(dat1$strike_num1)),which(dat1$strike_num1>0))

dat1$strike_seq[idx] = as.numeric(dat1$strike_num1[idx])*2-1
dat1$strike_seq[second_index] = as.numeric(dat1$strike_num1[second_index])*2
dat1$strike_seq[which(as.numeric(dat1$strike_num1)>n0)] = as.numeric(dat1$strike_num1[which(as.numeric(dat1$strike_num1)>n0)])+n0

ungroup(dat1)