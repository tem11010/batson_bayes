# Create Strike Sequence Variable (from strike_num) for each case (ID)

## Premise: PP starts first (one strike), then PD (two strikes), then PP ...
## until all strikes used or foregone (skip)

library(dplyr)

dat0 <- readRDS("jury_data_cleaned.RDS")

dat1 <- dat0 %>% group_by(ID)

### find number of strikes by both PP and P

n0 = length(dat1$strike_num)-sum(dat1$strike_num==0)-length(unique(dat1$strike_num))+1
idx = intersect(which(dat1$strike_num<=n0),which(dat1$strike_num>0))

### find the second index of duplicated strike numbers
second_index = intersect(which(duplicated(dat1$strike_num)),which(dat1$strike_num>0))

dat1$strike_seq[idx] = as.numeric(dat1$strike_num[idx])*2-1
dat1$strike_seq[second_index] = as.numeric(dat1$strike_num[second_index])*2
dat1$strike_seq[which(as.numeric(dat1$strike_num)>n0)] = as.numeric(dat1$strike_num[which(as.numeric(dat1$strike_num)>n0)])+n0

ungroup(dat1)