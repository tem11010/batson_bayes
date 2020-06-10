# Create Strike Sequence Variable (from strike_num) for each case (ID)

## Premise: PP starts first (one strike), then PD (two strikes), then PP ...
## until all strikes used or foregone (skip)

library(dplyr)

dat0 <- readRDS("jury_data_cleaned.RDS")

extract_seq <- function(dat){
  dat$strike_num <- as.numeric(dat$strike_num)
  dat = dat[order(dat$strike_num),]
  min0 <- min(dat$strike_num[which(dat$strike_num>0)])
  legth <- length(dat$strike_num[which(dat$strike_num>0)])
  dat$strike_seq[which(dat$strike_num>0)] <- seq(from=min0,to = min0+legth-1,by = 1)
  return(dat)
}

dat1 <- dat0 %>% group_split(ID)
test_l <- lapply(1:length(dat1), function(x) extract_seq(dat1[[x]]))
dat1 = do.call(rbind.data.frame,test_l)

saveRDS(dat1, file = "jury_data_cleaned_new.rds")

