#### dummy strike data ####

df0 <- data.frame(round = c(1:10), 
                  num_cog = c(3, 4, 4, 3, 2, 2, 2, 2, 2,2),
                  total = rep(9, 10), 
                  cog = c(0, 1, 1, 1, 1, 0, 1, 1, 1,0),
                  party = rep(c("PP","PD"),5)
)

party_choices <- c("PP","PD")


#### attorney name choices ####

dat0 <- readRDS(here::here("prototype","jury_data_cleaned_new.rds"))

atty_levels_p <- factor(dat0$P_atty_l)
atty_levels_d <- factor(dat0$D_atty_l)

atty_levels <- c("None",levels(shist$atty))
