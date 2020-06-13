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

atty_levels_p <- dat0 %>%
  pull("P_atty_l") %>%
  unlist() %>%
  unique() %>%
  str_trim() %>%
  factor()

atty_levels_d <- dat0 %>%
  pull("D_atty_l") %>%
  unlist() %>%
  unique() %>%
  str_trim() %>%
  factor()

atty_levels_p <- c("None",levels(atty_levels_p))
atty_levels_d <- c("None",levels(atty_levels_d))