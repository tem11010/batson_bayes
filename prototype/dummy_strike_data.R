df0 <- data.frame(round = c(1:10), 
                  num_cog = c(3, 4, 4, 3, 2, 2, 2, 2, 2,2),
                  total = rep(9, 10), 
                  cog = c(0, 1, 1, 1, 1, 0, 1, 1, 1,0),
                  party = rep(c("PP","PD"),5)
)

party_choices <- c("PP","PD")


# dummy strike data

shist <- data.frame(round = c(1:10), 
                    num_cog = c(3, 4, 4, 3, 2, 2, 2, 2, 2,2),
                    total = rep(9, 10), 
                    cog = c(0, 1, 1, 1, 1, 0, 1, 1, 1,0),
                    party = rep(c("PP","PD"),5),
                    atty = factor(rep(c("Peter Parker","Matt Murdock"),5)),
                    firm = rep(c("Parker,Stan,and Lee","Hell's Kitchen Defense"),5)    
)

atty_levels <- c("None",levels(shist$atty))
atty_levels_p <- atty_levels
atty_levels_d <- atty_levels

cog_c_levels <- c("race","gender")