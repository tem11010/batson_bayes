
# Code for generating plots of simultaneous strike data
# output from the 'simultaneous.R' script. 
#-----------------------------------------------------------------#

library(dplyr)
library(tidyr)
library(stringr)
library(sjPlot)

load("data/simul_4.19.22.RData")

simul_1[[2]]
simul_2[[2]]
simul_3[[2]]


sm11 <- simul_1[[2]]
sm22 <- simul_2[[2]]
sm33 <- simul_3[[2]]

sm11$b <- "b_[curr] %~~% 1"
sm22$b <- "b_[curr] %~~% 2"
sm33$b <- "b_[curr] %~~% 3"

comb_sm <- rbind(sm11, sm22, sm33)
comb_sm$sim <- rep(factor(paste("",50:1, sep =""),
                    levels = paste("",50:1, sep =""),ordered = TRUE), 
                   3)

b_set <- data.frame(mean_b = c(1, 2, 3), 
                    b = c("b_[curr] %~~% 1", "b_[curr] %~~% 2", "b_[curr] %~~% 3"))

ggplot(data = comb_sm)+
  facet_grid(~b, labeller = label_parsed)+
  geom_errorbarh(aes(y = sim,
                     xmin = LB_CI_95,
                     xmax = UB_CI_95), height = 0, 
                 color = "grey")+
  geom_point(aes(x = mean_b, y = sim))+
  xlab("Posterior mean of b (bias parameter)")+
  geom_vline(xintercept = 0, lty = 1, lwd = 1.1)+
  ylab("Simulation Number")+
  scale_x_continuous(breaks = c(0:6))+
  theme_sjplot2()+
  geom_vline(data = b_set, aes(xintercept = mean_b), color = "black", lty=2)+
  theme(panel.grid = element_blank(), 
        axis.title = element_text(size = 14), 
        axis.text.x = element_text(size = 12), 
        axis.text.y = element_text(size = 12), 
        strip.background = element_rect(fill="grey95"))
ggsave("simultaneous_sims_plot_23.jpg", width = 8, height = 8,
       units = "in", dpi = 300, bg = "white")


  