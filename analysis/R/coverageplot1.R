# coverage plot1

p80 <- ggplot()+
  geom_tile(data = sims, aes(x = d, y = d_h,
                             fill = as.factor(sig.n)),
            color="black")+
  facet_grid(h~a, labeller = label_both)+
  scale_x_continuous(breaks = c(-3:3), expand = c(0,0))+
  scale_y_continuous(breaks = c(-3:3), expand = c(0,0))+
  xlab("Current b")+
  ylab("Historical b")+
  scale_fill_manual(values = c("white", "blue"))+
  coord_equal() +
  theme_minimal() +
  labs(fill = "coverage") +
  theme(axis.text = element_text(size =10), 
        #axis.title = element_text(size = 16), 
        #legend.title.align = 0.05, 
        #legend.title = element_text(size = 16), 
        legend.text = element_text(size = 14))
#p80

p90 <- ggplot()+
  geom_tile(data = sims, aes(x = d, y = d_h,
                             fill = as.factor(sig.n)),
            color="black")+
  facet_grid(h~a, labeller = label_both)+
  scale_x_continuous(breaks = c(-3:3), expand = c(0,0))+
  scale_y_continuous(breaks = c(-3:3), expand = c(0,0))+
  xlab("Current b")+
  ylab("Historical b")+
  scale_fill_manual(values = c("white", "blue"))+
  coord_equal()+
  theme_minimal()+
  labs(fill = "coverage") +
  theme(axis.text = element_text(size =10), 
        axis.title = element_text(size = 16),
        legend.title.align = 0.05, 
        legend.title = element_text(size = 16), 
        legend.text = element_text(size = 14))
#p90

p95 <- ggplot()+
  geom_tile(data = sims, aes(x = d, y = d_h,
                             fill = as.factor(sig.n)),
            color="black")+
  facet_grid(h~a, labeller = label_both)+
  scale_x_continuous(breaks = c(-3:3), expand = c(0,0))+
  scale_y_continuous(breaks = c(-3:3), expand = c(0,0))+
  xlab("Current b")+
  ylab("Historical b")+
  scale_fill_manual(values = c("white", "blue"))+
  coord_equal()+
  theme_classic()+  
  labs(fill = "coverage") +
  theme(axis.text = element_text(size =10), 
        axis.title = element_text(size = 16), 
        legend.title.align = 0.05, 
        legend.title = element_text(size = 16), 
        legend.text = element_text(size = 14))

#p95
