coverageplot_func <- function(dat,CI,xlabel,ylable){
  library(ggplot2)
  ggplot()+
    geom_tile(data = dat, aes(x = d, y = d_h,fill = CI),
              color="black")+
    facet_grid(h~a, labeller = labeller(
              h = label_value, 
              a = paste0("a = ",label_value))) +
    scale_fill_gradient(
      'coverage', 
      low = "white",
      high = "blue", 
      #breaks = c(0,0.5,0.7,0.9, 1), 
      #limits = c(0,1),
      #labels = c("low", "", "medium", "","high")
      )+
    scale_x_continuous(breaks = c(-3:3), expand = c(0,0))+
    scale_y_continuous(breaks = c(-3:3), expand = c(0,0))+
    xlab(xlabel)+
    ylab(ylabel)+
    coord_equal()+
    theme_classic()+
    theme(legend.pos = "bottom",
          axis.text = element_text(size = 10), 
          axis.title = element_text(size = 16), 
          #legend.title.align = 0.05, 
          #legend.title = element_text(size = 16), 
          #legend.text = element_text(size = 14)
    )
}