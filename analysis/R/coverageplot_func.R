coverageplot_func <- function(dat,CI,xlabel,ylabel){
  library(ggplot2)
  library(viridis)
  cust_labeller_a <- function(x) paste0("a = ", x)
  cust_labeller_h <- function(x) paste0("h = ", x)
  CI.d <- cut(CI, breaks = 5, dig.lab = 2)
  levels(CI.d)[1] <-   "(0,0.1]"
  ggplot()+
    geom_tile(data = dat, aes(x = d, y = d_h,fill = CI.d),
              color="black", alpha = 0.5)+
    facet_grid(h~a, 
               labeller = labeller(
                 h = cust_labeller_h, 
                 a = cust_labeller_a)
              ) +

    scale_fill_manual("Bias Detection Rate", values = rev(viridis(5)))+
    # scale_fill_gradient(
    #   'coverage', 
    #   low = "white",
    #   high = "blue", 
    #   #breaks = c(0,0.5,0.7,0.9, 1), 
    #   #limits = c(0,1),
    #   #labels = c("low", "", "medium", "","high")
    #   )+
    scale_x_continuous(breaks = c(-3:3), expand = c(0,0))+
    scale_y_continuous(breaks = c(-3:3), expand = c(0,0))+
    xlab(xlabel)+
    ylab(ylabel)+
    coord_equal()+
    theme_classic()+
    theme(legend.pos = "right",
          axis.text = element_text(size = 10), 
          axis.title = element_text(size = 16), 
          #legend.title.align = 0.05, 
          #legend.title = element_text(size = 16), 
          #legend.text = element_text(size = 14)
    )
}