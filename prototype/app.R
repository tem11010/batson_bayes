library(shiny)
library(rhandsontable)
library(dplyr)
library(mcmc)
library(Rcpp)
library(ggplot2)
library(shinythemes)

Rcpp::sourceCpp(here::here("prototype","mh_sampler.cpp"))

# Load starting dataframe

df0 <- data.frame(round = c(1:10), 
                  num_cog = c(3, 4, 4, 3, 2, 2, 2, 2, 2,2),
                  total = rep(9, 10), 
                  cog = c(0, 1, 1, 1, 1, 0, 1, 1, 1,0),
                  party = rep(c("PP","PD"),5)
                  )

party_choices <- c("PP","PD")

# Define UI for application

ui <- fluidPage(

    theme = shinytheme("cerulean"),
    
    # Application title
    titlePanel("Batson App"),

    fluidRow(
        column(4,
               rHandsontableOutput("hot")
        ),
        column(6,
               plotOutput("plot"),
               "Explanatory text here."
               
        )
            )
)

# Define server logic 
server <- function(input, output, session) {
    
    output$hot <- renderRHandsontable({
        
        rhandsontable(df0, width = 600, height = 300) %>%
            hot_col("round", format = "0") %>%
            hot_col("num_cog", format = "0") %>%
            hot_col("total", format = "0") %>%
            hot_col("cog", format = "0") %>%
            hot_col("party", type = "dropdown", source = party_choices) %>%
            hot_validate_numeric(cols = 4, min = 0) %>%
            hot_context_menu(allowRowEdit = TRUE)
    })
    
    # Calculate and Plot the Posterior Distribution
    
    # set seed for consistent results when data don't change
    set.seed(1234)
    
    output$plot <- renderPlot({
        if(is.null(input$hot)) return(NULL)
        df0 <- hot_to_r(input$hot)
        
        df_mp <- df0 %>%
            filter(party == "PP") %>%
            select(-c(party)) %>%
            as.matrix()
        
        df_md <- df0 %>%
            filter(party == "PD") %>%
            select(-c(party)) %>%
            as.matrix()
        
        out_p <- make_posterior(x = df_mp, niter = 11000, theta_start_val = 0,theta_proposal_sd =.5)
        out_d <- make_posterior(x = df_md, niter = 11000, theta_start_val = 0,theta_proposal_sd =.5)
        
        # add back the party variable and combine into single dataframe
        
        d_p <- data.frame(
            theta = out_p$theta[1001:11000],
            party = "Prosecution")
        
        d_d <- data.frame(
            theta = out_d$theta[1001:11000],
            party = "Defense")
        
        dat <- rbind(d_p,d_d)
        
        ## calculate credible intervals
        
        CI <- dat %>%
            group_by(party) %>%
            summarise(q1 = quantile(theta,0.1), q2 = quantile(theta,0.9)) %>%
            mutate(bias = ifelse(
                q1 <= 0 & q2 >= q2, "No Bias", "Bias"))
        
        # plot
        
        pplot <- ggplot(dat, aes(x = theta, fill = party, height = 400, width = 600)) + 
            geom_density(alpha = 0.3) +
            facet_grid(rows = vars(party))
        
        ## labels and theme
        
        pplot <- pplot  + theme_minimal() +
            labs (title = "Posterior density of d") +
            xlab("") + 
            ylab("") + 
            xlim(c(-6,6)) +
            scale_color_manual("Group",values = c("blue","darkred")) +
            scale_fill_manual("Group", values = c("blue","darkred")) +
            
            # edit text sizes for plot
            theme(legend.position="none",
                  axis.text = element_text(size = 16), 
                  strip.text = element_text(size = 18), 
                  plot.title = element_text(size = 24),
                  plot.subtitle = element_text(size = 16)) +
            # add line at zero for reference
            geom_vline(xintercept = 0, color = "black", lwd=1.5)
        
        # add 80% credible interval

        pplot + geom_vline(data=CI, aes(xintercept=q1, colour=party),
                           linetype="dashed", size = 0.9)+
                geom_vline(data=CI, aes(xintercept=q2,colour=party),
                       linetype="dashed", size = 0.9) +
                labs(subtitle = paste("80% HDI: Defense = ",CI$bias[1],
                                      "; Prosecution = ", CI$bias[2]))
    })
    
}

# Run the application 
shinyApp(ui = ui, server = server)
