library(shiny)
library(rhandsontable)
library(dplyr)
library(mcmc)
library(Rcpp)
library(ggplot2)
Rcpp::sourceCpp("mh_sampler.cpp")

# Load starting dataframe 

df0 <- data.frame(round = c(1:10), 
                  num_cog = c(3, 4, 4, 3, 2, 2, 2, 2, 2,2),
                  total = rep(9, 10), 
                  cog = c(0, 1, 1, 1, 1, 0, 0, 0, 1,0),
                  party = rep(c("PP","PD"),5)
                  )

party_choices <- c("PP","PD")

# Define UI for application

ui <- fluidPage(

    # Application title
    titlePanel("Batson App"),

    fluidRow(
        column(4,
               rHandsontableOutput("hot")
        ),
        column(6,
               plotOutput("plot"),
               "Explanatory text here",
               
        )
            )
)

# Define server logic 
server <- function(input, output, session) {
    
    output$hot = renderRHandsontable({
        
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
        
        out_p <- make_posterior(x = df_md, niter = 11000, theta_start_val = 0,theta_proposal_sd =.5)
        out_d <- make_posterior(x = df_mp, niter = 11000, theta_start_val = 0,theta_proposal_sd =.5)
        
        # add back the party variable and combine into single dataframe
        
        d_p <- data.frame(
            theta = out_p$theta[1001:11000],
            party = "Prosecution")
        
        d_d <- data.frame(
            theta = out_d$theta[1001:11000],
            party = "Defense")
        
        dat <- rbind(d_p,d_d)
        
        # plot
        
        pplot <- ggplot(dat, aes(x = theta, fill = party)) + 
            geom_density(alpha = 0.3) +
            facet_grid(rows = vars(party))
        
        # labels and theme
        
        pplot <- pplot  + theme_minimal() +
            theme(legend.position="none") +
            labs (title = "Posterior density of d",
                  subtitle = "80% HDI") +
            xlab("") + ylab("") + 
            xlim(-6,6) +
            scale_color_manual("Group",values = c("blue","darkred")) +
            scale_fill_manual("Group", values = c("blue","darkred"))
        
        # add 80% credible interval
        
        CI <- dat %>%
            group_by(party) %>%
            summarise(q1 = quantile(theta,0.1), q2 = quantile(theta,0.9))
        
        pplot + geom_vline(data=CI, aes(xintercept=q1, colour=party),
                           linetype="dashed", size = 0.9)+
                geom_vline(data=CI, aes(xintercept=q2,colour=party),
                       linetype="dashed", size = 0.9)

    })
}

# Run the application 
shinyApp(ui = ui, server = server)
