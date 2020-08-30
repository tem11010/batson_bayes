library(shiny)
library(rhandsontable)
library(dplyr)
library(mcmc)
library(Rcpp)
library(ggplot2)
library(shinythemes)
library(markdown)

Rcpp::sourceCpp(here::here("prototype","mh_sampler_pp.cpp"))
Rcpp::sourceCpp(here::here("prototype","mh_sampler.cpp"))

# Load dummy strike dataframe
source(here::here("prototype","dummy_strike_data.R"))

# Load calc_prior function

source(here::here("prototype","calc_prior.R"))

# Define Cognitive Class choices

cog_c_levels <- c("race","gender")
 
# Define UI for application
    
ui <- fluidPage(

    #Navbar structure for UI
    
    navbarPage("Batson App - Proof of Concept", theme = shinytheme("paper"),
               
        tabPanel("Prototype", fluid = TRUE,
            sidebarPanel(
                   "Prior Strike History by Attorney",
                   selectInput("atty_d", "Defense:", 
                               choices=atty_levels_d,
                               selected = "None"),
                   selectInput("atty_p", "Prosecution:", 
                               choices=atty_levels_p,
                               selected = "None"),
                   selectInput("cog_c", "Cognizable Class:", choices=cog_c_levels),
                   radioButtons("weight", "Prior Strike History Weight:", inline=TRUE, selected = 1,
                                choiceValues = c(1, 0.5, 0.2),
                                choiceNames = c("Equal","Half","Minimal") 
                                    ),
                   hr(),
                   
                   "Current Strike Tally",
                   rHandsontableOutput("hot")
                ),
            mainPanel(
                actionButton("updateButton", "Update",
                             icon = icon("refresh")),
                plotOutput("plot"))
                ),
        
        tabPanel("About", fluid = TRUE,
                 includeMarkdown("explanatory-text.md"))
        #,
        #tabPanel("Model", fluid = TRUE,
        #         includeMarkdown("model_text.md"))
        ))

    
# Define server logic 
server <- function(input, output, session) {

        output$hot <- renderRHandsontable({
            
            rhandsontable(df0, width = 600, height = 700) %>%
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

            # strike tally for prosecutor
            df_mp <- df0 %>%
                filter(party == "PP") %>%
                select(-c(party)) %>%
                as.matrix()
            
            # strike tally for defense
            df_md <- df0 %>%
                filter(party == "PD") %>%
                select(-c(party)) %>%
                as.matrix()
            
            # specify prior values
            
            # Note: same priors regardless of cognizable class if atty = "None"
            # In the following function, we may add one line to take input value of a0,
            # which used to control the weight of historical information
            # if prosecutor or defense is none, we use the vague norm prior and the posterior is calculated
            # use the function in mh_sampler.cpp. if prosecutor or defense is not none, we use power prior 
            # and use function in mh_sampler_pp.cpp to draw posteriors
            
            weight<- as.numeric(input$weight)
            
            # prosecutor
            
            if (input$atty_p != "None"){
                sub_p <- subset(input$atty_p,TRUE,dat0,input$cog_c)
                out_p <- make_posterior_p(x = df_mp,x_p =sub_p,a0 = weight, niter = 110000, 
                                          theta_start_val = 0, theta_proposal_sd =.5, 
                                          prior_mean = 0, prior_sd = 2)
                pp_prior_theta <- make_posterior_prior(x_p=sub_p,a0 = weight, niter = 110000, 
                                                       theta_start_val = 0, theta_proposal_sd =.5, 
                                                       prior_mean = 0, prior_sd = 2)
                pp_prior <- data.frame(theta = pp_prior_theta$theta[10001:110000], 
                                       party = "Prosecution", 
                                       posterior = "Prior")
            }else{
              out_p <- make_posterior(x = df_mp,niter = 110000,theta_start_val = 0, theta_proposal_sd =.5, 
                                      prior_mean = 0, prior_sd = 2)
              pp_prior_theta <- rnorm(100000,0,2)
              pp_prior <- data.frame(theta = pp_prior_theta, 
                                     party = "Prosecution", 
                                     posterior = "Prior")
              
            }
            
            # defense
            
            if (input$atty_d != "None"){
                sub_d <- subset(input$atty_d,FALSE,dat0,input$cog_c)
                out_d <- make_posterior_p(x = df_md,x_p = sub_d,a0= weight, niter = 110000, 
                                          theta_start_val = 0, theta_proposal_sd =.5, 
                                          prior_mean = 0, prior_sd = 2)
                pd_prior_theta <- make_posterior_prior(x_p=sub_d,a0 = weight, niter = 110000, 
                                                       theta_start_val = 0, theta_proposal_sd =.5, 
                                                       prior_mean = 0, prior_sd = 2)
                
                
                pd_prior <- data.frame(theta = pd_prior_theta$theta[10001:110000], 
                                       party = "Defense", 
                                       posterior = "Prior")
            }else{
              out_d <- make_posterior(x = df_md,niter = 110000,theta_start_val = 0, theta_proposal_sd =.5, 
                                      prior_mean = 0, prior_sd = 2)
              pd_prior_theta <- rnorm(100000,0,2)
              pd_prior <- data.frame(theta = pd_prior_theta, 
                                     party = "Defense", 
                                     posterior = "Prior")
            }
            
            
            # add back the party variable and combine into single dataframe
            
            d_p <- data.frame(
                theta = out_p$theta[10001:110000],
                party = "Prosecution", 
                posterior = "Posterior")
            
            
            d_d <- data.frame(
                theta = out_d$theta[10001:110000],
                party = "Defense", 
                posterior = "Posterior")
            
            dat <- rbind(d_p,d_d)
            
            
            ## generate prior probability distributions
                              
            priors <- rbind(pp_prior, pd_prior)

            ## merge priors and posteriors
            dat <- rbind(dat, priors)
            
            ## calculate credible intervals
            
            ## specify party as factor
            dat$party <- factor(dat$party, levels = c("Defense", "Prosecution"), 
                                ordered = TRUE)
            
            CI <- dat %>% filter(posterior == 'Posterior') %>%
                group_by(party) %>%
                summarise(q1 = quantile(theta,0.1), q2 = quantile(theta,0.9)) %>%
                mutate(bias = ifelse(
                    q1 <= 0 & q2 >= 0, "No Bias", "Bias"))
            
            # plot
            
            pplot <- ggplot(data=dat) + 
                geom_density(aes(x = theta, 
                                 fill = interaction(party, posterior),
                                 color = interaction(party, posterior),
                                 alpha = interaction(party, posterior), 
                                 ..scaled..))+
                facet_wrap(~party, nrow =2)
            
            ## labels and theme
            
             pplot <- pplot  + theme_minimal() +
                 labs (title = "Posterior density of b") +
                 xlab("") + 
                 ylab("") + 
                 xlim(c(-6,6)) +
                 scale_fill_manual("Group", values = c("blue", "darkred", "grey", "grey")) +
                 scale_color_manual("Group", values = c("blue", "darkred", "grey", "grey")) +
                 scale_alpha_manual("Group", values = c(0.3, 0.3, 0.1, 0.1)) +
                
                  # edit text sizes for plot
                 theme(legend.position="none",
                       axis.text = element_text(size = 16), 
                       strip.text = element_text(size = 18), 
                       plot.title = element_text(size = 24),
                       plot.subtitle = element_text(size = 16)) +
                  # add line at zero for reference
                 geom_vline(xintercept = 0, color = "black", lwd=1.5)
            
            # add 80% credible interval
    
            pplot + geom_vline(data=CI, aes(xintercept=q1), color = c("blue", "darkred"),
                               linetype="dashed", size = 0.9)+
                    geom_vline(data=CI, aes(xintercept=q2), color = c("blue", "darkred"),
                           linetype="dashed", size = 0.9) 
                    #+
                    #labs(subtitle = paste("80% HDI: Defense = ",CI$bias[1],
                    #                      "; Prosecution = ", CI$bias[2]))
        })

    }
    
# Run the application 
shinyApp(ui = ui, server = server)
