#FROM openanalytics/r-base

FROM rocker/shiny-verse:latest
#FROM cocomcie/rshiny-base:v1.0

# system libraries of general use
#RUN apt-get update && apt-get install -y \
#    sudo \
#    pandoc \
#    pandoc-citeproc \
#    libcurl4-gnutls-dev \
#    libcairo2-dev \
#    libxt-dev \
#    libssl-dev \
#    libssh2-1-dev \
#    libssl1.1 \
#    && rm -rf /var/lib/apt/lists/*

# Shiny related packages
RUN R -e "install.packages(c('shinythemes'), repos='https://cloud.r-project.org/')"

# install dependencies of the app
# RUN R -e "install.packages('rhandsontable', 'Rcpp','mcmc', repos='https://cloud.r-project.org/')"

# copy the app to the image
RUN mkdir /root/shinyapp
COPY app /root/shinyapp

RUN Rscript /root/shinyapp/dpp.R

EXPOSE 3838

CMD ["R", "-e", "shiny::runApp('/root/shinyapp', host='0.0.0.0', port=3838)"]

