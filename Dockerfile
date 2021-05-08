#FROM openanalytics/r-base

# Base image https://hub.docker.com/u/rocker/
FROM rocker/shiny-verse:latest

# system libraries of general use
RUN apt-get update && apt-get install -y \
    sudo \
#    pandoc \
#    pandoc-citeproc \
    libcurl4-gnutls-dev \
    libcairo2-dev \
    libxt-dev \
    libssl-dev \
    libssh2-1-dev \
    libssl1.1 \
    && rm -rf /var/lib/apt/lists/*

# basic shiny functionality - unnecessary because using rocker/shiny-verse image from Dockerhub
# RUN R -e "install.packages(c('shiny', 'rmarkdown'), repos='https://cloud.r-project.org/')"

# install dependencies of the app - not necessary because using shiny-verse image
#RUN R -e "install.packages('ggplot2', repos='https://cloud.r-project.org/')"

# copy the app to the image
RUN mkdir /root/shinyapp
COPY app /root/shinyapp

RUN Rscript /root/shinyapp/dpp.R

EXPOSE 3838

CMD ["R", "-e", "shiny::runApp('/root/shinyapp', host='0.0.0.0', port=3838)"]
