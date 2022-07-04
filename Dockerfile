FROM rocker/rstudio:latest

RUN apt-get update && apt-get install -y libudunits2-dev libproj-dev libgdal-dev

RUN install2.r --error \
    tidyverse \
    tmap \
    shiny


