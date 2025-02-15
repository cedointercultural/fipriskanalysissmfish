#to install rJava
#https://github.com/hannarud/r-best-practices/wiki/Installing-RJava-(Ubuntu)
#sudo add-apt-repository ppa:marutter/c2d4u3.5
#sudo apt-get update
#sudo apt-get install default-jdk
#sudo R CMD javareconf
#sudo apt-get install r-cran-rjava
#sudo apt-get install libgdal-dev libproj-dev
#Then in Rstudio install.packages("rJava")

#install.packages("testthat")

#install.packages("devtools")
#library(devtools)

#to install package Rfast
# sudo add-apt-repository ppa:dns/gnu
# sudo apt-get update
# sudo apt install libgsl-dev
#https://stackoverflow.com/questions/69583193/installing-r-gsl-package-on-ubuntu

devtools::install_github("valentinitnelav/geobuffer")

# needs terminal to provide password
#sudo apt-get install libmagick++-dev
#sudo apt-get install libgsl0-dev

.packages = c("tidyverse","devtools","raster","readxl",
              "sf","readr","stringr","data.table","Redmonder","psych",
              "geobuffer","stringr","Rfast","nFactors","rnaturalearth",
              "gridExtra","exactextractr","here", "patchwork","paletteer",
              "terrainr","plotrix","here","stringi","tidygeocoder",
              "Hmisc","stars","gstat","sp","factoextra","Rfast", "googlesheets4") 

#,"plyr", "tmap"
# Install CRAN packages (if not already installed)
.inst <- .packages %in% installed.packages()
if(length(.packages[!.inst]) > 0) install.packages(.packages[!.inst])

# Load packages into session 
lapply(.packages, require, character.only=TRUE)

#install.packages("rnaturalearthdata")
#install.packages("devtools") # I guess you also need this
#devtools::install_github("ropensci/rnaturalearthhires")
