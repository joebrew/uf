######
# Attach packages
#######
library(gdata)
library(dplyr)
library(maptools)
library(rgdal)

######
# Set working directory to the haiti directories on local machine
######
if(Sys.info()["sysname"] == "Windows"){
  wd <- 'C:/Users/BrewJR/Documents/uf/phc6194/hw10'
} else {
  wd <- '/home/joebrew/Documents/uf/phc6194/hw10'
}
setwd(wd)

##################
# DIRECTIONS
##################
# For this assignment you will investigate clustering of P. falciparum parasite 
# rates in an African country. Data and appropriate shapefiles are provided 
# through Sakai. Please perform the following tasks and include all requested 
# outputs and answers in a separate Word file numbered according to step:

##################
# 1. Choose a country to investigate, there are four options: Brazil, 
#    Ghana, Kenya, and Tanzania. State your choice in your word file.
##################

# I choose.... Ghana!

##################
# 2. Import the World and country shapefile of choice into ArcGIS. 
#    Import the country’s data.
##################
ghana_map <- readOGR("Ghana", "Ghana")
ghana_data <- read.csv("Ghana/Ghana_MAP_CSV.csv")

##################
# 3. Perform a global autocorrelation on the parasite rate 
#    variable with either ArcGIS or GeoDa. If using GeoDa, right 
#    click on plot and select “Display Statistics”. Copy html 
#    output if using ArcGIS or the Moran’s I plot if using GeoDa. 
#    Interpret these statistics.
##################


##################
# 4. Perform a local autocorrelation to show clusters 
#    using either ArcGIS (either local Moran’s I or Getis-Ord Gi) 
#    or GeoDa. Add any outputted maps to your Word file. Include 
#    legends and titles as appropriate. 
##################


##################
# 5. Describe you what you see in your maps in a few sentences.
##################

######
# 
######


######
# 
######


######
# 
######


######
# 
######


######
# 
######


######
# 
######


######
# 
######


######
# 
######


######
# 
######


######
# 
######


######
# 
######


######
# 
######


######
# 
######


######
# 
######


######
# 
######


######
# 
######

