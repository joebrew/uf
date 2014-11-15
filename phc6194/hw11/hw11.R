# Homework 11: Disease Cluster Analysis -SaTscan
# PHC 6937: Spatial Epidemiology
# 
# Stl_home is a shapefile which includes St Louis region county 
# homicide counts (variable:HC8893) and population (variable:PO8893). 
# In this assignment, you are requested to perform a purely spatial 
# scan test based on Poisson model to detect any disease clusters 
# in the study region.
# 
# Steps: Import the data into SatScan and set up the session to run the analysis. 
# Present the Output of the cluster information for evaluation 
# (including the primary cluster and secondary cluster if necessary)


######
# Attach packages
#######
library(gdata)
library(dplyr)
library(maptools)
library(rgdal)
library(spatstat)
library(maps)
library(SpatialEpi)

######
# Set working directory to the haiti directories on local machine
######
if(Sys.info()["sysname"] == "Windows"){
  wd <- 'C:/Users/BrewJR/Documents/uf/phc6194/hw11'
} else {
  wd <- '/home/joebrew/Documents/uf/phc6194/hw11'
}
setwd(wd)

######
# Import the St Louis crime and population shapefile
######
stl <- readOGR("stl_hom", "stl_hom")

######
# Visually check read-in and projection by mapping 
# onto state of Missouri and Illinois
######
map("county", c("missouri", "illinois"),
    fill = TRUE,
    col = "grey",
    border = "white")
map("state", c("missouri", "illinois"),
    col = "black",
    add = TRUE,
    lwd = 2)
plot(stl,
     add = TRUE,
     col = adjustcolor("red", alpha.f = 0.3),
     border = "darkred")

######
# Rename some variables for comprehensibility
######
stl$hom <- stl$HC8893
stl$pop <- stl$PO8893

######
# Sanity check: population and homicide (absolute) should correlate
######
plot(log(stl$pop), log(stl$hom),
     xlab = "Population",
     ylab = "Homicides",
     xaxt = "n",
     yaxt = "n",
     main = "Population and homicides (log-log)",
     pch = 16,
     col = adjustcolor("darkorange", alpha.f = 0.8)) 
axis(side = 1,
     at = log(quantile(stl$pop, probs = c(0, 0.1, 0.4, 0.7, 0.8, 0.9, 1))),
     labels = round(quantile(stl$pop, probs = c(0, 0.1, 0.4, 0.7, 0.8, 0.9, 1)), digits = -3),
     cex.axis = 0.6)
axis(side = 2,
     at = log(quantile(stl$hom, probs = seq(0,1,0.1))),
     labels = round(quantile(stl$hom, probs = seq(0,1,0.1))),
     cex.axis = 0.6)


######
# Use SpatialEpi's kulldorff function for clustering detection
# (spatial scan)
######
#my_geo <- matrix(coordinates(stl), ncol = 2)
my_geo <- latlong2grid(coordinates(stl))
my_cases <- stl$hom
my_population <- stl$pop
n_strata <- nrow(stl)
expected_cases <- (stl$pop / sum(stl$pop)) * sum(stl$hom) #expected(my_population, my_cases, n_strata)

## Kulldorff using Poisson likelihoods
k_poisson <- kulldorff(geo = my_geo,
                       cases = my_cases,
                       population = my_population,
                       expected.cases = expected_cases,
                       pop.upper.bound = 0.5,
                       n.simulations = 999,
                       alpha.level = 0.05,
                       plot = TRUE)

cluster <- k_poisson$most.likely.cluster$location.IDs.included

## plot
plot(stl, col = "grey", border = "white")
plot(stl[cluster,],add=TRUE,col=adjustcolor("darkred", alpha.f = 0.6))
title(main = "Most Likely Cluster after 999 Monte-Carlo simulations\n(no secondary clusters identified)")

######
# PRINT INFORMATION
######
summary(k_poisson)
k_poisson$most.likely.cluster
#Cluster:
as.character(stl$NAME[cluster])
