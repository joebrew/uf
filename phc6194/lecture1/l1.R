####################
# LOAD 
####################
library(maptools)
library(rgdal)
library(RColorBrewer)

####################
# SET WD TO THE GIS FOLDER
####################
setwd("C:/Users/BrewJR/Documents/uf/phc6194/lecture1")

#######
# COLORS
#######
mycols <- colorRampPalette(brewer.pal(8, "Greens"))(nrow(blocks)) 
mycols <- sample(mycols)

####################
# READ IN CENSUS TRACT-LEVEL DATA
####################

blocks <- readOGR("dataset",layer = "Alachua_County_Census_Blocks_tgr12001blk00")

ems <- readOGR("dataset", layer="fdem_ems_sep08_Project_Alachua_15_mile")
ems <- spTransform(ems, CRS(proj4string(blocks)))

boundary <- readOGR("dataset",layer = "Alachua_County_Boundary_tgr12001cty00")
boundary <- spTransform(boundary, CRS(proj4string(blocks)))

roads <- readOGR("dataset", layer = "Alachua_County_Roads_tgr12001lkA")
roads <- spTransform(roads, CRS(proj4string(blocks)))

water <- readOGR("dataset",
                 layer = "Alachua_County_water_bodies_tgr12001wat")
water <- spTransform(water, CRS(proj4string(blocks)))

plot(blocks,
     col=adjustcolor(mycols, alpha.f=0.7),
     border=FALSE)
plot(boundary, border = "grey", add=TRUE, lwd=1)
plot(water, add = TRUE, border = FALSE, col = adjustcolor("darkblue", alpha.f=0.6))
plot(roads, add = TRUE, col="black")
plot(ems, add=TRUE, border=FALSE, col=adjustcolor("darkred", alpha.f=0.5),
     cex=2, pch=16)
