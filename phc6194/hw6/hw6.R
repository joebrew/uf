# Load package
library(sp)
library(raster)
library(spatstat)
library(rgdal)

# Setwd
setwd("/home/joebrew/Documents/uf/phc6194/hw6")

# Read in objects and transform to same projection system

# Elevation (raster)
elevation <- readGDAL("elevation1.tif") # read in
relevation <- raster(elevation) # rasterlayer version
#ielevation <- as.im(elevation) #image version

# Hospitals (spatial dataframe)
hospitals <- readOGR(".", "Hospital")
hospitals <- spTransform(hospitals,
                          CRS(proj4string(elevation)))
# Land use (raster)
land_use <- readGDAL("Land_use.tif")
rland_use <- raster(land_use) # raster version

land_use <- spTransform(land_use,
                         CRS(proj4string(elevation)))
rland_use <- spTransform(rland_use,
                        CRS(proj4string(elevation)))

# Alachua boundary
boundary <- readOGR(".", "Alachua_Boundary")
boundary <- spTransform(boundary,
                        CRS(proj4string(elevation)))
#####
# Quick plot of elevation and hospitals to confirm that coordinate
# transformation worked
#####
#image(elevation, col= grey(1:99/100), axes=TRUE)
#plot(hospitals, col = "red", add = TRUE)
#image(land_use, col= adjustcolor(grey(1:99/100), alpha.f=0.2), axes=TRUE)

# Pick best place for new hospital.  
# It should be far from other hospitals and should be on appropriate land / slope

# First, let's generate a random set of possible points
x <- sample(seq(min(coordinates(elevation)[,1]), 
                max(coordinates(elevation)[,1]), 
                length = 10000), 
            10000)
y <- sample(seq(min(coordinates(elevation)[,2]), 
                max(coordinates(elevation)[,2]), 
                length = 10000), 
            10000)
posibs <- data.frame(cbind(x,y))
coordinates(posibs) <- ~x+y

# Now, let's get the land use for each of those points
posibs_land_use <- extract(rland_use, coordinates(posibs))

# Now let's get the elevation for each of those points
posibs_elevation <- extract(relevation, coordinates(posibs))

# Put land_use and elevation together
posibs_data <- data.frame("land_use" = posibs_land_use,
                          "elevation" = posibs_elevation)

# Bind it all into a spatial dataframe
posibs <- SpatialPointsDataFrame(coords = coordinates(posibs),
                                 data = posibs_data)

# Get right CRS / proj4string
proj4string(posibs) <- CRS(proj4string(elevation))

# So, now we have all these random points with their associated
# land_use and elevation
# This is called posibs, and it's a SpatialPointsDataFrame

# Now, let's keep only those points which are in Alachua County
x <- over(posibs, polygons(boundary))
posibs <- posibs[which(!is.na(x)),]

# Plot posibs to confirm that it works well
plot(posibs, col = adjustcolor("black", alpha.f = 0.1))

# Land use is a numeric coded object - and we have not been given instructions
# As far as how to code it! 
# Base on the (somewhat confusing) email, I'm assuming that 1-10 is good land 
# for hospitals and all others are bad?
# I'll recode accordingly
posibs$land_use <- ifelse(posibs$land_use >= 1 & posibs$land_use <= 10, "bad", "good")

# Now, let's keep only those possible locations with good land use
posibs <- posibs[which(posibs$land_use == "good"),]

# Let's plot to see what we're left with
plot(posibs, col = adjustcolor("black", alpha.f = 0.1))

# So, now we're left with about 50,000+ possible locations
# Let's get rid of those with bad slopes

# To get elevation change at each point, we'll
# need to make assumptions.  I'm assuming that the hospital
# will be 100 meters in all directions from a central point
library(rgeos)
buffers <- gBuffer(posibs, width = 100)
proj4string(buffers) <- CRS(proj4string(elevation))

# Get elevation differential for each area in buffers
zmax <- extract(relevation, 
                coordinates(posibs), 
                buffer = 100,
                fun = max,
                na.rm = TRUE) #max
zmin <- extract(relevation, 
                coordinates(posibs), 
                buffer = 100,
                fun = min,
                na.rm = TRUE) #min
# Now add total diferential for area back to posibs
posibs$slope <- zmax - zmin

# Get distance from nearest hospital
posibs$distance_from_nearest_hosp <- NA
for (i in 1:nrow(posibs)){
  hosp <- spDistsN1(coordinates(hospitals), pt = coordinates(posibs)[i,])
  posibs$distance_from_nearest_hosp[i] <-  min(hosp, na.rm = TRUE)
}

spDistsN1(coordinates(posibs), pt = coordinates(hospitals)[1,])

##################
# NOW FOR THE FINAL CALCULATIONS

# Let's scale both slope and distance from nearest hospital
posibs$slope_rec <- scale(posibs$slope)
posibs$distance_rec <- scale(posibs$distance_from_nearest_hosp)

# We wan't LOW slope but HIGH distance, so let's invert slope 
# so that both variables we want high
posibs$slope_rec <- posibs$slope_rec * -1

# I'm going to weight distance 3 times as much as slope,
# since slope can be fixed through modern engineering
posibs$distance_rec <- posibs$distance_rec *2

# Assign a score (at this point, it's just a combo of slope_rec and distance_rec,
# since I've already weighted)
posibs$score <- posibs$distance_rec + posibs$slope_rec

# Look at that score
hist(posibs$score)

# Make a color vector and plot
library(RColorBrewer)
myrange <- ceiling(max(posibs$score)) - ceiling(min(posibs$score))
mycols <- colorRampPalette(c("darkred",  "darkgreen"))(myrange + 1)
mycols <- adjustcolor(mycols, alpha.f = 0.4)
plot(posibs,
     pch = 16,
     cex = 0.6,
     col = mycols[ceiling(min(posibs$score)) + ceiling(posibs$score) + 1] )

# Best place to build new hospital
points(posibs[which(posibs$score == max(posibs$score)),],
       col = "darkblue", pch = 16,
       cex = 3)

# Haha - well, it's flat and far away from other hospitals...
# And I guess that's what I asked for.  :/