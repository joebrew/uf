library(rgdal)

setwd("C:/Users/BrewJR/Documents/uf/phc7901/")
#setwd("/home/joebrew/Documents/uf/phc7901")
# tract
tract <- readOGR("lac_census_tracts", "tl_2010_06037_tract10")
plot(tract, col = sample(rainbow(nrow(tract)), nrow(tract)), border = FALSE)

#block
block <- readOGR("lac_census_blocks", "tl_2010_06037_tabblock10")

# Convert taact to an equal area projection
# list of projections: http://www.remotesensing.org/geotiff/proj_list/
# Albers equal area for California
#http://spatialreference.org/ref/sr-org/california-current-albers-equal-area-conic-wgs84/ 
tract2 <- spTransform(tract,
                      CRS("+proj=aea +lat_1=30 +lat_2=50 +lat_0=40 +lon_0=-125 +x_0=0 +y_0=0 +ellps=WGS84 +datum=WGS84 +units=m +no_defs"))
block2 <- spTransform(block,
                      CRS("+proj=aea +lat_1=30 +lat_2=50 +lat_0=40 +lon_0=-125 +x_0=0 +y_0=0 +ellps=WGS84 +datum=WGS84 +units=m +no_defs"))

# Tract2 is in meters, so this is in squared meters:
library(rgeos)
gArea(tract2)
gArea(block2)

# Compute areas for tract2 and block2
tract2$area <- NA
for (i in 1:nrow(tract2)){
  tract2$area[i] <- tract2@polygons[[i]]@area
}
block2$area <- NA
for (i in 1:nrow(block2)){
  block2$area[i] <- block2@polygons[[i]]@area
}


# Get square miles (from meters)
MetersToMiles <- function(x){
  x * (1/2589988.110336)
}
tract2$sqmiles <- MetersToMiles(tract2$area)
block2$sqmiles <- MetersToMiles(block2$area)
mean(tract2$sqmiles)
mean(block2$sqmiles)

# Get maximum inner-tract distance
tract2$maxdist <- NA
for (i in 1:nrow(tract2)){
  mycoords <- tract2@polygons[[i]]@Polygons[[1]]@coords
  # Loop through all edge coordinates to get furthest distance
  dist <- list()
  for (j in 1:nrow(mycoords)){
    z <- 
      sqrt(
        ((mycoords[j,1] - mycoords[,1])^2) + ((mycoords[j,2] - mycoords[,2])^2)
      )
    z <- max(z)
    dist[j] <- z
  }
  tract2$maxdist[i] <- max(unlist(dist))
}

block2$maxdist <- NA
for (i in 1:nrow(block2)){
  mycoords <- block2@polygons[[i]]@Polygons[[1]]@coords
  # Loop through all edge coordinates to get furthest distance
  dist <- list()
  for (j in 1:nrow(mycoords)){
    z <- 
      sqrt(
        ((mycoords[j,1] - mycoords[,1])^2) + ((mycoords[j,2] - mycoords[,2])^2)
      )
    z <- max(z)
    dist[j] <- z
  }
  block2$maxdist[i] <- max(unlist(dist))
}

# Average maximum possible distance in each tract:
mean(tract2$maxdist / 1609)
mean(block2$maxdist / 1609)

# Max dist in miles
tract2$maxdistmiles <- tract2$maxdist / 1609
block2$maxdistmiles <- block2$maxdist / 1609

# Get actual location of researchers
library(ggmap)
authors <- read.csv("authors.csv", stringsAsFactors = FALSE)
#authors$place[c(1,3,5,7)] <- "UCLA"
x <- geocode(authors$place)  

par(mar = c(0,0,0,0))
par(oma = c(0,0,0,0))
plot(tract, border = NA, 
     col = sample(adjustcolor(c("black", "darkgrey", "grey", "brown", "grey30"),
                              alpha.f = 0.3), 
                  nrow(tract), replace = TRUE),
     lwd = 0.1,
     ylim = c(33.6, 34.8))
text(x$lon, x$lat, col = "red",
     labels = authors$name,
     pos = c(1,1,2,2,3,3,4,4),
     cex = 0.6)
points(x$lon, x$lat, col = "darkred", pch = 16)
points(x$lon, x$lat, col = "black")



# Plot by maximum allowable distance
my_colors <- colorRampPalette(c("grey", "darkred"))(max(ceiling(tract2$maxdist)))
par(mfrow=c(1,2))
plot(tract2,
     col = my_colors[ceiling(tract2$maxdist)],
     border = "black",
     lwd = 0.1,
     ylim = c(-690000, -560000))
plot(block2,
     col = my_colors[ceiling(block2$maxdist)],
     border = "black",
     lwd = 0.1,
     ylim = c(-690000, -560000))

plot(1:10, 1:10, type = "n")
myprobs <- seq(0.1, 1, 0.1)
legend("center",
       fill = my_colors[round(quantile(1:length(my_colors),
                                       probs = myprobs))],
       border = NA,
       y.intersp = 0.5,
       legend = round(
         c(quantile(tract2$maxdist/1609,
                    probs = myprobs)[1],
           NA, NA, NA,
           quantile(tract2$maxdist/1609,
                    probs = myprobs)[5],
           NA, NA, NA,
           quantile(tract2$maxdist/1609,
                    probs = myprobs)[9],
           NA
           )
         ,
         digits = 1),
       bty = "n")

# Get 1 mile buffer for each polygon of tract2
tract2buf <- gBuffer(tract2[1:100,], width=sample(1:5000, 1609)) # need more ram to perform on all

par(mfrow = c(1,2))
plot(tract2[1:100,], col = adjustcolor("black", alpha.f = 0.7),
     border = NA)
plot(tract2[1:100,], col = adjustcolor("black", alpha.f = 0.7),
     border = NA)
plot(tract2buf, add = T,
     col = adjustcolor("red", alpha.f=0.5))

# Do some travel distance stuff in time
library(ggmap)
TravelTime <- function(from, to){
  mapdist(from, to, mode = "driving")
}

# Get centroids
centroids <- coordinates(tract2)

# Random place
mycentroid <- c(620000, -640000)

# Subset centroids to only include places nearby
centroids <- data.frame(centroids)
names(centroids) = c("x", "y")

centroids$distance <- NA
for (i in 1:nrow(centroids)){
  xdist <- centroids$x[i] - mycentroid[1]
  ydist <- centroids$y[i] - mycentroid[2]
  
  dist <- sqrt(((xdist)^2) + ((ydist)^2))
  
  centroids$distance[i] <- dist
}

centroids <- centroids[which(centroids$distance <= 3218),]

# convert to lat lon
coordinates(centroids) <- ~x+y
proj4string(centroids) <- proj4string(tract)
centroids <- spTransform(centroids, CRS(proj4string(tract)))

for (i in 1:nrow(centroids)){
  centroids$travel_time[i] <-
    TravelTime(from = , to)
}

plot(tract2, border = "grey", col = adjustcolor("grey", alpha.f = 0.6))
> points(centroids, pch = 16, cex = 0.1, col = "red")

pbuf <- gBuffer(centroids, width = 3218)
plot(pbuf, add = T)
# # Read in California data
# cal <- read.csv("california_census_data/ACS_10_5YR_DP05_with_ann.csv", skip = 0)
# 
# # Clean up tract
# cal$NAME10 <- as.numeric(gsub("[^0-9.]", "", cal$GEO.display.label))

# Generate 100,000 random points into sp object
x <- runif(n = 100000,
           min = 552431.3,
           max = 669941.4)
y <- runif(n = 100000,
           min = -792038.9,
           max = -555461.6)
rand <- data.frame("x" = x,
                   "y" = y)
coordinates(rand) <- ~x+y
proj4string(rand) <- proj4string(tract2)

# Of these 100,000, how many mistakenly fall into more than one?
x <- over(rand, polygons(tract2))











mycols <- sample(rainbow(length(x@polygons[[1]]@Polygons)),
                 length(x@polygons[[1]]@Polygons))

plot(x, border = NA, col = mycols)
plot(y, border = NA, col = adjustcolor(mycols, alpha.f = 0.5),
     add = TRUE)
