# Attach necessary spatial packages
library(rgdal)
library(gstat)
library(geoR)
library(RColorBrewer)

# Set wd
setwd("/home/joebrew/Documents/uf/phc6194/hw7")

# Read in data
s1 <- readOGR("data", "SubSample1")
s2 <- readOGR("data", "SubSample2")

# Plot
xcol <- colorRampPalette(c("green", "red"))(max(ceiling(s1$PM25)))
plot(s1, pch = 16, col = xcol[ceiling(s1$PM25)])
points(s2, col = "black", pch = "?")

# Polygonal interpolation ############

# Create thiessen / voronoi polygons
# first, function: 
# (from http://stackoverflow.com/questions/9403660/how-to-create-thiessen-polygons-from-points-using-r-packages)
voronoipolygons <- function(x) {
  require(deldir)
  require(sp)
  if (.hasSlot(x, 'coords')) {
    crds <- x@coords  
  } else crds <- x
  z <- deldir(crds[,1], crds[,2])
  w <- tile.list(z)
  polys <- vector(mode='list', length=length(w))
  for (i in seq(along=polys)) {
    pcrds <- cbind(w[[i]]$x, w[[i]]$y)
    pcrds <- rbind(pcrds, pcrds[1,])
    polys[[i]] <- Polygons(list(Polygon(pcrds)), ID=as.character(i))
  }
  SP <- SpatialPolygons(polys)
  voronoi <- SpatialPolygonsDataFrame(SP, data=data.frame(x=crds[,1],
                                                          y=crds[,2], row.names=sapply(slot(SP, 'polygons'), 
                                                                                       function(x) slot(x, 'ID'))))
}

# Create thissen polygon version of s1 (training data)
s1poly <- voronoipolygons(s1)

# Bring in data
s1poly@data <- s1@data

# Define projection string for s1poly
proj4string(s1poly) <- proj4string(s1)

# Which of s1's polygons do all the s2 points fall into?
x <- over(s2, polygons(s1poly))

# Get all of s1's appropriate values for the s2 points

# Monthly aod
s2$MonthlyAOD.p <- NA
for (i in 1:nrow(s2)){
  ind <- x[i]
  s2$MonthlyAOD.p[i] <-
    s1poly$MonthlyAOD[ind]
}

# PM25
s2$PM25.p <- NA
for (i in 1:nrow(s2)){
  ind <- x[i]
  s2$PM25.p[i] <-
    s1poly$PM25[ind]
}

# Plot Correlation between monthly AOD true and predicted
JoePlot <- function(x,y){
  plot(x,y,
       xlab = "Predicted",
       ylab = "Observed",
       pch = 16,
       col = adjustcolor("black", alpha.f = 0.5))
  mylm <- lm(y ~x)
  abline(mylm, col = adjustcolor("darkred", alpha.f = 0.6))
  mycor <- cor(x,y)
  text(0.9*max(x),
       1.2*min(y),
       labels = paste0("Correlation coefficient:\n", round(mycor, digits =2)))
}

# MONTHLY AOD
JoePlot(s2$MonthlyAOD.p, s2$MonthlyAOD)

# PM 25
JoePlot(s2$PM25.p, s2$PM25)

# Calculate error
ErrorCalc <- function(x,y){
  z <- x-y
  z[which(z <= 0)] <- z[which(z <= 0)] * -1
  return(z)
}
s2$error <- ErrorCalc(s2$PM25.p, s2$PM25)

# Explore error
hist(s2$error, main ="Histogram of error")

# Fill out table
mean(s2$PM25.p)
median(s2$PM25.p)
min(s2$PM25.p)
max(s2$PM25.p)
sd(s2$PM25.p)
mean(s2$error)
median(s2$error)
min(s2$error)
max(s2$error)
sd(s2$error)
mean(s2$error^2)

## TRIANGULATION #####################

# Calculate triangles
library(deldir)
s1tri <- deldir(x = coordinates(s1)[,1],
                y = coordinates(s1)[,2])

# plot
plot(s1tri, wl = "triang", pch = NA)

# calculate tile perimeters
tilePerim(tile.list(s1tri))

# Create list of triangles
trilist <- triang.list(s1tri)


# Make list of triangles a polygon object
mypolys <- list()
for (i in 1:length(trilist)){
  
  x <- rbind(cbind(trilist[[i]]$x, trilist[[i]]$y),
             cbind(trilist[[i]]$x[1], trilist[[i]]$y[1]))
  
  x <- Polygon(x)
  
  mypolys[i] <- x
}

# Create a spatial polygons obejct from my polys
mytris <- SpatialPolygons(list(Polygons(mypolys, ID = 1)))

# give proj4string
proj4string(mytris) <- proj4string(s1)

# Get the mean value of the three coordinates which make up
# the vertices of each triangle
# And populate a vector
val_vector <- vector(length = length(mytris@polygons[[1]]@Polygons),
                     mode = "numeric")
for (i in 1: length(mytris@polygons[[1]]@Polygons)){
  
  # Extract vertices
  mymat <- mytris@polygons[[1]]@Polygons[[i]]@coords
  
  # Remove fourth (since it's just the loop back to close the triangle)
  mymat <- mymat[-4,]
  
#   #make df
#   mymat <- data.frame(mymat)

  # Get corresponding values in s1
  myvals <- s1$PM25[which(round(coordinates(s1)[,1], digits = 1) %in% round(mymat[,1], digits = 1) &
                            round(coordinates(s1)[,2], digits = 1) %in% round(mymat[,2], digits = 1))]
  
  #Get mean
  myval <- mean(myvals)
  val_vector[i] <- myval
}


# Bind the triangles with the val_vector (the interpolated values) # won't work
# mytris2 <- SpatialPolygonsDataFrame(mytris@polygons[[1]]@Polygons, 
#                                     data.frame("predicted" = val_vector))

# Can't quite get this to work
plot(mytris) # prediction triangles
points(s2, col = "red")

## IDW ####################
library(spatstat)

# # make ppp object of coordinates s1 and s2
# s1ppp <- as.ppp(coordinates(s1),
#                 c(min(coordinates(s1)[,1]),
#                   max(coordinates(s1)[,1]),
#                   min(coordinates(s1)[,2]),
#                   max(coordinates(s1)[,2])))
# s2ppp <- as.ppp(coordinates(s2),
#                 c(min(coordinates(s2)[,1]),
#                   max(coordinates(s2)[,1]),
#                   min(coordinates(s2)[,2]),
#                   max(coordinates(s2)[,2])))
# idw(x = s1ppp,
#     power = 1,
#     at = s2ppp)


## KRIGING ############
# Define color vector (using colorbrewer)
my_colors <- colorRampPalette(c("darkgreen", "red"))(100)

#   boundary_points <- boundary@polygons[[1]]@Polygons
#   boundary_points <- boundary_points[[1]]@coords
  

  
  # Get trap locations and data values
  a <- data.frame("x" = coordinates(s1)[,1],
                  "y" = coordinates(s1)[,2],
                  "z" = s1$PM25)
  # Make into a geodata object
  b <- as.geodata(a)
  
  # Predict multiple points in Alachua County's boundary
  x <- seq(min(coordinates(s2)[,1]), max(coordinates(s2)[,1]), length = 100)
  y <- seq(min(coordinates(s2)[,2]), max(coordinates(s2)[,2]), length = 100)
  
  # Make a grid of those points
  pred.grid <- expand.grid(x,y)
  
  
  # kriging calculations
  kc <- krige.conv(geodata = b, coords = b$coords, data = b$data,
                   locations = pred.grid,
                   krige = krige.control(type.krige = "ok",
                                         cov.pars = c(10, 10000)))
  
  
  # Plot!
  # displaying predicted values
  image(kc, loc = pred.grid, 
        col = my_colors,
        xlab=NA, ylab=NA,
        xaxt = "n",
        yaxt = "n",
        xpd = NA,
        bty = "n")
  
  # Define percentiles for legend
  legtemp <-  round(quantile(kc$predict, probs = seq(0,1,, length = 10)))
  
  legend(x="topright",
         fill = my_colors[c(1,11,22,33,44,55,66,77,88,100)],
         legend = c(legtemp[1], NA, NA, legtemp[4], NA, NA, legtemp[7], NA, NA, legtemp[10]),
         border = FALSE,
         bty = "n",
         ncol = 1,
         y.intersp = 0.5,
         title = " KrigingInterpolation",
         cex = 0.75)

