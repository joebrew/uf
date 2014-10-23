#####
# SET LOCAL WORKING DIRECTORY
#####
setwd("C:/Users/BrewJR/Documents/uf/phc6194/hw3/HW")

#####
# ATTACH PACKAGES
#####
library(maptools)
library(rgdal)
library(RColorBrewer)
library(Hmisc)
library(mapproj)
library(rgl)
library(raster)
library(spatstat)
library(classInt)

#####
# I FIRST EXPORTED THE LAYERS AS SHAPEFILES
# FROM ARCGIS, NOW I READ THEM INTO R
#####
county <- readOGR("HW",
                  layer = "usa_county")
boundary <- readOGR("HW",
                    layer = "Alachua_Boundary")
pop <- readOGR("HW",
               layer = "Alachua_CT_POP")
hospitals <- readOGR("HW",
                     layer = "FL_Hospitals")
# question2 <- readOGR("HW",
#              layer = "question2")

#####
# READ IN THE RASTER ELEVATION DATA
#####
ned <- readGDAL("HW/ned011.tif")

#####
# Convert some objects to ned's projection
#####
hospitals2 <- spTransform(hospitals,
                          CRS(proj4string(ned)))
boundary2 <- spTransform(boundary,
                         CRS(proj4string(ned)))
pop2 <- spTransform(pop,
                    CRS(proj4string(ned)))


#####
# Quick plot of ned and hospitals to confirm that coordinate
# transformation worked
#####
image(ned, col= grey(1:99/100), axes=TRUE)
plot(hospitals2, col = "red", add = TRUE)

#####
# Create a rasterlayer version of ned
#####
rned <- raster(ned)

#####
# Create an image version of ned
#####
ined <- as.im(ned)


# dned <- data.frame(ned)
# dhospitals2 <- data.frame(hospitals2)
# dhospitals2$x <- coordinates(hospitals2)[,1]
# dhospitals2$y <- coordinates(hospitals2)[,2]

#####
# Define a color vector and plot the image of ned
#####
mycols <- colorRampPalette(c("blue", "darkgreen", "grey", "white"))(60)
mycols <- adjustcolor(mycols, alpha.f=0.6)
plot(ined,
     col = mycols)
#points(hospitals2)

#####
# Use extract() to get the pixel values of the raster
# version of ned at the locations of the hospitals
#####
hospitals2$elevation <- extract(rned, coordinates(hospitals2))

#####
# Keep only hospitals which are in Alachua's borders
#####
x <- over(hospitals2, polygons(boundary2))
hospitals2 <- hospitals2[which(!is.na(x)),]
rm(x)


#####
# Plot hospitals with their elevation
#####
# Image
plot(ined,
     col = mycols,
     main = "Alachua hospitals and elevation")
#Points
points(hospitals2,
       col = adjustcolor("darkred", alpha.f=0.6),
       pch = 16)
# Text
text(x = coordinates(hospitals2)[,1],
     y = coordinates(hospitals2)[,2],
     labels = hospitals2$elevation, cex = 0.6,
     col = adjustcolor("black", alpha.f=0.7),
     pos = c(1,3))

#####
# READ IN CANCER DATA
#####
cancer <- read.csv("HW/cancer_fl.txt")

#####
# Fix names in the county shapefile and the cancer table
# In order to make them compatible
#####
cancer$name <- as.character(toupper(gsub(" County, FL", "", cancer$County)))  
county$name <- as.character(county$NAME2_)


# Loop to find closest match
cancer$newname <- NA
for (i in 1:nrow(cancer)){
  
  #Create matrix of match scores
  m <- adist(x = cancer$name[i],
             y = county$name,
             ignore.case = TRUE)
  
  #Select the index of the best (lowest) match
  best <- which(m == min(m), arr.ind=TRUE)[1,]
  best.ind <- as.numeric(best["col"])
  #best.ind <- which.min(best)
  
  #Assign to best.num the actual score of the best match
  best.num <- min(m)
  
  #Assign the best match to newad 
  cancer$newname[i] <- as.character(county$name[best.ind])
}

# See where matches weren't identical
x <- cancer$name[which(cancer$name != cancer$newname)]
y <- cancer$newname[which(cancer$name != cancer$newname)]
cbind(x,y) # perfect
rm(x,y)

# Since the match is good, let's replace name with newname
cancer$name <- cancer$newname
cancer$newname <- NULL

#####
# MERGE cancer AND county
#####
county <- merge(county, cancer)

######
# Make numerical variables actually behave nuemrically
######
MakeNum <- function(x){
  as.numeric(as.character(gsub(",", "", x)))
}
county$County_cod <- MakeNum(county$County_cod)
county$Deaths <- MakeNum(county$Deaths)
county$Population <- MakeNum(county$Population)
county$Crude_Rate <- MakeNum(county$Crude_Rate)
county$Age_Adjust <- MakeNum(county$Age_Adjust)

#####
# WRITE FUNCTION TO MAKE CHOROPLETH MAPS
#####
CountyFun <- function(var, color, percent = FALSE){
  plotvar <- var
  nclr <- 5
  plotclr <- brewer.pal(nclr, color)
  class <- classIntervals(plotvar, nclr, style = "quantile", dataPrecision=0) #use "equal" instead
  #class <- classIntervals(0:100, nclr, style="equal")
  colcode <- findColours(class, plotclr)
  
  if(percent){
    legcode <- paste0(gsub(",", " - ", gsub("[[]|[]]|[)]", "", names(attr(colcode, "table")))), "%")
  }else{
    legcode <- paste0(gsub(",", " - ", gsub("[[]|[]]|[)]", "", names(attr(colcode, "table")))))
  }
  
  plot(county, border=NA, col=colcode)
  legend("left", # position
         legend = legcode, #names(attr(colcode, "table")), 
         fill = attr(colcode, "palette"), 
         cex = 0.6, 
         border=NA,
         bty = "n")
}

CountyFun(county$Population / county$AREA_, "Blues")

#####
# CREATE GEOGRAPHICAL BUFFER
#####
library(rgeos)

# Check out the projection string of hospitals2 to confirm meters
proj4string(hospitals2)

# Create buffered shapefiles
mylist <- list()
for (i in 1:length(hospitals2$NAME)){
  mylist[i] <- 
    gBuffer(hospitals2[which(hospitals2$NAME == hospitals2$NAME[i]),], width = 5000)
}

# Unlist each hospital into its own object
hosp1 <- unlist(mylist[[1]])
hosp2 <- unlist(mylist[[2]])
hosp3 <- unlist(mylist[[3]])
hosp4 <- unlist(mylist[[4]])
hosp5 <- unlist(mylist[[5]])
hosp6 <- unlist(mylist[[6]])

# PLOT THE 5KM RADII
mycols <- colorRampPalette(c("brown", "white"))(60)
mycols <- adjustcolor(mycols, alpha.f=0.6)
mycols2 <- c("darkblue", "darkorange", "grey", "red", "purple", "brown")

# Elevation
plot(ined,
     col = mycols,
     main = NULL,
     ribbon = FALSE)
# County border
plot(boundary2,
     add = TRUE)
# Census tracts
plot(pop2,
     add = TRUE,
     col = adjustcolor("black", alpha.f = 0.1),
     border = adjustcolor("black", alpha.f=0.6))
#Points
points(hospitals2,
       col = adjustcolor(mycols2, alpha.f=0.8),
       pch = 16,
       cex = 0.4)
plot(hosp1, add = T, col = adjustcolor(mycols2[1], alpha.f=0.3), 
     border = adjustcolor(mycols[1], alpha.f=0.2))
plot(hosp2, add = T, col = adjustcolor(mycols2[2], alpha.f=0.3), 
     border = adjustcolor(mycols2[2], alpha.f=0.2))
plot(hosp3, add = T, col = adjustcolor(mycols2[3], alpha.f=0.3), 
     border = adjustcolor(mycols2[3], alpha.f=0.2))
plot(hosp4, add = T, col = adjustcolor(mycols2[4], alpha.f=0.3), 
     border = adjustcolor(mycols2[4], alpha.f=0.2))
plot(hosp5, add = T, col = adjustcolor(mycols2[5], alpha.f=0.3), 
     border = adjustcolor(mycols2[5], alpha.f=0.2))
plot(hosp6, add = T, col = adjustcolor(mycols2[6], alpha.f=0.3), 
     border = adjustcolor(mycols[6], alpha.f=0.2))

# Create a buffered shapefile
"hospitals2buffer" <- gBuffer(hospitals2, width = 1000)

#####
# GET POPULATION PER BUFFER ZONE
#####

# Define a function for calculating this
GetPop <- function(hospital){
 
  # Specify the object id's of the polygons of pop
  # which overlap with the buffer zone
  x <- over(pop2, polygons(hospital))
  
  # Sum up all those populations
  sum(pop2$pop[which(x %in% pop2$OBJECTID)],
      na.rm = TRUE)
}

# Calculate the total population for each buffer zone
GetPop(hosp1)
GetPop(hosp2)
GetPop(hosp3)
GetPop(hosp4)
GetPop(hosp5)
GetPop(hosp6)

# Task 3 answer
task3 <- data.frame(hospitals2$NAME)
names(task3) <- "Hospital"

task3$Population <- as.character(c(
  GetPop(hosp1),
  GetPop(hosp2),
  GetPop(hosp3),
  GetPop(hosp4),
  GetPop(hosp5),
  GetPop(hosp6)
  ))

#####
# SAVE IMAGE
#####
getwd()
save.image("HW/hw3.RData")
#####
#
#####


# RANDOM COUNTY PLOT
mycols3 <- colorRampPalette(c("blue", "darkgreen", "orange", "red"))(nrow(pop))

plot(pop, col = adjustcolor(rainbow(43), alpha.f = 0.7), border = "darkgrey")
plot(pop, col = adjustcolor(mycols3, alpha.f = 0.7), border = "darkgrey")


#####
#
#####


#####
#
#####







#####
#
#####


#####
#
#####


#####
#
#####


#####
#
#####


#####
#
#####


#####
#
#####


# hospitals2@data <- data.frame(hospitals2$data,
#                               rVal = extract(rned, coordinates(hospitals2)))
# 
# 
# xy@data <- data.frame(xy@data, rVal=extract(r, xy))
# str(xy@data)
# 
# ind_elevations <- nearest.raster.point(x = hospitals3$x,
#                                        y = hospitals3$y, 
#                                        w = ined, 
#                                        indices=TRUE)
