####################
# LOAD 
####################
library(maptools)
library(rgdal)
library(RColorBrewer)
library(Hmisc)
library(mapproj)
library(rgl)

####################
# SET WD TO THE GIS FOLDER
####################
setwd("C:/Users/BrewJR/Documents/uf/phc6194/hw2/")


World30 <- readOGR("MapProjection.mdb",layer = "World30_Shape_Index")


#########
# REMOVE BORDERS
#########
par(mar=c(0,0,0,0))
par(oma=c(0,0,0,0))

#########
# ATTACH PACKAGES
#########
library(RColorBrewer)
library(classInt)
library(maps)
library(rgdal)

##########
# READ IN WORLD MAP SHAPEFILE
##########
setwd("C:/Users/BrewJR/Desktop/")
world <- readOGR("world_borders",layer = "world_borders")

##########
# CREATE SOME RANDOM X (LONG) AND Y (LAT) POINTS 
# AND BIND INTO DF
##########
x <- runif(100000, min=-180, max=190)
y <- runif(100000, min = -86, max=84)
myPoints <- data.frame(cbind(x,y))

###########
# MAKE A SPATIAL POINTS FILE
###########
coordinates(myPoints) <- ~x+y

###########
# ASSIGN PROJECTION STRING
###########
proj4string(myPoints) <- proj4string(world)

###########
# CREATE VECTOR OF WHICH POLYGON EACH POINT IS IN
###########
myPolygons <- over(myPoints, polygons(world))

###########
# SAMPLE POINTS FOR EACH POLYGON
###########
# 
# # Pick a country 
# country <- "United States"
# 
# # factor by which to reduce / increase point density
# redinc <- 0.000001
# 
# nPoints <- ifelse(ceiling(redinc*sum(world$POP_CNTRY[which(world$CNTRY_NAME == country)],
#                                      na.rm = TRUE)) > 0,
#                   ceiling(redinc*sum(world$POP_CNTRY[which(world$CNTRY_NAME == country)],
#                                      na.rm = TRUE)),
#                   1)
# 
# # Sample points from that country
# countryPoints <- sample(x = myPoints[which(myPolygons %in% which(world$CNTRY_NAME == country))],
#                         size = nPoints,
#                         replace = TRUE)

# Plot those points
plot(countryPoints, pch=16, col=adjustcolor("black", alpha.f=0.6) )

# LOOP TO PLOT ALL POINTS
plot(1,1, pch=16, cex=10000000, xlim=c(-180,190), ylim=c(-86,84))
for (i in unique(world$CNTRY_NAME)){
  
  # Pick a country 
  country <- i
  
  # factor by which to reduce / increase point density
  redinc <- 0.000001
  
  nPoints <- ifelse(ceiling(redinc*sum(world$POP_CNTRY[which(world$CNTRY_NAME == i)],
                                       na.rm = TRUE)) > 0,
                    ceiling(redinc*sum(world$POP_CNTRY[which(world$CNTRY_NAME == i)],
                                       na.rm = TRUE)),
                    1)
  # # Sample points from that country
  
  if(length(myPoints[which(myPolygons %in% which(world$CNTRY_NAME == country))])>0){
    countryPoints <- sample(x = myPoints[which(myPolygons %in% which(world$CNTRY_NAME == country))],
                            size = nPoints,
                            replace = TRUE)
  }else{
    countryPoints <- NULL
  }
  

  
  # Plot those points
  if(length(countryPoints) > 0){
    points(countryPoints, pch=16, col=adjustcolor("white", alpha.f=0.6) )
  }else{return(NULL)}
  # Sys.sleep(0.25)
  
  
}


###########
#ASSIGN COLOR VECTOR
###########
mycols <- colorRampPalette(brewer.pal(9, "Spectral"))(length(world$CNTRY_NAME))

mycols2 <- mycols[myPolygons]

plot(1,1, pch=16, cex=10000000, xlim=c(-180,190), ylim=c(-86,84))
plot(myPoints,
     pch = 1:20,
     col = adjustcolor(mycols2, alpha.f = 0.2),
     cex = 0.01,
     add = TRUE)

############
# CREATE FUNCTION FOR MAPPING POINT DENSITY
############
# use sample


############
# MAP BY VOWELS IN COUNTRY NAME
############
mymap <- map("world")

# REMOVE EVERYTHING AFTER COLON (INCLUDING COLON):
mymap$names <- sub("(.*?):.*", "\\1", mymap$names)

# CREATE COLUMN FOR NUMBER OF LETTERS
mymap$letters <- nchar(as.character(mymap$names))

# FUNCTION FOR PLOTTING WORLD
MapFun <- function(var, color, border="black", proj="aitoff", param=NA){
  plotvar <- var
  nclr <- 5
  plotclr <- (brewer.pal(nclr, color))
  #class <- classIntervals(plotvar, nclr, style = "quantile", dataPrecision=0) #use "equal" instead
  class <- classIntervals(plotvar, nclr, style="equal")
  colcode <- findColours(class, plotclr)
  legcode <- paste0(gsub(",", " - ", gsub("[[]|[]]|[)]", "", names(attr(colcode, "table")))))
  map("world", projection = proj, param = param, 
      fill = TRUE,
      border=border, col=colcode)
  legend("bottomleft", # position
         legend = legcode, #names(attr(colcode, "table")), 
         fill = attr(colcode, "palette"), 
         cex = 0.85, 
         border=grey)
}

par(mar=c(0,0,0,0))
MapFun(var =  mymap$letters,
       color = "Greys",
       proj = "bonne",
       param = 70)






#######
# COLORS
#######
mycols <- colorRampPalette(brewer.pal(8, "Greens"))(nrow(blocks)) 
mycols <- sample(mycols)










mycols <- colorRampPalette(c("brown", "grey", "beige", "darkgrey"))(20)
mycols <- adjustcolor(mycols, alpha.f=0.8)

bor <- adjustcolor("black", alpha.f=0.6)

map("world", 
    fill = TRUE,
    col = mycols,
    border = bor)

abline(v=seq(-360,360,30), col = bor)
abline(h=seq(-360,360,30), col = bor)

abline(v=seq(-360,360,10), col = adjustcolor(bor, alpha.f=0.5))
abline(h=seq(-360,360,10), col = adjustcolor(bor, alpha.f=0.5))


abline(v=seq(-360,360,1), col = adjustcolor(bor, alpha.f=0.1))
abline(h=seq(-360,360,1), col = adjustcolor(bor, alpha.f=0.1))

map("world", 
    fill = TRUE,
    col = mycols,
    border = bor,
    bg = "black",
    add = T)




########
# READ FROM MDB
########
# FLCounties <- readOGR("MapProjection.mdb",layer = "FLCounties")
# County_Boundary <- readOGR("MapProjection.mdb",layer = "County_Boundary")
# Alachua_BlockGroup <- readOGR("MapProjection.mdb",layer = "Alachua_BlockGroup")
# Country08 <- readOGR("MapProjection.mdb",layer = "Country08")
# World30 <- readOGR("MapProjection.mdb",layer = "World30")
# States <- readOGR("MapProjection.mdb",layer = "States")
# Hospitals_WrongPrj <- readOGR("MapProjection.mdb",layer = "Hospitals_WrongPrj")
# 

# BASE COLOR OFF LETTERS
mycols <- colorRampPalette(c("grey",  "white"))(max(mymap$letters))
mycols <- adjustcolor(mycols, alpha.f=0.8)
mymap$color <- mycols[mymap$letters]


map("world", 
    fill = TRUE,
    col = "white",
    border = "black",
    bg = "beige")


map("world", projection = "", param=1,
    fill = TRUE,
    col = mymap$color,
    border = bor, 
    bg = "black")

map("world", 
    fill = TRUE,
    col = mymap$color,
    border = "black",
    bg = "black")
