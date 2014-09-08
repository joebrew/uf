par(mar=c(0,0,0,0))
par(oma=c(0,0,0,0))

library(RColorBrewer)
mycols <- colorRampPalette(brewer.pal(9, "Spectral"))(3784)


library(rgdal)
setwd("C:/Users/BrewJR/Desktop/")
world <- readOGR("world_borders",layer = "world_borders")

x <- runif(1000000, min=-180, max=190)
y <- runif(1000000, min = -86, max=84)

myPoints <- data.frame(cbind(x,y))
coordinates(myPoints) <- ~x+y
proj4string(myPoints) <- proj4string(world)

a <- over(myPoints, polygons(world))

mycols2 <- mycols[a]

plot(1,1, pch=16, cex=10000000, xlim=c(-180,190), ylim=c(-86,84))
plot(myPoints,
     pch = 1:20,
     col = adjustcolor(mycols2, alpha.f = 0.2),
     cex = 0.01,
     add = TRUE)

b <- ptransform(myPoints, src.proj = , dst.proj, silent=TRUE)

library(rgl)
spheres3d(x = 1, y = 1, z = 1, radius = 1, col = "green")

open3d()                                   # create new plot
spheres3d(x = 1, y = 1, z = 1, radius = 1) # produce sphere
axes3d()  

library(RColorBrewer)
mycols <- colorRampPalette(c("brown", "grey", "beige", "darkgrey"))(20)
mycols <- colorRampPalette(c("brown", 
                             "grey", 
                             "beige", 
                             "darkgrey", 
                             "darkred",
                             "darkorange",
                             "yellow"))(20)

library(maps)
map("world",
    fill = TRUE,
    col = adjustcolor(mycols, alpha.f = 0.4),
    border = adjustcolor("black", alpha.f = 0.6),
    bg = "grey")



mycols <- adjustcolor(mycols, alpha.f=0.8)
map("world", proj = "bonne", param = 45, fill = TRUE, 
    col = mycols, border = NA)


projlist <- c("aitoff", 
             # "albers", #requires 2 parameters
              "azequalarea", 
              "azequidist", 
              # "bicentric", #requires 1 parameter
              # "bonne", # param =40
             "conic", 
             "cylequalarea", 
             "cylindrical", 
             "eisenlohr", "elliptic",
              "fisheye", "gall", "gilbert", "guyou", "harrison", "hex", "homing",
              "lagrange", "lambert", "laue", "lune", "mercator", "mollweide", "newyorker",
              "orthographic", "perspective", "polyconic", "rectangular", "simpleconic",
              "sinusoidal", "tetra", "trapezoidal")

map("world", proj = "aitoff", fill = TRUE,
    col = mycols, border = NA)

map("world", proj = "albers", param=c(10,10), fill = TRUE,
    col = mycols, border = NA)

map("world", proj = "azequalarea",col = "black")

map("world", proj = "azequidist", col = "black")

map("world", proj = "bicentric", param=60, fill = TRUE,
    col = mycols, border = NA)

map("world", proj = "bonne", param=60, fill = TRUE,
    col = mycols, border = NA)

map("world", proj = "conic", param=60, fill = TRUE,
    col = mycols, border =  adjustcolor("black", alpha.f=0.6))

map("world", proj = "cylequalarea", param=100, fill = TRUE,
    col = mycols, border =  adjustcolor("black", alpha.f=0.6))


map("world", proj = "cylindrical", fill = TRUE,
    col = mycols, border = adjustcolor("black", alpha.f=0.6))







map("world", proj = "bonne", param=30, fill = TRUE,
    col = mycols, border = adjustcolor("black", alpha.f=0.6))

for (i in projlist){
  map("world", proj = i, fill = TRUE,
      col = mycols, border = NA)
}

########################################

myMap <- map("world")

range(myMap$y, na.rm=T)

#### OVER
library(rgdal)
a <- over(myPoints, polygons(myMap))

setwd("C:/Users/BrewJR/Documents/uf/phc6194/lecture2")
World30 <- readOGR("MapProjection.mdb",layer = "World30_Shape_Index")

myMap$country <- sub(":.*", "", myMap$names)

myCountries <- c("USA", "Spain", "Ethiopia",
                 "Denmark", "France", "UK",
                 "Portugal", "Ireland",
                 "Canada", "Mexico", "Belize", 
                 "Guatemala", "El Salvador",
                 "Togo", "Morocco", "Benin",
                 "Turkey")