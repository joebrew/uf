par(mar=c(0,0,0,0))
par(oma=c(0,0,0,0))

library(rgl)
spheres3d(x = 1, y = 1, z = 1, radius = 1, col = "green")

open3d()                                   # create new plot
spheres3d(x = 1, y = 1, z = 1, radius = 1) # produce sphere
axes3d()  

library(RColorBrewer)
mycols <- colorRampPalette(c("brown", "grey", "beige", "darkgrey"))(20)

library(maps)
map("world",
    fill = TRUE,
    col = adjustcolor(mycols, alpha.f = 0.4),
    border = adjustcolor("black", alpha.f = 0.6))



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

myMap$country <- sub(":.*", "", myMap$names)

myCountries <- c("USA", "Spain", "Ethiopia",
                 "Denmark", "France", "UK",
                 "Portugal", "Ireland",
                 "Canada", "Mexico", "Belize", 
                 "Guatemala", "El Salvador",
                 "Togo", "Morocco", "Benin",
                 "Turkey")