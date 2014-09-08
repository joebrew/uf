####################
# LOAD 
####################
library(maptools)
library(rgdal)
library(RColorBrewer)
library(Hmisc)
####################
# SET WD TO THE GIS FOLDER
####################
setwd("C:/Users/BrewJR/Documents/uf/phc6194/lecture2")

#######
# COLORS
#######
mycols <- colorRampPalette(brewer.pal(8, "Greens"))(nrow(blocks)) 
mycols <- sample(mycols)

########
# READ FROM MDB
########
FLCounties <- readOGR("MapProjection.mdb",layer = "FLCounties")
County_Boundary <- readOGR("MapProjection.mdb",layer = "County_Boundary")
Alachua_BlockGroup <- readOGR("MapProjection.mdb",layer = "Alachua_BlockGroup")
Country08 <- readOGR("MapProjection.mdb",layer = "Country08")
World30 <- readOGR("MapProjection.mdb",layer = "World30")
States <- readOGR("MapProjection.mdb",layer = "States")
Hospitals_WrongPrj <- readOGR("MapProjection.mdb",layer = "Hospitals_WrongPrj")


library(maps)
par(mar=c(0,0,0,0))
par(oma=c(0,0,0,0))

library(RColorBrewer)
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



map("world", projection = "", param=1,
    fill = TRUE,
    col = mycols,
    border = bor, 
    bg = "black")

map("world", 
    fill = TRUE,
    col = mycols,
    border = "black",
    bg = "black")
