library(rgdal)

##########
# BACKGROUND AND INSTRUCTIONS
##########
# Name2_:  name of county in FL;
# CWF7094: number of breast cancer deaths in White females in FL;
# CBF7094:  number of breast cancer deaths in Black females in FL;
# POPWF7094: Total number of population of White females in FL;
# POPWF7094: Total number of population of Black females in FL;

#######
#  BEFORE STARTING, WRITE A FULL USEFUL PLOTTING FUNCTIONS
#######
#####
# Compass Rose
#####

#(from http://r-sig-geo.2731867.n2.nabble.com/How-to-diplasy-a-compass-rose-on-a-map-td4509034.html)
compass_rose <-function(x,y,rot=0,cex=1) { 
  oldcex<-par(cex=cex) 
  mheight<-strheight("M") 
  xylim<-par("usr") 
  plotdim<-par("pin") 
  xmult<-(xylim[2]-xylim[1])/(xylim[4]-xylim[3])*plotdim[2]/plotdim[1] 
  point.angles<-seq(0,7*pi/4,by=pi/4)+pi*rot/180 
  crspans<-rep(c(mheight*3,mheight/2),4) 
  xpoints<-cos(point.angles)*crspans*xmult+x 
  ypoints<-sin(point.angles)*crspans+y 
  polygon(xpoints,ypoints) 
  txtxpoints<-cos(point.angles[c(1,3,5,7)])*1.33*crspans[1]*xmult+x 
  txtypoints<-sin(point.angles[c(1,3,5,7)])*1.33*crspans[1]+y 
  text(txtxpoints,txtypoints,c("E","N","W","S")) 
  par(oldcex) 
} 

#####
# COLLAPSE MAP INTO ONLY OUTER BOUNDARY
#####
collapse_map <- function(x){
  require(maptools)
  boundary <- unionSpatialPolygons(x, rep(1, length(x@polygons)))
}

library(RColorBrewer)
library(classInt)

#####
# CHOROPLETH MAP
#####
choro <- function(
  shape = NULL,
  boundary = NULL,
  main = NULL,
  var = NULL,
  color1 = "lightblue",
  color2 = "darkorange",
  legend_round = 2,
  legend_pos = "bottomleft",
  long_legend = TRUE,
  fixed_scale = NULL,
  lwd = 0.2,
  border = TRUE){
  my_colors <- colorRampPalette(c(color1, "white", color2))(10)
  if(!is.null(fixed_scale)){
    if(length(fixed_scale) != 10){stop("Fixed scale must be of length 10")}
    my_quantiles <- fixed_scale
  } else{
    my_quantiles <- quantile(var, na.rm = TRUE, probs = seq(0,1, length = 10))
  }
  my_values <- vector(mode = "numeric", length = length(var))
  for (i in 1:length(var)){
    diffs <- (var[i] - as.numeric(my_quantiles))^2
    best <- which.min(diffs)[1]
    my_values[i] <- best
  }
  map_colors <- my_colors[my_values]
  plot(shape, col = map_colors, border = border, lwd = lwd,
       main = main)
  plot(boundary, add = TRUE)
  if(long_legend){
    legend_colors <- colorRampPalette(my_colors)(25)
    legend(legend_pos, # position
           legend = c(min(round(my_quantiles, digits = legend_round)),
                      rep(NA, 11),
                      median(round(my_quantiles, digits = legend_round)),
                      rep(NA, 11),
                      max(round(my_quantiles, digits = legend_round))),
           fill = legend_colors,
           cex = 0.75,
           y.intersp = 0.5,
           border=NA,
           bty = "n")
  } else{
    legend_colors <- colorRampPalette(my_colors)(11)
    legend(legend_pos, # position
           legend = c(min(round(my_quantiles, digits = legend_round)),
                      rep(NA, 4),
                      median(round(my_quantiles, digits = legend_round)),
                      rep(NA, 4),
                      max(round(my_quantiles, digits = legend_round))),
           fill = legend_colors,
           cex = 0.75,
           y.intersp = 0.5,
           border=NA,
           bty = "n")
  }
  
  compass_rose(x = -80.5 ,y = 30,rot=0,cex=0.25)
}

#######
# 1. Open the shapefile of “Florida_breat_cancer” into OpenGeoda ;
#######

# set local working directory
setwd("/home/joebrew/Documents/uf/phc6194/hw9")

# read in shape file
fl <- readOGR(".", "Florida_breat_cancer")

# make easier to understand column names
names(fl@data)[c(2, 9:12)] <- 
  c("county", "white_female_deaths", "black_female_deaths",
    "white_female_population", "black_female_population")

# # There are some errors in the data!  How is it that there are more deaths
# # than females in some counties?
# fl$county[which(fl$white_female_deaths > fl$white_female_population)]
# fl$county[which(fl$black_female_deaths > fl$black_female_population)]
# fl$county[which((fl$white_female_deaths / fl$white_female_population) > 0.2)]
# fl$county[which((fl$black_female_deaths / fl$black_female_population) > 0.2)]
# 
# # I'm going to remove those
# fl <- fl[which(!fl$county %in% c("CALHOUN", "DE SOTO", "HOLMES",
#                                  "FLAGLER", "GILCHRIST", "LAFAYETTE", "WAKULLA")),]

# plot to confirm that there are no problems
boundary <- collapse_map(fl)

# plot
par(mfrow = c(1,2))
par(mar=c(1,1,1,1))
par(oma = c(0,0,0,0))
choro(shape = fl,
      boundary = boundary,
      var = fl$white_female_deaths / fl$white_female_population * 100000,
      long_legend = FALSE,
      legend_pos = "center",
      legend_round = -2,
      color1 = "lightblue",
      color2 = "red")
title(main = "White female breast cancer deaths per 100,000",
      cex.main = 0.5)

choro(shape = fl,
      boundary = boundary,
      var = fl$black_female_deaths / fl$black_female_population * 100000,
      long_legend = FALSE,
      legend_pos = "center",
      legend_round = -2,
      color1 = "lightblue",
      color2 = "red")
title(main = "Black female breast cancer deaths per 100,000",
      cex.main = 0.5)
par(mfrow = c(1,1))

#######
# 2. Calculate the raw and empirical Bayes, spatial rate and spatial 
#empirical Bayes smoothed rates for both White and Black females;
#######

# first, just mapthe death rate by county (basic choropleth)

# attach package to do empirical bayes smoothing
library(spdep)

# calculate raw and estmm (empirical) for white
fl$bayes_white_raw <- EBest(n = fl$white_female_deaths,
                            x = fl$white_female_population,
                            family = "poisson")[,1]
fl$bayes_white_estmm <- EBest(n = fl$white_female_deaths,
                            x = fl$white_female_population,
                            family = "poisson")[,2]

# calculate raw and estmm for black
fl$bayes_black_raw <- EBest(n = fl$black_female_deaths,
                            x = fl$black_female_population,
                            family = "poisson")[,1]
fl$bayes_black_estmm <- EBest(n = fl$black_female_deaths,
                              x = fl$black_female_population,
                              family = "poisson")[,2]

############
# 3. Save the calculated rates in the attribute table and export as a new shapefile;
############
print(fl@data[,c("county", "bayes_white_estmm", "bayes_black_estmm")])

print(fl@data[,c("county", "bayes_white_raw", "bayes_black_raw")])

# 4. Start the ArcMap, import the new shapefile and create the following cholopleth maps.  
#(All following maps use the same categories of the classification of the rate, 
#which you need to specify the appropriate cut-off points; 
#Each map also includes at least the title and the legend because it is a thematic map.)
# a. Raw and smoothed breast cancer mortality rates for White females; 
# i. Map 1: raw rate for White females;
# ii. Map 2: empirical Bayes rate for White females;
# iii. Map 3: spatial rate for White females;
# iv. Map 4: spatial empirical Bayes rate for White females;
# (Note: Display four maps in a single page)
# v. Export the maps as a picture and insert below evaluation;

# Raw
par(mfrow = c(2,2))
par(mar=c(1,1,1,1))
par(oma = c(0,0,0,0))
choro(shape = fl,
      boundary = boundary,
      var = fl$bayes_white_raw * 100000,
      long_legend = FALSE,
      legend_pos = "center",
      legend_round = -2,
      color1 = "lightblue",
      color2 = "red")

title(main = "Bayes raw estimates for white female breast cancer deaths per 100,000",
      cex.main = 0.5)
choro(shape = fl,
      boundary = boundary,
      var = fl$bayes_black_raw * 100000,
      long_legend = FALSE,
      legend_pos = "center",
      legend_round = -2,
      color1 = "lightblue",
      color2 = "red")

title(main = "Bayes raw estimates for black female breast cancer deaths per 100,000",
      cex.main = 0.5)

# Empircal
choro(shape = fl,
      boundary = boundary,
      var = fl$bayes_white_estmm * 100000,
      long_legend = FALSE,
      legend_pos = "center",
      legend_round = -2,
      color1 = "lightblue",
      color2 = "red")

title(main = "Bayes empirical estimates for white female breast cancer deaths per 100,000",
      cex.main = 0.5)
choro(shape = fl,
      boundary = boundary,
      var = fl$bayes_black_estmm * 100000,
      long_legend = FALSE,
      legend_pos = "center",
      legend_round = -2,
      color1 = "lightblue",
      color2 = "red")
title(main = "Bayes empirical estimates for black female breast cancer deaths per 100,000",
      cex.main = 0.5)
par(mfrow = c(1,1))


# b. Raw and smoothed breast cancer mortality rates for Black females;
# i. Map 1: raw rate for Black females;
# ii. Map 2: empirical Bayes rate for Black females;
# iii. Map 3: spatial rate for Black females;
# iv. Map 4: spatial empirical Bayes rate for Black females;
# (Note: Display four maps in a single page)
# v. Export the maps as a picture and insert below evaluation;

#
library(gstat)
library(geoR)
my_colors <- colorRampPalette(c("blue", "white", "red"))(100)
boundary <- unionSpatialPolygons(boundary, rep(1, length(boundary@polygons)))

SurfaceFun <- function(var = "bayes_white_raw",
                       boundary_shape = boundary){
  
  
  
  # getting coordinates of alachua boundary
  boundary_points <- boundary@polygons[[1]]@Polygons[[10]]
  boundary_points <- boundary_points@coords
  
  # Get trap locations and data values
  a <- data.frame("x" = coordinates(fl)[,1],
                  "y" = coordinates(fl)[,2],
                  "z" = fl@data[,var] * 100000)
  # Make into a geodata object
  b <- as.geodata(a)
  
  # Predict multiple points in Florida's boundary
  x <- seq(min(boundary_points[,1]), max(boundary_points[,1]), length = 100)
  y <- seq(min(boundary_points[,2]), max(boundary_points[,2]), length = 100)
  
  # Make a grid of those points
  pred.grid <- expand.grid(x,y)
  
  
  # kriging calculations
  kc <- krige.conv(geodata = b, coords = b$coords, data = b$data,
                   locations = pred.grid,
                   borders = boundary_points,
                   #borders = boundary@polygons,
                   # borders = ALACHUA BORDERS!,
                   krige = krige.control(type.krige = "ok",
                                         cov.pars = c(5000,10000000))) #10, 3.33 # what is this?
  
  
  
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
  
  legend(x="center",
         fill = my_colors[c(1,11,22,33,44,55,66,77,88,100)],
         legend = c(legtemp[1], NA, NA, legtemp[4], NA, NA, legtemp[7], NA, NA, legtemp[10]),
         border = FALSE,
         bty = "n",
         ncol = 1,
         y.intersp = 0.5,
         title = "Interpolation",
         cex = 0.75)
}

par(mfrow = c(2,2))
par(mar=c(1,1,1,1))
par(oma = c(0,0,0,0))
SurfaceFun("bayes_white_raw")
title(main = "White raw")
SurfaceFun("bayes_white_estmm")
title(main = "White interpolated")
SurfaceFun("bayes_black_raw")
title(main = "Black raw")
SurfaceFun("bayes_black_estmm")
title(main = "Black interpolated")
par(mfrow = c(1,1))

#5. Do these maps of raw, empirical Bayes, spatial rate and spatial empirical 
#Bayes smoothed rates are similar? If they are different, please explain why 
#they are different. 

# They look similar, but I believe that this is due to errors in the shapefile.
# If you look at the distribution of rates by county, you find some severe outliers
par(mfrow = c(1,2))
par(mar = c(5,4,2,2))
par(oma = c(1,1,1,1))
hist(fl$white_female_deaths / fl$white_female_population,
     main = "White female death rate")
hist(fl$black_female_deaths / fl$black_female_population,
     main = "Black female death rate")
par(mfrow = c(1,1))

par(mfrow = c(1,2))
x <- fl@data[order(fl$black_female_deaths / fl$black_female_population),]
barplot(x$black_female_deaths / x$black_female_population,
        names.arg = x$county,
        cex.names = 0.1,
        las = 3,
        main = "Black death rate by county")

x <- fl@data[order(fl$white_female_deaths / fl$white_female_population),]
barplot(x$white_female_deaths / x$white_female_population,
        names.arg = x$county,
        cex.names = 0.1,
        las = 3,
        main = "White death rate by county")
par(mfrow = c(1,1))


