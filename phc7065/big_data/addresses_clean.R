setwd('/home/joebrew/Documents/uf/phc7065/big_data/addresses_processed')


#####
# PACKAGES
#####
library(rgdal)
library(sp)  # classes for spatial data
library(raster)  # grids, rasters
library(rasterVis)  # raster visualisation
library(maptools)
library(rgeos)

#####
# READ IN ALACHUA ZIP CODE FILE
#####
zip <- readOGR('/home/joebrew/Documents/uf/phc7065/big_data/alachuazipcodes/',
               'ACDPS_zipcode')
# make lat lon
zip <- spTransform(zip,
                   CRS('+init=epsg:4326'))
plot(zip)

#####
# Define simple mathematical function
# for getting euclidean distance in kilometers
#####
get_distance <- function(lon1, 
                         lat1, 
                         lon2, 
                         lat2){
  rad <- pi/180
  a1 <- lat1 * rad
  a2 <- lon1 * rad
  b1 <- lat2 * rad
  b2 <- lon2 * rad
  dlon <- b2 - a2
  dlat <- b1 - a1
  a <- (sin(dlat/2))^2 + cos(a1) * cos(b1) * (sin(dlon/2))^2
  c <- 2 * atan2(sqrt(a), sqrt(1 - a))
  R <- 6378.145
  d <- R * c
  return(d)
}

#####
# LOAD GEOCODED LOCATIONS (from geocode.py)
#####

# Read in each geocoder's file
for(i in dir()[which(grepl('.csv', dir()))]){
  assign(gsub('.csv', '', i), read.csv(i))
}


#####
# Define function to get centroid, mean distance
# and number of indicators for each place
#####

# Make vector of names of dataframes
dfs <- names(which(sapply(.GlobalEnv, is.data.frame))) 

# Make a df name dmaster to house all the geocodings
master <- data.frame(address = arc$address,
                     lon_centroid = NA,
                     lat_centroid = NA,
                     mean_error = NA,
                     n_geocoders = NA,
                     mean_confidence = NA,
                     mean_fields_used = NA,
                     quality_string = NA,
                     mean_quality = NA)



# Loop through each row to get required data

for (i in 1:nrow(arc)){
  # Create empty vectors to put the coordinates into
  x <- vector(mode = "numeric", length = length(dfs))
  y <- vector(mode = "numeric", length = length(dfs))
  confidence <- vector(mode = "numeric", length = length(dfs))
  fields_used <- vector(mode = "numeric", length = length(dfs))
  quality <- vector(mode = "numeric", length = length(dfs))
  
  # Loop through all of the dfs to extract points
  for (j in 1:length(dfs)){
    df <- get(dfs[j])
    x[j] <- df$lng[i]
    y[j] <- df$lat[i]
    confidence[j] <- df$confidence[i]
    fields_used[j] <- df$fields_used[i]
    quality[j] <- df$quality[i]
  }
  
  # Clean out non-Uganda results
  places <- data.frame(x,y)
  places <- places[which(!is.na(places$x) & !is.na(places$y)),]
  coordinates(places) <- ~x + y
  proj4string(places) <- proj4string(zip)
  goods <- which(!is.na(over(x = places, y = polygons(zip))))
  places <- places[goods,]
  

  # Get Centroid
  x <- mean(places$x, na.rm = T)
  y <- mean(places$y, na.rm = T)
  
  # Get distance from centroid to points
  distances <- vector(mode = "numeric", length = length(places$x))
  for (k in 1:length(places$x)){
    
    # Pull out points
    x1 <- places$x[k]
    y1 <- places$y[k]
    
    # Get distance
    distances[k] <- get_distance(lon1 = x1,
                                 lat1 = y1,
                                 lon2 = x,
                                 lat2 = y)
  }
  # Calculate means
  mean_error <- mean(distances, na.rm = TRUE)
  mean_confidence <- mean(confidence, na.rm = TRUE)
  mean_fields_used <- mean(fields_used, na.rm = TRUE)
  mean_quality <- mean(quality, na.rm = TRUE)
  
  # Collapse the quality string
  quality_string <- paste0(quality, collapse = "|")
  
  # Assign to master
  master$lon_centroid[i] <- x
  master$lat_centroid[i] <- y
  master$mean_error[i] <- mean_error
  master$n_geocoders[i] <- length(distances)
  master$mean_confidence[i] <- mean_confidence
  master$mean_fields_used[i] <- mean_fields_used
  master$quality_string[i] <- quality_string
  master$mean_quality[i] <- mean_quality
  
}


cols <- colorRampPalette(c("yellow", "red"))(30)


# VISUALIZE

# make master_sp 
master_sp <- master
coordinates(master_sp) <- ~lon_centroid+lat_centroid

# Convert projection
proj4string(master_sp) <- proj4string(zip)

plot(zip)
points(master_sp, col = 'red', pch = 19)

# Contextualize in Florida
library(maps)
my_map <- map('county', 'fl', plot = FALSE)
cols <- colorRampPalette(c('black', 'grey'))(length(my_map$names))
map('county', 'fl',
    fill = TRUE,
    col = sample(cols, length(my_map$names)))
plot(zip, add = T, 
     col = rainbow(nrow(zip)),
     border = 'black')

