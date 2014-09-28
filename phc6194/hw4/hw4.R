#################
## Setwd
if ( Sys.info()["sysname"] == "Linux" ){
  setwd("/home/joebrew/")
} else {
  setwd("C:/Users/BrewJR/")
}

## Read txt file (converted from dbf file) for Orlando
orlando <- read.csv("Documents/uf/phc6194/hw4/ORLANDO.txt")

#### This script uses RCurl and RJSONIO to download data from Google's API:
#### Latitude, longitude, location type (see explanation at the end), formatted address
#### Notice ther is a limit of 2,500 calls per day

library(RCurl)
library(RJSONIO)
library(plyr)

url <- function(address, return.call = "json", sensor = "false") {
  root <- "http://maps.google.com/maps/api/geocode/"
  u <- paste(root, return.call, "?address=", address, "&sensor=", sensor, sep = "")
  return(URLencode(u))
}

geoCode <- function(address,verbose=FALSE) {
  if(verbose) cat(address,"\n")
  u <- url(address)
  doc <- getURL(u)
  x <- fromJSON(doc,simplify = FALSE)
  if(x$status=="OK") {
    lat <- x$results[[1]]$geometry$location$lat
    lng <- x$results[[1]]$geometry$location$lng
    location_type <- x$results[[1]]$geometry$location_type
    formatted_address <- x$results[[1]]$formatted_address
    return(c(lat, lng, location_type, formatted_address))
  } else {
    return(c(NA,NA,NA, NA))
  }
}



# Use plyr to getgeocoding for a vector
address <- paste(orlando$FAC_ADDR,
                 orlando$FAC_CITY,
                 "Florida",
                 orlando$FAC_ZIP)
locations <- ldply(address, function(x) geoCode(x))
names(locations) <- c("lat","lon","location_type", "forAddress")

orlando <- cbind(orlando, locations)

# save lat and lon as numeric objects
library(dplyr)
orlando <- 
  orlando %>% 
  mutate(lat = as.numeric(lat),
         lon = as.numeric(lon))

# Create the features necessary for ESRI's address locator tool:
# x <- 
#   orlando %>% 
#   mutate(feature_id = ,
#          house_number = ,
#          side = ,
#          prefix_direction = ,
#          prefix_type = ,
#          street_name = ,
#          suffix_type = ,
#          suffix_direction = ,
#          city_or_place = ,
#          zip_code = ,
#          state = ,
#          longitude = ,
#          )


# Save a shapefile version or orlando
library(sp)
library(rgdal)

# Keep only the non-NA's
orlando_sp <- orlando[which(is.finite(orlando$lat) &
                              is.finite(orlando$lon)),]

# Convert to spatial points data frame projected in latitude and longitude
orlando_sp <- SpatialPointsDataFrame(orlando_sp[,c("lon", "lat")], orlando_sp,
                                     proj4string = CRS("+init=epsg:4326"))


# Write the shapefile
writeOGR(orlando_sp,
         dsn = "Documents/uf/phc6194/hw4/orlando",
         layer = "orlando", 
         driver = "ESRI Shapefile")

# Read in orlando all lines (from http://www.census.gov/cgi-bin/geo/shapefiles2010/main)
orlando_all <- readOGR("Documents/uf/phc6194/hw4", "tl_2010_12095_edges")

# Read in orlando roads only
orlando_roads <- readOGR("Documents/uf/phc6194/hw4", "tl_2010_12095_roads")

# Save the .rdata file of the geocoded addresses (so as to not have to repeat)
save.image("Documents/uf/phc6194/hw4/hw4.RData")

#Location type, for more info check here: https://developers.google.com/maps/documentation/directions/
#"ROOFTOP" indicates that the returned result is a precise geocode for which we have location information accurate down to street address precision.
#RANGE_INTERPOLATED" indicates that the returned result reflects an approximation (usually on a road) interpolated between two precise points (such as intersections). Interpolated results are generally returned when rooftop geocodes are unavailable for a street address.
#GEOMETRIC_CENTER" indicates that the returned result is the geometric center of a result such as a polyline (for example, a street) or polygon (region).
#APPROXIMATE" indicates that the returned result is approximate.