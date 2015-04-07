library(RColorBrewer)
library(RCurl)
options(RCurlOptions = list(cainfo = system.file("CurlSSL", "cacert.pem", package = "RCurl")))

#########
# SET LOCAL WORKING DIRECTORY
#########
#setwd("C:/Users/BrewJR/Documents/misc") # change this line to whereever you cloned misc
setwd('/home/joebrew/Documents/uf/phc7065/big_data')
setwd("gnv_crime")


#########
# READ IN GAINESVILLE CRIME DATA
#########
# # define the link for gainesville crime
# my_link1 <- 'https://data.cityofgainesville.org/api/views/gvua-xt9q/rows.csv?accessType=DOWNLOAD'
# my_link2 <- getURL(my_link1)
# 
# # read in the data
# gnv <- read.csv(my_link2)

gnv <- read.csv("april1.csv", stringsAsFactors = FALSE)

#########
# CLEAN IT UP A BIT
#########
# write a function to clean lat lon points
clean_up <- function(x){
  
  # split the string to keep only the lat, lon, part
  a <- do.call(rbind, strsplit(as.character(x), "\n"))
  aa <- a[,3]
  
  # now split at the comma
  b <- do.call(rbind, strsplit(as.character(aa), ","))
  bb <- b#[,2]
  
  # make df
  bb <- data.frame(bb)
  
  # fix names
  names(bb) <- c("lat", "lon")
  
  # remove parentheses
  bb$lat <- as.numeric(gsub("\\(|\\)", "", bb$lat))
  bb$lon <- as.numeric(gsub("\\(|\\)", "", bb$lon))
  
  return(bb)
}
x <- clean_up(gnv$location)
# now join x to gnv
gnv <- cbind(gnv, x)
rm(x)

# Make a date column
gnv$date <- as.Date(substr(gnv$offense_date,1,10), format = "%m/%d/%Y")

# view it
hist(gnv$date, breaks = 100)

# Remove dates prior to 2013
gnv <- gnv[which(gnv$date > "2013-01-01"),]
hist(gnv$date, breaks = 100)

# Order dataframe by dates
gnv <- gnv[rev(order(gnv$date)),]

#####
# GROUP BY DATES
#####
library(dplyr)
gnv_agg <- gnv %>%
  group_by(date) %>%
  summarise(n = n())
all_dates <- data.frame(date = seq(min(gnv_agg$date),
                                   max(gnv_agg$date),
                                   1))
gnv_agg <- left_join(all_dates, gnv_agg)

plot(gnv_agg$date, gnv_agg$n,
     col = adjustcolor('black', alpha.f = 0.6))

# Add loess line
lw1 <- loess(n ~ as.numeric(date), data = gnv_agg)
ord <- order(gnv_agg$date)
lines(gnv_agg$date[ord], lw1$fitted[ord], col=adjustcolor("red", alpha.f=0.6),
      lwd=3)

#####
# FUNCTION TO SUBSET BY NARRATIVE
##### 
crime <- function(narrative = 'ROBBERY',
                  ts = FALSE,
                  lo = TRUE,
                  lin = TRUE,
                  span = 0.75,
                  legend = TRUE){
  
  
  sub_data <- gnv[which(grepl(narrative, gnv$narrative)),]
  if(ts){
    sub_data_agg <- sub_data %>%
      group_by(date) %>%
      summarise(n = n())
    
    all_dates <- data.frame(date = seq(min(sub_data_agg$date),
                                       as.Date(Sys.Date()),
                                       1))
    return_obj <- left_join(all_dates, sub_data_agg)
    
    # Fix 0
    return_obj$n[which(is.na(return_obj$n))] <- 0
    
    
    plot(return_obj$date, return_obj$n,
         col = adjustcolor('black', alpha.f = 0.6),
         xlab = 'Date',
         ylab = 'Incidences')
    
    if(lo){
      lw1 <- loess(return_obj$n ~ as.numeric(return_obj$date), span = span)
      ord <- order(return_obj$date)
      lines(return_obj$date[ord], lw1$fitted[ord], col=adjustcolor("red", alpha.f=0.6),
            lwd=3)
    }
    if(lin){
      fit <- lm(return_obj$n ~ as.numeric(return_obj$date))
      abline(fit, col = adjustcolor('darkblue', alpha.f = 0.6), lwd = 3)
    }
    if(legend){
      legend('topright',
             col = adjustcolor(c('darkred', 'darkblue'), alpha.f = 0.6),
             lwd = 3,
             legend = c('Loess', 'Linear'))
    }
    
    
  } else{
    return_obj <- sub_data
    
  }
  return(return_obj)  
}

# Examine a trend over time
assault <- crime(narrative = 'ASSAULT', ts = TRUE)

# Examine details
assault <- crime(narrative = 'ASSAULT', ts = FALSE)
table(assault$narrative)

# KIDNAPPING
kid <- crime(narrative = 'KIDNAP')
crime(narrative = 'KIDNAP', ts = TRUE, lo = FALSE)

# RUNAWAY
runaway <- crime(narrative = 'RUNAWAY', ts = TRUE)
runaway <- crime(narrative = 'RUNAWAY')

# PARTY
party <- crime(narrative = 'PARTY', ts = TRUE)
party <- crime(narrative = 'PARTY')

<<<<<<< HEAD
# THEFT
theft <- crime(narrative = 'THEFT', ts = FALSE)

=======
>>>>>>> 4d452ead1de4405b0656bd34f7a71ea12c95760f
term_of_interest <- gnv

######### 
# ADVANCED LEAFLET MAPS
##########

# Read in zip code shape file
library(rgdal)
#library(sp)
zip <- readOGR("zips_alachua", "ACDPS_zipcode")
zip <- spTransform(zip, CRS("+init=epsg:4326"))
zip$zip <- as.numeric(as.character(zip$ZIP))

library(rCharts)
# read in geojson version of zip
#zip_geoj <- readOGR("zips_alachua", "ACDPS_zipcode")
mymap <- Leaflet$new()
mymap$tileLayer(provider = "Stamen.TonerLite")
mymap$setView(c(29.65, -82.3), zoom = 10)
mymap$enablePopover(TRUE)

mymap$fullScreen(TRUE)

<<<<<<< HEAD


for (i in 1:nrow(term_of_interest)){
=======
maxi <- ifelse(nrow(term_of_interest) > 500,
              500,
              nrow(term_of_interest))
for (i in 1:maxi){
>>>>>>> 4d452ead1de4405b0656bd34f7a71ea12c95760f
  mymap$marker(c(term_of_interest$lat[i], term_of_interest$lon[i]),
               bindPopup = paste(term_of_interest$narrative[i],
                                 term_of_interest$offense_date[i]))
  
}

mymap


#########
# MAKE SOME MAPS
#########
# plot the points on a florida map
library(maps)
map("county", "florida")
points(gnv$lon, gnv$lat, col = "red")

# Plot the points on an Alachua map
library(rgdal)
library(sp)
zip <- readOGR("zips_alachua", "ACDPS_zipcode")
zip <- spTransform(zip, CRS("+init=epsg:4326"))
zip$zip <- as.numeric(as.character(zip$ZIP))
plot(zip, col = "grey", border = "white")

points(gnv$lon, gnv$lat,
       col = adjustcolor(rainbow(nrow(gnv)), alpha.f = 0.3),
       pch = 16,
       cex = 0.3)



########
# GET ZIP CODE FOR EACH CRIME
########
# make a spatial version of gnv
gnv <- gnv[which(!is.na(gnv$lat) & !is.na(gnv$lon)),]
gnv_sp <- SpatialPointsDataFrame(gnv[,c("lon", "lat")], gnv,
                                 proj4string = CRS("+init=epsg:4326"))
x <- over(gnv_sp, polygons(zip))

# Return to dataframe
gnv$zip <- zip$zip[x]

#########
# GET NUMBER OF CRIMES BY ZIP CODE
#########
library(dplyr)
crimes_zip <- 
  gnv %>%
  group_by(zip) %>%
  summarise(n_crimes = n()) %>%
  arrange(desc(n_crimes))

# bring those numbers into the spatial polygons data frame
zip_df <- data.frame(zip)

zip_df <- merge(x = zip_df,
                y = crimes_zip,
                by = "zip",
                all.x = TRUE,
                all.y = FALSE)
for (i in zip$zip){
  zip$n_crimes[which(zip$zip == i)] <-
    zip_df$n_crimes[which(zip_df$zip == i)]
}

##########
# WRITE A FUNCTION FOR MAKING A CHOROPLETH MAP OF THESE ZIP CODES
##########
library(classInt)
MapFun <- function(var, color){
  plotvar <- var
  nclr <- 8
  plotclr <- brewer.pal(nclr, color)
  class <- classIntervals(plotvar, nclr, style = "quantile", dataPrecision=0) #use "quantile" instead
  #class <- classIntervals(0:100, nclr, style="equal")
  colcode <- findColours(class, plotclr)
  legcode <- paste0(gsub(",", " - ", gsub("[[]|[]]|[)]", "", names(attr(colcode, "table")))))
  plot(zip, border="darkgrey", col=colcode)
  legend("bottomleft", # position
         legend = legcode, #names(attr(colcode, "table")), 
         fill = attr(colcode, "palette"), 
         cex = 0.8, 
         border=NA,
         bty = "n",
         y.intersp = 0.6)
}

# Now plot
MapFun(zip$n_crimes, "Blues")
title(main = "Number of crimes by zip code")

# Plot rate (adj. for pop)
MapFun(zip$n_crimes / zip$POP1996 * 100000, "Blues")
title(main = "Crime rate by zip code")


# PLOT WITH GOOGLE
library(googleVis)
n_rows <- 100
goo_loc <- paste(term_of_interest$lat[1:n_rows], term_of_interest$lon[1:n_rows], sep = ":")
goo_gnv <- data.frame(term_of_interest[1:n_rows,], goo_loc)
g.inter.map <- gvisMap(data = goo_gnv, 
                       locationvar = "goo_loc",
                       options=list(showTip=TRUE, 
                                    showLine=TRUE, 
                                    enableScrollWheel=TRUE,
                                    mapType='hybrid', 
                                    useMapTypeControl=TRUE,
                                    width=800,
                                    height=400),
                       tipvar = "narrative")
plot(g.inter.map)



# http://zevross.com/blog/2014/04/11/using-r-to-quickly-create-an-interactive-online-map-using-the-leafletr-package/
# Clean up zip code stuff to add to it

# give better variable name
zip$pop <- zip$POP1996

# create a df version of zip
zip_df <- data.frame(zip)

# simplify to spatial polygons class
library(leafletR)
library(rgdal) #for reading/writing geo files
library(rgeos) #for simplification
library(sp)


downloaddir<-paste0(getwd(), "/zips_alachua" )

filename<-list.files(downloaddir, pattern=".shp", full.names=FALSE)
filename<-gsub(".shp", "", filename)

# ----- Change zip to dat
dat <- zip
subdat<- dat

# ----- Transform to EPSG 4326 - WGS84 (required)
subdat<-spTransform(subdat, CRS("+init=epsg:4326"))

# ----- change name of field we will map
names(subdat)[names(subdat) == "POP1996"]<-"Population"

# # ----- save the data slot
subdat_data<- subdat@data

# ----- simplification yields a SpatialPolygons class
# subdat<-gSimplify(subdat,tol=0.01, topologyPreserve=TRUE)

# ----- to write to geojson we need a SpatialPolygonsDataFrame
subdat<-SpatialPolygonsDataFrame(subdat, data=subdat_data)



# ----- Write data to GeoJSON
leafdat<-paste(downloaddir, "/", filename, ".geojson", sep="") 

#zipgj <- toGeoJSON(data = zip, dest = paste0(getwd(),"/zips_alachua"))
zipgj <- toGeoJSON(data = subdat, dest = paste0(getwd(),"/zips_alachua"))

# # ------ Function to write to GeoJSON
# togeojson <- function(file, writepath = "~") {
#   url <- "http://ogre.adc4gis.com/convert"
#   tt <- POST(url, body = list(upload = upload_file(file)))
#   out <- content(tt, as = "text")
#   fileConn <- file(writepath)
#   writeLines(out, fileConn)
#   close(fileConn)
# }
# 
# library(httr)
# file <- paste0(getwd(), "/zips_alachua/", "ACDPS_zipcode.zip")
# togeojson(file, leafdat)


# ----- Create the cuts
cuts<-round(quantile(subdat$Population, probs = seq(0, 1, 0.20), na.rm = FALSE), 0)
cuts[1]<-0 # ----- for this example make first cut zero


# ----- Fields to include in the popup
popup<-c("ZIP", "Population")


# ----- Gradulated style based on an attribute
sty<-styleGrad(prop="Population", breaks=cuts, right=FALSE, style.par="col",
               style.val=rev(heat.colors(6)), leg="Population", lwd=1)




# ----- Create the map and load into browser
map<-leaflet(data=zipgj, dest=downloaddir, style=sty,
             title="index", base.map="osm",
             incl.data=TRUE,  popup=popup)

# ----- to look at the map you can use this code
browseURL(map)


#################################
# MODELING
##################################
# 
# # Let's just look at 2013
# gnv <- gnv[which(gnv$date >= "2013-01-01" &
#                    gnv$date <= "2013-12-31"),]

#install.packages("weatherData")
library(weatherData)

# Get weather for a period of time
weather13 <- getSummarizedWeather("GNV", 
                                  start_date = '2013-01-01',#min(gnv$date), 
                                  end_date = '2013-12-31',#max(gnv$date),
                                  opt_custom_columns = TRUE,
                                  custom_columns = c(2,4,20))
weather14 <- getSummarizedWeather("GNV", 
                                  start_date = '2014-01-01',#min(gnv$date), 
                                  end_date = '2014-12-31',#max(gnv$date),
                                  opt_custom_columns = TRUE,
                                  custom_columns = c(2,4,20))
weather15 <- getSummarizedWeather("GNV", 
                                  start_date = '2015-01-01',#min(gnv$date), 
                                  end_date = '2015-02-01',#max(gnv$date),
                                  opt_custom_columns = TRUE,
                                  custom_columns = c(2,4,20))

weather <- rbind(weather13, weather14, weather15)

# write.csv(weather, 'weather.csv')
# weather <- read.csv('weather.csv', stringsAsFactors = FALSE)

# format
weather$date <- as.Date(weather$Date, format = "%Y-%m-%d")

# # Merge to gnv
# gnv <- merge(x = gnv,
#              y = weather,
#              by = "date",
#              all.x = TRUE,
#              all.y = FALSE)


# Glimpse at your data
head(weather)

# Clean
weather$rain <- as.numeric(weather$PrecipitationIn)


# Plot max temperature
plot(weather$date, weather$Max_TemperatureF, 
     col = adjustcolor("darkred", alpha.f = 0.3),
     pch = 16,
     ylim = c(20, 100),
     xlab = "Date",
     ylab = "Temp")

# Add min temperature
points(weather$date, weather$Min_TemperatureF, 
       col = adjustcolor("darkblue", alpha.f = 0.3),
       pch = 16)

# Add legend
legend("bottom",
       pch = 16,
       col = adjustcolor(c("darkred", "darkblue"), alpha.f = 0.3),
       legend = c("High", "Low"),
       bty = "n")

# Where are we?
abline(v = Sys.Date() - c(0, 365, 730),
       col = adjustcolor("black", alpha.f = 0.4),
       lwd = 2)

####################
# Merge crime time series with weather
#gnv$Date <- NULL
library(dplyr)
ts <- gnv %>%
  group_by(date, zip) %>%
  summarise(n = n())

ts <- left_join(ts, weather, by = 'date')

# day of week
ts$day_of_week <- weekdays(ts$date)
ts$day_of_week <- relevel(factor(ts$day_of_week),
                         ref = 'Monday')

# MODEL
fit <- lm(n ~ factor(zip) + 
            #rain + 
            Max_TemperatureF + 
            #date +
            day_of_week, 
          data = ts)
summary(fit)

# PREDICT FOR TOMORROW
tomorrow <- data.frame(date = as.Date(Sys.Date()+1),
                       zip = unique(sort(gnv$zip)),
                       Max_TemperatureF  = 86,
                       rain = 0.2,
                       day_of_week = 'Friday')
tomorrow$date <- as.Date(tomorrow$date)
tomorrow

tomorrow$prediction <- predict(fit, tomorrow)
tomorrow

preds <- predict(fit, tomorrow, interval = 'prediction')
tomorrow$lwr <- preds[,2]
tomorrow$upr <- preds[,3]
tomorrow

# PLOT FORECAST
zip$forecast <- NA
for (i in 1:nrow(tomorrow)){
  zip$forecast[which(as.numeric(as.character(zip$ZIP)) ==
                       tomorrow$zip[i])] <-
    tomorrow$prediction[i]
}

# source choro
source('/home/joebrew/Documents/uf/phc7065/big_data/functions.R')
zip_border <- collapse_map(zip)
par(mar = c(5,5,5,2))
choro(shape = zip,
      boundary = zip_border,
      main = 'Crime forecast for tomorrow',
      var = zip$forecast,
      legend_round = 0)

