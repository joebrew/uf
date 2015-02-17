get_airport_weather <- function(station = "BGT", # CDG, BGT, ATL, JFK
                        start_date = Sys.Date() - 365,
                        end_date = Sys.Date()){

  # Format station name
  station <- toupper(gsub(" ", "%20", station))
  
  # Parse date components
  start_date <- as.Date(start_date, format = "%Y-%m-%d")
  start_day <- as.numeric(format(start_date, "%d"))
  start_month <- as.numeric(format(start_date, "%m"))
  start_year <- as.numeric(format(start_date, "%Y"))
  
  end_date <- as.Date(end_date, format = "%Y-%m-%d")
  end_day <- as.numeric(format(end_date, "%d"))
  end_month <- as.numeric(format(end_date, "%m"))
  end_year <- as.numeric(format(end_date, "%Y"))
  
  
    # Define link format for airports
    link <- paste0("http://www.wunderground.com/history/airport/",
                   station,
                   "/", start_year, 
                   "/", start_month, 
                   "/", start_day, 
                   "/CustomHistory.html?dayend=", end_day, 
                   "&monthend=", end_month, 
                   "&yearend=", end_year, 
                   "&req_city=NA&req_state=NA&req_statename=NA&format=1")
    
#     # Read in data from link
     df <- read.csv(link)
#     
#     # Clean up some column names (to be compatible with weather stations)
    Date <- df$EST ; df$EST <- NULL
    df$Mean.Sea.Level.PressureIn <- NULL
    df <- cbind(Date, df)
    names(df) <- gsub("[.]", "", names(df))
    df <- df[,which(!grepl("Visibility|Wind|Gust|MeanSeaLevelPressureIn|Events", names(df)))]
#   
#   # Format date to date object
    df$Date <- as.Date(df$Date, format = "%Y-%m-%d")
#   
#   # Standardize names
  names(df) <- c("date",
                 "temp_max",
                 "temp_mean",
                 "temp_min",
                 "dewpoint_max",
                 "dewpoint_mean",
                 "dewpoint_min",
                 "humidity_max",
                 "humidity_mean",
                 "humidity_min",
                 "pressure_max",
                 "pressure_min",
                 "precipitation",
                 "cloud_cover")
#   
  # Add a location column
  df$loc <- toupper(as.character(station))
  
  # print url source
  print(link)
  
  return(df)
}

# Get five years of data
for (i in 2010:2015){
  assign(paste0("weather", i), 
         get_airport_weather("KGNV",
                             start_date = paste0(i, "-01-01"),
                             end_date = paste0(i, "-12-31")))
}

# Bind together
weather <- rbind(weather2010,
                 weather2011,
                 weather2012,
                 weather2013,
                 weather2014,
                 weather2015)

# Write csv
write.csv(weather, 'gainesville_weather.csv')

# Plot
plot(weather$date, weather$temp_max)
points(weather$date, weather$temp_min, col = "blue")
