library(sas7bdat)

########
# READ IN DATA
########
x <- read.sas7bdat(paste0("/home/joebrew/Desktop/cottler/nmapss/",
                          "nmapssw4.sas7bdat"))


########
# Geocode zips
########
library(ggmap)
locations <- geocode(paste0("zip code ",x$Ques005[1:2500]))
x$lon <- NA
x$lon[1:2500] <- locations$lon
x$lat <- NA
x$lat[1:2500] <- locations$lat

########
# Save
########
save.image(paste0("/home/joebrew/Desktop/cottler/nmapss/",
                  "nmapssw4_geocoded.RData"))

########
# MAP
########
library(maps)
map("usa")
points(x$lon, x$lat,
       pch = 16,
       col = adjustcolor("blue", alpha.f = 0.2))

########
# READ IN ZIP CODE MAP
########
zip <- readOGR("/home/joebrew/Desktop/zip_code",
               "tl_2013_us_zcta510")

fl <- zip[which(coordinates(zip)[,1] < -81 &
                  coordinates(zip)[,1] > -83 &
                  coordinates(zip)[,2] > 26 &
                  coordinates(zip)[,2] < 28),]

# get n per zip
library(dplyr)
zip_df <- x %>%
  group_by(as.character(Ques005)) %>%
  summarise(n = n())
names(zip_df)[1] <- "ZCTA5CE10"

# loop into fl
fl@data <- merge(fl@data,
                 zip_df,
                 by = "ZCTA5CE10",
                 all.x = T,
                 all.y = F)

my_colors <- colorRampPalette(c("red", "white", "green"))(max(fl$n, na.rm =T))

plot(fl,
     col = my_colors[fl$n])

points(x$lon, x$lat,
       pch = 16,
       col = adjustcolor("blue", alpha.f = 0.2))


########
#
########
my_table <- table(x$city)
my_colors <- colorRampPalette(c("red", "darkblue"))(length(my_table))
barplot(my_table, col = my_colors)

########
#
########

