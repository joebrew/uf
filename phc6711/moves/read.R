zipfile <- "exportfeb20.zip"
location <- "/home/joebrew/Documents/uf/phc6711/moves"

# SET WD 
setwd(location)

# UNZIP MASTER FILE
unzip(zipfile = zipfile)

# UNZIP CSV
unzip(zipfile = 'csv.zip')

# CD INTO THE CSV DAILY FOLDER
setwd('csv/daily')

# READ JUST SUMMARY
setwd('summary')
for (i in dir()){
  assign(gsub(".csv", "", i), read.csv(i))
}

# LOOP THROUGH EACH DATAFRAME AND BIND

# Get list of current dataframes
dfs <- names(which(sapply(.GlobalEnv, is.data.frame))) 

# Make master dataframe to populate
master <- data.frame(Date = NA,
                     Activity = NA,
                     Group = NA, 
                     Duration = NA,
                     Distance = NA,
                     Steps = NA,
                     Calories = NA)
for (i in 1:length(dfs)){
  df <- get(dfs[i])
  master <- rbind(master, df)
}

# FORMAT MASTER

# make date object
master$Date <- as.Date(master$Date, format = "%m/%d/%y")

# order
master <- master[order(master$Date),]

########################################
# END OF DATA READ IN

#########################################
# BEGINNING OF DATA VIS
library(dplyr)

# Get overall calories by day
temp <- master %>%
  group_by(Date) %>%
  summarise(Distance = sum(Distance, na.rm = TRUE))
barplot(temp$Distance,
        names.arg = temp$Date,
        las = 3, 
        cex.names = 0.6)

# Plot duration just by type
temp <- master %>%
  filter(Activity == "walking") %>%
  group_by(Date, Activity) %>%
  summarise(walk = sum(Duration, na.rm = TRUE))
temp$Activity <- NULL

temp2 <- master %>%
  filter(Activity == "cycling") %>%
  group_by(Date, Activity) %>%
  summarise(bike = sum(Duration, na.rm = TRUE))
temp2$Activity <- NULL

temp3 <- master %>%
  filter(Activity == "running") %>%
  group_by(Date, Activity) %>%
  summarise(run = sum(Duration, na.rm = TRUE)) 
temp3$Activity <- NULL

tt <- left_join(left_join(temp, temp2), temp3)

mat <- as.matrix(tt[,2:4])
mat <- t(mat) / 60
barplot(mat, beside = TRUE, legend = TRUE, col = c("red", "blue", "green"),
        names.arg = tt$Date,
        las = 3)
title(main = "Minutes per day")
box("plot")

