options(stringsAsFactors = FALSE)

#####
# LOAD NICE PACKAGES
#####
library(plyr)

#####
# SET WORKING DIRECTORY TO WHEREVER 
# YOU'VE GOT THE ILI / ED / WEATHER DATA
#####
setwd('uf/phc7000/time_series')

#####
# READ IN ILI AND ALL ED DATA
#####
ili <- read.csv('ili.csv')
ed <- read.csv('all_emergency_alachua.csv')

#####
# READ IN WEATHER DATA
# (use the get_weather.R script to scrape weather data)
#####
weather <- read.csv('gainesville_weather.csv')

#####
# MERGE ALL TOGETHER
#####
temp <- join(x = ed, type = "left", 
                  y = ili,
                  by = "date")
df <- join(x = temp, type = "left",
                y = weather,
                by = "date")
rm(temp, ed, ili, weather)

####
# CLEAN UP A FEW COLUMNS
####
# format "date" as a date object
df$date <- as.Date(df$date)

# make sure "precipitation" is numeric
df$precipitation <- as.numeric(df$precipitation)

#####
# MAKE SOME FUN PLOTS
#####

# max daily temperature
plot(df$date, df$temp_max, xlab = "Date", ylab = "Temperature",
     col = adjustcolor("black", alpha.f = 0.3),
     ylim = c(0, 105))

# add minimum points
points(df$date, df$temp_min,
       col = adjustcolor("blue", alpha.f = 0.3))

# precipitation

# mean humidity

# correlation precipitation / humidity
plot(df$precipitation, df$humidity_mean)

# correlation min and max temp


# correlation cloud cover and precipitation
grouped <- df %>% group_by(cloud_cover) %>%
  summarise(precipitation = mean(precipitation, na.rm = TRUE))

barplot(grouped$precipitation,
        names.arg = grouped$cloud_cover,
        col = "lightblue",
        xlab = "Cloud cover",
        ylab = "Mean daily precipitation (in)")
box("plot")

#####
# TASK
#####

###
# 1. MAKE SOME INTERESTING FEATURES 
###
# day of week
df$day_of_week <- weekdays(df$date)

# weekend

# month

# day of year

# ili_cases as a percentage of all_cases

# mean rolling 21 day humidity average
df$rolling_average <- NA
for (i in 1:nrow(df)){
  df$rolling_average[i] <- 
    mean(df$humidity_mean[which(df$date > (df$date[i] - 21))], na.rm= TRUE)
}

###
# 2. PLOT CORRELATION BETWEEN DAY OF YEAR AND ILI CASES
###

###
# 3. PLOT CORRELATION BETWEEN HUMIDITY 
###

###
# 4. MAKE LINEAR MODEL TO PREDICT ILI CASES
###


###
# 5. DO THE SAME BUT USING POISSON
###
# (the logarithm of ili_cases is a function of...)
fit <- glm(ili_cases ~ temp_min,
           data = df,
           family = poisson(link = 'log'))
exp(coef(fit))

# Quasipoisson
fit_qpois <- glm(ili_cases ~ temp_min, 
                 data = df,
                 family = quasipoisson)

# Negative binomial regression
fit_nbi <- glm(ili_cases ~ temp_min,
               data = df)

# Add predictions
df$predicted_poisson <- predict(fit, newdata = df, type = "response")
df$predicted_qpois <- predict(fit_qpois, newdata = df, type = "response")
df$predicted_nbi <- predict(fit_nbi, newdata = df, type = "response")

# Visualize
plot(df$date, df$ili_cases, type = "l")
lines(df$date, df$predicted, col = "red", type = "l")
lines(df$date, df$predicted_qpois, col = "green", type = "l")
lines(df$date, df$predicted_nbi, col = "blue", type = "l")

###
# 6. WHICH DID BETTER / POISSON OR GAUSSIAN?
### 



#############################
# TRICKS
#############################



# What happens when you adjust your lag?
# mean rolling humidity average
for(j in 0:30){
  df$humidity_rolling_average <- NA
  for (i in 1:nrow(df)){
    df$humidity_rolling_average[i] <- 
      mean(df$humidity_mean[which(df$date >= (df$date[i] - j) &
                                    df$date <= df$date[i])], na.rm= TRUE)
  }
  plot(df$date, df$humidity_rolling_average, main = paste0("Rolling average of ", j+1, " days"))
  Sys.sleep(1)
}

# mean rolling max temperature average
plot(df$date, df$temp_max, col = adjustcolor("black", alpha.f = 0.3))
plot(df$date, df$temp_max,  type = "n")

for(j in seq(5,365,5)){
  df$temp_max_rolling_average <- NA
  df$temp_min_rolling_average <- NA
  
  for (i in 1:nrow(df)){
    df$temp_max_rolling_average[i] <- 
      mean(df$temp_max[which(df$date >= (df$date[i] - j) &
                                    df$date <= df$date[i])], na.rm= TRUE)
    df$temp_min_rolling_average[i] <- 
      mean(df$temp_min[which(df$date >= (df$date[i] - j) &
                                    df$date <= df$date[i])], na.rm= TRUE)
  }
  lines(df$date, df$temp_max_rolling_average,
        col = adjustcolor("darkred", alpha.f = 0.3))
  lines(df$date, df$temp_min_rolling_average,
        col = adjustcolor("blue", alpha.f = 0.3))
  #text(main = paste0("Rolling average of ", j+1, " days"))
  Sys.sleep(0.2)
}

#####
# ARIMA
#####
# Create time series object
ts2 <- ts(df$ili_cases,#[which(df$date <= "2014-01-01")],
          start = c(2012, 1),
          end = c(2015,12),
          frequency = 365)

# Seasonal decomposition
fit <- stl(ts2, s.window = "period")
plot(fit)

# additional plots
monthplot(ts2)
library(forecast)
seasonplot(ts2)

# fit an ARIMA model of order P, D, Q
fit <- arima(ts2, order=c(1, 0, 0))
             
# predictive accuracy
accuracy(fit)

# predict next 5 observations
forecast(fit, 5)
plot(forecast(fit, 5))

# Automated forecasting using an exponential model
fit <- ets(ts2)

# Automated forecasting using an ARIMA model
fit <- auto.arima(ts2)

