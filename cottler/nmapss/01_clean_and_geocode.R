library(sas7bdat)
library(car)
library(dplyr)
library(ggmap)
library(geocodeHERE)

########
# ESTABLISH WD FOR PRIVATE DATA
########
if ( Sys.info()["sysname"] == "Linux" ){
  public <- "/home/joebrew/Documents/uf/cottler"
  private <- "/media/joebrew/JB/uf/cottler/nmapss"
} else if(Sys.info()["user"] == "BrewJR" ){
  public <- "C:/Users/BrewJR/Documents/uf/cottler"
  private <- "E:/uf/cottler/nmapss"
} else {
  return("Use other system.")
}
setwd(public)


########
# READ IN DATA
########
nmapss <- read.sas7bdat(paste0(private,
                          "/nmapssw4.sas7bdat"))

########
# FILL IN ALL MISSINGS WITH NA'S
########

# Define function for missing
Missing <- function(var){
  nchar(as.character(var)) == 0
}

# Set to NA anything that is missing
for (j in 1:ncol(nmapss) ){
  x <- nmapss[,j]
  x[which(Missing(x))] <- NA
  nmapss[,j] <- x
}

########
# RECODE VARIABLE NAMES
########

nmapss$gender <- Recode(nmapss$Ques001,
                        "1 = 'male';
                        2 = 'female'")

nmapss$age <- nmapss$Ques002

nmapss$latino <- ifelse(nmapss$Ques003 ==1, FALSE,
                        ifelse(nmapss$Ques003 == 5, TRUE,
                               NA))

nmapss$race <- Recode(nmapss$Ques004,
                      "1 = 'AK/Eskimo';
                      2 = 'AmerInd';
                      3 = 'Asian';
                      4 = 'Black';
                      5 = 'MidEast';
                      6 = 'PacIsland';
                      7 = 'White';
                      8 = 'Multi';
                      9 = 'Othe'")

nmapss$zip <- nmapss$Ques005

nmapss$lived_with_mom_and_dad <- 
  as.logical(Recode(nmapss$Ques006A,
         "1 = 'FALSE';
         5 = 'TRUE'"))

# Skipping lived with mom / dad / foster parents / siblings vars

nmapss$family_dinner_week <- nmapss$Ques011

nmapss$grades <- Recode(nmapss$Ques013,
                        "1 = 'A';
                        2 = 'B';
                        3 = 'C';
                        4 = 'D';
                        5 = 'F';
                        98 = NA")

nmapss$grade <- nmapss$Ques012

nmapss$ever_supsended <- as.logical(Recode(nmapss$Ques014,
                                           "1 = 'FALSE';
                                           5 = 'TRUE'"))

nmapss$job <- as.logical(Recode(nmapss$Ques015,
                                           "1 = 'FALSE';
                                           5 = 'TRUE'"))

# Skipping job details
# Skipping cash, debit card, sports, money questions

nmapss$tv_hours <- nmapss$Ques021r

nmapss$video_game_hours <- nmapss$Ques022r

# Skipping bed time
# Skpping wake up time

nmapss$health <- Recode(nmapss$Ques025,
                        "1 = 'Excellent';
                        2 = 'Good';
                        3 = 'Fair';
                        4 = 'Poor'")

# Skpping police tickets/warnings, arrests
# Attention disorders
# Trouble / run away
# Weapon use / threat

nmapss$afraid_gain_weight <- as.logical(Recode(nmapss$Ques033,
                                               "1 = 'FALSE';
                                               5 = 'TRUE'"))
nmapss$vomit_to_lose_weight <- as.logical(Recode(nmapss$Ques034A,
                                                 "1 = 'FALSE';
                                               5 = 'TRUE'"))
nmapss$pills_to_lose_weight <- as.logical(Recode(nmapss$Ques034B,
                                                 "1 = 'FALSE';
                                               5 = 'TRUE'"))
nmapss$fast_to_lose_weight <- as.logical(Recode(nmapss$Ques034C,
                                                 "1 = 'FALSE';
                                               5 = 'TRUE'"))
nmapss$excess_exercise_to_lose_weight <- as.logical(Recode(nmapss$Ques034D,
                                                 "1 = 'FALSE';
                                               5 = 'TRUE'"))

# Skipping depression, loss of interest, stress
# Skipping religious attendance (p. 30)
# Skipping seen this medicine (31)
# Skpping info on medicine (32)
# Skipping drug use (33), adderall, concerta, daytrana, ritalin, vyvanse
# focalin, focalin xr, drexedrine, metadate, meth, amphetamines, etc.
# Skipping all drug questions through page 59/76

nmapss$marijuana_ever <- as.logical(Recode(nmapss$Ques114,
                                           "1 = 'FALSE';
                                               5 = 'TRUE'"))

nmapss$marijuana_age_first <- nmapss$Ques115

nmapss$marijuana_past_30_days <-Recode(nmapss$Ques116,
                                       "0 = 'zero';
                                       1 = '1-2 days';
                                       2 = '3-5 days';
                                       3 = '6-9 days';
                                       4 = '10-19 days';
                                       5 = '20-29 days';
                                       6 = 'All 30 days'")

# Skipping parent/guardian warning about marijuana

nmapss$ever_cocaine <- as.logical(Recode(nmapss$Ques117A,
                              "1 = 'FALSE';
                              5 = 'TRUE'"))

nmapss$ever_heroin <- as.logical(Recode(nmapss$Ques117B,
                              "1 = 'FALSE';
                              5 = 'TRUE'"))

nmapss$ever_ecstasy <- as.logical(Recode(nmapss$Ques117C,
                              "1 = 'FALSE';
                              5 = 'TRUE'"))

nmapss$ever_hallucinogens <- as.logical(Recode(nmapss$Ques117D,
                              "1 = 'FALSE';
                              5 = 'TRUE'"))

nmapss$ever_anabolic_steroids <- as.logical(Recode(nmapss$Ques117E,
                              "1 = 'FALSE';
                              5 = 'TRUE'"))

nmapss$ever_cough_syrup <- as.logical(Recode(nmapss$Ques117F,
                              "1 = 'FALSE';
                              5 = 'TRUE'"))

nmapss$ever_methamphetamine <- as.logical(Recode(nmapss$Ques117G,
                              "1 = 'FALSE';
                              5 = 'TRUE'"))

nmapss$ever_inhalants <- as.logical(Recode(nmapss$Ques117H,
                              "1 = 'FALSE';
                              5 = 'TRUE'"))

nmapss$never_drugs117 <- as.logical(Recode(nmapss$Ques118A,
                                        "1 = 'FALSE';
                              5 = 'TRUE'"))

# Skipping age of drugs, energy drinks, cigarettes

nmapss$alcohol_ever <- as.logical(Recode(nmapss$Ques124,
                                         "1 = 'FALSE';
                              5 = 'TRUE'"))

nmapss$alcohol_age_first <- nmapss$Ques125

nmapss$alcohol_drinks_last_week <- nmapss$Ques126

nmapss$alcohol_days_last_month <- Recode(nmapss$Ques127,
                                         "0 = 'zero';
                                       1 = '1-2 days';
                                       2 = '3-5 days';
                                       3 = '6-9 days';
                                       4 = '10-19 days';
                                       5 = '20-29 days';
                                       6 = 'All 30 days'")

nmapss$alcohol_binge_last_month <- as.logical(Recode(nmapss$Ques127,
                                                     "1 = 'FALSE';
                                                      5 = 'TRUE'"))

nmapss$alcohol_energy_mix <- as.logical(Recode(nmapss$Ques128A,
                                                     "1 = 'FALSE';
                                                      5 = 'TRUE'"))

# Skipping how got alcohol
# Skipping parental warnings about alcohol

nmapss$gamble_internet <- as.logical(Recode(nmapss$Ques130,
                                            "1 = 'FALSE';
                                                      5 = 'TRUE'"))

nmapss$second_life <- Recode(nmapss$Ques131,
                             "1 = 'No';
                             2 = 'Yes';
                             3 = 'Never heard of it'")

# Skipping question son other kids your age, adderall, etc.

nmapss$close_friends <- nmapss$Ques134

# Skipping questions on close friends
# Skpping questions on truthfulness

########
# Geocode zips
########
zip_df <- data.frame("zip" = unique(sort(nmapss$zip)))
zip_df$zip_long <- as.character(paste0("zip code ", zip_df$zip))
locations <- geocode(zip_df$zip_long)

zip_df$lon<- locations$lon
zip_df$lat <- locations$lat

# Merge zip_df to nmapss
nmapss <- merge(x = nmapss,
           y = zip_df,
           by = "zip",
           all.x = TRUE,
           all.y = FALSE)
########
# KEEP ONLY RECODED VARIABLES
########
rm(locations, x, zip_df, j, Missing)

nmapss <- nmapss[,!grepl("Ques", colnames(nmapss))]

########
# Save
########
save.image(paste0(private,
                  "/nmapssw4_geocoded.RData"))

########
# MAP
########
library(maps)
map("usa")
points(nmapss$lon, nmapss$lat,
       pch = 16,
       col = adjustcolor("blue", alpha.f = 0.2))

########
# READ IN ZIP CODE MAP
########
library(rgdal)
zip <- readOGR("/home/joebrew/Desktop/zip_code",
               "tl_2013_us_zcta510")

fl <- zip[which(coordinates(zip)[,1] < -81 &
                  coordinates(zip)[,1] > -83 &
                  coordinates(zip)[,2] > 26 &
                  coordinates(zip)[,2] < 28),]

# get n per zip
library(dplyr)
zip_df <- nmapss %>%
  group_by(as.character(zip)) %>%
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

points(nmapss$lon, nmapss$lat,
       pch = 16,
       col = adjustcolor("blue", alpha.f = 0.2))


########
#
########
my_table <- table(nmapss$city)
my_colors <- colorRampPalette(c("red", "darkblue"))(length(my_table))
barplot(my_table, col = my_colors)

########
# GEOCODE HERE (turned up too many NAś)
########
# # Create a matching zip_df
# zip_df <- data.frame("id" = 1:length(unique(sort(nmapss$zip))),
#   "zip" = paste0("Zip code ", unique(sort(nmapss$zip))))
# address_str <- df_to_string(zip_df)
# 
# # Use the geocodeHERE
# request_id <- geocodeHERE_batch_upload(address_string = address_str,
#                                        email_address = "joebrew@gmail.com")
# 
# # Check status
# while(geocodeHERE_batch_status(request_id) != "completed"){
#   print(geocodeHERE_batch_status(request_id))
#   Sys.sleep(1)
# }
# geocodeHERE_batch_status(request_id)
# 
# # download the data
# geocode_data <- geocodeHERE_batch_get_data(request_id)
# # match it back to your original addresses dataframe
# zip_df <- merge(zip_df, geocode_data, by.x="id", by.y="recId", all.x=T)
# str(zip_df)

