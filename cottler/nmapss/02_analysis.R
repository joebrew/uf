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
# Read in cleaned data
########
load(paste0(private, "/nmapss_geocoded.RData"))

########
# EXPLORE RELATIONSHIP BETWEEN ALCOHOL AND GAMBLING
########

# Here are the variables related to alcohol
names(nmapss[which(grepl("alcoho", names(nmapss)))])

########
# 1. Cottler: are those who gamble also those who drink?
########

my_colors <- adjustcolor(c("darkorange", "lightblue"), alpha.f = 0.8)

# drink only graphic
my_table <- table(nmapss$alcohol_ever)
my_prop_table <- prop.table(my_table)
my_barplot <-barplot(my_table,
                     main = "Ever drink alcohol",
                     col = my_colors,
                     names.arg = c("Nope", "Yep"),
                     ylim = c(0, max(my_table) * 1.2))
text(x = my_barplot[,1],
     y = my_table,
     pos = 1,
     labels = my_table)
text(x = my_barplot[,1],
     y = my_table,
     pos = 3,
     labels = paste0(round(my_prop_table*100, digits =2), "%"))

# internet gamble only graphic
my_table <- table(nmapss$gamble_internet)
my_prop_table <- prop.table(my_table)
my_barplot <-barplot(my_table,
                     main = "Ever gamble online",
                     col = my_colors,
                     names.arg = c("Nope", "Yep"),
                     ylim = c(0, max(my_table) * 1.2))

text(x = my_barplot[,1],
     y = my_table,
     pos = 1,
     labels = my_table,
     cex = 0.6)
text(x = my_barplot[,1],
     y = my_table,
     pos = 3,
     labels = paste0(round(my_prop_table*100, digits =2), "%"))

# Cross tabulation
x <- ifelse(nmapss$alcohol_ever, "ever drink", "never drink")
y <- ifelse(nmapss$gamble_internet, "ever gamble", "never gamble")
my_table <- table(x,y)
my_table
chisq.test(my_table)
my_barplot <- barplot(my_table, 
                      col = my_colors,
                      beside = TRUE,
                      ylim = c(0,max(my_table) *1.1))
legend(x = "topleft",
       bty = "n",
       fill = my_colors,
       legend = c("ever drink", "never drink"))
text(x = my_barplot,
     y = my_table,
     labels = my_table,
     pos = 3)
my_prop_table <- prop.table(my_table, 2) * 100
text(x = my_barplot,
     y = my_table,
     labels = paste0(round(my_prop_table, digits = 2), "%"),
     pos = 1,
     cex = 0.6)

# Is this significant?
fit <- glm(gamble_internet ~ 
             alcohol_ever,
           data = nmapss,
           family = binomial("logit"))
summary(fit)

# Odds ratios
exp(coef(fit))

## odds ratios and 95% CI
exp(cbind(OR = coef(fit), confint(fit)))

########
# BUT ISN'T GENDER AN ISSUE?
########
x <- nmapss$gender
y <- ifelse(nmapss$gamble_internet, "ever gamble", "never gamble")
my_table <- table(x,y)
my_table
chisq.test(my_table)
my_barplot <- barplot(my_table, 
                      col = my_colors,
                      beside = TRUE,
                      ylim = c(0,max(my_table) *1.1))
legend(x = "topleft",
       bty = "n",
       fill = my_colors,
       legend = c("female", "male"))
text(x = my_barplot,
     y = my_table,
     labels = my_table,
     pos = 3)
my_prop_table <- prop.table(my_table, 2) * 100
text(x = my_barplot,
     y = my_table,
     labels = paste0(round(my_prop_table, digits = 2), "%"),
     pos = 1,
     cex = 0.6)



########
# 2. View relationship after adjustment
########

# Is this significant?
fit <- glm(gamble_internet ~ 
             alcohol_ever + gender + age_group, #factor(age),
           data = nmapss,
           family = binomial("logit"))
summary(fit)

# Odds ratios
exp(coef(fit))

## odds ratios and 95% CI
x <- exp(cbind(OR = coef(fit), confint(fit)))
x
x <- data.frame(x[-1,])
my_barplot <- barplot(x$OR,
        #names.arg = row.names(x),
        names.arg = c("Ever alcohol", "Male", "14-16 y.o.", "17-18 y.o."),
        col = "darkorange",
        ylim = c(0,max(x$OR)*1.2),
        ylab = "Odds ratios for online gambling",
        border = NA)
box("plot")
abline(h = 1, col = adjustcolor("black", alpha.f = 0.7))
library(Hmisc)
errbar(x = my_barplot[,1],
       y = x$OR,
       yminus = x$X2.5..,
       yplus = x$X97.5..,
       pch = NA,
       add = TRUE,
       errbar.col = adjustcolor("darkblue", alpha.f = 0.8))


########
# 2. ADD ALL VARIABLES
########
nmapss$health <- factor(nmapss$health,
                        levels = c("Excellent", "Good", "Fair", "Poor"))
nmapss$location <- factor(nmapss$location,
                          levels = c("urban", "suburban", "rural"))
nmapss$family_dinner_week <- factor(ifelse(nmapss$family_dinner_week <=3,"<=3 weekly family meals",
                                           ifelse(nmapss$family_dinner_week >=4, ">= 4 weekly family meals",
                                                  NA)))
fit <- glm(gamble_internet ~ 
             alcohol_ever + gender + age_group + location +
             family_dinner_week + marijuana_ever + health +
             close_friends, 
           data = nmapss,
           family = binomial("logit"))
summary(fit)

# Odds ratios
exp(coef(fit))

## odds ratios and 95% CI
x <- exp(cbind(OR = coef(fit), confint(fit)))
x
x <- data.frame(x[-1,])
x
my_barplot <- barplot(x$OR,
                      #names.arg = row.names(x),
                      names.arg = c("alcohol\never",
                                    "male",
                                    "14-16 y.o.",
                                    "17-18 y.o.",
                                    "suburban",
                                    "rural",
                                    ">=4 weekly\nfamily meals",
                                    "marijuana\never",
                                    "good health",
                                    "fair health",
                                    "poor health",
                                    "close friends"),
                      col = "darkorange",
                      ylim = c(0,max(x$OR)*1.2),
                      ylab = "Odds ratios for online gambling",
                      border = NA,
                      cex.names = 0.6,
                      las = 3)
box("plot")
abline(h = 1, col = adjustcolor("black", alpha.f = 0.7))
library(Hmisc)
errbar(x = my_barplot[,1],
       y = x$OR,
       yminus = x$X2.5..,
       yplus = x$X97.5..,
       pch = NA,
       add = TRUE,
       errbar.col = adjustcolor("darkblue", alpha.f = 0.8))

# Is that model any good?
small_nmapss <- nmapss[,c("gamble_internet",
                          "alcohol_ever",
                          "gender",
                          "age_group",
                          "location",
                          "family_dinner_week",
                          "marijuana_ever",
                          "health",
                          "close_friends")]
no_na_data <- na.omit(small_nmapss)
fit <- glm(gamble_internet ~ 
             alcohol_ever + gender + age_group + location +
             family_dinner_week + marijuana_ever + health +
             close_friends, 
           data = no_na_data,
           family = binomial("logit"))
library(MASS)
step <- stepAIC(fit, direction="both")
step$anova # display results

# FINAL MODEL
fit <- glm(gamble_internet ~ 
             alcohol_ever + gender + age_group +
             family_dinner_week + health +
             close_friends, 
           data = no_na_data,
           family = binomial("logit"))


## odds ratios and 95% CI
x <- exp(cbind(OR = coef(fit), confint(fit)))
x
x <- data.frame(x[-1,])
x
my_barplot <- barplot(x$OR,
                      #names.arg = row.names(x),
                      names.arg = c("alcohol\never",
                                    "male",
                                    "14-16 y.o.",
                                    "17-18 y.o.",
                                    ">=4 weekly\nfamily meals",
                                    "good health",
                                    "fair health",
                                    "poor health",
                                    "close friends"),
                      col = "darkorange",
                      ylim = c(0,max(x$OR)*1.2),
                      ylab = "Odds ratios for online gambling",
                      border = NA,
                      cex.names = 0.6,
                      las = 3)
box("plot")
abline(h = 1, col = adjustcolor("black", alpha.f = 0.7))
library(Hmisc)
errbar(x = my_barplot[,1],
       y = x$OR,
       yminus = x$X2.5..,
       yplus = x$X97.5..,
       pch = NA,
       add = TRUE,
       errbar.col = adjustcolor("darkblue", alpha.f = 0.8))






########
# AGE OF ALCOHOL INITIATION AND GAMBLING LIKELIHOOD
########
age_df <- data.frame("age" = unique(sort(nmapss$alcohol_age_first)))

x <- by(nmapss,
   nmapss$alcohol_age_first,
   function(x){
    nrow(x[which(x[,"gamble_internet"]),]) / 
           nrow(x) * 100
   })
age_df$prob_gamble <- x
age_df <- age_df[which(age_df$age > 1),]
plot(age_df$age, age_df$prob_gamble,
     xlab = "Age at first alcoholic drink",
     ylab = "Probability of having gambled online",
     pch = 16,
     col = "lightblue")
points(age_df$age, age_df$prob_gamble,
       col = "darkorange")
my_line <- lm(age_df$prob_gamble[which(age_df$age >7)] ~ 
                age_df$age[which(age_df$age >7)])
abline(v = 8, lty = 6,
       col = adjustcolor("darkorange", alpha.f = 0.6))
abline(my_line,
       col = adjustcolor("lightblue", alpha.f = 0.8))

text(x = 5,
     y = 5,
     labels = nrow(nmapss[which(nmapss$alcohol_age_first <8),]))

text(x = 12,
     y = 5,
     labels = nrow(nmapss[which(nmapss$alcohol_age_first >=8),]))


#######
# EXAMINE ALCOHOL ONSET BY SEX
#######
boys <- by(nmapss[which(nmapss$gender == "male"),],
        nmapss$alcohol_age_first[which(nmapss$gender == "male")],
        function(x){
          nrow(x[which(x[,"gamble_internet"]),]) / 
            nrow(x) * 100
        })
age_df$prob_gamble_boys <- boys
plot(age_df$age, age_df$prob_gamble_boys,
     xlab = "Age at first alcoholic drink",
     ylab = "Probability of having gambled online",
     pch = 16,
     col = "lightblue")

girls <- by(nmapss[which(nmapss$gender == "female"),],
           nmapss$alcohol_age_first[which(nmapss$gender == "female")],
           function(x){
             nrow(x[which(x[,"gamble_internet"]),]) / 
               nrow(x) * 100
           })
#girls <- girls[-1]
age_df$prob_gamble_girls <- girls

points(age_df$age, age_df$prob_gamble_girls,
       col = "darkorange", pch =16)

legend(x = "topright",
       col = c("lightblue", "darkorange"),
       legend = c("boys", "girls"),
       bty = "n",
       pch = 16)

abline(v = 8, lty = 6,
       col = adjustcolor("darkgreen", alpha.f = 0.6))

boys_line <- lm(age_df$prob_gamble_boys[which(age_df$age >7)] ~ 
                age_df$age[which(age_df$age >7)])
abline(boys_line,
       col = adjustcolor("lightblue", alpha.f = 0.8))

girls_line <- lm(age_df$prob_gamble_girls[which(age_df$age >7)] ~ 
                  age_df$age[which(age_df$age >7)])
abline(girls_line,
       col = adjustcolor("darkorange", alpha.f = 0.8))

########
# Affect of age at first drink - multivariate model
########
over7 <- nmapss[which(nmapss$alcohol_age_first >= 8),]

fit <- glm(gamble_internet ~ 
             alcohol_age_first + gender*age,
           data = over7,
           family = binomial("logit"))
summary(fit)

# Odds ratios
exp(coef(fit))

## odds ratios and 95% CI
exp(cbind(OR = coef(fit), confint(fit)))

########
# GAM
########

# SIMPLE ##########
library(mgcv)
my_gam <- gam(gamble_internet ~  gender + s(alcohol_age_first), 
              data = nmapss,
              family=binomial("logit"))
plot(my_gam,
     xlab = "Age at first drink",
     ylab = "OR of gambling")
vis.gam(my_gam,
        color="terrain",
        plot.type="contour",
        main = "Risk of online gambling",
        xaxt = "n",
        xlab = "Gender",
        ylab = "Alcohol initiation age",
        type = "response",
        n.grid=100,
        #color = "gray",
        #too.far = 0.02,
        contour.col = "black")#adjustcolor("black", alpha.f=0.9))
axis(side = 1, at = 1:2,
     labels = c("Female", "Male"))

vis.gam(my_gam)

# WITH GEOGRAPHY ##########
library(mgcv)
my_gam <- bam(gamble_internet ~  
                s(lon, lat, bs="ad"), 
              data = nmapss,
              family=binomial("logit"))
plot(my_gam)

vis.gam(my_gam,
        color="terrain",
        plot.type="contour",
        main = "Risk of online gambling by place",
        xlab = "Longitude",
        ylab = "Latitude",
        type = "response",
        n.grid=100,
        #color = "gray",
        #too.far = 0.02,
        contour.col = adjustcolor("black", alpha.f=0.4))
library(maps)
map("usa", add = T)
vis.gam(my_gam)
vis.gam(my_gam, view=c("lon", "lat"),
        color="terrain",
        plot.type="contour",
        main = "Risk of online gambling",
        xaxt = "n",
        xlab = "Gender",
        ylab = "Alcohol initiation age",
        type = "response",
        n.grid=500,
        #color = "gray",
        #too.far = 0.02,
        contour.col = adjustcolor("black", alpha.f=0.4))


########
# 3d scatterplot
########
library(scatterplot3d)
library(rgl)
library(car)

x <- jitter(nmapss$age, factor = 3)
z <- jitter(nmapss$alcohol_age_first, factor = 3)
y <- jitter(nmapss$alcohol_drinks_last_week, factor = 3)

scatter3d(y ~ x + z, 
          fit="smooth", #linear, smooth, additive
          xlab = "Age", ylab = "Drinks last week", zlab = "Age at first drink",
          axis.col=c("darkmagenta", "black", "darkcyan"),
          surface.col=c("blue", "green", "orange", "magenta", "cyan", "red", 
                        "yellow", "gray"), surface.alpha=0.3,
          neg.res.col="red", pos.res.col="darkgreen",
          point.col = "darkblue")





########
# SUBSET FOR FLORIDA
########
fl <- nmapss[which(nmapss$state == "FL" &
                     nmapss$lat > 27 &
                     nmapss$lon < -81.5),]

my_gam <- bam(gamble_internet ~  
                s(lon, lat, bs="ad"), 
              data = fl,
              family=binomial("logit"))
plot(my_gam)

vis.gam(my_gam,
        color="terrain",
        plot.type="contour",
        main = "Risk of online gambling by place",
        xlab = "Longitude",
        ylab = "Latitude",
        type = "response",
        n.grid=100,
        #color = "gray",
        #too.far = 0.02,
        contour.col = adjustcolor("black", alpha.f=0.4),
        add = T)
map("county","fl")

########
# MAP WHOLE COUNTRY
########
map("usa")
nmapss$col <-ifelse(nmapss$gamble_internet, "red", "blue")

x <- jitter(nmapss$lon)
y <- jitter(nmapss$lat)
points(x,
       y,
       col = adjustcolor(nmapss$col, alpha.f = 0.3),
       pch = 16,
       cex = 0.7 )
x2 <- jitter(x[which(nmapss$gamble_internet)])
y2 <- jitter(y[which(nmapss$gamble_internet)])

points(x2, y2, pch = 3, col = "red")
