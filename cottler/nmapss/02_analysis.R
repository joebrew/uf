library(sas7bdat)
library(car)
library(dplyr)
library(ggmap)
library(geocodeHERE)
library(Hmisc)

########
# ESTABLISH WD FOR PRIVATE DATA
########
if ( Sys.info()["sysname"] == "Linux" ){
  public <- "/home/joebrew/Documents/uf/cottler/nmapss"
  private <- "/media/joebrew/JB/uf/cottler/nmapss"
} else if(Sys.info()["user"] == "BrewJR" ){
  public <- "C:/Users/BrewJR/Documents/uf/cottler/nmapss"
  private <- "E:/uf/cottler/nmapss"
} else {
  return("Use other system.")
}
setwd(public)

########
# Read in cleaned data
########
load(paste0(private, "/nmapss_geocoded.RData"))

#####
# SOURCE HELPER FUNCTIONS
#####
source("00_helpers.R")

#####
# ADD MORE VARIABLES
#####
nmapss$health <- factor(nmapss$health,
                        levels = c("Excellent", "Good", "Fair", "Poor"))
nmapss$location <- factor(nmapss$location,
                          levels = c("urban", "suburban", "rural"))
nmapss$family_dinner_week <- factor(ifelse(nmapss$family_dinner_week <=3,"<=3 weekly family meals",
                                           ifelse(nmapss$family_dinner_week >=4, ">= 4 weekly family meals",
                                                  NA)))

#####
# GET EVER CONSUMED ALCOHOL BY AGE AND SEX
#####
basic <- nmapss %>%
  group_by(gender, age) %>%
  summarise(alcohol = length(alcohol_ever[which(alcohol_ever)]),
            no_alcohol = length(alcohol_ever[which(!alcohol_ever)]),
            gamble = length(gamble_internet[which(gamble_internet)]),
            no_gamble = length(gamble_internet[which(!gamble_internet)]),
            total = n())
for (i in 3:6){
  new_name <- paste0("p_", colnames(basic)[i])
  basic[,new_name] <- basic[,i] / basic$total
}

#
age_plot <- function(gender = "female",
                    var = "p_alcohol",
                    add = FALSE,
                    col = "red",
                    ylim = c(0, 0.8),
                    ylab = "Probability of having consumed alcohol"){
  
  # Subset df
  df <- basic[which(basic$gender == gender),]
  
  # Get just values
  vals <- as.numeric(unlist(df[,var]))
  
  # Get just ages
  ages <- as.numeric(unlist(df[,"age"]))
  
  # Define offset
  h_offset <-  (as.numeric(gender == "female")* 0.2)
  
  # Get confidence interval on proportion
  ci <- simpasym(n = as.numeric(unlist(df$total)),
           p = vals)

  # Plot
  if(!add){
    plot(ages -0.1 + h_offset, vals, col = adjustcolor(col, alpha.f = 0.5),
         ylim = ylim,
         xlab = "Age",
         ylab = ylab,
         pch = 16,
         xaxt = "n")
  } else{
    points(ages -0.1 + h_offset, vals, col = adjustcolor(col, alpha.f = 0.5),
         ylim = c(0, 0.8),
         pch = 16)
  }
  lines(ages -0.1 + h_offset, vals, col = adjustcolor(col, alpha.f = 0.3), lty = 2)
  
  # add axis
  axis(side = 1, at = seq(1,20,1), labels = seq(1,20,1), tick = FALSE)

  errbar(x = ages -0.1 + h_offset,
         y = vals,
         yminus = ci$lb,
         yplus = ci$ub,
         pch = NA,
         add = TRUE,
         errbar.col = adjustcolor(col, alpha.f = 0.8))

  # Add a box around the plot
  box("plot")
}

# PLOT ALCOHOL
age_plot()
age_plot(gender = "male", add = TRUE, col = "blue")
abline(v = seq(0,20,1),
       col = adjustcolor("grey", alpha.f = 0.3),
       lwd = 35)

# PLOT GAMBLING
age_plot(var = "p_gamble", ylim = c(0, 0.17), ylab = "Probability of having ever gambled online")
age_plot(var = "p_gamble", add = TRUE, gender = "male", col = "blue")
abline(v = seq(0,20,1),
       col = adjustcolor("grey", alpha.f = 0.3),
       lwd = 35)
abline(h = seq(0,1, 0.05),
       col = adjustcolor("grey", alpha.f = 0.2))

# legend
legend(x = "topleft",
       lty = 2,
       pch = 16,
       col = adjustcolor(c("blue", "red"), alpha.f = 0.3),
       legend = c("Males", "Females"))

########
# EXPLORE RELATIONSHIP BETWEEN ALCOHOL AND GAMBLING
########

train_data <- nmapss[which(nmapss$alcohol_age_first >=
                             mean(nmapss$alcohol_age_first, na.rm = TRUE) - 
                             (2 * sd(nmapss$alcohol_age_first, na.rm = T))),]

# Model
fit <- glm(gamble_internet ~ 
              alcohol_age_first, #* gender,
           data = train_data,
           family = binomial("logit"))
summary(fit)

# Odds ratios
exp(coef(fit))

## odds ratios and 95% CI
exp(cbind(OR = coef(fit), confint(fit)))

# Get all possible prediction categories
test <- expand.grid(#age_group = levels(factor(train_data$age_group)),
                    alcohol_age_first = unique(train_data$alcohol_age_first))#,
                    #gender = unique(train_data$gender))
#test <- test[complete.cases(test),]

# Get predicted 
prediction <- predict(fit, test, type = "response", na.action = na.pass, se.fit = TRUE)
test$predicted <- as.numeric(prediction$fit)
test$lwr <- prediction$fit - (1.96 * prediction$se.fit)
test$upr <- prediction$fit + (1.96 * prediction$se.fit)

# Order test by age_group
test <- test[order(test$alcohol_age_first),]

reg_fun <- function(gender = "female",
                    col = "red",
                    var = "alcohol_age",
                    ylab = "Probability of having gambled on internet",
                    xlab = "Age",
                    ylim = c(0, 0.4),
                    add = FALSE){
  #Subset and order
  #df <- test[which(test$gender == gender),]
  df <- test
  df <- df[order(df$alcohol_age_first),]
  # Get ages
  ages <- df$alcohol_age
  

  
  # Define offset
  h_offset <-  (as.numeric(gender == "female")* 0.2)

 if(!add){
    plot(ages -0.1 + h_offset, df$predicted,
         pch = 16, 
         col = adjustcolor(col, alpha.f = 0.4),
         ylim = ylim,
         xlab = "Age",
         ylab = ylab,
         xaxt = "n")
  } else{
    points(ages -0.1 + h_offset, df$predicted,
         pch = 16, 
         col = adjustcolor(col, alpha.f = 0.4))
  }
  lines(ages -0.1 + h_offset, df$predicted,
        lty = 2,
        col = adjustcolor(col, alpha.f = 0.2))
  
  # add axis
  axis(side = 1, at = seq(0,20, 1),
       labels = seq(0, 20, 1),
       tick = FALSE)

  errbar(x = ages -0.1 + h_offset,
         y = df$predicted,
         yminus = df$lwr,
         yplus = df$upr,
         add = TRUE,
         errbar.col = adjustcolor(col, alpha.f = 0.8),
         pch = NA)
  
  # Add a box around the plot
  box("plot")
  
  print(df)
}
reg_fun(ylim = c(0, 0.23), col = "darkgreen")
#reg_fun(gender = "male", col = "blue", add = TRUE)
abline(v = seq(0,20,1),
       col = adjustcolor("grey", alpha.f = 0.3),
       lwd = 35)


abline(h = seq(0,1, 0.05),
       col = adjustcolor("grey", alpha.f = 0.2))

# legend
legend(x = "topright",
       lty = 2,
       pch = 16,
       col = adjustcolor(c("blue", "red"), alpha.f = 0.3),
       legend = c("Males", "Females"))


###################
# ADD MORE VARIABLES TO MODEL
###################
train_data <- nmapss[which(nmapss$alcohol_age_first >=
                             mean(nmapss$alcohol_age_first, na.rm = TRUE) - 
                             (2 * sd(nmapss$alcohol_age_first, na.rm = T))),]
train_data <-train_data[,c("gamble_internet",
                                       "alcohol_age_first",
                                       "gender",
                                       "location", 
                                       "health",
                                       "family_dinner_week",
                                       "lived_with_mom_and_dad",
                                      "race")]
train_data <- na.omit(train_data)

# Model
fit <- glm(gamble_internet ~ 
             alcohol_age_first * gender + location + health + family_dinner_week + lived_with_mom_and_dad + race,
           data = train_data,
           family = binomial("logit"))
summary(fit)

# Odds ratios
exp(coef(fit))

## odds ratios and 95% CI
exp(cbind(OR = coef(fit), confint(fit)))

library(MASS)
step <- stepAIC(fit, direction = "both")
step$anova

# Fit final
fit_final <- glm(gamble_internet ~ alcohol_age_first + gender + family_dinner_week + 
                   alcohol_age_first:gender,
                 data = na.omit(train_data),
                 family = binomial("logit"))

# Odds ratios
exp(coef(fit_final))

## odds ratios and 95% CI
exp(cbind(OR = coef(fit_final), confint(fit_final)))


# Get all possible prediction categories
test <- expand.grid(#age_group = levels(factor(train_data$age_group)),
  alcohol_age_first = unique(train_data$alcohol_age_first),
  gender = unique(train_data$gender),
  family_dinner_week = unique(train_data$family_dinner_week))
test <- test[complete.cases(test),]

# Get predicted 
prediction <- predict(fit_final, test, type = "response", na.action = na.pass, se.fit = TRUE)
test$predicted <- as.numeric(prediction$fit)
test$lwr <- prediction$fit - (1.96 * prediction$se.fit)
test$upr <- prediction$fit + (1.96 * prediction$se.fit)

# Order test by age_group
test <- test[order(test$alcohol_age_first),]

reg_fun <- function(gender = "female",
                    col = "red",
                    family_dinner_week = "<=3 weekly family meals",
                    var = "alcohol_age",
                    ylab = "Probability of having gambled on internet",
                    xlab = "Age",
                    ylim = c(0, 0.4),
                    add = FALSE,
                    lty = 1,
                    pch = 16){
  
  #Subset and order
  df <- test[which(test$gender == gender &
                     test$family_dinner_week == family_dinner_week),]
  df <- df[order(df$alcohol_age_first),]
  # Get ages
  ages <- df$alcohol_age
  
  
  
  # Define offset
  h_offset <-  (as.numeric(gender == "female")* 0.2) +
    (as.numeric(family_dinner_week == ">= 4 weekly family meals")* 0.1)
  
  if(!add){
    plot(ages -0.1 + h_offset, df$predicted,
         pch = pch, 
         col = adjustcolor(col, alpha.f = 0.4),
         ylim = ylim,
         xlab = "Age",
         ylab = ylab,
         xaxt = "n")
  } else{
    points(ages -0.1 + h_offset, df$predicted,
           pch = pch, 
           col = adjustcolor(col, alpha.f = 0.4))
  }
  lines(ages -0.1 + h_offset, df$predicted,
        lty = lty,
        col = adjustcolor(col, alpha.f = 0.2))
  
  # add axis
  axis(side = 1, at = seq(0,20, 1),
       labels = seq(0, 20, 1),
       tick = FALSE)
  
  errbar(x = ages -0.1 + h_offset,
         y = df$predicted,
         yminus = df$lwr,
         yplus = df$upr,
         add = TRUE,
         errbar.col = adjustcolor(col, alpha.f = 0.8),
         pch = NA, 
         lty = lty)
  
  # Add a box around the plot
  box("plot")
  
  print(df)
}
reg_fun(ylim = c(0, 0.23), lty = 1)
reg_fun(gender = "male", col = "blue", add = TRUE, lty = 1)
reg_fun(family_dinner_week = ">= 4 weekly family meals", add = TRUE, pch = 17, lty = 3)
reg_fun(gender = "male", col = "blue", family_dinner_week = ">= 4 weekly family meals", 
        add = TRUE, pch = 17, lty =3)

abline(v = seq(0,20,1),
       col = adjustcolor("grey", alpha.f = 0.3),
       lwd = 55)
abline(h = seq(0,1, 0.05),
       col = adjustcolor("grey", alpha.f = 0.2))

# legend
legend(x = "topright",
       lty = c(1, 3, 1 ,3),
       pch = c(16, 17, 16, 17),
       col = adjustcolor(c("blue", "blue", "red", "red"), alpha.f = 0.3),
       legend = c("Males (regular family meals)",
                  "Males (other)",
                  "Females (regular family meals)",
                  "Females (other"))




######################

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
my_data <- nmapss[,c("gamble_internet",
                     "alcohol_ever",
                     "gender",
                     "age",
                     #"race",
                     "lived_with_mom_and_dad",
                     "close_friends",
                     "marijuana_ever",
                     "grades")]
fit <- glm(gamble_internet ~ 
<<<<<<< HEAD
             alcohol_ever + 
             gender + 
             age + 
             #race + 
             lived_with_mom_and_dad + 
             close_friends +
             grades +
             marijuana_ever, 
           data = na.omit(my_data),
=======
             alcohol_ever + gender + age_group, #factor(age),
           data = nmapss,
>>>>>>> 38a153db4d0f2647c555e86f31ff28d9d434ecc9
           family = binomial("logit"))
summary(fit)

# Odds ratios
exp(coef(fit))

## odds ratios and 95% CI
exp(cbind(OR = coef(fit), confint(fit)))


# Variable selection
library(MASS)
step <- stepAIC(fit, direction = "both")
step$anova

# Fit final
fit_final <- glm(gamble_internet ~ alcohol_ever + gender + age + close_friends + 
                   grades,
                 data = na.omit(my_data),
                 family = binomial("logit"))

## odds ratios and 95% CI
x <- exp(cbind(OR = coef(fit_final), confint(fit_final)))
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
