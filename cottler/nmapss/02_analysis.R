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
load(paste0(private, "/nmapss.RData"))

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
                     ylim = c(0, max(my_table) * 1.1))
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
                     ylim = c(0, max(my_table) * 1.1))

text(x = my_barplot[,1],
     y = my_table,
     pos = 1,
     labels = my_table)
text(x = my_barplot[,1],
     y = my_table,
     pos = 3,
     labels = paste0(round(my_prop_table*100, digits =2), "%"))

# Cross tabulation
x <- ifelse(nmapss$alcohol_ever, "ever drink", "never drink")
y <- ifelse(nmapss$gamble_internet, "ever gamble", "never gamble")
my_table <- table(x,y)
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
# 2. View relationship after adjustment
########

# Is this significant?
fit <- glm(gamble_internet ~ 
             alcohol_ever + gender + factor(age),
           data = nmapss,
           family = binomial("logit"))
summary(fit)

# Odds ratios
exp(coef(fit))

## odds ratios and 95% CI
x <- exp(cbind(OR = coef(fit), confint(fit)))
x
x <- data.frame(x[-1,])
barplot(x$OR)


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
girls <- girls[-1]
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
             alcohol_age_first + gender,
           data = over7,
           family = binomial("logit"))
summary(fit)

# Odds ratios
exp(coef(fit))

## odds ratios and 95% CI
exp(cbind(OR = coef(fit), confint(fit)))




########
#
########




########
#
########




########
#
########




########
#
########



