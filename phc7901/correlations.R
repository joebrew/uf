setwd("C:/Users/BrewJR/Desktop")

# read in alachua data
dat <- read.csv("obesity_flu_absences_merged.csv")

# clean up the crazies
dat <- dat[which(dat$age_months <= 160),]
dat <- dat[which(dat$weight <= 300),]
dat <- dat[which(dat$height <= 75),]

# 3d scatterplot with smoothed plane
library(scatterplot3d)
library(rgl)
library(car)
scatter3d(dat$weight ~ dat$height + dat$age_months, 
          fit="smooth", #linear, smooth, additive
          ylab="Weight", xlab="Height", zlab="Age",
          axis.col=c("darkmagenta", "black", "darkcyan"),
          surface.col=c("blue", "green", "orange", "magenta", "cyan", "red", 
                        "yellow", "gray"), surface.alpha=0.3,
          neg.res.col="red", pos.res.col="darkgreen",
          point.col = "darkblue")

# scatter3d(dat$weight ~ dat$height + dat$age_months, 
#           fit="smooth", #linear, smooth, additive
#           ylab="Weight", xlab="Height", zlab="Age",
#           axis.col=c("darkmagenta", "black", "darkcyan"),
#           surface.col=c("blue", "green", "orange", "magenta", "cyan", "red", 
#                         "yellow", "gray"), surface.alpha=0.3,
#           neg.res.col="red", pos.res.col="darkgreen",
#           point.col = "grey")



# Height and weight
plot(dat$height, dat$weight,
     col = adjustcolor("blue", alpha.f = 0.1),
     pch = 16,
     xlab = "Height (inches)",
     ylab = "Weight (pounds)",
     ylim = c(0,300),
     xlim = c(0,80),
     cex = 0.8)
# points(dat$height, dat$weight,
#        col = adjustcolor("black", alpha.f = 0.2))
abline(lm(weight ~ height, data = dat),
       col = adjustcolor("darkgreen", alpha.f = 0.6),
       lwd = 2)
lines(lowess(dat$height, dat$weight, f = 1/10),
      col = adjustcolor("darkred", alpha.f = 0.5),
      lwd = 2)
legend("topleft",
       col = adjustcolor(c("darkgreen", "darkred"),
                         alpha.f = 0.6),
       lwd = 2,
       legend = c("Least squares", "Lowess"),
       bty = "n")

# High density scatterplot
library(hexbin)
HexFun <- function(x, y, xlab = NA, ylab = NA, main = NA){
  bin<-hexbin(x, y, xbins=50) 
  plot(bin, main=main,
       xlab = xlab,
       ylab = ylab)
}

HexFun(x = dat$height,
       y = dat$weight,
       main = "Hexagonal binning of height and weight",
       xlab = "Height",
       ylab = "Weight")
HexFun(x = dat$age_months / 12,
       y = dat$weight,
       main = "Hexagonal binning of age and weight",
       xlab = "Age (years)",
       ylab = "Weight")
HexFun(x = dat$age_months / 12,
       y = dat$height,
       main = "Hexagonal binning of age and height",
       xlab = "Age (years)",
       ylab = "Height")



x <- dat$height
y <- dat$weight
bin<-hexbin(x, y, xbins=50) 
plot(bin, main="Hexagonal Binning of height and weight",
     xlab = "Height",
     ylab = "Weight")

# Age and weight
library(dplyr)
x <- dat %>%
  group_by(age_months) %>%
  summarise(y = mean(weight))

plot(x$age_months, x$y)

plot(dat$age_months, dat$weight,
     col = adjustcolor("blue", alpha.f = 0.1),
     pch = 16,
     xlab = "Age (months)",
     ylab = "Weight (pounds)",
     cex = 0.8)
abline(lm(weight ~ age_months, data = dat),
       col = adjustcolor("darkgreen", alpha.f = 0.6),
       lwd = 2)
lines(lowess(dat$age_months, dat$weight, f = 1/10),
      col = adjustcolor("darkred", alpha.f = 0.5),
      lwd = 2)
legend("topleft",
       col = adjustcolor(c("darkgreen", "darkred"),
                         alpha.f = 0.6),
       lwd = 2,
       legend = c("Least squares", "Lowess"),
       bty = "n")


# Age and height

plot(dat$age_months, dat$height,
     col = adjustcolor("blue", alpha.f = 0.1),
     pch = 16,
     xlab = "Age (months)",
     ylab = "Height (inches)",
     cex = 0.8)
abline(lm(height ~ age_months, data = dat),
       col = adjustcolor("darkgreen", alpha.f = 0.6),
       lwd = 2)
lines(lowess(dat$age_months, dat$height, f = 1/10),
      col = adjustcolor("darkred", alpha.f = 0.5),
      lwd = 2)
legend("topleft",
       col = adjustcolor(c("darkgreen", "darkred"),
                         alpha.f = 0.6),
       lwd = 2,
       legend = c("Least squares", "Lowess"),
       bty = "n")

# Age and BMI
plot(dat$age_months, dat$z)


library(scatterplot3d)
library(rgl)
library(car)
scatter3d(dat$weight ~ dat$height + dat$age_months, 
          fit="smooth", #linear, smooth, additive
          ylab="Weight", xlab="Height", zlab="Age",
          col = "red")
