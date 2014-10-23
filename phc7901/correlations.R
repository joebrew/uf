dat <- read.csv("obesity_flu_absences_merged.csv")
dat <- dat[which(dat$age_months <= 160),]
dat <- dat[which(dat$weight <= 300),]
dat <- dat[which(dat$height <= 75),]
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
