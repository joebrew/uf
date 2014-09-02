# http://www.ncbi.nlm.nih.gov/pmc/articles/PMC1052246/?page=5

# FAKE DATA FRAME
df <- data.frame(1:10000)
names(df) <- "id"

# RANDOMLY ASSIGN ORIGINS
df$orig <- factor(sample(x = c("Kenyan", "British"),
                         size = length(df$id),
                         replace = TRUE))
levels(df$orig) <- c("Kenyan", "British")
######
# GIVE SYSTOLIC B.P.
######

# POPULATE EMPTY VECTOR
df$sbp <- NA

# ASSIGN TO KENYANS
df$sbp[which(df$orig == "Kenyan")] <- 
  rnorm(n = length(df$id[which(df$orig == "Kenyan")]),
        mean = 116,
        sd = 8)

# ASSIGN TO BRITS
df$sbp[which(df$orig == "British")] <- 
  rnorm(n = length(df$id[which(df$orig == "British")]),
        mean = 136,
        sd = 9.3)

#######
# ASSIGN URBAN VS. RURAL
#######
df$geo <- NA
df$geo <- factor(sample (x = c("Urban", "Rural"),
                         size = nrow(df),
                         replace = TRUE))

#######
# ASSIGN MALE FEMALE
#######
df$sex <- NA
df$sex <- factor(sample (x = c("Male", "Female"),
                         size = nrow(df),
                         replace = TRUE))

#######
# ADD POINTS FOR URBAN PEOPLE
# SUBTRACT FOR RURAL
########
urbanAdd <- 3.5

df$sbp[which(df$geo == "Rural")] <-
  df$sbp[which(df$geo == "Rural")] -
  (0.5 * urbanAdd)

df$sbp[which(df$geo == "Urban")] <-
  df$sbp[which(df$geo == "Urban")] +
  (0.5 * urbanAdd)

#######
# ADD POINTS FOR MALES 
# SUBTRACT FOR FEMALES
########
maleAdd <- 0.8

df$sbp[which(df$sex == "Female")] <-
  df$sbp[which(df$sex == "Female")] -
  (0.5 * maleAdd)


df$sbp[which(df$sex == "Male")] <-
  df$sbp[which(df$sex == "Male")] +
  (0.5 * maleAdd)

########
# SUBSET INTO TWO DATAFRAMES
# K = kenyans
# B = brits
########

k <- df[which(df$orig == "Kenyan"),]
b <- df[which(df$orig == "British"),]

# hist(k$sbp,
#      xlab = "Systolic Blood Pressure",
#      col =adjustcolor("darkred", alpha.f=0.6),
#      border = adjustcolor("black", alpha.f=0.3),
#      breaks = 30,
#      xlim = c(60, 200),
#      main = "SBP among both groups")
# hist(b$sbp,
#      xlab = NA,
#      col =adjustcolor("darkblue", alpha.f=0.6),
#      border = adjustcolor("black", alpha.f=0.3),
#      breaks = 30,
#      add = TRUE)
# 
# hist(b$sbp,
#      xlab = "Systolic Blood Pressure",
#      col =adjustcolor("darkblue", alpha.f=0.6),
#      border = adjustcolor("black", alpha.f=0.3),
#      breaks = 30,
#      xlim = c(60, 200),
#      main = "SBP among British civil servants")
# 
# legend(x= "topright",
#        fill = adjustcolor(c("darkblue", "darkred"), alpha.f=0.6),
#        border = adjustcolor("black", alpha.f = 0.3),
#        legend = c("British civil servants",
#                   "Kenyan nomads"),
#        bty = "n",
#        cex = 0.6)

#########
# LINEAR MODELS
#########

#### FUNCTION FOR PLOTTING OUTCOME OF LINEAR MODELS
library(Hmisc)
BarFun <- function(model, err = FALSE,
                   col = "darkred"){
  x <- cbind(OR = coef(model), confint(model))
  bp <- barplot(x[-1,1],
          names.arg = gsub(paste(colnames(df), collapse = "|"), 
                           "",  row.names(x)[-1]),
          border = adjustcolor("black", alpha.f=0.3),
          ylab = "Increase in SBP",
          ylim = c(0, max(x[-1,1])*1.2),
          col = adjustcolor(col, alpha.f=0.6))
  if(err){
  errbar(x = bp[,1], 
         y = x[-1,1],
         yplus = x[-1, 3],
         yminus = x[-1,2],
         errbar.col = "darkred",
         add = TRUE,
         pch = NA)
  }

}


# FIT SIMPLE
fitSimple <- lm(sbp ~  geo + sex, data = df)

# FIT FULL
fitFull <- lm(sbp ~  orig + geo + sex, data = df)

# FIT BRIT
fitBrit <- lm(sbp ~ geo + sex, data = b)

# FIT KENYAN
fitKenyan <- lm(sbp ~ geo + sex, data = k)


# BarFun(fitSimple, err = TRUE)
# BarFun(fitFull, err = TRUE, col = "darkgreen")
# title(main = "Regression output: predictors of SBP among both populations", cex.main = 1)
# 
# BarFun(fitBrit, err = TRUE, col = "darkred")
# BarFun(fitKenyan, err = TRUE, col = "darkred")
# 
# title(main = "Regression output: predictors of SBP among Kenyan nomads", cex.main = 1)

RoseFun <- function(british, kenyan){
  
  par(mar=c(4,4,1,1))
  par(oma=c(0,0,0,0))
  
  if(british & !kenyan){
    par(mfrow=c(2,1))
    BarFun(fitBrit, err = TRUE, col = "darkblue")
    title(main = "Regression output")
    hist(b$sbp,
         xlab = "Systolic Blood Pressure",
         col =adjustcolor("darkblue", alpha.f=0.6),
         border = adjustcolor("black", alpha.f=0.3),
         breaks = 30,
         xlim = c(60, 200),
         main = "SBP among British civil servants")
    
  } else if(!british & kenyan){
    par(mfrow=c(2,1))
    
    BarFun(fitKenyan, err = TRUE, col = "darkred")
    title(main = "Regression output")
    
    hist(k$sbp,
         xlab = "Systolic Blood Pressure",
         col =adjustcolor("darkred", alpha.f=0.6),
         border = adjustcolor("black", alpha.f=0.3),
         breaks = 30,
         xlim = c(60, 200),
         main = "SBP among Kenyan nomads")
    
  } else if(!british & !kenyan){
    
    barplot(1:10, col = "white", xaxt = "n", yaxt= "n", border = FALSE)
    legend(x="center", fill=NA, 
           legend = "Please select a population", border = FALSE, bty = "n", cex =2)
    
  } else if(british &  kenyan){
    par(mfrow=c(2,1))
    
    
    BarFun(fitFull, err = TRUE, col = "darkgreen")
    title(main = "Regression output")
    
    hist(k$sbp,
         xlab = "Systolic Blood Pressure",
         col =adjustcolor("darkred", alpha.f=0.6),
         border = adjustcolor("black", alpha.f=0.3),
         breaks = 30,
         xlim = c(60, 200),
         main = "SBP among both groups")
    hist(b$sbp,
         xlab = NA,
         col =adjustcolor("darkblue", alpha.f=0.6),
         border = adjustcolor("black", alpha.f=0.3),
         breaks = 30,
         add = TRUE)
    legend(x= "topright",
           fill = adjustcolor(c("darkblue", "darkred"), alpha.f=0.6),
           border = adjustcolor("black", alpha.f = 0.3),
           legend = c("British civil servants",
                      "Kenyan nomads"),
           bty = "n",
           cex = 0.6)
    
    
  }
}