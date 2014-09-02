wd <- "C:/Users/BrewJR/Documents/uf/phc7934/populationsAndHealth/allCountries"
setwd(wd)

# GET COUNTRY ABBREVIATIONS
setwd("./births")
df <- data.frame(gsub(".Births.txt", "", dir()))
names(df) <- "ab"

setwd(wd)
# GET COUNTRY NAMES
df$countries <- c("Australia",
               "Austria",
               "Belarus",
               "Belgium",
               "Bulgaria",
               "Canada",
               "Czech Republic",
               "Chile",
               "Denmark",
               "Estonia",
               "Finland",
               "France",
               "Germany",
               "Hungary",
               "Iceland",
               "Ireland",
               "Israel",
               "Italy",
               "Japan",
               "Latvia",
               "Lithuania",
               "Luxembourg",
               "Netherlands",
               "New Zealand",
               "Norway",
               "Poland",
               "Portugal",
               "Russia",
               "Slovakia",
               "Slovenia",
               "Spain",
               "Sweden",
               "Switzerland",
               "Taiwan",
               "U.K.",
               "U.S.A.",
               "Ukraine")

country <- "USA"
MortFun <- function(country, bar = TRUE, line = TRUE){
  setwd(wd)
  
  
  setwd("./births")
  b <- read.table(paste0(country, ".Births.txt"),header=TRUE,skip=2)
  
  setwd(wd)
  setwd("./deaths")
  d <- read.table(paste0(country, ".Deaths_1x1.txt"),header=TRUE,skip=2)
  
  setwd(wd)
  setwd("./population")
  p <- read.table(paste0(country, ".Population.txt"),header=TRUE,skip=2)
  
  setwd(wd)
  
  #######
  # SINCE B IS ON A NICE 1 YEAR = 1 ROW SYSTEM, ADD TO IT
  #######
  # populations
  b$p <- NA
  for (i in b$Year){
    b$p[which(b$Year == i)] <-
      sum(p$Total[which(p$Year == i)])
  }
  
  b$d <- NA
  for (i in b$Year){
    b$d[which(b$Year == i)] <-
      sum(d$Total[which(d$Year == i)])
  }
  
  #######
  # ADD A B VARIABLE (births) and a Y VARIABLE (year)
  #######
  b$b <- b$Total
  b$y <- b$Year
  
  #######
  # NAME THE DATAFRAME X
  #######
  x <- b
  
  # MAKE BIRTH RATE AND DEATH RATE PER 1000
  x$br <- x$b / x$p * 1000
  x$dr <- x$d / x$p * 1000
  
  #############
  # PLOT
  #############
  if(bar){
    barplot(x$b, border = FALSE,
            names.arg = x$y,
            las = 3,
            cex.names = 0.7)
    barplot(x$d, border = NA, col = adjustcolor("red", alpha.f=0.4), add = T)
    
    legend (x = "topright",
            fill = c("grey", adjustcolor("red", alpha.f = 0.4)),
            border = FALSE,
            bty = "n",
            legend = c("Births", "Deaths"),
            title = "Events per year",
            cex = 0.7)
  }else if(line){
    plot(x$y, x$br, ylim = c(0,max(x$br[which(is.finite(x$br))], na.rm=T)), 
         type = "l", col = "grey",
         ylab = "Year", 
         xlab = "Rate per 1,000")
    lines(x$y, x$dr, col = "red")
    legend(x = "bottomleft",
           lty = 1,
           col = c("grey", "red"),
           legend = c("Birth", "Death"),
           title = "Rate per 1,000",
           cex = 0.8)
    

  }

}

MortFun("FRACTP", bar = FALSE, line = TRUE)
MortFun("EST")
# 
# # MAKE A FUNCTION TO SHOW DEATH RATE BY AGE BY YEAR
# AgeFun <- function(year){
#   barplot(d$Total[which(d$Year == year)] / 
#             )
# }
# AgeFun(1935)
# 
# for (i in 1933:2000){
#   AgeFun(i)
#   Sys.sleep(0.2)
# }
