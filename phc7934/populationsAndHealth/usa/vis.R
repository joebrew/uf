MyFun <- function(country){
  
  
  #######
  # SET WORKING DIRECTORY 
  #######
  setwd(paste0("C:/Users/BrewJR/Documents/uf/phc7934/populationsAndHealth/", country))
  
  
  #######
  # READ IN DEATHS, BIRTHS AND POPULATION
  #######
  d <- read.table("deaths.txt",header=TRUE,skip=2)
  b <- read.table("births.txt",header=TRUE,skip=2)
  p <- read.table("population.txt",header=TRUE,skip=2)
  
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
  
  # (x is now a dataframe with b = births, d = deaths, p = population)
  
  # plot(x$y, x$b, ylim=c(0,5000000))
  #lines(x$y, x$d)
  barplot(x$b)
  barplot(x$d, col = "red", add = T)
  
  
}


MyFun("japan")
#######
# SET WORKING DIRECTORY 
#######
setwd("C:/Users/BrewJR/Documents/uf/phc7934/populationsAndHealth/usa")


#######
# READ IN DEATHS, BIRTHS AND POPULATION
#######
d <- read.table("deaths.txt",header=TRUE,skip=2)
b <- read.table("births.txt",header=TRUE,skip=2)
p <- read.table("population.txt",header=TRUE,skip=2)

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

# (x is now a dataframe with b = births, d = deaths, p = population)

# plot(x$y, x$b, ylim=c(0,5000000))
#lines(x$y, x$d)
barplot(x$b)
barplot(x$d, col = "red", add = T)

#######
#
#######




#######
#
#######


