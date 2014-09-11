bcol <- adjustcolor("darkgreen", alpha.f=0.4) 
ycol <- adjustcolor("darkblue", alpha.f=0.4)

x <- (1:1000)^(1/2)
y <- (1:1000)^3
plot(x,y, type = "n",
     xlab = "Time",
     ylab = "Value",
     xaxt = "n",
     yaxt = "n")

b <- rev(y)


for (i in seq(2,1000,length=20)){
  points(x[1:i],b[1:i], col = bcol, lwd = 5)
  points(x[1:i],y[1:i], col = ycol, lwd =5)
  Sys.sleep(time = 0.1)
}

legend(x="left",
       fill = c("darkgreen", "darkblue"),
       border = NA,
       legend = c("Risk of infection", "Likelihood of hesitancy"))

setwd("C:/Users/BrewJR/Documents/uf/phc6016/")
for (i in seq(2,1000,length=20)){
  png(filename = paste0("ts",i, ".png"))
  
  plot(x,y, type = "n",
       xlab = "Time",
       ylab = "Value",
       xaxt = "n",
       yaxt = "n")
  points(x[1:i],b[1:i], col = bcol, lwd = 5)
  points(x[1:i],y[1:i], col = ycol, lwd =5)
  dev.off() 
}



plot(x,y, type = "n",
     xlab = "Time",
     ylab = "Value",
     xaxt = "n",
     yaxt = "n")

lines(x,b, col = bcol, lwd = 5)
text(x[100], b[100],
     labels = "Risk of infection",
     col = bcol)


lines(x,y, col = ycol, lwd =5)
text(x[800], y[800],
     labels = "Likelihood of hesitancy",
     col = ycol)



x <- sample(1:1000, 1000)
y <- sample(1:1000, 1000)


mycol <- sample(c(rep("blue", 19), "red"), 1000, replace = TRUE)
mycol <- adjustcolor(mycol, alpha.f=0.6)

plot(x,y, col = mycol, pch = 16, cex =2,
     xaxt = "n", xlab = NA, yaxt = "n", ylab = NA)


mydata <- matrix(cbind(x,y), ncol = 2)
z <- kmeans(mydata, 25)
mydata <- data.frame(mydata)
mydata$cluster <- z$cluster

mydata$col <- ifelse(mydata$cluster == 1, "red", "blue")
mydata$col[1:40] <- "red"
mydata$col <- adjustcolor(mydata$col, alpha.f=0.6)

plot(x,y, col = mydata$col, pch = 16, cex =2,
     xaxt = "n", xlab = NA, yaxt = "n", ylab = NA)

for (i in 30:11){
  mydata <- matrix(cbind(x,y), ncol = 2)
  z <- kmeans(mydata, i)
  mydata <- data.frame(mydata)
  mydata$cluster <- z$cluster
  
  mydata$col <- ifelse(mydata$cluster == 1, "red", "blue")
  mydata$col[sample(1:nrow(mydata),i)] <- "red"
  mydata$col <- adjustcolor(mydata$col, alpha.f=0.6)
  
  png(filename = paste0("cluster",(1/i)*40, ".png"))
  
  plot(x,y, col = mydata$col, pch = 16, cex =2,
       xaxt = "n", xlab = NA, yaxt = "n", ylab = NA)
  dev.off()  
}

getwd()
setwd("C:/Users/BrewJR/Documents/uf/phc6016/")

cdc <- read.csv("pres1vaccinedata.csv")
cdc$kp <- cdc$kp * -1

FixFun <- function(x){ as.numeric(as.character(x))*-1}
cdc$mmr <- FixFun(cdc$mmr)
cdc$dtap <- FixFun(cdc$dtap)
cdc$varicella <- FixFun(cdc$varicella)

cdc$state <- gsub("[**]", "",cdc$state)
cdc$state <- tolower(cdc$state)

library(maps)
library(RColorBrewer)
library(classInt)
mymap <- map("state")
mymap$names <- sub("(.*?):.*", "\\1", mymap$names)

mymapdf <- data.frame(mymap$names)
names(mymapdf) <- "state"

library(plyr)
mymapdf <- join(x = mymapdf,
                y = cdc,
                by = "state",
                type = "left",
                match = "first")



MapFun <- function(var, color, border="grey"){
  plotvar <- var
  nclr <- 5
  plotclr <- rev(brewer.pal(nclr, color))
  class <- classIntervals(plotvar, nclr, style = "quantile", dataPrecision=0) #use "equal" instead
  #class <- classIntervals(0:100, nclr, style="equal")
  colcode <- findColours(class, plotclr)
  legcode <- paste0(gsub(",", " - ", gsub("[[]|[]]|[)]", "", names(attr(colcode, "table")))), "%")
  map("state", fill = TRUE, border=border, col=colcode)
  legend("bottomleft", # position
         legend = legcode, #names(attr(colcode, "table")), 
         fill = attr(colcode, "palette"), 
         cex = 0.85, 
         border=grey,
         bty = "n")
}

par(mfrow=c(3,1))
par(mar=c(5,2,1,1))
MapFun(var =  100 - mymapdf$mmr,
       color = "Spectral")
title(main = "MMR")

MapFun(var = 100 -  mymapdf$dtap,
       color = "Spectral")
title(main = "DTAP")

MapFun(var = 100 - mymapdf$varicella,
       color = "Spectral")
title(main = "VARICELLA")


