#######################################################################
## Vladimir Canudas-Romo
## Demography 
## Sample program for a dynamic plot
########################################################################

## select your working directory "setwd"

setwd(Country)

## download the Age-specific fertility rates period from Human Fertility Database
##  keep its name of asfrRR.txt
## read the data

A<-read.table("USAasfrRR.txt",header=TRUE,fill=TRUE,skip=2,as.is=T)
## now put the frame for the plots

plot(c(12,55),range(A$ASFR),col=0,xlab="age",ylab="Fertility rates")
age<-c(12:55)
title("Age-specific fertility rates in USA")

## this is a loop that will include each time a new line

for (t in min(A$Year):max(A$Year)){
B<-A[A$Year==t,]
mtext(t)
lines(age,B$ASFR,col='tomato',lty=1)

Sys.sleep(.5)

lines(age,B$ASFR,col='gray',lty=1)
mtext(t,col="white")
}

## at the end you want to keep the final line
 
lines(age,B$ASFR,col='tomato',lty=1)

## this writes the last year 
mtext(t)



