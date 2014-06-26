N <- 600
cases <- 300
controls <- 300
incExp <- 190/300
incNonExp <- 90/300

lab <- c("case", "control")
effect <- c(190, 90)
noEffect <- c(300-190, 300-90)

myTable <- as.data.frame(rbind(effect, noEffect))
colnames(myTable) <- c("case", "control")
myTable$odds <- myTable$case/myTable$control
oddsRatio <- myTable$odds[1]/myTable$odds[2]


#number 2
lab <- c("case", "control")
effect <- c(10, 6)
noEffect <- c(196, 386-196)

myTable <- as.data.frame(rbind(effect, noEffect))
colnames(myTable) <- c("case", "control")
myTable$odds <- myTable$case/myTable$control
myTable$risk <- myTable$case/(myTable$control+myTable$case)
riskRatio <- myTable$risk[1]/myTable$risk[2]
oddsRatio <- myTable$odds[1]/myTable$odds[2]

#### NUMBER 3

N <- 820
flu <- 310
noflu <- 820-310
nofluvac <- 370
novac <- 235
vac <- 820 - novac
fluvac <- 


