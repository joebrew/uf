#####
# INSTALL AND ATTACH THE foreach AND doMC PACKAGES
#####
library(foreach)
library(doMC)
registerDoMC()

# Using Multiple cores
nr <- 1000
nc <- 1000
x <- matrix(NA, nrow = nr, ncol = nc)
system.time(
  foreach(i = 1:nr) %dopar% {
    temp <- rnorm(nc)
    temp <- temp[sample(1:length(temp), round(0.63*length(temp)), replace = TRUE)]
    temp <- sample(temp, nc, replace = TRUE)
    x[i,] <- temp
  }
)

# # NON MULTICORE APPROACH
# nr <- 1000
# nc <- 1000
# x <- matrix(NA, nrow = nr, ncol = nc)
# system.time(
# for (i in 1:nr){
#   temp <- rnorm(nc)
#   temp <- temp[sample(1:length(temp), round(0.63*length(temp)), replace = TRUE)]
#   temp <- sample(temp, nc, replace = TRUE)
#   x[i,] <- temp
# }
# )
