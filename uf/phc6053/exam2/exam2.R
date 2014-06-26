#PART 1. BEARS DATA (50 POINTS)
#1. Model 1: Estimate and interpret the effect of
# a 10-inch increase in chest girth based upon this model.
estChest <- 12.2941190
estChestLow <- 11.3410424
estChestHigh <- 13.2471957

10 * c(estChest, estChestLow, estChestHigh)

#w. Model 2: Estimate and interpret the effect of a
#10-inch increase in chest girth based upon this model
#using the following comparisons:
estChest20 <- c(20*20*0.16672872,
                20*20*0.15648808,
                20*20*0.17696936)
estChest30 <-  c(30*30*0.16672872,
                 30*30*0.15648808,
                 30*30*0.17696936)
estChest40 <-  c(40*40*0.16672872,
                 40*40*0.15648808,
                 40*40*0.17696936)
estChest50 <-  c(50*50*0.16672872,
                 50*50*0.15648808,
                 50*50*0.17696936)
#a
estChest30 - estChest20
#b
estChest40 - estChest30
#c
estChest50 - estChest40

#3. Model 3: Estimate and interpret the effects below:
estChestL <- c(260.5875000, 220.3043907, 300.8706093)
estChestM <- c(87.1875, 49.1558657, 125.2191343)
estChestS <- 0
#a. Comparing Medium (M) to Small (S)
estChestM - estChestS

#b. 
estChestL - estChestM
