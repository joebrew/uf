#READ IN THE DATA FROM ASSIGNMENT 1;


setwd("C:/Users/BrewJR/Desktop/ass8")

library(sas7bdat)
library(foreign)

dat <- read.ssd(
  "C:/Users/BrewJR/Desktop/ass8", 
  "fghm122", 
  sascmd="C:/Program Files/SASHome93/SASFoundation/9.3/sas.exe")
  )

# PROBLEM 1. CREATE A NEW DATASET WHICH */
  # -(a) Contains all of the original data plus the categorized version of BMI previously created */
  # -(b) Creates a new variables called HBP (high blood pressure) which uses the groups defined above */
  # -(c) Removes all individuals in the underweight BMI group AND all individuals with a BMI which is 40 or larger */
  
  
  # 1(a) CREATE A BMIGROUP VARIABLE*/
dat$BMIGROUP <- ifelse(dat$BMI<18.5,
                       1,
                       ifelse(dat$BMI >= 18.5 & dat$BMI < 25,
                              2,
                              ifelse(dat$BMI >= 25 & dat$BMI < 30,
                                     3,
                                     ifelse(dat$BMI >= 30, 
                                            4,
                                            NA))))

# 1(b) CREATE HBP */
dat$HBP <- ifelse(dat$SYSBP >=0 & dat$SYSBP < 140,
                  0,
                  ifelse(dat$SYSBP >= 140,
                         1,
                         NA))


# 1(c) Remove all the individuals in the underweight BMI group AND 
#all the individuals with a BMI which is 40 or larger*/
dat2 <-dat[which(dat$BMIGROUP != 1 & 
                   dat$BMI <=40),]

# DONE WITH PROBLEM 1 */
  #%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%*/
  
  # PROBLEM 2: UNADJUSTED ODDS-RATIOS PREDICTING THE OUTCOME HBP = YES */
  # Using HBP as the outcome variable, predicting the probability that HBP = Yes, run simple logistic regression models
  #to obtain unadjusted odds ratios using the predictors:
  #AGE
#BMI
#BMIGROUP (ref = normal)
#SEX (ref = male)
#BPMEDS (ref = no)
#PREVSTRK (ref = no)
#For each model (in the order just listed), provide only the table of parameter estimates*/
  
  # AGE */
modelAGE <- glm(dat2$HBP ~ dat2$AGE, family="binomial")
exp(cbind(OR = coef(modelAGE), confint(modelAGE)))

# BMI */
modelBMI <- glm(dat2$HBP ~ dat2$BMI, family="binomial")
exp(cbind(OR = coef(modelBMI), confint(modelBMI)))

# BMIGROUP */
dat2$BMIGROUP <- factor(dat2$BMIGROUP, levels=c(2,3,4))
modelBMIGROUP <- glm(dat2$HBP ~ dat2$BMIGROUP, family="binomial")
exp(cbind(OR = coef(modelBMIGROUP), confint(modelBMIGROUP)))

# SEX */
modelSEX <- glm(dat2$HBP ~ dat2$SEX, family="binomial")
exp(cbind(OR = coef(modelSEX), confint(modelSEX)))

# BPMEDS */
modelBPMEDS <- glm(dat2$HBP ~ dat2$BPMEDS, family="binomial")
exp(cbind(OR = coef(modelBPMEDS), confint(modelBPMEDS)))

# PREVSTRK */
modelPREVSTRK <- glm(dat2$HBP ~ dat2$PREVSTRK, family="binomial")
exp(cbind(OR = coef(modelPREVSTRK), confint(modelPREVSTRK)))


# DONE WITH PROBLEM 2 */
  #%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%*/
  
  # PROBLEM 3. INVESTIGATE INTERACTION BETWEEN BPMEDS AND BMI
#Run a logistic regression model using the predictors BMI, BPMEDS(ref="no"), and the interaction between
#BMI and BPMEDS.  Provide only the table of parameter estimates.*/
  
  # FIRST, CREATE AN INTERACTION TERM BETWEEN BMI AND BPMEDS */
dat2$intBMI_BPMEDS <- dat2$BMI*dat2$BPMEDS

#NOW RUN THE MODEL */
model3 <- glm(dat2$HBP ~ 
                dat2$BMI +
                dat2$BPMEDS + 
                dat2$intBMI_BPMEDS, family="binomial")
exp(cbind(OR = coef(model3), confint(model3)))

# DONE WITH PROBLEM 3 */
  #%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%*/
  
  
  #PROBLEM 4. THE FOLLOWING ANALYSIS ADDS VARIABLES TO THE PREVIOUS INVESTIGATION */
  # Run a logistic regression modeul using the predictors AGE, SEX, PREVSTRK, BMI, BPMEDS (ref="no"),
#and the interaction between BMI and BPMEDS.  Provide only the table of parameter estimates */

model4 <- glm(dat2$HBP ~
                dat2$AGE +
                dat2$SEX +
                dat2$PREVSTRK +
                dat2$BMI +
                dat2$BPMEDS + 
                dat2$intBMI_BPMEDS, family="binomial")
exp(cbind(OR = coef(model4), confint(model4)))

# DONE WITH PROBLEM 4 */
#%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%*/

#PROBLEM 5
#FILL OUT THAT TABLE AND GET BMI (UNADJUSTED and ADJUSTED)
# FOR WHICH BPMEDS = NO and YES

#Subset dat2 into bpmeds=yes and bpmeds=no
dat2y <- dat2[which(dat2$BPMEDS == 1),]
dat2n <- dat2[which(dat2$BPMEDS == 0),]

#GET UNADJSUTED FOR BOTH
#BPMEDS=Yes
modelBMI_BPMEDSY <- glm(dat2y$HBP ~ dat2y$BMI, family="binomial")
exp(cbind(OR = coef(modelBMI_BPMEDSY), confint(modelBMI_BPMEDSY)))
#BPMEDS-No
modelBMI_BPMEDSN <- glm(dat2n$HBP ~ dat2n$BMI, family="binomial")
exp(cbind(OR = coef(modelBMI_BPMEDSN), confint(modelBMI_BPMEDSN)))

#GET ADJUSTED FOR BOTH
#BPMEDS=YES
modelBMI_BPMEDSYadj <- glm(dat2y$HBP ~ 
                             dat2y$AGE +
                             dat2y$SEX +
                             dat2y$PREVSTRK +
                             dat2y$BMI,
                           family="binomial")
exp(cbind(OR = coef(modelBMI_BPMEDSYadj), confint(modelBMI_BPMEDSYadj)))
#BPMEDS=NO
modelBMI_BPMEDSNadj <- glm(dat2n$HBP ~ 
                             dat2n$AGE +
                             dat2n$SEX +
                             dat2n$PREVSTRK +
                             dat2n$BMI,
                           family="binomial")
exp(cbind(OR = coef(modelBMI_BPMEDSNadj), confint(modelBMI_BPMEDSNadj)))

#%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%*/
  