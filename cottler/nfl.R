library(sas7bdat)

########
# READ IN DATA
########
nfl <- read.sas7bdat(paste0("/home/joebrew/Desktop/cottler/Cottlerlab-ESPN/",
                            "ESPN Clean Data PHI/espndatafinal.sas7bdat"))
########
# ADD SOME MORE UNDERSTANDABLE COLUMN NAMES
########
library(car)
nfl$health_start <- Recode(nfl$B1,
                           "1 = 'Excellent';
                           2 = 'Good';
                           3 = 'Fair';
                           4 = 'Poor'")
nfl$injury_end_career <- Recode(nfl$B7,
                                "1 = 'No';
                                5 = 'Yes'")
nfl$six_months_no_work_pain <- Recode(nfl$B8,
                                      "1 = 'No';
                                5 = 'Yes';
                                      3 = NA")
nfl$health_past12 <- Recode(nfl$B10,
                            "1 = 'Excellent';
                           2 = 'Good';
                           3 = 'Fair';
                           4 = 'Poor'")
nfl$sedatives_prescribed <- nfl$D1A
nfl$sedatives_prescribed_other <- nfl$D2A
nfl$sedatives_not_prescribed <- nfl$D3a
nfl$sedatives_abused <- ifelse(is.na(nfl$sedatives_prescribed_other),
                               FALSE,
                               ifelse(is.na(nfl$sedatives_not_prescribed),
                                      FALSE,
                                      ifelse(
                                        nfl$sedatives_prescribed_other > 0, TRUE,
                                        ifelse(
                                          nfl$sedatives_not_prescribed > 0,
                                          TRUE,
                                          FALSE))))

nfl$opioids_prescribed <- nfl$D7A
nfl$opioids_prescribed_other <- nfl$D8
nfl$opioids_not_prescribed <- nfl$D9
nfl$opioids_abused <- ifelse(is.na(nfl$opioids_prescribed_other),
                               FALSE,
                               ifelse(is.na(nfl$opioids_not_prescribed),
                                      FALSE,
                                      ifelse(
                                        nfl$opioids_prescribed_other > 0, TRUE,
                                        ifelse(
                                          nfl$opioids_not_prescribed > 0,
                                          TRUE,
                                          FALSE))))
# Does opioid abuse predict sedative abuse?
fit <- glm(opioids_abused ~ sedatives_abused,
           data = nfl,
           family = binomial("logit"))
exp(coef(fit))


# Does career ending injury and health past 12 months predict sedative abuse??
fit <- glm(sedatives_abused ~ injury_end_career ,#+ health_past12,
           data = nfl,
           family = binomial("logit"))
exp(cbind(OR = coef(fit), confint(fit)))########
#
########


########
#
########


########
#
########


########
#
########


########
#
########


########
#
########


########
#
########


########
#
########


