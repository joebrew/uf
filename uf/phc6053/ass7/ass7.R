

#q1
q1est <- exp(10*0.0252)
q1high <- exp((10*0.0252) + (1.96*(10*0.00715)))
q1low <- exp((10*0.0252) - (1.96*(10*0.00715)))

#q2
q2est <- exp(0.9367)
q2low <- exp(0.9367 - (1.96*0.2151))
q2high <- exp(0.9367 + (1.96*0.2151))

#q3
#p(x) = (e ^(B0+B1X1+...))/(1 + e^(B0+B1X1...))
q3px40 <- -0.6280 + (40*0.0252)
q3px50 <- -0.6280 + (50*0.0252)

q3oddsx40 <- exp((40*0.0252))
q3oddsx50 <- exp((50*0.0252))

q3or10 <- q3oddsx50/q3oddsx40
q3rr <- 0.632/0.38

#excess risk (same as ARR)
q3er <- 0.632-0.38

#q4

#q5
exp(0.4592)
exp(1.0536)
exp(0.8113)

#82-104 vs. 72-81
exp(log(2.251) - log(2.868))
exp(0.8113 - 1.0536) #another way to get same thing

#82-104 vs. 60-71
exp(log(2.251) - log(1.583)) 
exp(0.8113 - 0.4592)

#72-81 vs. 60-71
exp(log(2.868) - log(1.583))
exp(1.0536 - 0.4592)

#q6
sysbpOR <- exp(0.00987)
miordOR <- exp(0.9035)

#q7
#CANTRELL'S ANSWER
#P(X) = exp(linear predictor)/
#(1 + exp(linear predictor))

q7a <- 
  -5.0099 + #intercept
  (0*0.5074) + #gender
  (70*0.0258) + #age
  (25*0.0808) + #bmi
  (115*0.00987) + #sysbp
  (1*0.5848) + #mitype
  (1*0.9035) #miord

#prob
log(q7a) / (1-log(q7a))
#cantrell's way:
exp(q7a) / (1+exp(q7a)) #THIS IS CORRECT

q7b <- 
  -5.0099 + #intercept
  (1*0.5074) + #gender
  (60*0.0258) + #age
  (28*0.0808) + #bmi
  (135*0.00987) + #sysbp
  (1*0.5848) + #mitype
  (0*0.9035) #miord

#prob q7b
exp(q7b) / (1+exp(q7b)) 


#odds ratio 
exp(q7a/q7b)

#q8
logoddsf3071 <- 
  0.5351 + #intercept
  (1*0.9072) + # female
  (0* 0.8920) + # 72-104 years-old
  ((1*0) * -.7785) #gender*72-104
logoddsm3071 <- 
  0.5351 + #intercept
  (0*0.9072) + # female
  (0* 0.8920) + # 72-104 years-old
  ((0*0) * -.7785) #gender*72-104

FtoM_OR3071 <- exp(logoddsf3071)/ exp(logoddsm3071)



logoddsf72104 <- 
  0.5351 + #intercept
  (1*0.9072) + # female
  (1* 0.8920) + # 72-104 years-old
  ((1*1) * -.7785) #gender*72-104
logoddsm72104 <- 
  0.5351 + #intercept
  (0*0.9072) + # female
  (1* 0.8920) + # 72-104 years-old
  ((0*1) * -.7785) #gender*72-104

#CONFIDENCE INTERVALS
logoddsf72104high <- 
  logoddsf72104 +
  (1*1.96*0.3457) + #female
  (1*1.96*0.2758) + #72104
  (1*1*1.96*0.4731)
exp(logoddsf72104high)

logoddsf72104low<- 
  logoddsf72104 +
  (1*1.96*-0.3457) + #female
  (1*1.96*-0.2758) + #72104
  (1*1*1.96*-0.4731)
exp(logoddsf72104low)


FtoM_OR72104 <- exp(logoddsf72104) / exp(logoddsm72104 )

#q9

#FvsM40 
f40 <- 
  -1.2865 + #intercept
  (1*3.8674) + #female
  (40*0.0330) + #age
  (40*1*-0.0471) #age-gender interaction
m40 <- 
  -1.2865 + #intercept
  (0*3.8674) + #female
  (40*0.0330) + #age
  (40*0*-0.0471) #age-gender interaction
OR_FvsM40 <- exp(f40)/ exp(m40)

#FvsM50 
f50 <- 
  -1.2865 + #intercept
  (1*3.8674) + #female
  (50*0.0330) + #age
  (50*1*-0.0471) #age-gender interaction
m50 <- 
  -1.2865 + #intercept
  (0*3.8674) + #female
  (50*0.0330) + #age
  (50*0*-0.0471) #age-gender interaction
OR_FvsM50 <- exp(f50)/ exp(m50)

#FvsM60 
f60 <- 
  -1.2865 + #intercept
  (1*3.8674) + #female
  (60*0.0330) + #age
  (60*1*-0.0471) #age-gender interaction
m60 <- 
  -1.2865 + #intercept
  (0*3.8674) + #female
  (60*0.0330) + #age
  (60*0*-0.0471) #age-gender interaction
OR_FvsM60 <- exp(f60)/ exp(m60)


#FvsM70 
f70 <- 
  -1.2865 + #intercept
  (1*3.8674) + #female
  (70*0.0330) + #age
  (70*1*-0.0471) #age-gender interaction
m70 <- 
  -1.2865 + #intercept
  (0*3.8674) + #female
  (70*0.0330) + #age
  (70*0*-0.0471) #age-gender interaction
OR_FvsM70 <- exp(f70)/ exp(m70)


#FvsM80 
f80 <- 
  -1.2865 + #intercept
  (1*3.8674) + #female
  (80*0.0330) + #age
  (80*1*-0.0471) #age-gender interaction
m80 <- 
  -1.2865 + #intercept
  (0*3.8674) + #female
  (80*0.0330) + #age
  (80*0*-0.0471) #age-gender interaction
OR_FvsM80 <- exp(f80)/ exp(m80)


#FvsM90 
f90 <- 
  -1.2865 + #intercept
  (1*3.8674) + #female
  (90*0.0330) + #age
  (90*1*-0.0471) #age-gender interaction
m90 <- 
  -1.2865 + #intercept
  (0*3.8674) + #female
  (90*0.0330) + #age
  (90*0*-0.0471) #age-gender interaction
OR_FvsM90 <- exp(f90)/ exp(m90)

