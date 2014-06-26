*READ IN THE DATA FROM ASSIGNMENT 1;
LIBNAME exam 'C:\Users\BrewJR\Desktop\exam2';
DATA exam.mydata;
SET exam.fghm122;
RUN;

*ASSIGN THE DATA TO &DAT;
%let dat=ass8.mydata;
RUN;

/* TAKE A LOOK AT THE DATA */
proc print data=&dat (obs=7);
run;

*OPTIONAL: OUTPUT DIRECTLY AS A PDF FILE/;
ods pdf file = "C:\Users\BrewJR\Desktop\ass8\ass8.pdf" notoc;

/* PROBLEM 1. CREATE A NEW DATASET WHICH */
/* -(a) Contains all of the original data plus the categorized version of BMI previously created */
/* -(b) Creates a new variables called HBP (high blood pressure) which uses the groups defined above */
/* -(c) Removes all individuals in the underweight BMI group AND all individuals with a BMI which is 40 or larger */


/* 1(a) CREATE A BMIGROUP VARIABLE*/
data &dat;
set &dat;
BMIGROUP=.;
if BMI<18.5 then BMIGROUP=1;
if BMI >= 18.5 & BMI < 25 then BMIGROUP = 2;
if BMI >= 25 & BMI < 30 then BMIGROUP = 3;
if BMI >= 30 then BMIGROUP = 4;
RUN;

/* 1(b) CREATE HBP */
data &dat;
set &dat;
HBP=.;
if SYSBP >=0 & SYSBP < 140 then HBP = 0;
if SYSBP >= 140 then HBP = 1;
RUN;

/* 1(c) Remove all the individuals in the underweight BMI group AND all the individuals with a BMI which is 40 or larger*/
data dat2;
set &dat;
if BMIGROUP = 1 then delete;
if BMI > 40 then delete;
run;

/* TAKE A LOOK AT THE DATA AGAIN */
proc print data=dat2 (obs=7);
run;

/* DONE WITH PROBLEM 1 */
/*%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%*/

/* PROBLEM 2: UNADJUSTED ODDS-RATIOS PREDICTING THE OUTCOME HBP = YES */
/* Using HBP as the outcome variable, predicting the probability that HBP = Yes, run simple logistic regression models
to obtain unadjusted odds ratios using the predictors:
-AGE
-BMI
-BMIGROUP (ref = normal)
-SEX (ref = male)
-BPMEDS (ref = no)
-PREVSTRK (ref = no)
For each model (in the order just listed), provide only the table of parameter estimates*/

/* AGE */
proc logistic data = dat2;
model HBP(event="1") = AGE;
run;

/* BMI */
proc logistic data = dat2;
model HBP(event="1") = BMI;
run;

/* BMIGROUP */
proc logistic data = dat2;
class BMIGROUP (ref="2") / param=ref;
model HBP(event="1") = BMIGROUP;
run;

/* SEX */
proc logistic data = dat2;
model HBP(event="1") = SEX;
run;

/* BPMEDS */
proc logistic data = dat2;
model HBP(event="1") = BPMEDS;
run;

/* PREVSTRK */
proc logistic data = dat2;
model HBP(event="1") = PREVSTRK;
run;


/* DONE WITH PROBLEM 2 */
/*%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%*/

/* PROBLEM 3. INVESTIGATE INTERACTION BETWEEN BPMEDS AND BMI
Run a logistic regression model using the predictors BMI, BPMEDS(ref="no"), and the interaction between
BMI and BPMEDS.  Provide only the table of parameter estimates.*/

/* FIRST, CREATE AN INTERACTION TERM BETWEEN BMI AND BPMEDS */
data dat2;
set dat2;
intBMI_BPMEDS = BMI * BPMEDS;
run;

/*NOW RUN THE MODEL */
proc logistic data = dat2;
class BPMEDS(ref="0") / param=ref;
model HBP(event="1") = BMI BPMEDS intBMI_BPMEDS;
run;

/* DONE WITH PROBLEM 3 */
/*%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%*/


/*PROBLEM 4. THE FOLLOWING ANALYSIS ADDS VARIABLES TO THE PREVIOUS INVESTIGATION */
/* Run a logistic regression modeul using the predictors AGE, SEX, PREVSTRK, BMI, BPMEDS (ref="no"),
and the interaction between BMI and BPMEDS.  Provide only the table of parameter estimates */
proc logistic data = dat2;
class BPMEDS(ref="0") / param=ref;
class SEX(ref="1") / param=ref;
class PREVSTRK(ref="0") / param=ref;
model HBP(event="1") = AGE SEX PREVSTRK BMI BPMEDS intBMI_BPMEDS;
run;



ods pdf close;
/*%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%*/
