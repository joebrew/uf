LIBNAME ass5 'E:\workingdirectory\phc6053\ass5';
DATA ass5.mydata;
SET ass5.bfghm36;
RUN;

%let dat=ass5.mydata;
RUN;

ods pdf file = "E:\workingdirectory\phc6053\ass5\ass5.pdf" notoc;
proc print data=&dat (obs=7);
run;

/* CREATE A NEW VARIABLE CALLED FEMALE WHICH IS 0 FOR MALES AND 1 FOR FEMALES */
data &dat;
set &dat;
FEMALE=.;
if SEX=1 then FEMALE=0;
if SEX=2 then FEMALE=1;
RUN;

data &dat;
set &dat;
MALE=.;
if SEX=1 then MALE=1
if SEX=2 then MALE=0;
RUN;
proc print data=&dat (obs=7);
run;

/* PEARSON'S CORRELATION COEFFICIENT BETWEEN LNSBP, AGE, AND BMI */
proc corr data=&dat;
var LNSBP AGE BMI;
run;

/* MULTIPLE LINEAR REGRESSION Y=LNSBP, X=AGE, BMI, AND FEMALE */
proc reg data=&dat;
model LNSBP=BMI AGE FEMALE;
run;
/* ADD RESIDUALS and get CI*/
proc reg data=&dat;
model LNSBP=BMI AGE FEMALE/clb vif ;
output out= myresids residual=r predicted=fv;
run;
quit;
/*HIST OF RESIDUALS */
proc univariate data=myresids;
var r;
histogram;
run;

/* MULTIPLE LINEAR REGRESSION Y=LNSBP, X=AGE, BMI, AND MALE */
proc reg data=&dat;
model LNSBP=BMI AGE MALE;
run;
/* ADD RESIDUALS and get CI*/
proc reg data=&dat;
model LNSBP=BMI AGE MALE/clb ;
output out= myresids residual=r predicted=fv;
run;
quit;

/* MULTIPLE LINEAR REGRESSION Y=LNSBP, X=AGE, BMI, AND MALE */
proc reg data=&dat;
model LNSBP=BMI AGE MALE;
run;

/* SCATTERPLOT AND LOESS LINE: Y=LNSBP, X=AGE*/
proc loess data=&dat;
      model LNSBP=AGE;
   run;

 

   /* SCATTERPLOT WITH LOESS LINE BY GENDER*/
   /* FIRST SORT THE DATA */
   proc sort data=&dat;
   by FEMALE;
   run;

/* SCATTERPLOT AND LOESS LINE: Y=LNSBP, X=AGE*/
proc loess data=&dat;
by FEMALE;
      model LNSBP=AGE;
   run;
  /* SCATTERPLOT AND LOESS LINE: Y=LNSBP, X=BMI*/
proc loess data=&dat;
by FEMALE;
      model LNSBP=BMI;
   run;
 /* SCATTERPLOT AND LOESS LINE: Y=BMI, X=AGE*/
proc loess data=&dat;
 by FEMALE;
      model BMI=AGE;
   run;



ods pdf close;
