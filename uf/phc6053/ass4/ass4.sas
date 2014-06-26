LIBNAME ass4 'E:\workingdirectory\phc6053\ass4';
DATA ass4.mydata;
SET ass4.afghm31;
RUN;

%let dat=ass4.mydata;
RUN;

/*ods pdf file = "C:\Users\BrewJR\Desktop\ass3\ass3.pdf" notoc;*/
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

/* LINEAR REGRESSION Y=LNSBP, X=BMI */
proc reg data=&dat;
model LNSBP=BMI;
run;

/* ADD RESIDUALS and get CI*/
proc reg data=&dat;
model LNSBP=BMI/clb;
output out= myresids residual=r predicted=fv;
run;
quit;

/* LINEAR REGRESSION Y=LNSBP, X=FEMALE */
proc reg data=&dat;
model LNSBP=FEMALE;
run;

/* ADD RESIDUALS and get CI*/
proc reg data=&dat;
model LNSBP=FEMALE/clb;
output out= myresids residual=r predicted=fv;
run;
quit;


/* LINEAR REGRESSION Y=LNSBP, X=MALE */
proc reg data=&dat;
model LNSBP=MALE;
run;

/* ADD RESIDUALS and get CI*/
proc reg data=&dat;
model LNSBP=MALE/clb;
output out= myresids residual=r predicted=fv;
run;
quit;

/*FOR LNSBP=BMI, CREATE HIST OF RESIDULAS, QQ PLOT OF RESIDULAS, 
SCATTERPLOT OF RESIDUALS, SCATTERPLOT OF RESIDUALS VS. BMI, 
and SCATTERPLOT OF RESIDUALS VS PREDICTED VALUES 
proc reg data=&dat;
model LNSBP=BMI;
run;

/* ADD RESIDUALS and get CI 
proc reg data=&dat;
model LNSBP=BMI/clb;
output out= myresids residual=r predicted=fv;
run;
quit;

/*HIST OF RESIDUALS
proc univariate data=myresids;
var r;
histogram;
run;

/* QQPLOT OF RESIDS
proc univariate data=myresids;
var r;
qqplot;
run; */

/* LOTS OF OTHER GRAPHICS FOR LNSBP=BMI 
FIRST REPLACE MY RESIDS STUFF WITH BMI INFO*/
/* ADD RESIDUALS and get CI*/
proc reg data=&dat;
model LNSBP=BMI/clb;
output out= myresids residual=r predicted=fv;
run;
quit;
/*now make graphics*/

proc reg data=&dat plots(unpack)=all;
model LNSBP=BMI/clb;
output out=data2 p=yhat r=resid lclm=lclm uclm=uclm lcl=llclp ucl=uclp;
run;
quit;

/* LOTS OF OTHER GRAPHICS FOR LNSBP=FEMALE 
FIRST REPLACE THE MYRESIDS STUFF WITH DIFFERENT INFO*/


proc reg data=&dat;
model LNSBP=FEMALE/clb;
output out= myresids residual=r predicted=fv;
run;
quit;
/* now make graphics*/

proc reg data=&dat plots(unpack)=all;
model LNSBP=FEMALE/clb;
output out=data2 p=yhat r=resid lclm=lclm uclm=uclm lcl=llclp ucl=uclp;
run;
quit;




/*ods pdf close;*/
