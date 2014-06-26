options nonumber nodate;
ods pdf notoc;

%let dat=temp.hersdata;

proc contents data=&dat VARNUM;
run;

proc format;
value yn    0 = "No"
            1 = "Yes";
value phy   1 = "much less active"
            2 = "somewhat less active"
            3 = "about as active"
            4 = "somewhat more active"
            5 = "much more active";
value yna   0 = "B:No"
            1 = "A:Yes";
value phya  1 = "E:1-much less active"
            2 = "D:2-somewhat less active"
            3 = "C:3-about as active"
            4 = "B:4-somewhat more active"
            5 = "A:5-much more active";
run;


data hers;
set &dat;
where diabetes=0; 
format exercise drinkany yn. physact phy.;
run;

/** distribution of the outcome variable ***/

/* Using SGPLOT */
proc sgplot data=hers;
histogram glucose;
density glucose / type=normal;
run;

proc sgplot data=hers;
vbox glucose / category=exercise;
run;


/** simple linear regression of glucose on exercise **/
ods graphics on;
proc reg data=hers;
model glucose=exercise /clb;
model glucose = weight;
run;
quit;
ods graphics off;

ods graphics on;
proc glm data=hers order=formatted PLOTS=( diagnostics(unpack) ) ;
format exercise yna.;
class exercise ;
model glucose=exercise / solution clparm;
run;
quit;
ods graphics off;

ods graphics on;
proc glm data=hers order=internal;
class exercise ;
model glucose=exercise / solution;
run;
quit;
ods graphics off;

/* equivalence of reg, t-test and corr */
proc ttest data=hers;
class exercise;
var glucose;
run;

proc corr data=hers;
var exercise glucose;
run;

/** multiple linear regression of glucose on exercise age alcohol and BMI**/

ods graphics on;
proc reg data=hers;
model glucose=exercise age drinkany bmi / clb vif;
run;
quit;
ods graphics off;


ods graphics on;
proc glm data=hers order=formatted PLOTS=( diagnostics(unpack) );
format exercise drinkany yna.;
class exercise drinkany;
model glucose=exercise age drinkany bmi / solution;
run;
quit;
ods graphics off;


/** alternative coding for exercise ***/

data hers; set hers; 
if exercise=0 then exercise1=2;
if exercise=1 then exercise1=1;
run;


data hers2; set hers; 
if exercise=0 then exercise1=0;
if exercise=1 then exercise1=2;
run;

ods graphics on;
proc reg data=hers;
model glucose=exercise1 /clb;;
run;
quit;
ods graphics off;

/*** multilevel categorical predictors ***/
data hers; set hers;
physact2=0; /* Important to have no missing data! */
if physact=2 then physact2=1;
physact3=0;
if physact=3 then physact3=1;
physact4=0;
if physact=4 then physact4=1;
physact5=0;
if physact=5 then physact5=1;
run;

/* if there are missing data in physact ***/

data hers; set hers;
if physact ne . then physact2=0;
if physact=2 then physact2=1;
if physact ne . then physact3=0;
if physact=3 then physact3=1;
if physact ne . then physact4=0;
if physact=4 then physact4=1;
if physact ne . then physact5=0;
if physact=5 then physact5=1;
run;

ods graphics on;
proc reg data=hers;
model glucose=physact2 physact3 physact4 physact5 / clb;
test physact5-physact3=0;
run;
quit;
ods graphics off;


/** contrasts **/
ods graphics on;
proc glm data=hers order=formatted PLOTS=( diagnostics(unpack) );
format physact phya.;
class physact;
model glucose=physact /  solution;
/* these are backwards because of my formatting */
contrast 'physact 5 vs. 3' physact 1 0 -1 0 0;
estimate 'physact 5 vs. 3' physact 1 0 -1 0 0;
contrast 'physact 4 vs. 3' physact 0 1 -1 0 0;
estimate 'physact 4 vs. 3' physact 0 1 -1 0 0;
run;
quit;
ods graphics off;


/** two other ways to get same contrast results **/
ods graphics on;
proc glm data=hers order=internal;
class physact;
model glucose=physact /  solution;
contrast 'physact 5 vs. 3' physact 0 0 -1 0 1;
estimate 'physact 5 vs. 3' physact 0 0 -1 0 1;
contrast 'physact 4 vs. 3' physact 0 0 -1 1 0;
estimate 'physact 4 vs. 3' physact 0 0 -1 1 0;
run;
quit;
ods graphics off;

ods graphics on;
proc glm data=hers;
model glucose=physact2 physact3 physact4 physact5 /  solution;
contrast 'physact 5 vs. 3' physact3 -1 physact5 1;
estimate 'physact 5 vs. 3' physact3 -1 physact5 1;
contrast 'physact 4 vs. 3' physact4 1 physact3 -1;
estimate 'physact 4 vs. 3' physact4 1 physact3 -1;
run;
quit;
ods graphics off;



/* WRONG!!! unless the effect is linear as physact increases */
ods graphics on;
proc reg data=hers;
model glucose=physact / clb;
run;
quit;
ods graphics off;





/* Back to Body dataset */
*ods trace on;
proc contents data=temp.body varnum;
ods select position ;
run;
*ods trace off;

proc sgplot data=temp.body;
loess y=height x=pelvic_breadth / smooth=0.5 group = gender;
run;

/* The same model three ways */
ods graphics on;
proc reg data=temp.body plots=all;
model height = pelvic_breadth gender / clb vif;
output out=data1 p=yhat r=resid  lclm=lclm uclm=uclm lcl=lclp ucl=uclp;
run;
quit;
ods graphics off;

ods graphics on;
proc glm data=temp.body plots=all;
class gender;
model height = pelvic_breadth gender / solution;
run;
quit;
ods graphics off;

proc format;
value gen 1="A:Male" 0="B:Female";
run;

ods graphics on;
proc glm data=temp.body plots=all;
format gender gen.;
class gender;
model height = pelvic_breadth gender / solution;
run;
quit;
ods graphics off;

proc sort data=data1;
by height;
run;

proc print data= data1 (obs=10);
var height pelvic_breadth gender yhat resid lclm uclm lclp uclp; 
run;

/* A larger model */
ods graphics on;
proc reg data=temp.body plots=all;
model height = 	pelvic_breadth gender elbow_diam weight thigh_girth
				waist_girth bicep_girth calf_girth chest_girth knee_diam 
				wrist_girth forearm_girth  / clb vif;
output out=data2 p=yhat r=resid  lclm=lclm uclm=uclm lcl=lclp ucl=uclp;
run;
quit;
ods graphics off;


proc sort data=data2;
by height;
run;

proc print data= data2 (obs =5);
var 	height yhat resid lclm uclm lclp uclp pelvic_breadth 
		gender elbow_diam weight thigh_girth waist_girth 
		bicep_girth calf_girth chest_girth knee_diam 
		wrist_girth forearm_girth ; 
run;

ods pdf close;
