
proc print data=new.bear;
run;

options nodate;

proc sgplot data=new.bear;
loess y=weight  x=age / group=gender smooth=0.65;
run;

/* Looking at 96 months or less */
proc sgplot data=new.bear;
where age <=96;
loess y=weight  x=age / group=gender smooth=0.65;
run;

data new.mybear;
set new.bear;
if age <= 96; /* Keep these observations */
/* could also use -> if age > 96 then delete; */
if weight = . then delete; /* One observation with missing weight */
if chest < 30 then chest_group = "S";
if 30 <= chest <40 then chest_group="M";
if chest >= 40 then chest_group="L"; 
run;

/* Randomly choose 5 observations to reserve for validation purposes */
/*
proc surveyselect data=new.mybear method=SRS sampsize=5 out=new.mybeartest;
run;
*/

data new.mybear;
set new.mybear;
flag = 0;
if ID in (22 23 34 39 43) then flag=1;
run;


ods pdf file="H:\_Amy Docs\UF\000 Spring2014\03 PHC6053\Exams\Exam2\bearsFull.pdf" notoc startpage=never;
ods graphics width=4.5in height=3.5in;
%let quantvars = age length chest headlth  headwth month neck ;

title "5 observations for later model validation";
proc print data=new.mybear;
where flag=1;
var ID weight chest age gender;
run;
title;
ods startpage=now;
title 'Descriptive Summary of Data for Regression Models';
proc freq data=new.mybear;
where flag=0;
tables chest_group chest_group*gender;
run;

proc sort data=new.mybear;
by flag;
run;
ods startpage=now;
title 'Descriptive Summary of Data by Flag (1 = removed, 0 = used in models)';
proc means data=new.mybear n mean median min max std q1 q3 maxdec=1 fw=8;
class flag;
var weight &quantvars;
run;


/*Initial Investigation of relationship between outcome and each variable */
%macro loop_loess(mydat, outcome, qvars,cvar, sm=0.6);
%let nvar = %sysfunc(countw(&qvars));
%do i = 1 %to &nvar;
proc sgpanel data=&mydat; 
panelby flag;
loess y=&outcome x=%scan(&qvars, &i) / group=&cvar smooth=&sm;
run;
%end;
%mend;
ods startpage=now;
title 'Descriptive Summary of Data by Flag (1 = removed, 0 = used in models)';
proc sgpanel data=new.mybear;
panelby flag;
vbox weight / category=gender;
run;
%loop_loess(new.mybear,weight,&quantvars, gender);



/* Models */
ods startpage=now;
title 'Model 1: Chest';
proc glm data=new.mybear plots=all;
model weight = chest / solution clparm;
ods exclude nobs modelanova;
run;
quit;
ods startpage=now;
title 'Model 2: Chest Squared';
proc glm data=new.mybear plots=all;
model weight = chest*chest / solution  clparm;
ods exclude nobs modelanova;
run;
quit;
ods startpage=now;
title 'Model 3: Chest Groups';
proc glm data=new.mybear plots=all;
class chest_group;
model weight = chest_group / solution  clparm;
ods exclude nobs modelanova;
run;
quit;
ods startpage=now;
title 'Model 4: Age, Gender, and Interaction';
proc glm data=new.mybear plots=all;
class gender;
model weight = age gender gender*age/ solution clparm;
ods exclude nobs modelanova;
run;
quit;
ods startpage=now;
title 'Model 5: Chest, Age, Gender, and Interaction';
proc glm data=new.mybear plots=all;
class gender;
model weight = chest age gender gender*age/ solution clparm;
ods exclude nobs modelanova;
run;
quit;
ods startpage=now;
title 'Model 6: Chest squared, Age, Gender, and Interaction';
proc glm data=new.mybear plots=all;
class gender;
model weight = chest*chest age gender gender*age/ solution clparm;
ods exclude nobs modelanova;
run;
quit;
ods startpage=now;
title 'Model 7: Chest Group, Age, Gender, and Interaction';
proc glm data=new.mybear plots=all;
class gender chest_group;
model weight = chest_group age gender gender*age/ solution clparm;
ods exclude nobs modelanova;
run;
quit;

ods pdf close;





ods pdf file="H:\_Amy Docs\UF\000 Spring2014\03 PHC6053\Exams\Exam2\bearsshort.pdf" notoc startpage=never;


proc glm data=new.mybear;
model weight = chest / solution ;
ods exclude classlevels nobs overallanova modelanova;
run;
quit;

ods startpage=now;

proc glm data=new.mybear ;
model weight = chest*chest / solution ;
ods exclude classlevels nobs overallanova modelanova;
run;
quit;

ods startpage=now;
proc glm data=new.mybear ;
class chest_group;
model weight = chest_group / solution ;
ods exclude classlevels nobs overallanova modelanova;
run;
quit;

ods startpage=now;
proc glm data=new.mybear ;
class gender;
model weight = age gender gender*age/ solution;
ods exclude classlevels nobs overallanova modelanova;
run;
quit;

ods startpage=now;
proc glm data=new.mybear ;
class gender;
model weight = chest age gender gender*age/ solution;
ods exclude classlevels nobs overallanova modelanova;
run;
quit;

ods startpage=now;
proc glm data=new.mybear ;
class gender;
model weight = chest*chest age gender gender*age/ solution;
ods exclude classlevels nobs overallanova modelanova;
run;
quit;

ods startpage=now;
proc glm data=new.mybear ;
class gender chest_group;
model weight = chest_group age gender gender*age/ solution;
ods exclude classlevels nobs overallanova modelanova;
run;
quit;


ods pdf close;
ods trace on;
