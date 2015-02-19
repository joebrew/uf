****************************************************************;
* Reading Data into SAS (4DM Class)                            *;
*                                    First created on 01/08/14 *;             *<== Informative comments to yourself/others;
*                                 Last updated on 02/02/15 aba *;                  /*   * comments/Dates/authors ;   */
****************************************************************;
Options pageno=1 validVarName=any ps=54 ls=120 nocenter nofmterr noerrorabend mprint mlogic formchar='|____|+|___'; 

Libname RdOut 'F:\4Private\UF\UF2014\aUF2014\Session4';                       *<== Global Statements(System Options, Libnames, Value/Variable Formats, Titles, Footnotes and Macros;    
                                                          *NB: This statement has to change to your location of the data I sent with this file;
Run;                                                                          *<== Each step is executes when a Step boundarie is encountered;  
Proc format;                                                                     /* Semicolons are a MUST to mark the end of a SAS statement */
 Value yn    1='No'      5='Yes';
 Value gndr  0='Female'  1='Male';

Run;
**************************************;
**************************************;
*Data Step: Reading in DATALINE data *;                                       *<== Data Step should precede the procedure using the data;  
**************************************;
**************************************;
Data PBudget;
 Input EFName $ Salary12 Bonus13 Married $ NumofKids Birth date7. hired date7.;  
  Salary13 =(1 + Bonus13/100)*Salary12;
  NSalary  =Round(Salary13);
  Draise   =NSalary - Salary12;
  Female   =(EFName in('John','Steve'));
  now=today();
  Age=(now-birth)/364.25;
  Age2 =Round(Age);
  HasKids =(NumofKids >0);
   if NumofKids=. then HasKids =.m; 
Datalines;
Mary    38210  3.5 N 1 02MAR73 26MAR92
John    29755  3.0 Y 0 09NOV72 23NOV93
Beth    27985  2.5 Y 2 19MAY71 23OCT94
Steve   51587  4.0 Y 3 09NOV69 23NOV98
Sue     29855  3.5 N . 02APR70 17APR99
Molly   70998  2.5 Y 1 01DEC60 07MAY88
;
Run;
*PROC. Step: Performing DA ;                                                  *<== SAS procedures can be used anywhere, but after the data is made known to SAS;        
***************************; 
Proc print data=PBudget; *(Obs =1); 
 Format Female gndr. Salary12 Salary13 NSalary dollar10.2;
  Var EFName Female Age Age2 Salary12 Salary13 NSalary;          run;
run;
Proc freq data=PBudget;
 Tables Female age2 /xxxx;  *Missing; *Missprint;   run;
Proc means Data=PBudget n nmiss mean median min max std stderr Var skewness kurtosis maxdec=2 fw=10;
 Var Female Salary12 NSalary Draise Bonus13;        run;
run;
Proc freq data=PBudget;
 Tables Married*HasKids /xxxx;  *All;   *Chisq RELRISK Agree; *list;   run;
Proc sort data=PBudget; by Female;                  run;
Proc tTest data=PBudget;
 Format female gndr.;
  Class female;
   Var NSalary;                                     run;
Run;
*1*Alternative ways to read/Import data into SAS;
************************************************;
*****************************************;
*Data step: Reading in data from TXT file;
*****************************************;
Data PBudget3;
 Infile 'F:\4Private\UF\UF2015\ClassSessions\Session5\New Business\Pbudget.txt' Delimiter = ';,' MISSOVER DSD lrecl=80;         
  Input EFName $ Salary12 Bonus13;                        *Change data file location;
Run;
*PROC. Step: Performing DA ;
***************************;
Proc contents data=Pbudget3 varnum; * short;        run;
Proc print data=PBudget3;
 Format Salary12 dollar10.2;
 Var EFName Salary12 Bonus13;                       run;
Run;
***********************************************;
*Data step: Reading in TXT file via PROC IMPORT;
***********************************************;
Proc import Datafile="F:\4Private\UF\UF2015\ClassSessions\Session5\New Business\Pbudget2.txt"  
 Out=PBudget4 dbms=dlm replace;                           *Change data file location;
 Delimiter = ',';
 Getnames=yes;                                      run;
Run;
*PROC. Step: Performing DA ;
***************************;
Proc contents data=Pbudget4 varnum;     run;
Proc print data=PBudget4;
 Format Salary12 dollar10.2;
 Var EFName Salary12 Bonus13;          run;
Run;
****************************************;
*Data step: Reading in Excel data (XLS) ;
****************************************;
*Using SAS PROC IMPORT;
Proc import Out =PBudget5
	 Datafile ="F:\4Private\UF\UF2015\ClassSessions\Session5\New Business\PBudget.xls"                                      
      DBMS=xls replace;                                   *Change data file location;
	  Sheet='PBudget';
      Getnames =Yes;                                run;
Run;
*Using SAS PROC SQL;
Proc sql;
 Connect to Excel(path='F:\3INQUIRI\Projects_old\Rani Cold\AFinal_Clean.xlsx');
  Create table work.test as select * from connection to excel (select * from [AFinalClean$]);
  Disconnect from Excel;
Quit;
Run;Run;
*****************************************;
*Data step: Reading in Access data (MDB) ;
*****************************************;
Proc import out= PBudget6
            Datatable= "PBudget"
            DBMS=ACCESS REPLACE;
            Database="F:\4Private\UF\UF2015\ClassSessions\Session5\New Business\PBudget.mdb";   run;
Run;
*************************************************************;
*Data step: How to read in a SAS data(depends on SAS engine) ;
*************************************************************;
*Reading in an existent SAS data set;
Libname RdIn 'F:\4Private\UF\UF2015\ClassSessions\Session5\New Business';
Data EmailSurveyData; Set RdIn.fssemaildata;   *(Keep =AIDNum SSOS_SurgeryID SSOS_EmailListID);
 Rename SSOS_SurgeryID =SSSurgID SSOS_EmailListID =SSEmlLID;
Proc sort data=EmailSurveyData; by AIDNum;          run;   
Run;
Options pageno=1 validVarName=any ps=54 ls=220;
Proc contents data=EmailSurveyData;                 
Options pageno=1 validVarName=any ps=54 ls=120      run;
Run;
*Output SAS data to have a permaent SAS data set;
Libname RdOut 'F:\4Private\UF\UF2015\ClassSessions\Session5\New Business\data';
/*Data RdOut.TstnData; set EmailSurveyData;   run; */
Run;
*Create a SAS XPT file (using PROC COPY\CIMPORT) to read across SAS versions;
Libname init 'F:\4Private\UF\UF2015\ClassSessions\Session5\New Business\data';
Libname outit xport 'F:\4Private\UF\UF2015\ClassSessions\Session5\New Business\Emldata.xpt';

Proc copy in=init out=outit;                                      run;
*Proc cimport library=init file=OutIt;
Run;
*How to import data from SPSS/STATA or others;
***************************************;
*Data step: Reading in SPSS data (POR) ;
***************************************;
Libname sasdat   'F:\4Private\UF\UF2015\ClassSessions\Session5\New Business\';
Filename spssdat 'F:\4Private\UF\UF2015\ClassSessions\Session5\New Business\gi_all.por';

Proc convert spss=spssdat out=sasdat.GIntake;     run;
Run;
***************************************;
*Data step: Reading in SPSS data (SAV) ;
***************************************;
PROC IMPORT OUT= BRBUMACS 
            DATAFILE= "F:\4Private\UF\UF2015\ClassSessions\Session5\New Business\BRBUMData.sav" 
            DBMS=SPSS REPLACE;                    run;
Data Testdata; Set BRBUMACS;

Proc sort data=TestData; by ResID;                run;
Proc contents data=Testdata varnum;               run; 
Run;Run;
*2*Alternative ways to Read out/Export data from SAS;
****************************************************;
******************************************;
*Export SAS data into an Excel data (XLS) ;
******************************************;
Proc export DATA=PBudget
 OUTFILE ="F:\4Private\UF\UF2015\ClassSessions\Session5\New Business\New2015.xlsx"
  DBMS=xlsx;
  Sheet='nPBudget';                                   run;
Run;Run;
*3*Data Protection- Using encription;
************************************;
*****************************;
*Data step: Using ENCRYPTION ;
*****************************;
Data PBudget2(Encrypt=yes Read=Un14peu Alter=ibrA); Set PBudget;  
Proc sort data=PBudget2(read=Un14peu Alter=ibrA); by Female;      run;
Proc contents data=PBudget2(read=Un14peu Alter=ibrA);             run;
Run;
Proc tTest data=PBudget2;
 Format female gndr.;
  Class female;
   Var NSalary;                                     run;
Run;Run;

Run;

Run; Run;
