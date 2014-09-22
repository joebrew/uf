proc import
datafile="H:\Teaching\phc 6937-Spatial Epidemiology\Y2012\Week 4_Geocoding\
In class demo\Solid Waste Facility.xls" 
out=waste
dbms=excel replace;
run; *Change the Direcotry when you work on your data;
proc sort data=waste;
by ID__;
run;
data wasteb;
set waste;
*donot to change this part;
Latitudeb=compress(left(trim(Latitude)));
Longitudeb=compress(left(trim(Longitude)));
second=input(compress(scan(Latitudeb,-1,":"), "    "), 8.6);
minutes=input(scan(Latitudeb,2,":"), 8.0);
degree=input(scan(Latitudeb,1,":"), 8.0);
second_b=input(compress(scan(Longitudeb,-1,":"), "    "), 8.6);
minutes_b=input(scan(Longitudeb,2,":"),8.0);
degree_b=input(scan(Longitudeb,1,":"), 8.0);
lat=degree+minutes/60+second/3600;
long=0-(degree_b+ minutes_b/60+ second_b/3600);
run;



proc export data=wasteb
outfile="H:\Teaching\phc 6937-Spatial Epidemiology\Y2012\Week 4_Geocoding\
In class demo\FL_WASTE_site.xls"
dbms=EXCEL replace;
run; *Change the Direcotry when you work on your data;
