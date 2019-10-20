options linesize=75 pagesize=60 pageno=1 nodate;

/**********************************
  Jimmy Hickey
  2019-10-18
  ST703 Homework 5
**********************************/


/*
  1
*/

data grades; input grade iq time @@; 
iqtime=iq*time;
datalines;
75 105 10  79 110 12  68 120 6  85 116 13  
91 122 16  79 130 8  98 114 20  76 102 15  . 113 14
run;

proc reg data=grades;
  model grade=time / p clm cli xpx i covb;
run;


proc reg data=grades;
  model grade=iq time iqtime/ p clm cli xpx i covb;
run;

proc glm data=grades;
  model grade=iq time iq*time;
run;


/*
  4.
*/
data insulin;
  input concentration insulin;
  datalines;
low 1.59
low 1.73
low 3.64
low 1.97




/*
  5.
*/
data chem_influx;
  input dose log_d influx;
 datalines;
1  0.00  21
1  0.00  24
1  0.00  26
1  0.00  25
10 2.30  36
10 2.30  38
10 2.30  36
10 2.30  35
100  4.61  43
100  4.61  47
100  4.61  45
100  4.61  49
1000 6.91  54
1000 6.91  56
1000 6.91  58
1000 6.91  53
;
run;

* a ;

proc reg data=chem_influx simple;
  * clb: CI for coefficients;
  * clm: estimate for E(Y) by CI;
  * cli: predict Y by PI;
  model influx=log_d / alpha=0.05 clb clm cli;
run;