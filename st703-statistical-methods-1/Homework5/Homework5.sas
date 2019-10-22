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
1 1.59
1 1.73
1 3.64
1 1.97
2 3.36
2 4.01
2 3.49
2 2.89
3 3.92
3 4.82
3 3.87
3 5.39
;
run;

proc reg data=insulin plots=none;
  * clb: CI for coefficients;
  * clm: estimate for E(Y) by CI;
  * cli: predict Y by PI;
  model insulin=concentration / alpha=0.05 clb clm cli ;
run;


proc glm data=insulin;
  class concentration; 
  model insulin=concentration / xpx i p solution clparm;
  estimate 'conc 1' intercept 1 concentration 1 0 0;
  estimate 'conc 2' intercept 1 concentration 0 1 0; 
  estimate 'conc 3' intercept 1 concentration 0 0 1; 
run;

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
1000 6.91  53
1000 6.91  54
1000 6.91  56
1000 6.91  58
;
run;

* a ;

proc reg data=chem_influx alpha=0.01;
  model influx=log_d/ p clb cli xpx i covb;
run;

proc corr data=chem_influx;
  var influx log_d;
run;

proc glm data=chem_influx alpha=0.005;
  class log_d; 
  model influx=log_d / clm xpx i p solution clparm;
  estimate 'log_10' intercept 1 log_d 0 1 0 0; 
run;


/*
  6.
*/

* a i ;
proc reg data=chem_influx alpha=0.01;
  model influx=dose/ p clb cli xpx i covb;
run;

* a ii ;
proc reg data=chem_influx alpha=0.01;
  model influx=log_d/ p clb cli xpx i covb;
run;


