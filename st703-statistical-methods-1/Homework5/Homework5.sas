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