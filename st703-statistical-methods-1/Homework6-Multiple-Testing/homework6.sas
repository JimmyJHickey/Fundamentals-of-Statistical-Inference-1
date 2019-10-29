options linesize=75 pagesize=60 pageno=1 nodate;

/**********************************
  Squire James
  2019-10-23
  ST703 Homework 6
**********************************/

data plants;
input group height;
datalines;
1 32.94
1 35.98
1 34.76
1 32.40
2 30.55
2 32.64
2 32.37
2 32.04
3 31.23
3 31.09
3 30.62
3 30.42
4 34.41
4 34.88
4 34.07
4 33.87
5 35.61
5 35.00
5 33.65
5 32.91
;
run;


proc glm data=plants;
  class group;
  model height=group / clparm e;
  means group;
  contrast 'theta1'  group 0 1 1 -1 -1;
  contrast 'theta2'  group 0 1 -1 1 -1; 
  contrast 'theta3'  group 0 1 -1 -1 1;
  contrast 'theta4'  group 4 -1 -1 -1 -1; 

  estimate 'theta1'  group 0 1 1 -1 -1;
  estimate 'theta2'  group 0 1 -1 1 -1; 
  estimate 'theta3'  group 0 1 -1 -1 1;
  estimate 'theta4'  group 4 -1 -1 -1 -1; 

  means group / t scheffe bon tukey cldiff;
run;

proc multtest data=plants order=data fdr 
          plots=(adjusted(unpack) pbytest(vref=.05));
  class group;
  test mean(height / ddfm=pooled); 
  contrast '1-2' 1 -1;
  contrast '1-3' 1 0 -1; 
  contrast '1-4' 1 0 0 -1; 
  contrast '1-5' 1 0 0 0 -1; 
  contrast '2-3' 0 1 -1; 
  contrast '2-4' 0 1 0 -1; 
  contrast '2-5' 0 1 0 0 -1; 
  contrast '3-4' 0 0 1 -1;
  contrast '3-5' 0 0 1 0 -1;
  contrast '4-5' 0 0 0 1 -1;
run;
