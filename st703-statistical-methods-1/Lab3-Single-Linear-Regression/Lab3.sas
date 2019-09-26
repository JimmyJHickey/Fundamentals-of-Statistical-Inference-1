options linesize=75 pagesize=60 pageno=1 nodate;
/*
  Lab 3
*/


data corn;
  infile '/folders/myfolders/grad-scripts/st703-statistical-methods-1/data/corn.txt' firstobs=2;
  input year yield rain;
run;


/*
  plots=matrix: give a scatter plot matrix and histogram
  csscp: gives corrected sum of squares and cross products
  fisher: gives correlation statistics based on Fisher's z transforation
    95% lower bound with rho0 = 0.3
      note that type=lower is for the null hypothesis
*/
proc corr data=corn
  plots=matrix(histogram) csscp
  fisher(alpha=0.5 type=lower biasadj=no rho0=0.3);
  
  var rain yield;
run;

/* 
  predict
*/

data add_predict;
  input year yield rain;
datalines;
. . 14
;

data corn_pred; 
  set corn add_predict;
run;

* check output to check assumptions ; 
proc reg data=corn_pred simple;
  * clb: CI for coefficients;
  * clm: estimate for E(Y) by CI;
  * cli: predict Y by PI;
  model yield=rain / alpha=0.05 clb clm cli;
  id rain;
  
  output out=corn_reg
    residual=r pred=yhat
    ucl=pihigh lcl=pilow
    uclm=cihigh lclm=cilow
    stdp=stdmean;
run;

proc print data=corn_reg;
run;