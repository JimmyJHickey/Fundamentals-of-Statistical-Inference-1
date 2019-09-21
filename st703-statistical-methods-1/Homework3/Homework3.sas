options linesize=75 pagesize=60 pageno=1 nodate;
/**********************************
  Jimmy Hickey
  2019-09-16
  ST703 Homework 3
**********************************/

/*
  1
*/

data galton;
  infile '/folders/myfolders/grad-scripts/st703-statistical-methods-1/data/galton_height_l.txt' firstobs=2;
  input parent son;
run;

/*
  I
*/

* getting a confidence interval ignoring the parents average height;
proc ttest data=galton sides=2 alpha=0.05;
  var son;
run;


/*
  II
*/

proc corr data=galton
  plots=matrix(histogram) csscp
  fisher(alpha=0.05 type=twosided biasadj=no);
  
  var parent son;
run;


/*
  III
*/

data add_predict;
  input parent son;
datalines;
68 .
72 .
;
run;

data galton_pred;
  set galton add_predict;
run;

proc reg data=galton_pred simple;
  model son=parent / alpha=0.05 clb clm cli;
  id parent;
  output out=galton_reg
    residual=r pred=yhat
    ucl=pihigh lcl=pilow
    uclm=cihigh lclm=cilow
    stdp=stdmean;
run;

/*
  3
*/


data chirp;
  infile '/folders/myfolders/grad-scripts/st703-statistical-methods-1/data/chirps.txt' firstobs=2;
  input chirps temperature;
run;


proc corr data=chirp
  plots=matrix(histogram) csscp;
  var chirps temperature;
run;

* d. ;
data add_predict_chirp;
  input chirps temperature;
datalines;
. 80
. 105
;
run;

data chirp_pred;
  set chirp add_predict_chirp;
run;

proc reg data=chirp_pred simple;
  model chirps=temperature / alpha=0.05 clb clm cli;
  id temperature;
  output out=chirp_reg
    residual=r pred=yhat
    ucl=pihigh lcl=pilow
    uclm=cihigh lclm=cilow
    stdp=stdmean;
run;

