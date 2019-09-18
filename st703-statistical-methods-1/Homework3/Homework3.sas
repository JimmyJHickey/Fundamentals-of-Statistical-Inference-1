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
  f.
    question about standard error
    in parameter estimates table
  g.
    CI for beta_1
  h.
    regression equation
  l.
    R^2
  m & n.
    asking about sigma hat -> look for Root MSE
  o.
    STderr of Yhat when x=68
      SE(beta_0 hat + beta_1 hat * 68)
*/