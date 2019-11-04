options linesize=75 pagesize=60 pageno=1 nodate;

/* data vitamins; input supplment @; retain type; */
/*  */
/* do i=1 to 4;  */
/*   input weight @;  */
/*   output;  */
/* end; */
/*  */
/* datalines; */
/* 1 48 67 78 69 53 */
/* 2 65 49 37 75 63 */
/* 3 79 52 63 65 67 */
/* 4 59 50 59 42 34 */
/* 1 350 440 440 510 470 */
/* 2 400 450 370 530 420 */
/* 3 510 410 470 470 480 */
/* 4 530 520 520 510 430 */
/* ; */
/* run; */

data vitamins; input supplement @; retain type;

do i=1 to 4; 
  input wtgain calorie @; 
  output; 
end;

datalines;
1 48 350 67 440 78 440 69 510 53 470
2 65 400 49 450 37 370 75 530 63 420
3 79 510 52 410 63 470 65 470 67 480
4 59 530 50 520 59 520 42 510 34 430
;
run;



proc glm data=vitamins; **try the ANOVA model without confounder;
  class supplement; 
  model wtgain=supplement;
run;
proc glm data=vitamins; ** build the ANCOVA model with confounder;
  class supplement;
  model wtgain=supplement calorie;
  means supplement;    
  means supplement / tukey scheffe bon;
  lsmeans supplement / lines adjust=tukey stderr;
  lsmeans supplement / lines adjust=scheffe stderr;
  lsmeans supplement / lines adjust=bon stderr;
run;
proc glm data=vitamins; **check if the confounder is appropriate in ANCOVA;
  class supplement;
  model calorie=supplement;
run;


/*
  Sample Size Calculations
*/

* Find n for equal allocation ;

proc power;
  onewayanova test=overall alpha=0.05 
     groupmeans=230 | 230 | 320 stddev=100
     npergroup=. power=.8;
run;


* Find n where n_1 = 2n, n_2 = n, n_3 = n ;

proc power;
  onewayanova test=overall alpha=0.05 
     groupmeans=230 | 230 | 320 stddev=100
     groupweights=(2 1 1) ntotal=. power=.8;
run;


* Make a power plot ;

proc power;
  onewayanova test=overall alpha=0.05 
     groupmeans=230 | 230 | 320 stddev=100
     npergroup=3 to 25 by 1 power=.;
  
  
  * plot x=n min=3 max=25; 
  
  * add reference line ; 
  plot x=n min=3 max=25 yopts=(ref=.8 crossref=yes); *try, see what this does;
run;



data one;
  do n=3 to 25; output; end;
data one; set one;
  t=  3 ; *number of groups in 1-way model;
  nu1= t - 1  ; *numerator degrees of freedom;
  nu2=   n*t -   ; *denominator degrees of freedom;
  sumtau2=(30)**2 + (30)**2 + 60**2; *interested in this sum of squared differences;
  sigma2=10000; 
  ncp=n*sumtau2/sigma2; *noncentrality parameter;
  critval=quantile('f',0.95,nu1,nu2); *critical value for F-test;
  power=1-cdf('f',critval,nu1,nu2,ncp); 
proc print; run;
symbol1; symbol2;
proc gplot data=one;
  plot power*n;
run;
