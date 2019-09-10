options linesize=75 pagesize=60 pageno=1 nodate;
/**********************************
  Jimmy Hickey
  2019-09-08
  ST703 Homework 2
**********************************/

/*
  4.4
*/

* b. ;

data hcl;
  infile datalines delimiter=',';
  input infection_rate @@;
  datalines;
    2.95, 2.11, 1.83, 2.06, 1.90, 1.95, 1.71, 1.98, 2.24, 1.59, 1.87, 1.93, 2.11, 1.96, 1.94, 1.17, 1.77, 1.98, 2.34, 2.18, 2.26, 1.69, 2.12, 2.18, 1.91, 1.28, 2.27, 2.13, 1.38, 2.03, 2.29, 1.74, 2.07, 1.25, 2.37
  ;
run;

/* proc means data=hcl noprint; */
/*   output mean(infection_rate) = mean_infection_rate */
/* run; */

proc ttest data=hcl sides=l h0=2.5 alpha=0.1;
  var infection_rate;
run;

* (c) Compute the power of the test to detect that the true infection rate for the responding treated patients is 2.4 per month.;
proc power;
  onesamplemeans test=t dist=normal 
  mean=2.4 stddev=0.3517 ntotal=35 power=.
  alpha=0.1 nullmean=2.5 sides=l;
run;

* (d) Obtain and plot the power curve for true infection rates ranging from 2 per month to 3 per month. ;
proc power;
  onesamplemeans test=t dist=normal
  mean=2 3 stddev=0.3517 ntotal=25 power=.
  alpha=0.1 nullmean=2.5 sides=l;
  plot x=effect min=0 max=4 yopts=(ref=80 crossref=yes)
    vary(linestyle by mean);
run;

* (e) What sample size would we need to detect that true infection rate for responding treated patients is 2.4 per month with power at least 0.9? ;
proc power;
  onesamplemeans test=t dist=normal 
  mean=2.4 stddev=0.3517 ntotal=. power=0.9
  alpha=0.1 nullmean=2.5 sides=l;
run;

* (f) Overlay the power curve from (d) with the new power curve based on the sample size determined in (e). ;


/*
 4.11
*/


data lambs;
  input lamb $ before after;
datalines;
1 0.095 0.176
2 0.106 0.142
3 0.082 0.194
4 0.152 0.136
5 0.090 0.115
6 0.086 0.084
7 0.137 0.103
8 0.121 0.189
;
run;

* (a) ;

proc ttest data=lambs sides=u h0=0 alpha=0.05;
  paired after*before;
run;

* (b) ;
proc ttest data=lambs alpha=0.05;
  paired after*before;
run;

* (c) What is the power of the test to detect that histamine increases PVR on average by 0.05 units? ;
proc power;
  pairedmeans test=diff dist=normal 
  corr=-0.1114011 
  meandiff=0.05 pairedstddevs=(0.02563445, 0.04091433) npairs=8 power=.
  alpha=0.05 nulldiff=0 sides=u;
run;

* (d) What sample size is required to change the power in (c) to at least 0.9? ;

proc power;
  pairedmeans test=diff dist=normal
  corr=-0.1114011 
  meandiff=0.05 pairedstddevs=(0.02563445, 0.04091433) npairs=. power=0.9
  alpha=0.05 nulldiff=0 sides=u;
run;


/*
 6.2
*/

* (c) Use the p-value of an appropriate hypothesis test to draw conclusions about the claim that the germination rate exceeds 90% ; 

data germination;
  input germinated $ count;
  datalines;
germinated 44
ungerminated 6
;
run;

proc freq data=germination order=data;
  weight count;
  tables germinated/binomial(level='germinated' p=0.9);
run;

* (d) Using the Wald procedure, obtain and plot the power curve for the t ;

proc freq data=germination order=data;
  weight count;
  tables germinated / chisq plots=freqplot riskdiff(cl=(wald ac score));
run;


proc power;
  onesamplefreq test=z method=normal alpha=0.05
    nullp=0.9 sides=u p=0.88 ntotal=50 power=.;
   plot x=effect min=0 max=1;
run;