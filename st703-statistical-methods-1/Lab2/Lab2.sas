options linesize=75 pagesize=60 pageno=1 nodate;
/*
  Lab 2
*/

data st703.alloy;
  infile '/folders/myfolders/grad-scripts/st703-statistical-methods-1/data/AlloyStrength.txt';
  input strength @@;
run;

proc print data=st703.alloy;


* t-test, using proc means – only H0:µ=0 versus H1:µ≠0     [but can halve p-value] ;
proc means data=st703.alloy n mean min max var std stderr 
      lclm uclm median alpha=.1 t probt;
  var strength;
  label strength="Compressive Strength (psi)";
  title "Compressive strength measurements (psi) of";
  title2 "aluminum-lithium alloy specimens";
run;

* t-test, using proc univariate – only H0:µ=µ0 versus H1:µ≠µ0     [but can halve p-value] ;
proc univariate data=st703.alloy plots cibasic alpha=.1 mu0=160;
  var strength;
  label strength="Compressive Strength (psi)";
run;

* t-test, using proc ttest ;
proc ttest data=st703.alloy alpha=.025 h0=160 sides=u; *also have sides=2 or sides=l; 
  var strength;
  label strength="Compressive Strength (psi)";
run;


/*
  two sample t-test
*/

* 2-sample t-test using proc ttest ;
proc ttest data=st703.arsenic alpha=0.05 h0=0 sides=u;
  var conc;
  class type;
  label conc="Arsenic concentration (ppb)"l
  title "Arseinc Concentration in Drinking Water in Arizona";
  title2;
run;

* paired t-test using proc ttest ;
proc ttest data=st703.grip alpha=0.05 sides=u h0=5;
  paired dominant*off;
  title "Arm gripping strength";
  title2;
run;

/*
  Determining power
*/

proc power;
  onesamplemeans test=t dist=normal alpha=0.05
  nullmean=160 sides=u mean=170 stddev=33.7732 
  ntotal=80 power=.;
run; 
 
proc power;
  onesamplemeans test=t dist=normal alpha=0.05
  nullmean=160 sides=u mean=170 stddev=20 33.7732 40
  ntotal=50 80 100 power=.;
run;

proc power;
  onesamplemeans test=t dist=normal alpha=.05 .1
    nullmean=160 sides=u mean=170 stddev=20 33.7732 40
  ntotal=50 80 100 power=.;
  plot x=effect min=150 max=180 yopts=(ref=.8 crossref=yes)
    vary(panel by stddev, linestyle by ntotal, color by alpha);
  title "Power analysis";
run;

* graph power instead of effect ;
/* proc power; */
/*   onesamplemeans test=t dist=normal alpha=.05 */
/*     nullmean=160 sides=u mean=170 w00 stddev=20 33.7732 40 */
/*     ntotal=. power=0.8; */
/*   plot x=power min=0.1 max=0.9 yopts=(ref=.8 crossref=yes) */
/*     vary(panel by stddev, linestyle by mean, color by alpha); */
/*   title "Power analysis"; */
/* run; */


/*
  Determine sample size
*/

proc power;
  onesamplemeans test=t dist=normal alpha=.05
    nullmean=160 sides=u mean=170 stddev=20 33.7732 40
  ntotal=. power=.8 .9 .95;
  plot x=effect min=165 max=180 yopts=(ref=80 crossref=yes)
    vary(panel by stddev, linestyle by power);
  title "Power analysis";
run;
 
proc power;
  onesamplemeans test=t dist=normal alpha=.05 nfractional
    nullmean=160 sides=u mean=170 stddev=20 33.7732 40
  ntotal=. power=.8 .9 .95;                     *sides= 2 | u | l;
  plot x=effect min=165 max=180 yopts=(ref=80 crossref=yes)
    vary(panel by stddev, linestyle by power);    *x=effect | n | power;
  title "Power analysis";
run;

/*
 two sample t-test 
   group weights breaks the ntotal into groups  
   e.g. 
    ntotal=20 groupweights = (1 1)
      n_1 = 20*(1/2), n_2 = 20*(1/2)
    ntotal=20 groupweights = (1 2)
      n_1 = 20*(1/3), n_2 = 20*(2/3)
*/

proc power;
  twosamplemeans test=diff_satt dist=normal alpha=.05
    nulldiff=0 sides=u meandiff=10 groupstddevs=(15.3 7.6)
    ntotal=20 30 40 50 60 groupweights=(1 1) (1 2) (2 1)
    power=.;
  title "Power analysis";
run;
