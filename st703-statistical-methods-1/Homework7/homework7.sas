options linesize=75 pagesize=60 pageno=1 nodate;

/**********************************
  Jimmy Hickey
  2019-11-4
  ST703 Homework 7
**********************************/

/*
  3
*/


data soil;
  input depth water;
  datalines;
10  0.313
10  0.299
10  0.340
10  0.289
40  0.315
40  0.310
40  0.294
40  0.293
60  0.289
60  0.284
60  0.281
60  0.275
120 0.259
120 0.259
120 0.245
120 0.251
;;
run;


* SLR ;

proc glm data=soil;
  model water = depth;
run;

* One way/ most complicated model ;

proc glm data=soil;
  class depth;
  model water = depth;
run;


/*
  4
*/

* a ;

data numbahfour;
  nn=3; power=1-cdf('F', quantile('F',.95,5-1,nn*5-5), 5-1, nn*5-5, nn*6.8);
run;
* 0.829379963 ;


data numbahfour;
  nn=4; power=1-cdf('F', quantile('F',.95,5-1,nn*5-5), 5-1, nn*5-5, nn*6.8);
run;
* 0.9616126012  ;



* b ;

proc power;
  onewayanova test=overall alpha=0.05 
     groupmeans= 12 | 12 | 11 | 10 | 9 stddev=1
     npergroup=2 to 10 by 1 power=.;

  * add reference line ; 
  plot x=n min=2 max=10 yopts=(ref=.9 crossref=yes); *try, see what this does;
run;


* c ;

proc power;
  onewayanova test=overall alpha=0.05 
     groupmeans= 12 | 12 | 11 | 10 | 9 stddev=1,10
     npergroup=2 to 10 by 1 power=.;

  * add reference line ; 
  plot x=n min=2 max=10 yopts=(ref=.9 crossref=yes); *try, see what this does;
run;



* d ;

proc power;
  onewayanova test=overall alpha=0.05 
     groupmeans= 15 | 12 | 11 | 10 | 9 stddev=1
     npergroup=2 to 10 by 1 power=.;

  * add reference line ; 
  plot x=n min=2 max=10 yopts=(ref=.9 crossref=yes); *try, see what this does;
run;