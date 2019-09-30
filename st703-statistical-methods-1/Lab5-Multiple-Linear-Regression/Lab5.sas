options linesize=75 pagesize=60 pageno=1 nodate;


/*
  1
*/

libname mydata "/folders/myfolders/grad-scripts/st703-statistical-methods-1/data";

data cars;
  set mydata.cars1993;
run;


/*
  2
*/

proc print data=cars(obs=12);
run;


*  check which columns are missing observations ;
proc contents data=cars;
run;


proc freq data=cars;
run;


proc means data=cars;
run;

proc univariate data=cars;
run;


/*
  3
*/

* look at multicolinearity ;
proc corr data=cars plots=matrix(histogram nvar=all);
  var citympg numcyl engliters length wheelbase width weight;
run;


* just scatter plot between y and x ;
proc corr data=cars plots=matrix(histogram nvar=all);
  with citympg;
  var citympg numcyl engliters length wheelbase width weight;
run;


/*
  4
*/

proc reg data=cars;
  model citympg=numcyl engliters length wheelbase width weight / ss1 ss2;
run;


/*
  6
*/

* a ;
proc reg data=cars (where=(numcyl>0));
  model citympg= wheelbase weight / ss1 ss2;
run;


* b ;
proc reg data=cars (where=(numcyl>0));
  model citympg=numcyl engliters length wheelbase width weight / ss1 ss2;
  reduced: test numcyl=engliters=length=width=0;
run;

* c ;

* beta_weight = -0.01008 ;


* d ;

* beta_wheelbase = 0.19999 ;


* e ;
proc reg data=cars (where=(numcyl>0));
  model citympg= wheelbase weight / ss1 ss2;
  output out=myregout residual=r pred=yhat;
run;


* f ;

proc corr data=myregout;
  var citympg yhat;
run;

* 0.86 ;

* h ;
* corr(y_hat,  y)^2 = R^2 ;

* i ;
* literally all of them ;


* j ;
proc glm data=cars (where=(numcyl>0));
  model citympg = wheelbase weight weight*weight / ss1 ss2;
run;

* gives better R^2 ;

* k ;
data cars_add; weight=2957; wheelbase=97.4; numcyl=1;

data cars_pred;
  set cars cars_add;
run;