options linesize=75 pagesize=60 pageno=1 nodate;

data grades; input grade iq time @@; datalines;
75 105 10  79 110 12  68 120 6  85 116 13  
91 122 16  79 130 8  98 114 20  76 102 15  . 113 14
proc reg data=grades;
  model grade=iq time / p clm cli xpx i covb;
  title 'Exam grade (%) as a function of IQ and study time (hours)';
  run;
  
  
data bodyfat;
  infile '/folders/myfolders/grad-scripts/st703-statistical-methods-1/data/bodyfat.txt' firstobs=2;
  input x1 x2 x3 y;
run;

proc reg data=bodyfat;
  model y=x1 x2 x3;
  model y=x1      ;
  model y=   x2   ;
  model y=      x3;
  model y=x1 x2   ;
  model y=   x2 x3;
run;

proc reg data=bodyfat;
  model y=x1 x2 x3;
  myname1: test x2=x3=0;
  myname2: test x1=x3=0;
run;

proc glm data=bodyfat;
  model y=x1 x2 x3;
run;

proc reg data=bodyfat;
  model y=x1 x2 x3 / ss1 ss2;
run;

proc glm data=bodyfat;
  model y=x1|x3|x2@3 x1*x1 x2*x2 x3*x3;
run;

proc glmselect data=bodyfat plots=(criteria coefficients) seed=7405;
  model y=x1|x3|x2@3 x1*x1 x2*x2 x3*x3 / hierarchy=single
    selection=stepwise(select=rsquare stop=rsquare choose=cv)
  cvmethod=random(5) stats=all details=summary;
run;
