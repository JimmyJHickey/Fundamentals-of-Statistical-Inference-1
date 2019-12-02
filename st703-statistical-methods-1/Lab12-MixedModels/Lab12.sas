options linesize=75 pagesize=60 pageno=1 nodate;

* one way random effect ;

data cows;
  input sire trmt @;

  do block=1 to 8;
    input weight @;
    output;
  end;
  drop block;
  datalines;
177 1 61 100 56 113 99 103 75 62
200 2 75 102 95 103 98 115 98 94
201 3 58 60 60 57 57 59 54 100
202 4 57 56 67 59 58 121 101 101
203 5 59 46 120 115 115 93 105 75
;
run;

proc glm data=cows;
  class sire;
  model weight=sire;
  random sire / test;
  run;

proc mixed data=cows method=type3 cl covtest;
  class sire;
  model weight= / solution cl;
  random sire;
run;

proc mixed data=cows method=reml cl covtest;
  class sire;
  model weight= / solution cl;
  random sire;
run;


* two way ;

data milk;
  input carton @;

  do lab=1 to 5;
    input units @;
    logunit=log(units);
    output;
  end;
  datalines;
1 2200 2600 1900 2600 4000
1 2200 2500 2100 4300 3900
2 3000 3600 2500 2800 4800
2 2900 3500 2200 1800 4800
3 210 290 160 330 370
3 200 240 200 340 340
4 270 360 230 350 500
4 260 380 230 290 480
;
run;


proc mixed data = milk method = type3 cl covtest;
  class carton lab;
  model logunit = / solution;
  random carton|lab;
run;