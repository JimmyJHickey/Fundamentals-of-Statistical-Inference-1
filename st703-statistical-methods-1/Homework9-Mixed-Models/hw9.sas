options linesize=75 pagesize=60 pageno=1 nodate;

/**********************************
  Jimmy Hickey
  2019-11-25
  ST703 Homework 9 on Mixed Models
**********************************/

/*
  2
*/

data wethers;
  input diet @;

  do location=1 to 4;
    input weight @;
    output;
  end;
  datalines;
1 2.10 2.02 2.16 1.98
1 2.32 2.04 2.18 1.86
2 2.24 2.30 2.22 1.64
2 2.22 2.12 2.18 1.73
3 2.28 2.14 2.26 1.83
3 2.24 2.17 2.21 1.89
;
run;

proc mixed data=wethers method=type3 cl covtest alpha = 0.10;
  class diet location;
  model weight = diet / ddfm = satterthwaite solution cl;
  random location diet*location;
  
  lsmeans diet / adjust=bon cl alpha=0.1;
run;