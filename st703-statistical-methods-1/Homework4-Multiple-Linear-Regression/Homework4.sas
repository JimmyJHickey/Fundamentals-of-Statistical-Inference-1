options linesize=75 pagesize=60 pageno=1 nodate;
/**********************************
  Jimmy Hickey
  2019-09-28
  ST703 Homework 4
**********************************/

/*
  1
*/

data run;
  infile '/folders/myfolders/grad-scripts/st703-statistical-methods-1/data/ResolutionRun2004.txt' firstobs=3;
  input obs sex $ age age2 pace;
run;

proc reg data=run;
  model pace=age age2;
run;