options linesize=75 pagesize=60 pageno=1 nodate;

/**********************************
  Jimmy Hickey
  2019-12-05
  ST703 Homework 10 on Split Plots
**********************************/

data icecream;
 
  input brand cooler time taste;

  datalines;
1 1 1 62
1 1 2 57
1 1 3 51
1 2 1 66
1 2 2 46
1 2 3 58
1 3 1 62
1 3 2 47
1 3 3 54
1 4 1 47
1 4 2 45
1 4 3 40
2 1 1 59
2 1 2 52
2 1 3 56
2 2 1 53
2 2 2 54
2 2 3 51
2 3 1 54
2 3 2 52
2 3 3 43
2 4 1 55
2 4 2 38
2 4 3 48
3 1 1 66
3 1 2 50
3 1 3 59
3 2 1 63
3 2 2 54
3 2 3 51
3 3 1 53
3 3 2 50
3 3 3 41
3 4 1 56
3 4 2 47
3 4 3 57
  ;
run;


proc mixed data = icecream method = type3;
  class brand cooler time;
  model taste = brand|time / ddfm=satterthwaite;
  random cooler(brand);
  
  lsmeans time / adjust = bon cl alpha = 0.05;
run;


data averageicecream;

  input brand cooler yumminess;
  datalines;
1 1 56.67
1 2 56.67
1 3 54.33
1 4 44
2 1 55.67
2 2 52.67
2 3 49.67
2 4 47
3 1 58.33
3 2 56
3 3 48
3 4 53.33
  ;
run;

proc sql;
  CREATE TABLE average_icecram as 
    SELECT AVG(taste) "quotesaverage", brand, cooler
    FROM icecream
    GROUP BY brand, cooler;
quit;

proc glm data = averageicecream; 
  class brand;
  model yumminess = brand;
run;