options linesize=75 pagesize=60 pageno=1 nodate;


data milk;
  input carton @;

  do lab=1 to 4;
    input count @;
    lncount = log(count);
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

proc glm data=milk; 
  class lab carton; 
  model lncount=lab|carton / clparm;
  random lab|carton; 
  test h= lab carton e=lab*carton;
  random lab|carton / test;
  estimate 'mean' intercept 1;
run;


proc mixed data=milk cl; *to get asymmetric confidence intervals;
  class lab carton;
  model lncount= / solution cl;
  random lab|carton;
run;

/*
  Notice that the glm stderr for intercept estimate is way smaller that the mixed.
  This is because it didn't take the random effect into account. (just treated everything as fixed)
      DON'T TRUST GLM
*/



proc mixed data=milk cl; *to get asymmetric confidence intervals;
  class lab carton;
  model lncount= / solution cl ddfm=satterth;
  random carton|lab;
run;


data diabetes;
  do drug= 1 to 3;
    do admin= 1 to 2;
      input bsreduce @;
      output;
    end;
  end;

  datalines;
18 23 20 17 30 33
16 19 19 15 28 36
13 17 21 20 26 30
;
run;

proc glm data = diabetes;
  class drug admin;
  model bsreduce = drug admin(drug);
  lsmeans admin(drug) / adj = tukey lines;
run;

proc mixed data = diabetes;
  class drug admin;
  model bsreduce =  drug admin(drug);
/*   random drug admin(drug); */
  lsmeans admin(drug) / adj = tukey cl alpha=0.1;
run;


data chicken_plant;
  input day location count;
  lncount = log(count);
  datalines;
1       1      47500
1       1      39600
1       1      53100
1       1      99200
1       1      15700
1       1      32900
1       1      41500
1       1     286500
1       1      49800
1       1      34900
2       1      35000
2       1     195000
2       1      38000
2       1      14900
2       1      32000
2       1      15000
2       1      88000
2       1      29000
2       1     220000
2       1      92000
3       1      68000
3       1     110000
3       1      27000
3       1     345000
3       1      41500
3       1      15100
3       1      27000
3       1     195000
3       1      33000
3       1      91000
1       2      45700
1       2      71400
1       2      19500
1       2      28400
1       2     112100
1       2      21600
1       2      16100
1       2      13500
1       2      74600
1       2      80200
2       2      54000
2       2      45000
2       2      62000
2       2      15200
2       2      82000
2       2      39000
2       2      54000
2       2      67000
2       2      49000
2       2      53000
3       2      11000
3       2      35000
3       2       6200
3       2      18500
3       2      46000
3       2      77000
3       2      55000
3       2      26000
3       2      15000
3       2      42000
1       3       9000
1       3       9500
1       3      12700
1       3      12900
1       3      12600
1       3       8000
1       3      10700
1       3      21300
1       3      13900
2       3      14900
2       3       4000
2       3       7000
2       3      15000
2       3       6000
2       3       4000
2       3       4000
2       3      15000
2       3       6000
2       3       5000
3       3       6000
3       3      15000
3       3       2000
3       3       1000
3       3       4000
3       3       4000
3       3       6000
3       3      15000
3       3       7000
3       3       2000
1       3       9600
1       4       4300
1       4       3100
1       4      18100
1       4      26600
1       4      17200
1       4       6500
1       4      18300
1       4       6400
1       4      11300
1       4       6100
2       4       3000
2       4      14900
2       4       7000
2       4       5000
2       4       3000
2       4      15000
2       4       5000
2       4      15000
2       4      15000
2       4       4000
3       4       3000
3       4       6000
3       4      15000
3       4      14900
3       4       3000
3       4      14800
3       4      15000
3       4       4000
3       4       3000
3       4       5000
;
run;

proc mixed data = chicken_plant cl method = type3;
  class location day;
  model lncount = location / ddfm = satterthwaite;
  random day day*location;
  lsmeans location / adj=tukey;
run;

proc glm data=chicken_plant plots = diagnostics;
  class location day;
  model lncount = day | location;
  random day day*location / test;
  lsmeans location / adj=tukey lines;
run;

proc glm data=chicken_plant plots = diagnostics;
  class location day;
  model count = location | day;
  random day day*location / test;
  lsmeans location / adj=tukey lines;
run;


* nested random effects ;

data acid;
  do plant = 1 to 4;
    do leaf = 1 to 3;
      input acid @;
      output;
    end;
  end;
  
  datalines;
11.2 16.5 18.3 14.1 19.0 11.9 15.3 19.5 16.5 7.3 8.9 11.3
11.6 16.8 18.7 13.8 18.5 12.4 15.9 20.1 17.2 7.8 9.4 10.9
12.0 16.1 19.0 14.2 18.2 12.0 16.0 19.3 16.9 7.0 9.3 10.5
  ;
run;

proc mixed data = acid cl method = type3;
  class plant leaf;
  model acid = / solution ddfm=satterthwaite;
  random plant leaf(plant);
run;

proc glm data = acid plots = diagnostics;
  class plant leaf;
  model acid = plant leaf(plant);
  random plant leaf(plant) / test;
run;