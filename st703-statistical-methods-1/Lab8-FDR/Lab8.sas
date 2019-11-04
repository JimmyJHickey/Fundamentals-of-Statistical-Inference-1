options linesize=75 pagesize=60 pageno=1 nodate;

data antibiotics; input type $15. @; retain type;
do i=1 to 4; input binding @; output; end;
datalines;
Penicillin G    29.6 24.3 28.5 32
Tetracycline    27.3 32.6 30.8 34.8
Streptomycin    5.8 6.2 11 8.3
Erythromycin    21.6 17.4 18.3 19
Chloramphenicol 29.2 32.8 25 24.2
;
run;

data anti; set antibiotics;
  x1=(type="Penicillin G"); x2=(type="Tetracycline"); x3=(type="Streptomycin");
  x4=(type="Erythromycin"); x5=(type="Chloramphenicol");
run;

proc multtest data=antibiotics order=data bonferroni fdr 
          plots=(adjusted(unpack) pbytest(vref=.05));
  class type;
  
  * ddfm means use equal variances for each group ;
  test mean(binding / ddfm=pooled); 
  contrast '1-2' 1 -1;
  contrast '1-3' 1 0 -1; 
  contrast '1-4' 1 0 0 -1; 
  contrast '1-5' 1 0 0 0 -1; 
  contrast '2-3' 0 1 -1; 
  contrast '2-4' 0 1 0 -1; 
  contrast '2-5' 0 1 0 0 -1; 
  contrast '3-4' 0 0 1 -1;
  contrast '3-5' 0 0 1 0 -1;
  contrast '4-5' 0 0 0 1 -1;
run;


data tpvals;
  input test $ raw_p @@;
datalines;
1vs2 0.4526 1vs3 0.0504 1vs4 0.3947 1vs5 0.6800 1vs6 0.3243 1vs7  .0001 
2vs3 0.3270 2vs4 0.1822 2vs5 0.2439 2vs6 0.9510 2vs7 0.0037 3vs4 0.0392 
3vs5 0.0113 3vs6 0.2211 3vs7 0.0095 4vs5 0.5467 4vs6 0.1372 4vs7 0.0011 
5vs6 0.1281 5vs7  .0001 6vs7 0.0002
;
run;

proc multtest inpvalues=tpvals bonferroni fdr 
          plots=(adjusted(unpack) pbytest(vref=.05));
run;


** Source: http://www.lexjansen.com/nesug/nesug06/an/da24.pdf ;
%MACRO SUM_ANOVA(N_MEAN_STD=);
   TITLE1 'SUM_ANOVA: ANALYSIS OF VARIANCE ON SUMMARY STATISTICS';
   TITLE2 "YOU HAVE ENTERED: N_MEAN_STD=&N_MEAN_STD";
   DATA _SUMMARY;
     %LET I=0;
     %DO %WHILE (%SCAN(&N_MEAN_STD, &I+1, %STR( )) NE %STR( ));
       %LET I=%EVAL(&I+1);
     %END;
     ARRAY TEMPVALUE(&I) (&N_MEAN_STD);
     %DO K=1 %TO &I/3;
       GROUP = "GROUP&K";
       N_I = TEMPVALUE(%EVAL((&K-1)*3+1));
       YBAR_I = TEMPVALUE(%EVAL((&K-1)*3+2));
       STD_I = TEMPVALUE(%EVAL((&K-1)*3+3));
       YIS = YBAR_I + SQRT((STD_I**2)/N_I);
       YNS = N_I * YBAR_I - (N_I - 1)*YIS;
       Y = YIS; FREQ = N_I - 1; OUTPUT;
       Y = YNS; FREQ=1; OUTPUT;
   %END;
   
   PROC GLM DATA=_SUMMARY;
       CLASS GROUP;
       FREQ FREQ;
       MODEL Y = GROUP;
       LSMEANS GROUP/ PDIFF;
     means group/ bon scheffe tukey;
   RUN;
       TITLE;
%MEND SUM_ANOVA; 
%sum_anova(n_mean_std=4 28.600 3.218  4 31.375 3.171  
       4 7.825 2.384  4 19.075 1.806  4 27.800 3.990 );



data hiking;
  input x @;
  retain x;
  do i=1 to 4;
    input y @;
  output;
  end;
  drop i; 
datalines;
0   20.7 15.9 17.8 17.6
25  12.9 13.4 12.7  9.0
75  11.8 12.6 11.4 12.1
200  7.6  9.5  9.9  9.0
500  7.8  9.0  8.5  6.7
;
run;

data hiking; set hiking;
  xclass=x; 
run;

proc reg data=hiking;
  model y=x;
run;

proc glm data=hiking;
  class x;
  model y=x;
run;

data hiking_quartic;
  set hiking;
  x2 = x * x;
  x3 = x * x * x;
  x4 = x * x * x * x;
run;

proc reg data=hiking_quartic;
  simple: model y = x;
  quartic: model y=x x2 x3 x4;
  cubic: model y = x x2 x3; 
run;

proc glm data=hiking;
  class x;
  model y=x;
run;


/* THIS IS BROKEN ON MAC */

/* data hiking; set hiking; */
/*   xclass=x; run; */
/* symbol1 i=rlclm95 v=circle c=blue; */
/* symbol2 i=rqclm95 v=circle c=blue; */
/* proc gplot data=hiking; */
/*   plot y*x=1 y*x=2;  */
/* proc glm data=hiking; */
/*   class xclass; */
/*   model y=x xclass; */
/* proc glm data=hiking; */
/*   class xclass; */
/*   model y=x x*x xclass; */




* lack of fit test ;

proc glm data=hiking;
  class xclass;
  model y=x x*x x*x*x xclass;
run;



* modeling log ;
data hiking_log; set hiking;
  x = log(x+0.0000001);
  xclass = x;
  x2 = x*x;
  x3 = x*x*x;
  x4 = x*x*x*x;
run;