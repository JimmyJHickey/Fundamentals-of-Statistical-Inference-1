                                 options linesize=75 pagesize=60 pageno=1 nodate;

/*
  From ch6 slide 20
*/

data antibiotics; input type $15. @; retain type;
do i=1 to 4; input binding @; output; end;
datalines;
Penicillin G    29.6 24.3 28.5 32
Tetracycline    27.3 32.6 30.8 34.8
Streptomycin    5.8 6.2 11 8.3
Erythromycin    21.6 17.4 18.3 19
Chloramphenicol 29.2 32.8 25 24.2
;
data anti; set antibiotics;
  x1=(type="Penicillin G"); x2=(type="Tetracycline"); x3=(type="Streptomycin");
  x4=(type="Erythromycin"); x5=(type="Chloramphenicol");
  
proc reg data=anti plots=none;
  model binding=x1 x2 x3 x4 x5 / xpx i p;
  
proc glm data=antibiotics;
  class type; 
  model binding=type / xpx i p solution;
  
proc anova data=antibiotics;
  class type;
  model binding=type;
  
proc glm data=antibiotics order=data;
  class type;
  model binding=type / xpx i p solution;
  
/* 
  On your own
*/

* 1 ;
proc reg data=anti plots=none;
  model binding=x1 x2 x3 x5 x4 / xpx i p;
  
* get rid of intercept ;
proc reg data=anti plots=none;
  model binding=x1 x2 x3 x5 x4 / xpx i p noint;
  
* 8 ;
proc reg data=anti plots=none;
  model binding=x1 x2 x3 x5 x4 / noprint;

  * H0: mu1= mu3 ;
  A: test x1-x3=0;

  * H0: mu2 = mu4 = mu5 ;
  B: test x2=x4=x5;
  
  * H0: 1/2(mu1 + mu3) = 1/3(mu2 + mu4 + mu5) ;
  * C: test 0.5*x1 + 0.5*x3 = 0.333*x2 + 0.333*x4 + 0.333*x5 THIS DOESNT WORK WTF SAS ;
    C: test 3*x1+3*x3 = 2*x2+2*x4+2*x5; 
run;



/*
  full-versus-reduced model
*/


* 8 a ;
data anti; set anti;
  x1px3=x1+x3;
run;

proc reg data=anti plots=none;
  Full: model binding=x1 x2 x3 x4 x5;
  ReducedA: model binding=x1px3 x2 x4 x5;
run;

proc reg data=anti plots=none;
  ReducedA: model binding=x1 x2 x3 x4 x5;
  restrict x1=x3;
run;


* 8 b ;
data anti; set anti;
   x2px4px5 = x1 + x4 + x5;
run;

proc reg data=anti plots=none;
  Full: model binding=x1 x2 x3 x4 x5;
  ReducedA: model binding=x1 x2px4px5 x3;
run;

proc reg data=anti plots=none;
  ReducedB: model binding=x1 x2 x3 x4 x5;
  restrict x2=x4=x5;
run;


* 8 c ;

/* This doesn't seem to work */
/*  */
/* data anti; set anti; */
/*    three_x1x3 = 3 * x1 + 3 * x3; */
/*    two_x2x4x5 = 2 * x2 + 2 *x4 + 2 * x5; */
/* run; */
/*  */
/* proc reg data=anti plots=none; */
/*   Full: model binding=x1 x2 x3 x4 x5; */
/*   ReducedC: model binding=three_x1x3 two_x2x4x5; */
/* run; */

proc reg data=anti plots=none;
  ReducedC: model binding=x1 x2 x3 x4 x5;
  restrict 3*x1+3*x3 = 2*x2+2*x4+2*x5; 
run;


/*
  Contrasts
  
          contrast  |   estimate
  test    F-test    |     t-test
  multi   yes       |     no
  CI      no        |     yes
 decomp   yes       |     no
 sum=0    yes       |     no
*/

proc glm data=antibiotics;
  class type;
  model binding=type / clparm e;
  means type;
  contrast 'w/i bactericidal,i.e. hyp a'  type 0 0 1 -1 0; *note ordering is alphabetical;
  contrast 'w/i bacteriostatic,i.e. hyp b'  type 0 1 0 0 -1, type -2 1 0 0 1; 
  contrast 'w/i bacteriostatic,i.e. hyp b, part 1'  type 0 1 0 0 -1; 
  contrast 'w/i bacteriostatic,i.e. hyp b, part 2'  type -2 1 0 0 1; 
  contrast 'w/i bacteriostatic,i.e. hyp b'  type 0 1 0 0 -1, type 1 0 0 0 -1; 
  contrast 'bactericidal vs bacteriostatic,i.e. hyp c'  type -2 -2 3 3 -2; 
  estimate 'w/i bactericidal,i.e. hyp a'  type 0 0 1 -1 0; *note ordering is alphabetical;
  estimate 'w/i bacteriostatic,i.e. hyp b, part 1'  type 0 1 0 0 -1; 
  estimate 'w/i bacteriostatic,i.e. hyp b, part 2'  type -2 1 0 0 1; 
  estimate 'bactericidal vs bacteriostatic,i.e. hyp c'  type -2 -2 3 3 -2; 
  estimate 'bactericidal vs bacteriostatic,i.e. hyp c'  type -2 -2 3 3 -2 / divisor=6; 
  contrast 'aaa' type 1 1 0 0 0;
  
  contrast '11: different design for hyp b' type 0 1 0 -1 0, type -2 1 0 1 0;
run;

