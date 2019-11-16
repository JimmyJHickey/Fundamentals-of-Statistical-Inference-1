options linesize=75 pagesize=60 pageno=1 nodate;

/**********************************
  Jimmy Hickey
  2019-11-14
  ST703 Homework 8 on Factorial Design
**********************************/


/*
  1.
*/

data aspirin; 
  input subject before after diff;
  datalines;
1 12.3 12.0 0.3
2 12.0 12.3 -0.3
3 12.0 12.5 -0.5
4 13.0 12.0 1.0
5 13.0 13.0 0.0
6 12.5 12.5 0.0
7 11.3 10.3 1.0
8 11.8 11.3 0.5
9 11.5 11.5 0
10 11.0 11.5 -0.5
11 11.0 11.0 0
12 11.3 11.5 -0.2
;;
run;


* a. ;


proc ttest data=aspirin;
  paired before*after;
run;


* b. ;

data aspirin_treatment; 
  input subject proth treatment $;
  datalines;
1 12.3 before
1 12.0 after 
2 12.0 before
2 12.3 after 
3 12.0 before
3 12.5 after 
4 13.0 before
4 12.0 after 
5 13.0 before
5 13.0 after 
6 12.5 before
6 12.5 after 
7 11.3 before
7 10.3 after 
8 11.8 before
8 11.3 after 
9 11.5 before
9 11.5 after 
10 11.0 before
10 11.5 after 
11 11.0 before
11 11.0 after 
12 11.3 before
12 11.5 after 
;;
run;


proc glm data=aspirin_treatment;
  class subject treatment ;
  model proth =  treatment subject;
run;



/*
  2
*/


data cholest;
  input cohort $ age $ gender $ @;
  retain cohort age gender;
  do k=1 to 7;
    input chol @;
  output;
  end;
  drop k;
datalines;
I   younger women 221 213 202 183 185 197 162
II  younger men   271 192 189 209 227 236 142
III older   women 262 193 224 201 161 178 265
IV  older   men   192 253 248 278 232 267 289
;
run;


* a. ;

proc glm data=cholest;
  class cohort;
  model chol = cohort / clparm ;
  
  contrast 'Ages' cohort 2 2 -2 -2 ;
  contrast 'Genders' cohort 2 -2 2 -2 ;
  contrast 'Ages for Women' cohort 1 0 -1 0 ;
  contrast 'Ages for Men' cohort 0 1 0 -1 ;
  contrast 'Genders for younger' cohort 1 -1 0 0 ;
  contrast 'Genders for Older' cohort 0 0 1 -1 ;
  contrast 'Between ages for men and women the same' cohort 1 -1 -1 1 ;
  contrast 'Between genders for the same ages the same' cohort 1 -1 -1 1 ;
 
  estimate 'Ages' cohort 2 2 -2 -2 ;
  estimate 'Genders' cohort 2 -2 2 -2 ;
  estimate 'Ages for Women' cohort 1 0 -1 0 ;
  estimate 'Ages for Men' cohort 0 1 0 -1 ;
  estimate 'Genders for younger' cohort 1 -1 0 0 ;
  estimate 'Genders for Older' cohort 0 0 1 -1 ;
  estimate 'Between ages for men and women the same' cohort 1 -1 -1 1 ;
  estimate 'Between genders for the same ages the same' cohort 1 -1 -1 1 ;
run;



/*
  3.
*/

data bees;
  input trmt temp sucrose @;
  retain temp sucrose;
  do k=1 to 3;
    input energy @;
  output;
  end;
  drop k;
datalines;
1 20 20  3.1  3.7  4.7
2 20 40  5.5  6.7  7.3
3 20 60  7.9  9.2  9.3
4 30 20  6    6.9  7.5
5 30 40 11.5 12.9 13.4
6 30 60 17.5 15.8 14.7
7 40 20  7.7  8.3  9.5
8 40 40 15.7 14.3 15.9
9 40 60 19.1 18.0 19.9
;
run;

* 1 way model on treatment ;

proc glm data=bees order=data;
  class trmt;
  model energy= trmt / clparm;
  
  estimate 'A. temp-sucrose: 20-&30-40 vs 20&30-60' trmt 0 -1 1 0 1 -1 0 0 0 ;
  estimate 'B. temp-sucrose: 40' trmt 0 0.5 -0.5 0 0.5 -0.5 0 -1 1;
/*   estimate 'C. ' trmt -1 ; */
  
  
  contrast 'iii'  trmt 0 -1 1 0 1 -1 0 0 0,
                  trmt 0 0.5 -0.5 0 0.5 -0.5 0 -1 1;
run;