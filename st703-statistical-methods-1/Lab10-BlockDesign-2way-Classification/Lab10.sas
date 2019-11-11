options linesize=75 pagesize=60 pageno=1 nodate;

data heights;
  input therapy $ @;
  retain therapy;
  do block=1 to 5;
    input hatdiff @;
  output;
  end;
datalines;
CD 8 11 9 16 24
DP 2 1 12 11 19
LM -2 0 6 2 11
;
run;

proc glm data=heights;
  class therapy;
  model hatdiff=therapy;
run;

proc glm data=heights;
  class therapy block;
  model hatdiff=therapy block;
  means therapy / tukey;
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
  3
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

proc glm data = bees;
  class trmt;
  model energy = trmt / clparm;
  
  contrast 'temperature curve at sucrose=60 is parallel to the temperature curve at sucrose=40' trmt 0 -1 1 0 1 -1 0 0 0 ;
  contrast 'temp 40 between sucrose 60 and 40 parallel to the avg of other temps at same sucrose range' trmt 0 0.5 -0.5 0 0.5 -0.5 0 -1 1;
  contrast 'name 3' trmt -1 0.5 0.5 1 -0.5 -0.5 0 0 0;
  contrast 'name 4' trmt 0.5 -0.25 -0.25 0.5 -0.25 -0.25 -1 0.5 0.5;
run;