options linesize=75 pagesize=60 pageno=1 nodate;

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
run;

proc glm data = cholest;
  class age gender;
  model chol = age|gender / clparm;

  contrast '1 Ages' age 2 -2 gender 0 0 age*gender 1 1 -1 -1;
  contrast '2 Gender' age 0 0 gender 2 -2 age*gender 1 -1 1 -1;
  contrast '3 Ages, Women' age 1 -1 gender 0 0 age*gender 1 0 -1 0;
  contrast '4 Ages, Men' age 1 -1 gender 0 0 age*gender 0 1 0 -1;
  contrast '5 Gender, Young' age 0 0 gender 1 -1 age*gender 1 -1 0 0;
  contrast '6 Gender, Older' age 0 0 gender 1 -1 age*gender 0 0 1 -1;
  contrast '7 Age, Gender' age 0 0 gender 0 0 age*gender 1 -1 -1 1;
  
  estimate 'Main effect: Age' age 1 -1 gender 0 0 age*gender 0 0 0 0;
  estimate 'Main effect: Gender' age 0 0 gender 1 -1 age*gender 0 0 0 0;
  
  * these estimates will be 2 times bigger than they should because I didn't divide through by 2;
  estimate '1 Ages' age 2 -2 gender 0 0 age*gender 1 1 -1 -1;
  estimate '2 Gender' age 0 0 gender 2 -2 age*gender 1 -1 1 -1;
  estimate '3 Ages, Women' age 1 -1 gender 0 0 age*gender 1 0 -1 0;
  estimate '4 Ages, Men' age 1 -1 gender 0 0 age*gender 0 1 0 -1;
  estimate '5 Gender, Young' age 0 0 gender 1 -1 age*gender 1 -1 0 0;
  estimate '6 Gender, Older' age 0 0 gender 1 -1 age*gender 0 0 1 -1;
  estimate '7 Age, Gender' age 0 0 gender 0 0 age*gender 1 -1 -1 1;
  
run;