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

proc glm data=antibiotics;
  class type;
  model binding=type;
  means type / t scheffe bon tukey cldiff;
run;

* unequal group sizes ;
proc glm data=antibiotics(where=(binding<31));
  class type;
  model binding=type;
  means type / t scheffe bon tukey lines; 
run;

proc glm data=antibiotics order=data;
  class type;
  model binding=type;
  means type / t scheffe bon tukey;
run;


proc glm data=antibiotics order=data;
  class type;
  model binding=type;
  lsmeans type / pdiff;
  lsmeans type / pdiff adjust=scheffe; 
  lsmeans type / pdiff adjust=bon;
  lsmeans type / pdiff adjust=tukey;
run;