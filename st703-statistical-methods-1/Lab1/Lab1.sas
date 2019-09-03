options linesize=75 pagesize=60 pageno=1 nodate;
*Descriptive statistics for the alloy strength data;
/* There are multiple ways to indicate comments. */

data alloy;
	input strength @@;
	datalines;
		105 221 183 186 121 181 180 143
		97 154 153 174 120 168 167 141
		245 228 174 199 181 158 176 110
		163 131 154 115 160 208 158 133
		207 180 190 193 194 133 156 123
		134 178 76 167 184 135 229 146
		218 157 101 171 165 172 158 169
		199 151 142 163 145 171 148 158
		160 175 149 87 160 237 150 135
		196 201 200 176 150 170 118 149
;
proc print
	data=alloy; *data defaults to last created;
run; *this alerts SAS to execute previous commands;

/* Running proc means*/
proc means; run;

proc means var std stderr lclm uclm median;
	title "Compressive strength measurements (psi) of";
	title2 "aluminum-lithium alloy specimens";
	var strength;
run;

title;

proc means n mean min max var std stderr lclm uclm median alpha=.1;
	var strength;
run;

proc means n mean min max var std stderr lclm uclm median alpha=.1;
	where strength>130;
	var strength;
run;

/* 
	looking at the mercury contamination data set
*/

data mercury;
	input merc_level @@;
	datalines;
		1.230 0.490 0.490 1.080 0.590 0.280 0.180 0.100 0.940
		1.330 0.190 1.160 0.980 0.340 0.340 0.190 0.210 0.400
		0.040 0.830 0.050 0.630 0.340 0.750 0.040 0.860 0.430
		0.044 0.810 0.150 0.560 0.840 0.870 0.490 0.520 0.250
		1.200 0.710 0.190 0.410 0.500 0.560 1.100 0.650 0.270
		0.270 0.500 0.770 0.730 0.340 0.170 0.160 0.270
	;
run;

proc print data=mercury;
run;

* get 98% Confidence Interval for the mean;
proc means n lclm uclm alpha=.02;
	var merc_level;
run;

* Get 98% lower bound for the mean;
proc means n lclm alpha=.02;
	var merc_level;
run;


/*
	Reading data in from a file
*/


data alloy;
	infile '/folders/myfolders/ST703/data/AlloyStrength.txt';
	input strength @@;
run;

proc print;
run;

data alloy;
	infile '/folders/myfolders/ST703/data/AlloyStrength.txt' firstobs=3;
	input strength @@;
run;

proc print;
run;

/*
	Save data as permanent SAS datafile that can be directly accessed
*/

* make a library with the data;
libname st703 "/folders/myfolders/ST703/data";

* load the data;
data st703.alloy;
	infile '/folders/myfolders/ST703/data/AlloyStrength.txt';
	input strength @@;
run;

* now you can load the library and use the data;
libname st703 "/folders/myfolders/ST703/data";
proc print data=st703.alloy;


/*
	Using the univariate procedure
	gives summary statistics such as mean, stdev, etc.
*/

proc univariate data=st703.alloy;
	var strength;
run;

proc univariate data = st703.alloy plots cibasic alpha = 0.1;
	var strength;
	label strength = "Compressive Strength (psi)";
	histogram strength / normal kernel;
	inset n mean std stderr median / position=ne;
	qqplot strength / normal (mu=est sigma=est) square;
	qqplot strength / exponential (sigma = est theta = est) square;
run;

/* 
	Running a t-test
*/

proc ttest data=st703.alloy;
	var strength;
	label strength = "Compressive Strength (psi)";
run;

* get a 0.025 upper bound;
proc ttest data=st703.alloy sides=u alpha=0.025;
	var strength;
	label strength = "Compressive Strength (psi)";
run;

/*
	Comparing two groups
*/
data st703.arsenic;
  input city $17. conc type $;
datalines;
Phoenix           3  urban
Chandler          7  urban
Gilbert           25 urban
Glendale          10 urban
Mesa              15 urban
Paradise.Valley   6  urban
Peoria            12 urban
Scottsdale        25 urban
Tempe             15 urban
Sun.City          7  urban
Rimrock           48 rural
Goodyear          44 rural
New.River         40 rural
Apache.Junction   38 rural
Buckeye           33 rural
Nogales           21 rural
Black.Canyon.City 20 rural
Sedona            12 rural
Payson            1  rural
Casa.Grande       18 rural
; 
title "Arsenic Concentration in Drinking Water in Arizona"; title2;
proc print; run;

proc univariate data=st703.arsenic plots cibasic alpha=.02;
	var conc;
	
	* class type specifies the grouping;
	class type;
	
	label conc="Arsenic concentration (ppb)";
	histogram conc / normal kernel;
	inset n mean std stderr median / position=ne;
	qqplot conc / normal(mu=est sigma=est) square;
run;

proc means data=st703.arsenic n mean min max var std stderr lclm median alpha=.02;
	var conc; 
	class type;
run;

proc ttest data=st703.arsenic alpha=.05;
	var conc;
	class type;
	label conc="Arsenic concentration (ppb)";
run;

/*
	Paired data
*/

data st703.grip;
	infile "/folders/myfolders/ST703/data/GrippingStrength.txt" firstobs=2;
	input subj $ first $ dominant off diff;
run;

title "Arm gripping strength";
title2;

proc print; run;

proc sgplot data=st703.grip;
	reg x=dominant y=off; *"scatter x=dominant y=off" omits the regression line;
run;

proc ttest data=st703.grip alpha=0.05;
	paired dominant*off;
run;

/* 
	Common commands for popluation percentiles and probabilities
*/
data junk;
  z95=quantile('normal',.95);  *Pr( "n(0,1)" <= z95 )=.95  ;
  t975=quantile('t',.975,13);  *Pr( "t with 13 df" <= t975 )=.975 ;
  pp1=cdf('f',2.16037**2,1,13);*Pr( "F with 1 num & 13 den df" <= 2.16037**2 ) ;
  pp2=cdf('binom',2,.25,10);   *Pr( "bin(n=10,p=.25)" <= 2 ) ;
  pp3=cdf('chisq',3.841,1);    *Pr( "chi-square with 1 df" <= 3.841 ) ;
  run;
proc print; run;

/*
	Inference on proportions
*/

data ex1;
  input strengthlt100 $ count;
datalines;
yes 3
no  77
;
title "Alloy strength: estimate Pr(strength < 100)";
proc freq data=ex1 order=data;
  weight count;
  tables strengthlt100 /binomial(level='yes' cl=wald cl=ac cl=score cl=exact) alpha=.05 plots=freqplot;
  run;

data polio;
  input drug $ polio $ count;
datalines;
placebo yes 110
placebo no 201119
vaccine yes 33
vaccine no 200712
;
title "1954 Salk Polio Vaccine";
proc freq data=polio order=data;
  weight count;
  tables drug*polio / riskdiff(cl=(ac score));
  run;
