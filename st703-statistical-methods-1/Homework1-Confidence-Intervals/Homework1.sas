options linesize=75 pagesize=60 pageno=1 nodate;
/**********************************
	Jimmy Hickey
	2019-08-28
	ST703 Homework 1
	Exercises from the textbook
		4.4 [p.145] part (a) only
		4.5 [p.146] part (b) only
		4.8 [p.147] part (d) only
		4.10 [p.148] part (b) only.
**********************************/

/**********************************
	4.4 part a
	sigma is unknown
	sample size is large enough
**********************************/

data hcl;
	infile datalines delimiter=',';
	input infection_rate @@;
	datalines;
		2.95, 2.11, 1.83, 2.06, 1.90, 1.95, 1.71, 1.98, 2.24, 1.59, 1.87, 1.93, 2.11, 1.96, 1.94, 1.17, 1.77, 1.98, 2.34, 2.18, 2.26, 1.69, 2.12, 2.18, 1.91, 1.28, 2.27, 2.13, 1.38, 2.03, 2.29, 1.74, 2.07, 1.25, 2.37
	;
run;

proc print data=hcl;
run;

* In order to check normailty, we can check the qq plot;

proc univariate data=hcl plots alpha=.10;
	var infection_rate;
	qqplot infection_rate / normal(mu=est sigma=est) square;
run;

* The qqplot shows that the data look fairly normal ;
* Construct a 90% confidence interval for mu and interpret the interval;
proc means n lclm uclm alpha=.10;
	var infection_rate;
	title "90% Confidence Interval for the infection rate";
run;
title;
* We are 90% confident that the true mean infection rate is in (1.8577546, 2.0588169) in 1000*infections/month.
title;


/**********************************
	4.5 part b
	sigma unknown 
	sample size = 16 is not large
**********************************/

data social_index;
	infile datalines delimiter=',';
	input index @@;
	datalines;
		48, 75, 69, 58, 60, 68, 59, 66, 71, 52, 49, 60, 54, 55, 70, 57
	;
run;

proc print data=social_index;
run;

* Let's check the qqplot to see if the data look normal;

proc univariate data=social_index plots alpha=.10;
	var index;
	qqplot index / normal(mu=est sigma=est) square;
run;

* The qqplot shows that the data look normal, so we will use that assumption;

* Construct a 90% confidence interval for mu and interpret the interval;
proc means data=social_index n lclm uclm alpha=.10;
	var index;
	title "90% Confidence Interval for the social perceptiveness index";
run;
title;

* We are 90% confident that the true mean index is in (57.0674095, 64.3075905
) points.;

/**********************************
		4.8 part (d) only
		independent data
		sigma unknown
		sample sizes are small
**********************************/

data pituiatry;
	input sex $2. distance;
	datalines;
F 23.0
F 25.5
F 26.0
F 26.5
F 23.5
F 22.5
F 25.0
F 24.0
F 21.5
F 19.5
F 28.0
M 31.0
M 26.5
M 27.5
M 27.0
M 26.0
M 28.5
M 26.5
M 25.5
M 26.0
M 31.5
M 25.0
M 28.0
M 29.5
M 26.0
M 30.0
M 25.0
	;
run;

proc print data=pituiatry;
run;

* We can use a t-test to construct our confidence interval in the difference in these samples ; 
proc ttest data=pituiatry alpha=.05;
	var distance;
	class sex;
run;

* The qqplots show that the data are fairly normal;
* Using the Pooled method, we are 95% sure that the true difference in mean between females and males is within (-5.1790, -1.5766) millimeters.;


/**********************************
	4.10 [p.148] part (b) only.
	Data are dependent
	Sample sizes are not large
	Sigma (for distribution of difference) unknown
**********************************/

data mice_buddies;
	input pair $ control treatment;
datalines;
1 1.321 0.841
2 1.432 0.932
3 2.682 2.011
4 0.934 0.762
5 1.230 0.991
6 1.670 1.120
7 3.201 2.312
;
run;

proc print data=mice_buddies;
run;

* Now we can get a 95% confidence interval; 
proc ttest data=mice_buddies alpha=0.05;
	paired control*treatment;
run;

* The qqplot suggests that the differences are fairly normal;
* We are 95% sure that the true differnce in mean between the control and treatment groups is within (0.2739 0.7264) grams.;