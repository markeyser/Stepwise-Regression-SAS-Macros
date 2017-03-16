*****************************************************************************************
* Program: forward_stepwise_v0.sas   											        *
* Program location:                                                                     *
* Created: 08/27/2015                                                                   *
* Purpose: The macro performs an automated stepwise model selection process for         *
* PROC GENMOD which does not come with model selection options. Note taht the GENMOD    *
* procedure in SAS versions prior to 9.4 does not come with model selection options.  	*
* Note: SAS� users may face situations where some "powerful" options are only available *
* in certain SAS� procedures but not available in others. For example, the model        *
* selection options are available in PROC REG, LOGISTIC, PHREG, etc., but not in        *
* PROC GENMOD, CATMOD, MIXED, etc. This backwards selection macro could be used with    *
* proc GENNMOD, CATMOD, MIXED, GLIMMIX, etc.                                            *
* Author: Marcos Aguilera Keyser.                                                       *
* References: PharmaSUG 2012 - SP09 Automated forward selection for Generalized Linear  *
* Models with Categorical and Numerical Variables using PROC GENMOD                     *
* Manuel Sandoval, Pharmanet-i3, Mexico City, Mexico                                    *
* http://www.pharmasug.org/proceedings/2012/SP/PharmaSUG-2012-SP09.pdf                  *
*****************************************************************************************;

/*
Dataset used in this code:
=========================
claim_history.sas7bdat
claim_history.csv

The claim_history.sas7bdat dataset comes from the help library of SAS Enterprise-Miner
versi�n 7.1

Data description:

Observations 10302
Variables 27


Variable
Number Name 	  Type 		Format Label Length
1      ID 		  Numeric   BEST   		 8
2      KIDSDRIV   Numeric   BEST         8
3      BIRTH 	  Numeric   DATE         8
4      AGE 		  Numeric   BEST         8
5      HOMEKIDS   Numeric   BEST         8
6      YOJ 		  Numeric   BEST         8
7      INCOME 	  Numeric   DOLLAR       8
8      PARENT1 	  Character $CHAR        3
9      HOME_VAL   Numeric   DOLLAR       8
10     MSTATUS 	  Character $CHAR        4
11     GENDER 	  Character $CHAR        3
12     EDUCATION  Character $CHAR        13
13     OCCUPATION Character $CHAR        13
14     TRAVTIME   Numeric   BEST         8
15     CAR_USE    Character $CHAR        10
16     BLUEBOOK   Numeric   DOLLAR       8
17     TIF 		  Numeric   BEST         8
18     CAR_TYPE   Character $CHAR        11
19     RED_CAR    Character $CHAR        3
20     OLDCLAIM   Numeric   DOLLAR       8
21     CLM_FREQ   Numeric   BEST         8
22     REVOKED    Character $CHAR        3
23     MVR_PTS 	  Numeric   BEST         8
24     CLM_AMT 	  Numeric   DOLLAR       8
25     CAR_AGE 	  Numeric   BEST         8
26     CLAIM_FLAG Numeric   BEST         8
27     URBANICITY Character $CHAR        21

Interesting target varaibles could be:

CLAIM_FLAG : binary variable 1 if calim occured and 0 otherwise
CLM_AMT: claim amount including 0
CLM_FRQ: number of claims takes the values 0,1,2,etc
EXPO: the exposition, I added this variable, takes the value 1 always

-For a Binomial (logistic model) the target should be CLAIM_FLAG
-For a Severity model using Gamma, Inverse Gaussian or Log Normal the target
 should be CLM_AMT
-For a Frequency model using Poisson or Negative Binomial and the Zero
 Inflated Poisson and Negative Binomal,the target should be CLM_FRQ
-For the Cost of Claim model (or risk premium)

*/

*
In the claim_history.sas7bdat SAS dataset there are enough observations and variables
to perform and interesting variable selection exercise.

*-------------------------------------------------------------------------*;
*STEPWISE SELECTON MACRO FOR PROC GENMOD;
*-------------------------------------------------------------------------*;

*
The first part of the macro is the construction of all single factor
models and saving their results in a dataset (aggregated), where those
with p-values greater than 0.2 could be easily filtered out
;

************************** MACRO PARAMETERS *******************************;

*Put the name of the modeling dataset:;

%let dataset = mydata.claim_history;

*Put here the name of the name of the target varible;

%let target = CLM_AMT;

*Put here the name of all the potential explanatory variables, the
categorical and the coninous:;

%let explanatory =
GENDER
CAR_USE
EDUCATION
CAR_TYPE
MSTATUS
OCCUPATION
RED_CAR
URBANICITY
REVOKED
PARENT1
AGE
BIRTH
BLUEBOOK
CAR_AGE
HOMEKIDS
HOME_VAL
INCOME
KIDSDRIV
MVR_PTS
TIF
TRAVTIME
YOJ
;

*Put the name of the character variables with commas:;

%let char=
"GENDER"       ,
"CAR_USE"      ,
"EDUCATION"    ,
"CAR_TYPE"     ,
"MSTATUS"      ,
"OCCUPATION"   ,
"RED_CAR"      ,
"URBANICITY"   ,
"REVOKED"      ,
"PARENT1"
;

*Put the name of the character variables with commas:;

%let char_2=
GENDER       ,
CAR_USE      ,
EDUCATION    ,
CAR_TYPE     ,
MSTATUS      ,
OCCUPATION   ,
RED_CAR      ,
URBANICITY   ,
REVOKED      ,
PARENT1
;

*Put the name of the continous variables. Include the target variable also:;

%let interval =
CLM_AMT
AGE
BIRTH
BLUEBOOK
CAR_AGE
HOMEKIDS
HOME_VAL
INCOME
KIDSDRIV
MVR_PTS
TIF
TRAVTIME
YOJ
;

*In order to minimize the number of models that needed to be run, a stepwise
selection model was created, considering susceptible for entry all those
variables with a p-value in a single model less than 0.2, and with a p-value
in the aggregated model of less than 0.25;

%let single_model=0.2;
%let aggregated_model=0.05;

*Set the power Tweedie distribution parameter;

%let power = 1.5;

************************** END MACRO PARAMETERS ****************************;


/* Creation of the Aggregated Data Set*/


data explanatory;
	set /*tmp1*/
	&dataset
		(keep= &explanatory);
run;

proc contents data = explanatory varnum nodetails noprint
	out=explanatory_names (keep=name);
run;

data explanatory_names;
	set explanatory_names;
	j = _n_;
run;

* Determine the number of observations;
data _NULL_;
	if 0 then
		set explanatory_names nobs=n;
	call symputx('nrows',n);
	stop;
run;

proc datasets lib = work nolist;
	delete pva;
run;

options minoperator mlogic mprint;
%macro agrega / mindelimiter=',';
	%do obs = 1 %to &nrows;

data _null_;
	set explanatory_names;

	if j = &obs then
		call symputx("var", put(name, 30.));
run;

%if %upcase(&var) in(&char_2) %then
	%do;
		ods output Type3=pva&obs (rename=source=parm keep=source ProbChiSq);

		proc genmod data=&dataset NAMELEN=50;
			if _resp_ > 0 then
				d = 2*(_resp_*(_resp_**(1-&power)-_mean_**(1-&power))/
					  (1-&power)-(_resp_**(2-&power)-_mean_**(2-&power))/
					  (2-&power));
			else d = 2* _mean_**(2-&power)/(2-&power);
			variance var = _mean_**&power;
			deviance dev = d;
			class &var;
			model &target =  &var / link=log type3 scale=pearson;

			*scwgt expos;
			title "&var";
		run;

	%end;
%else
	%do;
		ods output Type3=pva&obs (rename=source=parm keep=source ProbChiSq);

		proc genmod data=&dataset NAMELEN=50;
			if _resp_ > 0 then
				d = 2*(_resp_*(_resp_**(1-&power)-_mean_**(1-&power))/
					  (1-&power)-(_resp_**(2-&power)-_mean_**(2-&power))/
					  (2-&power));
			else d = 2* _mean_**(2-&power)/(2-&power);
			variance var = _mean_**&power;
			deviance dev = d;
			model &target =  &var / link=log type3 scale=pearson;

			*scwgt expos;
			title "&var";
		run;

	%end;

data pva&obs;
	length parm $30;
	set pva&obs;
run;

proc append base=pva data=pva&obs force;
run;

	%end;
%mend agrega;

%agrega;

ods select all;

proc sort data=pva out=aggregate;
	by ProbChiSq;
run;

data elegibles;
	set aggregate;
	where probchisq le &single_model;
run;

proc sort data = elegibles;
	by probchisq;
run;

data elegibles;
	set elegibles;
	rename parm = source;
run;

title "Aggregate (all potential explanatory variables)";

proc print data=aggregate;
run;

title "Elegibles (individual model threshold = &single_model)";

proc print data=elegibles;
run;

/* STEPWISE PROCESS */

%let i = 1;

proc sql noprint;
	select count(source) into: totalfactor from elegibles;
quit;
* Also, the necessary set of macro variables needed to be initialized
as null, as the variables had to exist in order to be used in the rest
of the code;

%let catvar=;
%let invars=;
%let tempvar=;
%let tempcat=;

*options NOSYNTAXCHECK;/*Use this oiption before the last macro called
ONE only if you are sure that the failure of the macro is due to a lack
of convergence problem*/
options mlogic;

%macro one;
	%do %while (&i le &totalfactor);

data elegibles;
	set elegibles;

	if _n_ = 1 then
		do;
			call symput('testvar',source);

			if upcase(source) in (&char) then
				do;
					call symput('catvar_t',source);
				end;
			else
				do;
					call symput('catvar_t','');
				end;

			delete;
		end;
run;

title "Step &i";
title2 "Elegibles (aggregated model threshold = &aggregated_model)";
ods output parameterestimates = parms type3 = type3;

proc genmod data=&dataset NAMELEN=50;
	if _resp_ > 0 then
		d = 2*(_resp_*(_resp_**(1-&power)-_mean_**(1-&power))/
			  (1-&power)-(_resp_**(2-&power)-_mean_**(2-&power))/
			  (2-&power));
	else d = 2* _mean_**(2-&power)/(2-&power);
	variance var = _mean_**&power;
	deviance dev = d;
	class &catvar &catvar_t;
	model &target = &invars &testvar / link=log type3 scale=pearson;

	*scwgt expos;
run;

data stay;
	set type3;

	If source = "&testvar" and probchisq le &aggregated_model then
		do;
			call symput ('invars',"&tempvar"||" "||strip("&testvar"));
			call symput ('catvar',"&tempcat"||" "||strip("&catvar_t"));
		end;
	else if source = "&testvar" then
		delete;
run;

%let tempvar = &invars;
%let tempcat = &catvar;
%let i = %eval(&i+1);
	%end;
%mend one;

%one;

title "Selected Variables";
proc print data=stay;
run;


proc datasets lib=work kill nolist;
run;


/*
After the process was finished, the resulting model would need to be run one
more time, as two problems could arise.

First, the last factor considered could be not significant, and so, the model
would need to be rerun to have the pvalues
without that variable.

Also, if the last model did not converge, the model would also need to be
rerun to have the values of the next to last model.
*/
