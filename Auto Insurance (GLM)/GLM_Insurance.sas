%let PATH = /home/saranyanvasudeva0/my_courses/donald.wedding/c_8888/PRED411/UNIT02/HW;
%let NAME = mydata;

title 'Predict 411 Winter 2018 Sec 58 Unit 2 Assignment 1';
libname &NAME. "&PATH.";

%let INFILE = &NAME..LOGIT_INSURANCE;
%let STGFILE0 = logit_insurance_stg0;
%let STGFILE0_out = logit_insurance_Out;
%let STGFILE0_wide = insurance_Wide;
%let STGFILE0_long = insurance_Long;
%let STGFILE1 = logit_insurance_stg1;
%let STGFILE2 = logit_insurance_stg2;
%let STGFILE2_SORT = logit_insurance_stg2_sorted;
%let STGFILE3 = logit_insurance_stg3;

data &STGFILE0.;
	set &INFILE.;
run;

proc means data=&STGFILE0. N Mean Median StdDev	Min	Max P5 P95 NMISS STACKODSOUTPUT;
ods output Summary=&STGFILE0_out.;
run;

proc freq data=&STGFILE0.;
*tables CAR_TYPE CAR_USE EDUCATION JOB MSTATUS PARENT1 RED_CAR REVOKED SEX URBANICITY / NOCOL NOFREQ NOPERCENT;
tables CAR_USE*target_flag;
run;


proc sql noprint;                              
 select Variable into :varlist separated by ' '
 from &STGFILE0_out.
 where Variable not in ('INDEX','TARGET_FLAG','TARGET_AMT');
quit;

data &STGFILE0_wide. / view=&STGFILE0_wide.;     
retain &varlist;
set &STGFILE0.;
obsNum = _N_;
keep obsNum &varlist;
run;


proc transpose data=&STGFILE0_wide. name=VarName
out=&STGFILE0_Long.(rename=(Col1=_Value_) drop=_LABEL_);
by obsnum;
var OLDCLAIM BLUEBOOK ;
run;

title "Box Plots";
proc sgplot data=&STGFILE0_Long.;
   label _Value_ = "Standardized Value" VarName="Variable";
   vbox _Value_ / category=VarName;
run;


proc transpose data=&STGFILE0_wide. name=VarName
out=insurance_Long(rename=(Col1=_Value_) drop=_LABEL_);
by obsnum;
var INCOME HOME_VAL;
run;

title "Box Plots";
proc sgplot data=&STGFILE0_Long.;
   label _Value_ = "Standardized Value" VarName="Variable";
   vbox _Value_ / category=VarName;
*   xaxis discreteorder=data display=(nolabel);        /* respect ordering */
run;


proc corr data=&STGFILE0. pearson spearman rank outp=insurance_corr_Out plots(MAXPOINTS=NONE)=matrix(HIST);
var _numeric_;
with TARGET_FLAG;
run;

proc freq data=&STGFILE0.;
tables CAR_TYPE * TARGET_FLAG; 
run;


proc univariate normal plot data=&STGFILE0. noprint;
histogram BLUEBOOK INCOME HOME_VAL TRAVTIME BLUEBOOK OLDCLAIM/ normal;
*histogram BLUEBOOK / normal;
run;quit;


/* Correlation Matrix */
proc corr data=&STGFILE0. noprob outp=OutCorr 
          nomiss /** listwise deletion of missing values **/
          cov;   /**  include covariances **/
var BLUEBOOK INCOME HOME_VAL TRAVTIME OLDCLAIM;
run;
/* Correlation Matrix */

/* R Code
setwd("C:/Users/Saranyan/Dropbox/MSPA/Generalized Linear Models/Assignment 2")
insurance<-read.csv("logit_insurance.csv", header=TRUE)
library(rattle);
*/


/* IMPUTE WITH VALUES AND SET INDICATOR FROM DECISION TREE USING RATTLE */
DATA &STGFILE1.;
SET &STGFILE0.;
CAR_AGE_IMP=CAR_AGE;
CAR_AGE_IMP_IND=0;
HOME_VAL_IMP=HOME_VAL;
HOME_VAL_IMP_IND=0;
INCOME_IMP=INCOME;
INCOME_IMP_IND=0;
YOJ_IMP=YOJ;
YOJ_IMP_IND=0;
AGE_IMP=AGE;
AGE_IMP_IND=0;
JOB_IMP=JOB;
JOB_IMP_IND=0;

IF MISSING(AGE) THEN DO;
AGE_IMP_IND=1;
IF HOMEKIDS >= 0.5 AND KIDSDRIV < 0.5 AND YOJ < 14 AND HOME_VAL < 160000 THEN AGE_IMP=35;
ELSE IF HOMEKIDS >= 0.5 AND KIDSDRIV < 0.5 AND YOJ < 14 AND HOME_VAL >= 160000 THEN AGE_IMP=39;
ELSE IF HOMEKIDS >= 0.5 AND KIDSDRIV < 0.5 AND YOJ >= 14 THEN AGE_IMP=42;
ELSE IF HOMEKIDS >= 0.5 AND KIDSDRIV >= 0.5 THEN AGE_IMP=43;
ELSE IF HOMEKIDS < 0.5 AND YOJ < 12 AND JOB IN ('CLERICAL','PROFESSIONAL','STUDENT','z_Blue Collar') AND HOME_VAL < 153000 THEN AGE_IMP=44;
ELSE IF HOMEKIDS < 0.5 AND YOJ < 12 AND JOB IN ('CLERICAL','PROFESSIONAL','STUDENT','z_Blue Collar') AND HOME_VAL >= 153000 THEN AGE_IMP=47;
ELSE IF HOMEKIDS < 0.5 AND YOJ < 12 AND JOB NOT IN ('CLERICAL','PROFESSIONAL','STUDENT','z_Blue Collar') THEN AGE_IMP=48;
ELSE IF HOMEKIDS < 0.5 AND YOJ >= 12 THEN AGE_IMP=51;
END;

IF MISSING(CAR_AGE) THEN DO;
CAR_AGE_IMP_IND=1;
IF BLUEBOOK < 13000 THEN CAR_AGE_IMP=7.2;
ELSE IF BLUEBOOK >= 13000 THEN CAR_AGE_IMP=9.2;
END;

IF MISSING(HOME_VAL) THEN DO;
HOME_VAL_IMP_IND=1;
IF MSTATUS='z_No' AND INCOME < 75000 THEN HOME_VAL_IMP=58000;
ELSE IF MSTATUS='z_No' AND INCOME >= 75000 THEN HOME_VAL_IMP=130000;
ELSE IF MSTATUS NOT IN ('z_No') AND INCOME < 21000 AND JOB = 'STUDENT' THEN HOME_VAL_IMP=19000;
ELSE IF MSTATUS NOT IN ('z_No') AND INCOME < 21000 AND JOB NOT IN ('STUDENT') THEN HOME_VAL_IMP=104000;
ELSE IF MSTATUS NOT IN ('z_No') AND INCOME < 64000 AND INCOME >= 21000 THEN HOME_VAL_IMP=174000;
ELSE IF MSTATUS NOT IN ('z_No') AND INCOME >= 64000 AND INCOME < 91000 THEN HOME_VAL_IMP=242000;
ELSE IF MSTATUS NOT IN ('z_No') AND INCOME < 130000 AND INCOME >= 91000 THEN HOME_VAL_IMP=305000;
ELSE IF MSTATUS NOT IN ('z_No') AND INCOME >= 130000 THEN HOME_VAL_IMP=433000;
END;

IF MISSING(INCOME) THEN DO;
INCOME_IMP_IND=1;
IF JOB IN ('Home Maker', 'Student') THEN INCOME_IMP=9052;
ELSE IF JOB='Clerical' THEN INCOME_IMP=34000;
ELSE IF JOB NOT IN ('Clerical', 'Home Maker', 'Student') AND EDUCATION IN ('<High School', 'z_High School') THEN INCOME_IMP=52000;
ELSE IF JOB NOT IN ('Clerical', 'Home Maker', 'Student') AND EDUCATION IN ('Bachelors', 'Masters') THEN INCOME_IMP=82000;
ELSE IF JOB NOT IN ('Clerical', 'Home Maker', 'Student') AND EDUCATION NOT IN ('<High School', 'z_High School', 'Bachelors', 'Masters') THEN INCOME_IMP=131000;
END;

IF MISSING(JOB) THEN DO;
JOB_IMP_IND=1;
IF EDUCATION = 'PhD' THEN JOB_IMP='Doctor';
ELSE IF EDUCATION = 'Masters' THEN JOB_IMP='Lawyer';
ELSE IF EDUCATION NOT IN ('PhD', 'Masters') AND INCOME < 15000 THEN JOB_IMP='Student';
ELSE IF EDUCATION NOT IN ('PhD', 'Masters') AND INCOME >= 15000 AND INCOME < 45000 THEN JOB_IMP='Clerical';
ELSE IF EDUCATION NOT IN ('PhD', 'Masters') AND INCOME >= 45000 AND EDUCATION = 'Bachelors' THEN JOB_IMP='Professional';
ELSE IF EDUCATION NOT IN ('PhD', 'Masters') AND INCOME >= 45000 AND EDUCATION <> 'Bachelors' THEN JOB_IMP='z_Blue Collar';
END;

IF MISSING(YOJ) THEN DO;
YOJ_IMP_IND=1;
IF INCOME < 2.5 THEN YOJ_IMP=0;
ELSE IF INCOME > 2.5 AND AGE < 56 AND HOME_VAL < 25000 THEN YOJ_IMP=11;
ELSE IF INCOME > 2.5 AND AGE < 56 AND HOME_VAL >= 25000 AND JOB IN ('Doctor', 'Home Maker', 'Manager', 'Professional') THEN YOJ_IMP=11;
ELSE IF INCOME > 2.5 AND AGE < 56 AND HOME_VAL >= 25000 AND JOB NOT IN ('Doctor', 'Home Maker', 'Manager', 'Professional') THEN YOJ_IMP=12;
ELSE IF INCOME > 2.5 AND AGE >= 56 THEN YOJ_IMP=13;
END;

IF HOME_VAL_IMP=0 THEN RENTED_HOME_FLAG=1;
else RENTED_HOME_FLAG=0;


RUN;



proc univariate normal plot data=&STGFILE1. noprint;
histogram BLUEBOOK / normal;
run;quit;

proc univariate normal plot data=&STGFILE1. noprint;
histogram INCOME_IMP / normal;
run;quit;

proc univariate normal plot data=&STGFILE1. noprint;
histogram HOME_VAL_IMP / normal;
run;quit;

proc univariate normal plot data=&STGFILE1. noprint;
histogram TRAVTIME / normal;
run;quit;

/* Capping Observations */
data &STGFILE2.;
set &STGFILE1.;
if BLUEBOOK < 1500 then BLUEBOOK=1500;
else if BLUEBOOK > 39090 then BLUEBOOK=39090;

if INCOME_IMP > 214653 then INCOME_IMP=214653;

if HOME_VAL_IMP > 497746 then HOME_VAL_IMP=497746;

if TRAVTIME < 5 then TRAVTIME=5;
else if TRAVTIME > 75.14 then TRAVTIME=75.14;
run;



proc freq data=&STGFILE1.;
tables URBANICITY*TARGET_FLAG;
run;

/*
data training_set;
set logit_insurance_stg2;
if ranuni(123) <= .70 then output; 
run;


data validation_set;
set logit_insurance_stg2;
if ranuni(123) > .70 then output; 
run;
*/

/* Check for transformational needs */
data &STGFILE2.;
set &STGFILE2.;
if HOME_VAL_IMP=0 then log_HOME_VAL_IMP=0;
else log_HOME_VAL_IMP = log(HOME_VAL_IMP); 
if INCOME_IMP=0 then log_INCOME_IMP=0;
else log_INCOME_IMP = log(INCOME_IMP); 
drop YOJ INCOME HOME_VAL CAR_AGE AGE JOB;
run;



/* Model 1 */
proc logistic data=&STGFILE2. plot(only)=(roc(ID=prob)) descending;
class _character_ /param=ref;
model TARGET_FLAG( ref="0" ) = 
					AGE_IMP
					BLUEBOOK
					CAR_AGE_IMP
					CAR_TYPE
					CAR_USE 
					CLM_FREQ
					EDUCATION
					HOME_VAL_IMP
					RENTED_HOME_FLAG
					/*HOMEKIDS*/
					/*INCOME_IMP*/
					JOB_IMP
					KIDSDRIV
					MSTATUS
					MVR_PTS
					OLDCLAIM
					PARENT1
					/*RED_CAR*/
					REVOKED
					SEX
					TIF
					TRAVTIME
					URBANICITY
					YOJ_IMP
					/roceps=0.1
					;
output out=model1_out p=prob lower=lowprob upper=upperprob;
roc;
run;


/* Model 1.1 */
proc logistic data=&STGFILE2. plot(only)=(roc(ID=prob)) descending;
class _character_ /param=ref;
model TARGET_FLAG = 
					AGE_IMP
					BLUEBOOK
					CAR_AGE_IMP
					CAR_TYPE
					CAR_USE 
					CLM_FREQ
					EDUCATION
					log_HOME_VAL_IMP
					RENTED_HOME_FLAG
					/*HOMEKIDS
					log_INCOME_IMP*/
					JOB_IMP
					KIDSDRIV
					MSTATUS
					MVR_PTS
					OLDCLAIM
					PARENT1
					/*RED_CAR*/
					REVOKED
					SEX
					TIF
					TRAVTIME
					URBANICITY
					YOJ_IMP
					/roceps=0.1
					;
output out=model1_1_out p=prob lower=lowprob upper=upperprob;
roc;
run;


/* Model 2 */
proc logistic data=&STGFILE2. plot(only)=(roc(ID=prob)) descending;
class _character_ /param=ref;
model TARGET_FLAG = 
					AGE_IMP
					BLUEBOOK
					CAR_AGE_IMP
					CAR_TYPE
					CAR_USE 
					CLM_FREQ
					EDUCATION
					HOME_VAL_IMP
					RENTED_HOME_FLAG
					/*HOMEKIDS
					log_INCOME_IMP*/
					JOB_IMP
					KIDSDRIV
					MSTATUS
					MVR_PTS
					OLDCLAIM
					PARENT1
					/*RED_CAR*/
					REVOKED
					SEX
					TIF
					TRAVTIME
					URBANICITY
					YOJ_IMP
					/selection=stepwise slentry=0.05 slstay=0.05 roceps=0.1
					;
output out=model2_out p=prob lower=lowprob upper=upperprob;
roc;
run;


* KS Statistic  *;
proc npar1way data=model2_out edf;
    class target_flag;
    var prob;
run;





proc sort data=&STGFILE2. out=&STGFILE2_SORT.;
by DESCENDING TARGET_FLAG;
run;

/* Model 3 */
ods output parameterestimates=pe;

proc probit data=&STGFILE2_SORT. PLOTS=ALL ORDER=DATA;
class _character_;
model TARGET_FLAG = 
					AGE_IMP
					BLUEBOOK
					CAR_AGE_IMP
					CAR_TYPE
					CAR_USE 
					CLM_FREQ
					EDUCATION
					HOME_VAL_IMP
					RENTED_HOME_FLAG
					/*HOMEKIDS
					log_INCOME_IMP*/
					JOB_IMP
					KIDSDRIV
					MSTATUS
					MVR_PTS
					OLDCLAIM
					PARENT1
					/*RED_CAR*/
					REVOKED
					SEX
					TIF
					TRAVTIME
					URBANICITY
					YOJ_IMP
					;
output out=model3_out p=prob;
*roc;
run;

proc print data=pe label; 
id parameter Level1;
format estimate 12.10 
     probt    pvalue12.10; 
var _numeric_;
title "Parameter Estimates";
run;


* KS Statistic  *;
proc npar1way data=model3_out edf;
    class target_flag;
    var prob;
run;


/* Model 3.1 - With Probit Link Function */
proc logistic data=&STGFILE2. plot(only)=(roc(ID=prob)) descending;
class _character_/param=ref;
model TARGET_FLAG = 
					AGE_IMP
					BLUEBOOK
					CAR_AGE_IMP
					CAR_TYPE
					CAR_USE 
					CLM_FREQ
					EDUCATION
					HOME_VAL_IMP
					RENTED_HOME_FLAG
					/*HOMEKIDS
					log_INCOME_IMP*/
					JOB_IMP
					KIDSDRIV
					MSTATUS
					MVR_PTS
					OLDCLAIM
					PARENT1
					/*RED_CAR*/
					REVOKED
					SEX
					TIF
					TRAVTIME
					URBANICITY
					YOJ_IMP
					/link=probit roceps=0.1
					;
output out=model3_1_out p=prob lower=lowprob upper=upperprob;
roc;
run;

* KS Statistic *;
proc npar1way data=model3_1_out edf;
    class target_flag;
    var prob;
run;


/* Gain Chart */
/*
proc rank data=model1_out_sort out=model1_out_rank groups=10 descending;
	var prob;
	ranks RANK;
run;


proc means data=model1_out_rank1 noprint;
	class rank;
	output out=model1_out_mean sum(TARGET_FLAG)=TARGET_FLAG min(prob)=CUTOFF;
run;


proc sql;
select 
rank
, _freq_
, (select sum(_freq_) from model1_out_mean as inner where inner.rank <= outer.rank) as cum_freq
, tp
, (select sum(tp) from model1_out_mean as inner where inner.rank <= outer.rank) as pos_resp
from model1_out_mean as outer
group by rank, _freq_, tp;
run;

*/


/* Bingo Bonus */


/* TARGET_AMT Prediction */
data &STGFILE3.;
set &STGFILE2.;
CAR_TYPE_Minivan=0;
CAR_TYPE_Panel=0;
CAR_TYPE_Pickup=0;
CAR_TYPE_Sports=0;
CAR_TYPE_SUV=0;
EDUCATION_Bachelors=0;
EDUCATION_Masters=0;
EDUCATION_PhD=0;
EDUCATION_High_School=0;
EDUCATION_zHigh_School=0;
JOB_IMP_Home_Maker=0;
JOB_IMP_Masters=0;
JOB_IMP_Lawyer=0;
JOB_IMP_Manager=0;
JOB_IMP_Professional=0;
JOB_IMP_Student=0;
JOB_IMP_Blue_Collar=0;
JOB_IMP_Clerical=0;
JOB_IMP_Doctor=0;
/*
if CAR_TYPE="Minivan" then CAR_TYPE_Minivan=1;
else if CAR_TYPE="Panel Truck" then CAR_TYPE_Panel=1;
else if CAR_TYPE="Pickup" then CAR_TYPE_Pickup=1;
else if CAR_TYPE="Sports Car" then CAR_TYPE_Sports=1;
else if CAR_TYPE="Van" then CAR_TYPE_Van=1;
else if CAR_TYPE="z_SUV" then CAR_TYPE_SUV=1;
*/
if CAR_USE="Commercial" then CAR_USE_FLAG=1;
else if CAR_USE="Private" then CAR_USE_FLAG=0;

if EDUCATION="<High School" then EDUCATION_High_School=1;
else if EDUCATION="z_High School" then EDUCATION_zHigh_School=1;
else if EDUCATION="Bachelors" then EDUCATION_Bachelors=1;
else if EDUCATION="Masters" then EDUCATION_Masters=1;
else if EDUCATION="PhD" then EDUCATION_PhD=1;

if JOB_IMP="Clerical" then JOB_IMP_Clerical=1;
else if JOB_IMP="Doctor" then JOB_IMP_Doctor=1;
else if JOB_IMP="Home Maker" then JOB_IMP_Home_Maker=1;
else if JOB_IMP="Lawyer" then JOB_IMP_Lawyer=1;
else if JOB_IMP="Manager" then JOB_IMP_Manager=1;
else if JOB_IMP="Professional" then JOB_IMP_Professional=1;
else if JOB_IMP="Student" then JOB_IMP_Student=1;
else if JOB_IMP="z_Blue Collar" then JOB_IMP_Blue_Collar=1;


if MSTATUS="Yes" then MSTATUS_FLAG=1;
else if MSTATUS="z_No" then MSTATUS_FLAG=0;

if PARENT1="Yes" then PARENT1_FLAG=1;
else if PARENT1="No" then PARENT1_FLAG=0;

if REVOKED="Yes" then REVOKED_FLAG=1;
else if REVOKED="No" then REVOKED_FLAG=0;

if SEX="M" then SEX_FLAG=1;
else if SEX="z_F" then SEX_FLAG=0;

if URBANICITY="Highly Urban/ Urban" then URBANICITY_FLAG=1;
else if URBANICITY="z_Highly Rural/ Rural" then URBANICITY_FLAG=0;

if RED_CAR="yes" then RED_CAR_FLAG=1;
else if RED_CAR="no" then RED_CAR_FLAG=0;

if HOME_VAL_IMP=0 then log_HOME_VAL_IMP=0;
else log_HOME_VAL_IMP = log(HOME_VAL_IMP); 

if INCOME_IMP=0 then log_INCOME_IMP=0;
else log_INCOME_IMP = log(INCOME_IMP); 

run;

/* Split training datset by type of Vehicle used */


%let STG_SUV = logit_insurance_stg3;
%let STG_SPORTS = logit_insurance_stg3;
%let STG_VAN = logit_insurance_stg3;
%let STG_TRUCK = logit_insurance_stg3;
%let STG_PICKUP = logit_insurance_stg3;



data &STG_SUV.;
set &STGFILE3.;
if CAR_TYPE in ("z_SUV");
if TARGET_AMT > 0;
run;

data &STG_SPORTS.;
set &STGFILE3.;
if CAR_TYPE in ("Sports Car");
if TARGET_AMT > 0;
run;


data &STG_VAN.;
set &STGFILE3.;
if CAR_TYPE in ("Van","Minivan");
if TARGET_AMT > 0;
run;


data &STG_TRUCK.;
set &STGFILE3.;
if CAR_TYPE in ("Panel Truck");
if TARGET_AMT > 0;
run;


data &STG_PICKUP.;
set &STGFILE3.;
if CAR_TYPE in ("Pickup");
if TARGET_AMT > 0;
run;


proc reg data=&STG_SUV. PLOTS=(DIAGNOSTICS(UNPACK) RESIDUALBYPREDICTED QQPLOT OBSERVEDBYPREDICTED);
model TARGET_AMT=
				AGE_IMP
				BLUEBOOK
				CAR_AGE_IMP
				CAR_USE_FLAG
				CLM_FREQ
				EDUCATION_Bachelors
				EDUCATION_High_School
				EDUCATION_zHigh_School
				EDUCATION_Masters
				EDUCATION_PhD
				/*HOMEKIDS*/
				HOME_VAL_IMP
				/*INCOME_IMP*/
				JOB_IMP_Home_Maker
				JOB_IMP_Blue_Collar
				JOB_IMP_Clerical
				JOB_IMP_Doctor
				JOB_IMP_Lawyer
				JOB_IMP_Manager
				JOB_IMP_Masters
				JOB_IMP_Professional
				JOB_IMP_Student
				KIDSDRIV
				MSTATUS_FLAG
				MVR_PTS
				OLDCLAIM
				PARENT1_FLAG
				/*RED_CAR_FLAG*/
				RENTED_HOME_FLAG
				REVOKED_FLAG
				SEX_FLAG
				TIF
				TRAVTIME
				URBANICITY_FLAG
				YOJ_IMP
/*				log_HOME_VAL_IMP
				log_INCOME_IMP*/
				/ selection=stepwise slentry=0.05 slstay=0.05 adjrsq AIC BIC VIF;
run;


proc reg data=&STG_SPORTS. PLOTS=(DIAGNOSTICS(UNPACK) RESIDUALBYPREDICTED QQPLOT OBSERVEDBYPREDICTED);
model TARGET_AMT=
				AGE_IMP
				BLUEBOOK
				CAR_AGE_IMP
				CAR_USE_FLAG
				CLM_FREQ
				EDUCATION_Bachelors
				EDUCATION_High_School
				EDUCATION_zHigh_School
				EDUCATION_Masters
				EDUCATION_PhD
				/*HOMEKIDS*/
				HOME_VAL_IMP
				/*INCOME_IMP*/
				JOB_IMP_Home_Maker
				JOB_IMP_Blue_Collar
				JOB_IMP_Clerical
				JOB_IMP_Doctor
				JOB_IMP_Lawyer
				JOB_IMP_Manager
				JOB_IMP_Masters
				JOB_IMP_Professional
				JOB_IMP_Student
				KIDSDRIV
				MSTATUS_FLAG
				MVR_PTS
				OLDCLAIM
				PARENT1_FLAG
				/*RED_CAR_FLAG*/
				RENTED_HOME_FLAG
				REVOKED_FLAG
				SEX_FLAG
				TIF
				TRAVTIME
				URBANICITY_FLAG
				YOJ_IMP
/*				log_HOME_VAL_IMP
				log_INCOME_IMP*/
				/ selection=stepwise slentry=0.05 slstay=0.05 adjrsq AIC BIC VIF;
run;


proc reg data=&STG_VAN. PLOTS=(DIAGNOSTICS(UNPACK) RESIDUALBYPREDICTED QQPLOT OBSERVEDBYPREDICTED);
model TARGET_AMT=
				AGE_IMP
				BLUEBOOK
				CAR_AGE_IMP
				CAR_USE_FLAG
				CLM_FREQ
				EDUCATION_Bachelors
				EDUCATION_High_School
				EDUCATION_zHigh_School
				EDUCATION_Masters
				EDUCATION_PhD
				/*HOMEKIDS*/
				HOME_VAL_IMP
				/*INCOME_IMP*/
				JOB_IMP_Home_Maker
				JOB_IMP_Blue_Collar
				JOB_IMP_Clerical
				JOB_IMP_Doctor
				JOB_IMP_Lawyer
				JOB_IMP_Manager
				JOB_IMP_Masters
				JOB_IMP_Professional
				JOB_IMP_Student
				KIDSDRIV
				MSTATUS_FLAG
				MVR_PTS
				OLDCLAIM
				PARENT1_FLAG
				/*RED_CAR_FLAG*/
				RENTED_HOME_FLAG
				REVOKED_FLAG
				SEX_FLAG
				TIF
				TRAVTIME
				URBANICITY_FLAG
				YOJ_IMP
/*				log_HOME_VAL_IMP
				log_INCOME_IMP*/
				/ selection=stepwise slentry=0.05 slstay=0.05 adjrsq AIC BIC VIF;
run;


proc reg data=&STG_TRUCK. PLOTS=(DIAGNOSTICS(UNPACK) RESIDUALBYPREDICTED QQPLOT OBSERVEDBYPREDICTED);
model TARGET_AMT=
				AGE_IMP
				BLUEBOOK
				CAR_AGE_IMP
				CAR_USE_FLAG
				CLM_FREQ
				EDUCATION_Bachelors
				EDUCATION_High_School
				EDUCATION_zHigh_School
				EDUCATION_Masters
				EDUCATION_PhD
				/*HOMEKIDS*/
				HOME_VAL_IMP
				/*INCOME_IMP*/
				JOB_IMP_Home_Maker
				JOB_IMP_Blue_Collar
				JOB_IMP_Clerical
				JOB_IMP_Doctor
				JOB_IMP_Lawyer
				JOB_IMP_Manager
				JOB_IMP_Masters
				JOB_IMP_Professional
				JOB_IMP_Student
				KIDSDRIV
				MSTATUS_FLAG
				MVR_PTS
				OLDCLAIM
				PARENT1_FLAG
				/*RED_CAR_FLAG*/
				RENTED_HOME_FLAG
				REVOKED_FLAG
				SEX_FLAG
				TIF
				TRAVTIME
				URBANICITY_FLAG
				YOJ_IMP
/*				log_HOME_VAL_IMP
				log_INCOME_IMP*/
				/ selection=stepwise slentry=0.05 slstay=0.05 adjrsq AIC BIC VIF;
run;



proc reg data=&STG_PICKUP. PLOTS=(DIAGNOSTICS(UNPACK) RESIDUALBYPREDICTED QQPLOT OBSERVEDBYPREDICTED);
model TARGET_AMT=
				AGE_IMP
				BLUEBOOK
				CAR_AGE_IMP
				CAR_USE_FLAG
				CLM_FREQ
				EDUCATION_Bachelors
				EDUCATION_High_School
				EDUCATION_zHigh_School
				EDUCATION_Masters
				EDUCATION_PhD
				/*HOMEKIDS*/
				HOME_VAL_IMP
				/*INCOME_IMP*/
				JOB_IMP_Home_Maker
				JOB_IMP_Blue_Collar
				JOB_IMP_Clerical
				JOB_IMP_Doctor
				JOB_IMP_Lawyer
				JOB_IMP_Manager
				JOB_IMP_Masters
				JOB_IMP_Professional
				JOB_IMP_Student
				KIDSDRIV
				MSTATUS_FLAG
				MVR_PTS
				OLDCLAIM
				PARENT1_FLAG
				/*RED_CAR_FLAG*/
				RENTED_HOME_FLAG
				REVOKED_FLAG
				SEX_FLAG
				TIF
				TRAVTIME
				URBANICITY_FLAG
				YOJ_IMP
/*				log_HOME_VAL_IMP
				log_INCOME_IMP*/
				/ selection=stepwise slentry=0.05 slstay=0.05 adjrsq AIC BIC VIF;
run;



/* BINGO BONUS - PROC GENMOD */

proc genmod data=&STGFILE2_SORT. ORDER=DATA;
class _character_ / param=effect;
model TARGET_FLAG = AGE_IMP
					BLUEBOOK
					CAR_AGE_IMP
					CAR_TYPE
					CAR_USE 
					CLM_FREQ
					EDUCATION
					HOME_VAL_IMP
					RENTED_HOME_FLAG
					/*HOMEKIDS
					log_INCOME_IMP*/
					JOB_IMP
					KIDSDRIV
					MSTATUS
					MVR_PTS
					OLDCLAIM
					PARENT1
					/*RED_CAR*/
					REVOKED
					SEX
					TIF
					TRAVTIME
					URBANICITY
					YOJ_IMP /*/ dist=binomial link=logit;*/
					;
output out=genmod_out p=prob;
*roc;
run;



