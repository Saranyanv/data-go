%let PATH = /home/saranyanvasudeva0/my_courses/donald.wedding/c_8888/PRED411/UNIT03/HW;
%let NAME = mydata;

title 'Predict 411 Winter 2018 Sec 58 Unit 3 Assignment 1';
libname &NAME. "&PATH.";

%let INFILE = &NAME..WINE;
%let STGFILE0 = wine_stg0;

data &STGFILE0.;
set &INFILE.;
run;

proc means data=&STGFILE0. N Mean Median StdDev	Min	Max P5 P95 NMISS VAR STACKODSOUTPUT;
variable _numeric_;
ods output Summary=&STGFILE0._out;
run;

proc print data=&STGFILE0._out;
run;



proc univariate data=&STGFILE0. noprint;
histogram TARGET / midpoints = 0 to 8 by 1;
run;


proc univariate data=&STGFILE0. noprint;
var FixedAcidity CitricAcid ResidualSugar;
output out=PCTLOUT pctlpts = 25 50 75
					  PCTLPRE = FixedAcidity CitricAcid ResidualSugar
                      pctlname = pct25 pct50 pct75;
run;





data &STGFILE0.;
set &STGFILE0.;
ResidualSugar_Missing_Ind=0;
Chlorides_Missing_Ind=0;
FreeSulfurDioxide_Missing_Ind=0;
TotalSulfurDioxide_Missing_Ind=0;
pH_Missing_Ind=0;
Sulphates_Missing_Ind=0;
Alcohol_Missing_Ind=0;
Unrated_Wine_Ind=0;
if missing(ResidualSugar) then ResidualSugar_Bin='X';
else if ResidualSugar < -2 then ResidualSugar_Bin='A';
else if ResidualSugar >= -2 and  ResidualSugar < 3.9 then ResidualSugar_Bin='B';
else if ResidualSugar >= 3.9 and  ResidualSugar < 15.9 then ResidualSugar_Bin='C';
else if ResidualSugar >= 15.9 then ResidualSugar_Bin='D';

if missing(FixedAcidity) then FixedAcidity_Bin='X';
else if FixedAcidity < 5.2 then FixedAcidity_Bin='A';
else if FixedAcidity >= 5.2 and  FixedAcidity < 6.9 then FixedAcidity_Bin='B';
else if FixedAcidity >= 6.9 and  FixedAcidity < 9.5 then FixedAcidity_Bin='C';
else if FixedAcidity >= 9.5 then FixedAcidity_Bin='D';

if missing(CitricAcid) then CitricAcid_Bin='X';
else if CitricAcid < 0.03 then CitricAcid_Bin='A';
else if CitricAcid >= 0.03 and  CitricAcid < 0.31 then CitricAcid_Bin='B';
else if CitricAcid >= 0.31 and  CitricAcid < 0.58 then CitricAcid_Bin='C';
else if CitricAcid >= 0.58 then CitricAcid_Bin='D';

/*if missing(ResidualSugar) then do; ResidualSugar_Missing_Ind=1; end;*/
if missing(Chlorides) then do; Chlorides=0; Chlorides_Missing_Ind=1; end;
if missing(FreeSulfurDioxide) then do; FreeSulfurDioxide=0; FreeSulfurDioxide_Missing_Ind=1; end;
if missing(TotalSulfurDioxide) then do; TotalSulfurDioxide=0; TotalSulfurDioxide_Missing_Ind=1; end;
if missing(pH) then do; pH=0; pH_Missing_Ind=1; end;
if missing(Sulphates) then do; Sulphates=0; Sulphates_Missing_Ind=1; end;
if missing(Alcohol) then do; Alcohol=0; Alcohol_Missing_Ind=1; end;

if missing(STARS) then STARS_IND='X';
else if stars=1 then STARS_IND='A';
else if stars=2 then STARS_IND='B';
else if stars=3 then STARS_IND='C';
else if stars=4 then STARS_IND='D';

if missing(STARS) and LabelAppeal<-0.5 then STARS_REG_IMP=1.6;
else if missing(STARS) and LabelAppeal<0.5 and LabelAppeal>-0.5 then STARS_REG_IMP=2;
else if missing(STARS) and LabelAppeal>=0.5 then STARS_REG_IMP=2.4;
else STARS_REG_IMP=STARS;

if missing(STARS) then do; STARS=0; Unrated_Wine_Ind=1; end;

run;


proc freq data=&STGFILE0.;
tables ResidualSugar_Bin FixedAcidity_Bin CitricAcid_Bin;
run;

proc genmod data=&STGFILE0.;
class ResidualSugar_Bin FixedAcidity_Bin CitricAcid_Bin STARS_IND;
model TARGET = ResidualSugar_Bin 
				FixedAcidity_Bin
				CitricAcid_Bin
				AcidIndex
				Alcohol
				Alcohol_Missing_Ind
				Chlorides
				Chlorides_Missing_Ind
				Density
				FreeSulfurDioxide
				FreeSulfurDioxide_Missing_Ind
				LabelAppeal
				STARS_IND
				Unrated_Wine_Ind
				Sulphates
				Sulphates_Missing_Ind
				TotalSulfurDioxide
				TotalSulfurDioxide_Missing_Ind
				VolatileAcidity
				pH
				pH_Missing_Ind / link=log dist=poi;
output out=&STGFILE0. p=TARGET_POI;
run;

proc sgplot data=&STGFILE0.;
histogram TARGET_POI / transparency=0.7;
histogram TARGET / transparency=0.5;
keylegend / location=inside position=topright noborder across=2;
run;

proc sql;
select
/*sum(TARGET_POI-target)*100 as var_poi,sum(TARGET_NB-target)*100 as var_nb,*/
case 
when TARGET=round(TARGET_POI,1) then '0 - Match'
when abs(TARGET_POI - TARGET) <= 1 then '1 - Differ by 1'
when abs(TARGET_POI - TARGET) <= 2 then '2 - Differ by 2'
else '3 - Bigger variance'
end as category,
count(TARGET_POI)
from  &STGFILE0.
where TARGET > 0
group by category;
run;



data &STGFILE0.;
set &STGFILE0.;
ResidualSugar_Missing_Ind=0;
Chlorides_Missing_Ind=0;
FreeSulfurDioxide_Missing_Ind=0;
TotalSulfurDioxide_Missing_Ind=0;
pH_Missing_Ind=0;
Sulphates_Missing_Ind=0;
Alcohol_Missing_Ind=0;
Unrated_Wine_Ind=0;
if missing(ResidualSugar) then ResidualSugar_Bin='X';
else if ResidualSugar < -2 then ResidualSugar_Bin='A';
else if ResidualSugar >= -2 and  ResidualSugar < 3.9 then ResidualSugar_Bin='B';
else if ResidualSugar >= 3.9 and  ResidualSugar < 15.9 then ResidualSugar_Bin='C';
else if ResidualSugar >= 15.9 then ResidualSugar_Bin='D';

if missing(FixedAcidity) then FixedAcidity_Bin='X';
else if FixedAcidity < 5.2 then FixedAcidity_Bin='A';
else if FixedAcidity >= 5.2 and  FixedAcidity < 6.9 then FixedAcidity_Bin='B';
else if FixedAcidity >= 6.9 and  FixedAcidity < 9.5 then FixedAcidity_Bin='C';
else if FixedAcidity >= 9.5 then FixedAcidity_Bin='D';

if missing(CitricAcid) then CitricAcid_Bin='X';
else if CitricAcid < 0.03 then CitricAcid_Bin='A';
else if CitricAcid >= 0.03 and  CitricAcid < 0.31 then CitricAcid_Bin='B';
else if CitricAcid >= 0.31 and  CitricAcid < 0.58 then CitricAcid_Bin='C';
else if CitricAcid >= 0.58 then CitricAcid_Bin='D';

if missing(Chlorides) then do; Chlorides=0; Chlorides_Missing_Ind=1; end;
if missing(FreeSulfurDioxide) then do; FreeSulfurDioxide=0; FreeSulfurDioxide_Missing_Ind=1; end;
if missing(TotalSulfurDioxide) then do; TotalSulfurDioxide=0; TotalSulfurDioxide_Missing_Ind=1; end;
if missing(pH) then do; pH=0; pH_Missing_Ind=1; end;
if missing(Sulphates) then do; Sulphates=0; Sulphates_Missing_Ind=1; end;
if missing(Alcohol) then do; Alcohol=0; Alcohol_Missing_Ind=1; end;
/*if missing(STARS) then do; STARS=0; Unrated_Wine_Ind=1; end;*/
if missing(STARS) then STARS_IND='X';
else if stars=1 then STARS_IND='A';
else if stars=2 then STARS_IND='B';
else if stars=3 then STARS_IND='C';
else if stars=4 then STARS_IND='D';
run;



proc freq data=&STGFILE0.;
tables ResidualSugar_Bin FixedAcidity_Bin CitricAcid_Bin;
run;


proc genmod data=&STGFILE0.;
class ResidualSugar_Bin FixedAcidity_Bin CitricAcid_Bin STARS_IND;
model TARGET = /*ResidualSugar_Bin 
				FixedAcidity_Bin
				CitricAcid_Bin*/
				AcidIndex
				Alcohol
				Alcohol_Missing_Ind
				/*Chlorides
				Chlorides_Missing_Ind
				Density
				FreeSulfurDioxide
				FreeSulfurDioxide_Missing_Ind*/
				LabelAppeal
				STARS_IND
				Unrated_Wine_Ind
				/*Sulphates
				Sulphates_Missing_Ind
				TotalSulfurDioxide
				TotalSulfurDioxide_Missing_Ind*/
				VolatileAcidity
				/*pH
				pH_Missing_Ind*/ / link=log dist=nb;
output out=&STGFILE0. p=TARGET_NB;
run;



proc sgplot data=&STGFILE0.;
histogram TARGET_NB / transparency=0.7;
histogram TARGET / transparency=0.5;
keylegend / location=inside position=topright noborder across=2;
run;


proc sql;
select
sum(TARGET_POI-target)*100 as var_poi,sum(TARGET_NB-target)*100 as var_nb
from  &STGFILE0.
where TARGET > 0;
run;

proc sql;
select
/*sum(TARGET_POI-target)*100 as var_poi,sum(TARGET_NB-target)*100 as var_nb,*/
case 
when TARGET=round(TARGET_NB,1) then '0 - Match'
when abs(TARGET_NB - TARGET) <= 1 then '1 - Differ by 1'
when abs(TARGET_NB - TARGET) <= 2 then '2 - Differ by 2'
else '3 - Bigger variance'
end as category,
count(TARGET_NB)
from  &STGFILE0.
where TARGET > 0
group by category;
run;


proc sql;
select
/*sum(TARGET_POI-target)*100 as var_poi,sum(TARGET_NB-target)*100 as var_nb,*/
case 
when TARGET=round(TARGET_POI,1) then '0 - Match'
when abs(TARGET_POI - TARGET) <= 1 then '1 - Differ by 1'
when abs(TARGET_POI - TARGET) <= 2 then '2 - Differ by 2'
else '3 - Bigger variance'
end as category,
count(TARGET_POI)
from  &STGFILE0.
where TARGET > 0
group by category;
run;



proc genmod data=&STGFILE0.;
class ResidualSugar_Bin FixedAcidity_Bin CitricAcid_Bin STARS_IND;
   model TARGET = AcidIndex
					Alcohol
					Alcohol_Missing_Ind
					LabelAppeal
					STARS_IND
					Unrated_Wine_Ind / dist=ZIP link=log;
zeromodel AcidIndex
					Alcohol
					Alcohol_Missing_Ind
					LabelAppeal
					STARS_IND
					Unrated_Wine_Ind / link=logit;
output out=&STGFILE0. pred=P_TARGET_ZIP pzero=P_ZERO_ZIP;
run;

proc sgplot data=&STGFILE0.;
histogram P_ZERO_ZIP / transparency=0.7;
histogram P_TARGET_ZIP / transparency=0.6;
histogram TARGET / transparency=0.5;
keylegend / location=inside position=topright noborder across=2;
run;

proc sql;
select
'Non-Zero',
case 
when TARGET=round(P_TARGET_ZIP,1) then '0 - Match'
when abs(P_TARGET_ZIP - TARGET) <= 1 then '1 - Differ by 1'
when abs(P_TARGET_ZIP - TARGET) <= 2 then '2 - Differ by 2'
else '3 - Bigger variance'
end as category,
count(P_TARGET_ZIP)
from  &STGFILE0.
where TARGET > 0
group by category

union

select
'Zero',
case 
when TARGET=round(P_ZERO_ZIP,1) then '0 - Match'
when abs(P_ZERO_ZIP - TARGET) <= 1 then '1 - Differ by 1'
when abs(P_ZERO_ZIP - TARGET) <= 2 then '2 - Differ by 2'
else '3 - Bigger variance'
end as category,
count(P_ZERO_ZIP)
from  &STGFILE0.
where TARGET = 0
group by category
;
run;



proc genmod data=&STGFILE0.;
class ResidualSugar_Bin FixedAcidity_Bin CitricAcid_Bin STARS_IND;
   model TARGET = AcidIndex
					Alcohol
					Alcohol_Missing_Ind
					LabelAppeal
					STARS_IND
					Unrated_Wine_Ind / dist=ZINB link=log;
zeromodel AcidIndex
					Alcohol
					Alcohol_Missing_Ind
					LabelAppeal
					STARS_IND
					Unrated_Wine_Ind / link=logit;
   output out=&STGFILE0. pred=P_TARGET_ZINB pzero=P_ZERO_ZINB;
run;



proc sql;
select
'Non-Zero',
case 
when TARGET=round(P_TARGET_ZINB,1) then '0 - Match'
when abs(P_TARGET_ZINB - TARGET) <= 1 then '1 - Differ by 1'
when abs(P_TARGET_ZINB - TARGET) <= 2 then '2 - Differ by 2'
else '3 - Bigger variance'
end as category,
count(P_TARGET_ZINB)
from  &STGFILE0.
where TARGET > 0
group by category

union

select
'Zero',
case 
when TARGET=round(P_ZERO_ZINB,1) then '0 - Match'
when abs(P_ZERO_ZINB - TARGET) <= 1 then '1 - Differ by 1'
when abs(P_ZERO_ZINB - TARGET) <= 2 then '2 - Differ by 2'
else '3 - Bigger variance'
end as category,
count(P_ZERO_ZINB)
from  &STGFILE0.
where TARGET = 0
group by category
;
run;


proc sgplot data=&STGFILE0.;
histogram P_ZERO_ZINB / transparency=0.7;
histogram P_TARGET_ZINB / transparency=0.6;
histogram TARGET / transparency=0.5;
keylegend / location=inside position=topright noborder across=2;
run;


data &STGFILE0.;
set &INFILE.;
run;

data &STGFILE0.;
set &STGFILE0.;
ResidualSugar_Missing_Ind=0;
Chlorides_Missing_Ind=0;
FreeSulfurDioxide_Missing_Ind=0;
TotalSulfurDioxide_Missing_Ind=0;
pH_Missing_Ind=0;
Sulphates_Missing_Ind=0;
Alcohol_Missing_Ind=0;
Unrated_Wine_Ind=0;
STARS_IMP=STARS;
if missing(ResidualSugar) then do; 
ResidualSugar=0; 
ResidualSugar_Missing_Ind=1; end;
if missing(Chlorides) then do; Chlorides=0; Chlorides_Missing_Ind=1; end;
if missing(FreeSulfurDioxide) then do; FreeSulfurDioxide=0; FreeSulfurDioxide_Missing_Ind=1; end;
if missing(TotalSulfurDioxide) then do; TotalSulfurDioxide=0; TotalSulfurDioxide_Missing_Ind=1; end;
if missing(pH) then do; pH=0; pH_Missing_Ind=1; end;
if missing(Sulphates) then do; Sulphates=0; Sulphates_Missing_Ind=1; end;
if missing(Alcohol) then do; Alcohol=0; Alcohol_Missing_Ind=1; end;
if missing(STARS) then do; Unrated_Wine_Ind=1; end;
if missing(STARS) then STARS_IMP=0; 
run;

proc reg data=&STGFILE0.;
	model TARGET=ResidualSugar 
					ResidualSugar_Missing_Ind
					FixedAcidity
					CitricAcid
					AcidIndex
					Alcohol
					Alcohol_Missing_Ind
					Chlorides
					Chlorides_Missing_Ind
					Density
					FreeSulfurDioxide
					FreeSulfurDioxide_Missing_Ind
					/*LabelAppeal*/
					STARS_REG_IMP
					Unrated_Wine_Ind
					Sulphates
					Sulphates_Missing_Ind
					TotalSulfurDioxide
					TotalSulfurDioxide_Missing_Ind
					VolatileAcidity
					pH
					pH_Missing_Ind/ selection=stepwise slentry=0.05 slstay=0.05 adjrsq aic bic mse rmse cp vif;
output out=&STGFILE0. PREDICTED=P_TARGET_REG RESIDUAL=P_RESID_REG;
run;


proc sql;
select
case 
when TARGET=round(P_TARGET_REG,1) then '0 - Match'
when abs(P_TARGET_REG - TARGET) <= 1 then '1 - Differ by 1'
when abs(P_TARGET_REG - TARGET) <= 2 then '2 - Differ by 2'
else '3 - Bigger variance'
end as category,
count(P_TARGET_REG)
from  &STGFILE0.
group by category
;
run;



proc GLM data=&STGFILE0.;
class ResidualSugar_Bin FixedAcidity_Bin CitricAcid_Bin STARS_IND;
   model TARGET = ResidualSugar_Bin 
					FixedAcidity_Bin
					CitricAcid_Bin
					AcidIndex
					Alcohol
					Alcohol_Missing_Ind
					Chlorides
					Chlorides_Missing_Ind
					Density
					FreeSulfurDioxide
					FreeSulfurDioxide_Missing_Ind
					LabelAppeal
					STARS_IND
					Unrated_Wine_Ind
					Sulphates
					Sulphates_Missing_Ind
					TotalSulfurDioxide
					TotalSulfurDioxide_Missing_Ind
					VolatileAcidity
					pH
					pH_Missing_Ind;
output out=&STGFILE0. PREDICTED=P_TARGET_GLM RESIDUAL=P_RESID_GLM;
run;




/* Model Selection */
proc sql;
select
'NON-ZERO TARGET',
sum(abs(P_TARGET_ZIP - TARGET)**2) AS ZIP,
sum(abs(P_TARGET_ZINB - TARGET)**2) AS ZINB
from  &STGFILE0.
where target > 0

UNION

select
'ZERO TARGET',
sum(abs(P_TARGET_ZIP - TARGET)**2),
sum(abs(P_TARGET_ZINB - TARGET)**2)
from  &STGFILE0.
where target = 0
;
run;



/* Model Selection */
proc sql;
select
'FALSE NEGATIVE - NON ZERO',
sum(CASE WHEN TARGET > ROUND(P_TARGET_ZIP,1) THEN 1 else 0 end) as ZIP,
sum(CASE WHEN TARGET > ROUND(P_TARGET_ZINB,1) THEN 1 else 0 end) as ZINB
from  &STGFILE0.
where target > 0

UNION

select
'FALSE POSITIVE - NON ZERO',
sum(CASE WHEN ROUND(P_TARGET_ZIP,1) > TARGET THEN 1 else 0 end) as ZIP,
sum(CASE WHEN ROUND(P_TARGET_ZINB,1) > TARGET THEN 1 else 0 end) as ZINB
from  &STGFILE0.
where target > 0

UNION

select
'FALSE POSITIVE - ZERO',
sum(CASE WHEN ROUND(P_TARGET_ZIP,1) > TARGET THEN 1 else 0 end) as ZIP,
sum(CASE WHEN ROUND(P_TARGET_ZINB,1) > TARGET THEN 1 else 0 end) as ZINB
from  &STGFILE0.
where target = 0
;
run;


/* BINGO BONUS - Develop a LOGISTIC / POISSON model */



data &STGFILE0.;
set &STGFILE0.;
TARGET_LOGISTIC=0;
if TARGET>0 then TARGET_LOGISTIC=1;
run;

proc freq data= &STGFILE0.;
tables TARGET_LOGISTIC;
run;


proc logistic data=&STGFILE0. plot(only)=(roc(ID=prob)) descending;
class _character_ /param=ref;
model TARGET_LOGISTIC = AcidIndex
				Alcohol
				Alcohol_Missing_Ind
				LabelAppeal
				STARS_IND
				Unrated_Wine_Ind / selection=stepwise slentry=0.05 slstay=0.05 roceps=0.1
				;
output out=&STGFILE0. p=P_TARGET_LOGISTIC;
roc;
run;

proc genmod data=&STGFILE0.;
class ResidualSugar_Bin FixedAcidity_Bin CitricAcid_Bin STARS_IND;
model TARGET = ResidualSugar_Bin 
				FixedAcidity_Bin
				CitricAcid_Bin
				AcidIndex
				Alcohol
				Alcohol_Missing_Ind
				Chlorides
				Chlorides_Missing_Ind
				Density
				FreeSulfurDioxide
				FreeSulfurDioxide_Missing_Ind
				LabelAppeal
				STARS_IND
				Unrated_Wine_Ind
				Sulphates
				Sulphates_Missing_Ind
				TotalSulfurDioxide
				TotalSulfurDioxide_Missing_Ind
				VolatileAcidity
				pH
				pH_Missing_Ind / link=log dist=poi;
output out=&STGFILE0. p=TARGET_POI;
run;



data &STGFILE0.;
set &STGFILE0.;
TARGET_ZERO=2.8098 
				+ (-0.3949 * AcidIndex) 
				+ (-0.0188 * Alcohol) 
				+ (-0.4645 * LabelAppeal)
				+ (1.8219 * (STARS_IND in ('A')))
				+ (4.2541 * (STARS_IND in ('B')))
				+ (19.3573 * (STARS_IND in ('C')))
				+ (19.4877 * (STARS_IND in ('D')));
P_TARGET_ZERO = exp(TARGET_ZERO) / (1+exp(TARGET_ZERO));

TARGET_FULL=1.2053 + 
				(-0.0243 * (ResidualSugar_Bin in ('A'))) +
				(-0.062 * (ResidualSugar_Bin in ('B'))) +
				(0.0013 * (ResidualSugar_Bin in ('C'))) +
				(-0.0229 * (ResidualSugar_Bin in ('D'))) +
				(0 * (ResidualSugar_Bin in ('X'))) +
				(0.0041 * (FixedAcidity_Bin in ('A'))) +
				(-0.0082 * (FixedAcidity_Bin in ('B'))) +
				(0.044 * (FixedAcidity_Bin in ('C'))) +
				(0 * (FixedAcidity_Bin in ('D'))) +
				(-0.0211 * (CitricAcid_Bin in ('A'))) +
				(-0.0325 * (CitricAcid_Bin in ('B'))) +
				(0.0201 * (CitricAcid_Bin in ('C'))) +
				(0 * (CitricAcid_Bin in ('D'))) +
				(-0.0874 * AcidIndex) +
				(0.0041 * Alcohol) +
				(0.0629 * Alcohol_Missing_Ind) +
				(-0.0384 * Chlorides) +
				(-0.0037 * Chlorides_Missing_Ind) +
				(-0.2881 * Density) +
				(0.0001 * FreeSulfurDioxide) +
				(0.0211 * FreeSulfurDioxide_Missing_Ind) +
				(0.1598 * LabelAppeal) +
				(0.7626 * (STARS_IND in ('A'))) +
				(1.083 * (STARS_IND in ('B'))) +
				(1.2003 * (STARS_IND in ('C'))) +
				(1.3146 * (STARS_IND in ('D'))) +
				(0 * (STARS_IND in ('X'))) +
				(0 * Unrated_Wine_Ind) +
				(-0.0129 * Sulphates) +
				(-0.0171 * Sulphates_Missing_Ind) +
				(0.0001 * TotalSulfurDioxide) +
				(0.0255 * TotalSulfurDioxide_Missing_Ind) +
				(-0.0299 * VolatileAcidity) +
				(-0.0114 * pH) +
				(-0.0753 * pH_Missing_Ind) ;
				
P_TARGET_FULL = exp(TARGET_FULL);

P_TARGET_LOGISTIC=P_TARGET_FULL * P_TARGET_ZERO;

run;


proc means data=&STGFILE0. N Mean Median StdDev	Min	Max P5 P95 NMISS STACKODSOUTPUT;
var P_TARGET_LOGISTIC P_TARGET_ZERO P_TARGET_FULL;
run;

proc print data =&STGFILE0.;
var TARGET P_TARGET_LOGISTIC P_TARGET_ZERO P_TARGET_FULL;
run;

proc sql;
select
case 
when TARGET=round(P_TARGET_LOGISTIC,1) then '0 - Match'
when abs(P_TARGET_LOGISTIC - TARGET) <= 1 then '1 - Differ by 1'
when abs(P_TARGET_LOGISTIC - TARGET) <= 2 then '2 - Differ by 2'
else '3 - Bigger variance'
end as category,
count(P_TARGET_LOGISTIC)
from  &STGFILE0.
group by category
;
run;