%let INFILE_TEST = &NAME..WINE_TEST;
%let STGFILE0_TEST = wine_test;

data &STGFILE0_TEST.;
set &INFILE_TEST.;
run;


data &STGFILE0_TEST.;
set &STGFILE0_TEST.;
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

if missing(STARS) then do; STARS=0; Unrated_Wine_Ind=1; end;

TEMP_ZERO= -3.3005 +
			(0.4324 * AcidIndex) +
			(0.0297 * Alcohol) +
			(0.1913 * Alcohol_Missing_Ind) +
			(0.7202 * LabelAppeal) +
			(-2.068 * (STARS_IND IN ('A'))) +
			(-5.8195 * (STARS_IND IN ('B'))) +
			(-25.0258 * (STARS_IND IN ('C'))) +
			(-25.2159 * (STARS_IND IN ('D'))) +
			(0 * (STARS_IND IN ('X'))) +
			(0 * Unrated_Wine_Ind);

P_SCORE_ZERO = exp(TEMP_ZERO) / (1 + exp(TEMP_ZERO));

TEMP_FULL= 1.2025 +
			(-0.0202 * AcidIndex) +
			(0.007 * Alcohol) +
			(0.0751 * Alcohol_Missing_Ind) +
			(0.2323 * LabelAppeal) +
			(0.0649 * (STARS_IND IN ('A'))) +
			(0.194 * (STARS_IND IN ('B'))) +
			(0.289 * (STARS_IND IN ('C'))) +
			(0.3778 * (STARS_IND IN ('D'))) +
			(0 * (STARS_IND IN ('X'))) +
			(0 * Unrated_Wine_Ind);
			
P_SCORE_FULL = exp(TEMP_FULL);

P_TARGET = P_SCORE_FULL * (1 - P_SCORE_ZERO);

run;

PROC PRINT DATA=&STGFILE0_TEST.;
VAR INDEX P_TARGET;
RUN;