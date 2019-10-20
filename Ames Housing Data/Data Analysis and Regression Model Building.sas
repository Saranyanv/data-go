title 'Predict 410 Winter 2016 Sec 55 Assignment 3';
libname mydata '/scs/crb519/PREDICT_410/SAS_Data/' access=readonly;

data ames_stg0;
set mydata.ames_housing_data;
run;

* Create additional variables for computation and to qualify sample population;
data ames_stg1;
set ames_stg0;
format drop_condition $50.;
TotalSF = TotalBsmtSF + FirstFlrSF + SecondFlrSF;
if subclass >= 120 and subclass <= 180 then drop_condition='01: Planned Unit Development Homes';
else if Zoning ='A' or Zoning ='C' or Zoning ='FV' or Zoning ='I' then drop_condition='02: Properties in NonHousing zones';
else if BldgType = '2FmCon' or BldgType = 'Duplx' or BldgType = 'TwnhsE' or BldgType = 'TwnhsI' then
drop_condition='03: Not a single family dwelling';
else if TotalSF > 6000 then drop_condition='04: Trimming extremes in TotalSF';
else if GrLivArea < 400 OR GrLivArea > 3000 then drop_condition='05: Trimming extremes in GrLivArea';
else if GarageArea < 220 OR GarageArea > 1000 then drop_condition='06: Trimming extremes in
GarageArea';
else if TotalBsmtSF < 400 OR TotalBsmtSF > 2100 then drop_condition='07: Trimming extremes in
TotalBsmtSF';
else if FirstFlrSF < 400 OR FirstFlrSF > 2000 then drop_condition='08: Trimming extremes in FirstFlrSF';
else if MasVnrArea > 1000 then drop_condition='09: Trimming extremes in MasVnrArea';
else if BsmtFinSF1 > 1500 then drop_condition='10: Trimming extremes in BsmtFinSF1';
else if FullBath < 1 OR FullBath > 3 then drop_condition='11: Trimming extremes in FullBath';
else if YearBuilt < 1920 then drop_condition='12: Trimming extremes in YearBuilt';
else if OverallQual < 3 then drop_condition='13: Trimming extremes in OverallQual';
else drop_condition='14: Sample Population';
run;

* Keep only qualifed sample population in our work dataset;
data ames_stg2;
set ames_stg1;
where drop_condition='14: Sample Population';
run;

* Initial Data Exploratory Analysis;
data ames_stg3;
set ames_stg2 (keep=OverallQual TotalSF GrLivArea GarageArea TotalBsmtSF FirstFlrSF YearBuilt MasVnrArea FullBath BsmtFinSF1 SalePrice);
run;quit;

* Create log(SalePrice),log(TotalSF), log(GrLivArea) to be used later in the analysis;
data ames_stg4;
set ames_stg3;
log_SalePrice= log(SalePrice);
log_TotalSF= log(TotalSF);
log_GrLivArea= log(GrLivArea);
run;

proc corr data=ames_stg3 nosimple rank;
var BsmtFinSF1 FirstFlrSF FullBath GarageArea GrLivArea MasVnrArea OverallQual TotalBsmtSF TotalSF YearBuilt;
with SalePrice;

proc reg data=ames_stg3 plots=(RESIDUALBYPREDICTED RESIDUALS(UNPACK));
model SalePrice=BsmtFinSF1 FirstFlrSF GarageArea GrLivArea MasVnrArea TotalBsmtSF TotalSF
/ selection=rsquare ADJRSQ;
run;

/* Simple linear regression with TotalSF and original response variable */
proc reg data=ames_stg4 PLOTS=(DIAGNOSTICS(UNPACK) RESIDUALBYPREDICTED QQPLOT OBSERVEDBYPREDICTED);
model SalePrice = TotalSF;
output out=temp_TotalSF
p=PREDICT_SalePricehat
r=RESID_SalePriceresid
RSTUDENT=RSTUDENT_SalePrice
COOKD=COOKD_SalePrice;
run;

/* Simple linear regression with GrLivArea and original response variable */
proc reg data=ames_stg4 PLOTS=(DIAGNOSTICS(UNPACK) RESIDUALBYPREDICTED QQPLOT OBSERVEDBYPREDICTED);
model SalePrice = GrLivArea;
output out=temp_GrLivArea
p=PREDICT_SalePricehat
r=RESID_SalePriceresid
RSTUDENT=RSTUDENT_SalePrice
COOKD=COOKD_SalePrice;
run;

/* Multiple linear regression with 2 Predictors */
proc reg data=ames_stg4 PLOTS=(DIAGNOSTICS(UNPACK) RESIDUALBYPREDICTED QQPLOT OBSERVEDBYPREDICTED);
model SalePrice = TotalSF GrLivArea;
output out=temp_multi1
p=PREDICT_SalePricehat
r=RESID_SalePriceresid
RSTUDENT=RSTUDENT_SalePrice
COOKD=COOKD_SalePrice;
run;

proc univariate normal plot data=ames_stg4 ;
var TotalSF;
histogram TotalSF / normal;
run;quit;

proc univariate normal plot data=ames_stg4 ;
var GrLivArea;
histogram GrLivArea/ normal;
run;quit;

proc format;
value rstudent_sfmt
3 - high = '00: more than 3'
2 -< 3 = '01: 2 to 3'
1 -< 2 = '02: 1 to 2'
0 -< 1 = '03: 0 to 1'
-1 -< 0 = '04: -1 to 0'
-2 -< -1 = '05: -2 to -1'
-3 -< -2 = '06: -3 to -2'
low -< -3 = '08: Less than -3'
;
run;

data temp_multi2;
set temp_multi1;
format Rstudent_Range $50.;
format CookD_Range $50.;
Rstudent_Range = put(RSTUDENT_SalePrice,rstudent_sfmt.);
if COOKD_SalePrice > 4/(1926-(3+1)) then CookD_Range='01: CookD Potential outlier';
else CookD_Range='02: Not an outlier';
run;

title 'Distribution of Outliers by RStudent';
proc freq data=temp_multi2;
tables Rstudent_Range / nocol nocum nopercent norow;
run; quit;

title 'Distribution of Outliers by Cooks D';
proc freq data=temp_multi2;
tables CookD_Range / nocol nocum nopercent norow;
run; quit;

title 'Distribution of Outliers by Cooks D and RStudent';
proc freq data=temp_multi2;
tables Rstudent_Range * CookD_Range / nocol nocum nopercent norow;
run; quit;

data ames_stg5;
set temp_multi2;
format Type_of_Outlier $50.;
if CookD_Range='01: CookD Potential outlier' then Type_of_Outlier='01: CookD Potential outlier';
else if Rstudent_Range='00: more than 3' or Rstudent_Range='08: Less than -3' then Type_of_Outlier='02: RStudent outlier';
else if TotalSF >= 4196 or TotalSF <= 1489 then Type_of_Outlier='03: TotalSF outlier';
else if GrLivArea >= 2728 or GrLivArea <= 768 then Type_of_Outlier='04: GrLivArea outlier';
else Type_of_Outlier='05: Not an outlier';
run;

title 'Distribution of Outliers';
proc freq data=ames_stg5;
tables Type_of_Outlier;
run; quit;
data ames_stg6;
set ames_stg5;
where Type_of_Outlier='05: Not an outlier';
run;

/* Multiple linear regression with 2 Predictors, Without outliers */
proc reg data=ames_stg6 PLOTS=(DIAGNOSTICS(UNPACK) RESIDUALBYPREDICTED QQPLOT OBSERVEDBYPREDICTED);
model SalePrice = TotalSF GrLivArea;
output out=temp_multi1
p=PREDICT_SalePricehat
r=RESID_SalePriceresid
RSTUDENT=RSTUDENT_SalePrice
COOKD=COOKD_SalePrice;
run;

/* Multiple linear regression with 2 Predictors and transformed response variable */
proc reg data=ames_stg4 PLOTS=(DIAGNOSTICS(UNPACK) RESIDUALBYPREDICTED QQPLOT OBSERVEDBYPREDICTED);
model log_SalePrice = TotalSF GrLivArea;
output out=temp_multi2
p=PREDICT_SalePricehat
r=RESID_SalePriceresid
RSTUDENT=RSTUDENT_SalePrice
COOKD=COOKD_SalePrice;
run;

/* Multiple linear regression with 2 transformed Predictors and transformed response variable */
proc reg data=ames_stg4 PLOTS=(DIAGNOSTICS(UNPACK) RESIDUALBYPREDICTED QQPLOT OBSERVEDBYPREDICTED);
model log_SalePrice = log_TotalSF log_GrLivArea;
output out=temp_multi3
p=PREDICT_SalePricehat
r=RESID_SalePriceresid
RSTUDENT=RSTUDENT_SalePrice
COOKD=COOKD_SalePrice;
run;

/* Multiple linear regression with 1 transformed Predictor and transformed response variable */
proc reg data=ames_stg4 PLOTS=(DIAGNOSTICS(UNPACK) RESIDUALBYPREDICTED QQPLOT OBSERVEDBYPREDICTED);
model log_SalePrice = TotalSF log_GrLivArea;
output out=temp_multi3
p=PREDICT_SalePricehat
r=RESID_SalePriceresid
RSTUDENT=RSTUDENT_SalePrice
COOKD=COOKD_SalePrice;
run;

proc reg data=ames_stg4 PLOTS=(DIAGNOSTICS(UNPACK) RESIDUALBYPREDICTED QQPLOT OBSERVEDBYPREDICTED);
model log_SalePrice = log_TotalSF GrLivArea;
output out=temp_multi3
p=PREDICT_SalePricehat
r=RESID_SalePriceresid
RSTUDENT=RSTUDENT_SalePrice
COOKD=COOKD_SalePrice;
run;