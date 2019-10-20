title 'Predict 410 Winter 2016 Sec 55 Assignment 5';
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
else if Zoning ='A' or Zoning ='C' or Zoning ='FV' or Zoning ='I' then drop_condition='02: Properties in Non-Housing zones';
else if BldgType = '2FmCon' or BldgType = 'Duplx' or BldgType = 'TwnhsE' or BldgType = 'TwnhsI' then drop_condition='03: Not a single family dwelling';
else if TotalSF > 6000 then drop_condition='04: Trimming extremes in TotalSF';
else if GrLivArea < 400 OR GrLivArea > 3000 then drop_condition='05: Trimming extremes in GrLivArea';
else if GarageArea < 220 OR GarageArea > 1000 then drop_condition='06: Trimming extremes in GarageArea';
else if TotalBsmtSF < 400 OR TotalBsmtSF > 2100 then drop_condition='07: Trimming extremes in TotalBsmtSF';
else if FirstFlrSF < 400 OR FirstFlrSF > 2000 then drop_condition='08: Trimming extremes in FirstFlrSF';
else if MasVnrArea > 1000 or MasVnrArea=. then drop_condition='09: Trimming extremes in MasVnrArea';
else if BsmtFinSF1 > 1500 then drop_condition='10: Trimming extremes in BsmtFinSF1';
else if FullBath < 1 OR FullBath > 3 then drop_condition='11: Trimming extremes in FullBath';
else if YearBuilt < 1920 then drop_condition='12: Trimming extremes in YearBuilt';
else if OverallQual < 3 then drop_condition='13: Trimming extremes in OverallQual';
else drop_condition='14: Sample Population';
run;

title 'Frequency of occurrence of various Drop Conditions identified';
proc freq data=ames_stg1;
tables drop_condition;
run; quit;

* Keep only qualifed sample population in our work dataset;
data ames_stg2;
set ames_stg1;
where drop_condition='14: Sample Population';
run;

data ames_training;
set ames_stg2;
* generate a uniform(0,1) random variable with seed set to 123;
u = uniform(123);
if (u < 0.70) then train = 1;
else train = 0;
if (train=1) then train_response=SalePrice;
else train_response=.;
run;

title 'Distribution of Training and Testing datasets';
proc freq data=ames_training;
table train;
run;quit;

proc corr data=ames_training nosimple rank;
var BsmtFinSF1 FirstFlrSF FullBath GarageArea GrLivArea MasVnrArea OverallQual TotalBsmtSF TotalSF YearBuilt GarageCars TotRmsAbvGrd YearRemodel GarageYrBlt Fireplaces;
with train_response;
/* Model ADJRSQ */
proc reg data=ames_training outest=ADJRSQ_out1;
model train_response=BsmtFinSF1 FullBath GarageArea GrLivArea MasVnrArea OverallQual TotalSF YearBuilt GarageCars TotRmsAbvGrd YearRemodel GarageYrBlt Fireplaces
/ selection=ADJRSQ AIC BIC mse rmse CP VIF best=10;
output out=ADJRSQ_out2
PREDICTED=PREDICT_SalePricehat
RESIDUAL=RESID_SalePriceresid;
run;

proc print data=ADJRSQ_out2 (obs=10);
run;quit;

data err_adjrsq_train;
set ADJRSQ_out2;
ae=abs(PREDICT_SalePricehat-train_response);
se=abs(PREDICT_SalePricehat-train_response) ** 2;
run;
quit;

proc means data=err_adjrsq_train;
where train=1;
var ae se;
run;

data err_adjrsq_test;
set ADJRSQ_out2;
*where train=0;
ae=abs(PREDICT_SalePricehat-SalePrice);
se=abs(PREDICT_SalePricehat-SalePrice) ** 2;
run;
quit;

proc means data=err_adjrsq_test;
where train=0;
var ae se;
run;

proc format;
value perf_sfmt
0.15 - high = 'Grade 3'
0.10 -< 0.15 = 'Grade 2'
0 -< 0.10 = 'Grade 1'
;
run;

data ADJRSQ_op_valdtn_train;
set ADJRSQ_out2;
where train=1;
pred_grade_val = abs(abs(PREDICT_SalePricehat-train_response)/train_response);
pred_grade= put(pred_grade_val ,perf_sfmt.);
run; quit;
proc freq data=ADJRSQ_op_valdtn_train;
tables pred_grade;
run;

data ADJRSQ_op_valdtn_test;
set ADJRSQ_out2;
where train=0;
pred_grade_val = abs(abs(PREDICT_SalePricehat-SalePrice)/SalePrice);
pred_grade= put(pred_grade_val ,perf_sfmt.);
run; quit;

proc freq data=ADJRSQ_op_valdtn_test;
tables pred_grade;
run;

data ADJRSQ_op_valdtn_traintest;
set ADJRSQ_out2;
if train=1 then
do;
pred_grade_val = abs(abs(PREDICT_SalePricehat-train_response)/train_response);
pred_grade= put(pred_grade_val ,perf_sfmt.);
end;
else
do;
pred_grade_val = abs(abs(PREDICT_SalePricehat-SalePrice)/SalePrice);
pred_grade= put(pred_grade_val ,perf_sfmt.);
end;
run; quit;

proc freq data=ADJRSQ_op_valdtn_traintest;
tables pred_grade * train / norow nopercent;
run;

/* Model MAXR*/
proc reg data=ames_training outest=MAXR_out1;
model train_response=BsmtFinSF1 FullBath GarageArea GrLivArea MasVnrArea OverallQual TotalSF YearBuilt GarageCars TotRmsAbvGrd YearRemodel GarageYrBlt Fireplaces
/ selection=MAXR adjrsq aic bic mse rmse cp vif;
output out=MAXR_out2
PREDICTED=PREDICT_SalePricehat
RESIDUAL=RESID_SalePriceresid;
run;

proc print data=MAXR_out1;
run;quit;

data err_maxr_train;
set MAXR_out2;
ae=abs(PREDICT_SalePricehat-train_response);
se=abs(PREDICT_SalePricehat-train_response) ** 2;
run;
quit;

proc means data=err_maxr_train;
where train=1;
var ae se;
run;
data err_maxr_test;
set MAXR_out2;
*where train=0;
ae=abs(PREDICT_SalePricehat-SalePrice);
se=abs(PREDICT_SalePricehat-SalePrice) ** 2;
run;
quit;

proc means data=err_maxr_test;
where train=0;
var ae se;
run;

data MaxR_op_valdtn_train;
set MAXR_out2;
where train=1;
pred_grade_val = abs(abs(PREDICT_SalePricehat-train_response)/train_response);
pred_grade= put(pred_grade_val ,perf_sfmt.);
run; quit;

proc freq data=MaxR_op_valdtn_train;
tables pred_grade;
run;

data MaxR_op_valdtn_test;
set MAXR_out2;
where train=0;
pred_grade_val = abs(abs(PREDICT_SalePricehat-SalePrice)/SalePrice);
pred_grade= put(pred_grade_val ,perf_sfmt.);
run; quit;

proc freq data=MaxR_op_valdtn_test;
tables pred_grade;
run;

data MaxR_op_valdtn_traintest;
set MaxR_out2;
if train=1 then
do;
pred_grade_val = abs(abs(PREDICT_SalePricehat-train_response)/train_response);
pred_grade= put(pred_grade_val ,perf_sfmt.);
end;
else
do;
pred_grade_val = abs(abs(PREDICT_SalePricehat-SalePrice)/SalePrice);
pred_grade= put(pred_grade_val ,perf_sfmt.);
end;
run; quit;

proc freq data=MaxR_op_valdtn_traintest;
tables pred_grade * train / norow nopercent;
run;

/* Model Mallow's CP*/
proc reg data=ames_training outest=Cp_out1;
model train_response=BsmtFinSF1 FullBath GarageArea GrLivArea MasVnrArea OverallQual TotalSF YearBuilt GarageCars TotRmsAbvGrd YearRemodel GarageYrBlt Fireplaces
/ selection=cp adjrsq aic bic mse rmse vif best=10;
output out=Cp_out2
PREDICTED=PREDICT_SalePricehat
RESIDUAL=RESID_SalePriceresid;
run;

proc print data=Cp_out2(obs=10);
run;quit;

data err_Cp_train;
set Cp_out2;
ae=abs(PREDICT_SalePricehat-train_response);
se=abs(PREDICT_SalePricehat-train_response) ** 2;
run;
quit;

proc means data=err_Cp_train;
where train=1;
var ae se;
run;
data Cp_maxr_test;
set Cp_out2;
*where train=0;
ae=abs(PREDICT_SalePricehat-SalePrice);
se=abs(PREDICT_SalePricehat-SalePrice) ** 2;
run;
quit;
proc means data=Cp_maxr_test;
where train=0;
var ae se;
run;

data Cp_op_valdtn_train;
set Cp_out2;
where train=1;
pred_grade_val = abs(abs(PREDICT_SalePricehat-train_response)/train_response);
pred_grade= put(pred_grade_val ,perf_sfmt.);
run; quit;
proc freq data=Cp_op_valdtn_train;
tables pred_grade;
run;

data Cp_op_valdtn_test;
set Cp_out2;
where train=0;
pred_grade_val = abs(abs(PREDICT_SalePricehat-SalePrice)/SalePrice);
pred_grade= put(pred_grade_val ,perf_sfmt.);
run; quit;

proc freq data=Cp_op_valdtn_test;
tables pred_grade;
run;

/* Model Forward Selection*/
proc reg data=ames_training outest=forward_out1;
model train_response=BsmtFinSF1 FullBath GarageArea GrLivArea MasVnrArea OverallQual TotalSF YearBuilt GarageCars TotRmsAbvGrd YearRemodel GarageYrBlt Fireplaces
/ selection=forward slentry=0.1 adjrsq aic bic mse rmse cp vif;
output out=forward_out2
PREDICTED=PREDICT_SalePricehat
RESIDUAL=RESID_SalePriceresid;
run;

proc print data=forward_out2(obs=10);
run;quit;

data err_forward_train;
set forward_out2;
ae=abs(PREDICT_SalePricehat-train_response);
se=abs(PREDICT_SalePricehat-train_response) ** 2;
run;
quit;

proc means data=err_forward_train;
where train=1;
var ae se;
run;

data err_forward_test;
set forward_out2;
*where train=0;
ae=abs(PREDICT_SalePricehat-SalePrice);
se=abs(PREDICT_SalePricehat-SalePrice) ** 2;
run;
quit;

proc means data=err_forward_test;
where train=0;
var ae se;
run;

data Forward_op_valdtn_train;
set forward_out2;
where train=1;
pred_grade_val = abs(abs(PREDICT_SalePricehat-train_response)/train_response);
pred_grade= put(pred_grade_val ,perf_sfmt.);
run; quit;

proc freq data=Forward_op_valdtn_train;
tables pred_grade;
run;

data Forward_op_valdtn_test;
set forward_out2;
where train=0;
pred_grade_val = abs(abs(PREDICT_SalePricehat-SalePrice)/SalePrice);
pred_grade= put(pred_grade_val ,perf_sfmt.);
run; quit;

proc freq data=Forward_op_valdtn_test;
tables pred_grade;
run;

/* Model backward Selection*/
proc reg data=ames_training outest=backward_out1;
model train_response=BsmtFinSF1 FullBath GarageArea GrLivArea MasVnrArea OverallQual TotalSF YearBuilt GarageCars TotRmsAbvGrd YearRemodel GarageYrBlt Fireplaces
/ selection=backward slstay=0.1 adjrsq aic bic mse rmse cp vif;
output out=backward_out2
PREDICTED=PREDICT_SalePricehat
RESIDUAL=RESID_SalePriceresid;
run;


proc print data=backward_out2(obs=10);
run;quit;

data err_backward_train;
set backward_out2;
ae=abs(PREDICT_SalePricehat-train_response);
se=abs(PREDICT_SalePricehat-train_response) ** 2;
run;
quit;

proc means data=err_backward_train;
where train=1;
var ae se;
run;

data err_backward_test;
set backward_out2;
*where train=0;
ae=abs(PREDICT_SalePricehat-SalePrice);
se=abs(PREDICT_SalePricehat-SalePrice) ** 2;
run;
quit;

proc means data=err_backward_test;
where train=0;
var ae se;
run;

data backward_op_valdtn_train;
set backward_out2;
where train=1;
pred_grade_val = abs(abs(PREDICT_SalePricehat-train_response)/train_response);
pred_grade= put(pred_grade_val ,perf_sfmt.);
run; quit;

proc freq data=backward_op_valdtn_train;
tables pred_grade;
run;

data backward_op_valdtn_test;
set backward_out2;
where train=0;
pred_grade_val = abs(abs(PREDICT_SalePricehat-SalePrice)/SalePrice);
pred_grade= put(pred_grade_val ,perf_sfmt.);
run; quit;

proc freq data=backward_op_valdtn_test;
tables pred_grade;
run;

/* Model stepwise Selection*/
proc reg data=ames_training outest=stepwise_out1;
model train_response=BsmtFinSF1 FullBath GarageArea GrLivArea MasVnrArea OverallQual TotalSF YearBuilt GarageCars TotRmsAbvGrd YearRemodel GarageYrBlt Fireplaces
/ selection=stepwise slentry=0.1 slstay=0.1 adjrsq aic bic mse rmse cp vif;
output out=stepwise_out2
PREDICTED=PREDICT_SalePricehat
RESIDUAL=RESID_SalePriceresid;
run;

proc print data=stepwise_out2(obs=10);
run;quit;

data err_stepwise_train;
set stepwise_out2;
ae=abs(PREDICT_SalePricehat-train_response);
se=abs(PREDICT_SalePricehat-train_response) ** 2;
run;
quit;

proc means data=err_stepwise_train;
where train=1;
var ae se;
run;

data err_stepwise_test;
set stepwise_out2;
*where train=0;
ae=abs(PREDICT_SalePricehat-SalePrice);
se=abs(PREDICT_SalePricehat-SalePrice) ** 2;
run;
quit;

proc means data=err_stepwise_test;
where train=0;
var ae se;
run;

data stepwise_op_valdtn_train;
set stepwise_out2;
where train=1;
pred_grade_val = abs(abs(PREDICT_SalePricehat-train_response)/train_response);
pred_grade= put(pred_grade_val ,perf_sfmt.);
run; quit;

proc freq data=stepwise_op_valdtn_train;
tables pred_grade;
run;

data backward_op_valdtn_test;
set backward_out2;
where train=0;
pred_grade_val = abs(abs(PREDICT_SalePricehat-SalePrice)/SalePrice);
pred_grade= put(pred_grade_val ,perf_sfmt.);
run; quit;

proc freq data=stepwise_op_valdtn_test;
tables pred_grade;
run;