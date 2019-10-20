##### https://www.ncdc.noaa.gov/cdo-web/confirmation

library(forecast)
library(tseries)
library(sqldf)
library(fpp)
library(DataCombine)
library(ggplot2)
library(data.table)
library(lubridate)
library(dplyr)
library(chron)
library(RH2)
library(reshape2)
library(vars)
library(xts)
library(FinTS)
library(aTSA)
library(rugarch)

setwd("C:/Users/Saranyan/Dropbox/MSPA/Time Series Analysis/Homework/HW3/SrcFiles")
flightdelays <- read.csv("airlines_delay_causes.csv", header = TRUE, sep = ",", col.names=c("year","X.month","carrier","carrier_name","airport","airport_name","arr_flights","arr_del15","carrier_ct","X.weather_ct","nas_ct","security_ct","late_aircraft_ct","arr_cancelled","arr_diverted","X.arr_delay","X.carrier_delay","weather_delay","nas_delay","security_delay","late_aircraft_delay"), stringsAsFactors = FALSE)
flightdelays$YrMon <- flightdelays$year*100 + flightdelays$X.month

Weather_Boston_MA <- read.csv("Weather_Boston_MA.csv", header = TRUE, sep = ",", col.names=c("STATION","NAME","DATE","AWND","CDSD","CLDD","DP01","DP10","DSNW","DT00","DT32","DX32","DX70","DX90","EMNT","EMSN","EMXP","EMXT","HDSD","HTDD","PRCP","SNOW","TAVG","TMAX","TMIN","WDF2","WDF5","WSF2","WSF5"), stringsAsFactors = FALSE)

flightdelays_sum=aggregate(weather_delay~YrMon,FUN=sum,data=flightdelays)

flightdelays_ts=ts(flightdelays_sum$weather_delay, start = c(2010,01), end=c(2018,03), frequency=12)
Weather_Boston_MA_ts=ts(Weather_Boston_MA$TAVG, start = c(2010,01), end=c(2018,03), frequency=12)

adf.test(flightdelays_ts) ##### p-value = 0.01 - indicates stationarity
kpss.test(flightdelays_ts) #### p-value = 0.07326, its marginal, so differencing may not be required

ns <- nsdiffs(flightdelays_ts)
if(ns > 0) {
  xstar <- diff(flightdelays_ts,lag=frequency(flightdelays_ts),differences=ns)
} else {
  xstar <- flightdelays_ts
}
nd <- ndiffs(xstar)
if(nd > 0) {
  xstar <- diff(xstar,differences=nd)
}



Box.test(flightdelays_ts,type="Ljung-Box",lag=20) ###### p=9.154e-06 , so reject null hypotheses, hence auto correlation is present
par(mfcol=c(1,2))
Acf(flightdelays_ts)
Pacf(flightdelays_ts)


flightdelays_ts_train <- ts(flightdelays_sum$weather_delay, start = c(2010,01), end=c(2017,12), frequency=12)

test_df <- flightdelays_sum$weather_delay[97:99]
flightdelays_ts_test <- ts(flightdelays_sum$weather_delay[97:99], start = c(2018,01), end=c(2018,03), frequency=12)

merge <- ts.union(flightdelays_ts_train, Weather_Boston_MA_ts[1:96])

##### MODEL 1 :  ARIMA #####
fit_arima <- auto.arima(flightdelays_ts_train)
fcast_fit_arima <- forecast(fit_arima, h=12)
fcast_ts <- ts(fcast_fit_arima$mean, start=c(2018,01), end=c(2018,03), frequency=12)


##### MODEL 2 :  ARIMA WITH EXTERNAL REGRESSOR #####
reg_train <- as.numeric(Weather_Boston_MA_ts[1:96])

fit_arima_xreg <- auto.arima(flightdelays_ts_train, xreg = reg_train)
fcast_fit_arima_xreg <- forecast(fit_arima_xreg, h=12, xreg=reg_train)
fcast_ts_xreg <- ts(fcast_fit_arima_xreg$mean, start=c(2018,01), end=c(2018,03), frequency=12)


##### MODEL 3 :  VECTOR AUTOREGRESSION MODEL #####
adf.test(merge[,1], alternative = "stationary") #### p=0.01, stationarity
adf.test(merge[,2], alternative = "stationary") #### p=0.01, stationarity
kpss.test(merge[,1]) #### p=0.1, differencing not required
kpss.test(merge[,2]) #### p=0.1, differencing not required


VARselect(merge, lag.max=8, type="const")$selection
var <- VAR(merge, p=1, type="const") #### p-value < 2.2e-16
var <- VAR(merge, p=2, type="const") #### p-value = 0.01601
var <- VAR(merge, p=3, type="const") #### p-value = 0.00143
var <- VAR(merge, p=4, type="const") #### p-value = 0.007587
var <- VAR(merge, p=5, type="const") #### p-value = 0.2088
serial.test(var, lags.pt=10, type="PT.asymptotic") ### p=0.2088, null hypotheses is not rejected
summary(var)
fcst <- forecast(var)
fcast_ts_var <- ts(fcst$forecast$flightdelays_ts_train$mean[1:3], start=c(2018,01), end=c(2018,03), frequency=12)

var_residuals <- resid(var)
par(mfrow=c(2,1))
acf(var_residuals[,1])
acf(var_residuals[,2])


##### MODEL 4 :  NEURAL NETS MODEL #####

fit_nn <- nnetar(flightdelays_ts_train)
fcast_fit_nn = forecast(fit_nn,h=3)
fcast_ts_nn <- ts(fcast_fit_nn$mean, start=c(2018,01), end=c(2018,03), frequency=12)


##### MODEL 5 :  GARCH MODEL #####


### Check if varaiance is changing at different points in time
Box.test((flightdelays_sum$weather_delay^2),type="Ljung-Box",lag=12) ### p=0.004694, so reject null hypotheses, so auto correlations exists

We do not proceed further with GARCH models because sample size is less than 100. The rugarch package prohibits this, since output is not meaningful and doesnt converge.

##### MODEL 6 :  Zero Inflated Logistic MODEL #####

Weather_Boston_MA_DSNW <- Weather_Boston_MA[,c("DATE","DSNW")]
names(Weather_Boston_MA_DSNW) <- c("YrMon", "snowfall_days")

merge_dsnw <- merge(x = flightdelays_sum, y = Weather_Boston_MA_DSNW[1:99,], by = c("YrMon"))
Snowed_Ind <- as.data.frame(sqldf("select YrMon, case when snowfall_days > 0 then 1 else 0 end from merge_dsnw"))
names(Snowed_Ind) <- c("YrMon", "Snowed_Ind")
merge_dsnw <- merge(x = merge_dsnw, y = Snowed_Ind, by = c("YrMon"))


###merge_dsnw$YrMon <- as.factor(merge_dsnw$YrMon)
###merge_dsnw$Mon <- as.factor(merge_dsnw$YrMon %% 100)
###merge_dsnw$Yr <- as.factor((merge_dsnw$YrMon - (merge_dsnw$YrMon %% 100)) / 100)

M1 <- glm(Snowed_Ind ~ YrMon, data=merge_dsnw[1:96,], family=binomial(link=logit))
M2 <- glm(weather_delay ~ YrMon + snowfall_days, data=subset(merge_dsnw[1:96,], Snowed_Ind==1), family=Gamma(link=log))

YrMon=c(201801, 201802, 201803)
snowfall_days=c(5,2,3)
test_df <- data.frame(YrMon, snowfall_days)
###test_df$YrMon <- as.factor(test_df$YrMon)

###M1$xlevels[["201801"]] <- union(M1$xlevels[["201801"]], levels(test$y))

pred1 <- exp(predict(M1, newdata=test_df, type="response"))
pred2 <- predict(M2, newdata=test_df, type="response")
pred = pred1 * pred2


fcast_ts_ZI <- ts(pred, start = c(2018,01), end=c(2018,03), frequency=12)


#### Residuals Test ####

accuracy(fcast_fit_arima, flightdelays_ts_test)
accuracy(fcast_fit_arima_xreg, flightdelays_ts_test)
accuracy(var$varresult[[1]])
accuracy(fcst$forecast$flightdelays_ts_train$mean, flightdelays_ts_test)
accuracy(fcast_fit_nn, flightdelays_ts_test)
accuracy(fcast_ts_ZI, flightdelays_ts_test)

par(mfrow=c(2,2))
tsdisplay(residuals(fit_arima))
tsdisplay(residuals(fit_arima_xreg))
tsdisplay(resid(var))
tsdisplay(residuals(fit_nn))


Box.test(residuals(fit_arima), lag=12, fitdf=6, type="Ljung")
Box.test(residuals(fit_arima_xreg), lag=12, fitdf=6, type="Ljung")
Box.test(resid(var)[,1], lag=12, fitdf=6, type="Ljung")
Box.test(residuals(fit_nn), lag=12, fitdf=6, type="Ljung")
Box.test(residuals(fit_nn), lag=12, fitdf=6, type="Ljung")

par(mfrow=c(2,2))
hist(residuals(fit_arima))
hist(residuals(fit_arima_xreg))
hist(resid(var))
hist(residuals(fit_nn))

##### Forecast #####

plot.ts(flightdelays_ts_train, xlim=c(2010, 2018.5), main="Flight Delays in Boston, MA since January 2010")
lines(fcast_ts, col="red")
lines(fcast_ts_xreg, col="blue")
lines(fcast_ts_var, col="magenta")
lines(fcast_ts_nn, col="turquoise1")
lines(fcast_ts_ZI, col="gold")
lines(flightdelays_ts_test, col="lawngreen")
legend("topleft",legend=c("Forecast from Arima", "Forecast from Arima with xreg", "Forecast from VAR", "Forecast from Neural Nets", "Forecast from Zero-Inflated", "Test Data"),col=c("red", "blue", "magenta", "turquoise1", "gold", "lawngreen"), lty=1:1, cex=0.6)


ts.union(fcast_ts, fcast_ts_xreg, fcast_ts_var, fcast_ts_nn, fcast_ts_ZI, flightdelays_ts_test)
