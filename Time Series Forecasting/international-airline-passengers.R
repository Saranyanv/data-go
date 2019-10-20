library(forecast)
library(tseries)
library(sqldf)
library(fpp)
library(DataCombine)
library(ggplot2)
library (data.table)
library(lubridate)
library(dplyr)
library(chron)
library(RH2)
library(reshape2)
library(devtools)
library(vars)

setwd("C:/Users/Saranyan/Dropbox/MSPA/Time Series Analysis/Week 2")

mydata <- read.csv("international-airline-passengers.csv", header = TRUE, sep = ",", col.names=c("MonYY","Count"))
mytsdata_train <- ts(data = mydata$Count, start = c(1949,1), end = c(1959,12), frequency=12)
mytsdata_test <- ts(data = tail(mydata$Count,12), start = c(1960,1), end = c(1960,12), frequency=12) 


adf.test(mytsdata_train) #### p=0.01, stationarity

kpss.test(mytsdata_train) #### p=0.01, differencing required

ns <- nsdiffs(mytsdata_train)
if(ns > 0) {
  xstar <- diff(x,lag=frequency(mytsdata_train),differences=ns)
} else {
  xstar <- mytsdata_train
}
nd <- ndiffs(xstar)
if(nd > 0) {
  xstar <- diff(xstar,differences=nd)
}


Box.test(mytsdata_train,type="Ljung-Box",lag=24) ###### p=2.2e-16 , so reject null hypotheses, hence auto correlation is present
tsdisplay(mytsdata_train)

fit_arima <- auto.arima(mytsdata_train)
fcast_fit_arima <- forecast(fit_arima, h=12)
fcast_arima_ts <- ts(fcast_fit_arima$mean, start = c(1960,1), end = c(1960,12), frequency=12)
accuracy(fcast_fit_arima,mytsdata_test)

fit_nn <- nnetar(mytsdata_train)
fcast_fit_nn = forecast(fit_nn,h=12)
fcast_nn_ts <- ts(fcast_fit_nn$mean, start = c(1960,1), end = c(1960,12), frequency=12)
accuracy(fcast_fit_nn,mytsdata_test)


VARselect(diff(mytsdata_train), lag.max=8, type="const")$selection
var <- VAR(diff(ts), p=1, type="const")
serial.test(var, lags.pt=10, type="PT.asymptotic") ### p=0.2923, null hypotheses is not rejected
summary(var)
fcst <- forecast(var)
plot(fcst, xlab="Days")
diffinv(fcst$forecast$CVS.Adjusted$mean,xi=ts[600,1])

plot.ts(mytsdata_train, xlim=c(1949, 1961), ylim=c(0, 650), main="International airline passengers: monthly totals in thousands. Jan 49 – Dec 60") 
lines(fcast_arima_ts, col="red")
lines(fcast_nn_ts, col="violet")
lines(mytsdata_test, col="blue")
legend("topleft",legend=c("Forecast from Arima", "Forecast from Neural Nets", "Test Data"),col=c("red", "violet", "blue"), lty=1:2, cex=0.8)


f <- as.formula(paste("Count ~ MonYY", paste(n[!n %in% "medv"], collapse = " + ")))
fit_nn2 <- neuralnet(Count ~ MonYY,data=mytsdata_train,hidden=c(5,3),linear.output=T)



Box.test(residuals(fit_arima),type="Ljung-Box",lag=24, fitdf=4)
Box.test(residuals(fit_nn),type="Ljung-Box",lag=24, fitdf=4)