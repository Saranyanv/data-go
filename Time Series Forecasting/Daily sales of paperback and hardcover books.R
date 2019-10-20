7.8.1
library(fma)
mydata <- books
plot.ts(mydata, main="Daily sales of paperback and hardcover books")

7.8.2
fit1 <- ses(mydata[,1], alpha=0.1, h=4, initial="simple")
fit2 <- ses(mydata[,1], alpha=0.2, h=4, initial="simple")
fit3 <- ses(mydata[,1], alpha=0.3, h=4, initial="simple")
fit4 <- ses(mydata[,1], alpha=0.4, h=4, initial="simple")
fit5 <- ses(mydata[,1], alpha=0.5, h=4, initial="simple")
fit6 <- ses(mydata[,1], alpha=0.6, h=4, initial="simple")
fit7 <- ses(mydata[,1], alpha=0.7, h=4, initial="simple")
fit8 <- ses(mydata[,1], alpha=0.8, h=4, initial="simple")
fit9 <- ses(mydata[,1], alpha=0.9, h=4, initial="simple")

sse <- c(sum((mydata[,1]-fitted(fit1)))^2, sum((mydata[,1]-fitted(fit2)))^2, sum((mydata[,1]-fitted(fit3)))^2, sum((mydata[,1]-fitted(fit4)))^2,
sum((mydata[,1]-fitted(fit5)))^2, sum((mydata[,1]-fitted(fit6)))^2, sum((mydata[,1]-fitted(fit7)))^2, sum((mydata[,1]-fitted(fit8)))^2,
sum((mydata[,1]-fitted(fit9)))^2)

alpha <- c(0.1,0.2,0.3,0.4,0.5,0.6,0.7,0.8,0.9)

plot(alpha, sse, main="Alpha values to SSE")


7.8.3



fit_true_optimal <- ses(mydata[,1], initial='optimal', h=4)
sum((mydata[,1]-fitted(fit_true_optimal)))^2





fit1 <- ses(mydata[,2], alpha=0.1, h=4, initial="optimal")
fit2 <- ses(mydata[,2], alpha=0.2, h=4, initial="optimal")
fit3 <- ses(mydata[,2], alpha=0.3, h=4, initial="optimal")
fit4 <- ses(mydata[,2], alpha=0.4, h=4, initial="optimal")
fit5 <- ses(mydata[,2], alpha=0.5, h=4, initial="optimal")
fit6 <- ses(mydata[,2], alpha=0.6, h=4, initial="optimal")
fit7 <- ses(mydata[,2], alpha=0.7, h=4, initial="optimal")
fit8 <- ses(mydata[,2], alpha=0.8, h=4, initial="optimal")
fit9 <- ses(mydata[,2], alpha=0.9, h=4, initial="optimal")


sse <- c(sum((mydata[,2]-fitted(fit1)))^2, sum((mydata[,2]-fitted(fit2)))^2, sum((mydata[,2]-fitted(fit3)))^2, sum((mydata[,2]-fitted(fit4)))^2,
sum((mydata[,2]-fitted(fit5)))^2, sum((mydata[,2]-fitted(fit6)))^2, sum((mydata[,2]-fitted(fit7)))^2, sum((mydata[,2]-fitted(fit8)))^2,
sum((mydata[,2]-fitted(fit9)))^2)


alpha <- c(0.1,0.2,0.3,0.4,0.5,0.6,0.7,0.8,0.9)

plot(alpha, sse, main="Alpha values to SSE - HARDCOVER")




fit_true_optimal <- ses(mydata[,2], initial='optimal', h=4)
sum((mydata[,1]-fitted(fit_true_optimal)))^2



fit_optimal <- ses(mydata[,2], initial='simple', h=4)
fit_guess <- ses(mydata[,2], alpha=0.9, h=4, initial="simple")
par(mfrow=c(2,1))
plot(fit_optimal, main="Hardcover - optimal fit")
plot(fit_guess, main="Hardcover - best value identified fit")
sum((mydata[,1]-fitted(fit_optimal)))^2


fit_true_optimal <- ses(mydata[,2], initial='optimal', h=4)
sum((mydata[,2]-fitted(fit_true_optimal)))^2




fit_paperback_holt <- holt(mydata[,1], initial = "simple", h=4)
sse_paperback_holt <- sum((mydata[,1]-fitted(fit_paperback_holt)))^2


fit_hardcover_holt <- holt(mydata[,2], initial = "simple", h=4)
sse_hardcover_holt <- sum((mydata[,2]-fitted(fit_hardcover_holt)))^2


fit_ses_paperback <- ses(mydata[,1], h=4, initial="simple")
sse_ses_paperback <- sum((mydata[,1]-fitted(fit_ses_paperback)))^2


fit_ses_hardcover <- ses(mydata[,2], h=4, initial="simple")
sse_ses_hardcover <- sum((mydata[,2]-fitted(fit_ses_hardcover)))^2

sses <- c(sse_paperback_holt, sse_hardcover_holt, sse_ses_paperback, sse_ses_hardcover)



plot(mydata[,1], main="Daily sales of paperback books", ylab="Sales", xlim=c(0,37))
lines(fitted(fit), col="red", type="o")
lines(fit$mean, col="green", type="o")
legend("topleft",legend=c("Actuals","Fitted","Forecasted"),col=c("black", "red", "green"), lty=1:2, cex=0.8)



fit_paperback_holt <- holt(mydata[,1], initial = "simple", h=4)
fit_hardcover_holt <- holt(mydata[,2], initial = "simple", h=4)
fit_ses_paperback <- ses(mydata[,2], h=4, initial="simple")
fit_ses_hardcover <- ses(mydata[,2], h=4, initial="simple")

par(mfrow=c(2,2))
plot(mydata[,1], main="paperback (holt)", ylab="Sales", xlim=c(0,37), ylim=c(120,300))
lines(fit_paperback_holt$mean, col="green", type="o")
legend("topleft",legend=c("Actuals","Forecasted"),col=c("black", "green"), lty=1:2, cex=0.8)

plot(mydata[,1], main="paperback (ses)", ylab="Sales", xlim=c(0,37))
lines(fit_ses_paperback$mean, col="green", type="o")

plot(mydata[,2], main="hardcover (holt)", ylab="Sales", xlim=c(0,37), ylim=c(120,300))
lines(fit_hardcover_holt$mean, col="green", type="o")

plot(mydata[,2], main="hardcover (ses)", ylab="Sales", xlim=c(0,37))
lines(fit_ses_hardcover$mean, col="green", type="o")


fit_paperback_holt <- holt(mydata[,1], initial = "simple", h=4)
fit_hardcover_holt <- holt(mydata[,2], initial = "simple", h=4)
fit_ses_paperback <- ses(mydata[,1], h=4, initial="simple")
fit_ses_hardcover <- ses(mydata[,2], h=4, initial="simple")
summary(fit_paperback_holt)
summary(fit_hardcover_holt)
summary(fit_ses_paperback)
summary(fit_ses_hardcover)


library(fpp)
mydata <- ukcars
plot.ts(mydata, main="Quarterly UK passenger vehicle production data from 1977:1--2005:1")



decompose_mydata <- stl(mydata, s.window="periodic", robust=TRUE)
seasadj(decompose_mydata)



fit1 <- holt(mydata, h=8, damped = TRUE)
reseason_fcast <- fit$mean + rep(decompose_mydata$time.series[110:113,"seasonal"],2)
summary(fit)


fit2 <- holt(mydata, h=8)
reseason_fcast <- fit$mean + rep(decompose_mydata$time.series[110:113,"seasonal"],2)
summary(fit)

fit1 <- holt(mydata, h=8, damped = TRUE)
fit2 <- holt(mydata, h=8)
fit3 <- ets(mydata)
plot.ts(mydata, main="Quarterly UK passenger vehicle production data from 1977:1--2005:1")
lines(forecast(fit1, h=4)$mean,col="red")
lines(forecast(fit2, h=4)$mean,col="blue")
lines(forecast(fit3, h=4)$mean,col="violet")
legend("topleft",legend=c("Holt(dampen)","Holt","ETS"),col=c("red", "blue", "violet"), lty=1:2, cex=0.8)




mydata <- visitors
plot.ts(mydata, main="Monthly Australian short-term overseas visitors data, May 1985--April 2005")


hw(mydata, h=24, seasonal='multiplicative')

fit <- hw(mydata, h=24, seasonal='multiplicative')
fit1 <- hw(mydata, h=24, seasonal='multiplicative', damped=TRUE)
fit2 <- hw(mydata, h=24, seasonal='multiplicative', exponential=TRUE)
plot.ts(mydata, main="Monthly Australian short-term overseas visitors data, May 1985--April 2005")
lines(forecast(fit, h=4)$mean,col="red")
lines(forecast(fit1, h=4)$mean,col="blue")
lines(forecast(fit2, h=4)$mean,col="violet")
legend("topleft",legend=c("dampen)","Holt"),col=c("red", "blue"), lty=1:2, cex=0.8)



Holt-Winters' multiplicative method = 14.8295
Damped Holt-Winters' multiplicative method = 14.44801
Holt-Winters' multiplicative method with exponential trend = 14.49416




fit1 <- holt(mydata, seasonal='multiplicative')
fit2 <- ets(mydata)
fit3 <- ets(BoxCox(mydata, BoxCox.lambda(mydata)), model='AAZ')
fit4 <- snaive(BoxCox(mydata, BoxCox.lambda(mydata)))

decompose_mydata <- decompose(BoxCox(mydata, BoxCox.lambda(mydata)))
seasonal_mydata <- decompose_mydata$seasonal
fit5 <- ets(seasonal_mydata)


par(mfrow=c(3,2))
plot(fit1$residuals)
plot(fit2$residuals)
plot(fit3$residuals)
plot(fit4$residuals)
plot(fit5$residuals)



y1 <- ts(numeric(100))
e <- rnorm(100)
for(i in 2:100)
  y1[i] <- 0.6*y1[i-1] + e[i]

  
y2 <- ts(numeric(100))
e <- rnorm(100)
for(i in 2:100)
  y2[i] <- 0.6*y2[i-1] + e[i]
  

y3 <- ts(numeric(100))
e <- rnorm(100)
for(i in 2:100)
  y3[i] <- 0.6*y3[i-1] + e[i]
  
  
  
plot(y1, main="Exercise - 8.11.5b - 0.6")
plot(y2, main="Exercise - 8.11.5b - 0.7")
plot(y3, main="Exercise - 8.11.5b - 0.8")


mydata <- wmurders
tsdisplay(mydata)
adf.test(mydata)

diff_mydata <- diff(mydata)
tsdisplay(diff_mydata)

adf.test(diff_mydata)




y[i] <- 0.6*y[i-1] + e[i]
y[i] <- e[i] + theta[i]*e[i-1]


y <- ts(numeric(100))
e <- rnorm(100)
for(i in 2:100)
  y[i] <- e[i] + 0.6*e[i-1]

  
phi=0.6
theta=0.6
y <- ts(numeric(100))
  e <- rnorm(100)
  for (i in 2:100)
    y[i] <- phi*y[i-1] + theta*e[i-1] + e[i]

phi1=-0.8
phi2=0.3
z <- ts(numeric(100))
  e <- rnorm(100)
  for(i in 3: 100)
    z[i] <- phi1*z[i-1] + phi2*z[i-2] + e[i]

    
    
par(mfrow=c(2,1))
plot(y, main="ARMA(1,1)")
plot(z, main="AR(2)")
    
y1 <- ts(numeric(100))
e <- rnorm(100)
for(i in 2:100)
  y1[i] <- e[i] + 0.6*e[i-1]

  
y2 <- ts(numeric(100))
e <- rnorm(100)
for(i in 2:100)
  y2[i] <- e[i] + 0.7*e[i-1]
  

y3 <- ts(numeric(100))
e <- rnorm(100)
for(i in 2:100)
  y3[i] <- e[i] + 0.8*e[i-1]
  
  
par(mfrow=c(3,1))
plot(y1, main="Exercise - 8.11.5d - 0.6")
plot(y2, main="Exercise - 8.11.5d - 0.7")
plot(y3, main="Exercise - 8.11.5d - 0.8")


library(fpp)
mydata <- wmurders
diff_mydata <- diff(mydata)
fit <- Arima(diff_mydata, order=c(1,1,1))
tsdisplay(residuals(fit))
plot(fit$residuals, main="Residuals plot")



mydata <- austourists
plot.ts(mydata, main="Quarterly number of international tourists to Australia for the period 1999–2010")


plot(diff(mydata , lag=4, differences=1))
Acf(diff(mydata , lag=4, differences=1))
Pacf(diff(mydata , lag=4, differences=1))


mydata <- usmelec

plot(mydata, main="Total net generation of electricity (in billion kilowatt hours)", ylab="Billions of kilowatt hours (kWh)")
lines(ma(mydata, order=12), col="blue")


lambda_val <- BoxCox.lambda(mydata)
mydata_boxcox <- BoxCox(mydata, lambda=lambda_val)
plot(mydata_boxcox, main="Total net generation of electricity (in billion kilowatt hours)", ylab="Billions of kilowatt hours (kWh)")


plot(diff(mydata,12), main="Total net generation of electricity (in billion kilowatt hours)", ylab="Billions of kilowatt hours (kWh)")
adf.test(diff(mydata,12), alternative = "stationary")
kpss.test(diff(mydata,12))
plot(diff(diff(mydata,12)), main="Total net generation of electricity (in billion kilowatt hours)", ylab="Billions of kilowatt hours (kWh)")



ns <- nsdiffs(mydata)
if(ns > 0) {
  xstar <- diff(x,lag=frequency(x),differences=ns)
} else {
  xstar <- mydata
}
nd <- ndiffs(xstar)
if(nd > 0) {
  xstar <- diff(xstar,differences=nd)
}



tsdisplay(diff(diff(mydata,12)))
fit1 <- arima(mydata, order=c(2,2,0), seasonal=c(0,1,1))
fit2 <- arima(mydata, order=c(2,2,1), seasonal=c(0,1,1))
fit3 <- arima(mydata, order=c(1,2,0), seasonal=c(0,1,1))
fit1$aic
fit2$aic
fit3$aic


tsdisplay(fit2$residuals)
Box.test(residuals(fit2), lag=24, fitdf=4, type="Ljung")


fcast <- forecast(fit4, h=180)
plot(fcast, main="FORECAST - Total net generation of electricity (in billion kilowatt hours)")


setwd("C:/Users/Saranyan/Dropbox/MSPA/Time Series Analysis/Homework/HW2")
actual_elec <- read.csv("electricity-overview.csv", header = TRUE, sep = ",", col.names=c("Month","Electricity"), stringsAsFactors = FALSE)
mytsdata_actual <- ts(actual_elec$Electricity, start=c(1973, 1), frequency = 12)


plot(fcast, main="FORECAST - Total net generation of electricity (in billion kilowatt hours)")
lines(mytsdata_actual, col="red")