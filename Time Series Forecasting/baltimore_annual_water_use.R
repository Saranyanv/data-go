setwd("C:/Users/Saranyan/Dropbox/MSPA/Time Series Analysis/Week 3")

mydata <- read.csv("baltmore-city-annual-water-use-l.csv", header = TRUE, sep = ",", col.names=c("YYYY","Count"))


mytsdata <- ts(data = mydata$Count, start = c(1885), end = c(1963), frequency=1)


plot.ts(mydata)



library(forecast)
fit_AdAN <- ets(mytsdata, model="AAN", damped=TRUE)
fit_AAN <- ets(mytsdata, model="AAN")
fit_ZZZ <- ets(mytsdata, model="ZZZ")


fit_MAN <- ets(mytsdata_train, model="MAN")

plot(forecast(fit_MMN, h=30))
lines(mytsdata_test)


par(mfrow=c(3,1))
plot(forecast(fit_ZZZ), col="red", ylab="Liters per capita per day", main="Forecast from ETS(A,N,N) (Autoselected)")
lines(fitted(fit_ZZZ),col="green")
legend("topleft",legend=c("Actual","Fitted","Forecast"),col=c("red", "green", "blue"), lty=1:2, cex=0.8)

plot(forecast(fit_AdAN), col="red", ylab="Liters per capita per day")
lines(fitted(fit_AdAN),col="green")
#legend("topright",legend=c("Actual","Fitted","Forecast"),col=c("red", "green", "blue"), lty=1:2, cex=0.8)

plot(forecast(fit_AAN), col="red", ylab="Liters per capita per day")
lines(fitted(fit_AAN),col="green")
#legend("topright",legend=c("Actual","Fitted","Forecast"),col=c("red", "green", "blue"), lty=1:2, cex=0.8)


mytsdata_train <- window(mytsdata, start=1885, end=1950)
mytsdata_test <- window(mytsdata, start=1951)

fit_ANN_train <- ets(mytsdata_train, model="ANN")
fit_AAdN_train <- ets(mytsdata_train, model="AAN", damped=TRUE)

par(mfrow=c(2,1))
plot(forecast(fit_AAdN_train), col="green", ylab="Liters per capita per day")
lines(mytsdata_test,col="red")
legend("topleft",legend=c("Actual(Train)","Actual(Test)","Forecast"),col=c("green", "red", "blue"), lty=1:2, cex=0.8)

plot(forecast(fit_ANN_train), col="green", ylab="Liters per capita per day")
lines(mytsdata_test,col="red")
#legend("topright",legend=c("Actual(Train)","Actual(Test)","Forecast"),col=c("green", "red", "blue"), lty=1:2, cex=0.8)
