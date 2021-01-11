library(TSA)
data("beersales")
ts.plot(beersales,main="Monthly Beer Sales in Millions Of Barrels from Jan1975-Dec1990")
trainingbeer=beersales[1:179]

testingbeer=beersales[180:192]
length(testingbeer)
BoxCox.ts(trainingbeer)
library(tseries)
install.packages("tseries")
adf.test(trainingbeer)
par(mfrow=c(2,1))
acf(trainingbeer)
pacf(trainingbeer)
ts.plot(trainingbeer)
stbeer=diff(trainingbeer,lag=12) #deseasonalized
acf(stbeer)
pacf(stbeer)
install.packages("TSA")
library(TSA)
periodogram(trainingbeer)
m1=arima(trainingbeer,order=c(1,0,1),seasonal=list(order=c(1,1,1),period=12))
m1
tsdiag(m1)
m2=arima(trainingbeer,order=c(1,0,1),seasonal=list(order=c(2,1,1),period=12))
m2
tsdiag(m2)
m3=arima(trainingbeer,order=c(1,0,1),seasonal=list(order=c(3,1,1),period=12))
m3
tsdiag(m3)

#forecast
p1=predict(m1,13)
p2=predict(m2,13)
p3=predict(m3,13)

#MAPE
MAPE1=mean(abs((testingbeer-p1$pred)/testingbeer))
MAPE2=mean(abs((testingbeer-p2$pred)/testingbeer))
MAPE3=mean(abs((testingbeer-p3$pred)/testingbeer))


#Decomposition
trainingbeer=ts(trainingbeer, freq=12)
decompbeer=decompose(trainingbeer,type="additive")
plot(decompbeer)
decompbeer

#Lowess
lowessbeer=stl(trainingbeer,s.window="periodic")
plot(lowessbeer)

#4 different forecasts
library(forecast)
?forecast
m1=forecast(lowessbeer,method="arima",h=13)
m2=forecast(lowessbeer,method="ets",h=13)
m3=forecast(lowessbeer,method="naive",h=13)
m4=forecast(lowessbeer,method="rwdrift",h=13)
MAPE1=mean(abs((testingbeer-m1$mean[1:13])/testingbeer))
MAPE2=mean(abs((testingbeer-m2$mean[1:13])/testingbeer))
MAPE3=mean(abs((testingbeer-m3$mean[1:13])/testingbeer))
MAPE4=mean(abs((testingbeer-m4$mean[1:13])/testingbeer))
#smoothing
fit1=ses(trainingbeer,initial="simple",h=13)
summary(fit1)
fit2=holt(trainingbeer,initial = "simple",h=13)
summary(fit2)


