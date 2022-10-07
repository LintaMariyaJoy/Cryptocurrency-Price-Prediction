library(quantmod)
library(tseries)
library(timeSeries)
library(forecast)
library(lubridate)
library(dplyr)
library(prophet)
library(ggplot2)

print(getwd())
setwd("D:/R")

#read the data
Eth<-read.csv("Ethereum.csv")
View(Eth)
str(Eth)
head(Eth)
tail(Eth)

#change data format
Eth$Date=dmy(Eth$Date)
View(Eth)

#ADF test-Augmented Dickey Fuller Test to check it is stationary or not.null hypothesis is data is non stationary. Time series is non stationary data. If p value greater than or equal to 0.5 accept null hypothesis
Predict_Price=Eth$Close
Predict_Price
print(adf.test(Predict_Price))# accept null hypothesis, ie; Eth is non stationary data 

#Visualization
qplot(Date, Close, data=Eth,
      main="Ethereum Close price in 2015-2019")

#data is skewed-Log transformation
ds <- Eth$Date
y <- log(Eth$Close)
head(y)
df <- data.frame(ds, y)
qplot(ds, y, data=df,
      main="Ethereum Close price log transformation")

#PROPHET MODEL

#Prophet package Automatic Forecasting Procedure. Implements a procedure for forecasting time series data based on an additive model where non-linear trends are fit with yearly, weekly, and daily seasonality, plus holiday effects.
#prophet must contain ds and y with date and other variable
m <- prophet(df)
str(m)
future <- make_future_dataframe(m, periods = 365)#next 365 day future prediction
forecast <- predict(m, future)
head(forecast)
str(forecast)#yhat is the predicted value

#plot forecast
plot(m,forecast)
dyplot.prophet(m,forecast)
prophet_plot_components(m,forecast)

#model performance
pred <- forecast$yhat[1:1544]
head(pred)
actual <- m$history$y
head(actual)
plot(actual, pred)#it is linear

d<-data.frame(predicted=c(forecast$yhat[1:1544]),actual=c(m$history$y))
View(d)
head(d)
mean((d$actual - d$predicted)^2)
sqrt(mean((d$actual - d$predicted)^2))

#ARIMA MODEL

Eth_data<-ts(Eth$Close,frequency = 365,start = c(2015,8))
plot(Eth_data)

Ethdata <- 100*diff(log(Eth_data))
Eth_train <- Ethdata[1:(0.9*length(Ethdata))]
Eth_test <- Ethdata[(0.9*length(Ethdata)+1):length(Ethdata)]
arimamodel<-auto.arima(Eth_train)
fit <- Arima(Eth_train, order = c(1,0,0))
preds <- predict(fit, n.ahead = (length(Ethdata) - (0.9*length(Ethdata))))$pred
preds
forecast1<-forecast(fit,h=17)
forecast1
plot(forecast1)
accuracy(preds,Eth_test)

