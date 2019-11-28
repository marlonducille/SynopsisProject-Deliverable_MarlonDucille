
library(tseries)
library(forecast)

setwd("D:/DataAnalysis/Synopsis Project 2/Data")

df <- read.csv('PrescribingInfo-TimeData.csv')
#df <- df[order(df$Date),]
df <- na.omit(df)

df_ts <- ts(df$ActualCost, start=c(2017, 9), frequency = 12, end=c(2019, 9))
plot(df_ts)

decomposeAP <- decompose(df_ts,"additive")
autoplot(decomposeAP)

#diff <- diff(crime_ts)
#plot(diff)

#acf(diff)
#pacf(diff)

model <- auto.arima(df_ts, ic = 'aic', trace = TRUE);model

forecast <- forecast(model, level = c(95), h = 12)
autoplot(forecast)