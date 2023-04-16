library(VNDS)
library(tidyverse)
library(ggplot2)
library(yuima)
#Import the stock price of VND
BID = tq_get(symbol = 'BID', from = '2017-01-01', to = '2022-09-30',
             src = 'CAFEF')
#Plotting the Close Price of VND in that time interval 
BID = as.data.frame(BID)
plot(BID$date, BID$close, type = 'l', col = 'red',
     xlab = 'Time', ylab = 'Closing Price', main = 'BID')
#Import the stock price of VNINDEX
VNINDEX = tq_get(symbol = 'VNINDEX', from = '2017-01-01', to = '2022-09-30',
                 src = 'CAFEF')
#Plotting the Close Price of VN_Index in that time interval 
VNINDEX = as.data.frame(VNINDEX)
plot(VNINDEX$date, VNINDEX$close, type = 'l', col = 'blue',
     xlab = 'Time', ylab = 'Closing Price', main = 'VNINDEX')
#Log-return
Riskfree = 0.05118 #VietNam 10-years Goverment Bond (4th Dec)
BID$close = c(NA, 100 * diff(log(VND$close))) #Return of Asset (Rm-Rf=Ra )
VNINDEX$close = c(NA, 100 * diff(log(VNINDEX$close))) - Riskfree #(Rmarket-Rfree)
summary(BID[c('close')])
summary(VNINDEX[c('close')])
#Check length
length(BID$close)
length(VNINDEX$close)
data = merge(BID, VNINDEX, by = 'date') #VNI~x; VN_INDEX~y
data
#Linear Model
lm_CAPM = lm(close.x ~ close.y, data = data)
summary(lm_CAPM)
#Plotting Linear Regression
plot(data$close.y, data$close.x, col = 'blue',
     xlab = 'VN_INDEX Closing Price', ylab = 'BID Closing Price', main = 'LINEAR REGRESSION')
alpha = lm_CAPM$coefficients[1]
beta = lm_CAPM$coefficients[2]
lines(data$close.y[1:length(data$close.y)-1], alpha + beta * data$close.y[1:length(data$close.y)-1], lwd = 1, col = 'red')
