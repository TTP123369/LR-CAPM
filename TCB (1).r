library(VNDS)
library(tidyverse)
library(ggplot2)
library(yuima)
#Import the stock price of VND
TCB = tq_get(symbol = 'TCB', from = '2017-01-01', to = '2022-09-30',
             src = 'CAFEF')
#Plotting the Close Price of VND in that time interval 
TCB = as.data.frame(TCB)
plot(TCB$date, TCB$close, type = 'l', col = 'red',
     xlab = 'Time', ylab = 'Closing Price', main = 'TCB')
#Import the stock price of VNINDEX
VNINDEX = tq_get(symbol = 'VNINDEX', from = '2017-01-01', to = '2022-09-30',
                 src = 'CAFEF')
#Plotting the Close Price of VN_Index in that time interval 
VNINDEX = as.data.frame(VNINDEX)
plot(VNINDEX$date, VNINDEX$close, type = 'l', col = 'blue',
     xlab = 'Time', ylab = 'Closing Price', main = 'VNINDEX')
#Log-return
Riskfree = 0.05118 #VietNam 10-years Goverment Bond (4th Dec)
TCB$close = c(NA, 100 * diff(log(TCB$close))) #Return of Asset (Rm-Rf=Ra )
VNINDEX$close = c(NA, 100 * diff(log(VNINDEX$close))) - Riskfree #(Rmarket-Rfree)
summary(TCB[c('close')])
summary(VNINDEX[c('close')])
#Check length
length(TCB$close)
length(VNINDEX$close)
data = merge(VND, VNINDEX, by = 'date') #VNI~x; VN_INDEX~y
data
#Linear Model
lm_CAPM = lm(close.x ~ close.y, data = data)
summary(lm_CAPM)
#Plotting Linear Regression
plot(data$close.y, data$close.x, col = 'blue',
     xlab = 'VN_INDEX Closing Price', ylab = 'TCB Closing Price', main = 'LINEAR REGRESSION')
alpha = lm_CAPM$coefficients[1]
beta = lm_CAPM$coefficients[2]
lines(data$close.y[1:length(data$close.y)-1], alpha + beta * data$close.y[1:length(data$close.y)-1], lwd = 1, col = 'red')
