library(VNDS)
library(tidyverse)
library(ggplot2)
library(yuima)
#Import the stock price of VND
VCB = tq_get(symbol = 'VCB', from = '2017-01-01', to = '2022-09-30',
             src = 'CAFEF')
#Plotting the Close Price of VND in that time interval 
VCB = as.data.frame(VCB)
plot(VCB$date, VCB$close, type = 'l', col = 'red',
     xlab = 'Time', ylab = 'Closing Price', main = 'VCB')
#Import the stock price of VNINDEX
VNINDEX = tq_get(symbol = 'VNINDEX', from = '2017-01-01', to = '2022-09-30',
                 src = 'CAFEF')
#Plotting the Close Price of VN_Index in that time interval 
VNINDEX = as.data.frame(VNINDEX)
plot(VNINDEX$date, VNINDEX$close, type = 'l', col = 'blue',
     xlab = 'Time', ylab = 'Closing Price', main = 'VNINDEX')
#Log-return
Riskfree = 0.05118 #VietNam 10-years Goverment Bond (4th Dec)
VCB$close = c(NA, 100 * diff(log(VCB$close))) #Return of Asset (Rm-Rf=Ra )
VNINDEX$close = c(NA, 100 * diff(log(VNINDEX$close))) - Riskfree #(Rmarket-Rfree)
summary(VCB[c('close')])
summary(VNINDEX[c('close')])
#Check length
length(VCB$close)
length(VNINDEX$close)
data = merge(VCB, VNINDEX, by = 'date') #VNI~x; VN_INDEX~y
data
#Linear Model
lm_CAPM = lm(close.x ~ close.y, data = data)
summary(lm_CAPM)
#Plotting Linear Regression
plot(data$close.y, data$close.x, col = 'blue',
     xlab = 'VN_INDEX Closing Price', ylab = 'VCB Closing Price', main = 'LINEAR REGRESSION')
alpha = lm_CAPM$coefficients[1]
beta = lm_CAPM$coefficients[2]
lines(data$close.y[1:length(data$close.y)-1], alpha + beta * data$close.y[1:length(data$close.y)-1], lwd = 1, col = 'red')
