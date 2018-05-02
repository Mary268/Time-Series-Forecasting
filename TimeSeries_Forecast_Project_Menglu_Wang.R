#Libraries
library(forecast)
library(fpp)
library(astsa)
#Data Read
travel = read.csv("/Users/marywang/Documents/MSCI_718/project/input_data_team_28.csv")
BC =  travel[ which(travel$Coordinate == '23.2'),]
BC.ts = ts(BC[,6], frequency = 12, start = c(2000,12), end = c(2017,12))
tsdisplay(BC.ts, main = "Before Data Cleaning")
#Average of previous year's Feb and next year's Feb to remove the outlier of Feb 2010
BC.ts[111] = (BC.ts[99] + BC.ts[123])/2
tsdisplay(BC.ts, main = "After Data Cleaning")
#-------------------------------------------Discriptice Analysis------------------------------
BC.ts.qtr <- aggregate(BC.ts, nfrequency=4)
BC.ts.yr <- aggregate(BC.ts, nfrequency=1)
plot.ts(BC.ts, main = "Monthly BC Traveler Accommodation Price Index", xlab = "Year", ylab = "Price Index")
par(mfrow = c(2,3))
plot.ts(BC.ts, main = "Monthly BC Traveler Accommodation Price Index", xlab = "Year", ylab = "Price Index")
plot.ts(BC.ts.qtr, main = "Quarterly BC Traveler Accommodation Price Index", xlab = "Year", ylab = "Price Index")
plot.ts(BC.ts.yr, main = "Yearly BC Traveler Accommodation Price Index", xlab = "Year", ylab = "Price Index")
seasonplot(BC.ts, main = "SeasonPlot BC Traveler Accommodation Price Index", xlab = "Month", ylab = "Price Index")
monthplot(BC.ts, main = "MonthPlot BC Accommodation Price Index", xlab = "Month", ylab = "Price Index")
boxplot(BC.ts ~ cycle(BC.ts), xlab = "Month", ylab = "Price Index", main = "Boxplot BC Traveler Accommodation Price Index")

tsdisplay(BC.ts)
par(mfrow = c(2,1))
acf2(BC.ts)
lag.plot(BC.ts, lags = 12, do.lines = FALSE)

#Decomposition
par(mfrow = c(3,1))
BC.stl<-stl(BC.ts, s.window="periodic")
plot(BC.stl)
BC.decom.a<-decompose(BC.ts, type = "additive")
BC.decom.m<-decompose(BC.ts, type = "multiplicative")
plot(BC.decom.a)
plot(BC.decom.m)
par(mfrow = c(2,2))
acf(na.omit(BC.decom.a$random))
pacf(na.omit(BC.decom.a$random))
acf(na.omit(BC.decom.m$random))
pacf(na.omit(BC.decom.m$random))

#Difference order explore
BC.ts.diff <- diff(BC.ts)
BC.ts.diff2 <- diff(BC.ts.diff)
BC.ts.diff3 <- diff(BC.ts.diff2)
plot(BC.ts.diff3)

#-------------------------------------------Predictive Analysis------------------------------
getrmse <- function(x,h,order,seasonal,lambda)
{
  train.end <- time(x)[length(x)-h]
  test.start <- time(x)[length(x)-h+1]
  train <- window(x,end=train.end)
  test <- window(x,start=test.start)
  fit <- Arima(train, order=order,seasonal=seasonal,lambda=lambda)
  fc <- forecast(fit,h=h)
  return(accuracy(fc,test)[2,"RMSE"])
}
#Traing - Testing Data Split
train = window(BC.ts, start = c(2000,12), end = c(2014,12)) # training data
test = window(BC.ts, start = c(2015,1), end = c(2017,12))

#Remove Trend & Seasonality ( Stationary Process )
#find approprite d: d =1 is good enough to obtain stationary data
adf.test(train, k = 12)
adf.test(diff(train,differences = 1),k = 12)
adf.test(diff(train,differences = 2),k = 12)
adf.test(diff(train,differences = 3),k = 12)
#1. Linear Regression
ts_lm = tslm(train ~ trend + season)
ts_lm.m = tslm(log(train) ~ trend + season) #multiplicative
#Moving Average
#train.ma = ma(train, order = 12)

par(mfrow = c(2,1))
#lines(train.ma, col = "green")
acf(ts_lm$residuals)
acf(ts_lm.m$residuals)
#acf(na.omit(train - train.ma))

# ARMA for Residual Forecast Addictive
fcst.ts_lm <- forecast(ts_lm, h = 36)
fcst.ts_lm.m <- forecast(ts_lm.m, h = 36)
plot(BC.ts, main = "LR performance", ylab = "Price Index Data")
lines(ts_lm$fitted.values, col = "red")
lines(exp(ts_lm.m$fitted.values), col = "green")
lines(fcst.ts_lm$mean, col = "red")
lines(exp(fcst.ts_lm.m$mean), col = "green")
legend("topleft",lty=1, col=c(1,"red","green"), 
       c("Initial data", "Fitted of Addictive tslm", "Fitted of Multipilicative tslm"))
accuracy(fcst.ts_lm,test) # *** 6.29044 19.74885
accuracy(fcst.ts_lm.m,test)

plot(fcst.ts_lm, main = "BC.ts Trend & Seasonality Addictive Linear Regression", 
     xlim = c(2000.12,2020.1), ylim = c(65,170), xlab = "Year", ylab = "Price Index")
lines(ts_lm$fitted.values, col = 'green')
lines(test, col = 'red')
legend("topleft",lty=1, col=c(1,"green","blue","red"), 
       c("Training data", "Fitted Value", "Forecast on Test", "True Testing Data"))

plot(train, main = "BC.ts Trend & Seasonality Multiplicative Linear Regression", 
     xlim = c(2000.12,2020.1), ylim = c(65,170), xlab = "Year", ylab = "Price Index")
lines(exp(fcst.ts_lm.m$mean), col = "blue")
lines(exp(ts_lm$fitted.values), col = 'green')
lines(test, col = 'red')
legend("topleft",lty=1, col=c(1,"green","blue","red"), 
       c("Training data", "Fitted Value", "Forecast on Test", "True Testing Data"))

# 2. Exponential Smoothing Model: Holt's Winter    ***
#BC.ts.ses = ses(BC.ts, alpha = 0.2, initial = "simple", h = 24)
#BC.ts.holt = holt(BC.ts, initial = "optimal", h = 24)
hw = hw(train, initial = "optimal",seasonal="multiplicative", h = 36) # ***
hw.a = hw(train, initial = "optimal",seasonal="additive", h = 36)
par(mfrow = c(1,1))
plot(BC.ts, xlim = c(2000,2020),main = "Performance of HW models", xlab = "Year", ylab = "Price Index Data", PI=FALSE)
lines(hw.a$fitted, col = "red")
lines(hw$fitted, col = "green")
lines(hw.a$mean, col = "red", type="o")
lines(hw$mean, col = "green", type="o")
legend("topleft", lty = 1, col = c(1, "red","green"), 
       c("Data","Holt's Winter Addivtive", "Holt's Winter Multiplicative"))

#3 ETS
ets = ets(train)
plot(ets)
summary(ets)
ets.fcst = forecast(ets, h=36)
accuracy(ets.fcst,test)

#4  ARIMA 
#Exploratory Analysis of Difference Orders
#tsdisplay(arima(train, order = c(0,0,0), seasonal = c(0,1,0))$residual)
#tsdisplay(arima(train, order = c(0,0,0), seasonal = c(0,2,0))$residual)
#tsdisplay(arima(train, order = c(0,0,0), seasonal = c(0,3,0))$residual)
#tsdisplay(arima(train, order = c(0,1,0), seasonal = c(0,0,0))$residual)
#tsdisplay(arima(train, order = c(0,2,0), seasonal = c(0,0,0))$residual)
#tsdisplay(arima(train, order = c(0,1,0), seasonal = c(0,1,0))$residual)
#tsdisplay(arima(train, order = c(0,2,0), seasonal = c(0,1,0))$residual)
#tsdisplay(arima(train, order = c(0,1,0), seasonal = c(0,2,0))$residual)
#tsdisplay(arima(log(train), order = c(0,1,0), seasonal = c(0,3,0))$residual) # *** best models difference

#tsdisplay(arima(train, order = c(0,2,0), seasonal = c(0,2,0))$residual)
#adf.test(arima(train, order = c(0,0,0), seasonal = c(0,1,0))$residual,k = 12)
#adf.test(arima(train, order = c(0,0,0), seasonal = c(0,2,0))$residual,k = 12)
#adf.test(arima(train, order = c(0,1,0), seasonal = c(0,0,0))$residual,k = 12)
#adf.test(arima(train, order = c(0,2,0), seasonal = c(0,0,0))$residual,k = 12)
#adf.test(arima(train, order = c(0,1,0), seasonal = c(0,1,0))$residual,k = 12)
#adf.test(arima(log(train), order = c(0,1,0), seasonal = c(0,3,0))$residual,k = 12) # *** best models difference
#acf2(arima(train, order = c(0,1,0), seasonal = c(0,1,0))$residual)

autoArima = auto.arima(train) # (1,1,1)(2,0,0)12
summary(autoArima)
autoArima.fcst = forecast(autoArima, h=36)
accuracy(autoArima.fcst,test)

##Exploratory Analysis of Residuals after remove Seasonlity
plot(stl(train, s.window="periodic"))
train_seadj = seasadj(stl(train, s.window="periodic"))
train_seadj.autoArima = auto.arima(train_seadj)# (0,1,1)
summary(train_seadj.autoArima)

# Iteration to find the best order of ARIMA
# #accuracy(forecast(arima(train, order=c(1,1,1),seasonal=c(2,0,1)),h=36),test)
# accuracy(forecast(arima(train_seadj, order=c(0,1,1),seasonal=c(0,0,0)),h=36),test)
# 
# accuracy(forecast(arima(train, order=c(2,0,1),seasonal=c(3,1,0)),h=36),test)#18.495981
# accuracy(forecast(arima(train, order=c(1,1,1),seasonal=c(2,1,1)),h=36),test)
# accuracy(forecast(arima(train, order=c(1,1,1),seasonal=c(2,2,1)),h=36),test)#13.577191
# accuracy(forecast(arima(train, order=c(1,0,1),seasonal=c(2,2,1)),h=36),test)
# accuracy(forecast(arima(train, order=c(1,0,1),seasonal=c(2,3,1)),h=36),test)#11.089763
# accuracy(forecast(arima(train, order=c(2,0,1),seasonal=c(2,3,1)),h=36),test)#11.007917
# accuracy(forecast(arima(train, order=c(2,0,1),seasonal=c(2,3,1)),h=36),test)
# accuracy(forecast(arima(train, order=c(4,1,1),seasonal=c(2,3,1)),h=36),test)#6.612151
# accuracy(forecast(arima(train, order=c(2,1,1),seasonal=c(2,3,1)),h=36),test)#6.592784 ***
# accuracy(forecast(arima(train, order=c(3,1,1),seasonal=c(2,3,1)),h=36),test)#6.567043
# 
# tsdisplay(arima(train, order=c(3,1,1),seasonal=c(2,2,1))$residuals)
# accuracy(forecast(arima(train, order=c(3,1,1),seasonal=c(2,2,1)),h=26),test)# 12.401326
# 
# tsdisplay(arima(train, order=c(3,1,1),seasonal=c(2,1,3))$residuals)
# accuracy(forecast(arima(train, order=c(3,1,1),seasonal=c(2,1,3)),h=26),test)# 17.188926 ***
# 
# tsdisplay(arima(train, order=c(2,1,1),seasonal=c(2,2,1))$residuals)
# accuracy(forecast(arima(train, order=c(2,1,1),seasonal=c(2,2,1)),h=26),test)# 12.453915
# tsdisplay(arima(train, order=c(2,1,1),seasonal=c(2,1,1))$residuals)
# accuracy(forecast(arima(train, order=c(2,1,1),seasonal=c(2,1,1)),h=26),test)# 16.949997
# tsdisplay(arima(train, order=c(2,1,1),seasonal=c(1,1,1))$residuals)
# accuracy(forecast(arima(train, order=c(2,1,1),seasonal=c(2,1,1)),h=26),test)# 16.949997
# tsdisplay(arima(train, order=c(1,1,1),seasonal=c(2,1,1))$residuals)
# accuracy(forecast(arima(train, order=c(1,1,1),seasonal=c(2,1,1)),h=26),test)# 16.770516

#4  ARIMA Seasonality Adjusted
# accuracy(forecast(arima(train_seadj, order=c(2,0,1),seasonal=c(3,1,0)),h=36),test)
# accuracy(forecast(arima(train_seadj, order=c(1,1,1),seasonal=c(2,1,1)),h=36),test)
# accuracy(forecast(arima(train_seadj, order=c(1,1,1),seasonal=c(2,2,1)),h=36),test)
# accuracy(forecast(arima(train_seadj, order=c(1,0,1),seasonal=c(2,2,1)),h=36),test)
# accuracy(forecast(arima(train_seadj, order=c(1,0,1),seasonal=c(2,3,1)),h=36),test)
# accuracy(forecast(arima(train_seadj, order=c(2,0,1),seasonal=c(2,3,1)),h=36),test)
# accuracy(forecast(arima(train_seadj, order=c(2,0,1),seasonal=c(2,3,1)),h=36),test)
# accuracy(forecast(arima(train_seadj, order=c(4,1,1),seasonal=c(2,3,1)),h=36),test)
# #accuracy(forecast(arima(train_seadj, order=c(2,1,1),seasonal=c(2,3,1)),h=36),test)#13.34959 ***
# accuracy(forecast(arima(train_seadj, order=c(3,1,1),seasonal=c(2,3,1)),h=36),test)#13.327892

# h02 = train
# #C+S+c
# c(getrmse(h02,h=169,order=c(0,1,1),seasonal=c(0,0,0),lambda=0),# train_seadj.autoArima
#   getrmse(h02,h=169,order=c(1,1,1),seasonal=c(2,0,0),lambda=0),# autoArima
#   getrmse(h02,h=169,order=c(1,0,0),seasonal=c(0,1,0),lambda=0),#3
#   getrmse(h02,h=169,order=c(1,0,0),seasonal=c(0,0,1),lambda=0),#4
#   getrmse(h02,h=169,order=c(1,1,0),seasonal=c(0,0,0),lambda=0),#5
#   getrmse(h02,h=169,order=c(1,1,0),seasonal=c(1,0,0),lambda=0),#6 ***
#   getrmse(h02,h=169,order=c(1,1,0),seasonal=c(0,1,0),lambda=0),#7
#   getrmse(h02,h=169,order=c(1,1,0),seasonal=c(0,0,1),lambda=0),#8
#   getrmse(h02,h=169,order=c(1,0,1),seasonal=c(0,0,0),lambda=0),
#   getrmse(h02,h=169,order=c(1,0,1),seasonal=c(1,0,0),lambda=0),
#   getrmse(h02,h=169,order=c(1,0,1),seasonal=c(0,1,0),lambda=0),
#   getrmse(h02,h=169,order=c(1,0,1),seasonal=c(0,0,1),lambda=0),#
#   getrmse(h02,h=169,order=c(2,0,0),seasonal=c(0,0,0),lambda=0),
#   getrmse(h02,h=169,order=c(2,0,0),seasonal=c(1,0,0),lambda=0),
#   getrmse(h02,h=169,order=c(2,0,0),seasonal=c(0,1,0),lambda=0),
#   getrmse(h02,h=169,order=c(2,0,0),seasonal=c(0,0,1),lambda=0),
#   getrmse(h02,h=169,order=c(2,0,1),seasonal=c(0,0,0),lambda=0),
#   getrmse(h02,h=169,order=c(2,0,1),seasonal=c(1,0,0),lambda=0),
#   getrmse(h02,h=169,order=c(2,1,0),seasonal=c(0,0,0),lambda=0),
#   getrmse(h02,h=169,order=c(2,1,0),seasonal=c(1,0,0),lambda=0), # *** ***
#   getrmse(h02,h=169,order=c(2,1,0),seasonal=c(2,1,0),lambda=0),
#   getrmse(h02,h=169,order=c(2,1,1),seasonal=c(2,1,0),lambda=0),
#   getrmse(h02,h=169,order=c(2,1,0),seasonal=c(2,0,0),lambda=0),
#   getrmse(h02,h=169,order=c(2,1,0),seasonal=c(0,1,0),lambda=0),
#   getrmse(h02,h=169,order=c(2,1,0),seasonal=c(0,0,1),lambda=0))

# 3. Double Differnece of LogTransformed Train Data 
#                               *** d = 2 for arima
# accuracy(forecast(arima(log(train), order=c(2,0,1),seasonal=c(3,1,0)),h=36),test)
# accuracy(forecast(arima(log(train), order=c(1,1,1),seasonal=c(2,1,1)),h=36),test)
# accuracy(forecast(arima(log(train), order=c(1,1,1),seasonal=c(2,2,1)),h=36),test)
# accuracy(forecast(arima(log(train), order=c(1,0,1),seasonal=c(2,2,1)),h=36),test)
# accuracy(forecast(arima(log(train), order=c(1,0,1),seasonal=c(2,3,1)),h=36),test)
# accuracy(forecast(arima(log(train), order=c(2,0,1),seasonal=c(2,3,1)),h=36),test)
# accuracy(forecast(arima(log(train), order=c(2,0,1),seasonal=c(2,3,1)),h=36),test)
# accuracy(forecast(arima(log(train), order=c(4,1,1),seasonal=c(2,3,1)),h=36),test)
# accuracy(forecast(arima(log(train), order=c(2,1,1),seasonal=c(2,3,1)),h=36),test)
# accuracy(forecast(arima(log(train), order=c(3,1,1),seasonal=c(2,3,1)),h=36),test)#0.08384526 119.60061363 ***

auto_arima = auto.arima(log(train))
#sarima(train,1,0,1,0,1,1,12) #ARIMA(3,1,1)(1,0,0)[12]
#sarima(train,3,1,1,1,0,0,12) #ARIMA(3,1,1)(1,0,0)[12]
#sarima(train,3,1,1,2,3,1,12) #ARIMA(3,1,1)(1,0,0)[12]
plot(autoArima.fcst)

# Neture Network Machine-Learning is not appropriate for two resaons in this case:
#1. small dataset of BC.ts
#2. Initial different Randomness of NNR will generate different results with huge variance 
nn <- nnetar(train)
nn.fcst <- forecast(nn, h = 36)
accuracy(nn.fcst,test) # 0.8815743 26.0191117
plot(nn.fcst)

#Performance plot
par(mfrow = c(2,3))
plot(forecast(arima(train, order=c(2,1,1),seasonal=c(2,3,1)),h=36))
lines(test, col = "red")
legend("topleft",lty=1, col=c(1,"blue","red"), 
       c("Training data", "Forecast on Test", "True Testing Data"))
plot(forecast(arima(train, order=c(3,1,1),seasonal=c(2,1,3)),ylim = c(70,180),h=36))
lines(test, col = "red")
#plot(forecast(ts_lm, h = 36))
#lines(test, col = "red")
plot(forecast(hw,h=36),ylim = c(70,180))
lines(test, col = "red")
f = (forecast(arima(log(train), order=c(3,1,1),seasonal=c(2,3,1)),h=36))$mean
f.v = as.vector(f)
plot(train,xlim = c(2000,2018), ylim = c(60,185),main = "Log Transformed ARIMA(3,1,1)(2,3,1)12")
lines(exp(f), col = "blue")
lines(test, col = "red")
plot(forecast(ets(train),ylim = c(60,160), h=36))
lines(test, col = "red")
plot(forecast(nnetar(train),h=36))
lines(test, col = "red")

summary(arima(train, order=c(2,1,1),seasonal=c(2,3,1))) # 2.783906
tsdisplay(arima(train, order=c(2,1,1),seasonal=c(2,3,1))$residuals)
accuracy(forecast(arima(train, order=c(2,1,1),seasonal=c(2,3,1)),h=36),test) #7.027694 ***
tsdisplay(test - forecast(arima(train, order=c(2,1,1),seasonal=c(2,3,1)),h=36)$mean)

summary(arima(train, order=c(3,1,1),seasonal=c(2,3,1))) # 2.801656 
tsdisplay(arima(train, order=c(3,1,1),seasonal=c(2,3,1))$residuals)
accuracy(forecast(arima(train, order=c(3,1,1),seasonal=c(2,3,1)),h=36),test) #7.029369 ***
tsdisplay(test - forecast(arima(train, order=c(3,1,1),seasonal=c(2,3,1)),h=36)$mean)

summary(arima(log(train), order=c(3,1,1),seasonal=c(2,3,1)))
tsdisplay(arima(log(train), order=c(3,1,1),seasonal=c(2,3,1))$residuals) # ***
f = (forecast(arima(log(train), order=c(3,1,1),seasonal=c(2,3,1)),h=36))$mean
accuracy(exp(f),test) #7.897346 ******
tsdisplay(test - exp(forecast(arima(log(train), order=c(3,1,1),seasonal=c(2,3,1)),h=36)$mean))

summary(ets(train))
accuracy(forecast(ets(train), h=36),test)
tsdisplay(ets(train)$residuals)

summary(arima(train, order=c(2,1,1),seasonal=c(2,2,3)))
tsdisplay(arima(train, order=c(2,1,1),seasonal=c(2,2,3))$residuals)
accuracy(forecast(arima(train, order=c(2,1,1),seasonal=c(2,2,3)),h=26),test)# 11.721891 ***

a = arima(log(train), order=c(2,1,1),seasonal=c(3,2,1))
tsdisplay(a$residuals) # **
f = (forecast(arima(log(train), order=c(2,1,1),seasonal=c(3,2,1)),h=36))$mean
accuracy(exp(f),test) # 10.3559

a = arima(log(train), order=c(2,1,1),seasonal=c(2,1,1))
tsdisplay(a$residuals)
f = (forecast(arima(log(train), order=c(2,1,1),seasonal=c(2,1,1)),h=36))$mean
accuracy(exp(f),test) # 15.82667

#best_model:
summary(arima(log(train), order=c(3,1,1),seasonal=c(2,3,1)))# 0.02931448 ***
accuracy(exp(forecast(arima(log(train), order=c(3,1,1),seasonal=c(2,3,1)),h=36)$mean),test) #7.897346 ***
tsdisplay(arima(log(train), order=c(3,1,1),seasonal=c(2,3,1))$residuals)
#The Best Model's overall training data performance plot:
sarima(log(train),3,1,1,2,3,1,12)

best_model = arima(log(train), order=c(3,1,1),seasonal=c(2,3,1))
best_model.test = exp(forecast(best_model,h=36)$mean)
tsdisplay(arima(log(train), order=c(3,1,1),seasonal=c(2,3,1))$residuals)

summary(best_model)
accuracy(best_model.test,test) #7.897346

#Forecast Next Year: 2018
best_model.fcst = forecast(best_model,h=48)
#Log Transformation
best_model.fcst$mean = exp(best_model.fcst$mean) # Forecasts of 2018 Price Index mean value
best_model.fcst$lower = exp(best_model.fcst$lower)
best_model.fcst$upper = exp(best_model.fcst$upper)
best_model.fcst$x = exp(best_model.fcst$x)
#Forcasts of 2018 Plot
par(mfrow = c(1,1))
plot(best_model.fcst,xlim = c(2000,2020),ylim = c(50,390), main = "Forecasts of 2018 Price from Log-Transformed ARIMA(3,1,1)(2,3,1)[12]")
lines(test, col = "red")
legend("topleft",lty=1, col=c(1,"blue","red"), 
       c("Training data", "Forecasts", "True Testing Data"))
forecasts_2018 = data.frame(best_model.fcst)
write.csv(x = forecasts_2018[37:48,], file = "/Users/marywang/Documents/MSCI_718/project/output_data_team_28.csv")
