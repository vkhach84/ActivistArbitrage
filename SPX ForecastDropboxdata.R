#install packages by one command BY Faizan
library("forecast")
library("tidyr")
library(dplyr)#library needed for my lag function to work
library(grid)
library("fpp")
library("zoo")
library("ggplot2")
#install packages by one command
#installed.packages(c("ggplot2","zoo","fpp","dplyr","grid","tidyr","forecast"))
suppressWarnings(source("../AnalyticsLibraries/library.R"))
# Package options
suppressWarnings(ggthemr('fresh'))  # ggplot theme
opts_knit$set(progress=FALSE, verbose=FALSE)
opts_chunk$set(echo=FALSE, fig.align="center", fig.width=10, fig.height=6.35, results="asis")
options(knitr.kable.NA = '')

#data <- read.csv("C:/Users/vkhac/Documents/INSEADAnalytics/APPLE vs Index/SPXAAPLplaying.csv")
#from session menu go to set working directory to source file location, then you can
#load the file without the full location name into the file
ndata <- read.csv("AAPL SPX 10Y OiL - Copy.csv")
# SUPER usefull function of attach, which makes easier to give just the names 
#from the data file
attach(ndata)
str(data)
data$Date <- as.Date(Date,format="%m/%d/%Y")
#Vardan: Create a new column with id
#id<-seq(1,261,by=1)
#data<-data.frame(id,Date,aapl,spx,X10y,wti)
str(data)
#data<- data[order(id, decreasing = TRUE),]

ggplot(data,aes(x=aapl))+geom_histogram(bandwidth=1)
ggplot(data,aes(y=aapl,x=Date))+geom_boxplot()

ggplot(data, aes(x=Date)) + 
  geom_line(aes(y=aapl)) + 
  labs(title="Monthly Time Series", 
       subtitle="Returns Percentage from Economics Dataset", 
       caption="Source: Economics", 
       y="Returns %")

ggplot(ndata,aes(y=aapl,x=spx))+geom_point(mapping = NULL)+stat_summary(fun.data=mean_cl_normal) + 
  geom_smooth(method='lm',formula=aapl~spx)+theme_bw() +
  labs(y = "AAPL",
       x = "SPX",
       title = "Regression Line")

plot(spx,aapl, col=2)
cor(aapl,spx)


#forecasting Y as a time series, frequency as 1 is the original data as in plot(y), also becaues we have daily data, we use 7 for seasonality
aapl.ts<-ts(aapl, start=c(1), frequency = 7)
plot(aapl.ts)
#decompose using "classical" method, multiplicative form
fit <- decompose(aapl.ts, type="multiplicative") 
plot(fit)

##decompose using STL (Season and trend using Loess)
fit <- stl(y.ts, t.window=52, s.window="periodic", robust=TRUE)
plot(fit)
# "MAM" is multiplicative Holt-Winters' method with multiplicative errors
Model.MAM <- ets(y.ts, model="MAM")
Pred.Model.MAM <- forecast(Model.MAM, h=115, level=c(0.2, 0.4, 0.6, 0.8))
plot(Pred.Model.MAM, main="MAM Model")
acf(y.ts)
#additive error,  and no trend, no seasonality/"ANN" is simple 
#exponential smoothing with additive errors
Model.ANN <- ets(y.ts, model="ANN")
Pred.Model.ANN <- forecast(Model.ANN, h=115, level=c(0.2, 0.4, 0.6, 0.8))
plot(Pred.Model.ANN, main="ANN Model")
acf(y.ts)
#multiplicative error and trend and no seasonality, h=115 is the number of periods to forecast
Model.MMN <- ets(y.ts, model="MMN")
Pred.Model.MMN <- forecast(Model.MMN, h=115, level=c(0.2, 0.4, 0.6, 0.8))
plot(Pred.Model.MMN, main="MMN Model")
write.csv(Pred.Model.MMN, file = "Tumblr_Prediction.csv")


#creating deltas
daapl<-(aapl/lag(aapl,1)-1)
dspx<-(spx/lag(spx,1)-1)
ggplot(data, aes(x=Date)) + 
  geom_line(aes(y=daapl)) + 
  labs(title="Daily %change chart", 
       subtitle="ggplot rules", 
       caption="Source: Prof V", 
       y="Returns %")

dreg<-lm(daapl~dspx)
summary(dreg)
#Vardan: cannot draw the regression line, not sure why!
ndata<-data.frame(daapl,dspx)
ggplot(data,aes(y=daapl,x=dspx))+geom_point(mapping = NULL)+stat_summary(fun.data=NULL) + 
  geom_smooth(method='lm',formula=daapl~dspx)+theme_bw() +
  labs(y = "AAPL",
       x = "SPX",
       title = "Regression Line")


ggplot(ndata, aes(y=daapl, x=dspx)) +geom_point(mapping = NULL)+
  theme_bw() +
  labs(y = "% change AAPL",
       x = "% change SPX",
       title = "Relashionship")

cor(dx,dy)
summary(dreg)


#new dataset with deltas in it
ndata<-data.frame(daapl,dspx)
#omit NA in the data set
ndata<-na.omit(ndata)

ggplot(data,aes(y=daapl,x=dspx))+geom_point(mapping = NULL)+stat_summary() + 
  geom_smooth(method='lm',formula=daapl~dspx)+theme_bw() +
  labs(y = "AAPL",
       x = "SPX",
       title = "Regression Line")

#no problem with autocorrelation, as we have deltas
acf(ndata$dx)

#Arima forecasting
plot(x)
fit <- auto.arima(x,seasonal=FALSE)
plot(fit)
Acf(residuals(fit))
plot(forecast(fit,50)) #50 stands for 20 quarters = 5 years


#plot the trendline
par(mfrow=c(1,1))
plot(dy ~ dx)
abline(dreg, col="red",lwd=2)

#extract residuals
dreg <- resid(dreg)
plot(sreg)
abline(0,0, col="red")




#extract fitted values
reglogfit <- reglog$fitted.values #fitted values are only available for log regression
plot(reglogfit)

#plot rvf 
plot(reg ~ reglogfit)
#plot a horizontal line
abline(0,0, col="red") 
#make a prediction/not working correclty
forpredict = data.frame(x=c(2014.89,2200,1900))
predict(reglog,forpredict)


#charting boxplots
par(mfrow=c(1,2))
boxplot(dy)
boxplot(dx)


#created new data frame
ndata<-data.frame(log(data$SPX),log(data$AAPL))


#create a new subset
kvm<-subset(data,select = c("SPX","AAPL"))

#regression of raw datas
reg<-lm(y~x)
summary(reg)

#plot the trendline
par(mfrow=c(1,1))
plot(y ~ x)
abline(reg, col="red",lwd=2)

#extract residuals
reg <- resid(reg)
plot(reg)
abline(0,0, col="red")
#extract fitted values
reglogfit <- reglog$fitted.values #fitted values are only available for log regression
plot(reglogfit)

#plot rvf 
plot(reg ~ reglogfit)
#plot a horizontal line
abline(0,0, col="red") 
#make a prediction/not working correclty
forpredict = data.frame(x=c(2014.89,2200,1900))
predict(reglog,forpredict)




#testing for autocorrelations/stationary-has to have low p-value
acf(y)
acf(x)
ccf(y,x)
#Dickey-Fuller test
adf.test(dy)#non stationary data series

theme_set(theme_classic())


# Allow Default X Axis Labels
ggplot(data, aes(x=x)) + 
  geom_line(aes(y=y)) + 
  labs(title="Time Series Chart", 
       subtitle="Returns Percentage", 
       caption="Source: Vardan", 
       y="Returns %") # cannot understand






#other functions that I tried

data$Date <- as.Date(data$Date, format="%m-%d-%y")
date<-as.Date(data$Date,"yyyy/m/d")

#creating time series
vardan<-ts(data, start = c(2,1), end = c(2,10, frequency(12)))

#smoothening data
ndatasmoothed<-ksmooth(ndata$log.data.SPX.,ndata$log.data.AAPL., "normal",bandwidth = 5)
plot(ndatasmoothed)
plot(ndata)

shift<-function(x,n){c(x[-(seq(n))],rep(NA,n))}
#inserting a row
data$row<-seq(1,nrow(data),1)
#data$smoothed<-ksmooth(data$AAPL,data$row,"normal",bandwidth = 1)

