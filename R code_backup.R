#Midterm
library(readxl)
library(fpp2)
library(forecast)
library(ggplot2)
library(dplyr)
library(date)
library(plotly)
library(tidyr)

#Import covid data
CovidTrain <- read_excel("Predictive Analytics/Midterm/CovidTrain.xlsx")

#Check the data
View(CovidTrain)

str(CovidTrain)

#Sync case data to one date time series
GlobalCases <- CovidTrain %>% select(`Country Forecast`, Cases, Date) %>%
  group_by(Date) %>% 
  summarise(Cases = sum(Cases))

GlobalCases

#Sync fatality data to one date time series
GlobalFatalities <- CovidTrain %>% select('Country Forecast', Fatalities, Date) %>%
  group_by(Date) %>% 
  summarise(Fatalities = sum(Fatalities))

GlobalFatalities

#Change to Time Series
CovidtrainTS <- ts(data = CovidTrain$Fatalities, frequency = 140)

#line graph
autoplot(CovidtrainTS)

#plot Cases
PlotCase <- ggplot(GlobalCases, aes(x=Date, y=Cases, group=1)) +      
  geom_line() +  
  xlab("Date") + 
  ylab("Cases")

PlotCase

#Plot Fatalities
PlotFatal <- ggplot(GlobalFatalities, aes(x=Date, y=Fatalities, group=1)) +      
  geom_line() +  
  xlab("Date") + 
  ylab("Fatalities")

PlotFatal

#Now cases with a transformation
TransCases <- ggplot(GlobalCases, aes(x=Date, y=Cases, group=1)) +      
  geom_line() +  
  scale_y_continuous(trans='log2') + 
  xlab("Date") + 
  ylab("Cases")

TransCases

#Fatalities with a transformation
TransFatal <- ggplot(GlobalFatalities, aes(x=Date, y=Fatalities, group=1)) +      
  geom_line() +  
  scale_y_continuous(trans='log2') + 
  xlab("Date") + 
  ylab("Fatalities")

TransFatal

#range
range(GlobalFatalities$Date)

#dates
inds <- seq(as.Date("2020-01-22"), as.Date("2020-03-23"), by = "day")

#Cases time series
GlobalCasesTS <- ts(GlobalCases$Cases,  
                                start = c( 2020,  as.numeric(format(inds[1], "%j"))), 
                                frequency = 365)

#Plot case time series
plot(GlobalCasesTS, xlab="Time", ylab="Cases", bty="l")

#Fatalities time series
GlobalFatalTS <- ts(GlobalFatalities$Fatalities,  
                    start = c( 2020,  as.numeric(format(inds[1], "%j"))), 
                    frequency = 365)

#Plot case time series
plot(GlobalFatalTS, xlab="Time", ylab="Fatalities", bty="l")

#Sequence
indstrain <- seq(as.Date("2020-01-22"), as.Date("2020-03-15"), by = "day")
indsvalid <- seq(as.Date("2020-03-16"), as.Date("2020-03-22"), by = "day")

#Create validation data
trainTS <- window(GlobalFatalTS, start = c(2020, as.numeric(format(indstrain[1], "%j"))), end = c(2020, as.numeric(format(indstrain[length(indstrain)], "%j" ))))
validTS <- window(GlobalFatalTS, start = c(2020, as.numeric(format(indsvalid[1], "%j"))), end = c(2020, as.numeric(format(indsvalid[length(indsvalid)], "%j"))))

#Models - holt
ForecastTime <- 12 

holtFatal <- ets(trainTS, model = "AAN")
holtFatalPred <- forecast(holtFatal, h = ForecastTime, level = 0)
holtFatal

plot(holtFatalPred,  ylab = "Fatalities", xlab = "Date", bty = "l")
lines(holtFatalPred$fitted, col = "blue")
lines(validTS)

#accuracy
accuracy(holtFatalPred, validTS)

#Models - ets
etsline <- ets(trainTS, model = "AAN")
etslinePred <- forecast(etsline, h = ForecastTime, level = 0)
etslinePred

plot(etslinePred,  ylab = "Fatalities", xlab = "Date", bty = "l")
lines(etslinePred$fitted, col = "blue")
lines(validTS, col = "red")

#accuracy
accuracy(etslinePred, validTS)

#Models - Arima
autoline <- auto.arima(trainTS)
autolinePred <- forecast(autoline, h = 30)
autolinePred

plot(autolinePred,  ylab = "Fatalities", xlab = "Date", bty = "l")
lines(autolinePred$fitted, col = "blue")
lines(validTS, col = "red")

#accuracy
accuracy(autolinePred, validTS)

#Models - ses
sestrain <- ses(trainTS, h = 30)
autoplot(sestrain)

#accuracy
accuracy(sestrain, validTS)
