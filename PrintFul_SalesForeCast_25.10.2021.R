rm(list=ls())
library(fpp2)
library(lubridate)
library(ggplot2)
library(forecast)
library(dplyr)
install.packages('tidyverse')
library(tidyverse)

#############################################
# READ DATA
#############################################

#read data from csv
data <- read.csv("../R/PrintFul/sales28.csv") %>%
  arrange(desc(Store), desc(X)) %>%
  mutate(year = year(Date)) %>%
  mutate(week = isoweek(Date)) %>%
  mutate(dayOfWeek = wday(Date) - 1) %>%
  mutate(month = month(Date))  %>%
  mutate(totalMonth = ifelse(year == 2014, 12 + month(Date), month(Date))) %>%
  subset(Sales != 0)

#############################################
# DATA INSPECTION
#############################################

#Show how much date data available for each store
for (x in 1:28){
  print(paste(paste(x, ' store contains '), nrow(data %>% subset(Store == x))))
}

#Sales per month for all stores with boxplot and SD
boxplot(Sales ~ month, data, xlab = "Month",
        ylab = "Sales per month", main = "Sales per month for all stores")

#Sales per month for all stores only mean value bars
ggplot(data, aes(x = factor(month), y = Sales)) + 
  stat_summary(fun = "mean", geom = "bar")

#Sales per week for all stores with boxplot and SD
boxplot(Sales ~ week, data, xlab = "Week",
        ylab = "Sales", main = "Sales per Week for all stores")

#Sales per day of week for all stores with boxplot and SD
boxplot(Sales ~ dayOfWeek, data, xlab = "Day of week",
        ylab = "Sales", main = "Sales per day of Week for all stores")

#Sales per Store with boxplot and SD
boxplot(Sales ~ Store, data, xlab = "Store",
        ylab = "Sales", main = "Sales per Store")

#############################################
# AGGREGATE DATA AND FIND BEST FORECAST MODEL
#############################################

# Aggregate Sales data by month
data_aggr <- data %>%
  subset(totalMonth <= 18) %>% 
  group_by(totalMonth) %>% 
  dplyr::summarize(MonthSales = sum(Sales)) %>% 
  as.data.frame()


# Time plot for forecast
Y <- ts(data_aggr[,2],start=c(2013,1),frequency = 12)
autoplot(Y) +
  ggtitle("Time plot: Sales per month") +
  ylab("Sales") +
  xlab("Month")

# Data has no trend

#investigate seasonality
ggseasonplot(Y) +
  ggtitle("Seasonal plot: Change in Sales") +
  ylab("Sales") +
  xlab("Month")


#Use benchmark method to forecast, 

#choosing seasonal naive method, since data is seasonal
fit_snaive <- snaive(Y) #RSD = 146095.3347 
print(summary(fit_snaive))
checkresiduals(fit_snaive)

#ETS method
fit_ets <- ets(Y) #RSD = 366438.1
print(summary(fit_ets))
checkresiduals(fit_ets)

#ARIMA method
fit_arima <- auto.arima(Y, d=1, D=1, stepwise = FALSE, approximation = FALSE, trace = TRUE) #RSD = 231495
print(summary(fit_arima))
checkresiduals(fit_arima)

#############################################
# FORECAST BY S-NAIVE
#############################################

fcst <- forecast(fit_snaive, h = 24)
autoplot(fcst, include = 60)