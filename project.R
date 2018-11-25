library(dplyr)
library(ggplot2)
library(TSA)


rm(list = ls())

# Read in data
sp <- read.csv("s&p500.csv")

# Create time-series
sp_500 <- ts(sp$Close, start=c(2018,01,01), freq=250)

# Plot time series
plot(sp_500)

# Calculate Percentage Change
sp <- sp %>%
      mutate(perc = (Close - lag(Close))/lag(Close), Date = as.Date(Date)) 


# Percentage Change from Day-Day
plot(sp$perc, type = "l")

# Density Plot of prices
ggplot(sp, aes(Close)) + 
  geom_histogram(bins = 50, aes(y = ..density..), col = "red", fill = "red", alpha = 0.3) + 
  geom_density()


# Moving Average
sp <- sp %>%
  mutate(week_average = rollmean(Close, k = 5, fill = NA))

# This is done to smoothen the data a little bit
plot.ts(sp$week_average, col = "red")


# Comparing daily high and low prices
ggplot(sp) +
  geom_line(aes(x=Date,y=High,colour="blue")) +
  geom_line(aes(x=Date,y=Low,colour="red")) 

# Comparing daily open and close prices
ggplot(sp) +
  geom_line(aes(x=Date,y=Open,colour="blue")) +
  geom_line(aes(x=Date,y=Close,colour="red")) 



# Analysing time series

acf(sp_500)

pacf(sp_500)

eacf(sp_500)

set.seed(92397)

res=armasubsets(y=sp_500,nar=14,nma=14,y.name='test',ar.method='ols')

plot(res)


# First Difference

sp_500 <- diff(sp$Close, differences=1)

plot(sp_500,type = "l")

acf(sp_500)

pacf(sp_500)

eacf(sp_500)

set.seed(92397)

res=armasubsets(y=sp_500,nar=14,nma=14,y.name='test',ar.method='ols')

plot(res)

# Fitting model

fit <- arima(sp_500, order = c(1,0,0),seasonal = c(8,0,0))

plot(fit$residuals)

acf(fit$residuals)

pacf(fit$residuals)




