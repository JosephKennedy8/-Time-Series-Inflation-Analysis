# Prerequisites and imports
library("Rcpp")
library("ggplot2")
library("stats")
library("forecast")
path <- file.path("C:", "Users", "josep", "OneDrive", "Documents", "Uni", "Data 
Science - 4th Year", "Time Series and Machine Learning", "Coursework 1", fsep = 
                    "\\")
setwd(path)
dataset.raw <- read.csv("DataInflation.csv")
dataset.ts <- ts(data = dataset.raw[,2], start = c(2016, 7), frequency=12)
#-----------------------------------------------------
# Question 1
#-----------------------------------------------------
layout = par(mfrow = c(1, 2)) # Allows for two graphs to be presented side-by-side
plot(dataset.ts, col="red",main="Monthly Inflation Rates (%) - Original Series", 
     ylab=expression(X[t]), xlab="Time") # Plots Original data
differences <- diff(dataset.ts)
plot(differences, col="blue", main="Monthly Inflation Rates (%) - Differenced 
Series", ylab=expression(Y[t])) # Plots Differenced data
#-----------------------------------------------------
# Question 2
#-----------------------------------------------------
acf(dataset.ts, main = "ACF of Original Series", lag.max = 20, xlab="Lag (Months)")
# ACF of original data
pacf(dataset.ts, main = "PACF of Original Series", lag.max = 20, xlab="Lag 
(Months)") # Partial ACF of original data
acf(differences, main = "ACF of Differenced Series", lag.max = 20, xlab="Lag 
(Months)") # ACF of differenced data
pacf(differences, main = "PACF of Differenced Series", lag.max = 20, xlab="Lag 
(Months)") # Partial ACF of differenced data
#-----------------------------------------------------
# Question 4
#-----------------------------------------------------
estimated_model <- Arima(dataset.ts, order = c(1, 1, 3)) # fit model
coef <- as.array(estimated_model$coef)
summary(estimated_model) # Obtain useful summary stats for model including AIC and 
coefficients
#-----------------------------------------------------
# Question 5
#-----------------------------------------------------
colleague_est_model <- Arima(dataset.ts, order = c(2, 1, 5)) # fit colleague's 
model
summary(colleague_est_model) # Obtain summary statistics for this model
#-----------------------------------------------------
# Question 6
#-----------------------------------------------------
layout = par(mfrow = c(1, 1)) # create better visuals for CI forecasting
forecasted_values <- forecast(estimated_model, h = 12) # h = 12 for 12 months
plot(forecasted_values, main="Inflation Rate Forecast for Next 12 Months", 
     ylab="Inflation Rate (%)", xlab="Time") # Plot the forecasted values with CI