install.packages("forecast") # Installing packages
library(ggplot2)             # Loading libraries
library(forecast)
library(tseries)

# EXPLORATORY DATA ANALYSIS
data <- read.csv("C:/Users/Binu Jemima/OneDrive/Desktop/ts3.csv") # Reading CSV file

set.seed(23323493)          # Setting seed according to my student number
train_data <- data[1:300, ] # Splitting 'ts3.csv' dataset
test_data <- data[301:401, ]

ts_data <- ts(train_data$x, frequency = 12) # Converting to time series object
plot(ts_data, type = "l", xlab = "x", ylab = "y") # Ploting time series object

# DATA PREPARATION
decomposed <- decompose(ts_data) # Decomposing time series object into components
plot(decomposed)

Box.test(ts_data, lag = 12, type = "Ljung-Box") # Performing Ljung-Box test on data

adf_test <- adf.test(ts_data) # Performing ADF test on data
print(adf_test)

diff_ts <- diff(ts_data, differences = 1) # Performing first-order differencing on data
adf_ts <- adf.test(diff_ts)                # Performing ADF test on differenced data
print(adf_ts)

# MODELING
sarima_model1 <- auto.arima(diff_ts, seasonal = TRUE) # Fitting SARIMA model 1
summary(sarima_model1)

seasonal_diff <- diff(ts_data, lag = 12)              # Performing seasonal differencing on data
sarima_model2 <- auto.arima(seasonal_diff, seasonal = TRUE) # Fitting SARIMA model 2 
summary(sarima_model2)

# INTERPRETATION
forecasted_values <- forecast(sarima_model2, h = length(test_data$x)) # Forecasting 
autoplot(forecasted_values)                                           # Ploting the forecasted values

# DIAGNOSTICS
plot(residuals(sarima_model2),               # Ploting residuals 
     main = "Residuals of the Model",
     ylab = "Residuals",
     xlab = "Time")

Box.test(residuals(sarima_model2), lag = 12, type = "Ljung-Box") # Performing Ljung-Box test on residuals

hist(residuals(sarima_model2),             # Ploting histogram for resdiuals
     main = "Histogram of Residuals",
     xlab = "Residuals",
     breaks = 20)
