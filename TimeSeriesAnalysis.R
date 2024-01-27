
library(tidyverse)
library(lubridate)
library(forecast)


# Load the dataset
file_path <- file.choose()
ifc_data <- read_csv(file_path)

ifc_data$YearMonth <- floor_date(mdy(ifc_data$'Date Disclosed'), "month")

monthly_investment <- ifc_data %>%
  group_by(YearMonth) %>%
  summarize(TotalInvestment = sum(`Total IFC investment as approved by Board(Million - USD)`, na.rm = TRUE))

head(monthly_investment)

ts_data <- ts(monthly_investment$TotalInvestment, 
              start = c(year(min(monthly_investment$YearMonth)), 
                        month(min(monthly_investment$YearMonth))), 
              frequency = 12)

# Plot the time series
plot(ts_data)

# Fit a model
fit <- auto.arima(ts_data)

# Forecast next N months (e.g., 12 months)
forecast_future <- forecast(fit, h = 12)

# Plot the forecast
plot(forecast_future)
set.seed(123)

# Assuming you have a time series 'ts_data'
total_length <- length(ts_data)
train_percentage <- 0.8

# Generate random indices for training set
train_indices <- sample(1:total_length, round(total_length * train_percentage))

# Create training and testing sets
train <- ts_data[train_indices]
test <- ts_data[-train_indices]

fit <- auto.arima(train)

forecast_future <- forecast(fit, h = length(test))

# Calculate accuracy metrics
forecast_errors <- forecast_future$mean - test
mae <- mean(abs(forecast_errors))
mse <- mean(forecast_errors^2)
rmse <- sqrt(mse)

# Print the metrics
cat("MAE:", mae, "\nMSE:", mse, "\nRMSE:", rmse, "\n")

plot(forecast_future)
lines(test, col = 'red')

residuals <- residuals(fit)
hist(residuals, main = "Residuals Distribution", xlab = "Residuals")
acf(residuals, main = "ACF of Residuals")

