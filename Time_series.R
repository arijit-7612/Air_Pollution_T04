library(dplyr)
library(readr)
library(ggplot2)
library(caret)
library(forecast)

setwd("C:/Users/HP/Desktop/CODING/R")
df <- read.csv("air_quality_health_impact_data.csv")

if ("Date" %in% names(df)) {
  df$Date <- as.Date(df$Date, format = "%Y-%m-%d")
} else {
  df$Date <- 1:nrow(df)  # Use row index if no date provided
}

df <- df %>% arrange(Date)

if (!"PM2_5" %in% names(df)) {
  stop("\u274c PM2_5 column not found in the dataset.")
}

# Creating Time Series Object
ts_data <- ts(df$PM2_5, frequency = 7)  # Assuming daily data with weekly seasonality

# Train-Test Split (80%-20%)
split_index <- floor(0.8 * length(ts_data))
ts_train <- window(ts_data, end = c(1, split_index))
ts_test <- window(ts_data, start = c(1, split_index + 1))

auto_model <- auto.arima(ts_train)

forecast_horizon <- length(ts_test)
forecast_result <- forecast(auto_model, h = forecast_horizon)

predictions <- as.numeric(forecast_result$mean)
actuals <- as.numeric(ts_test)

# Evaluating Model Performance
rmse_val <- sqrt(mean((actuals - predictions)^2))
mae_val <- mean(abs(actuals - predictions))


cat("\n\ud83d\udcca ARIMA Model Performance:\n")
cat(sprintf("\u2705 RMSE: %.2f\n", rmse_val))
cat(sprintf("\u2705 MAE: %.2f\n", mae_val))


# Plotting Actual vs Forecasted PM2.5 levels
plot_df <- data.frame(Index = 1:length(actuals), Actual = actuals, Predicted = predictions)

ggplot(plot_df, aes(x = Index)) +
  geom_line(aes(y = Actual), color = "blue", size = 1.2) +
  geom_line(aes(y = Predicted), color = "red", linetype = "dashed", size = 1.2) +
  labs(title = "Actual vs Forecasted PM2.5 Levels",
       x = "Index", y = "PM2.5 Level") +
  theme_minimal()
