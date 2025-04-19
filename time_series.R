library(dplyr)
library(readr)
library(ggplot2)
library(caret)
library(randomForest)
library(Metrics)

setwd("C:/Users/HP/Desktop/CODING/R")
df <- read.csv("air_quality_health_impact_data.csv")


if ("Date" %in% names(df)) {
  df$Date <- as.Date(df$Date, format = "%Y-%m-%d")
} else {
  df$Date <- 1:nrow(df)  
}


df <- df %>% arrange(Date)

# Creating lag features for PM2.5 to use past values for prediction
lag_days <- 3
for (i in 1:lag_days) {
  df[[paste0("PM2_5_lag", i)]] <- lag(df$PM2_5, i)
}

# Removing NA values
df <- na.omit(df)

features <- paste0("PM2_5_lag", 1:lag_days)
target <- "PM2_5"

# Train/test split (80% train, 20% test)
set.seed(123)
train_index <- createDataPartition(df$PM2_5, p = 0.8, list = FALSE)
train_data <- df[train_index, ]
test_data <- df[-train_index, ]

rf_model <- randomForest(x = train_data[, features], y = train_data[[target]], ntree = 100)

predictions <- predict(rf_model, test_data[, features])

# Evaluating model
rmse_val <- rmse(test_data[[target]], predictions)
mae_val <- mae(test_data[[target]], predictions)
r2_val <- R2(predictions, test_data[[target]])

cat("\nModel Performance:\n")
cat(sprintf("RMSE: %.2f\n", rmse_val))
cat(sprintf("MAE: %.2f\n", mae_val))
cat(sprintf("R-squared: %.2f\n", r2_val))

# Plotting Actual vs Predicted PM2.5
plot_df <- data.frame(Index = 1:nrow(test_data), Actual = test_data[[target]], Predicted = predictions)

ggplot(plot_df, aes(x = Index)) +
  geom_line(aes(y = Actual), color = "blue", size = 1.2) +
  geom_line(aes(y = Predicted), color = "red", linetype = "dashed", size = 1.2) +
  labs(title = "Actual vs Predicted PM2.5 Levels",
       x = "Index", y = "PM2.5 Level") +
  theme_minimal()
