
library(readr)
library(caret)
library(randomForest)
library(Metrics)
library(ggplot2)
library(gridExtra)


setwd("D:/Air pollutionanalysis")
df <- read.csv("air_quality_health_impact_data.csv")


features <- c("AQI", "PM10", "PM2_5", "NO2", "SO2", "O3")
target <- "HealthImpactScore"

# data preprocessing
X <- df[, features]
y <- df[[target]]
data <- cbind(X, HealthImpactScore = y)

# data split
set.seed(42)
trainIndex <- createDataPartition(data$HealthImpactScore, p = .8, list = FALSE)
trainData <- data[trainIndex, ]
testData <- data[-trainIndex, ]

# Random Forest model 
rf_model <- randomForest(HealthImpactScore ~ ., data = trainData)

# Prediction
rf_preds <- predict(rf_model, testData)

# Evaluate R²
rf_r2 <- R2(rf_preds, testData$HealthImpactScore)
cat("Random Forest R2:", rf_r2, "\n")

# Predicted vs Actual Plot
rf_plot <- ggplot(data.frame(Actual = testData$HealthImpactScore, Predicted = rf_preds), aes(x = Actual, y = Predicted)) +
  geom_point(color = "steelblue", alpha = 0.6) +
  geom_abline(slope = 1, intercept = 0, color = "darkred", linetype = "dashed") +
  ggtitle("Random Forest: Predicted vs Actual") +
  theme_minimal()

# Feature Importance Plot
rf_importance <- importance(rf_model)
rf_imp_df <- data.frame(Feature = rownames(rf_importance), Importance = rf_importance[, 1])
rf_imp_plot <- ggplot(rf_imp_df, aes(x = reorder(Feature, Importance), y = Importance)) +
  geom_bar(stat = "identity", fill = "steelblue") +
  coord_flip() +
  ggtitle("Random Forest Feature Importance") +
  theme_minimal()

# Arrange plots
grid.arrange(rf_plot, rf_imp_plot, ncol = 2)

# predict example 
example <- testData[1, features]
actual_value <- testData$HealthImpactScore[1]
rf_example_pred <- predict(rf_model, example)

cat("\n--- Random Forest Example ---\n")
cat("Actual Health Impact Score:", actual_value, "\n")
cat("Random Forest Prediction  :", rf_example_pred, "\n")