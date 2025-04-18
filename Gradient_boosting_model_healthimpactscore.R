library(readr)
library(caret)
library(gbm)
library(Metrics)
library(ggplot2)
library(gridExtra)

setwd("E:/Air Pollution")
air_data <- read.csv("air_quality_health_impact_data.csv")

pollutants <- c("AQI", "PM10", "PM2_5", "NO2", "SO2", "O3")
health_score <- "HealthImpactScore"

# Data PreProcessing
pollution_inputs <- air_data[, pollutants]
health_output <- air_data[[health_score]]
model_data <- cbind(pollution_inputs, HealthImpactScore = health_output)

# Data Split
set.seed(42)
split_indices <- createDataPartition(model_data$HealthImpactScore, p = 0.8, list = FALSE)
training_set <- model_data[split_indices, ]
testing_set <- model_data[-split_indices, ]

# Model Training
boosted_model <- gbm(HealthImpactScore ~ ., data = training_set,
                     distribution = "gaussian",
                     n.trees = 100,
                     interaction.depth = 3,
                     shrinkage = 0.1,
                     cv.folds = 5,
                     verbose = FALSE)

# Find the optimal number of trees
optimal_trees <- gbm.perf(boosted_model, method = "cv")

# Prediction
boosted_predictions <- predict(boosted_model, testing_set, n.trees = optimal_trees)

# Model Evaluation
boosted_r2_score <- R2(boosted_predictions, testing_set$HealthImpactScore)
cat("Gradient Boosting R2:", boosted_r2_score, "\n")

# Plot: Actual vs Predicted
boosted_actual_vs_pred_plot <- ggplot(data.frame(Actual = testing_set$HealthImpactScore, Predicted = boosted_predictions), aes(x = Actual, y = Predicted)) +
  geom_point(color = "darkgreen", alpha = 0.6) +
  geom_abline(slope = 1, intercept = 0, color = "darkred", linetype = "dashed") +
  ggtitle("Gradient Boosting: Predicted vs Actual") +
  theme_minimal()

# Plot: Feature Importance
boosted_importance <- summary(boosted_model, n.trees = optimal_trees, plotit = FALSE)
boosted_feature_plot <- ggplot(boosted_importance, aes(x = reorder(var, rel.inf), y = rel.inf)) +
  geom_bar(stat = "identity", fill = "darkgreen") +
  coord_flip() +
  ggtitle("Gradient Boosting Feature Importance") +
  xlab("Feature") + ylab("Relative Importance") +
  theme_minimal()

# Plot Display 
grid.arrange(boosted_actual_vs_pred_plot, boosted_feature_plot, ncol = 2)

# Predict example 
sample_input <- testing_set[1, pollutants]
actual_health_score <- testing_set$HealthImpactScore[1]
predicted_score_gb <- predict(boosted_model, sample_input, n.trees = optimal_trees)

cat("\n--- Gradient Boosting Example Prediction ---\n")
cat("Actual Health Impact Score:", actual_health_score, "\n")
cat("Predicted Score (Gradient Boosting):", predicted_score_gb, "\n")

