library(ggplot2)
library(corrplot)
library(dplyr)
library(readr)
library(reshape2)


setwd("D:/Air pollutionanalysis")
data <- read.csv("air_quality_health_impact_data.csv")
# Correlation Heatmap

numeric_data <- data %>% select(where(is.numeric))
# correlation matrix
cor_matrix <- cor(numeric_data, use = "complete.obs")

x11()

# Plotting 
corrplot(cor_matrix, method = "color", type = "upper", 
         tl.col = "black", tl.srt = 90,
         addCoef.col = "black", 
         col = colorRampPalette(c("blue", "white", "red"))(200),
         number.cex = 0.7)