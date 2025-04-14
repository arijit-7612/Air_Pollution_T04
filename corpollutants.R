setwd("C:/Users/HP/Desktop/CODING/R")
getwd()

# --- Load Required Libraries ---
library(tidyverse)   # For data manipulation
library(corrplot)    # For correlation plot

# --- Step 1: Load the Data ---
data <- read.csv("air_quality_health_impact_data.csv")
head(data)

# --- Step 2: Select Pollutant Columns ---
pollutants <- data %>% select(PM10, PM2_5, NO2, SO2, O3)
summary(pollutants)

# --- Step 3: Compute Correlation Matrix ---
cor_matrix <- cor(pollutants, use = "complete.obs")
print("Correlation Matrix:")
print(cor_matrix)

# --- Step 4: Visualize Correlations ---
# Basic Heatmap
heatmap(cor_matrix, main = "Pollutant Correlation Heatmap",
        col = heat.colors(256), scale = "none")

# Enhanced Correlation Plot
corrplot(cor_matrix, method = "color", type = "upper",
         tl.col = "black", tl.srt = 45, addCoef.col = "black",
         title = "Pollutant Correlation Matrix", mar = c(0,0,1,0))

# --- Step 5: Extract Most Correlated Pairs ---
cor_df <- as.data.frame(as.table(cor_matrix)) %>%
  filter(Var1 != Var2) %>%
  arrange(desc(abs(Freq)))

# Remove duplicates like (A,B) and (B,A)
cor_df_unique <- cor_df %>%
  rowwise() %>%
  mutate(pair = paste(sort(c(Var1, Var2)), collapse = "-")) %>%
  distinct(pair, .keep_all = TRUE) %>%
  select(Var1, Var2, Correlation = Freq)

print("Top Correlated Pollutant Pairs:")
print(head(cor_df_unique, 5))

