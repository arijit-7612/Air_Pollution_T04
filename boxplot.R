library(ggplot2)
library(corrplot)
library(dplyr)
library(readr)
library(reshape2)
setwd("D:/Air pollutionanalysis")
data <- read.csv("air_quality_health_impact_data.csv")

# Boxplot for HospitalAdmissions
boxplot(data$HospitalAdmissions, main = "Boxplot of HospitalAdmissions", 
        col = "skyblue", horizontal = TRUE)

# Boxplot for RespiratoryCases
boxplot(data$RespiratoryCases, main = "Boxplot of RespiratoryCases", 
        col = "skyblue", horizontal = TRUE)

# Boxplot for HealthImpactScore
boxplot(data$HealthImpactScore, main = "Boxplot of HealthImpactScore", 
        col = "skyblue", horizontal = TRUE)

# Boxplot for CardiovascularCases
boxplot(data$CardiovascularCases, main = "Boxplot of CardiovascularCases", 
        col = "skyblue", horizontal = TRUE)