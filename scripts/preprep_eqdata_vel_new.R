install.packages("lme4")
install.packages("lmerTest")
install.packages("caret")
install.packages("ggplot2")
install.packages("xtable")
install.packages("openxlsx")
install.packages("GGally")
install.packages("corrplot")


library(lme4)
library(lmerTest)
library(ggplot2)
library(xtable)
library(caret)
library(openxlsx)
library(GGally)
library(corrplot)

# Read data
eqdata_vel_new_raw <- read.csv("/Users/admin/Desktop/WORK/s-wave-intensity-estimation-nz/data/eqdata_vel_new.csv", header = TRUE)
head(eqdata_vel_new_raw)
summary(eqdata_vel_new_raw)


# Outlier investigation in preliminary analysis

boxplot(eqdata_vel_new_raw$Pv)
boxplot(eqdata_vel_new_raw$PGV)
boxplot(eqdata_vel_new_raw$PGV_Pv_ratio)
