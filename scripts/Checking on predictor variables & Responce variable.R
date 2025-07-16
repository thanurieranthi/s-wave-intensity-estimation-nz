install.packages("lme4")
install.packages("lmerTest")
install.packages("caret")
install.packages("ggplot2")
install.packages("xtable")
install.packages("leaps")
install.packages("glmnet")
install.packages("sjPlot")
install.packages("dplyr")
install.packages("Metrics")
install.packages("reshape2")

library(lme4)
library(lmerTest)
library(ggplot2)
library(xtable)
library(caret)
library(leaps)
library(glmnet)
library(sjPlot)
library(dplyr)
library(Metrics)
library(reshape2)


#Read data
eqdata_raw <- read.csv("C:/Users/thanu/Desktop/IP_WORK/Data_Final.csv", header = TRUE)
head(eqdata_raw)
summary(eqdata_raw)

cleaned_data <- eqdata_raw[eqdata_raw$Pa != 9.043736232, ]
summary(cleaned_data)

model1x2 <- lm(PGV ~ Pv, data = cleaned_data)
summary(model1x2)

par(mfrow=c(1,1))
with(cleaned_data, {
  plot(Pv, PGV)
  abline(model1x2)
})

#Convert the Character variables to factors
eqdata_raw$StationName <- factor(eqdata_raw$StationName)
eqdata_raw$SiteClass <- factor(eqdata_raw$SiteClass)
levels(eqdata_raw$StationName)

#Removing outliers 
eqdata <- eqdata_raw[eqdata_raw$Pa != 9.043736232, ]
summary(eqdata)

#Dividing data into Train set & Test set
set.seed(123)
trainIndex <- createDataPartition(eqdata$PGV, p = .8, list = FALSE, times = 1)
data_train <- eqdata[trainIndex,]
data_test  <- eqdata[-trainIndex,]


###Choose best simple linear regression model
modelAx1 <- lm(log(PGV)~ log(Pa), data = data_train)
modelAx2 <- lm(log(PGV) ~ log(Pv), data = data_train)
modelAx3 <- lm(log(PGV) ~ log(Pd), data = data_train)
modelBx1 <- lm(log(PGA) ~ log(Pa), data = data_train)
modelBx2 <- lm(log(PGA) ~ log(Pv), data = data_train)
modelBx3 <- lm(log(PGA) ~ log(Pd), data = data_train)
modelCx1 <- lm(log(PGD) ~ log(Pa), data = data_train)
modelCx2 <- lm(log(PGD) ~ log(Pv), data = data_train)
modelCx3 <- lm(log(PGD) ~ log(Pd), data = data_train)

summary(modelAx1)
summary(modelAx2)
summary(modelAx3)
summary(modelBx1)
summary(modelBx2)
summary(modelBx3)
summary(modelCx1)
summary(modelCx2)
summary(modelCx3)

AIC(modelAx1,modelAx2,modelAx3,modelBx1,modelBx2,modelBx3,modelCx1,modelCx2,modelCx3)
BIC(modelAx1,modelAx2,modelAx3,modelBx1,modelBx2,modelBx3,modelCx1,modelCx2,modelCx3)

# Make predictions on the test set
predicted_values_modelAx1 <- predict(modelAx1, newdata = data_test)
predicted_values_modelAx2 <- predict(modelAx2, newdata = data_test)
predicted_values_modelAx3 <- predict(modelAx3, newdata = data_test)
predicted_values_modelBx1 <- predict(modelBx1, newdata = data_test)
predicted_values_modelBx2 <- predict(modelBx2, newdata = data_test)
predicted_values_modelBx3 <- predict(modelBx3, newdata = data_test)
predicted_values_modelCx1 <- predict(modelCx1, newdata = data_test)
predicted_values_modelCx2 <- predict(modelCx2, newdata = data_test)
predicted_values_modelCx3 <- predict(modelCx3, newdata = data_test)

# Calculate RMSE
rmse__modelAx1 <- sqrt(mean(((log(data_test$PGV)) - predicted_values_modelAx1)^2))
rmse__modelAx2 <- sqrt(mean((log(data_test$PGV) - predicted_values_modelAx2)^2))
rmse__modelAx3 <- sqrt(mean((log(data_test$PGV) - predicted_values_modelAx3)^2))
rmse__modelBx1 <- sqrt(mean(((log(data_test$PGA)) - predicted_values_modelBx1)^2))
rmse__modelBx2 <- sqrt(mean((log(data_test$PGA) - predicted_values_modelBx2)^2))
rmse__modelBx3 <- sqrt(mean((log(data_test$PGA) - predicted_values_modelBx3)^2))
rmse__modelCx1 <- sqrt(mean(((log(data_test$PGD)) - predicted_values_modelCx1)^2))
rmse__modelCx2 <- sqrt(mean((log(data_test$PGD) - predicted_values_modelCx2)^2))
rmse__modelCx3 <- sqrt(mean((log(data_test$PGD) - predicted_values_modelCx3)^2))

print(paste("RMSE:", rmse__modelAx1))
print(paste("RMSE:", rmse__modelAx2))
print(paste("RMSE:", rmse__modelAx3))
print(paste("RMSE:", rmse__modelBx1))
print(paste("RMSE:", rmse__modelBx2))
print(paste("RMSE:", rmse__modelBx3))
print(paste("RMSE:", rmse__modelCx1))
print(paste("RMSE:", rmse__modelCx2))
print(paste("RMSE:", rmse__modelCx3))
# Calculate MAPE
mape__modelAx1 <- mean(abs((log(data_test$PGV) - predicted_values_modelAx1) / (log(data_test$PGV)))) * 100
mape__modelAx2 <- mean(abs((log(data_test$PGV) - predicted_values_modelAx2) / (log(data_test$PGV)))) * 100
mape__modelAx3 <- mean(abs((log(data_test$PGV) - predicted_values_modelAx3) / (log(data_test$PGV)))) * 100
mape__modelBx1 <- mean(abs((log(data_test$PGA) - predicted_values_modelBx1) / (log(data_test$PGA)))) * 100
mape__modelBx2 <- mean(abs((log(data_test$PGA) - predicted_values_modelBx2) / (log(data_test$PGA)))) * 100
mape__modelBx3 <- mean(abs((log(data_test$PGA) - predicted_values_modelBx3) / (log(data_test$PGA)))) * 100
mape__modelCx1 <- mean(abs((log(data_test$PGD) - predicted_values_modelCx1) / (log(data_test$PGD)))) * 100
mape__modelCx2 <- mean(abs((log(data_test$PGD) - predicted_values_modelCx2) / (log(data_test$PGD)))) * 100
mape__modelCx3 <- mean(abs((log(data_test$PGD) - predicted_values_modelCx3) / (log(data_test$PGD)))) * 100

print(paste("MAPE:", mape__modelAx1, "%"))    
print(paste("MAPE:", mape__modelAx2, "%")) 
print(paste("MAPE:", mape__modelAx3, "%")) 
print(paste("MAPE:", mape__modelBx1, "%"))    
print(paste("MAPE:", mape__modelBx2, "%")) 
print(paste("MAPE:", mape__modelBx3, "%")) 
print(paste("MAPE:", mape__modelCx1, "%"))    
print(paste("MAPE:", mape__modelCx2, "%")) 
print(paste("MAPE:", mape__modelCx3, "%")) 

###Choose best regression model using Two variables
modelBx1x2 <- lm(log(PGV) ~ log(Pa) + log(Pv), data = data_train)
modelBx1x3 <- lm(log(PGV) ~ log(Pa) + log(Pd), data = data_train)
modelBx2x3 <- lm(log(PGV) ~ log(Pv) + log(Pd), data = data_train)
modelBx1x2 <- lm(log(PGA) ~ log(Pa) + log(Pv), data = data_train)
modelBx1x3 <- lm(log(PGA) ~ log(Pa) + log(Pd), data = data_train)
modelBx2x3 <- lm(log(PGA) ~ log(Pv) + log(Pd), data = data_train)
modelBx1x2 <- lm(log(PGD) ~ log(Pa) + log(Pv), data = data_train)
modelBx1x3 <- lm(log(PGD) ~ log(Pa) + log(Pd), data = data_train)
modelBx2x3 <- lm(log(PGD) ~ log(Pv) + log(Pd), data = data_train)

summary(modelBx1x2)
summary(modelBx1x3)
summary(modelBx2x3)

AIC(modelBx1x2, modelBx1x3, modelBx2x3)
BIC(modelBx1x2, modelBx1x3, modelBx2x3)

# Make predictions on the test set
predicted_values_modelBx1x2 <- predict(modelBx1x2, newdata = data_test)
predicted_values_modelBx1x3 <- predict(modelBx1x3, newdata = data_test)
predicted_values_modelBx2x3 <- predict(modelBx2x3, newdata = data_test)

# Calculate RMSE
rmse__modelBx1x2 <- sqrt(mean((log(data_test$PGV) - predicted_values_modelBx1x2)^2))
rmse__modelBx1x3 <- sqrt(mean((log(data_test$PGV) - predicted_values_modelBx1x3)^2))
rmse__modelBx2x3 <- sqrt(mean((log(data_test$PGV) - predicted_values_modelBx2x3)^2))


print(paste("RMSE:", rmse__modelBx1x2))
print(paste("RMSE:", rmse__modelBx1x3))
print(paste("RMSE:", rmse__modelBx2x3))

# Calculate MAPE
mape__modelBx1x2 <- mean(abs((log(data_test$PGV) - predicted_values_modelBx1x2) / (log(data_test$PGV)))) * 100
mape__modelBx1x3 <- mean(abs((log(data_test$PGV) - predicted_values_modelBx1x3) / (log(data_test$PGV)))) * 100
mape__modelBx2x3 <- mean(abs((log(data_test$PGV) - predicted_values_modelBx2x3) / (log(data_test$PGV)))) * 100

print(paste("MAPE:", mape__modelBx1x2, "%"))    
print(paste("MAPE:", mape__modelBx1x3, "%")) 
print(paste("MAPE:", mape__modelBx2x3, "%")) 

###Choose best regression model using all three variables
modelCx1x2x3 <- lm(log(PGV) ~ log(Pa) + log(Pv) + log(Pd), data = data_train)
summary(modelCx1x2x3)
AIC(modelCx1x2x3)
BIC(modelCx1x2x3)
predicted_values_modelCx1x2x3 <- predict(modelCx1x2x3, newdata = data_test)
rmse__modelCx1x2x3 <- sqrt(mean((log(data_test$PGV) - predicted_values_modelCx1x2x3)^2))
print(paste("RMSE:", rmse__modelCx1x2x3))
mape__modelCx1x2x3 <- mean(abs((log(data_test$PGV) - predicted_values_modelCx1x2x3) / (log(data_test$PGV)))) * 100
print(paste("MAPE:", mape__modelCx1x2x3, "%"))


### Ridge Regression
x = as.matrix(log(data_train[, c("Pa", "Pv", "Pd")]))
y_train = log(data_train$PGV)
y_train <- matrix(y_train, ncol = 1)

x_test = as.matrix(log(data_test[, c("Pa", "Pv", "Pd")]))
y_test = log(data_test$PGV)
y_test <- matrix(y_test, ncol = 1)

lambdas <- 10^seq(2, -3, by = -.1)
ridge_reg = glmnet(x, y_train, nlambda = 25, alpha = 0, family = 'gaussian', lambda = lambdas)

summary(ridge_reg)

cv_ridge <- cv.glmnet(x, y_train, alpha = 0, lambda = lambdas)
optimal_lambda <- cv_ridge$lambda.min
optimal_lambda

# Compute R^2 from true and predicted values
eval_results <- function(true, predicted, df) {
  SSE <- sum((predicted - true)^2)
  SST <- sum((true - mean(true))^2)
  R_square <- 1 - SSE / SST
  RMSE = sqrt(SSE/nrow(df))

  # Model performance metrics
  data.frame(
    RMSE = RMSE,
    Rsquare = R_square
  )
}

# Prediction and evaluation on train data
predictions_train <- predict(ridge_reg, s = optimal_lambda, newx = x)
eval_results(y_train, predictions_train, data_train)

# Prediction and evaluation on test data
predictions_test <- predict(ridge_reg, s = optimal_lambda, newx = x_test)
eval_results(y_test, predictions_test, data_test)

## Lasso Regression
lambdas <- 10^seq(2, -3, by = -.1)

# Setting alpha = 1 implements lasso regression
lasso_reg <- cv.glmnet(x, y_train, alpha = 1, lambda = lambdas, standardize = TRUE, nfolds = 5)
plot(lasso_reg)

# Best 
lambda_best <- lasso_reg$lambda.min 
lambda_best

lasso_model <- glmnet(x, y_train, alpha = 1, lambda = lambda_best, standardize = TRUE)

predictions_train <- predict(lasso_model, s = lambda_best, newx = x)
eval_results(y_train, predictions_train, data_train)

predictions_test <- predict(lasso_model, s = lambda_best, newx = x_test)
eval_results(y_test, predictions_test, data_test)

### multiple linear regression- Pv and site class as predivctor variables  
modelDx2<- lm(log(PGV) ~ log(Pv) + SiteClass, data = data_train)
summary(modelDx2)
AIC(modelDx2)
BIC(modelDx2)
predicted_values_modelDx2 <- predict(modelDx2, newdata = data_test)
rmse__modelDx2 <- sqrt(mean((log(data_test$PGV) - predicted_values_modelDx2)^2))
print(paste("RMSE:", rmse__modelDx2))
mape__modelDx2 <- mean(abs((log(data_test$PGV) - predicted_values_modelDx2) / (log(data_test$PGV)))) * 100
print(paste("MAPE:", mape__modelDx2, "%"))


