install.packages("lme4")
install.packages("lmerTest")
install.packages("dplyr")
install.packages("lattice")
install.packages("car")
install.packages("nlme")
install.packages("performance")
install.packages("sjPlot")
install.packages("glmmTMB")
install.packages("flexplot")

library(lme4)
library(lmerTest)
library(dplyr)
library(lattice)
library(car)
library(nlme)
library(performance)
library(sjPlot)
library(glmmTMB)
library(ggplot2)
library(sjPlot)
library(glmmTMB)


setwd("C:/Users/thanu/Desktop/MSc Final Research/Rscript")
eqdata<-read.csv('eqdata.csv',check.names=FALSE)
head(eqdata)
str(eqdata)

eqdata$EarthquakeID<-factor(eqdata$EarthquakeID)
eqdata$EarthquakeKey<-factor(eqdata$EarthquakeKey)
eqdata$StationName<-factor(eqdata$StationName)
eqdata$SiteClass<-factor(eqdata$SiteClass)
eqdata$Year_Index<-factor(eqdata$Year_Index)
eqdata$Month_Index<-factor(eqdata$Month_Index)


# Split data into training and testing sets
set.seed(123)
train_index <- sample(seq_len(nrow(eqdata)), size = 0.8 * nrow(eqdata))
train_data <- eqdata[train_index, ]
test_data <- eqdata[-train_index, ]





#ordinary least-squares (OLS) multiple regression / Classical regression models
model_ols<-lm(log_PGV ~ log_Pv, data = train_data)
summary(model_ols)
plot(model_ols)
AIC(model_ols)
BIC(model_ols)


# residuals_ols <- resid(model_ols)
# sd_error_ols <- sd(residuals_ols)
# print(sd_error_ols)
# cr_model2<-lm(log_PGV ~ log_Pv+SiteClass+ log_Pv*SiteClass, data = train_data)
# summary(cr_model2)

# Linear mixed effect modeling optimizer
control <- lmerControl(optimizer = "bobyqa")


#Null model
model_null <- lmer(log_PGV ~ 1 + (1 | SiteClass) + (1|StationName) + (1| EarthquakeID) + (1| Year_Index), data = train_data, REML = FALSE, control = control)
summary(model_null)
anova(model_null)
performance::r2(model_null)
dotplot(ranef(model_null,condVar=TRUE))

# Extract variance components
variance_components <- as.data.frame(VarCorr(model_null))
var_between_station <- variance_components$vcov[variance_components$grp == "StationName"]
var_between_site_class <- variance_components$vcov[variance_components$grp == "SiteClass"]
var_between_earthquake_id <- variance_components$vcov[variance_components$grp == "EarthquakeID"]
var_between_year_index <- variance_components$vcov[variance_components$grp == "Year_Index"]
var_within <- attr(VarCorr(model_null), "sc")^2  # Residual variance

# Calculate ICC for each 
ICC_station <- var_between_station / (var_between_station + var_within)
ICC_site_class <- var_between_site_class / (var_between_site_class + var_within)
ICC_earthquake_id <- var_between_earthquake_id / (var_between_earthquake_id + var_within)
ICC_year_index <- var_between_year_index / (var_between_year_index + var_within)

# Print ICCs
print(paste("ICC for station:", ICC_station))
print(paste("ICC for site class:", ICC_site_class))
print(paste("ICC for earthquake ID:", ICC_earthquake_id))
print(paste("ICC for year index:", ICC_year_index))

# Calculate the average cluster size for each grouping variable
average_cluster_size_station <- mean(tapply(eqdata$log_PGV, eqdata$StationName, length))
average_cluster_size_site_class <- mean(tapply(eqdata$log_PGV, eqdata$SiteClass, length))
average_cluster_size_earthquake_id <- mean(tapply(eqdata$log_PGV, eqdata$EarthquakeID, length))
average_cluster_size_year_index <- mean(tapply(eqdata$log_PGV, eqdata$Year_Index, length))

# Calculate the design effect for each grouping variable
design_effect_station <- 1 + (average_cluster_size_station - 1) * ICC_station
design_effect_site_class <- 1 + (average_cluster_size_site_class - 1) * ICC_site_class
design_effect_earthquake_id <- 1 + (average_cluster_size_earthquake_id - 1) * ICC_earthquake_id
design_effect_year_index <- 1 + (average_cluster_size_year_index - 1) * ICC_year_index

# Print design effects
print(paste("Design effect for station:", design_effect_station))
print(paste("Design effect for site class:", design_effect_site_class))
print(paste("Design effect for earthquake ID:", design_effect_earthquake_id))
print(paste("Design effect for year index:", design_effect_year_index))


#Random Intercept model
model_ri <- lmer(log_PGV ~ 1 + log_Pv + (1|StationName) + (1| EarthquakeID) + (1| Year_Index)  , data = train_data, REML = FALSE, control = control)
summary(model_ri)
anova(model_ri)
performance::r2(model_ri)
dotplot(ranef(model_ri,condVar=TRUE))
plot(model_ri)

anova(model_null, model_ri)

#Random slope model
model_ris_1 <- lmer(log_PGV ~ 1 + log_Pv+(1 + log_Pv|StationName) + (1| EarthquakeID) + (1| Year_Index), data = train_data, REML = FALSE, control = control)
summary(model_ris_1)
anova(model_ris_1)
performance::r2(model_ris_1)
dotplot(ranef(model_ris_1$StationName,condVar=TRUE))

#Fixed effects
fixed_effects_model_ris_1  <- summary(model_ris_1)$coefficients
print(fixed_effects_model_ris_1)


# Extract random effects variance components
ranef_var_model_ris_1 <- VarCorr(model_ris_1)
print(ranef_var_model_ris_1)

# standard error of residuals
residuals_ris_1 <- resid(model_ris_1)

sd_error_ris_1 <- sd(residuals_ris_1)

print(sd_error_ris_1)

anova(model_ri,model_ris_1)




model_ris_2 <- lmer(log_PGV ~ 1 + log_Pv + (1 + log_Pv|StationName) +(1 + log_Pv| Year_Index)+ (1| EarthquakeID), data = train_data, REML = FALSE, control = control)
summary(model_ris_2)
anova(model_ris_2)
performance::r2(model_ris_2)
dotplot(ranef(model_ris_2,condVar=TRUE))





model_ris_3 <- lmer( log_PGV ~ 1 + log_Pv + (1 + log_Pv|StationName) + (1 + log_Pv| EarthquakeID) + (1 | Year_Index), data = train_data, REML = FALSE, control = control)
summary(model_ris_3)
anova(model_ris_3)
performance::r2(model_ris_3)
dotplot(ranef(model_ris_3,condVar=TRUE))

anova(model_null,model_ri,model_ris_1,model_ris_2,model_ris_3)
anova(model_null,model_ri)
anova(model_ri,model_ris_1)
anova(model_ris_1,model_ris_2)
anova(model_ris_2,model_ris_3)




plot_model(model2.2, type = "diag") # Residual diagnostics


AIC(cr_model1, cr_model2)
BIC(cr_model1, cr_model2)


# Make predictions on the test set
predicted_values_cr_model1 <- predict(cr_model1, newdata = test_data)
predicted_values_cr_model2 <- predict(cr_model2, newdata = test_data)
predicted_values_null_model <- predict(null_model, newdata = test_data, allow.new.levels = TRUE)
predicted_values_model1.1 <- predict(model1.1, newdata = test_data, allow.new.levels = TRUE)
predicted_values_model2.1 <- predict(model2.1, newdata = test_data, allow.new.levels = TRUE)
predicted_values_model2.2 <- predict(model2.2, newdata = test_data, allow.new.levels = TRUE)
predicted_values_model2.3 <- predict(model2.3, newdata = test_data, allow.new.levels = TRUE)


# Calculate RMSE
rmse__cr_model1 <- sqrt(mean(((test_data$log_PGV - predicted_values_cr_model1)^2)))
rmse__cr_model2 <- sqrt(mean(((test_data$log_PGV - predicted_values_cr_model2)^2)))
rmse__null_model <- sqrt(mean(((test_data$log_PGV - predicted_values_null_model)^2)))
rmse__model1.1 <- sqrt(mean(((test_data$log_PGV - predicted_values_model1.1)^2)))
rmse__model2.1 <- sqrt(mean(((test_data$log_PGV - predicted_values_model2.1)^2)))
rmse__model2.2 <- sqrt(mean(((test_data$log_PGV - predicted_values_model2.2)^2)))
rmse__model2.3 <- sqrt(mean(((test_data$log_PGV - predicted_values_model2.3)^2)))


print(paste("RMSE:", rmse__cr_model1))
print(paste("RMSE:", rmse__cr_model2))
print(paste("RMSE:", rmse__null_model))
print(paste("RMSE:", rmse__model1.1))
print(paste("RMSE:", rmse__model2.1))
print(paste("RMSE:", rmse__model2.2))
print(paste("RMSE:", rmse__model2.3))


# Calculate MAPE
mape__cr_model1 <- mean(abs((test_data$log_PGV - predicted_values_cr_model1) / (test_data$log_PGV))) * 100
mape__cr_model2 <- mean(abs((test_data$log_PGV - predicted_values_cr_model2) / (test_data$log_PGV))) * 100
mape__null_model <- mean(abs((test_data$log_PGV - predicted_values_null_model) / (test_data$log_PGV))) * 100
mape__model1.1 <- mean(abs((test_data$log_PGV - predicted_values_model1.1) / (test_data$log_PGV))) * 100
mape__model2.1 <- mean(abs((test_data$log_PGV - predicted_values_model2.1) / (test_data$log_PGV))) * 100
mape__model2.2 <- mean(abs((test_data$log_PGV - predicted_values_model2.2) / (test_data$log_PGV))) * 100
mape__model2.3 <- mean(abs((test_data$log_PGV - predicted_values_model2.3) / (test_data$log_PGV))) * 100



print(paste("MAPE:", mape__cr_model1, "%")) 
print(paste("MAPE:", mape__cr_model2, "%"))
print(paste("MAPE:", mape__null_model, "%"))
print(paste("MAPE:", mape__model1.1, "%"))
print(paste("MAPE:", mape__model2.1, "%"))
print(paste("MAPE:", mape__model2.2, "%"))
print(paste("MAPE:", mape__model2.3, "%"))






all_coefs <- coef(model2.2)
str(all_coefs)
all_coefs <- unlist(all_coefs)
ordered_coefs <- sort(all_coefs)
first_coefs <- ordered_coefs[1:2:10]
print(first_coefs)
last_coefs <- ordered_coefs[(length(ordered_coefs)-10):length(ordered_coefs)]
all_selected_coefs <- c(first_coefs, last_coefs)
print(all_selected_coefs)

head(train_data)

library(flexplot)
simple_model <- lmer(log_PGV ~ 1 + log_Pv + (1 + log_Pv|StationName), data = train_data, REML = FALSE, control = control)
visualize(simple_model, plot ="model", sample=288, pch = 8)
p + theme(legend.text = element_text(size =2))
visualize(simple_model, plot ="model",formula =log_PGV~log_Pv | StationName,  sample=5, legend(pch = 8) )
visualize(simple_model, plot ="model",formula =log_PGV~log_Pv + StationName,  sample=5,legend(pch = 2) )

simple_model2 <- lmer(log_PGV ~ 1 + log_Pv + (1 + log_Pv|StationName) + (1 + log_Pv| EarthquakeID) + (1| Year_Index) , data = train_data, REML = FALSE, control = control)
visualize(simple_model2, plot ="model", sample=20 )
visualize(simple_model2,formula =log_PGV~log_Pv |Year_Index ,  sample=10 )
visualize(simple_model2,formula =log_PGV~log_Pv + StationName,  sample=15)


install.packages("devtools")
library(devtools)


# Plot data
install.packages("emmeans")
library(emmeans)


# Subset schools
subsch = sort(unique(train_data$StationName))[95:109]
# Get the data for these schools and name it as "dat"
dat = train_data[train_data$StationName %in% subsch,c("StationName","log_Pv","log_PGV")]
dat$StationName = as.factor(dat$StationName)
xyplot(log_PGV~log_Pv|as.factor(Year_Index),
       group=StationName, type=c("p","r"), test_data)

# load the lattice package
library(lattice)
# Call xyplot and add regression lines
xyplot(log_PGV~log_Pv|StationName,type=c("p","r"), lwd=3, dat)

par(mar = c(4, 4, .1, .1))
xyplot(log_PGV~log_Pv, group=StationName, type=c("p","r"),
       auto.key = list(columns = nlevels(dat$StationName)), dat)

par(mar = c(4, 4, .1, .1))
xyplot(log_PGV~log_Pv, group=StationName, type=c("p","r"), train_data)



# Sort the data for better plotting
dSci= eqdata[order(train_data$StationName, train_data$log_Pv),]
# Make the plot
xyplot(log_PGV~log_Pv, group=StationName, type=c("p","r"), dSci)



predicted_values_cr_model1 <- predict(cr_model1, newdata = test_data)
predicted_values_model2.2 <- predict(model2.2, newdata = test_data, allow.new.levels = TRUE)

#Step 5: Define the number of stations to analyze (modify as needed)
num_stations <- 15
par(mfrow = c(3, 5))

# Step 6: Loop through stations and create plots
for (i in 1:num_stations) {
  # Extract station ID for the current iteration (assuming order reflects stations)
  current_station_id <- eqdata$StationName[i]
  
  # Filter data for the current station
  station_subset <- subset(eqdata, StationName == current_station_id)
  actual_intensity <- test_data$log_PGV  # Assuming "s_wave_intensity" is the outcome variable
  
  # Extract predictions from fitted models (assuming appropriate variable names)
  simple_predictions_subset <- predict(cr_model1, newdata = test_data)
  multilevel_predictions_subset <- predict(model2.2, newdata = test_data, allow.new.levels = TRUE)
  
  # Create the scatter plot
  plot(actual_intensity, simple_predictions_subset, 
       pch = 16, col = "blue", 
       xlab = "Actual S-wave Intensity", 
       ylab = "Predicted S-wave Intensity (Simple Model)",
       main = paste("Station", current_station_id))
  
  # Add points for the multilevel model predictions
  points(actual_intensity, multilevel_predictions_subset, 
         pch = 16, col = "red")
  
  # Add legend
  legend("topright", legend = c("Simple Model", "Multilevel Model"), 
         pch = c(16, 16), col = c("blue", "red"))
  
  # Add regression line (optional)
  abline(lm(actual_intensity ~ simple_predictions_subset))
  abline(lm(actual_intensity ~ multilevel_predictions_subset), col = "red")
}


subsch <- sort(unique(train_data$StationName))[95:100]
par(mfrow = c(3, 2))
for (station_name in subsch) {
  # Filter data for the current station
  station_subset <- subset(eqdata, StationName == station_name)
  actual_intensity <- station_subset$log_PGV 
  simple_predictions_subset <- predict(cr_model1, newdata = station_subset)
  multilevel_predictions_subset <- predict(model2.2, newdata = station_subset, allow.new.levels = TRUE)
  plot(actual_intensity, simple_predictions_subset, 
       pch = 16, col = "blue", 
       xlab = "Actual S-wave Intensity", 
       ylab = "Predicted S-wave Intensity (Simple Model)",
       main = paste("Station", station_name))
  
  # Add points for the multilevel model predictions
  points(actual_intensity, multilevel_predictions_subset, 
         pch = 16, col = "red")
  
  # Add legend
  legend("topright", legend = c("Simple Model", "Multilevel Model"), 
         pch = c(16, 16), col = c("blue", "red"))
  
  # Add regression line (optional)
  abline(lm(actual_intensity ~ simple_predictions_subset), col = "yellow")
  abline(lm(actual_intensity ~ multilevel_predictions_subset), col = "red")
}

# Make predictions using fixed effects only and then using both fixed and random effects
train_data <- train_data %>%
  mutate(pred_fixef = predict(model_ris_1, newdata = ., re.form = NA),
         pred_ranef = predict(model_ris_1, newdata = ., re.form = ~(1 + log_Pv | StationName)),
         abs_diff = abs(pred_fixef - pred_ranef))

train_data <- train_data %>%
  mutate(pred_ols = predict(model_ols, newdata = .))

# Summarize the average absolute differences by station
station_diffs <- train_data %>%
  group_by(StationName) %>%
  summarize(avg_abs_diff = mean(abs_diff, na.rm = TRUE)) %>%
  arrange(desc(avg_abs_diff))

# Select the top 15 stations with the largest average absolute differences
top_stations <- station_diffs %>%
  top_n(6, avg_abs_diff) %>%
  pull(StationName)

# Filter the dataset to include only the selected stations
filtered_data <- train_data %>%
  filter(StationName %in% top_stations)


# # Sample a few stations (e.g., 5 stations)
# set.seed(123)  # Set seed for reproducibility
# sampled_stations <- train_data %>% 
#   distinct(StationName) %>%
#   sample_n(15) %>%
#   pull(StationName)

# # Filter the dataset based on the sampled stations
# filtered_data <- train_data %>%
#   filter(StationName %in% sampled_stations)








# Make predictions using fixed effect only and then random effects and plot the results
filtered_data %>%
  mutate(pred_fixef = predict(model_ris_1, newdata = ., re.form = NA),
         pred_ranef = predict(model_ris_1, newdata = ., re.form = ~(1+log_Pv|StationName)),
         pred_ols = predict(model_ols, newdata = .)) %>%
  ggplot(aes(x = log_Pv, y = log_PGV)) +
  geom_point(shape = 21,
             size = 0.6,
             color = "black",
             fill = "grey",
             show.legend = TRUE) +
  geom_line(aes(y = pred_fixef),
            color = "blue",
            size = 1) +
  geom_line(aes(y = pred_ranef),
            color = "green",
            size = 1) +
  geom_line(aes(y = pred_ols),
              color = "red",
              size = 0.8) +
  facet_wrap(~StationName) +
  labs(x = "log_Pv",
       y = "log_PGV",
       color = "Legend") +
  theme(legend.position = "bottom") +
  scale_color_manual(values = c("Fixed Effects Prediction" = "blue",
                                "Random Effects Prediction" = "green",
                                "OLS Prediction" = "red"))


filtered_data %>%
  mutate(pred_fixef = predict(model_ris_1, newdata = ., re.form = NA),
         pred_ranef = predict(model_ris_1, newdata = ., re.form = ~(1+log_Pv|StationName)),
         pred_ols = predict(model_ols, newdata = .)) %>%
  ggplot(aes(x = log_Pv, y = log_PGV)) +
  geom_point(shape = 21,
             size = 0.6,
             color = "black",
             fill = "grey",
             show.legend = TRUE) +
  geom_line(aes(y = pred_fixef, color = "Fixed Effects Prediction"),
            size = 1) +
  geom_line(aes(y = pred_ranef, color = "Random Effects Prediction"),
            size = 1) +
  geom_line(aes(y = pred_ols, color = "OLS Prediction"),
            size = 0.8) +
  facet_wrap(~StationName) +
  labs(x = "log_Pv",
       y = "log_PGV") +
  theme(legend.position = "bottom",
  legend.text = element_text(size = 12),
  legend.title = element_blank()) +
  scale_color_manual(values = c("Fixed Effects Prediction" = "blue",
                                "Random Effects Prediction" = "green",
                                "OLS Prediction" = "red"),
                     breaks = c( "Fixed Effects Prediction", "Random Effects Prediction", "OLS Prediction")
)


# Function to calculate RMSE on test set
calc_rmse <- function(model, data) {
  predictions <- predict(model, newdata = data, allow.new.levels = TRUE)
  sqrt(mean((data$log_PGV - predictions)^2))
}

# Function to calculate MSE on test set
calc_mse <- function(model, data) {
  predictions <- predict(model, newdata = data,allow.new.levels = TRUE)
  mean((data$log_PGV - predictions)^2)
}

# Function to calculate MAPE on test set
calc_mape <- function(model, data) {
  predictions <- predict(model, newdata = data, allow.new.levels = TRUE)
  mean(abs((data$log_PGV - predictions) / data$log_PGV)) * 100
}





# Function to calculate R² for linear mixed models
calc_r2 <- function(model) {
  performance::r2(model)
}

# Function to check model convergence
# Function to check convergence
check_convergence <- function(model) {
  if (is.null(model@optinfo$conv$opt)) {
    return("converged")
  } else {
    return("not converged")
  }
}

# Extract model comparison metrics
model_summaries <- data.frame(
  Model = c("cr_model1", "null_model", "model1.1", "model2.1", "model2.2", "model2.3"),
  AIC = c(AIC(cr_model1), AIC(null_model), AIC(model1.1), AIC(model2.1), AIC(model2.2), AIC(model2.3)),
  BIC = c(BIC(cr_model1), BIC(null_model), BIC(model1.1), BIC(model2.1), BIC(model2.2), BIC(model2.3)),
  R2_marginal = c(NA, calc_r2(null_model)$R2_marginal, calc_r2(model1.1)$R2_marginal, calc_r2(model2.1)$R2_marginal,calc_r2(model2.2)$R2_marginal,calc_r2(model2.3)$R2_marginal),
  R2_conditional = c(NA, calc_r2(null_model)$R2_conditional, calc_r2(model1.1)$R2_conditional, calc_r2(model2.1)$R2_conditional,calc_r2(model2.2)$R2_conditional,calc_r2(model2.3)$R2_conditional),
  RMSE = c(calc_rmse(cr_model1, test_data), calc_rmse(null_model, test_data), calc_rmse(model1.1, test_data), calc_rmse(model2.1, test_data),calc_rmse(model2.2, test_data),calc_rmse(model2.3, test_data)),
  MSE = c(calc_mse(cr_model1, test_data), calc_mse(null_model, test_data), calc_mse(model1.1, test_data), calc_mse(model2.1, test_data),calc_mse(model2.2, test_data),calc_mse(model2.2, test_data)),
  MAPE = c(calc_mape(cr_model1, test_data), calc_mape(null_model, test_data), calc_mape(model1.1, test_data), calc_mape(model2.1, test_data),calc_mape(model2.2, test_data),calc_mape(model2.3, test_data)),
  Convergence = c(NA, check_convergence(null_model), check_convergence(model1.1), check_convergence(model2.1),check_convergence(model2.2),check_convergence(model2.3))
)

# Display the summary table
print(model_summaries)



anova(null_model, model1.1, model2.1, model2.2, model2.3)


##Checking model assumptions>>>>>>

# Extract residuals and fitted values
residuals <- residuals(model2.1)
fitted <- fitted(model2.1)

# 1. Linearity
# Plot observed vs fitted values
ggplot(data.frame(fitted, log_PGV = train_data$log_PGV), aes(x = fitted, y = log_PGV)) +
  geom_point() +
  geom_abline(slope = 1, intercept = 0, col = 'red') +
  labs(x = "Fitted Values", y = "Observed Values")

# Residuals vs fitted values plot
ggplot(data.frame(fitted, residuals), aes(x = fitted, y = residuals)) +
  geom_point() +
  geom_hline(yintercept = 0, col = 'red') +
  labs(x = "Fitted Values", y = "Residuals")

# 2. Normality of residuals
# Q-Q plot of the residuals
qqnorm(residuals)
qqline(residuals, col = 'red')

# Histogram of the residuals
ggplot(data.frame(residuals), aes(x = residuals)) +
  geom_histogram(aes(y = ..density..), binwidth = 0.2, fill = 'darkgray', alpha = 0.5) +
  geom_density(col = 'red') +
  labs(x = "Residuals", y = "Density")

plot(model2.1, refit = TRUE)  # Residuals vs fitted values
plot(model2.1, cookwd = TRUE)

# 4. Independence of residuals
# Plot residuals over time or order of observations (if time/order data available)
# Here, assuming the order of observations is in the row numbers
ggplot(data.frame(order = 1:length(residuals), residuals), aes(x = order, y = residuals)) +
  geom_point() +
  geom_line() +
  geom_hline(yintercept = 0, col = 'red') +
  labs(title = "Residuals Over Order of Observations", x = "Order", y = "Residuals") 

plot(model2.1, residuals = TRUE)
plot(model2.1, residuals = TRUE)

# Extract random effects and convert to data frame
random_effects <- ranef(model2.1)
station_random_effects <- as.data.frame(random_effects$StationName) %>% mutate(grp = rownames(.))
earthquake_random_effects <- as.data.frame(random_effects$EarthquakeID) %>% mutate(grp = rownames(.))
year_random_effects <- as.data.frame(random_effects$Year_Index) %>% mutate(grp = rownames(.))

# Plot random effects for StationName using ggplot2
ggplot(station_random_effects, aes(x = grp, y = `(Intercept)`)) +
  geom_point() +
  geom_errorbar(aes(ymin = `(Intercept)` - 1.96 * `(Intercept)`, ymax = `(Intercept)` + 1.96 * `(Intercept)`), width = 0.1) +
  labs(title = "Random Effects for StationName", x = "Station Name", y = "Random Effect") +
  theme(axis.text.x = element_text(angle = 90))

qqnorm(station_random_effects$`(Intercept)`)
qqline(station_random_effects$`(Intercept)`, col = "red")

# Plot random effects for EarthquakeID using ggplot2
ggplot(earthquake_random_effects, aes(x = grp, y = `(Intercept)`)) +
  geom_point() +
  geom_errorbar(aes(ymin = `(Intercept)` - 1.96 * `(Intercept)`, ymax = `(Intercept)` + 1.96 * `(Intercept)`), width = 0.1) +
  labs(title = "Random Effects for EarthquakeID", x = "Earthquake ID", y = "Random Effect") +
  theme(axis.text.x = element_text(angle = 90, hjust = 1))

# Plot random effects for Year_Index using ggplot2
ggplot(year_random_effects, aes(x = grp, y = `(Intercept)`)) +
  geom_point() +
  geom_errorbar(aes(ymin = `(Intercept)` - 1.96 * `(Intercept)`, ymax = `(Intercept)` + 1.96 * `(Intercept)`), width = 0.1) +
  labs(title = "Random Effects for Year_Index", x = "Year Index", y = "Random Effect") +
  theme(axis.text.x = element_text(angle = 90, hjust = 1))

library(performance)
check_model(model2.1)
diagnostic_plots <- plot(check_model(model2.1, panel = FALSE))
diagnostic_plots[[1]]
diagnostic_plots[[2]]
diagnostic_plots[[3]]
diagnostic_plots[[4]]
diagnostic_plots[[5]]
diagnostic_plots[[6]]
diagnostic_plots[[7]]
diagnostic_plots[[8]]




##Preliminary Analysis plot
# Time-Based Plots
ggplot(eqdata, aes(x = Year_Index, y = log_PGV)) +
  geom_boxplot() +
  labs(title = "Time Series Plot of log_PGV", x = "Year Index", y = "log_PGV")

ggplot(eqdata, aes(x = Year_Index, y = log_Pv)) +
  geom_boxplot() +
  labs(title = "Time Series Plot of log_Pv", x = "Year Index", y = "log_Pv")


# Sample 25 stations
sampled_stations <- eqdata %>%
  distinct(StationName) %>%
  sample_n(50)

# Filter the data to include only the sampled stations
sampled_data <- eqdata %>%
  filter(StationName %in% sampled_stations$StationName)

# Group-Based Plots
ggplot(sampled_data, aes(x = StationName, y = log_PGV)) +
  geom_boxplot() +
  labs(title = "Boxplot of log_PGV by StationName", x = "StationName", y = "log_PGV") +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))  # Adjust for readability


# Correlation Matrix
cor_matrix <- cor(eqdata[, c("log_PGV", "log_Pv")])
print(cor_matrix)


# Anova tests
anova_model1 <- aov(log_PGV ~ StationName, data = eqdata)
summary(anova_model1)

anova_model2 <- aov(log_PGV ~ Year_Index, data = eqdata)
summary(anova_model2)

anova_model3 <- aov(log_PGV ~ EarthquakeID, data =eqdata)
summary(anova_model3)

#Prediction
coef(model_ris_1)
ranef(model_ris_1)


VarCorrMat <- as.data.frame(VarCorr(model_ris_1))

