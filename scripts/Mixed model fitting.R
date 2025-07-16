install.packages("car")
install.packages("nlme")
install.packages("performance")
install.packages("sjPlot")
install.packages("glmmTMB")




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


#Multiple linear Regression 

model1<-lm(`log(PGV)`~`log(Pv)`+SiteClass, data = eqdata)
summary(model1)

model2<-lm(`log(PGV)`~`log(Pv)`+SiteClass+`log(Pv)`*SiteClass, data = eqdata)
summary(model2)

model3<-lm(`log(PGV)`~`log(Pv)`+SiteClass+ StationName +`log(Pv)`*SiteClass +`log(Pv)`*StationName, data = eqdata)
summary(model3)


library(lme4)
library(lmerTest)
library(car)
library(nlme)
library(performance)

control=lmerControl(optimizer="Nelder_Mead",
                    optCtrl=list(method="Brent"))

control2=lmerControl(optimizer="nloptwrap",
                     optCtrl=list(algorithm="NLOPT_LN_COBYLA",
                                  xtol_rel=1e-6,
                                  xtol_abs=1e-10,
                                  ftol_abs=1e-10))


# Random intercept for station
modelR1<-lmer(`log(PGV)`~
               1+`log(Pv)`+
               (1|StationName),
               data=eqdata,REML = FALSE,control = control)

# add Random Intercepts for Earthquake ID
modelR2<-lmer(`log(PGV)`~
                1+`log(Pv)`+
                (1|StationName)+ (1|EarthquakeID),
              data=eqdata,REML = FALSE,control = control)


# add Random Intercepts for Site Class
modelR3<-lmer(`log(PGV)`~
                1+`log(Pv)`+
                (1|StationName)+ (1|EarthquakeID) + (1 | SiteClass),
              data=eqdata,REML = FALSE,control = control)




#add Random Intercepts for Year Index
modelR4<-lmer(`log(PGV)`~
                1+`log(Pv)`+
                (1|StationName)+ (1|EarthquakeID) + (1 | SiteClass) + (1 | Year_Index),
              data=eqdata,REML = FALSE,control = control)




# Random Slopes and interceot for station
modelR5<-lmer(`log(PGV)`~
                1+`log(Pv)`+
                (1+`log(Pv)`|StationName)+ (1|EarthquakeID) + (1 | SiteClass) + (1 | Year_Index),
              data=eqdata,REML = FALSE,control = control)


# Random Slopes and Intercepts for Earthquake ID
modelR6<-lmer(`log(PGV)`~
                1+`log(Pv)`+
                (1+`log(Pv)`|StationName)+ (1+`log(Pv)`|EarthquakeID) + (1 | SiteClass) + (1 | Year_Index),
              data=eqdata,REML = FALSE,control = control)



# Interaction between Pv and Site Class
modelR7<-lmer(`log(PGV)`~
                1+`log(Pv)`* SiteClass +
                (1+`log(Pv)`|StationName)+ (1+`log(Pv)`|EarthquakeID) + (1 | Year_Index),
              data=eqdata,REML = FALSE,control = control)

modelR8<-lmer(`log(PGV)`~
                1+`log(Pv)`* SiteClass +
              (1 | StationName)+ (1|EarthquakeID) + (1 | Year_Index),
              data=eqdata,REML = FALSE,control = control)

# Full Model with All Random Effects
modelR9<-lmer(`log(PGV)`~
                1+`log(Pv)`+ SiteClass + Year_Index +
              (1 | StationName)+ (1|EarthquakeID) + (1 | SiteClass) + (1 | Year_Index),
              data=eqdata,REML = FALSE,control = control)



anova(modelR1, modelR2, modelR3, modelR4, modelR5, modelR6, modelR7, modelR8, modelR9)


library(sjPlot)
library(glmmTMB)
plot_model(modelR7, type = "diag") # Residual diagnostics

# Split data into training and testing sets
set.seed(123)
train_index <- sample(seq_len(nrow(eqdata)), size = 0.8 * nrow(eqdata))
train_data <- eqdata[train_index, ]
test_data <- eqdata[-train_index, ]

# Fit model on training data
final_model <- modelR7<-lmer(`log(PGV)`~
                               1+`log(Pv)`* SiteClass +
                               (1+`log(Pv)`|StationName)+ (1+`log(Pv)`|EarthquakeID) + (1 | Year_Index),
                             data=eqdata,REML = FALSE,control = control)

# Predict on test data
predicted_values_modelR7 <- predict(final_model, newdata = test_data)


# Calculate RMSE
rmse__modelR7 <- sqrt(mean(((test_data$`log(PGV)` - predicted_values_modelR7)^2)))



print(paste("RMSE:", rmse__modelR7))


# Calculate MAPE
mape__modelR7 <- mean(abs((test_data$`log(PGV)` - predicted_values_modelR7) / (test_data$`log(PGV)`))) * 100


print(paste("MAPE:", mape__modelR7, "%"))    




