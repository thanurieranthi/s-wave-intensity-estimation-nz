install.packages("corrplot")
install.packages("dplyr")

library(dplyr)




setwd("C:/Users/thanu/Desktop/MSc Final Research/Rscript")
eqdata<-read.csv('eqdata.csv',check.names=FALSE)
head(eqdata)
str(eqdata)
summary(eqdata)

eqdata$EarthquakeID<-factor(eqdata$EarthquakeID)
eqdata$EarthquakeKey<-factor(eqdata$EarthquakeKey)
eqdata$StationName<-factor(eqdata$StationName)
eqdata$SiteClass<-factor(eqdata$SiteClass)
eqdata$Year_Index<-factor(eqdata$Year_Index)
eqdata$Month_Index<-factor(eqdata$Month_Index)

str(eqdata)

hist(eqdata$`log(PGV)`,main= NULL, xlab="log(PGV)", ylab="Frequency", col="lightblue", border="black")
hist(eqdata$`log(Pv)`, main="Distribution of log(Pv)", xlab="log(Pv)", ylab="Frequency", col="lightgreen", border="black")

#Boxplots
boxplot(eqdata$`log(PGV)`, ylab="log(PGV)")
boxplot(eqdata$`log(Pv)`, , ylab="log(Pv)")

# Bar Plot for Categorical Variable
barplot(table(eqdata$SiteClass), xlab="SiteClass", ylab="Frequency", col="lightpink", border="black")
barplot(table(eqdata$SiteClass), xlab="SiteClass", ylab="Frequency", col="lightpink", border="black")
barplot(table(eqdata$StationName), xlab="StationName", ylab="Frequency")
barplot(table(eqdata$Year_Index),names.arg=c("2013", "2014", "2015", "2016", "2017", "2018", "2019", "2020", "2021","2022"),  xlab="Year", ylab="Frequency", border="black")
barplot(table(eqdata$Month_Index),names.arg=c("January", "February", "March", "April", "May", "June", "July", "August", "September","October", "November", "December"),  xlab="Month", ylab="Frequency", border="black")




#Scatter plot
ggplot(eqdata, aes(x=`log(Pv)`, y =`log(PGV)`,colour=SiteClass))+
  geom_point()

library(corrplot)
# Correlation Matrix
corr_matrix <- cor(eqdata[, sapply(eqdata, is.numeric)])
corrplot(corr_matrix, method="circle")


# QQ Plots
qqnorm(eqdata$`log(PGV)`, main= "Normal Q-Q Plot of log(PGV)")
qqline(eqdata$`log(PGV)`, col = "red")

qqnorm(eqdata$`log(Pv)`, main= "Normal Q-Q Plot of log(Pv)")
qqline(eqdata$`log(Pv)`, col = "red")

boxplot(`log(PGV)` ~ SiteClass, data=eqdata, main="log(PGV) by SiteClass", xlab="SiteClass", ylab="log(PGV)", cex.main=0.9, font.main=1)

boxplot(log_PGV ~ Year_Index, data=eqdata, main="log(PGV) by YearIndex", xlab="YearIndex", ylab="log(PGV)")

# Randomly select 50 unique Earthquake IDs from eqdata
sampled_earthquake_ids <- eqdata %>%
  distinct(EarthquakeID) %>%
  sample_n(5)

# Filter the original dataset to include only rows with the sampled Earthquake IDs
sampled_data <- eqdata %>%
  filter(EarthquakeID %in% sampled_earthquake_ids$EarthquakeID)


boxplot(log_PGV ~ EarthquakeID, data=sampled_data, main="log(PGV) by EarthquakeID", xlab="EarthquakeID", ylab="log(PGV)")

modelAx1 <- lm(log_PGV~ log_Pa, data = eqdata)
modelAx2 <- lm(log_PGV ~ log_Pv, data = eqdata)
modelAx3 <- lm(log_PGV ~ log_Pd, data = eqdata)

par(mfrow=c(1,1))
with(eqdata, {
  plot(log_Pa,log_PGV)
  abline(modelAx1)
})

par(mfrow=c(1,1))
with(eqdata, {
  plot(log_Pv,log_PGV)
  abline(modelAx2)
})

par(mfrow=c(1,1))
with(eqdata, {
  plot(log_Pd,log_PGV)
  abline(modelAx3)
})



#standardize explanatory variable
eqdata$`log(Pv)` <- scale(eqdata$`log(Pv)`, center = TRUE, scale = TRUE)

#Simple linear regression model
model1<-lm(`log(PGV)`~`log(Pv)`, data = eqdata)
summary(model1)

par(mfrow=c(1,1))
with(eqdata, {
  plot(`log(Pv)`, `log(PGV)`)
  abline(model1)
})

plot(model1, which = 1)
plot(model1, which = 2)

library(ggplot2)

ggplot(eqdata, aes(x=`log(Pv)`, y =`log(PGV)`))+
  geom_point() + geom_smooth(method = "lm")


#residual Plots
plot(model1, which = 1)
plot(model1, which = 2)


#Check data Independence
boxplot(`log(PGV)` ~ SiteClass, data = eqdata)

ggplot(eqdata, aes(x=`log(Pv)`, y =`log(PGV)`,colour=SiteClass))+
  geom_point()

(split_plot <- ggplot(aes(`log(Pv)`, `log(PGV)`), data = eqdata) + 
    geom_point() + 
    facet_wrap(~ SiteClass) + # create a facet for each site class
    ylab("log(PGV)"))



#Modify the model

model2 <- lm(`log(PGV)`~`log(Pv)` + SiteClass , data = eqdata)
summary(model2)









library(lme4)
library(lmerTest)
library(car)
library(nlme)












model2<-lmer(`log(PGV)`~`log(Pv)`+ (1|SiteClass),data=eqdata, REML = FALSE)
summary(model2)
anova(model2)
fixed_effects2<- fixef(model2)
random_effects2<- ranef(model2)
print (random_effects2)
print(fixed_effects2)


model3<-lmer(`log(PGV)`~`log(Pv)`+ (1+`log(Pv)`|SiteClass),data=eqdata, REML = FALSE)
summary(model3)
anova(model3)
fixed_effects3<- fixef(model3)
random_effects3<- ranef(model3)
print (random_effects3)
print(fixed_effects3)



