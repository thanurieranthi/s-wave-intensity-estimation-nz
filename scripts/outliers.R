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

#Read data
eqdata_raw <- read.csv("C:/Users/thanu/Desktop/IP_WORK/Data_Final.csv", header = TRUE)
head(eqdata_raw)
summary(eqdata_raw)


# Outlier investigation in preliminary analysis

boxplot(eqdata_raw$Pa)
boxplot(eqdata_raw$Pv)
boxplot(eqdata_raw$Pd)
boxplot(eqdata_raw$PGV)

median_value <- median(eqdata_raw$Pv, na.rm = TRUE)

# Print the median value
print(median_value)

hist(eqdata_raw$Pv)
Q1 <- quantile(eqdata_raw$Pa, 0.25)
Q3 <- quantile(eqdata_raw$Pa, 0.75)
IQR <- Q3 - Q1

lower_whisker <- Q1 - 1.5 * IQR
upper_whisker <- Q3 + 1.5 * IQR


outliers <- which(eqdata_raw$Pa > upper_whisker | eqdata_raw$Pa < lower_whisker)

outlier_data <- eqdata_raw[outliers, ]
cat("Number of outliers:", length(outliers), "\n")
print(outlier_data)

write.xlsx(outlier_data, file = "C:/Users/thanu/Desktop/IP_WORK/OutlierData_Pa.csv"
, rowNames = FALSE)

#Removing outliers from data considering Pa
cleaned_data <- eqdata_raw[eqdata_raw$Pa != 9.043736232, ]
boxplot(cleaned_data$Pa)

##Descriptive statistics

model1p1 <- lm(PGV ~ Pa, data = cleaned_data)
summary(model1p1)
model1p2 <- lm(PGV ~ Pv, data = cleaned_data)
summary(model1p2)
model1p3 <- lm(PGV ~ Pd, data = cleaned_data)
summary(model1p3)

par(mfrow=c(1,1))
with(cleaned_data, {
  plot(Pa, PGV)
  abline(model1p1)
})

par(mfrow=c(1,1))
with(cleaned_data, {
  plot(Pv, PGV)
  abline(model1p2)
})

par(mfrow=c(1,1))
with(cleaned_data, {
  plot(Pd, PGV)
  abline(model1p3)
})


#check correlation
ggpairs(eqdata_raw[, c("Pa", "Pv", "Pd")], title = "Correlation Matrix", lower = list(continuous = wrap("smooth", alpha = 0.5)), upper = list(continuous = wrap("cor", method = "pearson")))


#Removing outliers considering PGV
Q_1 <- quantile(eqdata_raw$PGV, 0.25)
Q_3 <- quantile(eqdata_raw$PGV, 0.75)
IQR1 <- Q_3 - Q_1

lower_whisker1 <- Q_1 - 1.5 * IQR1
upper_whisker1 <- Q_3 + 1.5 * IQR1

outliers1 <- which(eqdata_raw$PGV > upper_whisker1 | eqdata_raw$PGV < lower_whisker1)

outlier_data1 <- eqdata_raw[outliers1, ]
cat("Number of outliers:", length(outliers1), "\n")
print(outlier_data1)

write.xlsx(outlier_data1, file = "C:/Users/thanu/Desktop/IP_WORK/OutlierData_PGV.csv"
           , rowNames = FALSE)







# Assuming your data is in a data frame called 'data'
correlation_matrix <- cor(cleaned_data[, c("Pa", "Pv", "Pd")])

# Create the correlation matrix plot
corrplot(correlation_matrix, method = "circle", type = "upper", tl.col = "black", tl.srt = 45)


boxplot(`log(PGV)` ~ , data=eqdata, main="log(PGV) by SiteClass", xlab="SiteClass", ylab="log(PGV)", cex.main=0.9, font.main=1)

