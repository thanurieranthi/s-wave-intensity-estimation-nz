install.packages("lme4")
install.packages("lmerTest")
install.packages("dplyr")
install.packages("lattice")


library(lme4)
library(lmerTest)
library(dplyr)
library(lattice)

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

str(eqdata)

control=lmerControl(optimizer="Nelder_Mead",
                    optCtrl=list(method="Brent"))

control2=lmerControl(optimizer="nloptwrap",
                     optCtrl=list(algorithm="NLOPT_LN_COBYLA",
                                  xtol_rel=1e-6,
                                  xtol_abs=1e-10,
                                  ftol_abs=1e-10))


# Fit the null model (unconditional means model)
null_model <- lmer(`log(PGV)` ~ 1 + (1 | StationName), data = eqdata)
summary(null_model)
dotplot(ranef(null_model,condVar=TRUE))

# Extract variance components
variance_components <- as.data.frame(VarCorr(null_model))
var_between <- variance_components$vcov[1]  # Between-group (level-2) variance
var_within <- variance_components$vcov[2]   # Within-group (level-1) variance

# Calculate ICC
ICC <- var_between / (var_between + var_within)
print(paste("ICC:", ICC))


# Calculate the average cluster size
average_cluster_size <- with(eqdata, tapply(`log(PGV)`, StationName, length))
average_cluster_size <- mean(average_cluster_size)
print(paste("Average Cluster Size:", average_cluster_size))


# Calculate the design effect
design_effect <- 1 + (average_cluster_size - 1) * ICC
print(paste("Design Effect:", design_effect))



# Fit the null model with random intercepts for each grouping variable
null_model2 <- lmer(`log(PGV)`~ 1 + (1 | StationName) + (1 | SiteClass) + (1 | EarthquakeID) + (1 | Year_Index), data = eqdata, REML = FALSE,control = control)
summary(null_model2)
dotplot(ranef(null_model2,condVar=TRUE))


# Extract variance components
variance_components2 <- as.data.frame(VarCorr(null_model2))
var_between_station <- variance_components2$vcov[variance_components2$grp == "StationName"]
var_between_site_class <- variance_components2$vcov[variance_components2$grp == "SiteClass"]
var_between_earthquake_id <- variance_components2$vcov[variance_components2$grp == "EarthquakeID"]
var_between_year_index <- variance_components2$vcov[variance_components2$grp == "Year_Index"]
var_within2 <- attr(VarCorr(null_model2), "sc")^2  # Residual variance

# Calculate ICC for each level
ICC_station <- var_between_station / (var_between_station + var_within2)
ICC_site_class <- var_between_site_class / (var_between_site_class + var_within2)
ICC_earthquake_id <- var_between_earthquake_id / (var_between_earthquake_id + var_within2)
ICC_year_index <- var_between_year_index / (var_between_year_index + var_within2)

# Print ICCs
print(paste("ICC for station:", ICC_station))
print(paste("ICC for site class:", ICC_site_class))
print(paste("ICC for earthquake ID:", ICC_earthquake_id))
print(paste("ICC for year index:", ICC_year_index))

# Calculate the average cluster size for each grouping variable
average_cluster_size_station <- mean(tapply(eqdata$`log(PGV)`, eqdata$StationName, length))
average_cluster_size_site_class <- mean(tapply(eqdata$`log(PGV)`, eqdata$SiteClass, length))
average_cluster_size_earthquake_id <- mean(tapply(eqdata$`log(PGV)`, eqdata$EarthquakeID, length))
average_cluster_size_year_index <- mean(tapply(eqdata$`log(PGV)`, eqdata$Year_Index, length))

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
