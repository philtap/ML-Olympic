#--------------------------------------------------------------------------------------------------
#   
#       DMML project  
#       Script    : DMML_ML_methods_Eco.R (Economic)
#       Author    : Philippe Tap
#       Date      : April 2020
#
#--------------------------------------------------------------------------------------------------
# This script apply ML methods to the first data set, based on general and economic factors
#  - economic factors : Population and GDPpC a year before the current Olympic 
#  - general factors: 
#        - Was the country host before or after the current games pre_host2, pre_host,  Host, post_host,post_host2
#        - Number of events entered by the country: TotalEntries
#        - Awarded.Total: total medal awarded at the current games
# The following models are applied
# EcoLm1	  all eco variables
# EcoLm1	  all eco variables (polynomial)
# EcoRf1	  tuning=1
# EcoRf2	  tuning=10
# EcokNN1	  k=33
# EcokNN2	  k=3
# EcokNN3	  k=15
#--------------------------------------------------------------------------------------------------

# Load required libraries 
library(stringr)
library(Amelia)
library(dplyr)
library (data.table)
library(ggplot2)
library (car)
library(caret)
library(mlbench)
library(glmnet)
library(GGally)
library(FNN)
library(ipred)

olympicData <- read.csv("data/olympic_data.csv", header=T, na.strings=c(""), stringsAsFactors = F)

str(olympicData)
######################################################################
# Prepare the data set
######################################################################
# Change number data types to integer and numeric
olympicData$Population = as.numeric(olympicData$Population)
olympicData$GDPpC =  as.numeric(olympicData$GDPpC) 
olympicData$GoldMedals = as.integer(olympicData$GoldMedals)
olympicData$SilverMedals  =  as.integer(olympicData$SilverMedals ) 
olympicData$BronzeMedals = as.integer(olympicData$BronzeMedals)
olympicData$TotalMedals  =  as.integer(olympicData$TotalMedals ) 
olympicData$gold_pre1 = as.integer(olympicData$gold_pre1)
olympicData$gold_pre2 = as.integer(olympicData$gold_pre2)
olympicData$gold_pre3 = as.integer(olympicData$gold_pre3)
olympicData$med_pre1 = as.integer(olympicData$med_pre1)
olympicData$med_pre2 = as.integer(olympicData$med_pre2)
olympicData$med_pre3 = as.integer(olympicData$med_pre3)

# Convert all Y/N columns to logical
olympicData$Host  = ifelse(olympicData$Host == 'Y' , TRUE, FALSE)
olympicData$pre_host2  = ifelse(olympicData$pre_host2 == 'Y' , TRUE, FALSE)
olympicData$pre_host  = ifelse(olympicData$pre_host == 'Y' , TRUE, FALSE)
olympicData$post_host  = ifelse(olympicData$post_host == 'Y' , TRUE, FALSE)
olympicData$post_host2  = ifelse(olympicData$post_host2 == 'Y' , TRUE, FALSE)
# Convert them to factors
olympicData$Host  = as.factor(olympicData$Host)
olympicData$pre_host2  = as.factor(olympicData$pre_host2 )
olympicData$pre_host  = as.factor(olympicData$pre_host)
olympicData$post_host  = as.factor(olympicData$post_host)
olympicData$post_host2  = as.factor(olympicData$post_host2)

colnames(olympicData)
str(olympicData)

ggpairs(data=olympicData, columns=c("TotalMedals","Population", "GDPpC", "TotalEntries"), title="Economic variables - Correlation")


###########################################################
# Additional variables
###########################################################

# Add squares of the 2 economic variables 
#olympicData$PopulationSqr = olympicData$Population^2
#olympicData$GDPpCSqr =  olympicData$GDPpC^2 

# Add the Log of the 2 economic variables
#olympicData$PopulationLog = log10(olympicData$Population)
#olympicData$PopulationLog1 = log10(olympicData$Population + 1)
#olympicData$GDPpCLog1 = log10(olympicData$GDPpC + 1)


##############################################################
# Subsets of the main data set
#############################################################

# The OlympicALL data set will contain all years including 2020
olympicDataALL <- olympicData

# The Olympic data set will only contain the years 1992-2020 including the independent variables
olympicData <- olympicData [ olympicData$Year != 2020,  ]

# The OlympicALL data set will only contain theyears with complete data including the independent variables
olympicData2020 <- olympicDataALL [ olympicDataALL$Year == 2020,  ]

# For some algorithms, data set must not contain any NAs so suppress NAs 
olympicData = olympicData [ ! is.na (olympicData$GDPpC) & ! is.na (olympicData$Population) ,]

# Use this data set to store the predictions for each algorithm
olympicDataPredictions = olympicData [ , c("Year" , "CountryCode", "TotalMedals") ]

# For some algorithms, data set must not contain any NAs so suppress NAs 
olympicData2020 <- olympicData2020 [ ! is.na (olympicData2020$GDPpC) & ! is.na (olympicData2020$Population) ,]

# Use this data set to store the predictions for 2020 for each algorithm
olympicData2020Predictions = olympicData2020 [ , c("Year" , "CountryCode", "TotalMedals") ]


# olympicData2016 = olympicData [ Year == 2016, ]
# 
# Counts <- table (olympicData2016$TotalMedals )
# 
# barplot(Counts, main="Medal count per country",
#         xlab="Medal Count",ylab="Number of countries"
# )


######################################################################################
# modelEco1: Linear regression - Economic 1 - with all 'economic variables
#     a)  Cross validation
#####################################################################################

set.seed(41)

# Shuffle row indices: rows
rows <- sample(nrow(olympicData))
shuffled_olympicData <- olympicData[rows, ]

# Determine which row to split on: split, at 80%
split <- round(nrow(shuffled_olympicData) * 0.80)

train = shuffled_olympicData[1:split, ]

# Create test set 
test  = shuffled_olympicData[(split + 1):nrow(shuffled_olympicData), ]

# Fit the first linear model using Economic variables on training set 
modelEco1 <- lm(TotalMedals ~ GDPpC + Population + 
                pre_host2  +pre_host + Host + post_host +post_host2 +  
                TotalEntries + Awarded.Total , train)

summary (modelEco1)
# Predict medals on test set
predictdMed1 <- predict(modelEco1, test)

# Compute errors between predicted value and actual value error
error <- predictdMed1 - test[["TotalMedals"]]

# Calculate RMSE excluding NAs
RMSE = sqrt(mean(error [!is.na(error)]^2))
RMSE
# [1] 4.085927

###########################################################################################
# EcoLm1 - Linear regression - Economic 1 - with all 'economic variables
#     b)  10-fold  Cross validation 
###########################################################################################
train_control = trainControl(
  method = "cv", 
  number = 10,
  savePredictions = TRUE,
  verboseIter = TRUE)

# Fit linear model 1 using 10-fold CV
EcoLm1 <- train(
  TotalMedals ~ GDPpC + Population +  pre_host2  + pre_host + Host + post_host +post_host2 +  TotalEntries + Awarded.Total, 
  olympicData,
  method = "lm",
  trControl = train_control
)
# + Fold01: intercept=TRUE 
# - Fold01: intercept=TRUE 
# + Fold02: intercept=TRUE 
# - Fold02: intercept=TRUE 
# + Fold03: intercept=TRUE 
# - Fold03: intercept=TRUE 
# + Fold04: intercept=TRUE 
# - Fold04: intercept=TRUE 
# + Fold05: intercept=TRUE 
# - Fold05: intercept=TRUE 
# + Fold06: intercept=TRUE 
# - Fold06: intercept=TRUE 
# + Fold07: intercept=TRUE 
# - Fold07: intercept=TRUE 
# + Fold08: intercept=TRUE 
# - Fold08: intercept=TRUE 
# + Fold09: intercept=TRUE 
# - Fold09: intercept=TRUE 
# + Fold10: intercept=TRUE 
# - Fold10: intercept=TRUE 
# Aggregating results
# Fitting final model on full training set

summary(EcoLm1)

# Call:
#   lm(formula = .outcome ~ ., data = dat)
# 
# Residuals:
#   Min      1Q  Median      3Q     Max 
# -31.306  -0.819   0.798   1.526  60.734 
# 
# Coefficients:
#   Estimate Std. Error t value Pr(>|t|)    
# (Intercept)    -9.729e+00  2.575e+00  -3.778 0.000165 ***
#   GDPpC          -7.680e-06  7.851e-06  -0.978 0.328172    
# Population      1.225e-02  1.299e-03   9.434  < 2e-16 ***
#   pre_host2TRUE   8.947e+00  2.201e+00   4.065 5.08e-05 ***
#   pre_hostTRUE    1.164e+01  2.182e+00   5.333 1.14e-07 ***
#   HostTRUE        4.983e+00  2.232e+00   2.232 0.025779 *  
#   post_hostTRUE   7.155e+00  2.187e+00   3.272 0.001095 ** 
#   post_host2TRUE -1.919e-01  2.160e+00  -0.089 0.929230    
# TotalEntries    1.296e-01  2.252e-03  57.545  < 2e-16 ***
#   Awarded.Total   8.414e-03  2.813e-03   2.991 0.002835 ** 
#   ---
#   Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
# 
# Residual standard error: 5.568 on 1322 degrees of freedom
# Multiple R-squared:  0.8206,	Adjusted R-squared:  0.8194 
# F-statistic: 672.1 on 9 and 1322 DF,  p-value: < 2.2e-16

EcoLm1
# Linear Regression 
# 
# 1332 samples
# 9 predictor
# 
# No pre-processing
# Resampling: Cross-Validated (10 fold) 
# Summary of sample sizes: 1198, 1199, 1198, 1199, 1199, 1198, ... 
# Resampling results:
#   
#   RMSE     Rsquared   MAE     
# 5.76936  0.8084207  2.918942
# 
# Tuning parameter 'intercept' was held constant at a value of TRUE

# summary(model11k) - is this command relevant ?

EcoLm1$finalModel
# Call:
#   lm(formula = .outcome ~ ., data = dat)
# 
# Coefficients:
#   (Intercept)           GDPpC      Population   pre_host2TRUE    pre_hostTRUE        HostTRUE   post_hostTRUE  post_host2TRUE    TotalEntries  
# -9.729e+00      -7.680e-06       1.225e-02       8.947e+00       1.164e+01       4.983e+00       7.155e+00      -1.919e-01       1.296e-01  
# Awarded.Total  
# 8.414e-03  

# Examine model predictions for each fold.
EcoLm1$resample

# Plot importance of each feature
# 1. Open jpeg file
jpeg("Graphs/EcoLm1_feature_imp.jpg")
# 2. Create the plot
ggplot(varImp(EcoLm1))
# 3. Close the file
dev.off()


# ---------------------------------------------
# Make predictions for the whole data set
# ---------------------------------------------

# Make predictions for the whole data set
predictionsEcoLm1<- predict(EcoLm1,olympicData)

predictionsEcoLm1  <- round(predictionsEcoLm1 )
predictionsEcoLm1
# Append predictions to the Prediction data set 
olympicDataPredictions <- cbind(olympicDataPredictions,predictionsEcoLm1)

#----------------------------------------------------
# Make predictions for 2020
#----------------------------------------------------
predictions2020EcoLm1<- round(predict(EcoLm1,olympicData2020))

# Append predictions to the Prediction data set 
olympicData2020Predictions <- cbind(olympicData2020Predictions,predictions2020EcoLm1)

# Show the predictions visually
# 1. Open jpeg file
jpeg("Graphs/EcoLm1_test_predictions.jpg")
# 2. Create the plot
plot(olympicDataPredictions$TotalMedals, olympicDataPredictions$predictionsEcoLm1, xlab="y", ylab=expression(hat(y)), xlim=c(0,120),ylim=c(0,120)  ,pch=19, col="red")
title(main = "EcoLm1 Model - Predictions" , sub = "Full data set")
# 3. Close the file
dev.off()



# The predictions are unexpected , not the expcted order of magnitude: normalise them
AwardedMedals2020 = 1073
sumPredictions = sum(olympicData2020Predictions$predictions2020EcoLm1)

olympicData2020Predictions$predictions2020EcoLm1Adj = round((olympicData2020Predictions$predictions2020EcoLm1 *AwardedMedals2020) / sumPredictions)

olympicData2020 <- cbind(olympicData2020,predictions2020EcoLm1)


# Check lm gives the same summary results for EcoLm1
EconomicLM1 <- lm(TotalMedals ~ GDPpC + Population +
                    pre_host2  +pre_host + Host + post_host +post_host2 +
                    TotalEntries + Awarded.Total , olympicData)

summary(EconomicLM1)



############################################################################################
# modelEco2: Linear regression - Economic 2 - incl polynomial functions for Population and GDPpC
#     a) Cross validation  
############################################################################################

# Fit the linear model 2 using Economic variables on train: model
# Use polymnomial for GDPpC and Population

# Use same training and test set as  Economic 1

modelEco2 <- lm(TotalMedals ~ poly(GDPpC,3)  + poly(Population,3) + 
                  pre_host2  +pre_host + Host + post_host +post_host2 +  
                  TotalEntries + Awarded.Total , train)
# Call:
#   lm(formula = TotalMedals ~ poly(GDPpC, 3) + poly(Population, 
#                                                    3) + pre_host2 + pre_host + Host + post_host + post_host2 + 
#        TotalEntries + Awarded.Total, data = train)

# Residuals:
#   Min      1Q  Median      3Q     Max 
# -30.422  -1.068   0.529   1.520  49.138 
# 
# Coefficients:
#   Estimate Std. Error t value Pr(>|t|)    
# (Intercept)          -10.495542   2.636571  -3.981 7.34e-05 ***
#   poly(GDPpC, 3)1       -4.457381   5.419737  -0.822 0.411016    
# poly(GDPpC, 3)2       16.079238   5.425809   2.963 0.003110 ** 
#   poly(GDPpC, 3)3      -24.535547   5.241495  -4.681 3.23e-06 ***
#   poly(Population, 3)1  34.312438   5.584149   6.145 1.14e-09 ***
#   poly(Population, 3)2 -10.482480   5.578705  -1.879 0.060519 .  
# poly(Population, 3)3   9.574160   5.328361   1.797 0.072649 .  
# pre_host2TRUE          6.009644   2.182430   2.754 0.005995 ** 
#   pre_hostTRUE          21.984404   2.967320   7.409 2.61e-13 ***
#   HostTRUE              17.469624   2.433058   7.180 1.31e-12 ***
#   post_hostTRUE          2.227700   2.538173   0.878 0.380319    
# post_host2TRUE         2.437896   2.317652   1.052 0.293095    
# TotalEntries           0.125541   0.002912  43.112  < 2e-16 ***
#   Awarded.Total          0.009599   0.002844   3.375 0.000765 ***
#   ---
#   Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
# 
# Residual standard error: 4.991 on 1052 degrees of freedom
# Multiple R-squared:  0.8306,	Adjusted R-squared:  0.8285 
# F-statistic: 396.8 on 13 and 1052 DF,  p-value: < 2.2e-16

summary (modelEco2)

# Predict medals on test set
predictdMed2 <- predict(modelEco2, test)

# Compute errors between predicted value and actual value error
error2 <- predictdMed2- test[["TotalMedals"]]

# Calculate RMSE
# Calculate RMSE excluding NAs
RMSE = sqrt(mean(error [!is.na(error)]^2))
RMSE
#[1] 4.613185


############################################################################################
# EcoLm2: Linear regression - Economic 2 - incl polynomial functions for Population and GDPpC
#     b) 10-fold  Cross validation  
############################################################################################

# 10 fold cross validation
train_control = trainControl(
  method = "cv", 
  number = 10,
  savePredictions = TRUE,
  verboseIter = TRUE)

# Fit linear model 1 using 10-fold CV
EcoLm2 <- train(
  TotalMedals ~ poly(GDPpC,3)  + poly(Population,3) + pre_host2  +pre_host + Host + post_host + post_host2 +  TotalEntries + Awarded.Total, 
  olympicData,
  method = "lm",
  trControl = train_control
)

EcoLm2 
# Linear Regression 
# 
# 1332 samples
# 9 predictor
# 
# No pre-processing
# Resampling: Cross-Validated (10 fold) 
# Summary of sample sizes: 1199, 1199, 1198, 1198, 1198, 1199, ... 
# Resampling results:
#   
#   RMSE      Rsquared   MAE     
# 5.875125  0.8044078  2.993739
#
#Tuning parameter 'intercept' was held constant at a value of TRUE

summary(EcoLm2 )
# Call:
#   lm(formula = .outcome ~ ., data = dat)

# Residuals:
#   Min      1Q  Median      3Q     Max 
# -31.306  -0.819   0.798   1.526  60.734 
# 
# Coefficients:
#   Estimate Std. Error t value Pr(>|t|)    
# (Intercept)    -9.729e+00  2.575e+00  -3.778 0.000165 ***
#   GDPpC          -7.680e-06  7.851e-06  -0.978 0.328172    
# Population      1.225e-02  1.299e-03   9.434  < 2e-16 ***
#   pre_host2TRUE   8.947e+00  2.201e+00   4.065 5.08e-05 ***
#   pre_hostTRUE    1.164e+01  2.182e+00   5.333 1.14e-07 ***
#   HostTRUE        4.983e+00  2.232e+00   2.232 0.025779 *  
#   post_hostTRUE   7.155e+00  2.187e+00   3.272 0.001095 ** 
#   post_host2TRUE -1.919e-01  2.160e+00  -0.089 0.929230    
# TotalEntries    1.296e-01  2.252e-03  57.545  < 2e-16 ***
#   Awarded.Total   8.414e-03  2.813e-03   2.991 0.002835 ** 
#   ---
#   Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
# 
# Residual standard error: 5.568 on 1322 degrees of freedom
# Multiple R-squared:  0.8206,	Adjusted R-squared:  0.8194 
# F-statistic: 672.1 on 9 and 1322 DF,  p-value: < 2.2e-16

EcoLm2$finalModel
# Call:
#   lm(formula = .outcome ~ ., data = dat)
# 
# Coefficients:
#   (Intercept)       `poly(GDPpC, 3)1`       `poly(GDPpC, 3)2`       `poly(GDPpC, 3)3`  `poly(Population, 3)1`  
# -10.275438               -6.965931               22.533765              -31.906512               54.471740  
# `poly(Population, 3)2`  `poly(Population, 3)3`           pre_host2TRUE            pre_hostTRUE                HostTRUE  
# -9.728898               17.799363                8.276997               11.213556                4.402390  
# post_hostTRUE          post_host2TRUE            TotalEntries           Awarded.Total  
# 6.940110               -0.204088                0.132409                0.009184 

# examine model predictions for each fold.
EcoLm2$resample

# Check lm givs the same results
EconomicLM2 <- lm(TotalMedals ~ GDPpC + Population +
                    pre_host2  +pre_host + Host + post_host +post_host2 +
                    TotalEntries + Awarded.Total , olympicData)

summary(EconomicLM2)

# Plot importance of each feature
# 1. Open jpeg file
jpeg("Graphs/EcoLm2_feature_imp.jpg")
# 2. Create the plot
ggplot(varImp(EcoLm2))
# 3. Close the file
dev.off()

# ---------------------------------------------
# Make predictions for the whole data set
# ---------------------------------------------

# Make predictions for the whole data set
predictionsEcoLm2<- predict(EcoLm2,olympicData)
predictionsEcoLm2

predictionsEcoLm2  <- round(predictionsEcoLm2 )
predictionsEcoLm2
# Append predictions to the Prediction data set 
olympicDataPredictions <- cbind(olympicDataPredictions,predictionsEcoLm2)

#----------------------------------------------------
# Make predictions for 2020
#----------------------------------------------------
predictions2020EcoLm2<- predict(EcoLm2,olympicData2020)

predictions2020EcoLm2 <- round(predictions2020EcoLm2)

# Append predictions to the Prediction data set 
olympicData2020Predictions <- cbind(olympicData2020Predictions,predictions2020EcoLm2)


# Show the predictions visually
# 1. Open jpeg file
jpeg("Graphs/EcoLm2_test_predictions.jpg")
# 2. Create the plot
plot(olympicDataPredictions$TotalMedals, olympicDataPredictions$predictionsEcoLm2, xlab="y", ylab=expression(hat(y)), xlim=c(0,120),ylim=c(0,120)  ,pch=19, col="red")
title(main = "EcoLm2 Model - Predictions" , sub = "Full data set")
# 3. Close the file
dev.off()

# The predictions are unexpected , not the expcted order of magnitude: normalise them
AwardedMedals2020 = 1073
sumPredictions = sum(olympicData2020Predictions$predictions2020EcoLm2)

olympicData2020Predictions$predictions2020EcoLm2Adj = round((olympicData2020Predictions$predictions2020EcoLm2 *AwardedMedals2020) / sumPredictions)

# Add also to the Olympisc 2020 data set
olympicData2020 <- cbind(olympicData2020,predictions2020EcoLm2)


# Check lm gives the same summary results for EcoLm1
EconomicLM1 <- lm(TotalMedals ~ GDPpC + Population +
                    pre_host2  +pre_host + Host + post_host +post_host2 +
                    TotalEntries + Awarded.Total , olympicData)

summary(EconomicLM1)

# Sum the number of medals
sum(predictions2020EcoLm2)
#[1] 191640260609

sum(olympicData2020Predictions$predictions2020EcoLm2)



###########################################################################################
# EcoRf1 - Random Forest- Economic 1 - with all 'economic variables incl polynomial functions
# for Population and GDPpC
# Use tuneLength = 1
###########################################################################################


# Fit random forest
EcoRf1 <- train(
  TotalMedals ~ poly(GDPpC,3) + poly(Population,3) + pre_host2  +pre_host + Host + post_host +post_host2 + TotalEntries + Awarded.Total,
  data = olympicData, 
  method = "ranger",
  tuneLength = 1
)

EcoRf1

#Random Forest 
#
# 1332 samples
# 9 predictor
# 
# No pre-processing
# Resampling: Bootstrapped (25 reps) 
# Summary of sample sizes: 1332, 1332, 1332, 1332, 1332, 1332, ... 
# Resampling results across tuning parameters:
#   
#   splitrule   RMSE      Rsquared   MAE     
# variance    4.668177  0.8890890  1.943031
# extratrees  4.792668  0.8903981  2.009976
# 
# Tuning parameter 'mtry' was held constant at a value of 3
# Tuning parameter 'min.node.size' was held constant at a value of 5
# RMSE was used to select the optimal model using the smallest value.
# The final values used for the model were mtry = 3, splitrule = variance and min.node.size = 5.

EcoRf1$finalModel
# Ranger result
# 
# Call:
#   ranger::ranger(dependent.variable.name = ".outcome", data = x,      mtry = min(param$mtry, ncol(x)), min.node.size = param$min.node.size,      splitrule = as.character(param$splitrule), write.forest = TRUE,      probability = classProbs, ...) 
# 
# Type:                             Regression 
# Number of trees:                  500 
# Sample size:                      1332 
# Number of independent variables:  13 
# Mtry:                             3 
# Target node size:                 5 
# Variable importance mode:         none 
# Splitrule:                        variance 
# OOB prediction error (MSE):       17.88854 
# R squared (OOB):                  0.8958193 
# examine model predictions for each fold.
# EcoRf1$resample

# ---------------------------------------------
# Make predictions for the whole data set
# ---------------------------------------------

predictionsEcoRf1<- predict(EcoRf1,olympicData)

predictionsEcoRf1 <- round(predictionsEcoRf1)

# Append predictions to the Prediction data set 
olympicDataPredictions <- cbind(olympicDataPredictions,predictionsEcoRf1)

#----------------------------------------------------
# Make predictions for 2020
#----------------------------------------------------
predictions2020EcoRf1<- predict(EcoRf1,olympicData2020)

predictions2020EcoRf1 <- round(predictions2020EcoRf1)
predictions2020EcoRf1

# Append predictions to the Prediction data set 
olympicData2020Predictions <- cbind(olympicData2020Predictions,predictions2020EcoRf1)

# Sum the number of medals
sum(predictions2020EcoRf1)

sum(olympicData2020Predictions$predictions2020EcoRf1)

# Show the predictions visually
# 1. Open jpeg file
jpeg("Graphs/EcoRf1_test_predictions.jpg")
# 2. Create the plot
plot(olympicDataPredictions$TotalMedals, olympicDataPredictions$predictionsEcoRf1, xlab="y", ylab=expression(hat(y)), xlim=c(0,120),ylim=c(0,120)  ,pch=19, col="red")
title(main = "EcoRf1 Model - Predictions" , sub = "Full data set")
# 3. Close the file
dev.off()

###########################################################################################
# EcoRf2 - Random Forest- Economic 2 - with all 'economic variables incl polynomial functions
# for Population and GDPpC
# Use tuneLength = 10
###########################################################################################

# Fit random forest
EcoRf2 <- train(
    TotalMedals ~ poly(GDPpC,3) + poly(Population,3) + pre_host2  +pre_host + Host + post_host +post_host2 + TotalEntries + Awarded.Total,
    data = olympicData, 
    method = "ranger",
    tuneLength = 10
  )

#summary(EcoRf2) - not applicable here

# Use the mean of the R2 /RMSE for the 25 samples
mean(EcoRf2$resample$Rsquared)
mean(EcoRf2$resample$RMSE)
mean(EcoRf2$resample$MAE)

EcoRf2$finalModel
# Ranger result
# 
# Call:
#   ranger::ranger(dependent.variable.name = ".outcome", data = x,      mtry = min(param$mtry, ncol(x)), min.node.size = param$min.node.size,      splitrule = as.character(param$splitrule), write.forest = TRUE,      probability = classProbs, ...) 
# 
# Type:                             Regression 
# Number of trees:                  500 
# Sample size:                      1332 
# Number of independent variables:  13 
# Mtry:                             13 
# Target node size:                 5 
# Variable importance mode:         none 
# Splitrule:                        extratrees 
# Number of random splits:          1 
# OOB prediction error (MSE):       12.24847 
# R squared (OOB):                  0.9286664


# Plot importance of each feature
# 1. Open jpeg file
#jpeg("Graphs/EcoRf2_feature_imp.jpg")
# 2. Create the plot
#ggplot(varImp(EcoRf2))
# 3. Close the file
#dev.off()
# ---------------------------------------------
# Make predictions for the whole data set
# ---------------------------------------------

predictionsEcoRf2<- predict(EcoRf2,olympicData)

predictionsEcoRf2 <- round(predictionsEcoRf2)

# Append predictions to the Prediction data set 
olympicDataPredictions <- cbind(olympicDataPredictions,predictionsEcoRf2)

#----------------------------------------------------
# Make predictions for 2020
#----------------------------------------------------
predictions2020EcoRf2<- predict(EcoRf2,olympicData2020)

predictions2020EcoRf2 <- round(predictions2020EcoRf2)
predictions2020EcoRf2
#predictions2020EcoRf2 = as.data.frame (predictions2020EcoRf2)

# Append predictions to the Prediction data set 
olympicData2020Predictions <- cbind(olympicData2020Predictions,predictions2020EcoRf2)

# Sum the number of medals
sum(predictions2020EcoRf2)

# this looks high
sum(olympicData2020Predictions$predictions2020EcoRf2)

# Show the predictions visually
# 1. Open jpeg file
jpeg("Graphs/EcoRf2_test_predictions.jpg")
# 2. Create the plot
plot(olympicDataPredictions$TotalMedals, olympicDataPredictions$predictionsEcoRf2, xlab="y", ylab=expression(hat(y)), xlim=c(0,120),ylim=c(0,120)  ,pch=19, col="red")
title(main = "EcoRf2 Model - Predictions" , sub = "Full data set")
# 3. Close the file
dev.off()


##########################################################
# EcokNN -  kNN regressions
#  All results variables
#########################################################
# https://quantdev.ssri.psu.edu/sites/qdev/files/kNN_tutorial.html

#--------------------------------------------------------------------------------
# Preparation of the data for kNN
# Only numeric variables can be used and they must be scaled
#--------------------------------------------------------------------------------

# Take a copy of the data set
olympicData2 = olympicData

# Put TotalMedals (outcome variable) in its own object
TotalMedals_outcome <- olympicData2 %>% select(TotalMedals)

Non_predictors <-  olympicData2 [, c("Year", "HostCity", "HostCountry", "HostCountryCode")]
# remove original from the data set

olympicData2 <- select (olympicData2,-c(TotalMedals,Year, HostCity, HostCountry, HostCountryCode, CountryCode,
                                        gold_pre1, med_pre1, gold_pre2, med_pre2, gold_pre3, med_pre3))

# Scale the numeric variables
olympicData2[, c("Population", "GDPpC","Awarded.Gold", "Awarded.Silver", "Awarded.Bronze" , "Awarded.Total",
                  "TotalEntries", "GoldMedals",  "SilverMedals", "BronzeMedals" )]  <- scale(olympicData2[, c(
                    "Population", "GDPpC","Awarded.Gold", "Awarded.Silver", "Awarded.Bronze" , "Awarded.Total",
                   "TotalEntries","GoldMedals",  "SilverMedals", "BronzeMedals") ])

# Second, we need to dummy code any factor or categorical variables.
# Examine the structure of the data to determine which variables need to be dummy coded.

# We now dummy code variables that have just two levels and are coded 1/0.
olympicData2$pre_host2 <- ifelse(olympicData2$pre_host2 == TRUE, 1, 0)
olympicData2$pre_host <- ifelse(olympicData2$pre_host == TRUE, 1, 0)
olympicData2$Host <- ifelse(olympicData2$Host == TRUE, 1, 0)
olympicData2$post_host <- ifelse(olympicData2$post_host == TRUE, 1, 0)
olympicData2$post_host2 <- ifelse(olympicData2$post_host2 == TRUE, 1, 0)

str(olympicData2)

head(olympicData2)

#We split the data into training and test sets.
# We partition 75% of the data into the training set and the remaining 25% into the test set.

set.seed(1234) # set the seed to make the partition reproducible

# 75% of the sample size
smp_size <- floor(0.75 * nrow(olympicData2))

train_ind <- sample(seq_len(nrow(olympicData2)), size = smp_size)

# creating test and training sets that contain all of the predictors
reg_pred_train <- olympicData2[train_ind, ]
reg_pred_test <- olympicData2[-train_ind, ]
# Split outcome variable into training and test sets using the same partition as above.
medals_outcome_train <- TotalMedals_outcome[train_ind, ]
medals_outcome_test <- TotalMedals_outcome[-train_ind, ]

Non_predictors_train <- Non_predictors[train_ind, ]
Non_predictors_test <- Non_predictors[-train_ind, ]

#-----------------------------------------------------------------
# EcoKNN1 - kNN regression - k=33
#-----------------------------------------------------------------

# Decide on the number of neighbors (k).
#There are several rules of thumb, one being the square root of the number of observations in the training set.
# In this case, we select 33 as the number of neighbors, which is approximately the square root of our sample size N = 1070.
knn_reg_results <- knn.reg(reg_pred_train, reg_pred_test, medals_outcome_train, k = 33)
print(knn_reg_results)

# Predictions on the test set
knn_reg_results$pred

# Store predictors independent variable
predictions_test = Non_predictors_test
predictions_test = cbind(predictions_test , medals_outcome_test)
predictions_test = cbind(predictions_test, knn_reg_results$pred)

# Show the predictions visually
# 1. Open jpeg file
jpeg("Graphs/EcokNN1_test_predictions.jpg")
# 2. Create the plot
plot(medals_outcome_test, knn_reg_results$pred, xlab="y", ylab=expression(hat(y)), xlim=c(0,120),ylim=c(0,120)  ,pch=19, col="red")
title(main = "EcokNN1 Model - Predictions (k=33) " , sub = "Training set")
# 3. Close the file
dev.off()


#mean square prediction error
mean((medals_outcome_test- knn_reg_results$pred) ^ 2)
#[1] 7.439263

# Root mean square prediction error RMSE
sqrt( mean((medals_outcome_test- knn_reg_results$pred) ^ 2))
#[1] 2.727501

#mean absolute error
mean(abs(medals_outcome_test - knn_reg_results$pred))
#[1] 1.558955

#-----------------------------------------------------------------
# EcokNN2 - kNN regression - k=3
#-----------------------------------------------------------------

# Decide on the number of neighbors (k).
#There are several rules of thumb, one being the square root of the number of observations in the training set.
# In this case, we select 33 as the number of neighbors, which is approximately the square root of our sample size N = 1070.
knn_reg_results2 <- knn.reg(reg_pred_train, reg_pred_test, medals_outcome_train, k = 3)
print(knn_reg_results2)

# Predictions on the test set
knn_reg_results2$pred

# Store predictors independent variable
predictions_test = cbind(predictions_test, knn_reg_results2$pred)

# Show the predictions visually
# 1. Open jpeg file
jpeg("Graphs/EcokNN2_test_predictions.jpg")
# 2. Create the plot
plot(medals_outcome_test, knn_reg_results2$pred, xlab="y", ylab=expression(hat(y)), xlim=c(0,120),ylim=c(0,120)  ,pch=19, col="red")
title(main = "EcokNN2 Model - Predictions (k=3) " , sub = "Training set")
# 3. Close the file
dev.off()

#mean square prediction error
mean((medals_outcome_test- knn_reg_results2$pred) ^ 2)

# Root mean square prediction error RMSE
sqrt( mean((medals_outcome_test- knn_reg_results2$pred) ^ 2))
#[1] 3.413708

#mean absolute error
mean(abs(medals_outcome_test - knn_reg_results2$pred))
#[1] 1.199005

#-----------------------------------------------------------------
# EcokNN3 - kNN regression - k=15
#-----------------------------------------------------------------

# Decide on the number of neighbors (k).
#There are several rules of thumb, one being the square root of the number of observations in the training set.
# In this case, we select 33 as the number of neighbors, which is approximately the square root of our sample size N = 1070.
knn_reg_results3 <- knn.reg(reg_pred_train, reg_pred_test, medals_outcome_train, k = 15)
print(knn_reg_results3)

# Predictions on the test set
knn_reg_results3$pred

# Store predictors independent variable
predictions_test = cbind(predictions_test, knn_reg_results3$pred)

# Show if the prediction is good , if so it should be near y=x line


# Show the predictions visually
# 1. Open jpeg file
jpeg("Graphs/EcokNN3_test_predictions.jpg")
# 2. Create the plot
plot(medals_outcome_test, knn_reg_results3$pred, xlab="y", ylab=expression(hat(y)), xlim=c(0,120),ylim=c(0,120)  ,pch=19, col="red")
title(main = "EcokNN3 Model - Predictions (k=15) " , sub = "Training set")
# 3. Close the file
dev.off()

#mean square prediction error
mean((medals_outcome_test- knn_reg_results3$pred) ^ 2)
#[1] 7.439263

# Root mean square prediction error RMSE
sqrt( mean((medals_outcome_test- knn_reg_results3$pred) ^ 2))

sqrt( mean((medals_outcome_test- knn_reg_results3$pred) ^ 2))
# [1] 4.903387

#mean absolute error
mean(abs(medals_outcome_test - knn_reg_results3$pred))
#[1] 1.558955

##################################################################
# Write the 2020 predictions for Data set 2 - previous results
# Save as csv for further use
##################################################################

write.csv(olympicData2020Predictions,"Graphs/Eco_predictions.csv", row.names = FALSE, quote=FALSE) 