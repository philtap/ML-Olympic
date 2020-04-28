#--------------------------------------------------------------------------------------------------
#   
#       DMML project  
#       Script    : DMML_ML_methods_Res.R (Previous results)
#       Author    : Philippe Tap
#       Date      : April 2020
#
#--------------------------------------------------------------------------------------------------
# This script apply ML methods to the second data set, based on general and previous results factors
#  - previous results : total medals and gold medals at the last 3 Olympics
#        gold_pre1 , med_pre1, gold_pre2, med_pre2, gold_pre3,med_pre3,  
#  - general factors: 
#        - Was the country host before or after the current games pre_host2, pre_host,  Host, post_host,post_host2
#        - Number of events entered by the country: TotalEntries
#        - Awarded.Total: total medal awarded at the current games
#

# The following models are applied
# ResLM1	  all prev results
# ResLM2   without Gold results
# ResRF1	  all results, tuning=10
# ReskNN1	all results, k=33
# ReskNN2	all results, k=3
# ReskNN3	all results, k=15
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

# Read the main data file
olympicData <- read.csv("data/olympic_data.csv", header=T, na.strings=c(""), stringsAsFactors = F)

str(olympicData)

######################################################################
# Prepare data set 1: the Economic data set 
######################################################################

# Change data types to integer and numeric, where applicable
# Ignore warning messages:
# Warning message:
# NAs introduced by coercion 

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


# Keep only the necessary columns and re-order
col_order <- c("Year", "CountryCode", 
               "HostCity"   ,     "HostCountry"   ,  "HostCountryCode" , 
               "Awarded.Gold" ,  "Awarded.Silver" , "Awarded.Bronze" , "Awarded.Total",
               "pre_host2" ,  "pre_host"  , "Host" , "post_host" ,  "post_host2"  ,     
               "gold_pre1" ,  "med_pre1" ,  "gold_pre2",  "med_pre2" , "gold_pre3" , "med_pre3" ,
               "TotalEntries" ,"GoldMedals"  , "SilverMedals", "BronzeMedals" ,"TotalMedals") 

olympicData  <- as.data.frame(olympicData ) [, col_order]

###############################################################################################
#  Missing values , Data cleanup
###############################################################################################

# Show missing values vs observed 
missmap(olympicData, main = "Missing values vs observed")

# Show missing values per column
sapply(olympicData,function(x) sum(is.na(x)))

attach(olympicData)
olympicData_na = olympicData [is.na(gold_pre1) |  is.na(med_pre1) | is.na(gold_pre2 ) |  is.na( med_pre2 ) |  is.na( gold_pre3 )| is.na( med_pre3) ,]

olympicData_na_counts = table(olympicData_na$Year)
olympicData_na_counts
barplot(olympicData_na_counts, main = "NA per year", xlab = "Year" , ylab = "Count", ylim=c(0,120))


##############################################################
# Subsets of the main data set
#############################################################


# The OlympicALL data set will contain all years including 2020
olympicDataALL <- olympicData

# The Olympic data set will only contain the years 1992-2020 including the independent variables
olympicData <- olympicData [ olympicData$Year != 2020,  ]

# The OlympicALL data set will only contain the years with complete data including the independent variables
olympicData2020 <- olympicDataALL [ olympicDataALL$Year == 2020,  ]

# For some algorithms, data set must not contain any NAs so suppress rows with NAs in results columns
olympicData = olympicData [ ! is.na(olympicData$gold_pre1) & !is.na(olympicData$med_pre1) & 
                              !is.na(olympicData$gold_pre2 ) & !is.na(olympicData$med_pre2 ) &
                              !is.na( olympicData$gold_pre3 ) & !is.na( olympicData$med_pre3),]

# Show Missing values vs observed after  
missmap(olympicData, main = "Missing values vs observed")

# missing values
sapply(olympicData,function(x) sum(is.na(x)))

# Use a predictions data set to store the predictions for each algorithm
olympicDataPredictions = olympicData [ , c("Year" , "CountryCode", "TotalMedals") ]

# For some algorithms, data set must not contain any NAs so suppress NAs 
olympicData2020 <- olympicData2020 [ ! is.na(olympicData2020$gold_pre1) & !is.na(olympicData2020$med_pre1) &
                                       !is.na(olympicData2020$gold_pre2 ) & !is.na(olympicData2020$med_pre2 ) 
                                     & !is.na( olympicData2020$gold_pre3 ) & !is.na( olympicData2020$med_pre3),]


# Use this data set to store the predictions for 2020 for each algorithm
olympicData2020Predictions = olympicData2020 [ , c("Year" , "CountryCode", "TotalMedals") ]

#############################
# Correlation
#############################

# Show correlation between the Total medals results 

# Plot importance of each feature
# 1. Open jpeg file
jpeg("Graphs/Res_variables_correlation.jpg")
# 2. Create the plot
ggpairs(data=olympicData, columns=c("TotalMedals","med_pre1", "med_pre2", "med_pre3"), title="Previous results variables - Correlation")
# 3. Close the file
dev.off()



###########################################################################################
# ResLm1 - Linear regression - Results 1 - with all results variables
#    10-fold  Cross validation 
###########################################################################################

str(olympicData)

train_control = trainControl(
  method = "cv", 
  number = 10,
  savePredictions = TRUE,
  verboseIter = TRUE)

# Fit linear model 1 using 10-fold CV
ResLm1 <- train(
  TotalMedals ~ gold_pre1 + med_pre1 + gold_pre2 + med_pre2 + gold_pre3 + med_pre3 +
                pre_host2+ pre_host + Host + post_host + post_host2 + TotalEntries + Awarded.Total, 
  olympicData,
  method = "lm",
  trControl = train_control
)

ResLm1

# Linear Regression 
# 
# 1092 samples
# 13 predictor
# 
# No pre-processing
# Resampling: Cross-Validated (10 fold) 
# Summary of sample sizes: 982, 983, 983, 982, 983, 983, ... 
# Resampling results:
#   
#   RMSE     Rsquared   MAE     
# 2.82224  0.9481437  1.270014
# 
# Tuning parameter 'intercept' was held constant at a value of TRUE

summary(ResLm1)

# Residuals:
#   Min       1Q   Median       3Q      Max 
# -15.0172  -0.2730   0.0977   0.1966  18.7720 
# 
# Coefficients:
#   Estimate Std. Error t value Pr(>|t|)    
# (Intercept)    -0.0796304  1.5289912  -0.052   0.9585    
# gold_pre1       0.8261405  0.0727445  11.357  < 2e-16 ***
#   med_pre1        0.4653837  0.0398346  11.683  < 2e-16 ***
#   gold_pre2      -0.0445385  0.0792178  -0.562   0.5741    
# med_pre2        0.0966444  0.0390866   2.473   0.0136 *  
#   gold_pre3      -0.1149719  0.0706945  -1.626   0.1042    
# med_pre3        0.0410824  0.0344154   1.194   0.2328    
# pre_host2TRUE  -5.1687354  1.1973100  -4.317 1.73e-05 ***
#   pre_hostTRUE   -7.4200999  1.2651401  -5.865 5.97e-09 ***
#   HostTRUE       11.2159344  1.1146479  10.062  < 2e-16 ***
#   post_hostTRUE   7.1974048  1.1033598   6.523 1.06e-10 ***
#   post_host2TRUE  5.6192074  1.0164567   5.528 4.06e-08 ***
#   TotalEntries    0.0247138  0.0024707  10.003  < 2e-16 ***
#   Awarded.Total  -0.0001795  0.0016368  -0.110   0.9127    
# ---
#   Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
# 
# Residual standard error: 2.542 on 1078 degrees of freedom
# Multiple R-squared:  0.9605,	Adjusted R-squared:   0.96 
# F-statistic:  2015 on 13 and 1078 DF,  p-value: < 2.2e-16



ResLm1$finalModel
# Call:
#   lm(formula = .outcome ~ ., data = dat)
# 
# Coefficients:
#   (Intercept)       gold_pre1        med_pre1       gold_pre2        med_pre2       gold_pre3        med_pre3   pre_host2TRUE  
# -0.0796304       0.8261405       0.4653837      -0.0445385       0.0966444      -0.1149719       0.0410824      -5.1687354  
# pre_hostTRUE        HostTRUE   post_hostTRUE  post_host2TRUE    TotalEntries   Awarded.Total  
# -7.4200999      11.2159344       7.1974048       5.6192074       0.0247138      -0.0001795  



# examine model predictions for each fold.
ResLm1$resample
# Plot importance of each feature
# 1. Open jpeg file
jpeg("Graphs/ResLM1_feature_imp.jpg")
# 2. Create the plot
ggplot(varImp(ResLm1))
# 3. Close the file
dev.off()



# ---------------------------------------------
# Make predictions for the whole data set
# ---------------------------------------------

# This works 
predictionsResLm1<- predict(ResLm1,olympicData)

predictionsResLm1  <- round(predictionsResLm1 )

# Append predictions to the Prediction data set 
olympicDataPredictions <- cbind(olympicDataPredictions,predictionsResLm1)

# Show the predictions visually
# 1. Open jpeg file
jpeg("Graphs/ResLM1_test_predictions.jpg")
# 2. Create the plot
plot(olympicDataPredictions$TotalMedals, olympicDataPredictions$predictionsResLm1, xlab="y", ylab=expression(hat(y)), xlim=c(0,120),ylim=c(0,120)  ,pch=19, col="red")
title(main = "ResLm1 Model - Predictions" , sub = "Full data set")
# 3. Close the file
dev.off()

# ---------------------------------------------
# Make predictions for 2020
# ---------------------------------------------

predictions2020ResLm1<- predict(ResLm1,olympicData2020)

predictions2020ResLm1  <- round(predictions2020ResLm1 )
predictions2020ResLm1 
# Append predictions to the Prediction data set 
olympicData2020Predictions <- cbind(olympicData2020Predictions,predictions2020ResLm1)

sum(olympicData2020Predictions$predictions2020ResLm1)
# [1] 985

#------------------------------------------------------------------------------------------------------
# Use lm to show the results are the same 
ResultLM1 <- lm( TotalMedals ~ gold_pre1 + med_pre1 + gold_pre2 + med_pre2 + gold_pre3 + med_pre3 
                 +pre_host2+ pre_host + Host + post_host + post_host2 + TotalEntries + Awarded.Total, 
                olympicData)

summary(ResultLM1)
#------------------------------------------------------------------------------------------------------

###########################################################################################
# ResLm2 - Linear regression - Results 2 - without the Gold results variables
#   10-fold  Cross validation 
###########################################################################################
train_control = trainControl(
  method = "cv", 
  number = 10,
  savePredictions = TRUE,
  verboseIter = TRUE)

# Fit linear model 2 using 10-fold CV (withoutprevious results for gold medals)
ResLm2 <- train(
  TotalMedals ~  med_pre1 + med_pre2  + med_pre3 + pre_host2+ pre_host + Host + post_host + post_host2 + TotalEntries + Awarded.Total, 
  olympicData,
  method = "lm",
  trControl = train_control
)

ResLm2

# Linear Regression 
# 
# 1092 samples
# 10 predictor
# 
# No pre-processing
# Resampling: Cross-Validated (10 fold) 
# Summary of sample sizes: 983, 983, 982, 982, 983, 983, ... 
# Resampling results:
#   
#   RMSE   Rsquared   MAE     
# 2.846  0.9427945  1.255514
# 
# Tuning parameter 'intercept' was held constant at a value of TRUE

summary(ResLm2)

# Residuals:
#   Min       1Q   Median       3Q      Max 
# -18.6859  -0.1669   0.0878   0.1692  26.0541 
# 
#   Coefficients:
#   Estimate Std. Error t value Pr(>|t|)    
#   (Intercept)     0.2084716  1.6180866   0.129 0.897510    
#   med_pre1        0.7862125  0.0298989  26.296  < 2e-16 ***
#   med_pre2        0.1103881  0.0308288   3.581 0.000358 ***
#   med_pre3       -0.0253905  0.0200416  -1.267 0.205468    
#   pre_host2TRUE  -3.8810193  1.2028100  -3.227 0.001290 ** 
#   pre_hostTRUE   -3.2116205  1.2783946  -2.512 0.012142 *  
#   HostTRUE       12.5959539  1.1281355  11.165  < 2e-16 ***
#   post_hostTRUE   7.8093835  1.1606573   6.728 2.78e-11 ***
#   post_host2TRUE  6.5619328  1.0644697   6.165 9.97e-10 ***
#   TotalEntries    0.0171370  0.0022434   7.639 4.81e-14 ***
#   Awarded.Total  -0.0004305  0.0017326  -0.248 0.803820    
# ---
#   Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
# 
# Residual standard error: 2.695 on 1081 degrees of freedom
# Multiple R-squared:  0.9554,	Adjusted R-squared:  0.955 
# F-statistic:  2318 on 10 and 1081 DF,  p-value: < 2.2e-16
#   Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
# 
# Residual standard error: 2.542 on 1078 degrees of freedom
# Multiple R-squared:  0.9605,	Adjusted R-squared:   0.96 
# F-statistic:  2015 on 13 and 1078 DF,  p-value: < 2.2e-16



ResLm2$finalModel
# Call:
#   lm(formula = .outcome ~ ., data = dat)
# 
# Coefficients:
#   (Intercept)       gold_pre1        med_pre1       gold_pre2        med_pre2       gold_pre3        med_pre3   pre_host2TRUE  
# -0.0796304       0.8261405       0.4653837      -0.0445385       0.0966444      -0.1149719       0.0410824      -5.1687354  
# pre_hostTRUE        HostTRUE   post_hostTRUE  post_host2TRUE    TotalEntries   Awarded.Total  
# -7.4200999      11.2159344       7.1974048       5.6192074       0.0247138      -0.0001795  



# examine model predictions for each fold.
ResLm2$resample

# 1. Open jpeg file
jpeg("Graphs/ResLM2_feature_imp.jpg")
# 2. Create the plot
ggplot(varImp(ResLm2))
# 3. Close the file
dev.off()
# ---------------------------------------------
# Make predictions for the whole data set
# ---------------------------------------------

# This works 
predictionsResLm2<- predict(ResLm2,olympicData)

predictionsResLm2  <- round(predictionsResLm2 )

# Append predictions to the Prediction data set 
olympicDataPredictions <- cbind(olympicDataPredictions,predictionsResLm2)

# Show the predictions visually
# 1. Open jpeg file
jpeg("Graphs/ResLM2_test_predictions.jpg")
# 2. Create the plot
plot(olympicDataPredictions$TotalMedals, olympicDataPredictions$predictionsResLm2, xlab="y", ylab=expression(hat(y)), xlim=c(0,120),ylim=c(0,120)  ,pch=19, col="red")
title(main = "ResLm2 Model - Predictions" , sub = "Full data set")
# 3. Close the file
dev.off()




# ---------------------------------------------
# Make predictions for 2020
# ---------------------------------------------

predictions2020ResLm2<- predict(ResLm2,olympicData2020)

predictions2020ResLm2  <- round(predictions2020ResLm2 )
predictions2020ResLm2 
# Append predictions to the Prediction data set 
olympicData2020Predictions <- cbind(olympicData2020Predictions,predictions2020ResLm2)

sum(olympicData2020Predictions$predictions2020ResLm2)
# [1] 985


##################################################
#Important - Calculate the VIF 
##################################################
# Use lm to get some details
Lm2Res <- lm( TotalMedals ~ med_pre1 + med_pre2  + med_pre3 + pre_host2+ 
                pre_host + Host + post_host + post_host2 + 
                TotalEntries + Awarded.Total, olympicData)

summary(Lm2Res)
vif (Lm2Res)

# med_pre1      med_pre2      med_pre3     pre_host2      pre_host          Host 
# 21.010402     21.561883      9.678134      1.188812      1.342933      1.219359 
# post_host    post_host2  TotalEntries Awarded.Total 
# 1.107017      1.085261      4.982175      1.025308 

# Due to high colinearity, for med_pre1 and med_pre2,  remove med_pre2 as it is less significant 
Lm3Res <- lm( TotalMedals ~ med_pre1 + med_pre3 + pre_host2+ 
                pre_host + Host + post_host + post_host2 + 
                TotalEntries + Awarded.Total, olympicData)
summary(Lm3Res)
vif (Lm3Res)
# med_pre1      med_pre3     pre_host2      pre_host          Host     post_host 
# 11.754025      8.241763      1.102976      1.314131      1.157799      1.069841 
# post_host2  TotalEntries Awarded.Total 
# 1.074347      4.788862      1.025091 

# Similar performance , due to a high p value for its coefficient, remove med_pre3
vif (Lm3Res)

Lm4Res <- lm( TotalMedals ~ med_pre1+ pre_host2+ 
                pre_host + Host + post_host + post_host2 + 
                TotalEntries + Awarded.Total, olympicData)
summary(Lm4Res)
vif (Lm4Res)

# med_pre1     pre_host2      pre_host          Host     post_host    post_host2 
# 4.690098      1.092482      1.166231      1.154384      1.056100      1.058018 
# TotalEntries Awarded.Total 
# 4.733039      1.022333 

###########################################################################################
# ResLm3 - Linear regression - Results 3 - with a subset of results variables
#    10-fold  Cross validation 
###########################################################################################

str(olympicData)

train_control = trainControl(
  method = "cv", 
  number = 10,
  savePredictions = TRUE,
  verboseIter = TRUE)

# Fit linear model 1 using 10-fold CV
ResLm3 <- train( 
  TotalMedals ~ med_pre1+ pre_host2 + 
                pre_host + Host + post_host + post_host2 + 
                TotalEntries + Awarded.Total, 
  olympicData,
  method = "lm",
  trControl = train_control
)

ResLm3
# Linear Regression 
# 
# 1092 samples
# 8 predictor
# 
# No pre-processing
# Resampling: Cross-Validated (10 fold) 
# Summary of sample sizes: 982, 983, 983, 983, 983, 983, ... 
# Resampling results:
#   
#   RMSE      Rsquared   MAE     
# 2.768193  0.9539326  1.247873

summary(ResLm3)
# 
# Residuals:
#   Min       1Q   Median       3Q      Max 
# -19.4504  -0.1598   0.1026   0.1776  26.8664 
# 
# Coefficients:
#   Estimate Std. Error t value Pr(>|t|)    
# (Intercept)     0.1188229  1.6240757   0.073  0.94169    
# med_pre1        0.8594021  0.0141881  60.572  < 2e-16 ***
#   pre_host2TRUE  -2.7375991  1.1588179  -2.362  0.01833 *  
#   pre_hostTRUE   -3.9337266  1.1972916  -3.286  0.00105 ** 
#   HostTRUE       11.6817551  1.1031985  10.589  < 2e-16 ***
#   post_hostTRUE   7.0325089  1.1393402   6.172 9.49e-10 ***
#   post_host2TRUE  6.9284029  1.0562761   6.559 8.36e-11 ***
#   TotalEntries    0.0187447  0.0021977   8.529  < 2e-16 ***
#   Awarded.Total  -0.0003602  0.0017388  -0.207  0.83595    
# ---
#   Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
# 
# Residual standard error: 2.708 on 1083 degrees of freedom
# Multiple R-squared:  0.9549,	Adjusted R-squared:  0.9546 
# F-statistic:  2867 on 8 and 1083 DF,  p-value: < 2.2e-16


# examine model predictions for each fold.
ResLm3$resample

# Plot importance of each feature
# 1. Open a jpeg file
jpeg("Graphs/ResLm3_feature_imp.jpg")
# 2. Create the plot
ggplot(varImp(ResLm3))
# 3. Close the file
dev.off()


# ---------------------------------------------
# Make predictions for the whole data set
# ---------------------------------------------

# This works 
predictionsResLm3<- predict(ResLm3,olympicData)

predictionsResLm3  <- round(predictionsResLm3 )

# Append predictions to the Prediction data set 
olympicDataPredictions <- cbind(olympicDataPredictions,predictionsResLm3)

# Show the predictions visually
# 1. Open jpeg file
jpeg("Graphs/ResLm3_test_predictions.jpg")
# 2. Create the plot
plot(olympicDataPredictions$TotalMedals, olympicDataPredictions$predictionsResLm3, xlab="y", ylab=expression(hat(y)), xlim=c(0,120),ylim=c(0,120)  ,pch=19, col="red")
title(main = "ResLm3 Model - Predictions" , sub = "Full data set")
# 3. Close the file
dev.off()

# ---------------------------------------------
# Make predictions for 2020
# ---------------------------------------------

predictions2020ResLm3<- predict(ResLm3,olympicData2020)

predictions2020ResLm3  <- round(predictions2020ResLm3 )
predictions2020ResLm3 
# Append predictions to the Prediction data set 
olympicData2020Predictions <- cbind(olympicData2020Predictions,predictions2020ResLm3)

sum(olympicData2020Predictions$predictions2020ResLm3)
# [1] 985

#------------------------------------------------------------------------------------------------------
# Use lm to show the results are the same 
ResultLm3 <- lm( TotalMedals ~ gold_pre1 + med_pre1 + gold_pre2 + med_pre2 + gold_pre3 + med_pre3 
                 +pre_host2+ pre_host + Host + post_host + post_host2 + TotalEntries + Awarded.Total, 
                 olympicData)

summary(ResultLm3)
#------------------------------------------------------------------------------------------------------

###########################################################################################
# ResRf1 - Random Forest- Results 1 - with all Results variables 
# Use tuneLength = 10
###########################################################################################


# Fit random forest
ResRf1 <- train(
  TotalMedals ~ gold_pre1 + med_pre1 + gold_pre2 + med_pre2 + gold_pre3 +
                med_pre3 + pre_host2  +pre_host +
                Host + post_host +post_host2 + TotalEntries + Awarded.Total,
  data = olympicData, 
  method = "ranger",
  tuneLength = 10
)

ResRf1


ResRf1$finalModel

# Use the mean of the R2 /RMSE for the 25 samples
mean(ResRf1$resample$Rsquared)
mean(ResRf1$resample$RMSE)
mean(ResRf1$resample$MAE)

#ggplot(varImp(ResRf1))

# ---------------------------------------------
# Make predictions for the whole data set
# ---------------------------------------------

predictionsResRf1<- predict(ResRf1,olympicData)

predictionsResRf1 <- round(predictionsResRf1)

# Append predictions to the Prediction data set 
olympicDataPredictions <- cbind(olympicDataPredictions,predictionsResRf1)


# Show the predictions visually
# 1. Open jpeg file
jpeg("Graphs/ResRf1_test_predictions.jpg")
# 2. Create the plot
plot(olympicDataPredictions$TotalMedals, olympicDataPredictions$predictionsResRf1, xlab="y", ylab=expression(hat(y)), xlim=c(0,120),ylim=c(0,120)  ,pch=19, col="red")
title(main = "ResRf1 Model - Predictions" , sub = "Full data set")
# 3. Close the file
dev.off()


#----------------------------------------------------
# Make predictions for 2020
#----------------------------------------------------
predictions2020ResRf1<- predict(ResRf1,olympicData2020)

predictions2020ResRf1 <- round(predictions2020ResRf1)
predictions2020ResRf1

# Append predictions to the Prediction data set 
olympicData2020Predictions <- cbind(olympicData2020Predictions,predictions2020ResRf1)

# Sum the number of medals predicted
sum(predictions2020ResRf1)
sum(olympicData2020Predictions$predictions2020ResRf1)

###########################################################################################
# gLm1 - GLMNet regression - Results 1 - with all results variables
#   10-fold  Cross validation  - ***did not work ***
###########################################################################################
# 
# myControl <- trainControl(
#   method = "cv", 
#   number = 10,
#   summaryFunction = twoClassSummary,
#   classProbs = FALSE, # IMPORTANT!
#   verboseIter = TRUE
# )
# 
# # Fit glmnet model: model
#   Glm2 <- train(
#     TotalMedals ~  med_pre1 + med_pre2  + med_pre3 +  
#                    pre_host2+ pre_host + Host + post_host + post_host2 +
#                   TotalEntries + Awarded.Total, 
#   olympicData,
#   method = "glmnet",
#   trControl = myControl
# )


##########################################################
# AllkNN methods -  kNN regressions 
#  Using the 'Result' variables
#########################################################

#--------------------------------------------------------------------------------
# Preparation of the data for kNN
# Only numeric variables can be used and they must be scaled
#--------------------------------------------------------------------------------

# Take a copy of the data set
olympicData2 = olympicData

# Put TotalMedals (outcome variable) in its own object
TotalMedals_outcome <- olympicData2 %>% select(TotalMedals)

# Kep any non predictors separately
Non_predictors <-  olympicData2 [, c("Year", "HostCity", "HostCountry", "HostCountryCode")]

# Remove any variables from the data set that are not to be used as dependent variables
# Keep only the necessary columns and re-order
col_order <- c( "Awarded.Gold" ,  "Awarded.Silver" , "Awarded.Bronze" , "Awarded.Total",
                 "pre_host2" ,  "pre_host"  , "Host" , "post_host" ,  "post_host2"  ,     
                 "gold_pre1" ,  "med_pre1" ,  "gold_pre2",  "med_pre2" , "gold_pre3" , "med_pre3" ,
                 "TotalEntries" ,"GoldMedals"  , "SilverMedals", "BronzeMedals" ) 

olympicData2  <- as.data.frame(olympicData2 ) [, col_order]


# Scale the numeric variables
olympicData2[, c("Awarded.Gold", "Awarded.Silver", "Awarded.Bronze" , "Awarded.Total",
                 "gold_pre1", "med_pre1", "gold_pre2", "med_pre2", "gold_pre3", "med_pre3", "TotalEntries",
                 "GoldMedals",  "SilverMedals", "BronzeMedals" )]  <- scale(olympicData2[, c("Awarded.Gold", "Awarded.Silver", "Awarded.Bronze" , "Awarded.Total",
                                                                                             "gold_pre1", "med_pre1", "gold_pre2", "med_pre2", "gold_pre3", "med_pre3", "TotalEntries",
                                                                                             "GoldMedals",  "SilverMedals", "BronzeMedals") ])

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
# AllKNN1 - kNN regression - k=33
#-----------------------------------------------------------------

# Decide on the number of neighbors (k).
#There are several rules of thumb, one being the square root of the number of observations in the training set.
#In this case, we select 33 as the number of neighbors, which is approximately the square root of our sample size N = 1070.
knn_reg_results <- knn.reg(reg_pred_train, reg_pred_test, medals_outcome_train, k = 33)

# Store predictors, independent variable with the test prediction 
predictions_test = Non_predictors_test
predictions_test = cbind(predictions_test , medals_outcome_test)
predictions_test = cbind(predictions_test, knn_reg_results$pred)

# Show if the prediction is good , if so it should be near y=x line

# Show the predictions visually
# 1. Open jpeg file
jpeg("Graphs/ReskNN1_test_predictions.jpg")
# 2. Create the plot
plot(medals_outcome_test, knn_reg_results$pred, xlab="y", ylab=expression(hat(y)), xlim=c(0,120),ylim=c(0,120)  ,pch=19, col="red")
title(main = "ReskNN1 Model - Predictions (k=33) " , sub = "Training set")
# 3. Close the file
dev.off()


#mean square prediction error
mean((medals_outcome_test- knn_reg_results$pred) ^ 2)
#[1] 52.45491

# Root mean square prediction error RMSE
sqrt( mean((medals_outcome_test- knn_reg_results$pred) ^ 2))
#[1] 7.242576

#mean absolute error
mean(abs(medals_outcome_test - knn_reg_results$pred))
#[1] 2.175599

#-----------------------------------------------------------------
# AllKNN2 - kNN regression - k=3
#-----------------------------------------------------------------

# Decide on the number of neighbors (k).
#There are several rules of thumb, one being the square root of the number of observations in the training set.
#In this case, we select 33 as the number of neighbors, which is approximately the square root of our sample size N = 1070.
knn_reg_results2 <- knn.reg(reg_pred_train, reg_pred_test, medals_outcome_train, k = 3)

# Store prediction with predictors and independent variable
predictions_test = cbind(predictions_test, knn_reg_results2$pred)


# Show the predictions visually
# 1. Open jpeg file
jpeg("Graphs/ReskNN2_test_predictions.jpg")
# 2. Create the plot
plot(medals_outcome_test, knn_reg_results2$pred, xlab="y", ylab=expression(hat(y)), xlim=c(0,120),ylim=c(0,120)  ,pch=19, col="red")
title(main = "ReskNN2 Model - Predictions (k=33) " , sub = "Training set")
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
# AllKNN3 - kNN regression - k=15
#-----------------------------------------------------------------

# Decide on the number of neighbors (k).
#There are several rules of thumb, one being the square root of the number of observations in the training set.
#In this case, we select 33 as the number of neighbors, which is approximately the square root of our sample size N = 1070.
knn_reg_results3 <- knn.reg(reg_pred_train, reg_pred_test, medals_outcome_train, k = 15)

# Store predictors independent variable
predictions_test = cbind(predictions_test, knn_reg_results3$pred)

# Show the predictions visually
# 1. Open jpeg file
jpeg("Graphs/ReskNN3_test_predictions.jpg")
# 2. Create the plot
plot(medals_outcome_test, knn_reg_results3$pred, xlab="y", ylab=expression(hat(y)), xlim=c(0,120),ylim=c(0,120)  ,pch=19, col="red")
title(main = "ReskNN3 Model - Predictions (k=33) " , sub = "Training set")
# 3. Close the file
dev.off()


#mean square prediction error
mean((medals_outcome_test- knn_reg_results3$pred) ^ 2)

# Root mean square prediction error RMSE
sqrt( mean((medals_outcome_test- knn_reg_results3$pred) ^ 2))
#[1] 4.903387

#mean absolute error
mean(abs(medals_outcome_test - knn_reg_results3$pred))
#[1] 1.558955

##################################################################
# Write the 2020 predictions for Data set 2 - previous results
# Save as csv for further use
##################################################################

write.csv(olympicData2020Predictions,"Graphs/Res_predictions.csv", row.names = FALSE, quote=FALSE)         
         