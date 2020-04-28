#--------------------------------------------------------------------------------------------------
#   
#       DMML project  
#       Script    : DMML_ML_methods_All.R (all factors, Enonomic and previous results)
#       Author    : Philippe Tap
#       Date      : April 2020
#
#--------------------------------------------------------------------------------------------------
# This script apply ML methods to the third data set, which is a combination of the first 2 data sets
# It is based on general, economic and previous results factors
#  - economic factors : Population and GDPpC a year before the current Olympic 
#  - previous results : total medals and gold medals at the last 3 Olympics
#        gold_pre1 , med_pre1, gold_pre2, med_pre2, gold_pre3,med_pre3,  
#  - general factors: 
#        - Was the country host before or after the current games pre_host2, pre_host,  Host, post_host,post_host2
#        - Number of events entered by the country: TotalEntries
#        - Awarded.Total: total medal awarded at the current games
#
# The following models are applied
# AllLM1	  all prev results
# AllLM2    without Gold results
# AllRF1	  all results, tuning=1
# AllRF2	  all results, tuning=10
# AllkNN1	  all results, k=33
# AllkNN2	  all results, k=3
# AllkNN3	  all results, k=15
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
library(GGally)
library(FNN)
library(ipred)
#library (class)

setwd("C:/Users/philippet/OneDrive - Datalex/NCIRL/Modules/Data mining and Machine learning I/Project/ML-Olympic")

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
# Convert them to factors
olympicData$Host  = as.factor(olympicData$Host)
olympicData$post_host2  = ifelse(olympicData$post_host2 == 'Y' , TRUE, FALSE)
olympicData$pre_host2  = as.factor(olympicData$pre_host2 )
olympicData$pre_host  = as.factor(olympicData$pre_host)
olympicData$post_host  = as.factor(olympicData$post_host)
olympicData$post_host2  = as.factor(olympicData$post_host2)

colnames(olympicData)
str(olympicData)

# Select all the necessary columns for the 'ALL in One' regression containing Economic and Results and re-order
col_order <- c("Year", "CountryCode",
               "Population", "GDPpC",
               "HostCity"   ,     "HostCountry"   ,  "HostCountryCode" , 
               "Awarded.Gold" ,  "Awarded.Silver" , "Awarded.Bronze" , "Awarded.Total",
               "pre_host2" ,  "pre_host"  , "Host" , "post_host" ,  "post_host2"  ,     
               "gold_pre1" ,  "med_pre1" ,  "gold_pre2",  "med_pre2" , "gold_pre3" , "med_pre3" ,
               "TotalEntries" ,"GoldMedals"  , "SilverMedals", "BronzeMedals" ,"TotalMedals") 

olympicData  <- as.data.frame(olympicData ) [, col_order]

###############################################################################################
#  Missing values , Data cleanup
###############################################################################################

# Missing values vs observed 
missmap(olympicData, main = "Missing values vs observed")

#missing values
sapply(olympicData,function(x) sum(is.na(x)))


colnames(olympicData)

# Examine missing values that may impede the models
olympicData_na = olympicData [ is.na (olympicData$GDPpC)      |  is.na (olympicData$Population) |  
                               is.na (olympicData$gold_pre1)  |  is.na(olympicData$med_pre1) | 
                               is.na (olympicData$gold_pre2 ) |  is.na( olympicData$med_pre2) | 
                               is.na (olympicData$gold_pre3 ) |  is.na( olympicData$med_pre3) ,]
# 322 observations have NAs

olympicData_na_counts = table(olympicData_na$Year)
olympicData_na_counts
barplot(olympicData_na_counts, main = "NA per year", xlab = "Year" , ylab = "Count", ylim=c(0,120), col = "lightblue")


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
olympicData = olympicData [ ! is.na (olympicData$GDPpC) & ! is.na (olympicData$Population) &
                              ! is.na(olympicData$gold_pre1) & !is.na(olympicData$med_pre1) & !is.na(olympicData$gold_pre2 ) & 
                              !is.na(olympicData$med_pre2 ) & !is.na( olympicData$gold_pre3 ) & !is.na( olympicData$med_pre3),]

# Missing values vs observed 
missmap(olympicData, main = "Missing values vs observed")

#missing values
sapply(olympicData,function(x) sum(is.na(x)))


# Use this data set to store the predictions for each algorithm
olympicDataPredictions = olympicData [ , c("Year" , "CountryCode", "TotalMedals") ]

# For some algorithms, data set must not contain any NAs so suppress NAs 
olympicData2020 <- olympicData2020 [ ! is.na (olympicData2020$GDPpC) & ! is.na (olympicData2020$Population) &
                                       ! is.na(olympicData2020$gold_pre1) & !is.na(olympicData2020$med_pre1) & !is.na(olympicData2020$gold_pre2 ) & 
                                       !is.na(olympicData2020$med_pre2 ) & !is.na( olympicData2020$gold_pre3 ) & !is.na( olympicData2020$med_pre3),]


# Use this data set to store the predictions for 2020 for each algorithm
olympicData2020Predictions = olympicData2020 [ , c("Year" , "CountryCode", "TotalMedals") ]


#############################
# Correlation
#############################

ggpairs(data=olympicData, columns=c("Population","GDPpC" , "med_pre1", "med_pre2", "med_pre3"), title="Previous results variables - Correlation")


###########################################################################################
# AllLm1 - Linear regression - All 1 - with all results variables
#    10-fold  Cross validation 
###########################################################################################
train_control = trainControl(
  method = "cv", 
  number = 10,
  savePredictions = TRUE,
  verboseIter = TRUE)

# Fit linear model 1 using 10-fold CV
AllLm1 <- train(
  TotalMedals ~ GDPpC + Population +  gold_pre1 + med_pre1 + gold_pre2 + med_pre2 + gold_pre3 + med_pre3 +pre_host2+ pre_host + Host + post_host + post_host2 + TotalEntries + Awarded.Total, 
  olympicData,
  method = "lm",
  trControl = train_control
)

AllLm1

summary(AllLm1)

# Residuals:
#   Min       1Q   Median       3Q      Max 
# -15.0046  -0.2903   0.0993   0.2091  18.4152 
# 
# Coefficients:
#   Estimate Std. Error t value Pr(>|t|)    
# (Intercept)    -5.084e-02  1.573e+00  -0.032   0.9742    
# GDPpC           1.330e-06  3.844e-06   0.346   0.7294    
# Population      5.366e-04  6.796e-04   0.790   0.4300    
# gold_pre1       8.155e-01  7.480e-02  10.902  < 2e-16 ***
#   med_pre1        4.646e-01  4.030e-02  11.528  < 2e-16 ***
#   gold_pre2      -5.065e-02  8.045e-02  -0.630   0.5291    
# med_pre2        9.994e-02  3.973e-02   2.515   0.0120 *  
#   gold_pre3      -1.185e-01  7.160e-02  -1.654   0.0983 .  
# med_pre3        4.427e-02  3.504e-02   1.263   0.2067    
# pre_host2TRUE  -5.169e+00  1.210e+00  -4.270 2.13e-05 ***
#   pre_hostTRUE   -7.320e+00  1.286e+00  -5.692 1.62e-08 ***
#   HostTRUE        1.122e+01  1.129e+00   9.938  < 2e-16 ***
#   post_hostTRUE   7.137e+00  1.119e+00   6.378 2.68e-10 ***
#   post_host2TRUE  5.543e+00  1.033e+00   5.368 9.78e-08 ***
#   TotalEntries    2.453e-02  2.540e-03   9.658  < 2e-16 ***
#   Awarded.Total  -2.440e-04  1.690e-03  -0.144   0.8852    
# ---
#   Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
# 
# Residual standard error: 2.569 on 1054 degrees of freedom
# Multiple R-squared:  0.9604,	Adjusted R-squared:  0.9598 
# F-statistic:  1704 on 15 and 1054 DF,  p-value: < 2.2e-16
# AllLm1


AllLm1$finalModel

# Coefficients:
#   (Intercept)           GDPpC      Population       gold_pre1        med_pre1       gold_pre2  
# -5.084e-02       1.330e-06       5.366e-04       8.155e-01       4.646e-01      -5.065e-02  
# med_pre2       gold_pre3        med_pre3   pre_host2TRUE    pre_hostTRUE        HostTRUE  
# 9.994e-02      -1.185e-01       4.427e-02      -5.169e+00      -7.320e+00       1.122e+01  
# post_hostTRUE  post_host2TRUE    TotalEntries   Awarded.Total  
# 7.137e+00       5.543e+00       2.453e-02      -2.440e-04  


# examine model predictions for each fold.
AllLm1$resample
# RMSE  Rsquared       MAE Resample
# 1  1.926380 0.9580799 0.9650272   Fold01
# 2  2.028964 0.9398348 1.1102522   Fold02
# 3  3.421766 0.9622864 1.3863730   Fold03
# 4  2.532155 0.9209092 1.1966120   Fold04
# 5  4.197046 0.9576281 1.7339485   Fold05
# 6  2.928953 0.8964699 1.4095108   Fold06
# 7  2.598409 0.9176237 1.1156871   Fold07
# 8  3.193930 0.9498379 1.4788223   Fold08
# 9  2.771533 0.9673574 1.1505226   Fold09
# 10 3.293470 0.9676242 1.5863660   Fold10



# Plot importance of each feature
# 1. Open jpeg file
jpeg("Graphs/AllLM1_feature_imp.jpg")
# 2. Create the plot
ggplot(varImp(AllLm1))
# 3. Close the file
dev.off()

# Use lm to get some details like vif
Lm1All <- lm( TotalMedals ~ GDPpC + Population +  gold_pre1 + med_pre1 + gold_pre2 + med_pre2 + gold_pre3 + med_pre3 +pre_host2+ pre_host + Host + post_host + post_host2 + TotalEntries + Awarded.Total, 
                 olympicData)

summary(Lm1All)
vif (Lm1All)

# GDPpC    Population     gold_pre1      med_pre1     gold_pre2      med_pre2 
# 1.105770      1.399773     20.142481     41.975768     21.518793     39.378795 
# gold_pre3      med_pre3     pre_host2      pre_host          Host     post_host 
# 20.039821     32.533773      1.324143      1.494385      1.342805      1.131608 
# post_host2  TotalEntries Awarded.Total 
# 1.123010      6.990297      1.047942 




# ---------------------------------------------
# Make predictions for the whole data set
# ---------------------------------------------

predictionsAllLm1<- predict(AllLm1,olympicData)

predictionsAllLm1  <- round(predictionsAllLm1 )

# Append predictions to the Prediction data set 
olympicDataPredictions <- cbind(olympicDataPredictions,predictionsAllLm1)

# Show the predictions visually
# 1. Open jpeg file
jpeg("Graphs/AllLm1_test_predictions.jpg")
# 2. Create the plot
plot(olympicDataPredictions$TotalMedals, olympicDataPredictions$predictionsAllLm1, xlab="y", ylab=expression(hat(y)), xlim=c(0,120),ylim=c(0,120)  ,pch=19, col="red")
title (main = "AllLm1 Model - Predictions" , sub = "Full data set")
# 3. Close the file
dev.off()


# ---------------------------------------------
# Make predictions for 2020
# ---------------------------------------------

predictions2020AllLm1<- predict(AllLm1,olympicData2020)

predictions2020AllLm1  <- round(predictions2020AllLm1 )

# Append predictions to the Prediction data set 
olympicData2020Predictions <- cbind(olympicData2020Predictions,predictions2020AllLm1)

sum(olympicData2020Predictions$predictions2020AllLm1)
# [1] 985


##########################################################
# AllkNN1 -  kNN regression 
#  All results variables
#########################################################
# https://quantdev.ssri.psu.edu/sites/qdev/files/kNN_tutorial.html

#--------------------------------------------------------------------------------
# Preparation of the data for kNN
# Only numeric variables can be used and they must be scaled
#--------------------------------------------------------------------------------

# Take a copy of the data set
olympicData2 = olympicData

# Scale variable and use numeric variables only
str(olympicData2)

#'data.frame':	1070 obs. of  27 variables:
# $ Year           : int  1992 1992 1992 1992 1992 1992 1992 1992 1992 1992 ...
# $ CountryCode    : chr  "ALG" "AND" "AUS" "AUT" ...
# $ Population     : num  26.5543 0.0567 17.2719 7.7727 10.0394 ...
# $ GDPpC          : num  8286 19534 26397 26199 25668 ...
# $ HostCity       : chr  "Barcelona" "Barcelona" "Barcelona" "Barcelona" ...
# $ HostCountry    : chr  "Spain" "Spain" "Spain" "Spain" ...
# $ HostCountryCode: chr  "ESP" "ESP" "ESP" "ESP" ...
# $ Awarded.Gold   : int  260 260 260 260 260 260 260 260 260 260 ...
# $ Awarded.Silver : int  257 257 257 257 257 257 257 257 257 257 ...
# $ Awarded.Bronze : int  298 298 298 298 298 298 298 298 298 298 ...
# $ Awarded.Total  : int  815 815 815 815 815 815 815 815 815 815 ...
# $ pre_host2      : Factor w/ 2 levels "FALSE","TRUE": 1 1 1 1 1 1 1 1 1 1 ...
# $ pre_host       : Factor w/ 2 levels "FALSE","TRUE": 1 1 1 1 1 1 1 1 1 1 ...
# $ Host           : Factor w/ 2 levels "FALSE","TRUE": 1 1 1 1 1 1 1 1 1 1 ...
# $ post_host      : Factor w/ 2 levels "FALSE","TRUE": 1 1 1 1 1 1 1 1 1 1 ...
# $ post_host2     : Factor w/ 2 levels "FALSE","TRUE": 1 1 2 1 1 1 1 1 1 1 ...
# $ gold_pre1      : int  0 0 3 1 0 0 0 1 0 0 ...
# $ med_pre1       : int  0 0 14 1 2 0 0 6 0 0 ...
# $ gold_pre2      : int  0 0 4 1 1 0 0 1 0 0 ...
# $ med_pre2       : int  2 0 24 3 4 0 0 8 0 1 ...
# $ gold_pre3      : int  0 0 2 1 1 0 0 2 0 0 ...
# $ med_pre3       : int  0 0 9 4 1 0 0 4 0 0 ...
# $ TotalEntries   : int  27 7 238 87 85 7 6 140 7 9 ...
# $ GoldMedals     : int  1 0 7 0 0 0 0 2 0 0 ...
# $ SilverMedals   : int  0 0 9 2 1 0 0 1 0 0 ...
# $ BronzeMedals   : int  1 0 11 0 2 0 0 0 0 0 ...
# $ TotalMedals    : int  2 0 27 2 3 0 0 3 0 0 ...

# Put TotalMedals (outcome variable) in its own object
TotalMedals_outcome <- olympicData2 %>% select(TotalMedals)

Non_predictors <-  olympicData2 [, c("Year", "HostCity", "HostCountry", "HostCountryCode")]
# remove original from the data set

olympicData2 <- select (olympicData2,-c(TotalMedals,Year, HostCity, HostCountry, HostCountryCode, CountryCode))

# Scale the numeric variables
olympicData2[, c("Population", "GDPpC","Awarded.Gold", "Awarded.Silver", "Awarded.Bronze" , "Awarded.Total",
             "gold_pre1", "med_pre1", "gold_pre2", "med_pre2", "gold_pre3", "med_pre3", "TotalEntries",
             "GoldMedals",  "SilverMedals", "BronzeMedals" )]  <- scale(olympicData2[, c("Population", "GDPpC","Awarded.Gold", "Awarded.Silver", "Awarded.Bronze" , "Awarded.Total",
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
# kNN regression - k=33
#-----------------------------------------------------------------

# Decide on the number of neighbors (k).
#There are several rules of thumb, one being the square root of the number of observations in the training set.
I#n this case, we select 33 as the number of neighbors, which is approximately the square root of our sample size N = 1070.
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
jpeg("Graphs/AllkNN1_test_predictions.jpg")
# 2. Create the plot
plot(medals_outcome_test, knn_reg_results$pred, xlab="y", ylab=expression(hat(y)), xlim=c(0,120),ylim=c(0,120)  ,pch=19, col="red")
title(main = "AllkNN1 Model - Predictions (k=33) " , sub = "Training set")
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

##########################################################
# AllkNN2 -  kNN regression 
#  All results variables
#########################################################
#-----------------------------------------------------------------
# kNN regression 2 - k=3
#-----------------------------------------------------------------
#There are several rules of thumb, one being the square root of the number of observations in the training set.
# In this case, we select 33 as the number of neighbors, which is approximately the square root of our sample size N = 1070.
knn_reg_results2 <- knn.reg(reg_pred_train, reg_pred_test, medals_outcome_train, k = 3)
print(knn_reg_results2)

# Predictions on the test set
knn_reg_results$pred 

# Store predictors independent variable
predictions_test = cbind(predictions_test, knn_reg_results2$pred)

# Show the predictions visually
# 1. Open jpeg file
jpeg("Graphs/AllkNN1_test_predictions.jpg")
# 2. Create the plot
plot(medals_outcome_test, knn_reg_results2$pred, xlab="y", ylab=expression(hat(y)), xlim=c(0,120),ylim=c(0,120)  ,pch=19, col="red")
title(main = "AllkNN2 Model - Predictions (k=3) " , sub = "Training set")
# 3. Close the file
dev.off()

#mean square prediction error
mean((medals_outcome_test- knn_reg_results2$pred) ^ 2)
#[1] 11.6534

# Root mean square prediction error RMSE
sqrt( mean((medals_outcome_test- knn_reg_results2$pred) ^ 2))
#[1] 3.413708

#mean absolute error
mean(abs(medals_outcome_test - knn_reg_results2$pred))
# [1] 1.199005


##########################################################
# AllkNN3 -  kNN regression 
#  All results variables
#########################################################
#-----------------------------------------------------------------
# kNN regression 2 - k=15
#-----------------------------------------------------------------
#There are several rules of thumb, one being the square root of the number of observations in the training set.
# In this case, we select 33 as the number of neighbors, which is approximately the square root of our sample size N = 1070.
knn_reg_results3 <- knn.reg(reg_pred_train, reg_pred_test, medals_outcome_train, k = 15)
print(knn_reg_results3)

# Predictions on the test set
knn_reg_results$pred 

# Store predictors independent variable
predictions_test = cbind(predictions_test, knn_reg_results3$pred)

# Show the predictions visually
# 1. Open jpeg file
jpeg("Graphs/AllkNN3_test_predictions.jpg")
# 2. Create the plot
plot(medals_outcome_test, knn_reg_results3$pred, xlab="y", ylab=expression(hat(y)), xlim=c(0,120),ylim=c(0,120)  ,pch=19, col="red")
title(main = "AllkNN3 Model - Predictions (k=15) " , sub = "Training set")
# 3. Close the file
dev.off()

#mean square prediction error
mean((medals_outcome_test- knn_reg_results3$pred) ^ 2)
#[1] 24.0432

# Root mean square prediction error RMSE
sqrt( mean((medals_outcome_test- knn_reg_results3$pred) ^ 2))
#[1] 4.903387

#mean absolute error
mean(abs(medals_outcome_test - knn_reg_results3$pred))
#[1] 1.558955

###########################################################################################
# AllRf1 - Random Forest- All 1 - with all Results variables
# Use tuneLength = 1
###########################################################################################
# Missing values vs observed 
missmap(olympicData, main = "Missing values vs observed")

#missing values
sapply(olympicData,function(x) sum(is.na(x)))

# Fit random forest
AllRf1 <- train(
  TotalMedals ~ GDPpC + Population + gold_pre1 + med_pre1 + gold_pre2 + med_pre2 + gold_pre3 +
    med_pre3 + pre_host2  +pre_host +
    Host + post_host +post_host2 + TotalEntries + Awarded.Total,
  data = olympicData,
  method = "ranger",
  tuneLength = 1
)

AllRf1
# Random Forest 
# 
# 1070 samples
# 15 predictor
# 
# No pre-processing
# Resampling: Bootstrapped (25 reps) 
# Summary of sample sizes: 1070, 1070, 1070, 1070, 1070, 1070, ... 
# Resampling results across tuning parameters:
#   
#   splitrule   RMSE      Rsquared   MAE     
# variance    3.030338  0.9440698  1.323338
# extratrees  3.009729  0.9455202  1.339083
# 
# Tuning parameter 'mtry' was held constant at a value of 3
# Tuning parameter 'min.node.size' was
# held constant at a value of 5
# RMSE was used to select the optimal model using the smallest value.
# The final values used for the model were mtry = 3, splitrule = extratrees and min.node.size = 5.
#  AllRf1$finalModel
 # Ranger result
 # 
 # Call:
 #   ranger::ranger(dependent.variable.name = ".outcome", data = x,      mtry = min(param$mtry, ncol(x)), min.node.size = param$min.node.size,      splitrule = as.character(param$splitrule), write.forest = TRUE,      probability = classProbs, ...) 
 # 
 # Type:                             Regression 
 # Number of trees:                  500 
 # Sample size:                      1070 
 # Number of independent variables:  15 
 # Mtry:                             3 
 # Target node size:                 5 
 # Variable importance mode:         none 
 # Splitrule:                        extratrees 
 # Number of random splits:          1 
 # OOB prediction error (MSE):       8.77021 
 # R squared (OOB):                  0.9466381 
# ---------------------------------------------
# Make predictions for the whole data set
# ---------------------------------------------

predictionsAllRf1<- predict(AllRf1,olympicData)

predictionsAllRf1 <- round(predictionsAllRf1)

# Append predictions to the Prediction data set
olympicDataPredictions <- cbind(olympicDataPredictions,predictionsAllRf1)

# Show the predictions visually
# 1. Open jpeg file
jpeg("Graphs/AllRf1_test_predictions.jpg")
# 2. Create the plot
plot(olympicDataPredictions$TotalMedals, olympicDataPredictions$predictionsAllRf1, xlab="y", ylab=expression(hat(y)), xlim=c(0,120),ylim=c(0,120)  ,pch=19, col="red")
title(main = "AllRf1 Model - Predictions" , sub = "Full data set")
# 3. Close the file
dev.off()


#----------------------------------------------------
# Make predictions for 2020
#----------------------------------------------------
predictions2020AllRf1<- predict(AllRf1,olympicData2020)

predictions2020AllRf1 <- round(predictions2020AllRf1)
predictions2020AllRf1

# Append predictions to the Prediction data set
olympicData2020Predictions <- cbind(olympicData2020Predictions,predictions2020AllRf1)

# Sum the number of medals predicted
sum(predictions2020AllRf1)
sum(olympicData2020Predictions$predictions2020AllRf1)

###########################################################################################
# AllRf2 - Random Forest- All 1 - with all Results variables
# Use tuneLength = 10
###########################################################################################
# Missing values vs observed 
missmap(olympicData, main = "Missing values vs observed")

#missing values
sapply(olympicData,function(x) sum(is.na(x)))

# Fit random forest
AllRf2 <- train(
  TotalMedals ~ GDPpC + Population + gold_pre1 + med_pre1 + gold_pre2 + med_pre2 + gold_pre3 +
    med_pre3 + pre_host2  +pre_host +
    Host + post_host +post_host2 + TotalEntries + Awarded.Total,
  data = olympicData,
  method = "ranger",
  tuneLength = 10
)

AllRf2
# Random Forest 

# 1070 samples
# 15 predictor
# 
# No pre-processing
# Resampling: Bootstrapped (25 reps) 
# Summary of sample sizes: 1070, 1070, 1070, 1070, 1070, 1070, ... 
# Resampling results across tuning parameters:
#   
#   mtry  splitrule   RMSE      Rsquared   MAE     
# 2    variance    3.055768  0.9446385  1.407476
# 2    extratrees  3.149784  0.9434571  1.501043
# 3    variance    2.982037  0.9455102  1.314124
# 3    extratrees  3.010421  0.9454855  1.343350
# 4    variance    2.966919  0.9457129  1.289801
# 4    extratrees  2.957881  0.9466997  1.303124
# 6    variance    2.960836  0.9453594  1.280397
# 6    extratrees  2.939013  0.9469946  1.286715
# 7    variance    2.980394  0.9446164  1.285604
# 7    extratrees  2.935267  0.9471718  1.284476
# 9    variance    2.987844  0.9442537  1.288086
# 9    extratrees  2.930315  0.9472349  1.280455
# 10    variance    2.986526  0.9443765  1.290544
# 10    extratrees  2.933072  0.9468738  1.281860
# 12    variance    3.009700  0.9433391  1.298454
# 12    extratrees  2.937509  0.9467189  1.281307
# 13    variance    3.025759  0.9427080  1.304959
# 13    extratrees  2.932756  0.9468032  1.280641
# 15    variance    3.035522  0.9423382  1.311287
# 15    extratrees  2.927416  0.9468630  1.279633
# 
# Tuning parameter 'min.node.size' was held constant at a value of 5
# RMSE was used to select the optimal model using the smallest value.
# The final values used for the model were mtry = 15, splitrule = extratrees and min.node.size = 5

# Use the mean of the R2 /RMSE for the 25 samples
mean(AllRf1$resample$Rsquared)
mean(AllRf1$resample$RMSE)
mean(AllRf1$resample$MAE)

# AllRf2$finalModel
# Ranger result
# 
# Call:
#   ranger::ranger(dependent.variable.name = ".outcome", data = x,      mtry = min(param$mtry, ncol(x)), min.node.size = param$min.node.size,      splitrule = as.character(param$splitrule), write.forest = TRUE,      probability = classProbs, ...) 
# 
# Type:                             Regression 
# Number of trees:                  500 
# Sample size:                      1070 
# Number of independent variables:  15 
# Mtry:                             15 
# Target node size:                 5 
# Variable importance mode:         none 
# Splitrule:                        extratrees 
# Number of random splits:          1 
# OOB prediction error (MSE):       8.365248 
# R squared (OOB):                  0.9491021


# ---------------------------------------------
# Make predictions for the whole data set
# ---------------------------------------------

predictionsAllRf2<- predict(AllRf2,olympicData)

predictionsAllRf2 <- round(predictionsAllRf2)

# Append predictions to the Prediction data set
olympicDataPredictions <- cbind(olympicDataPredictions,predictionsAllRf2)

# Show the predictions visually
# 1. Open jpeg file
jpeg("Graphs/AllRf2_test_predictions.jpg")
# 2. Create the plot
plot(olympicDataPredictions$TotalMedals, olympicDataPredictions$predictionsAllRf2, xlab="y", ylab=expression(hat(y)), xlim=c(0,120),ylim=c(0,120)  ,pch=19, col="red")
title(main = "AllRf2 Model - Predictions" , sub = "Full data set")
# 3. Close the file
dev.off()

#----------------------------------------------------
# Make predictions for 2020
#----------------------------------------------------
predictions2020AllRf2<- predict(AllRf2,olympicData2020)

predictions2020AllRf2 <- round(predictions2020AllRf2)
predictions2020AllRf2

# Append predictions to the Prediction data set
olympicData2020Predictions <- cbind(olympicData2020Predictions,predictions2020AllRf2)

# Sum the number of medals predicted
sum(predictions2020AllRf2)
sum(olympicData2020Predictions$predictions2020AllRf2)

##################################################################
# Write the 2020 predictions for Data set 2 - previous results
# Save as csv for further use
##################################################################

write.csv(olympicData2020Predictions,"Graphs/ALL_predictions.csv", row.names = FALSE, quote=FALSE) 


top_20_AllRf2 = olympicData2020Predictions[order(olympicData2020Predictions$predictions2020AllRf2, decreasing= T),]
top_20_AllRf2 = x[1:20,]

col_order <- c("Year", "CountryCode",
               "predictions2020AllRf2") 

top_20_AllRf2  <- as.data.frame(top_20_AllRf2  ) [, col_order]

write.csv(top_20_AllRf2 ,"Graphs/top_20_AllRf2.csv", row.names = FALSE, quote=FALSE)


