
#-----------------------------------------------------------------------
#   
#       DMML project  
#       Script   : DMML_ETL_economic_data.R
#       Author  : Philippe Tap
#       Date: April 2020
#
#-----------------------------------------------------------------------
# This script deals with the ETL of the 'pwt91.csv' economic file
# It outputs the cleaned and transformed data to 'economic_table.csv'
#
# Data source - pwt91.csv
# https://www.rug.nl/ggdc/productivity/pwt/
#----------------------------------------------------------------------
# Identifier variables
#----------------------------------------------------------------------
# countrycode	3-letter ISO country code
# country	Country name
# year	Year
#----------------------------------------------------------------------
# Real GDP, employment and population levels
#----------------------------------------------------------------------
# pop	  Population (in millions)
# rgdpe	Expenditure-side real GDP at chained PPPs (in mil. 2011US$)
# emp	  Number of persons engaged (in millions)
#----------------------------------------------------------------------
# National accounts-based variables	
#----------------------------------------------------------------------
# rgdpna	Real GDP at constant 2011 national prices (in mil. 2011US$)

#################################
# Load required libraries 
#################################

library(stringr)
library(Amelia)
library(dplyr)
library (data.table)

# Read the initial csv file and put it in a data.frame.
csvData <- read.csv("data/pwt91.csv", header=T, na.strings=c(""), stringsAsFactors = F)

# Keep only the relavnt columns and only the rows since 1988
# A lot of data is missing before 1988
economicData = csvData [ csvData$year >= 1988, c('countrycode', 'country', 'year','rgdpe', 'pop',	'emp')]

# Examine the data set 
attach(economicData)
summary(economicData)
str(economicData)


# Keep economic data only for pre-olympic years e.g 2011, 2015...
economicOlympicData = economicData [ (year %% 4) == 3 , ]

# Add an OlympicYear column for the corresponding Olympic year e.g. 2012, 2016 ...
economicOlympicData$OlympicYear = economicOlympicData$year + 1

# Add a GDPpC column for the GDP per capita
economicOlympicData$GDPpC = economicOlympicData$rgdpe / economicOlympicData$pop

# Add an EmpRate column for the employment rate
economicOlympicData$EmpRate = economicOlympicData$emp / economicOlympicData$pop

# Rename the columns
names(economicOlympicData)[names(economicOlympicData) == "countrycode"] <- "ISOCode"
names(economicOlympicData)[names(economicOlympicData) == "country"] <- "Country"
names(economicOlympicData)[names(economicOlympicData) == "pop"] <- "Population"

colnames (economicOlympicData)

# Select final columns and re-order
col_order = c("OlympicYear", "ISOCode", "Country", "Population" , "GDPpC" , "EmpRate" )

economicOlympicData <- as.data.frame(economicOlympicData) [, col_order]

attach(economicOlympicData)

################################################################
# Remove ',' in the country names
################################################################

subset(economicOlympicData , grepl(",", economicOlympicData$Country) )

# Remove all ',' in Name, Team, Event, 
economicOlympicData$Country<- str_remove_all(economicOlympicData$Country, ",")


################################################################
#   Missing values
################################################################

#------------------------------
# OVERALL MISSING VALUES
#------------------------------

missmap(economicOlympicData, main = "Missing values vs observed")

# NA values per column
sapply(economicOlympicData,function(x) sum(is.na(x)))

#------------------------------
#  Missing population 
#------------------------------

pop_na = economicOlympicData [ is.na(Population), c("OlympicYear", "ISOCode", "Country","Population")]
pop_na
# OlympicYear ISOCode                   Country Population
# 2898         1992     CUW                   Curaçao         NA
# 2902         1996     CUW                   Curaçao         NA
# 2906         2000     CUW                   Curaçao         NA
# 2910         2004     CUW                   Curaçao         NA
# 10582        1992     SXM Sint Maarten (Dutch part)         NA
# 10586        1996     SXM Sint Maarten (Dutch part)         NA
# 10590        2000     SXM Sint Maarten (Dutch part)         NA
# 10594        2004     SXM Sint Maarten (Dutch part)         NA

# Neither of these countries are Olympic Nations - so no action

#------------------------------
#  Missing rgdpe
#------------------------------

rgdpe_na = economicOlympicData [ is.na(GDPpC), c("OlympicYear", "ISOCode", "Country","Population")]
rgdpe_na
#          OlympicYear ISOCode                Country  Population
# 2898         1992     CUW                   Curaçao         NA
# 2902         1996     CUW                   Curaçao         NA
# 2906         2000     CUW                   Curaçao         NA
# 2910         2004     CUW                   Curaçao         NA
# 10582        1992     SXM Sint Maarten (Dutch part)         NA
# 10586        1996     SXM Sint Maarten (Dutch part)         NA
# 10590        2000     SXM Sint Maarten (Dutch part)         NA
# 10594        2004     SXM Sint Maarten (Dutch part)         NA

# Neither of these countries are Olympic Nations - so no action

#---------------------------------------------------------------------
#  Missing EmpRate - Actually decided to ignore emp and empRate
#---------------------------------------------------------------------
emp_na = economicOlympicData [ is.na(EmpRate), ]

emp_na

emp_na_counts = table(emp_na$OlympicYear)
emp_na_counts

# 1992 1996 2000 2004 2008 2012 2016 
# 6    7    7    7    7   10   10 

barplot(emp_na_counts, main = "NAs per Year (emp)", xlab = "Year" , ylab = "Count")

emp_na_counts = table(emp_na$Country)
emp_na_counts

# only exotic countries 

# Anguilla       Antigua and Barbuda                   Bermuda    British Virgin Islands            Cayman Islands 
# 4                         4                         2                         6                         2 
# Curaçao                  Dominica                Montserrat     Saint Kitts and Nevis                Seychelles 
# 5                         4                         7                         7                         1 
# Sint Maarten (Dutch part)  Turks and Caicos Islands 
# 7                         5 

# ATG  Antigua and Barbuda IOC =  ANT	
# DMA Dominica   IOC =  DMA	 Dominica
# BMU Bermuda  IOC = BER	
# VGB British Virgin Islands  IOC = IVB	
# KNA  Saint Kitts and Nevis IOC = SKN 	 
# CYM Cayman Islands IOC = CAY	 
# SYC Seychelles IOC = SEY	

sapply(economicOlympicData,function(x) sum(is.na(x))) 
################################################################
#   Missing values - end
################################################################

###############################################################
#  Remove EmpRate column
#  Decided not to use it in the study as too many missing values
###############################################################

col_order = c("OlympicYear", "ISOCode", "Country", "Population" , "GDPpC" )

economicOlympicData <- as.data.frame(economicOlympicData) [, col_order]

##################################################################
# This is the final version of the economicOlympicData data set
##################################################################

write.csv(economicOlympicData,"data/economic_table.csv", row.names = FALSE, quote=FALSE)


