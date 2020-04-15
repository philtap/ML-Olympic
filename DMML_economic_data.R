#-----------------------------------------------------------------------
#                 DMML_economic_data.R
#-----------------------------------------------------------------------
# This script deals with the ETL of the pwt91.csv economic file
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

# Load required libraries 
library(stringr)
library(Amelia)
library(dplyr)
library (data.table)

setwd("C:/Users/philippet/OneDrive - Datalex/NCIRL/Modules/Data mining and Machine learning I/Project/Datasets/")

# Read the initial csv file and put it in a data.frame.
csvData <- read.csv("pwt91.csv", header=T, na.strings=c(""), stringsAsFactors = F)

economicData = csvData [ csvData$year >= 1988, c('countrycode', 'country','year'	,'rgdpe', 'pop',	'emp',  'rgdpna')]

attach(economicData)
summary(economicData)
str(economicData)




################################################################
#   Missing values
###############################################################

missmap(economicData, main = "Missing values vs observed")

# NA values per column
sapply(economicData,function(x) sum(is.na(x)))

rgdpe_na = economicData [ is.na(rgdpe), ]

rgdpe_na_counts = table(rgdpe_na$country)

rgdpe_na_counts


# From 1988
# Armenia                Azerbaijan                   Belarus    Bosnia and Herzegovina 
# 2                         2                         2                         2 
# Croatia                   Curaçao            Czech Republic                   Estonia 
# 2                        17                         2                         2 
# Georgia                Kazakhstan                Kyrgyzstan                    Latvia 
# 2                         2                         2                         2 
# Lithuania                Montenegro           North Macedonia       Republic of Moldova 
# 2                         2                         2                         2 
# Russian Federation                    Serbia Sint Maarten (Dutch part)                  Slovakia 
# 2                         2                        17                         2 
# Slovenia                Tajikistan              Turkmenistan                   Ukraine 
# 2                         2                         2                         2 
# Uzbekistan                     Yemen 
# 2                         1 



# From 1990 only 2 exotic countries  
# Curaçao Sint Maarten (Dutch part) 
# 15                        15 

barplot(rgdpe_na_counts, main = "NAs per Year", xlab = "Year" , ylab = "Count")

emp_na = economicData [ is.na(emp), ]
table(emp_na$year)

table(emp_na$country)
# only exotic countries 

# Anguilla       Antigua and Barbuda                     Aruba                   Bermuda 
# 15                        19                         1                        10 
# British Virgin Islands            Cayman Islands                   Curaçao                  Dominica 
# 27                        10                        21                        16 
# Montserrat     Saint Kitts and Nevis                Seychelles Sint Maarten (Dutch part) 
# 28                        28                         2                        28 
# Turks and Caicos Islands 
# 21 
################################################################
#   Missing values - end
################################################################


# Keep economic data only for pre-olympic years e.g 2011, 2015...
economicOlympicData = economicData [ (year %% 4) == 3 , ]


# Add an OlympicYear column for the corresponding Olympic year e.f 2012, 2016 ...
economicOlympicData$OlympicYear = economicOlympicData$year + 1

# Add a gdppc column for the GDP per capita
economicOlympicData$GDPpC = economicOlympicData$rgdpe / economicOlympicData$pop

# Add an emprate column for the employment rate
economicOlympicData$EmpRate = economicOlympicData$emp / economicOlympicData$pop

names(economicOlympicData)[names(economicOlympicData) == "countrycode"] <- "ISOCode"
names(economicOlympicData)[names(economicOlympicData) == "country"] <- "Country"
names(economicOlympicData)[names(economicOlympicData) == "pop"] <- "Population"

colnames (economicOlympicData)

# Select final columns and re-order
col_order = c("OlympicYear", "ISOCode", "Country", "Population" , "GDPpC" , "EmpRate" )

economicOlympicData <- as.data.frame(economicOlympicData) [, col_order]

##################################################################
# This is the final version of the economicOlympicData data set
##################################################################

write.csv(economicOlympicData,"economic_table.csv", row.names = FALSE)


