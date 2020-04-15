

# Merge all data sources 
# Note start in 1988

#   Data                          Script                          csv
#   Participation/medals          DMML_project_athletes.R       summer_table.csv
#   Economic data                 DMML_economic_data.R          economic_table.csv
#   Olympic info                  N/A (Manual)                  olympic_cities.csv
#   Host/Neighbours               To do from Olympic info

#-----------------------------------------------------------------------
#                 DMML_project_dataset.R
#-----------------------------------------------------------------------
# This script deals with the ETL of all data sources to form our data set(s)
#

# Load required libraries 
library(stringr)
library(Amelia)
library(dplyr)
library (data.table)


# IMPORTANT: FROM NOW ON ONLY KEEP THE YEARS THAT ARE NEEDED 

################################################################################################
# 1. Join Results with Economics data
################################################################################################
results <- read.csv("summer_entry_table.csv", header=T, na.strings=c(""), stringsAsFactors = F)

economics <- read.csv("economic_table.csv", header=T, na.strings=c(""), stringsAsFactors = F)

sapply(economics,function(x) sum(is.na(x))) 
sapply(results,function(x) sum(is.na(x))) 

str(results)
str(economics)

# resultsTable = as.data.table(results)
# economicsTable = as.data.table(economics)

#setkey(resultsTable,Year, Country)
#setkey(economicsTable, OlympicYear, countrycode)

colnames(results )
colnames(economics )


# Perform the join between the first 2 tables
olympicData <- merge(results, economics, 
                   by.x=c("Year", "ISOCode"),
                   by.y=c("OlympicYear", "ISOCode"),
                   all.x=TRUE)

colnames(olympicData )

col_order <- c("Year","CountryCode", "ISOCode", "Population" , "GDPpC" , "EmpRate" ,  "TotalEntries" ,"GoldMedals"  , "SilverMedals", "BronzeMedals" ,"TotalMedals") 

olympicData  <- as.data.frame(olympicData ) [, col_order]

colnames(olympicData )

str (olympicData)

olympicData2016 =  olympicData [ olympicData$Year == 2016 ,]


# Find the NA values
sapply(main,function(x) sum(is.na(x))) 

################################################################################################
# 2. Join current OlympicData data set with Olympic characteristics 
################################################################################################

olympicCities <- read.csv("olympic_cities.csv", header=T, na.strings=c(""), stringsAsFactors = F)

colnames(olympicCities)
str(olympicCities)

# Perform the join between the first 2 tables
olympicData <- merge(olympicData, olympicCities, 
                     by.x=c("Year"),
                     by.y=c("Year"),
                     all.x=TRUE)

colnames(olympicData )

col_order <- c("Year", "CountryCode", 
               "HostCity"   ,     "HostCountry"   ,  "HostCountryCode" ,  "Awarded.Gold" ,  "Awarded.Silver" , "Awarded.Bronze" , "Awarded.Total", 
               "Population" , "GDPpC" , "EmpRate" ,  
               "TotalEntries" ,"GoldMedals"  , "SilverMedals", "BronzeMedals" ,"TotalMedals") 

olympicData  <- as.data.frame(olympicData ) [, col_order]

################################################################################################
# 3. Populate Host column (Y/N) in OlympicData data set 
################################################################################################

olympicData$Host = ifelse (olympicData$CountryCode == olympicData$HostCountryCode , "Y", "N" )

################################################################################################
# 3. Populate pre=host2, pre-host1 and post-host1 post-host2 columns (Y/N) in OlympicData data set 
################################################################################################


# change to a factor