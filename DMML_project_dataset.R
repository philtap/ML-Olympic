

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


# Set the current directory

setwd("C:/Users/philippet/OneDrive - Datalex/NCIRL/Modules/Data mining and Machine learning I/Project/Datasets/")


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

col_order <- c("Year","Country" , "CountryCode", "ISOCode", "Population" , "GDPpC" , "TotalEntries" ,"GoldMedals"  , "SilverMedals", "BronzeMedals" ,"TotalMedals") 

olympicData  <- as.data.frame(olympicData ) [, col_order]

colnames(olympicData )

str (olympicData)

# Find the NA values
sapply(olympicData,function(x) sum(is.na(x))) 

# country_na_counts = table(olympicData$Country,olympicData$Year)
# country_na_counts

country_na = olympicData [ is.na(olympicData$Country), ]
country_na 
country_na_counts = table(country_na $Year)
country_na_counts
barplot(country_na_counts, main = "NAs per Year (Country)", xlab = "Year" , ylab = "Count", ylim=c(0,200))

country_na = olympicData [ is.na(olympicData$Country), ]
country_na



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
               "Population" , "GDPpC" ,  "pre.host.country" ,  "pre.host.country2",  "post.host.country",  "post.host.country2",  
               "TotalEntries" ,"GoldMedals"  , "SilverMedals", "BronzeMedals" ,"TotalMedals") 

olympicData  <- as.data.frame(olympicData ) [, col_order]

################################################################################################
# 3. Populate Host column (Y/N) in OlympicData data set 
################################################################################################

olympicData$Host = ifelse (olympicData$CountryCode == olympicData$HostCountryCode , "Y", "N" )

################################################################################################
# 4. Populate pre=host2, pre-host1 and post-host1 post-host2 columns (Y/N) in OlympicData data set 
################################################################################################

olympicData$pre_host2 = ifelse (olympicData$CountryCode == olympicData$pre.host.country2 , "Y", "N" )
olympicData$pre_host = ifelse (olympicData$CountryCode == olympicData$pre.host.country , "Y", "N" )
olympicData$post_host = ifelse (olympicData$CountryCode == olympicData$post.host.country , "Y", "N" )
olympicData$post_host2 = ifelse (olympicData$CountryCode == olympicData$post.host.country2 , "Y", "N" )

colnames(olympicData)

col_order <- c("Year", "CountryCode", 
               "HostCity"   ,     "HostCountry"   ,  "HostCountryCode" ,  "Awarded.Gold" ,  "Awarded.Silver" , "Awarded.Bronze" , "Awarded.Total",
               "Population" , "GDPpC" , 
               "pre_host2" ,  "pre_host"  ,     "Host"      ,   "post_host" ,  "post_host2"   ,     
               "TotalEntries" ,"GoldMedals"  , "SilverMedals", "BronzeMedals" ,"TotalMedals") 

olympicData  <- as.data.frame(olympicData ) [, col_order]
                              
# Review data and change relevant columns to  a factor






##################################################################
#  Insert 2020 data - Should be 2021! with economic data form 2020
##################################################################

# Set up the participants for 2020 as the same as in 2016 
# Initialise the number of entries the the same as in 2016

result2020 = olympicData [Year == 2016, c("Year", "CountryCode","TotalEntries")  ]
result2020$Year = 2020

olympicData2020 = result2020 

str(olympicData2020)

# Set up the Host city data 
olympicData2020$HostCity = olympicCities$HostCity[olympicCities$Year == 2020 ]
olympicData2020$HostCountry =olympicCities$HostCountry[olympicCities$Year == 2020 ]
olympicData2020$HostCountryCode =olympicCities$HostCountryCode[olympicCities$Year == 2020 ]
olympicData2020$Awarded.Gold = olympicCities$Awarded.Gold[olympicCities$Year == 2020 ]
olympicData2020$Awarded.Silver =olympicCities$Awarded.Silver[olympicCities$Year == 2020 ]
olympicData2020$Awarded.Bronze =olympicCities$Awarded.Bronze[olympicCities$Year == 2020 ]
olympicData2020$Awarded.Total =olympicCities$Awarded.Total[olympicCities$Year == 2020 ]

colnames(olympicData2020)

#  Populate Host column (Y/N) in OlympicData data set 
olympicData2020$Host = ifelse (olympicData2020$CountryCode == olympicData2020$HostCountryCode , "Y", "N" )

# Populate pre-host2, pre-host1 and post-host1 post-host2 columns (Y/N) in OlympicData data set

# First initialise the country code of the prehosts and post hosts
olympicData2020$pre.host.country2 = olympicCities$pre.host.country2[olympicCities$Year == 2020 ]
olympicData2020$pre.host.country = olympicCities$pre.host.country[olympicCities$Year == 2020 ]
olympicData2020$post.host.country = olympicCities$post.host.country[olympicCities$Year == 2020 ]
olympicData2020$post.host.country2 = olympicCities$post.host.country2[olympicCities$Year == 2020 ]

# Use this information to determine if the partcipating country is  host, pre host or post host
olympicData2020$pre_host2 = ifelse (olympicData2020$CountryCode == olympicData2020$pre.host.country2 , "Y", "N" )
olympicData2020$pre_host = ifelse (olympicData2020$CountryCode == olympicData2020$pre.host.country , "Y", "N" )
olympicData2020$post_host = ifelse (olympicData2020$CountryCode == olympicData2020$post.host.country , "Y", "N" )
olympicData2020$post_host2 = ifelse (olympicData2020$CountryCode == olympicData2020$post.host.country2 , "Y", "N" )


# Set the results to NA 
olympicData2020$GoldMedals <- NA
olympicData2020$SilverMedals <- NA
olympicData2020$BronzeMedals <- NA
olympicData2020$TotalMedals <- NA

colnames(olympicData2020)

# Add the economic data for 2020

sapply(olympicData2020,function(x) sum(is.na(x))) 

economics2020 <- read.csv("economic_data_2020.csv", header=T, na.strings=c(""), stringsAsFactors = F)

col_order <- c( "country"   ,   "IOC"       ,   "ISO" , "gdpPerCapita", "pop"  ) 

economics2020  <- as.data.frame(economics2020 ) [, col_order]
colnames(economics2020)

head(economics2020)

olympicData2020 <- merge(olympicData2020, economics2020, 
                     by.x=c( "CountryCode"),
                     by.y=c("IOC"),
                     all.x=TRUE)

# Rename the columns
names(olympicData2020)[names(olympicData2020) == "pop"] <- "Population"
names(olympicData2020)[names(olympicData2020) == "gdpPerCapita"] <- "GDPpC"


colnames(olympicData2020)
# [1] "CountryCode"        "Year"               "TotalEntries"       "HostCity"           "HostCountry"       
# [6] "HostCountryCode"    "Awarded.Gold"       "Awarded.Silver"     "Awarded.Bronze"     "Awarded.Total"     
# [11] "Host"               "pre.host.country2"  "pre.host.country"   "post.host.country"  "post.host.country2"
# [16] "pre_host2"          "pre_host"           "post_host"          "post_host2"         "GoldMedals"        
# [21] "SilverMedals"       "BronzeMedals"       "TotalMedals"        "country"            "ISO"               
# [26] "GDPpC"              "Population"    

col_order <- c("Year", "CountryCode", 
               "HostCity"   ,     "HostCountry"   ,  "HostCountryCode" ,  "Awarded.Gold" ,  "Awarded.Silver" , "Awarded.Bronze" , "Awarded.Total",
               "Population" , "GDPpC" , 
               "pre_host2" ,  "pre_host"  ,     "Host"      ,   "post_host" ,  "post_host2"   ,     
               "TotalEntries" ,"GoldMedals"  , "SilverMedals", "BronzeMedals" ,"TotalMedals") 

olympicData2020  <- as.data.frame(olympicData2020 ) [, col_order]



#   verified 2020 data set has same columns as main data set 

#colnames(olympicData)
#colnames(olympicData2020)

# Combine 2020 with the main olympic Data set
olympicData = rbind(olympicData,olympicData2020)


# Limit the period to 1992 - 2020
olympicData = olympicData [olympicData$Year >=  "1992" ,]



##################################################################
# This is the final version of the entry/medal table
# Save as csv
##################################################################

write.csv(olympicData,"olympic_data.csv", row.names = FALSE, quote=FALSE)


# TO DO NOW: Change all dates to 2021



###############################################################################################
#  Missing values , Data cleanup
###############################################################################################

####################################
#  TO DO NEXT
####################################

# Missing values vs observed 
missmap(olympicData, main = "Missing values vs observed")

#missing values
sapply(olympicData,function(x) sum(is.na(x)))

population_na = olympicData[ is.na(olympicData$Population), c("Year", "CountryCode","Population" , "GDPpC") ]

population_na_counts = table(population_na$Year)
population_na_counts
barplot(population_na_counts, main = "NAs per Year (Country)", xlab = "Year" , ylab = "Count", ylim=c(0,50))

population_na_cou_counts = table(population_na$CountryCode)
population_na_cou_counts
# Population      GDPpC       
#  205             205   


# CountryCode Population GDPpC
# 10          ASA         NA    NA
# 43          COK         NA    NA
# 81          GUM         NA    NA
# 89          IOA         NA    NA
# 95          ISV         NA    NA
# 106         KOS         NA    NA
# 114         LIB         NA    NA
# 159         ROT         NA    NA
# 183         SWZ         NA    NA

missing_economics <- read.csv("missing_economic_data.csv", header=T, na.strings=c(""), stringsAsFactors = F)

# Change data types
str(missing_economics)
missing_economics$Population =    as.numeric(missing_economics$Population)
missing_economics$GDP  = as.numeric(missing_economics$GDP)
missing_economics$GDPpC =  as.numeric(missing_economics$GDPpC) 

str(missing_economics)
colnames(missing_economics)

# Calculate the GDPpC
missing_economics$GDPpC = missing_economics$GDP / missing_economics$Population

# Rename the columns before merge with main data set
names(missing_economics)[names(missing_economics) == "Population"] <- "MissPopulation"
names(missing_economics)[names(missing_economics) == "GDPpC"] <- "MissGDPpC"

# Reduce the number of columns
col_order <- c( "Year"   ,"IOC.CountryCode"       ,  "MissPopulation"   , "MissGDPpC"   ) 
missing_economics  <- as.data.frame(missing_economics ) [, col_order]

# Merge the 2 columns to the main data set
olympicData <- merge(olympicData, missing_economics, 
                     by.x=c("Year", "CountryCode"),
                     by.y=c("Year", "IOC.CountryCode"),
                     all.x=TRUE)
# Replace the Population and GDPpC, if missing, by the data in the additional columns

olympicData$Population = ifelse(is.na(olympicData$Population), olympicData$MissPopulation, olympicData$Population)
olympicData$GDPpC = ifelse(is.na(olympicData$GDPpC), olympicData$MissGDPpC , olympicData$GDPpC)

olympicData <- select (olympicData,-c(MissPopulation,MissGDPpC ))

###############################################################################################
#  Missing values - review again
###############################################################################################

population_gdppc_na = olympicData[ is.na(olympicData$Population) | is.na(olympicData$GDPpC), c("Year", "CountryCode","Population" , "GDPpC") ]

population_gdppc_na$GDP <- NA

colnames(population_gdppc_na)

population_gdppc_na_counts = table(population_gdppc_na$Year)
population_gdppc_na_counts
barplot(population_gdppc_na_counts, main = "NAs per Year (Country)", xlab = "Year" , ylab = "Count", ylim=c(0,50))

col_order <- c( "Year" ,"CountryCode", "Population" ,   "GDP",  "GDPpC"  ) 

population_gdppc_na  <- as.data.frame(population_gdppc_na ) [, col_order]

# Ouput the remaining missing data 
write.csv(population_gdppc_na,"economic_missing_data2.csv", row.names = FALSE, quote=FALSE)

###################################################################
# Last step : TO DO
# Add previous results to the data set
###################################################################


# Play with a separate data set with results only
# olympicDataResults = olympicData [ , c("Year", "CountryCode", "GoldMedals"  , "SilverMedals", "BronzeMedals" ,"TotalMedals")]
# 
# olympicDataResults$med_pre3
# olympicDataResults$smed_pre2
# olympicDataResults$med_pre1  = olympicData$TotalMedals [ olympicData$Year == (olympicDataResults$Year -4) ]
# olympicDataResults$gold_pre3
# olympicDataResults$gold_pre2
# olympicDataResults$gold_pre1
# 
# # This is returning an error
# olympicDataResults$med_pre1 [Year ==2000] = olympicDataResults$TotalMedals  [Year ==1996]
# olympicDataResults$gold_pre1 [Year ==2000] = olympicDataResults$GoldMedals  [Year ==1996]
# In olympicDataResults$med_pre1[Year == 2000] = olympicDataResults$TotalMedals[Year ==  :
#  number of items to replace is not a multiple of replacement length
