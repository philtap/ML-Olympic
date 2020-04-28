
#--------------------------------------------------------------------------------------------------
#   
#       DMML project  
#       Script    : DMML_ETL_main_dataset.R
#       Author    : Philippe Tap
#       Date      : April 2020
#
#--------------------------------------------------------------------------------------------------
# This script deals with the ETL of all data sources into our final data set(s)
# Merge all data sources 
#
#   Data                          Origin                                csv
#   Participation/medals          DMML_ETL_athletes_data.R              summer_entry_table.csv
#   Olympic info                  Generated manually                    olympic_cities.csv
#   Economic data                 DMML_ETL_economic_data.R              economic_table.csv
#   Additional Economic data      Generated manually                    missing_economic_data.csv
#   Economic data for 2020        From github plus manual changes       economic_data_2020.csv
#
#  It generates the 'olympic_data.csv' file, which will be the source data set for the 
#  Machine Learning analysis
#  The file contains full data  from 1992 to 2016 (incl. actual medals) and 2020 (without medals)
#---------------------------------------------------------------------------------------------------
# 
#

# Load required libraries 
library(stringr)
library(Amelia)
library(dplyr)
library (data.table)


################################################################################################
# 1. Join Results with Economics data
################################################################################################
results <- read.csv("data/summer_entry_table.csv", header=T, na.strings=c(""), stringsAsFactors = F)

economics <- read.csv("data/economic_table.csv", header=T, na.strings=c(""), stringsAsFactors = F)

sapply(economics,function(x) sum(is.na(x))) 
sapply(results,function(x) sum(is.na(x))) 

str(results)
str(economics)

colnames(results )
colnames(economics )

# Perform the join between the first 2 data frames
olympicData <- merge(results, economics, 
                   by.x=c("Year", "ISOCode"),
                   by.y=c("OlympicYear", "ISOCode"),
                   all.x=TRUE)

colnames(olympicData )

# Re-oder the columns
col_order <- c("Year","Country" , "CountryCode", "ISOCode", "Population" , "GDPpC" , "TotalEntries" ,"GoldMedals"  , "SilverMedals", "BronzeMedals" ,"TotalMedals") 
olympicData  <- as.data.frame(olympicData ) [, col_order]

colnames(olympicData )
str (olympicData)

################################################
# Missing data
################################################
# Find the NA values
sapply(olympicData,function(x) sum(is.na(x))) 

# country_na_counts = table(olympicData$Country,olympicData$Year)
# country_na_counts

country_na = olympicData [ is.na(olympicData$Country), ]
country_na 
country_na_counts = table(country_na$Year)
country_na_counts
barplot(country_na_counts, main = "NA Country per Year", xlab = "Year" , ylab = "Count", ylim=c(0,200))


################################################################################################
# 2. Join current OlympicData data set with Olympic characteristics 
################################################################################################

olympicCities <- read.csv("data/olympic_cities.csv", header=T, na.strings=c(""), stringsAsFactors = F)

colnames(olympicCities)
str(olympicCities)
# "Year"               "HostCity"           "HostCountry"        "HostCountryCode"    "Held"               
# "Awarded.Gold"       "Awarded.Silver"    "Awarded.Bronze"     "Awarded.Total"      
# "pre.host.country2"  "pre.host.country"   "post.host.country"  "post.host.country2"

# Perform the join between the main data set and the Olympic games characteristics
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
# 3. Populate Host column (Y/N) for each country in OlympicData data set based on HostCountryCode
################################################################################################

olympicData$Host = ifelse (olympicData$CountryCode == olympicData$HostCountryCode , "Y", "N" )

###################################################################################################
# 4. Populate pre=host2, pre-host1 and post-host1 post-host2 columns (Y/N) in OlympicData data set 
###################################################################################################

olympicData$pre_host2 = ifelse (olympicData$CountryCode == olympicData$pre.host.country2 , "Y", "N" )
olympicData$pre_host = ifelse (olympicData$CountryCode == olympicData$pre.host.country , "Y", "N" )
olympicData$post_host = ifelse (olympicData$CountryCode == olympicData$post.host.country , "Y", "N" )
olympicData$post_host2 = ifelse (olympicData$CountryCode == olympicData$post.host.country2 , "Y", "N" )

colnames(olympicData)

# Re-order the columns 
col_order <- c("Year", "CountryCode", 
               "HostCity"   ,     "HostCountry"   ,  "HostCountryCode" ,  "Awarded.Gold" ,  "Awarded.Silver" , "Awarded.Bronze" , "Awarded.Total",
               "Population" , "GDPpC" , 
               "pre_host2" ,  "pre_host"  ,     "Host"      ,   "post_host" ,  "post_host2"   ,     
               "TotalEntries" ,"GoldMedals"  , "SilverMedals", "BronzeMedals" ,"TotalMedals") 

olympicData  <- as.data.frame(olympicData ) [, col_order]
                              
# Review data and change relevant columns to  a factor- may not be needed

str(olympicData)


#######################################################################
#  5. Insert 2020 data 
#######################################################################

# Set up the participants for 2020 as the same as in 2016 
# Initialise the number of entries the same as in 2016

# result2020 = olympicData [Year == 2016, c("Year", "CountryCode","TotalEntries")  ]
result2020 = olympicData [olympicData$Year == 2016, c("Year", "CountryCode","TotalEntries")  ]
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

economics2020 <- read.csv("data/economic_data_2020.csv", header=T, na.strings=c(""), stringsAsFactors = F)

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
# olympicData = olympicData [olympicData$Year >=  "1988" ,]


###################################################################
# 6. Add and populate previous results columns to the data set
###################################################################

# Temporary Data set with results only 
olympicDataPreResults = olympicData [  , c("Year", "CountryCode", "GoldMedals" ,"TotalMedals")]

# Rename columns
names(olympicDataPreResults)[names(olympicDataPreResults) == "GoldMedals"] <- "gold_pre1"
names(olympicDataPreResults)[names(olympicDataPreResults) == "TotalMedals"] <- "med_pre1"

# Move the results to the pre-result columns for the next Olympic
olympicDataPreResults$Year = olympicDataPreResults$Year + 4

# Merge theese columns to the main data set
olympicData <- merge(olympicData, olympicDataPreResults, 
                     by.x=c("Year", "CountryCode"),
                     by.y=c("Year", "CountryCode"),
                     all.x=TRUE)

# Temporary Data set with results only 
olympicDataPreResults2 = olympicData [  , c("Year", "CountryCode", "GoldMedals" ,"TotalMedals")]

# Rename columns
names(olympicDataPreResults2)[names(olympicDataPreResults2) == "GoldMedals"] <- "gold_pre2"
names(olympicDataPreResults2)[names(olympicDataPreResults2) == "TotalMedals"] <- "med_pre2"

# Move the results to the pre-result columns for the second next Olympic
olympicDataPreResults2$Year = olympicDataPreResults2$Year + 8

olympicData <- merge(olympicData, olympicDataPreResults2, 
                     by.x=c("Year", "CountryCode"),
                     by.y=c("Year", "CountryCode"),
                     all.x=TRUE)

# Play with a separate data set with results only
olympicDataPreResults3 = olympicData [  , c("Year", "CountryCode", "GoldMedals" ,"TotalMedals")]

# Rename columns
names(olympicDataPreResults3)[names(olympicDataPreResults3) == "GoldMedals"] <- "gold_pre3"
names(olympicDataPreResults3)[names(olympicDataPreResults3) == "TotalMedals"] <- "med_pre3"

# Move the results to the pre-result columns for the third next Olympic
olympicDataPreResults3$Year = olympicDataPreResults3$Year + 12

olympicData <- merge(olympicData, olympicDataPreResults3, 
                     by.x=c("Year", "CountryCode"),
                     by.y=c("Year", "CountryCode"),
                     all.x=TRUE)

############################################################################
# Now that previous results are populated , reduce data set to 1992 - 2020
###########################################################################

colnames(olympicData)

olympicData = olympicData [olympicData$Year >=1992 ,   ]

###############################################################################################
#  Missing values , Data cleanup
###############################################################################################

# Missing values vs observed 
# missmap(olympicData, main = "Missing values vs observed")

#missing values
sapply(olympicData,function(x) sum(is.na(x)))

# Rows with NA Population 
population_na = olympicData[ is.na(olympicData$Population), c("Year", "CountryCode","Population" , "GDPpC") ]

# Rows with NA Population per year
population_na_counts = table(population_na$Year)
population_na_counts
# 1992 1996 2000 2004 2008 2012 2016 2020 
# 23   24   27   29   30   30   33    8 

barplot(population_na_counts, main = "NA Population per Year", xlab = "Year" , ylab = "Count")

# Rows with NA Population per country
population_na_cou_counts = table(population_na$CountryCode)
population_na_cou_counts

########################################################
# 7. Populate some of the economics missing data
#######################################################

# Load a file containing some of the missing data to a temporary data set
missing_economics <- read.csv("data/missing_economic_data.csv", header=T, na.strings=c(""), stringsAsFactors = F)

# Change data types 
str(missing_economics)
missing_economics$Population =    as.numeric(missing_economics$Population)
missing_economics$GDP  = as.numeric(missing_economics$GDP)
missing_economics$GDPpC =  as.numeric(missing_economics$GDPpC) 

str(missing_economics)
colnames(missing_economics)

# Add the GDPpC column
missing_economics$GDPpC = missing_economics$GDP / missing_economics$Population

# Rename the columns before merge with main data set, to avoid overwriting existing data 
names(missing_economics)[names(missing_economics) == "Population"] <- "MissPopulation"
names(missing_economics)[names(missing_economics) == "GDPpC"] <- "MissGDPpC"

# Reduce the number of columns in temporary data set 
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

colnames(olympicData)

# Remove the duplicate columns eand re-order

col_order <- c("Year", "CountryCode", 
               "HostCity"   ,     "HostCountry"   ,  "HostCountryCode" , 
               "Awarded.Gold" ,  "Awarded.Silver" , "Awarded.Bronze" , "Awarded.Total",
               "Population" , "GDPpC" , 
               "pre_host2" ,  "pre_host"  , "Host" , "post_host" ,  "post_host2"  ,     
                "gold_pre1" ,  "med_pre1" ,  "gold_pre2",  "med_pre2" , "gold_pre3" , "med_pre3" ,
               "TotalEntries" ,"GoldMedals"  , "SilverMedals", "BronzeMedals" ,"TotalMedals") 

olympicData  <- as.data.frame(olympicData ) [, col_order]


# Missing values vs observed 
#missmap(olympicData, main = "Missing values vs observed")


##################################################################
# 8. This is the final version of the Olympic data set
# Save as csv for further use
##################################################################

write.csv(olympicData,"data/olympic_data.csv", row.names = FALSE, quote=FALSE)


###############################################################################################
#  Economic Missing values - review again
###############################################################################################

population_gdppc_na = olympicData[ is.na(olympicData$Population) | is.na(olympicData$GDPpC), c("Year", "CountryCode","Population" , "GDPpC") ]

population_gdppc_na$GDP <- NA

colnames(population_gdppc_na)

population_gdppc_na_counts = table(population_gdppc_na$Year)
population_gdppc_na_counts
barplot(population_gdppc_na_counts, main = "NA Population/GDPpC per year", xlab = "Year" , ylab = "Count", ylim=c(0,20))

col_order <- c( "Year" ,"CountryCode", "Population" ,   "GDP",  "GDPpC"  ) 

population_gdppc_na  <- as.data.frame(population_gdppc_na ) [, col_order]
population_gdppc_na

# Ouput the remaining missing data  
write.csv(population_gdppc_na,"data/economic_missing_data2.csv", row.names = FALSE, quote=FALSE)



