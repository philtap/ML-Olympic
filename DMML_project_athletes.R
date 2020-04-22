#-----------------------------------------------------------------------
#                 DIA project 
#           DMML_project_athletes.R
#-----------------------------------------------------------------------
# This script deals with the ETL of the athlete_events.csv Olympic athletes file

# Load required libraries 
library(stringr)
library(Amelia)
library(dplyr)
library(data.table)

########################################################################
#                  Read the dataset
########################################################################

getwd()

# TO DO : parameterise the folder?

setwd("C:/Users/philippet/OneDrive - Datalex/NCIRL/Modules/Data mining and Machine learning I/Project/Datasets/")


# Note is 'fread' more efficient than read.csv

athleteData <- read.csv("athlete_events.csv", header=T, na.strings=c(""), stringsAsFactors = F)

# Summary of the data
summary(athleteData)
str(athleteData)
athleteData

names(athleteData)

####################################################################
# Restrict data to 
# - Summer Olympics
# - meaningful columns
####################################################################

# If start with 1992 , 1984 is enough here to get previous 2 results 
# If start with 1988 , 1980 is enough here to get previous 2 results 

athleteSummerData = athleteData [athleteData$Season == 'Summer' & athleteData$Year >=  "1976" ,c("ID", "Name" ,"Sex", "Team","NOC","Year","Sport","Event","Medal" )]   
summary(athleteSummerData)
str(athleteSummerData)

# Rename NOC column to Country
names(athleteSummerData)[names(athleteSummerData) == "NOC"] <- "Country"
athleteSummerData

names(athleteSummerData)


##################################################################
#  Convert Medal and Sex to factors
##################################################################
athleteSummerData$Medal  = as.factor (athleteSummerData$Medal)
athleteSummerData$Medal <- ordered(athleteSummerData$Medal , levels = c("NA","Bronze", "Silver", "Gold"))

athleteSummerData$Sex  = as.factor (athleteSummerData$Sex)

str(athleteSummerData)

########################################################################
#                  DATA CLEANUP 
########################################################################

###################################################################
# First, investigate any NA values 
##################################################################

# No NA values per column
sapply(athleteSummerData,function(x) sum(is.na(x)))

# Double check this - Medal = NA for athletes without medal
athleteSummerData[is.na(athleteSummerData$Medal), ]

# Missing values vs observed 
missmap(athleteSummerData, main = "Missing values vs observed")

#
# IMPORTANT : The medal data is not NA is R but "NA" 
# So is.na functions , see above, do not return data 

# No 'NA' in most columns
sum( athleteSummerData$Name == 'NA')
sum (athleteSummerData$Sex == 'NA')
sum( athleteSummerData$Team == 'NA')
sum( athleteSummerData$Country == 'NA')
sum (athleteSummerData$Year == 'NA')
sum (athleteSummerData$Sport == 'NA')
sum( athleteSummerData$Event == 'NA')

# As expected, 'NA' is a value for medal, meaning the athlete did not get a medal 
# No action needed
sum (athleteSummerData$Medal == 'NA')


###################################################################
# IMPORTANT : Ensure no ',' inside the columns
##################################################################

grep (',' , athleteSummerData$Name )

# Find ',' in each of the columns
subset(athleteSummerData , grepl(",", athleteSummerData$Name) ) 
subset(athleteSummerData , grepl(",", athleteSummerData$Sex) )
subset(athleteSummerData , grepl(",", athleteSummerData$Team) ) 
subset(athleteSummerData , grepl(",", athleteSummerData$Country) )
subset(athleteSummerData , grepl(",", athleteSummerData$Year) )
subset(athleteSummerData , grepl(",", athleteSummerData$Sport) )
subset(athleteSummerData , grepl(",", athleteSummerData$Event) ) 
subset(athleteSummerData , grepl(",", athleteSummerData$Medal) )

# Remove all ',' in Name, Team, Event, 
athleteSummerData$Name<- str_remove_all(athleteSummerData$Name, ",")
athleteSummerData$Team<- str_remove_all(athleteSummerData$Team, ",")
athleteSummerData$Event<- str_remove_all(athleteSummerData$Event, ",")

###################################################################
# Order and Save the data frame 
###################################################################

# Order the data frame

athleteSummerData = athleteSummerData[with (athleteSummerData, order (Year,Sport,Event,Medal,Country) ),]

athleteSummerData


########################################################################################
#   DATA TRANSFORMATION 
########################################################################################

# Transform data frame in a table to allow for aggregation

athleteTable <- as.data.table(athleteSummerData)


# In order to count the unique entries and medals for a country, we need 
# to determine if an athlete's entry in individual or part of a team entry
# Lets's mark each row as Indvidual or Team based on the event 

# Initialise all rows as Individual, we will overwrite this in case of a 
# Team event 
athleteTable$EventType <- "Individual"

# The following classification was established after research on Wikipedia

athleteTable$EventType [athleteTable$Event %like% "Team" ] =  "Team"
athleteTable$EventType [athleteTable$Event %like% "4 X" ] =  "Team"
athleteTable$EventType [athleteTable$Event %like% "Relay" ] = "Team"
athleteTable$EventType [athleteTable$Event %like% "Group" ] = "Team"
athleteTable$EventType [athleteTable$Event %like% "Duet" ] = "Team"
athleteTable$EventType [athleteTable$Event %like% "Pairs" ] = "Team"
athleteTable$EventType [athleteTable$Event %like% "Doubles" ] = "Team"
athleteTable$EventType [athleteTable$Event %like% "Double Sculls" ] = "Team"
athleteTable$EventType [athleteTable$Event %like% "Two Person" ] = "Team"
athleteTable$EventType [athleteTable$Event %like% "Three Person" ] = "Team"
athleteTable$EventType [athleteTable$Event %like% "Fours" ] =  "Team"
athleteTable$EventType [athleteTable$Event %like% "Quadruple" ] = "Team"
athleteTable$EventType [athleteTable$Event %like% "Eights" ] =  "Team"
athleteTable$EventType [athleteTable$Event %like% "Relay" ] = "Team"
athleteTable$EventType [athleteTable$Event %like% "Madison" ] = "Team"	
athleteTable$EventType [athleteTable$Event %like% "Synchronized Platform" ] = "Team"
athleteTable$EventType [athleteTable$Event %like% "Synchronized Springboard" ] = "Team"
athleteTable$EventType [athleteTable$Event %like% "Multihull" ] = "Team"
athleteTable$EventType [athleteTable$Event %like% "Baseball" ] = "Team"
athleteTable$EventType [athleteTable$Event %like% "Basketball" ] = "Team"
athleteTable$EventType [athleteTable$Event %like% "Football" ] = "Team"
athleteTable$EventType [athleteTable$Event %like% "Handball" ] = "Team"
athleteTable$EventType [athleteTable$Event %like% "Hockey" ] = "Team"
athleteTable$EventType [athleteTable$Event %like% "Volleyball" ] = "Team"
athleteTable$EventType [athleteTable$Event %like% "Water Polo" ] = "Team"
athleteTable$EventType [athleteTable$Event %like% "Rugby" ] = "Team"
athleteTable$EventType [athleteTable$Event %like% "Softball" ] = "Team"

# To help with count the unique entries and medals for a country,
# let's add an Entry Name column
#  - for individual events: name of the athlete
#  - for teams : the team name
#         - the country name: France
#         - in case of multiple enties for an event: France 1, France 2

athleteTable$EntryName <- "NA"

athleteTable$EntryName = ifelse(athleteTable$EventType == "Team", athleteTable$Team, athleteTable$Name)

########################################################################
# Save to clean transformed data to a csv file
# Used for the DIA project
# Note: file is saved with quotes for now and can be edited manually in Notepad ++ to remove the quotes.
########################################################################

write.csv(athleteTable,"summer_athletes_wquotes.csv", row.names = FALSE, quote=FALSE)

#########################################################################
# Aggregation 
#########################################################################

# Transform data table to a table of disctinct medals
# Count only one medal for each event, medal colour and country 

# 1. Table of Distinct entries , i.e a participating team (e.g BasketBall) counts as 1 entry per country
# 2 separate athletes for the same country in an Individual event count as one entry each  

Entries = athleteTable [   , 
                           .( distinct_entry = length(EntryName)  ), 
                                   by = .(  Year, Country, EventType , Event, EntryName, Medal )  ]

# Temp
EntriesGold   =  Entries [Medal == "Gold"]
EntriesSilver =  Entries [Medal == "Silver"] 
EntriesBronze =  Entries [Medal == "Bronze" ]


# 2. Create 4 tables to ggregate data per year and country
# - one aggregating participation 
# - 3 aggregating medals: Gold, Silver, Bronze 

TableOfParticipation= Entries[, .( TotalEntries = sum(distinct_entry) )  ,  by= .(  Year, Country )  ]
TableOfGold =    Entries[Medal == "Gold" , .( GoldMedals = sum(distinct_entry) )  ,  by= .(  Year, Country )  ]
TableOfSilver =  Entries [Medal == "Silver" , .( SilverMedals = sum(distinct_entry) )  ,  by= .(  Year, Country )  ]
TableOfBronze =  Entries [Medal == "Bronze" , .( BronzeMedals = sum(distinct_entry) )  ,  by= .(  Year, Country )  ]


#  3. Join the participants and medals tables 

# Set the ON clause as keys of the tables
setkey(TableOfParticipation, Year, Country)
setkey(TableOfGold,Year, Country)
setkey(TableOfSilver,Year, Country)
setkey(TableOfBronze,Year, Country)

# Perform the join between the first 2 tables
Temp <- merge(TableOfParticipation, TableOfGold, all=TRUE)

# Join result with the 3rd table 
Temp2 <- merge( Temp, TableOfSilver, all=TRUE)

# Join result with 4th table
medalTable <- merge( Temp2, TableOfBronze, all=TRUE)

# Find the NA values
sapply(medalTable,function(x) sum(is.na(x))) 


# Replace NA by 0 everywhere
medalTable$GoldMedals [is.na(medalTable$GoldMedals)] <- 0;
medalTable$SilverMedals [is.na(medalTable$SilverMedals)] <- 0;
medalTable$BronzeMedals [is.na(medalTable$BronzeMedals)] <- 0;


# Find the NA values
sapply(medalTable,function(x) sum(is.na(x))) 
# No NA values !

# Add a column: Total medals: sum of the 3 medal  columns 
medalTable$TotalMedals = medalTable$GoldMedals + medalTable$SilverMedals + medalTable$BronzeMedals
medalTable

# Compare this table in detail with 
# https://en.wikipedia.org/wiki/2012_Summer_Olympics_medal_table
# USA , CHN slight difference, RUS large diff 


keycol <-c( "Year",  "Country")
setorderv(medalTable, keycol)
medalTable


###################################################################################
# Adding ISO country codes which will be needed when joining with Economic data
###################################################################################

# First rename the Country column in medalData to avoid confusion with Country column in countryData
names(medalTable)[names(medalTable) == "Country"] <- "CountryCode"

countryData <- read.csv("country_codes.csv", header=T, na.strings=c(""), stringsAsFactors = F)



medalTable <- merge(medalTable, countryData, 
                      by.x=c("CountryCode"),
                      by.y=c("IOC"),
                      all.x=TRUE)

colnames(medalTable)


medalTable [ is.na(medalTable$ISO) ]
#  CountryCode Year TotalEntries GoldMedals SilverMedals BronzeMedals TotalMedals Country  ISO
#   1:         EUN 1992          421         45           38           29         112    <NA> <NA>
#   2:         IOA 1992           68          0            1            2           3    <NA> <NA>
#   3:         IOA 2000            4          0            0            0           0    <NA> <NA>
#   4:         IOA 2012            4          0            0            0           0    <NA> <NA>
#   5:         IOA 2016            9          1            0            1           2    <NA> <NA>
#   6:         LIB 1976            4          0            0            0           0    <NA> <NA>
#   7:         LIB 1980           19          0            0            1           1    <NA> <NA>
#   8:         LIB 1984           27          0            0            0           0    <NA> <NA>
#   9:         LIB 1988           31          0            0            0           0    <NA> <NA>
#   10:         LIB 1992           18          0            0            0           0    <NA> <NA>
#   11:         LIB 1996            1          0            0            0           0    <NA> <NA>
#   12:         LIB 2000            6          0            0            0           0    <NA> <NA>
#   13:         LIB 2004            5          0            0            0           0    <NA> <NA>
#   14:         LIB 2008            6          0            0            0           0    <NA> <NA>
#   15:         LIB 2012           10          0            0            0           0    <NA> <NA>
#   16:         LIB 2016            9          0            0            0           0    <NA> <NA>
#   17:         ROT 2016           12          0            0            0           0    <NA> <NA>


# Remove all ',' in Name, Team, Event, 
medalTable$Country<- str_remove_all(medalTable$Country, ",")

subset(medalTable , grepl(",", medalTable$Country) )

# Rename the country code for consistency
names(medalTable)[names(medalTable) == "ISO"] <- "ISOCode"

colnames(medalTable)

col_order <- c("Year","CountryCode", "ISOCode", "TotalEntries" ,"GoldMedals"  , "SilverMedals", "BronzeMedals" ,"TotalMedals") 

medalTable <- as.data.frame(medalTable) [, col_order]



##################################################################
# This is the final version of the entry/medal table
# Save as csv
##################################################################

write.csv(medalTable,"summer_entry_table.csv", row.names = FALSE, quote=FALSE)

