#-----------------------------------------------------------------------
#                 DMML_project_medals.R
#-----------------------------------------------------------------------
# This script deals with the ETL of the summer.csv Olympic medal file

# TO DO : Add details
#

# Load required libraries 
library(stringr)
library(Amelia)
library(dplyr)
library (data.table)
# https://cran.r-project.org/web/packages/data.table/vignettes/datatable-intro.html


setwd("C:/Users/philippet/OneDrive - Datalex/NCIRL/Modules/Data mining and Machine learning I/Project/Datasets/medals")
# If you are a Windows user, remember to escape your \ s, as \ (as mentioned in lab 1) is a reserved character
# and so must be escaped. E.g.: C:\\some directory\\some other directory.

getwd()
# Now we are ready to read in our csv and put it in a data.frame.
medalData <- read.csv("summer.csv", header=T, na.strings=c(""), stringsAsFactors = T)

# Summary of the data
summary(medalData)
str(medalData)
medalData
names(medalData)

########################################################################
#       Convert variables 
########################################################################

# Convert Facors to character variables
medalData$Athlete = as.character(medalData$Athlete)
medalData$Discipline = as.character(medalData$Discipline)
medalData$Event = as.character(medalData$Event)
medalData$Sport = as.character(medalData$Sport)
medalData$City = as.character(medalData$City)
medalData$Country = as.character(medalData$Country)

# Order the levels for Medal
print(levels(medalData$Medal))
medalData$Medal <- ordered(medalData$Medal , levels = c("Bronze", "Silver", "Gold"))
print(levels(medalData$Medal))

str(medalData)

# $ Year      : int  1896 1896 1896 1896 1896 1896 1896 1896 1896 1896 ...
# $ City      : chr  "Athens" "Athens" "Athens" "Athens" ...
# $ Sport     : chr  "Aquatics" "Aquatics" "Aquatics" "Aquatics" ...
# $ Discipline: chr  "Swimming" "Swimming" "Swimming" "Swimming" ...
# $ Athlete   : chr  "HAJOS, Alfred" "HERSCHMANN, Otto" "DRIVAS, Dimitrios" "MALOKINIS, Ioannis" ...
# $ Country   : chr  "HUN" "AUT" "GRE" "GRE" ...
# $ Gender    : Factor w/ 2 levels "Men","Women": 1 1 1 1 1 1 1 1 1 1 ...
# $ Event     : chr  "100M Freestyle" "100M Freestyle" "100M Freestyle For Sailors" "100M Freestyle For Sailors" ...
# $ Medal     : Ord.factor w/ 3 levels "Bronze"<"Silver"<..: 3 2 1 3 2 1 3 2 1 3 ...


########################################################################
#                  DATA CLEANUP (missing values)
########################################################################


# Observe missing values


missmap(medalData, main = "Missing values vs observed")

# NA values per column
sapply(medalData,function(x) sum(is.na(x)))

medalData[is.na(medalData$Country), ]

# Year   City         Sport          Discipline         Athlete Country Gender    Event  Medal
# 29604 2012 London     Athletics           Athletics         Pending    <NA>  Women    1500M   Gold
# 31073 2012 London Weightlifting       Weightlifting         Pending    <NA>  Women     63KG   Gold
# 31092 2012 London Weightlifting       Weightlifting         Pending    <NA>    Men     94KG Silver
# 31111 2012 London     Wrestling Wrestling Freestyle KUDUKHOV, Besik    <NA>    Men Wf 60 KG Silver

medalData [medalData$Athlete == 'Pending', ]
#       Year   City         Sport    Discipline Athlete Country Gender Event  Medal
# 29604 2012 London     Athletics     Athletics Pending    <NA>  Women 1500M   Gold
# 31073 2012 London Weightlifting Weightlifting Pending    <NA>  Women  63KG   Gold
# 31092 2012 London Weightlifting Weightlifting Pending    <NA>    Men  94KG Silver

# Women 1500M
# https://en.wikipedia.org/wiki/Athletics_at_the_2012_Summer_Olympics_%E2%80%93_Women%27s_1500_metres
# These developments meant that six of the race's top nine finishers were linked to PED usage. The aforementioned ESPN story called the race "one of the dirtiest in Olympic history."[12]
# In 2017, the IOC officially reassigned the gold medal to Maryam Yusuf Jamal, but pending the outcome of anti-doping proceedings against several lower-placed finishers the silver and bronze remain vacant.
# In 2018, the IOC reallocated silver and bronze medals, upgrading Tomashova despite her doping suspension.[16]
# 
# Wrestling Freestyle 60 KG
# On 29 August 2016, a report indicated that a retested sample for silver medalist Besik Kudukhov taken at the time of this event 
# had returned a positive result (later disclosed as dehydrochlormethyltestosterone).
# [2] On 27 October 2016, the IOC stated that they were unaware that Kudukhov had died in a car accident in December 2013 
# at the time the decision to include his samples in the re-analysis process was made. 
# Since such proceedings cannot be conducted against a deceased person, the IOC dropped all disciplinary proceedings 
# against him. As a result, Olympic results that would most likely have been reviewed will remain uncorrected.[3]

#  How to deal with NA
# 1) Remove row 31166 

# medalData[is.na(medalData$Country), ]
# 
# medalData <- medalData[-c(31166), ]

# 2) Row 31111 -> RUS


medalData[medalData$Athlete=='KUDUKHOV, Besik' & medalData$Gender=='Men' & medalData$Year==2012 & medalData$Medal == 'Silver',]

medalData[medalData$Athlete=='KUDUKHOV, Besik' & medalData$Gender=='Men' & medalData$Year==2012 & medalData$Medal == 'Silver',] ['Country'] <- 'RUS'

medalData [medalData$Athlete == 'Pending', ]
sapply(medalData,function(x) sum(is.na(x)))

#3) 1500m Women 2012 
# Lots of doping in this race resulting in result changes

# Year   City         Sport          Discipline         Athlete Country Gender    Event  Medal
# 29604 2012 London     Athletics           Athletics         Pending    <NA>  Women    1500M   Gold

medalData[medalData$Event=='1500M' & medalData$Gender=='Women' & medalData$Year==2012,]
# Year   City     Sport Discipline             Athlete Country Gender Event  Medal
# 29604 2012 London Athletics  Athletics             Pending    <NA>  Women 1500M   Gold
# 29605 2012 London Athletics  Athletics        BULUT, Gamze     TUR  Women 1500M Silver
# 29606 2012 London Athletics  Athletics JAMAL, Maryam Yusuf     BRN  Women 1500M Bronze

# Real results
# 1st place, gold medalist(s)	Maryam Yusuf Jamal	 Bahrain	4:10.74	
# 2nd place, silver medalist(s)	Tatyana Tomashova	 Russia	4:10.90	
# 3rd place, bronze medalist(s)	Abeba Aregawi	 Ethiopia	4:11.03


medalData[medalData$Event=='1500M' & medalData$Gender=='Women' & medalData$Year==2012 & medalData$Medal == 'Gold' ,]['Athlete']  <- 'JAMAL, Maryam Yusuf'
medalData[medalData$Event=='1500M' & medalData$Gender=='Women' & medalData$Year==2012 & medalData$Medal == 'Gold' ,]['Country'] <- 'BRN'

medalData[medalData$Event=='1500M' & medalData$Gender=='Women' & medalData$Year==2012 & medalData$Medal == 'Silver' ,]['Athlete'] <- 'TOMASHOVA, Tatyana'
medalData[medalData$Event=='1500M' & medalData$Gender=='Women' & medalData$Year==2012 & medalData$Medal == 'Silver' ,]['Country'] <- 'RUS'

medalData[medalData$Event=='1500M' & medalData$Gender=='Women' & medalData$Year==2012 & medalData$Medal == 'Bronze' ,]['Athlete'] <- 'AREGAWI, Abeba'
medalData[medalData$Event=='1500M' & medalData$Gender=='Women' & medalData$Year==2012 & medalData$Medal == 'Bronze' ,]['Country'] <- 'ETH'

medalData[medalData$Event=='1500M' & medalData$Gender=='Women' & medalData$Year==2012,]

 
medalData [medalData$Athlete == 'Pending', ]
medalData[is.na(medalData$Country), ]

# 4) weighlifting
#       Year   City         Sport    Discipline Athlete Country Gender Event  Medal
# 31073 2012 London Weightlifting Weightlifting Pending    <NA>  Women  63KG   Gold
# 31092 2012 London Weightlifting Weightlifting Pending    <NA>    Men  94KG Silver

# 4.1 Women 

# Current 
medalData[medalData$Event=='63KG' & medalData$Gender=='Women' & medalData$Year==2012,]
# Year   City         Sport    Discipline              Athlete Country Gender Event  Medal
# 29933 2012 London Weightlifting Weightlifting              Pending    <NA>  Women  63KG   Gold
# 29934 2012 London Weightlifting Weightlifting TSARUKAEVA, Svetlana     RUS  Women  63KG Silver
# 29935 2012 London Weightlifting Weightlifting    GIRARD, Christine     CAN  Women  63KG Bronze

# Actual results
# 1st place, gold medalist(s)	 Christine Girard (CAN)	A	62.87	103	105	105	103	130	133	135	133	236
# 2nd place, silver medalist(s)	 Milka Maneva (BUL)	A	62.66	98	102	105	102	125	131	134	131	233
# 3rd place, bronze medalist(s)	 Luz Acosta (MEX)	A	62.91	99	103	104	99	119	125	127	125	224
# NA values per column after cleanup

medalData[medalData$Event=='63KG' & medalData$Gender=='Women' & medalData$Year==2012 & medalData$Medal == 'Gold',]['Athlete'] <- 'GIRARD, Christine'
medalData[medalData$Event=='63KG' & medalData$Gender=='Women' & medalData$Year==2012 & medalData$Medal == 'Gold',] ['Country'] <- 'CAN'

medalData[medalData$Event=='63KG' & medalData$Gender=='Women' & medalData$Year==2012 & medalData$Medal == 'Silver',]['Athlete'] <- 'MANEVA, Milka'
medalData[medalData$Event=='63KG' & medalData$Gender=='Women' & medalData$Year==2012 & medalData$Medal == 'Silver',]['Country'] <- 'BUL'

medalData[medalData$Event=='63KG' & medalData$Gender=='Women' & medalData$Year==2012 & medalData$Medal == 'Bronze',]['Athlete'] <- 'ACOSTA, Luz'
medalData[medalData$Event=='63KG' & medalData$Gender=='Women' & medalData$Year==2012 & medalData$Medal == 'Bronze',]['Country'] <- 'MEX'


medalData[medalData$Event=='63KG' & medalData$Gender=='Women' & medalData$Year==2012,]

# 4.2 Men
# Current 
medalData[medalData$Event=='94KG' & medalData$Gender=='Men' & medalData$Year==2012,]
# Year   City         Sport    Discipline                      Athlete Country Gender Event  Medal
# 29951 2012 London Weightlifting Weightlifting MOHAMMADPOURKARKARAGH, Saeid     IRI    Men  94KG   Gold
# 29952 2012 London Weightlifting Weightlifting                      Pending    <NA>    Men  94KG Silver
# 29953 2012 London Weightlifting Weightlifting                  KIM, Minjae     KOR    Men  94KG Bronze
# Actual results
# 1st place, gold medalist(s)	 Saeid Mohammadpour (IRI)		
# 2nd place, silver medalist(s)	 Kim Min-jae (KOR)	A	
# 3rd place, bronze medalist(s)	 Tomasz Zielinski (POL)	B	

# TO DO - insert a row as no row exists for Silver
medalData[medalData$Event=='94KG' & medalData$Gender=='Men' & medalData$Year==2012 & medalData$Medal == 'Silver' ,]['Athlete'] <- 'KIM, Minjae'
medalData[medalData$Event=='94KG' & medalData$Gender=='Men' & medalData$Year==2012 & medalData$Medal == 'Silver' ,]['Country'] <- 'KOR'

medalData[medalData$Event=='94KG' & medalData$Gender=='Men' & medalData$Year==2012 & medalData$Medal == 'Bronze' ,]['Athlete'] <- 'ZIELINSKI,  Tomasz'
medalData[medalData$Event=='94KG' & medalData$Gender=='Men' & medalData$Year==2012 & medalData$Medal == 'Bronze' ,]['Country'] <- 'POL'


sapply(medalData,function(x) sum(is.na(x)))
medalData[is.na(medalData$Country), ]
medalData [medalData$Athlete == 'Pending', ]

write.csv(medalData,"summer_clean.csv", row.names = FALSE, quote=FALSE)


########################################################################
#                  DATA CLEANUP (data anomalies)
########################################################################

# The following entry causes issues with csv, correct the Athlete name
# 1900,Paris,Rugby,Rugby,BINOCHE Jean, LÃ©on,FRA,Men,Rugby,Gold,Rugby Rugby Rugby,Men

medalData [medalData$Athlete == 'BINOCHE Jean, LÃfÂ©on', ]

medalData[25036, ]['Athlete'] <- 'BINOCHE Jean'
medalData [medalData$Athlete == 'BINOCHE Jean', ]

# Issue with the event: Epée - it is 'mispelled' Ã???pÃ©e, which causes issues 


medalData [medalData$Sport == 'Fencing', ]

# Show the rows causing an issue 
grep ("Ã???pÃ©e" , medalData$Event )
grep ("Ã???pÃ©e" , medalData$Event )

# Here is one of the rows causing an issue 
str_detect(medalData[30144, ]['Event'], "Ã???pÃ©e") 
medalData[30144, ]

medalData$Event <- str_replace_all(medalData$Event, "Ã???pÃ©e", "Epee")


# Verify if changed
grep ("Ã???pÃ©e" , medalData$Event )
medalData[30144, ]


########################################################################################
#                   DATA CLEANUP - formatting issues
########################################################################################

# 1. Athlete column: remove the ',' to avoid issues with csv file


grep (',' , medalData$Athlete )

medalData$Athlete <- str_remove_all(medalData$Athlete, ",")

grep (',' , medalData$Athlete )

#medalData$Athlete


# 2. Event column: remove the ',' to avoid issues with csv file

grep (',' , medalData$Event)

medalData$Event <- str_remove_all(medalData$Event, ",")

grep (',' , medalData$Event)

medalData$Event


########################################################################################
#                   DATA PREPARATION
########################################################################################


# 1. Create a new column for olympic_event as a concatenation of Sport/Discipline/Event
# This is needed to get an Event unique identifier 
medalData$OlympicEvent = paste(medalData$Sport,medalData$Discipline, medalData$Event) 

# 2. Create a column to define the gender of an event : Men, Women and Mixed
# This is required to aggregate medals correctly for a pair of mixed people
# or in case of an event that allows both Men and Women for Individual
# and Teams e.g.  Equestrian

medalData$EventGender =  medalData$Gender
levels(medalData$EventGender) <- c(levels(medalData$EventGender), "Mixed") 

str(medalData)

# 3. Populate EventGender with " Mixed" where applicable
##########################################################################
# https://en.wikipedia.org/wiki/Category:Mixed_events_at_the_Olympics
##########################################################################


# 3.1. Mixed doubles tennis /badminton at the Summer Olympics 

############################################################################
# For tennis and  badminton, set the EventGender of Mixed Doubles to Mixed
############################################################################


medalData$EventGender [medalData$Event == 'Mixed Doubles' ] <- 'Mixed'
medalData$EventGender [medalData$Event == 'Mixed Doubles Indoor' ] <- 'Mixed'

medalData [medalData$Event == 'Mixed Doubles', ]


filter ( medalData[c("Sport","Event", "EventGender")] , grepl("Mixed",Event)) 


# 3.2  Equestrian : Mixed for Individual and Teams 

attach (medalData)

medalData [Sport == 'Equestrian',]

# There is no Equestrian event that are Gender specific
filter ( medalData [Sport == 'Equestrian',] , grepl("Woman",Event)) 
filter ( medalData [Sport == 'Equestrian',] , grepl("Men",Event)) 

#[1] Year       City       Sport      Discipline Athlete    Country    Gender     Event      Medal     
#<0 rows> (or 0-length row.names)

##########################################
# Set all Equestrian events to mixed
##########################################

medalData$EventGender [Sport == 'Equestrian' ] <- 'Mixed'

# Review
medalData  [Sport == 'Equestrian' & Year == 2012 , ] 


########################################################
# TO DO : Look for other Mixed sports 
########################################################

#############################################################
# 4. Mixed-sex sailing at the Summer Olympics - TO DO
#############################################################
medalData  [Sport == 'Sailing' & Year == 2012 , ]

# In 2016 Nacra17	event was mixed 
# In 2012 no mixed event
# In 2008 - possibly mixed events

#############################################################
# 5. Mixed-sex shooting at the Summer Olympics  - TO DO
#############################################################

medalData  [Sport == 'Shooting' , ] 

# After 1992 - no mixed events

# 1984-1992 - mixed
# Shotgun
# Trap	mixed
# Skeet	mixed

# Before 1980 - All mixed
# 
# Rifle
# 50 m rifle three positions	mixed
# 50 m rifle prone	mixed
# 
# Pistol
# 50 m pistol	mixed
# 25 m rapid fire pistol	mixed
# 
# Shotgun
# Trap	mixed
# Skeet	mixed
# 
# Running target
# 50 m running target	mixed

sapply(medalData,function(x) sum(is.na(x)))

##################################################################
# This is the final clean version of medal data frame . Save it
##################################################################

write.csv(medalData,"summer_medals_wquotes.csv", row.names = FALSE, quote=FALSE)


########################################################################################
#                   DATA TRANSFORMATION - Aggregation
########################################################################################

# Transform data frame in a table to allow for aggregation

medalTable <- as.data.table(medalData)

medalTable


#########################################################################
# Aggregation 
#########################################################################


# Transform data table to a table of disctinct medals
# Count only one medal for each event, medal colour and country 

# 1. Distinct medals 
TableOfMedals = medalTable [   , 
                                  .( distinct_medals = length(unique(OlympicEvent, EventGender)) ), 
                                  by = .(  Year, OlympicEvent, EventGender,  Medal, Country)  ]

TableOfMedals
tail (TableOfMedals)

# 2.  Create 3 tables, aggregating medals, one for each type of medal Gold  / Silver  / Bronze 

TableOfGold =  TableOfMedals[Medal == "Gold" , .( gold_medals = sum(distinct_medals) )  ,  by= .(  Year, Country )  ]
TableOfSilver =  TableOfMedals [Medal == "Silver" , .( silver_medals = sum(distinct_medals) )  ,  by= .(  Year, Country )  ]
TableOfBronze =  TableOfMedals [Medal == "Bronze" , .( bronze_medals = sum(distinct_medals) )  ,  by= .(  Year, Country )  ]

TableOfGold
TableOfSilver
TableOfBronze

#  3. Join the 3 medal tables 

# https://rstudio-pubs-static.s3.amazonaws.com/52230_5ae0d25125b544caab32f75f0360e775.html

# Set the ON clause as keys of the tables
setkey(TableOfGold,Year, Country)
setkey(TableOfSilver,Year, Country)
setkey(TableOfBronze,Year, Country)

# Perform the join between the first 2 tables
Temp <- merge(TableOfGold,TableOfSilver, all=TRUE)

# Join result with the 3rd table , save in medalTable
medalTable <- merge( Temp, TableOfBronze, all=TRUE)

medalTable

# Find the NA values
sapply(medalTable,function(x) sum(is.na(x))) 


# Replace NA by 0 everywhere
medalTable$gold_medals [is.na(medalTable$gold_medals)] <- 0;
medalTable$silver_medals [is.na(medalTable$silver_medals)] <- 0;
medalTable$bronze_medals [is.na(medalTable$bronze_medals)] <- 0;

# Find the NA values
sapply(medalTable,function(x) sum(is.na(x))) 
# No NA values !

# Add a column: Total medals: sum of the 3 medal  columns 
medalTable$total_medals = medalTable$gold_medals + medalTable$silver_medals + medalTable$bronze_medals
medalTable

# Compare this table in detail with 
# https://en.wikipedia.org/wiki/2012_Summer_Olympics_medal_table
# USA , CHN slight difference, RUS large diff 


keycol <-c( "Year",  "Country")
setorderv(medalTable, keycol)
medalTable



# Save to csv  - DONE
write.csv(medalTable,"summer_medal_table.csv", row.names = FALSE, quote=FALSE)

# To convert tables back to data frame
# totalMedalData = as.data.frame (totalTable)

?data.table


########################################################################
#                 Data exploration
########################################################################

firstathletes <- medalData$Athlete [1:10]   
print(paste(firstathletes, sep=""))

# Overall count of medals
medalCounts <- table(medalData$Medal)
barplot(medalCounts, main="Medal count",
        xlab="Medal",ylab="Count", col="lightblue")

#Overall count of medals per year
Counts <- table(medalData$Medal,medalData$Year)
barplot(Counts, main="Medal count per year",
        xlab="Year",ylab="Count", col=c("#E9A984","#DDD9D6", "#EBC26A"),
        legend.text=c("Bronze", "Silver", "Gold"))

#Overall count of medals per gender
Counts <- table(medalData$Medal,medalData$Gender)
barplot(Counts, main="Medal count per gender",
        xlab="Year",ylab="Count", col=c("#E9A984","#DDD9D6", "#EBC26A"),
        legend.text=c("Bronze", "Silver", "Gold"))

# Overall count of medals per gender per year
Counts <- table(medalData$Gender,medalData$Year)
barplot(Counts, main="Medal count per gender",
        xlab="Year",ylab="Count", col=c("blue","pink"))

Counts <- table(medalData$Country)
barplot(Counts, main="Medal count per country",
        xlab="Country",ylab="Count",
)

# Simple Pie Chart - should pick the top 10 countries

counts = table(medalData$Country)
str(counts)
top10=head(sort(counts, decreasing=TRUE), n = 10)
top10
pie(top10, main="Top 10 Countries ")

# Simple Pie Chart - should pick the top 10 countries for 2012
# Note , too many medals returend because we count one medal per member of a team/pair/double
counts = table(medalData$Country[medalData$Year==2012])
str(counts)
top10=head(sort(counts, decreasing=TRUE), n = 10)
top10

pie(top10, main="Total medals for Top 10 Countries- 2012")

