install.packages("RMariaDB")
install.packages("dplyr")
install.packages("tidyverse")

library(RMariaDB) 
library(plyr)
library(dplyr)
library(caret)
library(ggplot2)
library(lubridate, warn.conflicts = FALSE)
library("RColorBrewer")
library(tidyr)

# Connect to the database
con= dbConnect(MariaDB(),
               user= 'deepAnalytics',
               password='Sqltask1234!',
               dbname= 'dataanalytics2018',
               host= 'data-analytics-2018.cbrosir2cswx.us-east-1.rds.amazonaws.com')

# List the tables contained in the database
dbListTables(con)

# Lists attributes contained in a table
dbListFields(con,'iris')


# Use asterisk to specify all attributes for download
irisALL <- dbGetQuery(con, "SELECT * FROM iris")

# Use attribute names to specify specific attributes for download
irisSELECT <- dbGetQuery(con, "SELECT SepalLengthCm, SepalWidthCm FROM iris")

dbListFields(con,'yr_2006')
yr_2006 <- dbGetQuery(con, "SELECT Date,
                          Time,
                         Sub_metering_1, 
                         Sub_metering_2, 
                         Sub_metering_3
                      FROM yr_2006")

summary(yr_2006)
str(yr_2006)
head(yr_2006)
tail(yr_2006)

#Get row counts by month
#Has data only for the 12 th month
yr_2006_month <- dbGetQuery (con, "select 
                                    EXTRACT(MONTH FROM Date) as month
                                    , count(*) as num 
                             from yr_2006 
                             group by month" )


yr_2007 <- dbGetQuery(con, "SELECT Date,
                          Time,
                         Sub_metering_1, 
                         Sub_metering_2, 
                         Sub_metering_3
                      FROM yr_2007")
# Has data for the whole year
yr_2007_month <- dbGetQuery (con, "select 
                                    EXTRACT(MONTH FROM Date) as month
                                    , count(*) as num 
                             from yr_2007 
                             group by month" )



yr_2008 <- dbGetQuery(con, "SELECT Date,
                          Time,
                         Sub_metering_1, 
                         Sub_metering_2, 
                         Sub_metering_3
                      FROM yr_2008")
#Has data for the whole year
yr_2008_month <- dbGetQuery (con, "select 
                                    EXTRACT(MONTH FROM Date) as month
                                    , count(*) as num 
                             from yr_2008 
                             group by month" )

yr_2009 <- dbGetQuery(con, "SELECT Date,
                          Time,
                         Sub_metering_1, 
                         Sub_metering_2, 
                         Sub_metering_3
                      FROM yr_2009")
#Has data for the whole year
yr_2009_month <- dbGetQuery (con, "select 
                                    EXTRACT(MONTH FROM Date) as month
                                    , count(*) as num 
                             from yr_2009 
                             group by month" )

yr_2010 <- dbGetQuery(con, "SELECT Date,
                          Time,
                         Sub_metering_1, 
                         Sub_metering_2, 
                         Sub_metering_3
                      FROM yr_2010")

#Has data for the only 1-11 month
yr_2010_month <- dbGetQuery (con, "select 
                                    EXTRACT(MONTH FROM Date) as month
                                    , count(*) as num 
                             from yr_2010 
                             group by month" )


#combine 2007, 2008, 2009 data
newDF <- bind_rows(yr_2007,yr_2008,yr_2009)
str(newDF)


### EDA for month and year 
tempDF <- newDF
tempDF$Date <- as.Date(tempDF$Date)

# Extract month and year columns
tempDF <- mutate( tempDF, Month = format(Date, "%m"),
                 Year= format(Date, "%y")
                 ) 
#Group by Month
my_group1 <- group_by(tempDF, Month, Year)

# Count the values by month
tempDF<-summarise(my_group1, total = n())

# plot each month by year
ggplot( tempDF,aes( x= Month, y=total, fill= Year)) +
  geom_bar(stat= "identity", position=position_dodge())
  
### end EDA for month and year                    
  

# Combine Date and Time attribute values in a new attribute column
updatedDF <-cbind(newDF,paste(newDF$Date,newDF$Time), stringsAsFactors=FALSE)

## Give the new attribute in the 6th column a header name 
str(updatedDF)
colnames(updatedDF)[6] <-"DateTime"
str(updatedDF)

updatedDF = updatedDF[,c(ncol(updatedDF),1 : (ncol(updatedDF)-1))]

str(updatedDF)

updatedDF$DateTime <- as.POSIXct(updatedDF$DateTime, "%Y/%m/%d %H:%M:%S")


str(updatedDF)
#Set the time zone to Europe/paris
attr(updatedDF$DateTime, "tzone") <- "Europe/Paris"

#Create a new attribute named year
updatedDF$year <- year(updatedDF$DateTime)

# Do the same for quarter, month, week, weekday, day, hour and minute

updatedDF$quarter <- quarter(updatedDF$DateTime)
updatedDF$month <- month(updatedDF$DateTime)
updatedDF$week <- week(updatedDF$DateTime)
updatedDF$weekday <- wday(updatedDF$DateTime)
updatedDF$day <- day(updatedDF$DateTime)
updatedDF$hour <- hour(updatedDF$DateTime)
updatedDF$minute <- minute(updatedDF$DateTime)

str(updatedDF)
summary(updatedDF)


# gives the column names and index
data.frame(colnames(updatedDF))


# Find the max of each of the three columns whose index is specified
tDF1 <- mapply(max, updatedDF[,c(4,5,6)])
tDF1
barplot(tDF1, ylim= c(0,100), col= "green")


tDF2 <- mapply(sum, updatedDF[,c(4,5,6)])
tDF2
barplot(tDF2, ylim= c(0,10000000), col= "red")


tDF3 <- mapply(mean, updatedDF[,c(4,5,6)])
tDF3
barplot(tDF3, ylim= c(0,10), col= "red")



#DFsub_meter <- newDF %>% gather(key= submeter, value= Value, Sub_metering_1 : Sub_metering_3)





#7.sub_metering_1: energy sub-metering No. 1 (in watt-hour of active energy). 
#It corresponds to the KITCHEN, containing mainly a dishwasher,
#an oven and a microwave (hot plates are not electric but gas powered). 
#8.sub_metering_2: energy sub-metering No. 2 (in watt-hour of active energy). 
#It corresponds to the LAUNDRY ROOM, containing a washing-machine, a tumble-drier, a refrigerator and a light. 
#9.sub_metering_3: energy sub-metering No. 3 (in watt-hour of active energy). 
#It corresponds to an electric WATER HEATER & AIR CONDITIONER.
# missing values in measurements
# All calendar timestamps are present in the dataset but for some timestamps, the measurement values are missing:
#a missing value is represented by the absence of value between two consecutive semi-colon attribute separators.

## Plot all of sub-meter 1
plot(newDF$Sub_metering_1)

## Subset the second week of 2008 - All Observations
houseWeek <- filter(newDF, year == 2008 & week == 2)

## Plot subset houseWeek
plot(houseWeek$Sub_metering_1)


