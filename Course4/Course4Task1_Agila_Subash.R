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

## Combine Date and Time attribute values in a new attribute column
newDF <-cbind(newDF,paste(newDF$Date,newDF$Time), stringsAsFactors=FALSE)

## Give the new attribute in the 6th column a header name 

colnames(newDF)[6] <-"DateTime"
newDF = newDF[,c(ncol(newDF),1 : (ncol(newDF)-1))]
newDF$DateTime <- as.POSIXct(newDF$DateTime, "%Y/%m/%d %H:%M:%S")

#Set the time zone to Europe/paris
attr(newDF$DateTime, "tzone") <- "Europe/Paris"

#Create a new attribute named year
newDF$year <- year(newDF$DateTime)

# Do the same for quarter, month, week, weekday, day, hour and minute

newDF$quarter <- quarter(newDF$DateTime)
newDF$month <- month(newDF$DateTime)
newDF$week <- week(newDF$DateTime)
newDF$weekday <- wday(newDF$DateTime)
newDF$day <- day(newDF$DateTime)
newDF$hour <- hour(newDF$DateTime)
newDF$minute <- minute(newDF$DateTime)


str(newDF)
summary(newDF)
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


