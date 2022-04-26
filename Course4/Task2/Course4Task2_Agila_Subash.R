install.packages("RMariaDB")
install.packages("dplyr")
install.packages("tidyverse")
install.packages("lazyeval")
install.packages("forecast")
install.packages("broom")
install.packages("caret")
install.packages("plotly")
install.packages("ggfortify")

library(RMariaDB) 
library(plyr)
library(dplyr)
library(caret)
library(ggplot2)
library(lubridate, warn.conflicts = FALSE)
library("RColorBrewer")
library(tidyr)
library(plotly)
library(ggfortify)
library (forecast)
library(broom)




# Connect to the database
con= dbConnect(MariaDB(),
               user= 'deepAnalytics',
               password='Sqltask1234!',
               dbname= 'dataanalytics2018',
               host= 'data-analytics-2018.cbrosir2cswx.us-east-1.rds.amazonaws.com')


dbListFields(con,'yr_2006')
yr_2006 <- dbGetQuery(con, "SELECT Date,
                          Time,
                         Sub_metering_1, 
                         Sub_metering_2, 
                         Sub_metering_3
                      FROM yr_2006")


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

# Find the sum of each of the three columns whose index is specified
tDF2 <- mapply(sum, updatedDF[,c(4,5,6)])
tDF2
barplot(tDF2, ylim= c(0,10000000), col= "red")

# Find the mean of each of the three columns whose index is specified
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
plot(updatedDF$Sub_metering_1)

## Subset the second week of 2008 - All Observations
houseWeek <- filter(updatedDF, year == 2008 & week == 2)

## Plot subset houseWeek
plot(houseWeek$Sub_metering_1)

# Data for 9 th day of January 2008 - All observations

houseDay <- filter(updatedDF, year==2008 & month==1 & day==9)

plot_ly(houseDay,x= ~houseDay$DateTime, 
        y= houseDay$Sub_metering_1, 
        type= 'scatter',
        mode = 'lines')


#Plot sub_meter 1, 2, 3 with title, legend and labels- All observations

plot_ly(houseDay, x=~houseDay$DateTime, 
        y= houseDay$Sub_metering_1, 
        name= 'kitchen',
        type= 'scatter',
        mode = 'lines') %>%
  add_trace(y= houseDay$Sub_metering_2, 
            name= 'Laundry Room',
            type= 'scatter',
            mode = 'lines') %>%
  add_trace(y= houseDay$Sub_metering_3, 
            name= 'Water Heater & AC',
            type= 'scatter',
            mode = 'lines') %>%
  layout(title = "Power Consumption January 9 th, 2008",
         xaxis= list(title = "Time"),
         yaxis = list(title= "Power(watt-hours)"))




## Subset the 9th day of January 2008 - 10 Minute frequency
houseDay10 <- filter(updatedDF, year == 2008 & month == 1 & day == 9 & 
                       (minute == 0 | minute == 10 | minute == 20 | minute == 30 | minute == 40 | minute == 50))

## Plot sub-meter 1, 2 and 3 with title, legend and labels - 10 Minute frequency
plot_ly(houseDay10, x = ~houseDay10$DateTime,
        y = ~houseDay10$Sub_metering_1, 
        name = 'Kitchen', 
        type = 'scatter',
        mode = 'lines') %>%
  add_trace(y = ~houseDay10$Sub_metering_2, 
            name = 'Laundry Room', 
            mode = 'lines') %>%
  add_trace(y = ~houseDay10$Sub_metering_3,
            name = 'Water Heater & AC',
            mode = 'lines') %>%
  layout(title = "Power Consumption January 9th, 2008",
         xaxis = list(title = "Time"),
         yaxis = list (title = "Power (watt-hours)"))


#visualization with plotly for a Week 

houseJanWeek4 <- filter(updatedDF, year == 2008 & month == 1 & week == 2 & 
                       (minute == 0 |minute == 20 |minute == 40| minute == 60 ))

## Plot sub-meter 1, 2 and 3 with title, legend and for year Jan 2008, week 2, every 20 minutes
plot_ly(houseJanWeek4, x = ~houseJanWeek4$DateTime,
        y = ~houseJanWeek4$Sub_metering_1, 
        name = 'Kitchen', 
        type = 'scatter',
        mode = 'lines') %>%
  add_trace(y = ~houseJanWeek4$Sub_metering_2, 
            name = 'Laundry Room', 
            mode = 'lines') %>%
  add_trace(y = ~houseJanWeek4$Sub_metering_3,
            name = 'Water Heater & AC',
            mode = 'lines') %>%
  layout(title = "Power Consumption Week 2 of Jan, 2008",
         xaxis = list(title = "Time"),
         yaxis = list (title = "Power (watt-hours)"))

plot_ly(houseJanWeek4, x = ~houseJanWeek4$DateTime,
        y = ~houseJanWeek4$Sub_metering_1, 
        name = 'Kitchen', 
        type = 'scatter',
        mode = 'lines') %>%
  layout(title = "Kitchen Consumption Week 2 of Jan, 2008",
         xaxis = list(title = "Time"),
         yaxis = list (title = "Power (watt-hours)"))


#Visualization for July, August 2008 

houseJulyAug <- filter(updatedDF, year == 2008 & (month == 7| month==8))

unique(houseJulyAug[c("week")])

unique(updatedDF[c("year")])




plot_ly(houseJulyAug , x = ~houseJulyAug$DateTime,
        y = ~houseJulyAug $Sub_metering_1, 
        name = 'Kitchen', 
        type = 'scatter',
        mode = 'lines') %>%
  add_trace(y = ~houseJulyAug$Sub_metering_2, 
            name = 'Laundry Room', 
            mode = 'lines') %>%
  add_trace(y = ~houseJulyAug$Sub_metering_3,
            name = 'Water Heater & AC',
            mode = 'lines') %>%
  layout(title = "Power July and August",
         xaxis = list(title = "Time"),
         yaxis = list (title = "Power (watt-hours)"))


# Just to check if there are any missing data for any of the weeks.
temptest1 <- updatedDF %>% 
  filter( year== 2008 & (week > 26 & week < 36) ) %>%
  group_by(week) %>%
  summarise(subMeter_1=sum(Sub_metering_1), subMeter_2=sum(Sub_metering_1))


  plot_ly(x=temptest1$week,y=~temptest1$subMeter_1, 
          name='kitchen',
          type='bar', 
          ) %>%
  add_trace(y=~temptest1$subMeter_2, 
            name='LaundryRoom',
            type='bar' 
  ) %>%
 layout(title = "Power consumption of Kitchen and Laundry Room of July and August by every week",
           xaxis = list(title = "week"),
           yaxis = list (title = "Power (watt-hours)"))
  

### Time Series Analysis ##############
  ## Submeter_1
#Subset to one observation per week on Mondays at 8:00 P.M for 2007, 2008, 2009
  
house070809weekly <- filter(updatedDF, weekday==2 & hour== 20 & minute ==1)


## create TS object with Submeter3

tsSM3_070809weekly <- ts(house070809weekly$Sub_metering_3, 
                         frequency = 52, 
                         start= c(2007,1))



autoplot(tsSM3_070809weekly,
         color = 'blue',
         xlab="Time",
         ylab="Watt Hours",
         main="Sub_meter 3")



  ##Submeter_ 1 (kitchen)
## Method 1 : taking data during the peak hour based on usage data
#Subset to one observation per week on Mondays between 6:00  and 9:00 P.M for 2/2008

houseEverydayKitchen <- filter(updatedDF, year == 2008 & month==2 &
                                 (hour >17 & hour < 20)  
                                 )

#create TS object with Submeter_1 (kitchen)

tsSM1_EverydayKitchen <- ts(houseEverydayKitchen$Sub_metering_1 ,
                         frequency = 3479, 
                         start= c(200802,1)
                    
                         )

tsSM1_EverydayKitchen

autoplot(tsSM1_EverydayKitchen,
         color = 'blue',
         xlab="Time",
         ylab="Watt Hours",
         main="Sub_meter 1 Kitchen")

## Method 1 : Summarizing the monthly data for all three years and using the data

houseUsage_monthly <- updatedDF %>%
  group_by(year(DateTime), month(DateTime)) %>%
  summarise(Sub_meter_1 = round(sum(`Sub_metering_1`/1000),3),
            Sub_meter_2 = round(sum(`Sub_metering_2`/1000),3),
            Sub_meter_3 = round(sum(`Sub_metering_3`/1000),3)
            )
  
head(houseUsage_monthly)

tsSM1_montlyKitchen <- ts(houseUsage_monthly$Sub_meter_1,
                          frequency = 12,
                          start= c(2008,1)
                                )

autoplot(tsSM1_montlyKitchen, 
         color='purple',
         xlab="time",
         ylab="Watt Hours",
         main = "Sub_meter 1 Kitchen")



## Submeter_2 (Laundry Room)

house070809daily <- filter(updatedDF, hour==20 & minute==1)


tsSM2_daily <- ts(house070809daily$Sub_metering_2, frequency = 365, start=c(2007,1))

autoplot(tsSM2_daily,
         ts.colour = 'orange',
         xlab="Time",
         ylab="Watt Hours",
         main="Sub_meter 2 Laundry")


#### End Time series Analysis###########


#### Start : Forecasting time series###########

##Sub_meter 3 ##


##Apply time series linear regression to the sub-meter 3 ts object and
# use summary to obtain R2 and RMSE from the model you built


fitSM3 <- tslm(tsSM3_070809weekly ~ trend + season)
summary(fitSM3)
glance(fitSM3)


## Create the forecast for sub-meter 3. Forecast ahead 20 time periods 

forecastfitSM3 <- forecast(fitSM3, h=20)

# Plot the forecast for sub_meter 3
plot(forecastfitSM3)

## Create the forecast for sub-meter 3. Forecast ahead 20 time periods 
# with confidence levels 80 and 90
forecastfitSM3_withC <- forecast(fitSM3, h=20, level= c(80,90))


#plot sub_meter 3 forecast, limit y and add labels

plot(forecastfitSM3_withC, ylim= c(0,20), ylab= "Watt_Hours", xlab= "Time")

## Sub _meter 1###

##Apply time series linear regression to the sub-meter 1 (kitchen) ts object and
# use summary to obtain R2 and RMSE from the model you built

fitSM1 <- tslm(tsSM1_montlyKitchen ~ trend + season)
summary(fitSM1)
glance(fitSM1)

forecastfitSM1 <- forecast(fitSM1,h=20)
plot(forecastfitSM1)
  
# Create the forecast for sub-meter 1 (kitchen). forecast ahead 30 time periods

forecastfitSM1 <- forecast(fitSM1, h=20, c(80,90))
plot(forecastfitSM1)

plot(forecastfitSM1, ylim= c(0,90), ylab= "Watt_Hours", xlab= "Time")



  







