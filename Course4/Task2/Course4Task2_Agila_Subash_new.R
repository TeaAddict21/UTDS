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
str(updatedDF)

## Give the new attribute in the 6th column a header name 
colnames(updatedDF)[6] <-"DateTime"
str(updatedDF)

# Move the DateTime attribute within the dataset
updatedDF = updatedDF[,c(ncol(updatedDF),1 : (ncol(updatedDF)-1))]
str(updatedDF)

#Convert DateTime from character to POSIXct
updatedDF$DateTime <- as.POSIXct(updatedDF$DateTime, "%Y/%m/%d %H:%M:%S")
str(updatedDF)

#Set the time zone to Europe/paris
attr(updatedDF$DateTime, "tzone") <- "Europe/Paris"
str(updatedDF)

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
  layout(title = "Power Consumption January 9th, 2008
         10 min frequency",
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
  layout(title = "Power Consumption Week 2 of Jan, 2008
         every 20 min frequency",
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
  

####################### Time Series Analysis ##########################

   ## Submeter_3
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


#---------------------------------------
### Every Saturday for SubMeter 1 (kitchen)

# Subset to 1 observation per week on Saturday at 8:00pm for 2007, 2008 and 2009
house070809weekly_Sat <- filter(updatedDF, weekday == 7 & hour == 20 & minute == 1)

# Create TS object with SubMeter 1
tsSM1_070809weekly_Sat <- ts(house070809weekly_Sat$Sub_metering_1,
                             frequency=52, 
                             start=c(2007,1))

# Plot sub-meter 1 with plot.ts
plot.ts(tsSM1_070809weekly_Sat, main = "Time Series Analysis
            Sub-meter 1-Kitchen",
             col="dark green")





## Submeter_2 (Laundry Room)

house070809daily <- filter(updatedDF,weekday==1 & hour==20 & minute==1)

tsSM2_daily <- ts(house070809daily$Sub_metering_2, frequency = 52, start=c(2007,1))

autoplot(tsSM2_daily,
         colour = 'orange',
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

## Create the forecast for sub-meter 3. Forecast ahead 20 time periods 
# with confidence levels 80 and 90
forecastfitSM3_withC <- forecast(fitSM3, h=20, level= c(80,90))


#plot sub_meter 3 forecast, limit y and add labels

plot(forecastfitSM3_withC, ylim= c(0,20), ylab= "Watt_Hours", xlab= "Time", 
     main= "AC & Heater Mondays at 8:00 P.M")

## Sub _meter 1###

# My trial (weekly--"Saturday") for Sub-meter 1 
# Subset to 1 observation per week on Saturday at 8:00pm for 2007, 2008 and 2009
fitSM1 <- tslm(tsSM1_070809weekly_Sat ~ trend + season) 
summary(fitSM1)

# Create Sub-meter 1 forecast with confidence levels 80 and 90
forecastfitSM1c <- forecast(fitSM1, h=24, level=c(80,90))
# Plot sub-meter 3 forecast, limit y and add labels
plot(forecastfitSM1c, ylim = c(0, 20), ylab= "Watt-Hours", xlab="Time",
     main = "Kitchen Forecast 2007-2009 Every Saturday 8:00pm")

### Sub-Meter 2 (2007-2009)

# daily data for 365 days, taken at 18:00pm per day
fitSM2 <- tslm(tsSM2_daily ~ trend + season) 
summary(fitSM2)

# Create sub-meter 2 forecast &  forecast ahead 24 (1 day) period with confidence levels 80 and 90
forecastfitSM2c <- forecast(fitSM2, h=24, level=c(80,90))
# Plot sub-meter 2 forecast, limit y and add labels
plot(forecastfitSM2c, ylim = c(0, 20), ylab= "Watt-Hours", xlab="Time", 
     main = "Laundry Room Forecast 2007-2009 Daily 8:00pm")
  

##### End ForeCasting  ############

###--------Decomposing a Seasonal Time Series-----------##


# Decompose Sub-meter 3 into trend, seasonal and random
components070809SM3weekly <- decompose(tsSM3_070809weekly)
# Plot decomposed sub-meter 3 
plot(components070809SM3weekly)

summary(components070809SM3weekly)

#---------------
#Sub meter1
# Decompose Sub-meter 1 into trend, seasonal and random
components070809SM1weekly_Sat <- decompose(tsSM1_070809weekly_Sat)
# Plot decomposed sub-meter 1 
plot(components070809SM1weekly_Sat)
summary(components070809SM1weekly_Sat)


# Sub Meter 2

# Decompose Sub-meter 2 into trend, seasonal and random
components070809SM2daily <- decompose(tsSM2_daily) 
# Plot decomposed sub-meter 2
plot(components070809SM2daily)

summary(components070809SM2daily)

######--- End Decomposing a Seasonal Time Series-------#


####----------------Holt-Winters Forecasting (Start) ----------------###

# Remove Seasonal Components
# Seasonal adjusting sub-meter 3 by subtracting the seasonal component & plot

# Sub_meter 3:
  
tsSM3_070809Adjusted <- tsSM3_070809weekly - components070809SM3weekly$seasonal

autoplot(tsSM3_070809Adjusted)

# Test Seasonal Adjustment by running Decompose again 
plot(decompose(tsSM3_070809Adjusted))


# Holt Winters Exponential Smoothing
tsSM3_HW070809 <- HoltWinters(tsSM3_070809Adjusted, beta= FALSE, gamma= FALSE)
plot(tsSM3_HW070809, ylim= c(0,25))
# Forecast HoltWinters with confidence levels
tsSM3_HW070809for <- forecast(tsSM3_HW070809, h=25)
# Plot only the forecasted area
plot(tsSM3_HW070809for, ylim = c(0, 20), ylab= "Watt-Hours", xlab="Time - Sub-meter 3",
      main = "Sub-meter 3 HWForecast, Mondays, 20:00")

## Forecast HoltWinters with diminished confidence levels
tsSM3_HW070809forC <- forecast(tsSM3_HW070809, h=25, level=c(10,25))
## Plot only the forecasted area
plot(tsSM3_HW070809forC, ylim = c(0, 10), ylab= "Watt-Hours", xlab="Time - Sub-meter 3", start(2010))


#-----------------

# Sub_meter1: (Kitchen)


# Seasonal adjusting sub-meter 1 by subtracting the seasonal component & plot
tsSM1_070809Adjusted <- tsSM1_070809weekly_Sat - components070809SM1weekly_Sat$seasonal

## Holt Winters Simple Exponential Smoothing & Plot
tsSM1_HW070809 <- HoltWinters(tsSM1_070809Adjusted, beta=FALSE, gamma=FALSE)
plot(tsSM1_HW070809, ylim = c(0, 25))

tsSM1_HW070809forC <- forecast(tsSM1_HW070809, h=25, level=c(10,25))

## Plot only the forecasted area
plot(tsSM1_HW070809forC, ylim = c(0, 35), ylab= "Watt-Hours", xlab="Time - Sub-meter 1",
     start(2010), main = "Kitchen HoltWinters Forecast, Every Saturday 8:00pm") 


#-------------

# Sub_meter 2 (Laundry Room)

# My trail Sub-Meter 2 (daily data for 365 days and from 2007-2009, taken at 18:00pm per day)
# Seasonal adjusting sub-meter 2 by subtracting the seasonal component & plot
tsSM2_070809Adjusted <- tsSM2_daily - components070809SM2daily$seasonal

# Holt Winters Triple Exponential Smoothing & Plot
tsSM2_HW070809 <- HoltWinters(tsSM2_070809Adjusted) 
plot(tsSM2_HW070809, ylim = c(0, 25))

# Forecast HoltWinters with 80-90 confidence levels
tsSM2_HW070809forC <- forecast(tsSM2_HW070809, h=25, level=c(10,25))
# Plot only the forecasted area
plot(tsSM2_HW070809forC, ylim = c(0, 3), ylab= "Watt-Hours", xlab="Time - Sub-meter 2", 
     start(2010), main = "Laundry Room HoltWinters Forecast, Everyday 8:00pm")



