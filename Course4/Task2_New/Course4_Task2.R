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

dbListFields(con)
