# Sentiment Analysis for Iphone 

# Summary : Compare different classifiers for both iPhone and Galaxy 
# with small data sets.
# Improve performance metrics with feature selection/feature engineering
# after identifying most optimal model use that to predict sentiment in Large Matix

# Install packages

install.packages("caret")
install.packages("corrplot")
install.packages("readr")
install.packages("doParallel")
install.packages('e1071', dependencies=TRUE)
install.packages("C50")
install.packages("kknn")
install.packages("gbm")


#Library

library(caret)
library(corrplot)
library(readr)
library(e1071)
library(C50)
library(kknn)
library(gbm)

library (doParallel)


#Find the number of cores

detectCores()

#Create clusters with desired number of cores
cl <- makeCluster(6)
registerDoParallel(cl)

#Get the number of cores are assigned for R and Rstudio
getDoParWorkers()

#Import Data

iphone_smallmatrix <- read.csv("iphone_smallmatrix_labeled_8d.csv")
iphone_largematrix <- read.csv()

#Training Set

summary(iphone_smallmatrix)
str(iphone_smallmatrix)
head(iphone_smallmatrix)
tail(iphone_smallmatrix)

#check for missing values

is.na(iphone_smallmatrix)
any(is.na(iphone_smallmatrix))

#plot
hist(iphone_smallmatrix$iphonesentiment)

qqnorm(iphone_smallmatrix$iphonesentiment) 





#stop cluster after performing tasks.
#stopCluster()

