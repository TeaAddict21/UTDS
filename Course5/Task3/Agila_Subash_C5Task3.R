# Sentiment Analysis for Iphone 

# Summary : Compare different classifiers for both iPhone and Galaxy 
# with small data sets.
# Improve performance metrics with feature selection/feature engineering
# after identifying most optimal model use that to predict sentiment in Large Matix


# Sentiment Value :

# 0 : sentiment unclear
# 1 : very negative
# 2 : somewhat negative
# 3 : neutral
# 4 : somewhat positive
# 5 : very positive
#

# Install packages

install.packages("caret")
install.packages("corrplot")
install.packages("readr")
install.packages("doParallel")
install.packages('e1071', dependencies=TRUE)
install.packages("C50")
install.packages("kknn")
install.packages("gbm")
install.packages("AppliedPredictiveModeling")
install.packages("tidtverse")
install.packages("plotly")


#Library

library(caret)
library(corrplot)
library(readr)
library(e1071)
library(C50)
library(kknn)
library(gbm)
library(AppliedPredictiveModeling)
library (randomForest)
library (doParallel)
library(plotly)



#---------------1. Set Up Parallel Processing-------------------
#Find the number of cores
detectCores()

#Create clusters with desired number of cores
cl <- makeCluster(6)
registerDoParallel(cl)

#Get the number of cores are assigned for R and Rstudio
getDoParWorkers()

#Import Data
iphone_smallmatrix <- read.csv("iphone_smallmatrix_labeled_8d.csv")
largematrix <- read.csv("LargeMatrix.csv")


#-----------------2. Explore the Data--------------------------

summary(largematrix)
str(largematrix)
head(largematrix)
nrow(largematrix)
ncol(largematrix)


nrow(iphone_smallmatrix)
ncol(iphone_smallmatrix)



plot_ly(iphone_smallmatrix,x=~iphone_smallmatrix$iphonesentiment, type = 'histogram')

#plot
hist(iphone_smallmatrix$iphonesentiment)
qqnorm(iphone_smallmatrix$iphonesentiment)

#check for missing values
is.na(iphone_smallmatrix)
any(is.na(iphone_smallmatrix))

#----------------3. Preprocessing & Feature Selection --------------

#-----Correlation----------------
#select relevant columns for iphone

iphone_columns <- iphone_smallmatrix %>%
  select (starts_with("iphone"), starts_with("ios"), iphonesentiment)

# Use the cor() and corrplot() functions
options(max.print = 1000000)
iphoneCOR<- cor(iphone_columns)
corrplot(iphoneCOR)

#-----Examine Feature Variance----

## Examine feature variance: nearZeroVar() with saveMetrics = TRUE 
# returns an object containing a table including: 
#frequency ratio, percentage unique, zero variance and near zero variance 

nzvMetrics <- nearZeroVar(iphoneCOR, saveMetrics = TRUE)
nzvMetrics

nzv <- nearZeroVar(iphoneCOR, saveMetrics = FALSE)
 
#--------Recursive Feature elimination---------
#sample the data before using RFE
set.seed(101)
# Take 1000 sample and work through it.

iphoneSample <- iphone_smallmatrix [sample(1:nrow(iphone_smallmatrix ), 1000, replace=FALSE),]

#Set up rfeControl with randomforest, repeated cross validation and no updates

ctrl <- rfeControl(functions= rfFuncs,
                   method= "repeatedcv",
                   repeats= 5,
                   verbose= FALSE)

#Use rfe and omit the response variable (attribute 59 iphonesentiment)

rfeResults <- rfe(iphoneSample[,1:58],
                  iphoneSample$iphonesentiment,
                  sizes =(1:58),
                  rfeControl=ctrl)



#Training Set
summary(iphone_sample)
str(iphone_sample)
head(iphone_sample)
tail(iphone_sample)

#change variable types
iphone_sample$iphonesentiment <- as.factor(iphone_sample$iphonesentiment)

#normalize

preprocessParams <- preProcess(iphone_sample[,1:58], method= c("center", "scale"))
print(preprocessParams)

iphone_sample_N <-predict(preprocessParams,iphone_sample )
str(iphone_sample_N)
ncol(iphone_sample_N)


#------------------Prediction Set------------------------

summary(largematrix)
str(largematrix)
head(largematrix)
nrow(largematrix)
ncol(largematrix)

is.na(largematrix)
any(is.na(iphone_smallmatrix))

#drop ID variable
largematrix$id <- NULL

#change variable types

largematrix$iphonesentiment <- as.factor (largematrix$iphonesentiment)

ncol(largematrix)
ncol(iphone_sample_N)



#stop cluster after performing tasks.
#stopCluster()

