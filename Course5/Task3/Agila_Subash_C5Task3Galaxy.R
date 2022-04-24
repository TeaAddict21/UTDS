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
install.packages("tidyverse")
install.packages("plotly")
install.packages("kernlab")
install.packages('dplyr')


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
library(kernlab)
library(dplyr)


#---------------1. Set Up Parallel Processing-------------------
#Find the number of cores
detectCores()

#Create clusters with desired number of cores
cl <- makeCluster(8)
registerDoParallel(cl)

#Get the number of cores are assigned for R and Rstudio
getDoParWorkers()

#Import Data
largematrix <- read.csv("galaxy_LargeMatrix.csv")
GSmallMatrix <- read.csv("galaxy_smallmatrix_labeled_9d.csv")

#-----------------2. Explore the Data (Galaxy)-------------------------

summary(GSmallMatrix)
str(GSmallMatrix)
head(GSmallMatrix)
nrow(GSmallMatrix)
ncol(GSmallMatrix)

is.na(GSmallMatrix)
any(is.na(GSmallMatrix))

str(largematrix)


plot_ly(GSmallMatrix,x=~GSmallMatrix$galaxysentiment, type = 'histogram')

#plot
hist(GSmallMatrix$galaxysentiment)
summary(GSmallMatrix$galaxysentiment)
qqnorm(GSmallMatrix$galaxysentiment)

#----------------3. Preprocessing & Feature Selection (Galaxy)--------------

# 1-----Correlation----------------
#select relevant columns for iphone


options(max.print = 1000000)
GSmallCOR1 <- cor(GSmallMatrix)
corrplot(GSmallCOR1)

GSmallCOR <- GSmallMatrix %>%
  select (starts_with("galaxy"), starts_with("samsung"), galaxysentiment)

# Use the cor() and corrplot() functions
GSmallCOR2<- cor(GSmallCOR)
corrplot(GSmallCOR2)

str(GSmallCOR)


# 2-----Examine Feature Variance----

## Examine feature variance: nearZeroVar() with saveMetrics = TRUE 
# returns an object containing a table including: 
#frequency ratio, percentage unique, zero variance and near zero variance 

nzvMetricsGS <- nearZeroVar(GSmallMatrix, saveMetrics = TRUE)
nzvMetricsGS

nzvGS <- nearZeroVar(GSmallMatrix, saveMetrics = FALSE)
nzvGS

# create a new data set and remove near zero variance features
GSmallNZV <- GSmallMatrix[,-nzvGS]
str(GSmallNZV)


#--------Recursive Feature elimination---------

#sample the data before using RFE
set.seed(101)
# Take 1000 sample and work through it.
GSample <- GSmallMatrix [sample(1:nrow(GSmallMatrix), 1000, replace=FALSE),]

#Set up rfeControl with randomforest, repeated cross validation and no updates
ctrl <- rfeControl(functions= rfFuncs,
                   method= "repeatedcv",
                   repeats= 5,
                   verbose= FALSE)

#Use rfe and omit the response variable (attribute 59 iphonesentiment)
rfeResults <- rfe(GSample[,1:58],
                  GSample$galaxysentiment,
                  sizes =(1:58),
                  rfeControl=ctrl)

rfeResults
str(rfeResults)

plot(rfeResults, type=c("g","o"))

#create new data set with rfe recommended features
GSmallRFE <- GSmallMatrix [,predictors(rfeResults)]

# add the dependent variable to iphoneRFE
GSmallRFE$galaxysentiment <- GSmallMatrix$galaxysentiment
str(GSmallRFE)
varImp(rfeResults)

#-----------Preprocessing---------------------

# Factorize the dependent variable 
GSmallMatrix$galaxysentiment <- as.factor(GSmallMatrix$galaxysentiment)
GSmallNZV$galaxysentiment <- as.factor(GSmallNZV$galaxysentiment)
GSmallRFE$galaxysentiment <- as.factor(GSmallRFE$galaxysentiment)

str(GSmallMatrix$galaxysentiment)
summary(GSmallMatrix$galaxysentiment)
summary(GSmallNZV$galaxysentiment)
summary(GSmallRFE$galaxysentiment)

#----------------------------------------------------------------------------

#---4. Model Development and Evaluation (Galaxy)----------------------------

#---------- Out of the Box Model Development------------------------

set.seed(102)
# Define an 70%/30% train/test split of the galaxy
inTraining <- createDataPartition(GSmallMatrix$galaxysentiment, p = .70, list = FALSE)
training <- GSmallMatrix[inTraining,]
testing <- GSmallMatrix[-inTraining,]

# 10 fold cross validation 
fitControl <- trainControl(method = "cv", number = 10)

#--------- C5.0 training--------------

set.seed(103)
C50 <- train(galaxysentiment~., data = training, method = "C5.0", trControl=fitControl)
# Testing 
prediction_C50 <- predict(C50, testing)

plot(C50)


##--------- Random Forest Train--------------
set.seed(104)
rf <- train(galaxysentiment~., data = training, method = "rf", trControl=fitControl)
# Testing 
prediction_rf<- predict(rf, testing)

plot(rf)
##-------------SVM with 10-fold cross validation ------
set.seed(105)
svm <- train(galaxysentiment~., data = training, method = "svmLinear", trControl=fitControl)
# Testing 
prediction_svm<- predict(svm, testing)

plot(svm)
###------------- KKNN with 10-fold cross validation------
set.seed(106)

kknn <- train(galaxysentiment~., data = training, method = "kknn", trControl=fitControl)
# Testing 
prediction_kknn<- predict(kknn, testing)
plot(kknn)


#---------- Model Performance----------------

# Evaluate C5.0 Model
postResample(prediction_C50, testing$galaxysentiment)
# Evaluate RF Model
postResample(prediction_rf, testing$galaxysentiment)
# Evaluate SVM Model
postResample(prediction_svm, testing$galaxysentiment)
# Evaluate KKNN Model
postResample(prediction_kknn, testing$galaxysentiment)


#------Confusion Metrics---------------

# Create a confusion matrix from random forest predictions 
CMRF <- confusionMatrix(prediction_rf, testing$galaxysentiment) 
CMRF

CM_C50 <- confusionMatrix(prediction_C50, testing$galaxysentiment)
CM_C50
#------------------------------------------------------------


#C50 (Best performing model) for #GSmallNZV  # GSmallRFE
set.seed(214)
# Define an 70%/30% train/test split of the GSmallNZV
inTraining_GSmallNZV <- createDataPartition(GSmallNZV$galaxysentiment, p = .70, list = FALSE)
training_NZV <- GSmallNZV[inTraining,]
testing_NZV <- GSmallNZV[-inTraining,]

# Apply C50 for GSmallNZV

C50_NZV <- train(galaxysentiment~., data = training_NZV, method = "C5.0", trControl=fitControl)

#Testing 
prediction_C50_NZV <- predict(C50_NZV, testing_NZV)

postResample(prediction_C50_NZV, testing_NZV$galaxysentiment)

#---------------

set.seed(234)
# Define an 70%/30% train/test split of the GSmallRFE
inTraining_GSmallRFE <- createDataPartition(GSmallRFE$galaxysentiment, p = .70, list = FALSE)
training_RFE <- GSmallRFE[inTraining,]
testing_RFE <- GSmallRFE[-inTraining,]

C50_RFE <- train(galaxysentiment~., data = training_RFE, method = "C5.0", trControl=fitControl)

#Testing 
prediction_C50_RFE <- predict(C50_RFE, testing_RFE)

#---------------------- PostResample NZV RFE--------------

postResample(prediction_C50_NZV, testing_NZV$galaxysentiment)
postResample(prediction_C50_RFE, testing_RFE$galaxysentiment)


#-------------5.Feature Engineeering----------------------
#----------Recode--------------GSmallMatrixRC$galaxysentiment

GSmallRC <- GSmallMatrix

# Recode sentiment to combine factor levels 0 & 1 and 4 & 5
GSmallRC$galaxysentiment <- recode(GSmallRC$galaxysentiment, '0' = 1, '1' = 1, '2' = 2, '3' = 3, '4' = 4, '5' = 4) 


str(GSmallRC$galaxysentiment)
summary(GSmallRC$galaxysentiment)


# Make iphonesentiment a factor
GSmallRC$galaxysentiment <- as.factor(GSmallRC$galaxysentiment)

summary(GSmallRC$galaxysentiment)
str(GSmallRC$galaxysentiment)
str(largematrix)



prediction_C50 <- predict(C50, GSmallRC)

postResample(prediction_C50,GSmallRC$galaxysentiment)




#######################################################
#------------ Make Predictions on Large Matrix for iphone-----
#######################################################


largematrix$id <- NULL

largematrix$galaxysentiment <- as.factor(largematrix$galaxysentiment)
summary(largematrix)
str(largematrix)

summary(largematrix$galaxysentiment)

#predicting

prediction_C50_final <- predict(C50, largematrix)

summary(prediction_C50_final)

histogram(prediction_C50_final)

#stop cluster after performing tasks.
stopCluster(cl)

