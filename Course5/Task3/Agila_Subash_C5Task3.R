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

# 1-----Correlation----------------
#select relevant columns for iphone

#iphonetemp <- iphone_smallmatrix
options(max.print = 1000000)
tempCOR1 <- cor(iphone_smallmatrix)
corrplot(tempCOR)

iphoneCOR <- iphone_smallmatrix %>%
  select (starts_with("iphone"), starts_with("ios"), iphonesentiment)

# Use the cor() and corrplot() functions
tempCOR2<- cor(iphoneCOR)
corrplot(tempCOR2)


# 2-----Examine Feature Variance----

## Examine feature variance: nearZeroVar() with saveMetrics = TRUE 
# returns an object containing a table including: 
#frequency ratio, percentage unique, zero variance and near zero variance 

nzvMetrics <- nearZeroVar(iphone_smallmatrix, saveMetrics = TRUE)
nzvMetrics

nzv <- nearZeroVar(iphone_smallmatrix, saveMetrics = FALSE)
nzv

# create a new data set and remove near zero variance features
iphoneNZV <- iphone_smallmatrix[,-nzv]
str(iphoneNZV)

 
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

rfeResults
plot(rfeResults, type=c("g","o"))

#create new data set with rfe recommended features
iphoneRFE <- iphone_smallmatrix [,predictors(rfeResults)]

# add the dependent variable to iphoneRFE
iphoneRFE$iphonesentiment <- iphone_smallmatrix$iphonesentiment
str(iphoneRFE)
varImp(rfeResults)

#-----------Preprocessing---------------------

# Factorize the dependent variable 
iphone_smallmatrix$iphonesentiment <- as.factor(iphone_smallmatrix$iphonesentiment)
iphoneNZV$iphonesentiment <- as.factor(iphoneNZV$iphonesentiment)
iphoneRFE$iphonesentiment <- as.factor(iphoneRFE$iphonesentiment)

str(iphone_smallmatrix$iphonesentiment)


#---4. Model Development and Evaluation----------------------------

#---------- Out of the Box Model Development------------------------


# Define an 70%/30% train/test split of the iphoneDF
inTraining <- createDataPartition(iphone_smallmatrix$iphonesentiment, p = .70, list = FALSE)
training <- iphone_smallmatrix[inTraining,]
testing <- iphone_smallmatrix[-inTraining,]

# 10 fold cross validation 
fitControl <- trainControl(method = "cv", number = 10)

#--------- C5.0 training--------------
C50 <- train(iphonesentiment~., data = training, method = "C5.0", trControl=fitControl)
# Testing 
prediction_C50 <- predict(C50, testing)

plot(C50)


##--------- Random Forest Train--------------
rf <- train(iphonesentiment~., data = training, method = "rf", trControl=fitControl)
# Testing 
prediction_rf<- predict(rf, testing)

plot(rf)
##-------------SVM with 10-fold cross validation ------
svm <- train(iphonesentiment~., data = training, method = "svmLinear", trControl=fitControl)
# Testing 
prediction_svm<- predict(svm, testing)

plot(svm)
###------------- KKNN with 10-fold cross validation------
kknn <- train(iphonesentiment~., data = training, method = "kknn", trControl=fitControl)
# Testing 
prediction_kknn<- predict(kknn, testing)
plot(kknn)


#---------- Model Performance----------------

# Evaluate C5.0 Model
postResample(prediction_C50, testing$iphonesentiment)
# Evaluate RF Model
postResample(prediction_rf, testing$iphonesentiment)
# Evaluate SVM Model
postResample(prediction_svm, testing$iphonesentiment)
# Evaluate KKNN Model
postResample(prediction_kknn, testing$iphonesentiment)


#------Confusion Metrics---------------

# Create a confusion matrix from random forest predictions 
CMRF <- confusionMatrix(prediction_rf, testing$iphonesentiment) 
CMRF

CM_C50 <- confusionMatrix(prediction_C50, testing$iphonesentiment)
CM_C50


#-RF (Best performing model) for iphoneNZV,iphoneRFE,

# Define an 70%/30% train/test split of the iphoneNZV
inTraining_iphoneNZV <- createDataPartition(iphoneNZV$iphonesentiment, p = .70, list = FALSE)
training_NZV <- iphoneNZV[inTraining,]
testing_NZV <- iphoneNZV[-inTraining,]

# Apply RandomForest with 10-fold cross validation on iphoneNZV
rf_NZV <- train(iphonesentiment~., data = training_NZV, method = "rf", trControl=fitControl)

# Testing 
prediction_rf_NZV<- predict(rf_NZV, testing_NZV)

#---------------


# Define an 70%/30% train/test split of the iphoneRFE
inTraining_iphoneRFE <- createDataPartition(iphoneRFE$iphonesentiment, p = .70, list = FALSE)
training_RFE <- iphoneRFE[inTraining,]
testing_RFE <- iphoneRFE[-inTraining,]

# Apply RandomForest with 10-fold cross validation on iphoneRFE
rf_RFE <- train(iphonesentiment~., data = training_RFE, method = "rf", trControl=fitControl)
# Testing 
prediction_rf_RFE<- predict(rf_RFE, testing_RFE)

#---------------------- PostResample NZV RFE--------------

postResample(prediction_rf_NZV, testing_NZV$iphonesentiment)
postResample(prediction_rf_RFE, testing_RFE$iphonesentiment)


#-------------5.Feature Engineeering----------------------
#----------Recode--------------

iphoneRC <- iphone_smallmatrix

# Recode sentiment to combine factor levels 0 & 1 and 4 & 5
iphoneRC$iphonesentiment <- recode(iphoneRC$iphonesentiment, '0' = 1, '1' = 1, '2' = 2, '3' = 3, '4' = 4, '5' = 4) 

# Make iphonesentiment a factor
iphoneRC$iphonesentiment <- as.factor(iphoneRC$iphonesentiment)

str(iphoneRC$iphonesentiment)

# Define an 70%/30% train/test split of the iphoneRC
inTrainingRC <- createDataPartition(iphoneRC$iphonesentiment, p = .70, list = FALSE)
training_RC <- iphoneRC[inTraining,]
testing_RC <- iphoneRC[-inTraining,]

# Use The Best RandomForest with 10-fold cross validation on Recoding the Dependant variable
rf_RC <- train(iphonesentiment~., data = training_RC, method = "rf", trControl=fitControl)
# Testing 
prediction_rf_RC<- predict(rf_RC, testing_RC) 

# Evaluate the model
postResample(prediction_rf_RC,testing_RC$iphonesentiment)


#-----------------Principle Component Analysis-----------

# Data = training and testing from iphone_smallmatrix (no feature selection) 
# Excluded the dependent variable and set threshold to .95

preprocessParams <- preProcess(training[,-59], method=c("center", "scale", "pca"), thresh = 0.95)
print(preprocessParams)

# Use predict to apply pca parameters, create training, exclude dependant
train.pca <- predict(preprocessParams, training[,-59])

# Add the dependent to training
train.pca$iphonesentiment <- training$iphonesentiment

# Use predict to apply pca parameters, create testing, exclude dependant
test.pca <- predict(preprocessParams, testing[,-59])

# Add the dependent to testing
test.pca$iphonesentiment <- testing$iphonesentiment

# 10 fold cross validation 
fitControl <- trainControl(method = "cv", number = 10)

# Apply RandomForest Model with 10-fold cross validation on Principal Component Analysis 
rf_pca <- train(iphonesentiment~., data = train.pca, method = "rf", trControl=fitControl)

# Testing 
prediction_rf_pca<- predict(rf_pca, test.pca)

#-----Model Evaluation on PCA Parameter

# Evaluate the model
postResample(prediction_rf_pca, test.pca$iphonesentiment)

#stop cluster after performing tasks.
#stopCluster()

