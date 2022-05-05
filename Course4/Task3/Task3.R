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


# Find how many cores are on your machine
detectCores() 
# Create Cluster with desired number of cores. 
cl <- makeCluster(6)
# Register Cluster
registerDoParallel(cl)
# Confirm how many cores are now "assigned" to R and RStudio
getDoParWorkers()  




# load the dataset
training <- read.csv("trainingData.csv")

# check headings of the first 10 variables with the first 5 observations
head(training, n=5)[1:10]

# check headings of the last few variables with the first 3 observations
head(training, n=3)[520:529]



#-------------- find and take out zero variance variables-----------------
rzv_training <- training[ -which(apply(training, 2, var) == 0 )] 

# training has 529 variable where as rzv_taining has only 474 variables.

# check if all zero variance columns are removed
which(apply(rzv_training, 2, var) == 0)


#Remove latitude and longitude since we have relative positioning
names(rzv_training)[466:467]

# Remove userid, phoneID, timestamp
names(rzv_training)[472:474]

# Remove dependent variables (longitude, latitude, userid, phoneID, timestamp)
rzv_training <- rzv_training[, -c(466,467,472:474)]

#---------- Devise a single unique Identifier  ----------------------
rzv_training$LOCATION <- paste(rzv_training$BUILDINGID, rzv_training$FLOOR,
                               rzv_training$SPACEID, 
                               rzv_training$RELATIVEPOSITION, sep="")
# convert data type to factor  
rzv_training$LOCATION <- as.factor(rzv_training$LOCATION)
# make sure the data type has been converted
str(rzv_training$LOCATION)

#Restrict the models to individual buildings
unique(rzv_training$BUILDINGID)

#It has 0,1,2 buildings.Create a subset for each building

training_b0 <- subset(rzv_training, BUILDINGID == 0)
training_b1 <- subset(rzv_training, BUILDINGID == 1)
training_b2 <- subset(rzv_training, BUILDINGID == 2)
# remove individual location variables
training_b0[, c(467:470)] <- NULL
training_b1[, c(467:470)] <- NULL
training_b2[, c(467:470)] <- NULL
# applying factor() to avoid extra levels
training_b0$LOCATION <- factor(training_b0$LOCATION)
training_b1$LOCATION <- factor(training_b1$LOCATION)
training_b2$LOCATION <- factor(training_b2$LOCATION)
# check how many levels of LOCATION for building0
str(training_b0$LOCATION)


set.seed(101)

fitControl <- trainControl(method = "cv", number = 10)

#-------------For Building_b0 subset-------------------------

# split training and testing datasets for building 0 subset
inTraining_b0 <- createDataPartition(training_b0$LOCATION, p = .75, list = FALSE )
training_b0ss <- training_b0[inTraining_b0, ]
testing_b0ss <- training_b0[-inTraining_b0, ]

# C5.0 model
C50_b0 <- train(LOCATION~., data = training_b0ss, method = "C5.0", 
                trControl = fitControl)
# testing 
prediction_C50_b0 <- predict(C50_b0, testing_b0ss)

# random forest
rf_b0 <- train(LOCATION~., data = training_b0ss, method = "rf",
               trControl = fitControl)
prediction_rf_b0 <- predict(rf_b0, testing_b0ss)

# KNN
KNN_b0 <- train(LOCATION~., data = training_b0ss, method = "knn",
                trControl = fitControl)

prediction_KNN_b0 <- predict(KNN_b0, testing_b0ss)


#----------------For Building_b1 subset--------------------------

# split training and testing datasets for building 0 subset
inTraining_b1 <- createDataPartition(training_b1$LOCATION, p = .75, list = FALSE )
training_b1ss <- training_b1[inTraining_b1, ]
testing_b1ss <- training_b1[-inTraining_b1, ]

# C5.0 model
C50_b1 <- train(LOCATION~., data = training_b1ss, method = "C5.0", 
                trControl = fitControl)
# testing 
prediction_C50_b1 <- predict(C50_b1, testing_b1ss)

# random forest
rf_b1 <- train(LOCATION~., data = training_b1ss, method = "rf",
               trControl = fitControl)
prediction_rf_b1 <- predict(rf_b1, testing_b1ss)

# KNN
KNN_b1 <- train(LOCATION~., data = training_b1ss, method = "knn",
                trControl = fitControl)

prediction_KNN_b1 <- predict(KNN_b1, testing_b1ss)

#-----------------For Building_b2--------------------------

# split training and testing datasets for building 0 subset
inTraining_b2 <- createDataPartition(training_b2$LOCATION, p = .75, list = FALSE )
training_b2ss <- training_b2[inTraining_b2, ]
testing_b2ss <- training_b2[-inTraining_b2, ]

# C5.0 model
C50_b2 <- train(LOCATION~., data = training_b2ss, method = "C5.0", 
                trControl = fitControl)
# testing 
prediction_C50_b2 <- predict(C50_b2, testing_b2ss)

# random forest
rf_b2 <- train(LOCATION~., data = training_b2ss, method = "rf",
               trControl = fitControl)
prediction_rf_b2 <- predict(rf_b2, testing_b2ss)

# KNN
KNN_b2 <- train(LOCATION~., data = training_b2ss, method = "knn",
                trControl = fitControl)

prediction_KNN_b2 <- predict(KNN_b2, testing_b2ss)

#-----------------Accuracy and Kappa for Building_0 (b_0)------

# evaluate C5.0 
cm_C50_b0 <- confusionMatrix(prediction_C50_b0, testing_b0ss$LOCATION)
postResample(prediction_C50_b0, testing_b0ss$LOCATION)

# evaluate Random Forest
cm_rf_b0 <- confusionMatrix(prediction_rf_b0, testing_b0ss$LOCATION)
postResample(prediction_rf_b0, testing_b0ss$LOCATION)

# evaluate KNN
cm_KNN_b0 <- confusionMatrix(prediction_KNN_b0, testing_b0ss$LOCATION)
postResample(prediction_KNN_b0, testing_b0ss$LOCATION)

# resample for all three models 
resample_b0 <- resamples( list(C50 = C50_b0, RF = rf_b0, KNN = KNN_b0))
summary(resample_b0)

#------------------Accuracy and Kappa for Building_1(b1)----------

# evaluate C5.0 
cm_C50_b1 <- confusionMatrix(prediction_C50_b1, testing_b1ss$LOCATION)
postResample(prediction_C50_b1, testing_b1ss$LOCATION)

# evaluate Random Forest
cm_rf_b1 <- confusionMatrix(prediction_rf_b1, testing_b1ss$LOCATION)
postResample(prediction_rf_b1, testing_b1ss$LOCATION)

# evaluate KNN
cm_KNN_b1 <- confusionMatrix(prediction_KNN_b1, testing_b1ss$LOCATION)
postResample(prediction_KNN_b1, testing_b1ss$LOCATION)

# resample for all three models 
resample_b1 <- resamples( list(C50 = C50_b1, RF = rf_b1, KNN = KNN_b1))
summary(resample_b1)

#----------------------------Accuracy and Kappa for Building_1(b1)----------

# evaluate C5.0 
cm_C50_b2 <- confusionMatrix(prediction_C50_b2, testing_b2ss$LOCATION)
postResample(prediction_C50_b2, testing_b2ss$LOCATION)

# evaluate Random Forest
cm_rf_b2 <- confusionMatrix(prediction_rf_b2, testing_b2ss$LOCATION)
postResample(prediction_rf_b2, testing_b2ss$LOCATION)

# evaluate KNN
cm_KNN_b2 <- confusionMatrix(prediction_KNN_b2, testing_b2ss$LOCATION)
postResample(prediction_KNN_b2, testing_b2ss$LOCATION)

# resample for all three models 
resample_b2 <- resamples( list(C50 = C50_b2, RF = rf_b2, KNN = KNN_b2))
summary(resample_b2)

#----------------------------

# Stop Cluster. (After performing tasks, stop the cluster.)
#stopCluster(cl)
