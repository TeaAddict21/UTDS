#install packages and call the libraries

###
install.packages("caret", dependencies = c("Depends","Suggests"))
install.packages("AppliedPredictiveModeling")
install.packages("tidyverse")
install.packages("corrplot")


library(plyr)
library(dplyr)
library(caret)
library(AppliedPredictiveModeling)
library(ggplot2)
library(tidyverse)
library(randomForest)
library(corrplot)
library(gbm)
library(C50)
library(readr)
library(e1071)
library(mlbench)
library(randomForest)


####
set.seed(998)

ExistingProducts_2017 <- read_csv("existingproductattributes2017.csv",show_col_types = FALSE)
NewProducts_2017<- read_csv("newproductattributes2017.csv",show_col_types = FALSE)

spec(ExistingProducts_2017)
str(NewProducts_2017)

dmy <- dummyVars("~.", data=ExistingProducts_2017)
dataExisting <- data.frame(predict(dmy, newdata=ExistingProducts_2017 ))



dmy2 <-dummyVars("~.", data=NewProducts_2017)
dataNew <- data.frame(predict(dmy2, newdata=NewProducts_2017))



# Returns a value greater than 1 if a column has missing data

colSums(is.na(dataExisting))
colSums(is.na(dataNew))

# delete the feature that has null values
dataExisting$BestSellersRank <- NULL
dataNew$BestSellersRank <- NULL

#Find the correlation
corrData <- cor(dataExisting)
corrplot(corrData,
          method = "color",
          diag= FALSE,
          type = "upper",
          order = "FPC",
          addCoef.col = "black",
          )
 
#Select the features needed for the regression algorithms
 df <- dataExisting
 df<- select(dataExisting, ProductTypePC,
             ProductTypeSmartphone,
             ProductTypeLaptop,
             ProductTypeNetbook,
             x4StarReviews,
             x3StarReviews,
             x2StarReviews,
             x1StarReviews,
             PositiveServiceReview,
             NegativeServiceReview,
             Volume)
 
 df1<- dataNew
 df1<- select(dataNew, ProductTypePC,
             ProductTypeSmartphone,
             ProductTypeLaptop,
             ProductTypeNetbook,
             x4StarReviews,
             x3StarReviews,
             x2StarReviews,
             x1StarReviews,
             PositiveServiceReview,
             NegativeServiceReview,
             Volume)
 
 
 dataExisting <- df
 dataNew<- df1
 
 colnames(dataNew)
 colnames(dataExisting)
 
 #partitioning the data
 set.seed (825)
 
 trainIndex <- createDataPartition(y,p=0.75,list=FALSE)
 training<- dataExisting[trainIndex,]
 testing<- dataExisting[-trainIndex,]

 
  #4 fold cross validation
 fitControl<-trainControl(method="cv",
                          number=4 # number of folds
                          ) 
 
 
 #SVM Linear2 model
 set.seed (825)
 svmFit <- train(Volume~.,
                 data=training,
                 method= "svmLinear2",
                 trControl=fitControl,
                 scale = FALSE)
svmFit 
summary(svmFit)
svmPred <- predict(svmFit,testing )
postResample(svmPred,testing$Volume)
plot(svmFit)


#Random Forest Regression
set.seed(225)
rfFit <- train(Volume~.,
                data=training,
                method= "rf",
                trControl=fitControl)

rfFit 
summary(rfFit)
rfPred <- predict(rfFit,testing )
postResample(rfPred,testing$Volume)
plot(rfFit)

#GMB regression
gbmFit <- train(Volume~.,
                data=training,
                method="gbm",
                trControl= fitControl)
set.seed(325)
gbmFit 
summary(gbmFit)
gbmPred <- predict(gbmFit,testing )
postResample(gbmPred,testing$Volume)
plot(gbmFit)


# compare models :
resamps <- resamples(list(gbm= gbmFit,rf= rfFit, svm= svmFit))
resamps
summary(resamps)

# Final Prediction using random forest
set.seed(225)
dataNew$PredictedVolume <- predict(rfFit,dataNew )
finalPred = predict(rfFit, dataNew)

#Write the prediction to an output file
output <- dataNew
output$prediction <- finalPred
write.csv(output, file="Task3_output.csv", row.names = TRUE)
finalPred
view(finalPred)


#sales predictions for four target product types 
#found in the new product attributes data set


ProductType <- c("PC","Smartphone","Laptop","netbook")
Volume <- as.numeric.c(0,0,0,0)





df_ProductTypePC = dataNew %>% 
  filter(ProductTypePC == 1) %>% 
  group_by(ProductTypePC) %>%
  summarise(sum_Volume = sum(PredictedVolume)) %>%
  rename(ProductType = ProductTypePC) 

df_ProductTypeSmartPhone = dataNew %>% 
  filter(ProductTypeSmartphone == 1) %>% 
  group_by(ProductTypeSmartphone) %>%
  summarise(sum_Volume = sum(PredictedVolume)) %>%
  rename(ProductType = ProductTypeSmartphone) 

df_ProductTypeLaptop = dataNew %>% 
  filter(ProductTypeLaptop == 1) %>% 
  group_by(ProductTypeLaptop) %>%
  summarise(sum_Volume = sum(PredictedVolume)) %>%
  rename(ProductType = ProductTypeLaptop) 

df_ProductTypeNetbook = dataNew %>% 
  filter(ProductTypeNetbook == 1) %>% 
  group_by(ProductTypeNetbook) %>%
  summarise(sum_Volume = sum(PredictedVolume)) %>%
  rename(ProductType = ProductTypeNetbook) 



 Volume <- replace(Volume, 1, df_ProductTypePC$sum_Volume)
 Volume <- replace(Volume, 2, df_ProductTypeSmartPhone$sum_Volume)
 Volume <- replace(Volume, 3, df_ProductTypeLaptop$sum_Volume)
 Volume <- replace(Volume, 4, df_ProductTypeNetbook$sum_Volume)
 
 
 
 
 df_test <- data.frame(ProductType,Volume)
 view(df_test)
 sapply(df_test, class)
 transform(df_test,Volume = as.integer(Volume) )
 
 ggplot(df_test, aes(ProductType, Volume, fill= ProductType)) + geom_col()
 
 
 # chart that displays the impact of 
 #customer and service reviews have on sales volume.
 
 

 
 df_all <- dataNew %>% gather( key="Reviews", value = " PredictedVolume",
        x4StarReviews,x3StarReviews,x2StarReviews,x1StarReviews,
        PositiveServiceReview,NegativeServiceReview)

 ggplot(df_all, aes(Reviews, PredictedVolume, fill= Reviews)) + geom_col()



  







