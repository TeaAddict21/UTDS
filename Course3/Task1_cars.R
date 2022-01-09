#install.packages("readr")
library(readr)
#data<-read.table("cars.csv",header= TRUE,sep = ",")
CarData<-read.csv("cars.csv")
attributes(CarData)
summary(CarData) # Gives statistics (min, mean, max)
str(CarData) # Gives data types
names(CarData) # gives the column names
CarData$name
hist(CarData$speed)
plot(CarData$distance,CarData$speed)
qqnorm(CarData$speed)
set.seed(123)
trainSize<-round(nrow(CarData)*0.7)
testSize<-nrow(CarData)-trainSize
trainSize
testSize
training_indices<-sample(seq_len(nrow(CarData)),size=trainSize)
trainSet<-CarData[training_indices,] 
testSet<-CarData[-training_indices,]
lmMod<-lm(distance~speed,data=CarData)
summary(lmMod)
distPred<-predict(lmMod,CarData)
distPred
