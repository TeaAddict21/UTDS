install.packages("readr")
install.packages("dplyr") 
install.packages("ggplot2")
library("readr")
library(dplyr)

IrisDataset <- read.csv("iris.csv")

attributes(IrisDataset) #gives column names,class name, and row name

summary(IrisDataset) # gives statistical summary of each column of the data

str(IrisDataset) # structure of the data (column name, data type, sample values)

names(IrisDataset) # Prints the names of the columns

IrisDataset$Species  # prints the values of the column Species

unique(IrisDataset[c("Species")]) # Added this to see the unique values
IrisDataset2<-IrisDataset

#Replacing a char value with a numeric value to display the data in histogram.
IrisDataset2<-IrisDataset2%>% mutate(Species=replace(Species, Species== "setosa",11))
IrisDataset2<-IrisDataset2%>% mutate(Species=replace(Species, Species== "versicolor",22))
IrisDataset2<-IrisDataset2%>% mutate(Species=replace(Species, Species== "virginica",33))
unique(IrisDataset2[c("Species")])

IrisDataset2$Species<-as.numeric(IrisDataset2$Species) # converts the datatype to numeric


hist(IrisDataset2$Species) # need to be fixed


plot(IrisDataset$Sepal.Length)

unique(IrisDataset[c("Petal.Length")])

plot(IrisDataset$Petal.Length,IrisDataset$Petal.Width,pch=8, col= c("red","blue","green3"))

#qqnorm(IrisDataset) #Use qqnorm on individual column instead of whole dataset

     
#IrisDataset$Species<- as.numeric(IrisDataset$Species) # need to be fixed
#qqnorm(IrisDataset$Species)
set.seed(123)
     
# trainSize <- round(nrow(IrisDataset) * 0.2)
# Normally the 70-80% of the data would be allocated to training purpose

trainSize <- round(nrow(IrisDataset) * 0.7)
  
testSize <- nrow(IrisDataset) - trainSize
     
trainSize
     
testSize
     
trainSet <- IrisDataset[training_indices, ]
     
testSet <- IrisDataset[-training_indices, ]
     
set.seed(405)
     
trainSet <- IrisDataset[training_indices, ]
     
testSet <- IrisDataset[-training_indices, ]

     

#LinearModel<- lm(paste(trainSet$Petal.Width ~ testSet$Petal.Length))

# lm(Y~X,datasource)
LinearModel<-lm(Petal.Length~Petal.Width,data=IrisDataset)

methods(class="lm")
coefficients(LinearModel)
     
     summary(LinearModel)
     
     #prediction<-predict(LinearModeltestSet)
     prediction<-predict(LinearModel)
     
     #predictions
     prediction
     
     plot(Petal.Length~Petal.Width,data=IrisDataset)
     abline(LinearModel)
     
     
     library(ggplot2)
     ggplot(IrisDataset, aes(x=Petal.Width, y= Petal.Length)) +geom_point()+ 
       stat_smooth(method="lm", col="red")
     
     