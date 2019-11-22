library(caret)
library(ggplot2)
library(readr)
library(outliers)
library(OneR)
library(Hmisc)
library(C50)

#bringing the data
complete<-read.csv("CompleteResponses.csv")

#abouth the data
summary(complete)
str(complete)
nrow(complete)

#corr matrix, we see car and elevel highly corr we will have to get rid of one of them
res2 <- rcorr(as.matrix(complete))
res2


#removing car attribute
complete <- complete[c(1,2,3,5,6,7)]
str(complete)

#PREPROCESSING

#transforming from numeric to factors
#complete$brand<-as.factor(complete$brand)

complete$elevel<-as.factor(complete$elevel) 
complete$zipcode<-as.factor(complete$zipcode)
complete$brand<-as.factor(complete$brand)
levels(complete$brand) <- c("A","S")
str(complete)

#finding missing values
sum(is.na(complete)) #no missing values

#finding outliers in credit
outlier_tfCredit = outlier(complete$credit,logical=TRUE)
sum(outlier_tfCredit)

#what were the ouliers in credit
find_outlierCredit = which(outlier_tfCredit==TRUE,arr.ind=TRUE)
#Removing the outliers in credit
complete= complete[-find_outlierCredit,]
nrow(complete)


#finding outliers in salary
outlier_tfSalary = outlier(complete$credit,logical=TRUE)
sum(outlier_tfSalary)

#what were the ouliers in credit
find_outlierSalary = which(outlier_tfSalary==TRUE,arr.ind=TRUE)
#Removing the outliers in credit
complete= complete[-find_outlierSalary,]
nrow(complete)




#binning the credit and salary
bin(
  complete$credit,
  nbins = 5, 
  labels = NULL, 
  method = c( "content")
  )

bin(
  complete$salary,
  nbins = 5, 
  labels = NULL, 
  method = c( "content")
)

str(complete)


#creating the trainig and testing sets
set.seed(123)

inTrain<- createDataPartition(y= complete$brand,p=.75, list = FALSE) 

str(inTrain)

#partitioning the data

training <- complete[ inTrain,]
testing <- complete[-inTrain,]
nrow(training)
nrow(testing)



testing
training
str(testing)

#traincontrol-modifies resampling method

ctrl <- trainControl(
  method = "repeatedcv",
  number = 10,
  repeats = 3,
  summaryFunction = multiClassSummary

)


treeFit <- train(
  brand~.,
  data = training,
  preProcess = c("range"),
  method = "C5.0", 
  trControl=ctrl,
  metric = "Accuracy",
  tuneLength = 3

)

treeFit

#PREDICT THE BRAND
treeBrand <- predict(treeFit, newdata = testing)

#computes class probabilities for the model
treeProbs <- predict(treeFit, newdata = testing, type = "prob")


#confusion matrix and the statistics

confusionMatrix(data= treeBrand, testing$brand)


#decision tree on the data
library(rpart)

rpartMod = rpart(brand ~ .
                , data = training
                , control = rpart.control(minsplit = 5
                , cp = 0
                , maxdepth = 4)
                )

pred = predict(rpartMod, testing, type = "class")
sum(testing[, 5] != pred)

#visualize decision tree
library(rattle)
fancyRpartPlot(rpartMod)

