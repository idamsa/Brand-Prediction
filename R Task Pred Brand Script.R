library(caret)
library(ggplot2)
library(readr)
library(outliers)
library(OneR)
library(Hmisc)
library(C50)

#bringing the data_____________________________________________________________________________

complete<-read.csv("CompleteResponses.csv")

#about the data______________________________________________________________________________

summary(complete)
str(complete)
nrow(complete)

#normally distributed?
shapiro.test(complete$salary) ###data is not normally distributed

#PLOTTING THE DATA exploratory________________________________________________________________

hist(complete$credit, freq = F)
hist(complete$age, freq = F) 
hist(complete$salary, freq = FALSE)

hist(complete$salary,probability=T, main="data distrbution
",xlab="salary")
lines(density(complete$salary),col=2)

#salary hist

ggplot(complete,aes(x=salary))+
  geom_histogram(bins = 30,color="black",fill="cyan2")+
  ggtitle("The Distribution of the Salary")+
  scale_x_continuous(name="Salary",breaks=seq(20000,500000,50000))


#credit hist

ggplot(complete,aes(x=credit))+
  geom_histogram(binwidth  =20000,color="black",fill="cyan2")+
  ggtitle("The Distribution of the Credit")+
  scale_x_continuous(name="Credit", breaks=seq(0,500000,50000))

#brand only related bar/pie 

ggplot(complete,aes(x=Preferedbrand, fill=Preferedbrand))+
  geom_bar()+
  labs(title="Prefered Brands by Number")+
  theme_light()


ggplot(complete, aes(x="",fill=Preferedbrand))+
  geom_bar()+ 
  coord_polar(theta="y")+
  labs(x=" ",y=" ", title = "Proportion of Prefered Brands")+
  theme_light()

#Relation age and pref scatter

ggplot(complete, aes(x=age, y=salary, col=Preferedbrand)) + 
  geom_point()+
  labs(title="Relationship Between Age, Salary and Brand Preference")



#FEATURE SELECTION ___________________________________________________________________FEATURE SELECTION

#corr matrix, we see car and elevel highly corr we will have to get rid of one of them at least
#also age and salary low corelated
res2 <- rcorr(as.matrix(complete))

res2

#removing car,elevels,credit attributes
complete <- complete[c(1,2,7)]
str(complete)

#_________________________________________________________________________________________
#PREPROCESSING

#normalizing the age and salary

normalize <- function(x) {
  return ((x - min(x)) / (max(x) - min(x)))
}
 
complete$salary<-normalize(complete$salary)
complete$age<-normalize(complete$age)

#transforming from numeric to factors

complete$brand<-as.factor(complete$brand)
levels(complete$brand) <- c("Acer","Sony")
str(complete)


#finding missing values
sum(is.na(complete)) #no missing values


#finding outliers in salary
outlier_tfSalary = outlier(complete$salary,logical=TRUE)
sum(outlier_tfSalary)

#what were the ouliers in salary
find_outlierSalary = which(outlier_tfSalary==TRUE,arr.ind=TRUE)
#Removing the outliers in salary
complete= complete[-find_outlierSalary,]
nrow(complete)

#binning the salary

bin(
  complete$salary,
  nbins = 5, 
  labels = NULL, 
  method = c( "content")
)

str(complete)



#_________________________________________________________________________________SPLITTING THE DATA
#SPLITTING THE DATA
#creating the trainig and testing sets
set.seed(123)

inTrain<- createDataPartition(y= complete$brand,p=.75, list = FALSE) 

str(inTrain)

#partitioning the data

training <- complete[ inTrain,]
testing <- complete[-inTrain,]
nrow(training)
nrow(testing)



head(testing)
head(training)
str(testing)

#decision tree on the data for feature importance__________________________________________________DECISION TREE ONLY

library(rpart)

rpartMod = rpart(brand ~ .
                 , data = training
                 , control = rpart.control(minsplit = 5
                                           , cp = 0
                                           , maxdepth = 3)
)

pred = predict(rpartMod, testing, type = "class")
sum(testing[, 5] != pred)

#visualize decision tree# we see that the tree chose salary and age as the nodes

library(rattle)
fancyRpartPlot(rpartMod)

#___________________________________________________________________________________________BUILDING THE MODELS________
#BUILGING THE MODELS
#traincontrol-modifies resampling method


ctrl <- trainControl(
                     method = "repeatedcv",
                     number = 10,
                     repeats = 3,
                     summaryFunction = multiClassSummary,
                     classProbs = TRUE
                     )

#____________________________________________________________________________________DECISION TREE

#C5.0 Decision Tree

set.seed(123)
treeFit <- train(
                 brand~.,
                 data = training,
                 method = "C5.0", 
                 trControl=ctrl,
                 tuneLength = 3,
                )

treeFit
varImp(treeFit)

#PREDICT THE BRAND
treeBrand <- predict(treeFit, newdata = testing)

#computes class probabilities for the model
treeProbs <- predict(treeFit, newdata = testing, type = "prob")
treeProbs

#confusion matrix and the statistics

confusionMatrix(data= treeBrand, testing$brand)

#plot predicted vs observed tree


summary(treeFit)

#________________________________________________________________________________________RANDOM FOREST

#Random Forest

ctrl <- trainControl(
                     method = "repeatedcv",
                     number = 10,
                     repeats = 3,
                     summaryFunction = multiClassSummary
                    )


#mtry
mtry <- c(2,1,3,5,12,32)
rfGrid <- expand.grid(.mtry=mtry)

set.seed(123)


#train the model
system.time(
          randomForestFit <- train(
                                   brand~.,
                                   data = training,
                                   preProcess = c("center"),
                                   method = "rf", 
                                   trControl=ctrl,
                                   tuneGrid=rfGrid,
                                   importance=T
                                  )
            )

randomForestFit
varImp(randomForestFit)

#PREDICT THE BRAND
rfBrand <- predict(randomForestFit, newdata = testing)

#computes class probabilities for the model
rfProbs <- predict(randomForestFit, newdata = testing, type = "prob")
rfProbs

#confusion matrix for the random forest model
confusionMatrix(data= rfBrand, testing$brand)


#importance

plot(varImp(randomForestFit))

summary(randomForestFit)


####_______________________________________________COMPARING THE MODELS

resamps <- resamples(list(tree = treeFit, forest = randomForestFit))
summary(resamps)

#visualize the comparation 
xyplot(resamps, what = "BlandAltman")

dotplot(resamps)

bwplot(resamps)

#For each resample, there are paired results a paired t–test can be used to assess whether there
#is a difference in the average resampled area under the ROC curve

diffs <- diff(resamps)
summary(diffs)
plot.train(treeFit)
#rf better

###APPLYING THE RandomTree MODEL ON THE INCOMPLETE DATA_____________________________________________________


incompletef <- read.csv("SurveyIncomplete.csv")
incomplete

#feature sel

incomplete<- incompletef[c(1,2,7)]

#preprocessing

incomplete$salary<-normalize(incomplete$salary)
incomplete$age<-normalize(incomplete$age)
incomplete$brand<-as.factor(incomplete$brand)
levels(incomplete$brand) <- c("Acer","Sony")
str(incomplete)

bin(
  incomplete$salary,
  nbins = 5, 
  labels = NULL, 
  method = c( "content")
)

str(incomplete)

#apllying model

finalPredictionBrand <- predict(treeFit, newdata = incomplete, )

finalPredictionBrandProbs <- predict(treeFit, newdata = incomplete, type = "prob" )

finalPredictionBrandProbs


#summary predictions

summary(finalPredictionBrand)


#construct the csv with the predicted values

completeNewData <- incompletef[c(1,2,3,4,5,6)]
completeNewData <- cbind(completedNewData, predictedBrand = finalPredictionBrand)
completeNewData
write.csv(completeNewData, "PredictedBrands.csv")

