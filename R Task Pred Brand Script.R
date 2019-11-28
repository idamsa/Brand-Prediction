##################################################################
# Task 2 Part 2 of Course
# Predict which Brand of Products Customers Prefer
##################################################################


#loading the libraies needed

library(caret)
library(ggplot2)
library(readr)
library(outliers)
library(OneR)
library(Hmisc)
library(C50)
library(rattle)
library(rpart)

# Reading the file

complete <- read.csv("CompleteResponses.csv")

# Data summary

summary(complete)

# Data structure
str(complete)


# Shapiro test to check if the data is normally distributed
shapiro.test(complete$salary) 

####################################################
# Exploratory plotting of the data
####################################################

# Histograms on some of the variables checking for the distribution

hist(complete$credit, freq = F)
hist(complete$age, freq = F) 
hist(complete$salary, freq = FALSE)

# Histogram of the salary with the distribution line superpossed

hist(complete$salary,probability=T, main="data distrbution
",xlab="salary")
lines(density(complete$salary),col=2)

# Histogram od the salary with ggplot

ggplot(complete,aes(x=salary))+
  geom_histogram(bins = 30,color="black",fill="cyan2")+
  ggtitle("The Distribution of the Salary")+
  scale_x_continuous(name="Salary",breaks=seq(20000,500000,50000))


# Histogram od the credit with ggplot

ggplot(complete,aes(x=credit))+
  geom_histogram(binwidth  =20000,color="black",fill="cyan2")+
  ggtitle("The Distribution of the Credit")+
  scale_x_continuous(name="Credit", breaks=seq(0,500000,50000))

# Barchart of the two brands

ggplot(complete,aes(x=Preferedbrand, fill=Preferedbrand))+
  geom_bar()+
  labs(title="Prefered Brands by Number")+
  theme_light()

# Piechart of the brands

ggplot(complete, aes(x="", fill=Preferedbrand))+
  geom_bar() + 
  coord_polar(theta="y") +
  labs(x=" ",y=" ", title = "Proportion of Prefered Brands") +
  theme_light()

# Scatter plot checking the relation between age, salary and brand

ggplot(complete, aes(x=age, y=salary, col=Preferedbrand)) + 
  geom_point()+
  labs(title="Relationship Between Age, Salary and Brand Preference")


#######################################################
#FEATURE SELECTION
#######################################################

# Running a correlation matrix on the data checking for highly correlated features

print(res2 <- rcorr(as.matrix(complete)))

# Removing unecessary features

complete <- complete[c(1,2,7)]

#######################################################
#PREPROCESSING
######################################################

# Normalizing the age and salary

normalize <- function(x) {
  return ((x - min(x)) / (max(x) - min(x))) # creates the normalization formula
} 
 

complete$salary <- normalize(complete$salary)
complete$age <- normalize(complete$age)

# Transforming from numeric to factors and assigning the levels

complete$brand <- as.factor(complete$brand)
levels(complete$brand) <- c("Acer","Sony")


# Checking for missing values

print(sum(is.na(complete)))

# Finding outliers in salary 

outlier_tfSalary <- outlier(complete$salary,logical = TRUE)
sum(outlier_tfSalary)

# Assigning the outliers to a vector

find_outlierSalary <- which(outlier_tfSalary == TRUE, arr.ind = TRUE)

# Removing the outliers in salary

complete= complete[-find_outlierSalary,]
nrow(complete)

# Binning the salary

bin(
  complete$salary,
  nbins = 5, 
  labels = NULL, 
  method = c( "content")
)


####################################################
#SPLITTING THE DATA
####################################################

# Creating the trainig and testing sets

set.seed(123)

inTrain <- createDataPartition(y= complete$brand,p=.75, list = FALSE) 

# Partitioning the data

training <- complete[ inTrain,]
testing <- complete[-inTrain,]


# Decision tree on the data to check how and where it splits

rpartMod = rpart(brand ~ .
                 , data = training
                 , control = rpart.control(minsplit = 5
                                           , cp = 0
                                           , maxdepth = 3)
)

pred = predict(rpartMod, testing, type = "class")
sum(testing[, 5] != pred)

# Plotting the decision tree

fancyRpartPlot(rpartMod)

####################################################################
# BUILDING ,TRAINING AND TESTING THE MODELS
####################################################################

# Setting the sampling methos Cross Validation


ctrl <- trainControl(
                     method = "repeatedcv",
                     number = 10,
                     repeats = 3,
                     summaryFunction = multiClassSummary,
                     classProbs = TRUE
                     )

# Training the C5.0 Decision Tree

set.seed(123)
treeFit <- train(
                 brand~.,
                 data = training,
                 method = "C5.0", 
                 trControl=ctrl,
                 tuneLength = 3,
                )

# Predicting the brand using the Decision Tree

treeBrand <- predict(treeFit, newdata = testing)

# Building the confusion matrix 

confusionMatrix(data= treeBrand, testing$brand)


# Training the Random Forest

ctrl <- trainControl(
                     method = "repeatedcv",
                     number = 10,
                     repeats = 3,
                     summaryFunction = multiClassSummary
                    )


# Setting the mtry
mtry <- c(2,1,3,5,12,32)
rfGrid <- expand.grid(.mtry=mtry)

set.seed(123)


# Training the Random Forest the model

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


# Predicting the brand using the Random Forest

rfBrand <- predict(randomForestFit, newdata = testing)

# Compute class probabilities for the model

rfProbs <- predict(randomForestFit, newdata = testing, type = "prob")
rfProbs

# Compute the confusion matrix for the random forest model

confusionMatrix(data= rfBrand, testing$brand)


##########################################################################
# COMPARING THE MODELS
############################################################################

# Resampling 
resamps <- resamples(list(tree = treeFit, forest = randomForestFit))

summary(resamps)

# Visualizations of  the comparation 

xyplot(resamps, what = "BlandAltman")

dotplot(resamps)

bwplot(resamps)


diffs <- diff(resamps)
summary(diffs)

#####################################################################
# APPLYING THE RandomTree MODEL ON THE INCOMPLETE DATA
#####################################################################

incompletef <- read.csv("SurveyIncomplete.csv")
incomplete

# Feature selection

incomplete <- incompletef[c(1,2,7)]

# Preprocessing

incomplete$salary <- normalize(incomplete$salary)
incomplete$age <- normalize(incomplete$age)

incomplete$brand <- as.factor(incomplete$brand)
levels(incomplete$brand) <- c("Acer","Sony")


bin(
  incomplete$salary,
  nbins = 5, 
  labels = NULL, 
  method = c( "content")
)


# Apllying the trained model model

finalPredictionBrand <- predict(treeFit, newdata = incomplete, )

# Summary predictions

summary(finalPredictionBrand)


# Construct the csv with the predicted values added 

completeNewData <- incompletef[c(1,2,3,4,5,6)]
completeNewData <- cbind(completedNewData, predictedBrand = finalPredictionBrand)
write.csv(completeNewData, "PredictedBrands.csv")

