
#q1
#Which of the following commands will create training and test sets with about 
#50% of the observations assigned to each? 
adData = data.frame(diagnosis,predictors)
trainIndex = createDataPartition(diagnosis, p = 0.50,list=FALSE)
training = adData[trainIndex,]
testing = adData[-trainIndex,]


#q2
#Make a plot of the outcome (CompressiveStrength) versus the index of the samples. 
#Color by each of the variables in the data set (you may find the cut2() function 
#in the Hmisc package useful for turning continuous covariates into factors). 
#What do you notice in these plots? 
rm(list=ls())
library(ggplot2)
library(lattice)
library(AppliedPredictiveModeling)
library(caret)
data(AlzheimerDisease)
data(concrete)
set.seed(975)
inTrain = createDataPartition(mixtures$CompressiveStrength, p = 3/4)[[1]]
training = mixtures[ inTrain,]
testing = mixtures[-inTrain,]
qplot(CompressiveStrength, Cement, data=concrete)
index<-colnames(concrete[,c(1,2,3,4,5,6,7)])
featurePlot(x=training[,index], y=training$CompressiveStrength, plot="pairs")
#There is a step-like pattern in the plot of outcome versus index in the training
#set that isn't explained by any of the predictor variables so there may be 
#a variable missing.


#q3
#Make a histogram and confirm the SuperPlasticizer variable is skewed. 
#Normally you might use the log transform to try to make the data more symmetric.
#Why would that be a poor choice for this variable?
rm(list=ls())
library(AppliedPredictiveModeling)
data(concrete)
library(caret)
set.seed(975)
inTrain = createDataPartition(mixtures$CompressiveStrength, p = 3/4)[[1]]
training = mixtures[ inTrain,]
testing = mixtures[-inTrain,]
qplot(Superplasticizer, data=training) 
#There are values of zero so when you take the log() transform those values 
#will be -Inf.

#q4
#Find all the predictor variables in the training set that begin with IL. 
#Perform principal components on these variables with the preProcess()
#function from the caret package. Calculate the number of principal components
#needed to capture 80% of the variance. How many are there? 
library(caret)
library(AppliedPredictiveModeling)
set.seed(3433)
data(AlzheimerDisease)
adData = data.frame(diagnosis,predictors)
inTrain = createDataPartition(adData$diagnosis, p = 3/4)[[1]]
training = adData[ inTrain,]
testing = adData[-inTrain,]
ss <- training[,grep('^IL', x = names(training))]
preProc <- preProcess(ss, method='pca', thresh=0.9,
                      outcome=training$diagnosis)
preProc
preProc$rotation
#9


#q5
#Create a training data set consisting of only the predictors with variable
#names beginning with IL and the diagnosis. Build two predictive models, 
#one using the predictors as they are and one using PCA with principal components 
#explaining 80% of the variance in the predictors. Use method="glm" in the train 
#function. What is the accuracy of each method in the test set? Which is more accurate? 
set.seed(3433)
## grep the predictors starting with 'IL'
IL_str <- grep("^IL", colnames(training), value = TRUE)
## make a subset of these predictors
predictors_IL <- predictors[, IL_str]
df <- data.frame(diagnosis, predictors_IL)
inTrain = createDataPartition(df$diagnosis, p = 3/4)[[1]]
training = df[inTrain, ]
testing = df[-inTrain, ]
## train the data using the first method
modelFit <- train(diagnosis ~ ., method = "glm", data = training)




















