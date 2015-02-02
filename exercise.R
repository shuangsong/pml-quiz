#note of practical machine learning
rm(list=ls())
library(caret)
data(spam)
library(kernlab)
library(ggplot2)
library(AppliedPredictiveModeling)
inTrain<-createDataPartition(y=spam$type, p=0.75, list=FALSE)
training<-spam[inTrain,]
testing<-spam[-inTrain,]
dim(training)
#fit a model
set.seed(32343)
#to be reproducible
modelFit<-train(type~., data=training, method="glm")
print(modelFit)
modelFit$finalModel
prediction<-predict(modelFit, newdata=testing)
summary(prediction)
#get table of spam and nonspam
table(prediction, testing$type)
#get accuracy 0.9243
confusionMatrix(prediction, testing$type)
#get feature plot
featurePlot(x=training[,c(1,2,3)], y=training$type, plot="pairs")
#or qplot
qplot(you, your, data=training)
qq+geom_smooth(method="lm", formula=you~your)



#procedure to do cross validation

#use training set
#split into training/testing set
#build model on training set
#evaluate on test set
#repeat and average estimated errors.
