#1
#1. Subset the data to a training set and testing set based on the Case variable in the 
#data set.
#2. Set the seed to 125 and fit a CART model with the rpart method using all predictor
#variables and default caret settings.
#3. In the final model what would be the final model prediction for cases with the
#following variable values:
#a. TotalIntench2 = 23,000; FiberWidthCh1 = 10; PerimStatusCh1=2
#b. TotalIntench2 = 50,000; FiberWidthCh1 = 10;VarIntenCh4 = 100
#c. TotalIntench2 = 57,000; FiberWidthCh1 = 8;VarIntenCh4 = 100
#d. FiberWidthCh1 = 8;VarIntenCh4 = 100; PerimStatusCh1=2 

library(AppliedPredictiveModeling)
data(segmentationOriginal)
library(caret)
library(rpart)
#head(segmentationOriginal)
training<-segmentationOriginal[segmentationOriginal$Case=="Train",]
testing<-segmentationOriginal[segmentationOriginal$Case=="Test",]
dim(training)
set.seed(125)
modelFit<-train(Class~., method="rpart", data=training)
modelFit$finalModel
prediction1<-predict(modelFit, training)

#a. PS
#b. WS
#c. PS
#d. Not possible to predict 


#2
#k-fold:
#If K is small in a K-fold cross validation is the bias in the estimate of 
#out-of-sample (test set) accuracy smaller or bigger? If K is small is the 
#variance in the estimate of out-of-sample (test set) accuracy smaller or bigger. 
#Is K large or small in leave one out cross validation? 

#bias larger and variance smaller, equals to sample size


#3
#(NOTE: If you have trouble installing the pgmm package, you can download 
#the olive dataset here: olive_data.zip. After unzipping the archive, 
#you can load the file using the load() function in R.)

#These data contain information on 572 different Italian olive oils from 
#multiple regions in Italy. Fit a classification tree where Area is the outcome 
#variable. Then predict the value of area for the following data frame using the 
#ree command with all defaults.
#What is the resulting prediction? Is the resulting prediction strange? Why or why not?
library(pgmm)
library(caret)
library(AppliedPredictiveModeling)
data(olive)
olive = olive[,-1]
newdata = as.data.frame(t(colMeans(olive)))
inTrain<-createPartition(y=olive$Area, p=0.7, list=FALSE)
training<-olive[inTrain,]
testing<-olive[-inTrain,]
dim(training)
dim(testing)
set.seed(125)
modFit<-train(Area~., method="rpart",data=training)
print(modFit$finalModel)
predict(modFit, testing)
predict(modFit, newdata=as.data.frame(t(colMeans(olive))))

#2.875. It is strange because Area should be a qualitative variable - but 
#tree is reporting the average value of Area as a numeric variable in the 
#leaf predicted for newdata

#4
#Then set the seed to 13234 and fit a logistic regression model (method="glm", 
#be sure to specify family="binomial") with Coronary Heart Disease (chd) as the 
#outcome and age at onset, current alcohol consumption, obesity levels, cumulative
#tabacco, type-A behavior, and low density lipoprotein cholesterol as predictors.
#Calculate the misclassification rate for your model using this function and a 
#prediction on the "response" scale: 
#What is the misclassification rate on the training set? What is the misclassification
#rate on the test set? 
library(ElemStatLearn)
library(randomForest)
library(kernlab)
data(SAheart)
set.seed(8484)
train = sample(1:dim(SAheart)[1],size=dim(SAheart)[1]/2,replace=F)
trainSA = SAheart[train,]
testSA = SAheart[-train,]
set.seed(13234)
model<-train(chd~age+alcohol+obesity+tobacco+typea+ldl, data=trainSA, method="glm", 
             family="binomial")
model$finalModel
prediction1<-predict(model, newdata=testSA)
prediction2<-predict(model, newdata=trainSA)
missClass = function(values,prediction){sum(((prediction > 0.5)*1) 
                                            != values)/length(values)}
missClass(testSA$chd, prediction1)
missClass(trainSA$chd, prediction2)
#Test Set Misclassification: 0.31
#Training Set: 0.27

#5
#Set the variable y to be a factor variable in both the training and test set.
#Then set the seed to 33833. Fit a random forest predictor relating the factor 
#variable y to the remaining variables. Read about variable importance in random 
#forests here: http://www.stat.berkeley.edu/~breiman/RandomForests/cc_home.htm#ooberr
#The caret package uses by defualt the Gini importance. Calculate the variable 
#importance using the varImp function in the caret package. What is the order of 
#variable importance?
library(caret)
library(ElemStatLearn)
library(kernlab)
library(AppliedPredictiveModeling)
library(randomForest)
library(ggplot2)
library(Metrics)
data(vowel.train)
data(vowel.test) 
vowel.train$y<-factor(vowel.train$y)
vowel.test$y<-factor(vowel.test$y)
set.seed(33833)
tr<-trainControl(method="cv", number=5)
model<-train(y ~., method="rf", data=vowel.train, trControl=tr)
plot(varImp(model))

#The order of the variables is:
 #x.2, x.1, x.5, x.6, x.8, x.4, x.9, x.3, x.7,x.10
















