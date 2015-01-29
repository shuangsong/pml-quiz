#1

#Set the variable y to be a factor variable in both the training and test set. 
#Then set the seed to 33833. Fit (1) a random forest predictor relating the factor
#variable y to the remaining variables and (2) a boosted predictor using the "gbm" method. Fit these both with the train() command in the caret package.

#What are the accuracies for the two approaches on the test data set? 
#What is the accuracy among the test set samples where the two methods agree? 
library(ElemStatLearn)
library(caret)
library(randomForest)
data(vowel.train)
data(vowel.test) 
vowel.train$y<-factor(vowel.train$y)
vowel.test$y<-factor(vowel.test$y)
set.seed(33833)
tr<-trainControl(method="cv", number=5)
model1<-train(y ~., method="rf", data=vowel.train, trControl=tr)
model2<-train(y ~., method="gbm", data=vowel.train)
pred1<-predict(model1, newdata=vowel.test)
pred2<-predict(model2, newdata=vowel.test)
comb <- data.frame(pred1, pred2, y = vowel.test$y)
#comb_fit <- train(y ~ ., data = comb, method = "gam")
comb_fit <- train(y ~ ., data = comb, method = "rf")
pred3 <- predict(comb_fit, newdata = vowel.test)
# confusion matrixes
c1 <- confusionMatrix(pred1, vowel.test$y)
c2 <- confusionMatrix(pred2, vowel.test$y)
c3 <- confusionMatrix(pred3, comb$y)
c1
c2
c3


#2
#Set the seed to 62433 and predict diagnosis with all the other variables using a
#random forest ("rf"), boosted trees ("gbm") and linear discriminant analysis ("lda")
#model. Stack the predictions together using random forests ("rf"). What is the
#resulting accuracy on the test set? Is it better or worse than each of the individual 
#predictions? 
library(caret)
library(gbm)
set.seed(3433)
library(AppliedPredictiveModeling)
data(AlzheimerDisease)
adData = data.frame(diagnosis,predictors)
inTrain = createDataPartition(adData$diagnosis, p = 3/4)[[1]]
training = adData[ inTrain,]
testing = adData[-inTrain,]
set.seed(62433)
tr<-trainControl(method="cv", number=3)
model1<-train(diagnosis ~., method="rf",  data=training, trControl=tr)
model2<-train(diagnosis ~., method="gbm", data=training)
model3<-train(diagnosis ~., method="lda", data=training)
pred1<-predict(model1, testing)
pred2<-predict(model2, testing)
pred3<-predict(model3, testing)
predDF<-data.frame(pred1, pred2, pred3, diagnosis=testing$diagnosis)
combModel<-train(diagnosis~., method="rf", data=predDF)
combPred<-predict(combModel, testing)
c1<- confusionMatrix(pred1, testing$diagnosis)
c1
c2<- confusionMatrix(pred2, testing$diagnosis)
c2
c3<- confusionMatrix(pred3, testing$diagnosis)
c3
c4<- confusionMatrix(combPred, testing$diagnosis)
c4
#3
#Set the seed to 233 and fit a lasso model to predict Compressive Strength. 
#Which variable is the last coefficient to be set to zero as the penalty increases? 
#(Hint: it may be useful to look up ?plot.enet). 
set.seed(3523)
library(AppliedPredictiveModeling)
library(lars)
library(caret)
library(elasticnet)
data(concrete)
inTrain = createDataPartition(concrete$CompressiveStrength, p = 3/4)[[1]]
training = concrete[ inTrain,]
testing = concrete[-inTrain,]
set.seed(233)
fit <- train(CompressiveStrength ~ ., data = training, method = "lasso")
#plot on the windows screen
resize.win <- function(Width=6, Height=6)
{
        # it is for windows
        dev.off(); 
        windows(record=TRUE, width=Width, height=Height)
}
resize.win(5,5)
plot(rnorm(100))
resize.win(10,10)
plot(rnorm(100))
#use plot.enet
#? plot.enet to find
plot.enet(fit$finalModel, xvar = "penalty", use.color = TRUE)

#4
#Fit a model using the bats() function in the forecast package to the 
#training time series. Then forecast this model for the remaining time points.
#For how many of the testing points is the true value within the 95% prediction 
#interval bounds? 
library(lubridate) 
library(forecast)# For year() function below
dat = read.csv("C:/Users/stephanie song/Desktop/gaData.csv")
training = dat[year(dat$date) < 2012,]
testing = dat[(year(dat$date)) > 2011,]
tstrain = ts(training$visitsTumblr)
fit<-bats(tstrain, use.parallel=FALSE)
plot(forecast(fit))
h <- dim(testing)[1]
fcase<-forecast(fit,level = 95, h = h)
accuracy(fcase, testing$visitsTumblr)

# some one wrote this but I don't get it..
result <- c()
l <- length(fcase$lower)
for (i in 1:l){
        x <- testing$visitsTumblr[i]
        a <- fcase$lower[i] < x & x < fcase$upper[i]
        result <- c(result, a)
}
sum(result)/l * 100

#?????


#5
#Set the seed to 325 and fit a support vector machine using the e1071 
#package to predict Compressive Strength using the default settings. 
#Predict on the testing set. What is the RMSE? 
library(caret)
library(e1071)
set.seed(3523)
library(AppliedPredictiveModeling)
data(concrete)
inTrain = createDataPartition(concrete$CompressiveStrength, p = 3/4)[[1]]
training = concrete[ inTrain,]
testing = concrete[-inTrain,]
set.seed(325)
svm.model<-svm(CompressiveStrength~., data=training)
pred<-predict(svm.model, newdata=testing)
accuracy(pred, testing$CompressiveStrength)
#6.715009













