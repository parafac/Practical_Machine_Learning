# project 1

# required library

rm(list=ls())

library(caret)
library(rattle)
library(rpart.plot)

# library(AppliedPredictiveModeling)
# library(randomForest)

# download the data files

# fileURL <- "https://d396qusza40orc.cloudfront.net/predmachlearn/pml-training.csv"
# localFile <- "./pml-training.csv"
# download.file(fileURL, destfile = localFile)
# 
# fileURL <- "https://d396qusza40orc.cloudfront.net/predmachlearn/pml-testing.csv"
# localFile <- "./pml-testing.csv"
# download.file(fileURL, destfile = localFile)


# read in data

trainingData <- read.csv("./pml-training.csv", header=TRUE, sep=",", 
                        na.string=c("", " ", "NA", "#DIV/0!"))
testingData  <- read.csv("./pml-testing.csv", header=TRUE, sep=",", 
                        na.string=c("", " ", "NA", "#DIV/0!"))

dim(trainingData)
dim(testingData)

# a<- is.na(trainingData)
# b<- colSums(a)
# c<- b/nrow(trainingData)
# d<- which(c <= 0.1)

# discover near zero variance columns

nsv <- nearZeroVar(trainingData, saveMetric=TRUE)
sum(nsv$ZeroVar)
sum(nsv$nzv)

nzvColNames <- sort(rownames(nsv[nsv$nzv,]))

# nsv

# remove columns of near zero variance

trainingSet <- trainingData[,!nsv$nzv]
testingSet  <- testingData[,!nsv$nzv]

dim(trainingSet)
dim(testingSet)

jcols <- grep("amplitude_|avg_|kurtosi_|ls_|max_|min_|skewness_|stddev_|var_", names(trainingSet))

trainingSet <- trainingSet[,-jcols]
testingSet  <- testingSet[,-jcols]

dim(trainingSet)
dim(testingSet)

# check the variable names are still matched
# note the last column won't match

sum(names(trainingSet) != names(testingSet))

# identify columns that have more than 80% of missing values
# or equivalently, keep only those less than 20% of missing values

colIdx <- which(colSums(is.na(trainingSet))/nrow(trainingSet) <= 0.20)
trainingSet <- trainingSet[, colIdx]
testingSet  <- testingSet[, colIdx]

dim(trainingSet)
dim(testingSet)


# the first 7 columns are not relevant to analysis, not include them

# nCols <- ncol(trainingSet)
# trainingSet <- trainingSet[, 8:nCols]
# testingSet <- testingSet[, 8:nCols]

trainingSet <- trainingSet[,-1]
testingSet  <- testingSet[,-1]

trainingSet$classe <- as.factor(trainingSet$classe)

dim(trainingSet)
dim(testingSet)

# check the variable names are still matched
# note the last column won't match

sum(names(trainingSet) != names(testingSet))

# feature plot

featurePlot(x=trainingSet[,c("pitch_arm", "roll_arm", "yaw_arm", "pitch_dumbbell", 
                             "roll_dumbbell", "yaw_dumbbell", "pitch_forearm",
                             "roll_forearm", "yaw_forearm")], 
            y=trainingSet$classe, plot="pairs")

par(mfrow=c(1,3))

featurePlot(x=trainingSet[,c("pitch_arm", "roll_arm", "yaw_arm")], 
            y=trainingSet$classe, plot="pairs")


featurePlot(x=trainingSet[,c("pitch_dumbbell", 
                             "roll_dumbbell", "yaw_dumbbell")], 
            y=trainingSet$classe, plot="pairs")

featurePlot(x=trainingSet[,c("pitch_forearm",
                             "roll_forearm", "yaw_forearm")], 
            y=trainingSet$classe, plot="pairs")

featurePlot(x=trainingSet[,c("pitch_belt",
                             "roll_belt", "yaw_belt")], 
            y=trainingSet$classe, plot="pairs")


featurePlot(x=trainingSet[,c("pitch_arm", "roll_arm", "yaw_arm",
                             "pitch_belt","roll_belt", "yaw_belt")], 
            y=trainingSet$classe, plot="pairs")

featurePlot(x=trainingSet[,c("total_accel_belt", "total_accel_arm", "total_accel_dumbbell","total_accel_forearm")], 
            y=trainingSet$classe, plot="pairs")


par(mfrow=c(1,1))

# split training data into training set and cross-validation set

set.seed(3245)
inTrain <- createDataPartition(trainingSet$classe, p = 0.6)[[1]]
myTraining <- trainingSet[inTrain,]
myTesting  <- trainingSet[-inTrain,]

# training <- trainingSet[ inTrain,]
# validation <- trainingSet[-inTrain,]
# testing <- testingSet
# 

# multivariable regression model  -- wrong model

# modelFit_lm <- train(classe ~ ., method="lm", data=training)
# pred_lm <- predict(modelFit_lm, newdata=validation)


# decision tree model

modelFit_rpart <- train(classe ~ ., data=myTraining, method="rpart")
pred_rpart <- predict(modelFit_rpart, newdata=myTesting)
confusionMatrix(myTesting$classe, pred_rpart)
accuracy_rpart <- sum(myTesting$classe == pred_rpart)/length(myTesting$classe)
print(modelFit_rpart$finalModel)
print(accuracy_rpart)

library(rpart.plot)
library(rattle)

windows()
fancyRpartPlot(modelFit_dt$finalModel)

plot(modelFit_dt$finalModel, uniform=TRUE, main="Classification Tree")
text(modelFit_dt$finalModel, use.n=TRUE, all=TRUE, cex=0.8)


# random forest model

modelFit_rf <- train(classe ~ ., data=myTraining, method="rf")
pred_rf <- predict(modelFit_rf, newdata=myTesting)
confusionMatrix(myTesting$classe, pred_rf)
accuracy_rf <- sum(myTesting$classe == pred_rf)/length(myTesting$classe)
print(modelFit_rf$finalModel)
print(accuracy_rf)

# tree with boosting

modelFit_gbm <- train(classe ~ ., data=myTraining, method="gbm", verbose=FALSE)
pred_gbm <- predict(modelFit_gbm, newdata=myTesting)
confusionMatrix(myTesting$classe, pred_gbm)
accuracy_gbm <- sum(myTesting$classe == pred_gbm)/length(myTesting$classe)
print(modelFit_gbm$finalModel)
print(accuracy_gbm)

# lda - linear discriminant analysis

modelFit_lda <- train(classe ~ ., data=myTraining, method="lda")
pred_lda <- predict(modelFit_lda, newdata=myTesting)
confusionMatrix(myTesting$classe, pred_lda)
accuracy_lda <- sum(myTesting$classe == pred_lda)/length(myTesting$classe)
print(accuracy_lda)
print(modelFit_lda$finalModel)


# Final test on the testing data 

prediction_rpart <- predict(modelFit_rpart, newdata=testingSet)
print(prediction_rpart)

prediction_rf <- predict(modelFit_rf, newdata=testingSet)
print(prediction_rf)

prediction_gbm <- predict(modelFit_gbm, newdata=testingSet)
print(prediction_gbm)

prediction_lda <- predict(modelFit_lda, newdata=testingSet)
print(prediction_lda)


# print output

write_prediction = function(x){
    n = length(x)
    for(i in 1:n){
        filename = paste0("problem_id_",i,".txt")
        write.table(x[i],file=filename,quote=FALSE,row.names=FALSE,col.names=FALSE)
    }
}


#Prediction Assignment Submission
predictTesting<- function(){
    pred_test <- predict(modelFit_gbm, newdata=testingSet)
    print(pred_test)
    ans <- as.vector(pred_test)
    write_prediction(ans)
}


write_prediction(prediction_gbm)