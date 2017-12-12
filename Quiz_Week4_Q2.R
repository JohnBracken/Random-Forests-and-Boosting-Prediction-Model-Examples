#Load libraries, test and training datasets.
library(caret)
library(gbm)
set.seed(3433)
library(AppliedPredictiveModeling)
data(AlzheimerDisease)
adData = data.frame(diagnosis,predictors)
inTrain = createDataPartition(adData$diagnosis, p = 3/4)[[1]]
training = adData[ inTrain,]
testing = adData[-inTrain,]

#Set random seed.
set.seed(62433)

#Create three different prediction models using the training data.
#One based on random forests, a boost model and linear discriminant
#analysis.
rf_fit <- train(diagnosis~., data = training, method = "rf")
boost_fit <- train(diagnosis~.,data = training, method = "gbm")
lda_fit <- train(diagnosis~., data = training, method = "lda")

#Predict diagnoses on the test data using all three models.
rf_predict <- predict(rf_fit, testing)
boost_predict <- predict(boost_fit, testing)
lda_predict <- predict(lda_fit, testing)

#Get confusion matrices and accuracy of all three models.
rf_accuracy <- confusionMatrix(rf_predict, testing$diagnosis)
boost_accuracy <- confusionMatrix(boost_predict, testing$diagnosis)
lda_accuracy <- confusionMatrix(lda_predict, testing$diagnosis)

#Combine the prediction results and actual diagnoses from the test data
#into one data frame.
predDF <- data.frame(rf_predict, boost_predict, lda_predict, diagnosis = testing$diagnosis)

#Create a combination model of all three models for diagnosis
combModFit <- train(diagnosis~., method = "rf", data = predDF)

#Predict diagnosis using the combination model on the test data.
combPred <- predict(combModFit, testing)

#Get the confusion matrix and accuracy of the combination model.
combo_accuracy <- confusionMatrix(combPred, testing$diagnosis)


