#Library needed for vowel data.  Split data into training
#and test sets.
library(ElemStatLearn)
data(vowel.train)
data(vowel.test)

#Convert the values to be predicted into factors.
vowel.train$y <- as.factor(vowel.train$y)
vowel.test$y <- as.factor(vowel.test$y)

#Set seed for repeatability
set.seed(33833)

#Create a random forest and boost model for the y values
#using the training dataset.
rf_fit <- train(y~., data = vowel.train, method = "rf")
boost_fit <- train(y~.,data =vowel.train,method = "gbm")

#Predict the y values for each model using the test data.
rf_predict <- predict(rf_fit, vowel.test)
boost_predict <- predict(boost_fit, vowel.test)

#Determine the confusion matrix of the predictions for the
#y value in the test data.  Also gives indication of accuracy
#Do it for each model.
rf_accuracy <- confusionMatrix(rf_predict, vowel.test$y)
boost_accuracy <- confusionMatrix(boost_predict, vowel.test$y)

#combine the predicted y values for each model, and the actual
#y values from the test data, into one data frame for comparison.
predDF <- data.frame(rf_predict, boost_predict, y=vowel.test$y)
predDF$agree_predict <-(predDF$rf_predict == predDF$boost_predict)

#Create a logical column that indicates whether or not the 
#predictions from the models agree with each other.
predDF$agree_predict <-(predDF$rf_predict == predDF$boost_predict)

#Count the number of times the rf and boost model predictions agree
#with each other
count_models <- table(predDF$agree_predict)["TRUE"]

#Count the number of times the models agree with each other and also agree
#with the actual y values in the test data.
predDF$agree_test<-(predDF$rf_predict == predDF$boost_predict) & (predDF$rf_predict == predDF$y)
count_test <- table(predDF$agree_test)["TRUE"]


#Calculate the combined accuracy of the agreeing models and print.
combined_accuracy <- count_test/count_models    
print(combined_accuracy)




