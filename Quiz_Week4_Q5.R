#Set a random seed
set.seed(3523)

#Apply all necessary libraries
library(AppliedPredictiveModeling)
library(caret)
library(forecast)
library(e1071)

#Load the data and create training and test datasets.
data(concrete)
inTrain = createDataPartition(concrete$CompressiveStrength, p = 3/4)[[1]]
training = concrete[ inTrain,]
testing = concrete[-inTrain,]

#Reset the seed
set.seed(325)

#Do a singular value decomposition model for compressive strength based on the training
#data.
modfit <- svm(CompressiveStrength~., data = training)

#Predict the compressive strength using the test data.
prediction <- predict(modfit, testing)

#Calculate the root mean square error using the accuracy function and print it.
RMSE <- accuracy(prediction, testing$CompressiveStrength)
print(RMSE)