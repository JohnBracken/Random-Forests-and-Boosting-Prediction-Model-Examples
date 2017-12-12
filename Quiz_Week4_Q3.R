
#Set an initial seed, along with the AppliedPredictiveModeling and 
#elasticnet libraries.  The elasticnet library is needed to do a lasso model.
set.seed(3523)
library(AppliedPredictiveModeling)
library(elasticnet)

#Load the concrete data.  Divide into training and test data sets.
data(concrete)
inTrain = createDataPartition(concrete$CompressiveStrength, p = 3/4)[[1]]
training = concrete[ inTrain,]
testing = concrete[-inTrain,]

#Reset the seed.
set.seed(233)

#Create a lasso fit model to model CompressiveStrength vs. other predictors in the training
#dataset.
modFit <- train(CompressiveStrength~., method = "lasso", data = training)

#Plot the model coefficients vs. penalty applied (lambda)
plot_model <- plot(modFit$finalModel, xvar = "penalty", use.color = TRUE, label=TRUE)

#Show the plot.  
print(plot_model)