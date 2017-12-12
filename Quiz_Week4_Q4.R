#Load libraries for the data.
library(lubridate) # For year() function below
library(forecast)

#Read in the data file.
dat = read.csv("gaData.csv")
training = dat[year(dat$date) < 2012,]
testing = dat[(year(dat$date)) > 2011,]

#Create a time series of the training tumbler visits column.
tstrain = ts(training$visitsTumblr)

#Fit a BATS model for the forecasting.
modfit <- bats(tstrain)

#Using the model, forecast what the testing data should be.
#Get the upper and lower 95% bound.
fcast <- forecast(modfit, level=95, nrow(testing))

#Create a column in the testing data to determine if the 
#true number of tumbler visits falls within the 95% prediction
#interval from the model.
testing$range <- (testing$visitsTumblr >= fcast$lower) & (testing$visitsTumblr <= fcast$upper)

#Calculate the percentage of true tumbler visit values fall
#within the forecasted prediction interval. 
percent <- table(testing$range)["TRUE"]/nrow(testing)

