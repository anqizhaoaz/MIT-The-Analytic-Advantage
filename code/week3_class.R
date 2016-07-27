
############## Week 3 class exercise

-1.5 + 3*1 -0.5*5
#The Logit is just log(Odds), and looks like the linear regression equation. So the Logit is -1.5 + 3*1 - 0.5*5 = -1.
exp(-1)
#What is the value of the Odds for this observation? odds is equal to e raised to the power of linear regression
1/(1+exp(1))
#What is the value of P(y = 1) for this observation?


## In R, create a logistic regression model to predict "PoorCare" using the independent variables "StartedOnCombination" and "ProviderCount". 
## Use the training set we created in the previous video to build the model.
quality <- read.csv("~/Downloads/mit/quality.csv")
str(quality)

library(caTools)
set.seed(88)
split = sample.split(quality$PoorCare, SplitRatio = 0.75)
qualityTrain = subset(quality, split == TRUE)
qualityTest = subset(quality, split == FALSE)

## built a logistic regression model 
QualityLog = glm(PoorCare ~ StartedOnCombination + ProviderCount, data=qualityTrain, family=binomial)
summary(QualityLog)
#The coefficient value is positive, meaning that positive values of the variable make the outcome of 1 more likely. This corresponds to Poor Care.

# the outcome of a logistic model is a probability20/25
predictTrain = predict(QualityLog, type="response")
tapply(predictTrain, qualityTrain$PoorCare, mean)
table(qualityTrain$PoorCare, predictTrain>0.5)

#Compute the test set predictions in R by running the command:
library(ROCR)
QualityLog = glm(PoorCare ~ OfficeVisits + Narcotics, data=qualityTrain, family=binomial)
summary(QualityLog)
predictTest = predict(QualityLog, type="response", newdata=qualityTest)
ROCRpredTest = prediction(predictTest, qualityTest$PoorCare)
auc = as.numeric(performance(ROCRpredTest, "auc")@y.values)
auc
#The AUC of a model has the following nice interpretation: 
#given a random patient from the dataset who actually received poor care, and a random patient from the dataset who actually received good care, the AUC is the perecentage of time that our model will classify which is which correctly.


#### recitation ####
PollingData <- read.csv("~/Downloads/mit/PollingData.csv")
str(PollingData)
table(PollingData$Year)

## missing data
summary(PollingData) #Rasmussen and SurveyUSA have missing data

## handle missing data
# remove variables or observations?
# fill the missing data with average data?
# multiple imputation
library(mice)

## select four variables
simple = PollingData[c("Rasmussen", "SurveyUSA", "PropR", "DiffCount")]
summary(simple)

##
set.seed(144)
imputed = complete(mice(simple))
summary(imputed)

## copy back
PollingData$Rasmussen = imputed$Rasmussen
PollingData$SurveyUSA = imputed$SurveyUSA

## split data into train and test
train = subset(PollingData, Year == 2004 | Year == 2008)
test = subset(PollingData, Year == 2012)
table(train$Republican)
# simple baseline model always predicts the more common outcomes, in this case, republican will win the state
# And we see that the simple baseline model will have accuracy of 53% on the training set.

table(sign(train$Rasmussen))
table(train$Republican, sign(train$Rasmussen))

