
#### Week 2 Homework

#### CLIMATE CHANGE ####
climate <- read.csv("~/Downloads/mit/climate_change.csv")
str(climate)

## split data
#training set, consisting of all the observations up to and including 2006,
#testing set consisting of the remaining years (hint: use subset).
climate_train <- subset(climate, climate$Year<=2006)
climate_test <- subset(climate, climate$Year>2006)
str(climate_train)
str(climate_test)

## Problem 1 - Creating Our First Model
## Problem 2 - Understanding the Model
#build a linear regression model to predict the dependent variable Temp
#using MEI, CO2, CH4, N2O, CFC.11, CFC.12, TSI, and Aerosols as independent variable
climateModel = lm(Temp ~ MEI + CO2 + CH4 + N2O + CFC.11 + CFC.12 + TSI + Aerosols, data=climate_train)
summary(climateModel)

## Correlation matrix
cor(climate_train)

#Problem 3 - Simplifying the Model
#build a model with only MEI, TSI, Aerosols and N2O as independent variables
climateModel2 = lm(Temp ~ MEI + N2O + TSI + Aerosols, data=climate_train)
summary(climateModel2)

## Problem 4 - Automatically Building the Model
simpleModel = step(climateModel)

climateModel3 = lm(Temp ~ MEI + CO2 + N2O + CFC.11 + CFC.12 + TSI + Aerosols, data=climate_train)
summary(climateModel3)

#It is interesting to note that the step function does not address the collinearity of the variables, 
#except that adding highly correlated variables will not improve the R2 significantly.
#The consequence of this is that the step function will not necessarily produce a very interpretable model 
#- just a model that has balanced quality and simplicity for a particular weighting of quality and simplicity (AIC).

## Problem 5 - Testing on Unseen Data
predictedTemp = predict(climateModel3, newdata = climate_test)
SSE = sum((predictedTemp - climate_test$Temp)^2)
SST = sum((mean(climate_train$Temp) - climate_test$Temp)^2) #mean of train dataset
rsquare = 1 - SSE/SST
rsquare


#### READING TEST SCORES ####
pisa2009train <- read.csv("~/Downloads/mit/pisa2009train.csv")
pisa2009test <- read.csv("~/Downloads/mit/pisa2009test.csv")

## Problem 1 - Dataset size
#How many students are there in the training set?
str(pisa2009train)

#summarize dataset
#Using tapply() on pisaTrain, what is the average reading test score of males?
tapply(pisa2009train$readingScore, pisa2009train$male, mean)

##check missing values
summary(pisaTrain)

#Removing missing values: to remove observations with any missing value from pisaTrain and pisaTest
pisaTrain = na.omit(pisa2009train)
pisaTest = na.omit(pisa2009test)
str(pisaTrain)
str(pisaTest)

## Problem 2 - Unordered factors in regression models
model1 = lm(readingScore~., data=pisaTrain)
summary(model1)

## Problem 3 - Building a model
#Set the reference level of the factor by typing the following two lines in your R console:
pisaTrain$raceeth = relevel(pisaTrain$raceeth, "White")
pisaTest$raceeth = relevel(pisaTest$raceeth, "White")

#build a linear regression model (call it lmScore) using the training set to predict readingScore using all the remaining variables
LinReg = lm(readingScore ~ ., data=pisaTrain)
summary(LinReg)
#Note that this R-squared is lower than the ones for the models we saw in the lectures and recitation. This does not necessarily imply that the model is of poor quality. More often than not, it simply means that the prediction problem at hand 
#(predicting a student's test score based on demographic and school-related variables) is more difficult than other prediction problems (like predicting a team's number of wins from their runs scored and allowed, or predicting the quality of wine from weather conditions).

#Problem 3 - Computing the root-mean squared error of the model
SSE = sum(LinReg$residuals^2)
RMSE = sqrt(SSE / nrow(pisaTrain))

#Identifying variables lacking statistical significance

#Problem 4 - Predicting on unseen data
predicted = predict(LinReg, newdata = pisaTest)
max(predicted)-min(predicted)
sse = sum((pisaTest$readingScore-predicted)^2)
sse
sqrt(mean((predicted-pisaTest$readingScore)^2))

#Problem 4 - Baseline prediction and test-set SSE
baseline = mean(pisaTrain$readingScore)
sst = sum((pisaTest$readingScor-baseline)^2)

#### DETECTING FLU EPIDEMICS VIA SEARCH ENGINE QUERY DATA ####
#Problem 1 - Understanding the Data
FluTrain <- read.csv("~/Downloads/mit/FluTrain.csv")
str(FluTrain)
which.max(FluTrain$ILI)
FluTrain$Week[303]
which.max(FluTrain$Queries)

#understand data
hist(FluTrain$ILI)
#The histogram of ILI can be obtained with hist(FluTrain$ILI). Visually, the data is skew right.
#When handling a skewed dependent variable, it is often useful to predict the logarithm of the dependent variable instead of the dependent variable itself -- this prevents the small number of 
#unusually large or small observations from having an undue influence on the sum of squared errors of predictive models. 

plot(log(FluTrain$ILI), FluTrain$Queries)

#Problem 2 - Linear Regression Model
#What is the training set R-squared value for FluTrend1 model (the "Multiple R-squared")?
cor(log(FluTrain$ILI), FluTrain$Queries)
FluTrend1 = lm(log(ILI)~Queries, data=FluTrain)
summary(FluTrend1)

#Problem 3 - Performance on the Test Set
FluTest <- read.csv("~/Downloads/mit/FluTest.csv")
str(FluTest)

PredTest1 = exp(predict(FluTrend1, newdata=FluTest))
PredTest1[11]
FluTest$ILI[11]
#(Observed ILI - Estimated ILI)/Observed ILI
(2.293422 - 2.187378)/2.293422

#What is the Root Mean Square Error (RMSE) between our estimates and the actual observations for the percentage of ILI-related physician visits, on the test set?
sse =  sum((FluTest$ILI - PredTest1)^2)
rmse = sqrt(sse / nrow(FluTest))
rmse

##Problem 4 - Training a Time Series Model
plot(FluTrain$Week, FluTrain$ILI)
library(zoo)
ILILag2 = lag(zoo(FluTrain$ILI), -2, na.pad=TRUE)
FluTrain$ILILag2 = coredata(ILILag2)
#In these commands, the value of -2 passed to lag means to return 2 observations before the current one; 
#a positive value would have returned future observations. The parameter na.pad=TRUE means to add missing values for the first two weeks of our dataset, where we can't compute the data from 2 weeks earlier.
ILILag2
str(FluTrain)

#Use the plot() function to plot the log of ILILag2 against the log of ILI. Which best describes the relationship between these two variables?
plot(log(FluTrain$ILILag2), log(FluTrain$ILI))
FluTrend2 = lm(log(ILI) ~ log(ILILag2) + Queries, data = FluTrain)
summary(FluTrend2)

## Problem 5 - Evaluating the Time Series Model in the Test Set
#we will also need to add ILILag2 to the FluTest data frame
ILILag2 = lag(zoo(FluTest$ILI), -2, na.pad=TRUE)
FluTest$ILILag2 = coredata(ILILag2)
summary(FluTest$ILILag2)

#filling the missing values
str(FluTrain) 
nrow(FluTrain)
str(FluTest)

FluTest$ILILag2[1] = FluTrain$ILI[416]
FluTest$ILILag2[2] = FluTrain$ILI[417]

PredTest2 = exp(predict(FluTrend2, newdata=FluTest))
sse =  sum((FluTest$ILI - PredTest2)^2)
rmse = sqrt(sse / nrow(FluTest))
rmse
