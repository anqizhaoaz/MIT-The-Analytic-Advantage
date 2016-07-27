
#### Week 3 Logistic regression home assignment ####

#### POPULARITY OF MUSIC RECORDS ####

## Problem 1 - Understanding the Data
songs <- read.csv("~/Downloads/mit/songs.csv", header=TRUE)
str(songs)

# How many observations (songs) are from the year 2010?
table(songs$year)

# How many songs does the dataset include for which the artist name is "Michael Jackson"?
table(songs$artistname=="Michael Jackson")

# Which of these songs by Michael Jackson made it to the Top 10? Select all that apply.
MJ = subset(songs, songs$artistname=="Michael Jackson")
subset(MJ, MJ$Top10==1)

# The variable corresponding to the estimated time signature (timesignature) is discrete, meaning that it only takes integer values (0, 1, 2, 3, . . . ). 
# What are the values of this variable that occur in our dataset? 
table(songs$timesignature)

# Out of all of the songs in our dataset, the song with the highest tempo is one of the following songs. Which one is it?
which.max(songs$tempo)
songs$songtitle[6206]

## Problem 2 - Creating Our Prediction Model
# We wish to predict whether or not a song will make it to the Top 10. 
# To do this, first use the subset function to split the data into a training set "SongsTrain" consisting of all the observations up to and including 2009 song releases, and a testing set "SongsTest", consisting of the 2010 song releases.
train = subset(songs, songs$year<=2009)
test = subset(songs, songs$year>2009)
str(songs)
str(train)
str(test)

# Now, use the glm function to build a logistic regression model to predict Top10 using all of the other variables as the independent variables. You should use SongsTrain to build the model.
nonvars = c("year", "songtitle", "artistname", "songID", "artistID")
SongsTrain = train[ , !(names(train) %in% nonvars) ]
SongsTest = test[ , !(names(test) %in% nonvars) ]
model1 = glm(Top10~., SongsTrain,  family=binomial)
summary(model1)

## Problem 3 - Beware of Multicollinearity Issues!
# What is the correlation between the variables "loudness" and "energy" in the training set?
cor(SongsTrain$energy, SongsTrain$loudness)
# Given that these two variables are highly correlated, Model 1 suffers from multicollinearity. 

# To avoid this issue, we will omit one of these two variables and rerun the logistic regression. 
# we'll build two variations of our original model: 
# Model 2, in which we keep "energy" and omit "loudness"
model2 = glm(Top10 ~ . - loudness, data=SongsTrain, family=binomial)
summary(model2)

# Model 3, in which we keep "loudness" and omit "energy"
model3 = glm(Top10 ~ . - energy, data=SongsTrain, family=binomial)
summary(model3)
# We just subtracted the variable loudness. We couldn't do this with the variables "songtitle" and "artistname", 
# because they are not numeric variables, and we might get different values in the test set that the training set has never seen. 
# But this approach (subtracting the variable from the model formula) will always work when you want to remove numeric variables.


## Problem 4 - Validating Our Model
predict_train = predict(model3, typle = "response")
predict_test = predict(model3, type="response", newdata=SongsTest)

table(SongsTest$Top10, predict_test>0.45)        
(309+19)/(309+19+40+5)
# How many non-hit songs does Model 3 predict will be Top 10 hits (again, looking at the test set), using a threshold of 0.45? 5
# How many songs does Model 3 correctly predict as Top 10 hits in 2010 (remember that all songs in 2010 went into our test set), using a threshold of 0.45? 19

# What is the sensitivity of Model 3 on the test set, using a threshold of 0.45?
19/(40+19)

# What is the specificity of Model 3 on the test set, using a threshold of 0.45?
309/(309+5)

# baseline model
table(SongsTest$Top10)
#The baseline model would get 314 observations correct, and 59 wrong, for an accuracy of 314/(314+59) = 0.8418231.

#### result summary 
## Model 3 has a very high specificity, meaning that it favors specificity over sensitivity. 
## While Model 3 only captures less than half of the Top 10 songs, it still can offer a competitive edge, since it is very conservative in its predictions.



#### PREDICTING PAROLE VIOLATORS ####
# Problem 1 - Loading the Dataset
parole <- read.csv("~/Downloads/mit/parole.csv")
str(parole)

# How many of the parolees in the dataset violated the terms of their parole?
table(parole$violator)

# Problem 2 - Preparing the Dataset
# In the last subproblem, we identified variables that are unordered factors with at least 3 levels, 
# so we need to convert them to factors for our prediction problem 
# they are states and crime
parole$state <- as.factor(parole$state)
parole$crime <- as.factor(parole$crime)

summary(parole)

# Problem 3 - Splitting into a Training and Testing Set
set.seed(144)
library(caTools)
split = sample.split(parole$violator, SplitRatio = 0.7)
train = subset(parole, split == TRUE)
test = subset(parole, split == FALSE)
# Roughly what proportion of parolees have been allocated to the training and testing sets?: 70% to training
# If you set a random seed, split, set the seed again to the same value, and then split again, you will get the same split.
# However, if you set the seed and then split twice, you will get different splits. If you set the seed to different values, you will get different splits.

# Problem 4 - Building a Logistic Regression Model
model1 = glm(violator ~ ., data=train, family = "binomial")
summary(model1)
# What variables are significant in this model? 
# race, state4, multiple.offenses
# What can we say based on the coefficient of the multiple.offenses variable?
# Our model predicts that a parolee who committed multiple offenses has 5.01 times higher odds of being a violator 
# than a parolee who did not commit multiple offenses but is otherwise identical. exp(1.6119919)

# Consider a parolee who is male, of white race, aged 50 years at prison release, from the state of Maryland, served 3 months, had a maximum sentence of 12 months, \
# did not commit multiple offenses, and committed a larceny.
# odds = exp(coeffieicent linear)
exp(-4.2411574+0.38 + 50*(-0.0001756) + 12*0.0802954 + 1*0.8867192 + 3*(-0.1238867) +0.6837143)

-(-4.2411574+0.38 + 50*(-0.0001756) + 12*0.0802954 + 1*0.8867192 + 3*(-0.1238867) +0.6837143)
1/(1+exp(1.707619))
# Therefore, the odds ratio is exp(-1.700629) = 0.183, and the predicted probability of violation is 1/(1+exp(1.700629)) = 0.154.

# Problem 5 - Evaluating the Model on the Testing Set
# Use the predict() function to obtain the model's predicted probabilities for parolees in the testing set, remembering to pass type="response".
predictTest = predict(model1, newdata=test, type= "response")
summary(predictTest)

# In the following questions, evaluate the model's predictions on the test set using a threshold of 0.5.
table(test$violator,predictTest>0.8)
# What is the model's sensitivity?
(12/(12+11)
# What is the model's specificity?
167/(167+12)
# What is the model's accuracy?
(167+12)/(167+12+12+11)

# What is the accuracy of a simple model that predicts that every parolee is a non-violator?
table(test$violator)
179/(179+23)

# The model at cutoff 0.5 has 12 false positives and 11 false negatives, while the baseline model has 0 false positives and 23 false negatives. Because a parole board is likely to assign more cost to a false negative, the model at cutoff 0.5 is likely of value to the board.

# From the previous question, the parole board would likely benefit from decreasing the logistic regression cutoffs, which decreases the false negative rate 
# while increasing the false positive rate.

# Using the ROCR package, what is the AUC value for the model?
library(ROCR)
ROCRpredTest = prediction(predictTest, test$violator)
auc = as.numeric(performance(ROCRpredTest, "auc")@y.values)
auc
# The probability the model can correctly differentiate between a randomly selected parole violator and a randomly selected parole non-violator
# The AUC deals with differentiating between a randomly selected positive and negative example. It is independent of the regression cutoff selected.

## Problem 6 - Identifying Bias in Observational Data


#### PREDICTING LOAN REPAYMENT ####
# Problem 1 - Preparing the Dataset
loans <- read.csv("~/Downloads/mit/loans.csv")
str(loans)

# What proportion of the loans in the dataset were not paid in full? Please input a number between 0 and 1.
table(loans$not.fully.paid)
1533/(1533+8045)

# Which of the following variables has at least one missing observation? Select all that apply.
summary(loans)
missing = subset(loans, is.na(log.annual.inc) | is.na(days.with.cr.line) | is.na(revol.util) | is.na(inq.last.6mths) | is.na(delinq.2yrs) | is.na(pub.rec))
nrow(missing) #62
table(missing$not.fully.paid)


# fill the missing 
library(mice)
set.seed(144)
vars.for.imputation = setdiff(names(loans), "not.fully.paid")
imputed = complete(mice(loans[vars.for.imputation]))
loans[vars.for.imputation] = imputed
#  We predicted missing variable values using the available independent variables for each observation. 


## Problem 2 - Prediction Models
library(caTools)
set.seed(144)
split = sample.split(loans$not.fully.paid, SplitRatio = 0.70)
loanTrain = subset(loans, split == TRUE)
loanTest = subset(loans, split == FALSE)
# Now, use logistic regression trained on the training set to predict the dependent variable not.fully.paid using all the independent variables.
model1 = glm(not.fully.paid~., data=loanTrain, family="binomial")
summary(model1)
# Which independent variables are significant in our model?

# Consider two loan applications, which are identical other than the fact that the borrower in Application A has FICO credit score 700 while the borrower in Application B has FICO credit score 710.
9.406e-03*10
# Because Application A is identical to Application B other than having a FICO score 10 lower, its predicted log odds differ by -0.009317 * -10 = 0.09317 from the predicted log odds of Application B.
exp(9.406e-03*10)
# Using the answer from the previous question, the predicted odds of loan A not being paid back in full are exp(0.09317) = 1.0976 times 
# larger than the predicted odds for loan B. Intuitively, it makes sense that loan A should have higher odds of non-payment than loan B, since the borrower has a worse credit score.

# predict test dataset
predicted.risk = predict(model1, newdata=loanTest, type="response")
loanTest$predicted.risk = predicted.risk
table(loanTest$not.fully.paid, loanTest$predicted.risk>0.5)
#What is the accuracy of the logistic regression model? Input the accuracy as a number between 0 and 1.
(2400+3)/(2400+3+13+457)
table(loanTest$not.fully.paid)
2413/(2413+460)

library(ROCR)
ROCRpredTest = prediction(predicted.risk, loanTest$not.fully.paid)
auc = as.numeric(performance(ROCRpredTest, "auc")@y.values)
auc


## Problem 3 - A "Smart Baseline"
# Using the training set, build a bivariate logistic regression model (aka a logistic regression model with a single independent variable) that predicts the dependent variable not.fully.paid using only the variable int.rate.
model2 = glm(not.fully.paid~int.rate, data=loanTrain, family="binomial")
summary(model2)

cor(loanTrain$int.rate, loanTrain$fico)

# Make test set predictions for the bivariate model. What is the highest predicted probability of a loan not being paid in full on the testing set?
pred2 = predict(model2, newdata=loanTest, type="response")
summary(pred2)
# According to the summary function, the maximum predicted probability of the loan not being paid back is 0.4266, which means no loans would be flagged at a logistic regression cutoff of 0.5.

# What is the test set AUC of the bivariate model?
library(ROCR)
ROCRpredTest = prediction(pred2, loanTest$not.fully.paid)
auc = as.numeric(performance(ROCRpredTest, "auc")@y.values)
auc

## Problem 4 - Computing the Profitability of an Investment
# How much does a $10 investment with an annual interest rate of 6% pay back after 3 years, using continuous compounding of interest?
10*exp(3*0.06)

## Problem 5 - A Simple Investment Strategy
loanTest$profit = exp(loanTest$int.rate*3) - 1
loanTest$profit[loanTest$not.fully.paid == 1] = -1
summary(loanTest$profit)

## Problem 6 - An Investment Strategy Based on Risk
# To meet this objective, we will analyze an investment strategy in which the investor only purchases loans with a high interest rate (a rate of at least 15%), 
# but amongst these loans selects the ones with the lowest predicted risk of not being paid back in full. We will model an investor 
# who invests $1 in each of the most promising 100 loans.

# First, use the subset() function to build a data frame called highInterest consisting of the test set loans with an interest rate of at least 15%.
summary(loanTest$int.rate)
highrate = subset(loanTest, loanTest$int.rate>0.15)

# What is the average profit of a $1 investment in one of these high-interest loans (do not include the $ sign in your answer)?
mean(highrate$profit)

# What proportion of the high-interest loans were not paid back in full?
table(highrate$not.fully.paid)
110/(110+327)

# How many of 100 selected loans were not paid back in full?
cutoff = sort(highrate$predicted.risk, decreasing=FALSE)[100]
newdata = subset(highrate, highrate$predicted.risk < cutoff)
table(newdata$not.fully.paid)
18/(18+81)
sum(newdata$profit)

