
####### Week 4 Trees, class assignment

## Quick Question
## Suppose you have a subset of 20 observations, where 14 have outcome A and 6 have outcome B. What proportion of observations have outcome A?
14/20
## If we set the threshold to 0.25 when computing predictions of outcome A, will we predict A or B for these observations?
## Since 70% of these observations have outcome A, we will predict A if the threshold is below 0.7, and we will predict B if the threshold is above 0.7.

############################# Data exploration: judge, jury and classifier #########################
## load stevens data
stevens <- read.csv("~/Downloads/mit/stevens.csv")
str(stevens)

## split data into train and test data
library(caTools)
set.seed(3000)
spl = sample.split(stevens$Reverse, SplitRatio=0.7)
train = subset(stevens, spl==TRUE)
test = subset(stevens, spl==FALSE)

#####################  CART model (classification and regression tree) #################
## build tree model
# install.packages("rpart")
# install.packages("rpart.plot")
library(rpart)
library(rpart.plot)
stevens_tree = rpart(Reverse ~ Circuit + Issue + Petitioner + Respondent + LowerCourt + Unconst, 
                     data=train, method = "class", minbucket = 25)
prp(stevens_tree)


## how well does the model perform on the prediction
predict_test = predict(stevens_tree, newdata=test, type="class")
table(test$Reverse, predict_test)
(41+71)/(41+36+22+71)
# the accuracy of our CART model is 0.6588.

## build a baseline model
table(test$Reverse)
93/(93+77)
# the accuracy of our baseline model is 0.547

## build a logistic regression model
stevens_logistic = glm(Reverse ~ Circuit + Issue + Petitioner + Respondent + LowerCourt + Unconst, 
                       data=train, family=binomial)
summary(stevens_logistic)
predict_test_logistic = predict(stevens_logistic, newdata=test, type="response")
table(test$Reverse, predict_test_logistic>0.7)
(60+46)/(60+17+47+46)
# the accuracy of our logistic regression model is 0.624.

## evaluation of tree model and generation of ROC curves
library(ROCR)
PredictROC = predict(stevens_tree, newdata=test)
# PredictROC shows two columns: it gives two numbers which can be thought of as the probability of outcome 0 and the probability of outcome 1.
# We'll use the second column as our probabilities to generate an ROC curve.
pred = prediction(PredictROC[,2], test$Reverse)
perf = performance(pred, "tpr", "fpr")
plot(perf)

## Compute the AUC of the CART model
as.numeric(performance(pred, "auc")@y.values)

## change minibucket to 5
stevens_tree2 = rpart(Reverse ~ Circuit + Issue + Petitioner + Respondent + LowerCourt + Unconst, 
                     data=train, method = "class", minbucket = 5)
prp(stevens_tree2)

## change minibucket to 100
stevens_tree3 = rpart(Reverse ~ Circuit + Issue + Petitioner + Respondent + LowerCourt + Unconst, 
                      data=train, method = "class", minbucket = 100)
prp(stevens_tree3)


############################## Random forest model ########################
# designed to improve the prediction accuracy of CART
# works by buding a large number of CART trees
# each tree can split on only a random subset of the variables
# each tree is built from a bagged/bootstrapped sample of data
# The most important variables in a CART tree are at the top of the tree

## Random forest parameters
# minimum number of observations in a subset
# number of trees
# install.packages("randomForest")
library(randomForest)

## We want to classification problem and make sure variables are factors.
train$Reverse = as.factor(train$Reverse)
test$Reverse = as.factor(test$Reverse)

## build random forest model
stevens_forest = randomForest(Reverse ~ Circuit + Issue + Petitioner + Respondent + LowerCourt + Unconst,
                              data=train, nodesize=25, ntree=200)

## prediction on test data
predict_forest = predict(stevens_forest, newdata=test)

## accuracy of random forest model
table(test$Reverse, predict_forest)
(40+74)/(40+37+19+74) #0.6705882, the random forest model improve improved the accuracy a little bit over CART. 

#### Quick Question: Let's see what happens if we set the seed to two different values and create two different random forest models.
#### First, set the seed to 100, and the re-build the random forest model, exactly like we did in the previous video (Video 5). Then make predictions on the test set. What is the accuracy of the model on the test set?
set.seed(100)
spl = sample.split(stevens$Reverse, SplitRatio=0.7)
train = subset(stevens, spl==TRUE)
test = subset(stevens, spl==FALSE)
train$Reverse = as.factor(train$Reverse)
test$Reverse = as.factor(test$Reverse)

## build random forest model
stevens_forest = randomForest(Reverse ~ Circuit + Issue + Petitioner + Respondent + LowerCourt + Unconst,
                              data=train, nodesize=25, ntree=200)

## prediction on test data
predict_forest = predict(stevens_forest, newdata=test)

## accuracy of random forest model
table(test$Reverse, predict_forest)
(45+72)/(45+32+18+75) # 0.6882353

#### Now, set the seed to 200, and then re-build the random forest model, exactly like we did in the previous video (Video 5). Then make predictions on the test set. What is the accuracy of this model on the test set?
set.seed(200)
spl = sample.split(stevens$Reverse, SplitRatio=0.7)
train = subset(stevens, spl==TRUE)
test = subset(stevens, spl==FALSE)
train$Reverse = as.factor(train$Reverse)
test$Reverse = as.factor(test$Reverse)

## build random forest model
stevens_forest = randomForest(Reverse ~ Circuit + Issue + Petitioner + Respondent + LowerCourt + Unconst,
                              data=train, nodesize=25, ntree=200)

## prediction on test data
predict_forest = predict(stevens_forest, newdata=test)

## accuracy of random forest model
table(test$Reverse, predict_forest)
(39+73)/(39+38+20+73) # 0.6588235

#### As we see here, the random component of the random forest method can change the accuracy. 
#### The accuracy for a more stable dataset will not change very much, but a noisy dataset can be significantly affected by the random samples.


############################ K-fold Cross-validation #######################
## How should we select parameter: minbucket?
# K fold Cross-validation: one way to properly select the parameter value.
# In CART, the value of minbucket can affect the model's out-of-sample accuracy.
# by using cross validation, we can be sure that we're selecting a smart parameter value.

## Steps
# Given training set, split into k pieces
# use k-1 folds to estimate a model, and test model on remaining one fold (validation set) for each candidate parameter 
# cp: It's like Adjusted R-squared for linear regression, and AIC for logistic regression, in that it measures
# the trade-off between model complexity and accuracy on the training set.
# A smaller cp value leads to a bigger tree, so a smaller cp value might over-fit the model to the training set.

#install.packages("caret")
#install.packages("e1071")
library(caret)
library(e1071)
library(ggplot2)

## define how many fold we want
numFolds = trainControl(method="cv", number=10) #(cv indicates cross validation, 10 means 10 folds)
## pick the possible values for our cp
cpGrid = expand.grid(.cp=seq(0.01,0.5,0.01)) # This will define our cp parameters to test as numbers from 0.01 to 0.5, in increments of 0.01.

## perform cross validation
train(Reverse ~ Circuit + Issue + Petitioner + Respondent + LowerCourt + Unconst, 
      data=train, method="rpart", trControl=numFolds, tuneGrid=cpGrid)
# Our data set here is Train, with a capital T, and then we need to add the arguments method = "rpart",
# since we want to cross validate a CART model, and then trControl = numFolds, the output of our trainControl function, 
# and then tuneGrid = cpGrid, the output of the expand.grid function.


## The final value used for the model was cp = 0.18."
# This is the cp value we want to use in our CART model. So now let's create a new CART model with this value of cp, instead of the minbucket parameter.
stevensTreeCV = rpart(Reverse ~ Circuit + Issue + Petitioner + Respondent + LowerCourt + Unconst, 
                      data=train, method = "class", cp = 0.18)
prp(stevensTreeCV)

## predicion of new CART model
predict_treeCV = predict(stevensTreeCV, newdata=test, type="class")
table(test$Reverse, predict_treeCV)
(59+64)/(59+18+29+64) # 0.7235294

## Cross validation helps us make sure we're selecting a good parameter value, and often this will significantly increase the accuracy.


######################## Data exploration:  Claim data ###########################
claim <- read.csv("~/Downloads/mit/ClaimsData.csv")
str(claim)
table(claim$bucket2009)/nrow(claim)

library(caTools)
sqt =sample.split(claim$bucket2009, SplitRatio=0.6)
train = subset(claim, sqt==TRUE)
test = subset(claim, sqt==FALSE)

## What is the average age of patients in the training set, ClaimsTrain?
summary(train) #72.59

## What proportion of people in the training set had at least one diagnosis code for diabetes?
table(train$diabetes)
104757/(104757+170046)

################### Baseline method on test dataset ###################
# The baseline method would predict that the cost bucket for a patient in 2009 will be the same as it was in 2008.
# And the baseline is to simply predict that the cost in the next "period" will be the cost in the current period.
table(test$bucket2009, test$bucket2008)
(110086+10803+2707+1562+98)/nrow(test) # 0.6837: the accuracy of the baseline method is 0.68.
penaltymatrix = matrix(c(0,1,2,3,4,2,0,1,2,3,4,2,0,1,2,6,4,2,0,1,8,6,4,2,0), byrow=TRUE, nrow=5) #create penalty matrix
penaltymatrix

## multiply by penalty matrix
as.matrix(table(test$bucket2009, test$bucket2008))*penaltymatrix

## calulate penalty error
sum(as.matrix(table(test$bucket2009, test$bucket2008))*penaltymatrix)/nrow(test)

## our goal will be to create a CART model that has an accuracy higher than 68% and a penalty error lower than 0.74.

## A new baseline method would predict cost bucket 1 for everyone.
# What would the accuracy of this baseline method be on the test set?
table(test$bucket2009) #122978/nrow(test)=0.67127
(0*122978 + 2*34840 + 4*16390 + 6*7937 + 8*1057)/nrow(test) #1.044301


############################ Build a CART model and predict healthcare costs ##############
library(rpart)
library(rpart.plot)
claim_tree = rpart(bucket2009 ~ age + arthritis + alzheimers + cancer + copd + depression +
                     diabetes + heart.failure + ihd + kidney + osteoporosis + stroke +
                     bucket2008 + reimbursement2008, data=train, method="class", cp=0.00005)
prp(claim_tree)

## make a prediction
predict_claim = predict(claim_tree, newdata=test, type="class")
table(test$bucket2009, predict_claim)
(114510+15777+171+230+0)/nrow(test) # accruacy is 0.71

as.matrix(table(test$bucket2009, predict_claim))*penaltymatrix
sum(as.matrix(table(test$bucket2009, predict_claim))*penaltymatrix)/nrow(test) 
#penalty error: 0.75

## The rpart function allows us to specify a parameter called loss.
#  This is the penalty matrix we want to use when building our model.
#  The first CART model, without the loss matrix, predicted bucket 1 for 78.6% of the observations in the test set. Did the second CART model, with the loss matrix, 
#  predict bucket 1 for more or fewer of the observations, and why?
claim_tree = rpart(bucket2009 ~ age + arthritis + alzheimers + cancer + copd + depression +
                     diabetes + heart.failure + ihd + kidney + osteoporosis + stroke +
                     bucket2008 + reimbursement2008, data=train, method="class", cp=0.00005,
                   parms=list(loss=penaltymatrix))

## make a prediction
predict_claim = predict(claim_tree, newdata=test, type="class")
table(test$bucket2009, predict_claim)
(93959+19498+4577+616+0)/nrow(test) # accruacy is 0.647

as.matrix(table(test$bucket2009, predict_claim))*penaltymatrix
sum(as.matrix(table(test$bucket2009, predict_claim))*penaltymatrix)/nrow(test) 
#penalty error: 0.641

## If you look at the classification matrix for the second CART model, we predicted bucket 1 less frequently. This is because, according to the penalty matrix, some of the worst types of errors are to predict bucket 1 when the actual cost bucket is higher.

