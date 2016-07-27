
#### Week 4 ####

#############  UNDERSTANDING WHY PEOPLE VOTE  #################
gerber <- read.csv("~/Downloads/mit/gerber.csv")
str(gerber)

#### Problem 1.1 - Exploration and Logistic Regression

## What proportion of people in this dataset voted in this election?
table(gerber$voting) 
108696/(108696+235388) #0.3159
mean(gerber$voting)

## Which of the four "treatment groups" had the largest percentage of people who actually voted (voting = 1)?
table(gerber$voting, gerber$hawthorne)
table(gerber$hawthorne)
12316/38204 # 0.3224 for hwathorne

table(gerber$voting, gerber$self)
table(gerber$self)
13191/38218 # 0.3452for self

table(gerber$voting, gerber$civicduty)
table(gerber$civicduty)
12021/38218 # 0.3145 for civicduty

table(gerber$voting, gerber$neighbors)
table(gerber$neighbors)
14438/38201 # 0.3779for neighbors

## Build a logistic regression model for voting using the four treatment group variables 
## as the independent variables (civicduty, hawthorne, self, and neighbors)
model_logistic = glm(voting~civicduty + hawthorne + self + neighbors, data=gerber, family=binomial)
summary(model_logistic) #all of the variables are significant

## Using a threshold of 0.3, what is the accuracy of the logistic regression model? 
prediction_logistic = predict(model_logistic, type="response")
table(gerber$voting, prediction_logistic > 0.3)
(134513+51966)/(134513+51966+100875+56730) #0.5420

## Using a threshold of 0.5, what is the accuracy of the logistic regression model?
table(gerber$voting, prediction_logistic > 0.5)
235388/(235388+108696) #0.6841

## Compare your previous two answers to the percentage of people who did not vote (the baseline accuracy) 
## and compute the AUC of the model. What is happening here?
1-mean(gerber$voting) #0.6841: the percentage of people who did not vote (the baseline accuracy)

## Compute the AUC with the following commands (if your model's predictions are called "predictLog"):
library(ROCR)
pred = prediction(prediction_logistic, gerber$voting)
as.numeric(performance(pred, "auc")@y.values) #0.5308

## Even though all of our variables are significant, our model does not improve over the baseline model of just predicting that someone will not vote, 
## and the AUC is low. So while the treatment groups do make a difference, this is a weak predictive model.

#### Problem 2 - Trees
library(rpart)
library(rpart.plot)

CARTmodel = rpart(voting ~ civicduty + hawthorne + self + neighbors, data=gerber)
prp(CARTmodel)
## If you plot the tree, with prp(CARTmodel), you should just see one leaf! There are no splits in the tree, because none of the variables make a big enough effect to be split on.

## to force the complete tree to be built
CARTmodel2 = rpart(voting ~ civicduty + hawthorne + self + neighbors, data=gerber, cp=0.0)
prp(CARTmodel2)
## We saw in Problem 1 that the highest fraction of voters was in the Neighbors group, followed by the Self group, followed by the Hawthorne group, and lastly the Civic Duty group. And we see here that the tree detects this trend.

## Make a new tree that includes the "sex" variable, again with cp = 0.0. Notice that sex appears as a split that is of secondary importance to the treatment group.
CARTmodel3 = rpart(voting ~ civicduty + hawthorne + self + neighbors + sex, data=gerber, cp=0.0)
prp(CARTmodel3)


#### Problem 3 - Interaction Terms
CARTmodel4 = rpart(voting ~ control, data=gerber, cp=0.0)
prp(CARTmodel4, digits = 6)

CARTmodel5 = rpart(voting ~ control+sex, data=gerber, cp=0.0)
prp(CARTmodel5, digits = 6)
## Now, using the second tree (with control and sex), determine who is affected more by NOT being in the control group (being in any of the four treatment groups):
0.334176-0.290456 #0.04372
0.345818-0.302795 #0.043023

## Going back to logistic regression now, create a model using "sex" and "control". Interpret the coefficient for "sex":
model_logistic2 = glm(voting~sex+control, data=gerber, family="binomial")
summary(model_logistic2)
## If you look at the summary of the model, you can see that the coefficient for the "sex" variable is -0.055791. This means that women are less likely to vote, since women have a larger value in the sex variable, and a negative coefficient means that larger values are predictive of 0.


## What is the absolute difference between the tree and the logistic regression for the (Woman, Control) case?
Possibilities = data.frame(sex=c(0,0,1,1),control=c(0,1,0,1))
predict(model_logistic2, newdata=Possibilities, type="response")
## The CART tree predicts 0.290456 for the (Woman, Control) case, and the logistic regression model predicts 0.2908065. So the absolute difference, to five decimal places, is 0.00035.


## How do you interpret the coefficient for the new variable in isolation? That is, how does it relate to the dependent variable?
model_logistic3 = glm(voting ~ sex + control + sex:control, data=gerber, family="binomial")
summary(model_logistic3)
## This coefficient is negative, so that means that a value of 1 in this variable decreases the chance of voting. This variable will have variable 1 if the person is a woman and in the control group.

## Now what is the difference between the logistic regression model and the CART model for the (Woman, Control) case?
predict(model_logistic3, newdata=Possibilities, type="response")
0.2904558-0.290456
## The logistic regression model now predicts 0.2904558 for the (Woman, Control) case, so there is now a very small difference (practically zero) between CART and logistic regression.

## This example has shown that trees can capture nonlinear relationships that logistic regression can not, but that we can get around this sometimes by using variables that are the combination of two variables. 



###########################  LETTER RECOGNITION ############################

#### Problem 1.1 - Predicting B or not B
letters <- read.csv("~/Downloads/mit/letters_ABPR.csv")
str(letters)

## create a new variable isB in the dataframe, which takes the value "TRUE" if the observation corresponds to the letter B, and "FALSE" if it does not
letters$isB <- as.factor(letters$letter=="B")

## Now split the data set into a training and testing set, putting 50% of the data in the training set. 
## Set the seed to 1000 before making the split. 
## The first argument to sample.split should be the dependent variable "letters$isB". 
## Remember that TRUE values from sample.split should go in the training set.
library(caTools)
set.seed(1000)
ratio = sample.split(letters$isB, SplitRatio=0.5)
train = subset(letters, ratio==TRUE)
test = subset(letters, ratio==FALSE)

## What is the accuracy of this baseline method on the test set?
table(test$isB)
1175/(383+1175) #0.754172

#### classification tree
## Now build a classification tree to predict whether a letter is a B or not, using the training set to build your model.
library(rpart)
library(rpart.plot)

CARTb = rpart(isB ~ . -letter, data=train, method="class")
prp(CARTb)
## We are just using the default parameters in our CART model, so we don't need to add the minbucket or cp arguments at all. We also added the argument method="class" since this is a classification problem.

## What is the accuracy of the CART model on the test set? (Use type="class" when making predictions on the test set.)
CARTb_prediction = predict(CARTb, newdata=test, type="class")
table(test$isB, CARTb_prediction)
(1126+319)/(49+1126+64+319) #0.9275

#### random FOREST
## Now, build a random forest model to predict whether the letter is a B or not (the isB variable) using the training set.
library(randomForest)
RandomForestb = randomForest(isB ~ . -letter, data=train)

## What is the accuracy of the model on the test set?
predict_forest = predict(RandomForestb, newdata=test)
## accuracy of random forest model
table(test$isB, predict_forest)
(1168+363)/(1168+7+20+363) #0.9827
## In lecture, we noted that random forests tends to improve on CART in terms of predictive accuracy. Sometimes, this improvement can be quite significant, as it is here.


#### Problem 2.1 - Predicting the letters A, B, P, R
## predict whether or not a letter is one of the four letters A, B, P or R.
letters$letter = as.factor( letters$letter )

library(caTools)
set.seed(2000)
ratio = sample.split(letters$letter, SplitRatio=0.5)
train = subset(letters, ratio==TRUE)
test = subset(letters, ratio==FALSE)

## In a multiclass classification problem, a simple baseline model is to predict the most frequent class of all of the options.
## What is the baseline accuracy on the testing set?
table(test$letter)
401/(395+383+401+379) #0.257


#### Problem 2.2 - Predicting the letters A, B, P, R
## Now build a classification tree to predict "letter", using the training set to build your model.
CARTletter =rpart(letter ~ . -isB, data=train, method="class")
prp(CARTb)

## What is the test set accuracy of your CART model? Use the argument type="class" when making predictions.
CARTletter_prediction = predict(CARTletter, newdata=test, type="class")
table(test$letter, CARTletter_prediction)
(348+318+363+340)/nrow(test) #0.8787


#### Problem 2.3 - Predicting the letters A, B, P, R
## Now build a random forest model on the training data, using the same independent variables 
## as in the previous problem -- again, don't forget to remove the isB variable. 
library(randomForest)
RandomForest_letter = randomForest(letter ~ . , data=train)
RandomForest_letter_prediction = predict(RandomForest_letter, newdata=test, type="class")
table(test$letter, RandomForest_letter_prediction)
(388+383+398+377)/nrow(test) #0.9923

## You should find this value rather striking, for several reasons. 
## The first is that it is significantly higher than the value for CART, highlighting the gain in accuracy that is possible from using random forest models. 
## The second is that while the accuracy of CART decreased significantly as we transitioned from the problem of predicting B/not B (a relatively simple problem) to the problem of predicting the four letters (certainly a harder problem), 
## the accuracy of the random forest model decreased by a tiny amount.



####################  PREDICTING EARNINGS FROM CENSUS DATA  ######################
census <- read.csv("~/Downloads/mit/census.csv")
str(census)

#### Problem 1.1 - A Logistic Regression Model
## split the data randomly into a training set and a testing set
library(caTools)
set.seed(2000)
spt = sample.split(census$over50k, SplitRatio=0.6)
train = subset(census, spt==TRUE)
test = subset(census, spt=FALSE)

## build a logistic regression model to predict the dependent variable "over50k", using all of the other variables in the dataset as independent variables. Use the training set to build the model.
census_logistic = glm(over50k ~ . , data=train, family = "binomial")

## Which variables are significant, or have factors that are significant? 
summary(census_logistic)

#### Problem 1.2 - A Logistic Regression Model
## What is the accuracy of the model on the testing set? Use a threshold of 0.5. 
census_predict_logistic = predict(census_logistic, newdata=test, type="response")
table(test$over50k, census_predict_logistic > 0.5)
(22612+4656)/(22612+1671+3039+4656) #0.8527

#### Problem 1.3 - A Logistic Regression Model
## What is the baseline accuracy for the testing set?
table(test$over50k)
24283/(24283+7695) #0.7594: We need to first determine the most frequent outcome in the training set.

#### Problem 1.4 - A Logistic Regression Model
## What is the area-under-the-curve (AUC) for this model on the test set?
library(ROCR)
pred = prediction(census_predict_logistic, test$over50k)
perf = performance(pred, "tpr", "fpr")
plot(perf)
as.numeric(performance(pred, "auc")@y.values) #0.9079

#### Problem 2.1 - A CART Model
## We have just seen how the logistic regression model for this data achieves a high accuracy. 
## Moreover, the significances of the variables give us a way to gauge which variables are relevant for this prediction task. However, it is not immediately clear which variables are more important than the others, 
## especially due to the large number of factor variables in this problem.

## build a classification tree to predict "over50k"
library(rpart)
library(rpart.plot)

census_class = rpart(over50k ~ ., data=train, method="class")
prp(census_class) # There are four splits in total.

#### Problem 2.2 - A CART Model
## Which variable does the tree split on at the first level (the very first split of the tree)?
#  the first level is the most significant

#### Problem 2.3 - A CART Model
## Which variables does the tree split on at the second level (immediately after the first split of the tree)? Select all that apply.
#  The second splits are on capitalgains and education.


#### Problem 2.4 - A CART Model
## What is the accuracy of the model on the testing set? Use a threshold of 0.5.
census_predict_class = predict(census_class, newdata=test, type="class")
table(test$over50k, census_predict_class)
(23092+3918)/(23092+1191+3777+3918) #0.8446

## This highlights a very regular phenomenon when comparing CART and logistic regression. CART often performs a little worse than logistic regression in out-of-sample accuracy. However, as is the case here, the CART model is often much simpler to describe and understand.

#### Problem 2.5 - A CART Model
## consider the ROC curve and AUC for the CART model on the test
census_predict_class = predict(census_class, newdata=test)
pred = prediction(census_predict_class[,2], test$over50k)
perf = performance(pred, "tpr", "fpr")
plot(perf)

#### Problem 2.6 - A CART Model
## What is the AUC of the CART model on the test set?
as.numeric(performance(pred, "auc")@y.values) #0.8439


#### Problem 3.1 - A Random Forest Model
## Before building a random forest model, we'll down-sample our training set. 
## While some modern personal computers can build a random forest model on the entire training set, others might run out of memory when trying to train the model since random forests 
## is much more computationally intensive than CART or Logistic Regression. For this reason, before continuing we will define a new training set to be used when building our random forest model, 
## that contains 2000 randomly selected obervations from the original training set. Do this by running the following commands in your R console (assuming your training set is called "train"):
set.seed(1)
trainSmall = train[sample(nrow(train), 2000), ]

library(randomForest)
set.seed(1)
census_forest = randomForest(over50k ~ ., data=trainSmall)
summary(census_forest)

## make predictions using this model on the entire test set. What is the accuracy of the model on the test set, using a threshold of 0.5?
census_predict_forest = predict(census_forest, newdata=test)

## accuracy of random forest model
table(test$over50k, census_predict_forest)
(23976+2760)/(23976+2760+307+4935)

#### Problem 3.2 - A Random Forest Model: the number of times
## As we discussed in lecture, random forest models work by building a large collection of trees. As a result, we lose some of the interpretability that comes with CART in terms of seeing how predictions are made and which variables are important. 
## However, we can still compute metrics that give us insight into which variables are important.
## One metric that we can look at is the number of times, aggregated over all of the trees in the random forest model, that a certain variable is selected for a split. 
vu = varUsed(census_forest, count=TRUE)
vusorted = sort(vu, decreasing = FALSE, index.return = TRUE)
dotchart(vusorted$x, names(census_forest$forest$xlevels[vusorted$ix]))
## This code produces a chart that for each variable measures the number of times that variable was selected for splitting (the value on the x-axis). 
## Which of the following variables is the most important in terms of the number of splits?

#### Problem 3.3 - A Random Forest Model: impurity
## A different metric we can look at is related to "impurity", which measures how homogenous each bucket or leaf of the tree is. In each tree in the forest, 
## whenever we select a variable and perform a split, the impurity is decreased. Therefore, one way to measure the importance of a variable is to average the reduction in impurity, 
## taken over all the times that variable is selected for splitting in all of the trees in the forest. 
varImpPlot(census_forest)


#### Problem 4.1 - Selecting cp by Cross-Validation
## We now conclude our study of this data set by looking at how CART behaves with different choices of its parameters.
## Let us select the cp parameter for our CART model using k-fold cross validation, with k = 10 folds.
library(caret)
library(e1071)
library(ggplot2)

## define how many fold we want
numFolds = trainControl(method="cv", number=10) #(cv indicates cross validation, 10 means 10 folds)
cartGrid = expand.grid( .cp = seq(0.002,0.1,0.002))

## perform cross validation
train(over50k ~ ., data=train, method="rpart", trControl=numFolds, tuneGrid=cartGrid)

#### Problem 4.2 - Selecting cp by Cross-Validation
## Fit a CART model to the training data using this value of cp. What is the prediction accuracy on the test set?
census_class2 = rpart(over50k ~ ., data=train, method="class", cp=0.002)
census_predict_class2 = predict(census_class2, newdata=test, type="class")
table(test$over50k, census_predict_class2)
(22966+4552)/nrow(test) #0.8605



#### Problem 4.3 - Selecting cp by Cross-Validation
## Compared to the original accuracy using the default value of cp, this new CART model is an improvement, 
## and so we should clearly favor this new model over the old one -- or should we? 
## Plot the CART tree for this model. How many splits are there?
prp(census_class2)

## This highlights one important tradeoff in building predictive models. 
## By tuning cp, we improved our accuracy by over 1%, but our tree became significantly more complicated. 
## In some applications, such an improvement in accuracy would be worth the loss in interpretability. 
## In others, we may prefer a less accurate model that is simpler to understand and describe over a more accurate -- but more complicated -- model.






