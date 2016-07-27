
#### Can we accurately predict voting outcomes by using informal polling questions?

## What predicts voting outcomes? 
## In this competition, you'll be using data from Show of Hands, an informal polling platform for use on mobile devices and the web, to see what aspects and characteristics of people's lives predict how they will be voting for the presidential election.

## Classification and Regression Trees (CART) and random forests (RF)


############################ load train dataset ##########################
train <- read.csv("~/Downloads/mit/train2016.csv",na.strings=c("","NA")) #fill missing values with NA
str(train) #5568 obs. of  108 variables
summary(train)
colnames(train)

test <- read.csv("~/Downloads/mit/test2016.csv",na.strings=c("","NA")) #fill missing values with NA
str(test) #1392 obs. of  107 variables
summary(test)

############################ Data exploration ##############################
## see voting distribution
# Democrat Republican 
# 2951       2617 
table(train$Party)
contrasts(train$Party)
2951/(2951+2617) #basline accuracy = 0.53

## number of complete cases
nrow(train[complete.cases(train),]) #697/5568, we cannot drop the observations with missing values.
nrow(test[complete.cases(test),]) #168/1392 in test dataset.

## probability of missing values
propmiss <- function(dataframe) 
  lapply(dataframe,function(x) data.frame(nmiss=sum(is.na(x)), ntotal=length(x), PropbabilityofMissing=sum(is.na(x))/length(x)))
propmiss(train)
propmiss(test)
# Q124742, first question in the dataset by natural question order, have the highest % missing (62.3%), whereas other questions have % of missing between 30% ~ 45%.

#library(Amelia)
#missmap(train, main = "Missing values vs observed")
#missmap(test, main = "Missing values vs observed of test dataset")


## change order level of education and income
train$Income <- ordered(train$Income, 
                        levels=c("under $25,000", "$25,001 - $50,000", "$50,000 - $74,999","$75,000 - $100,000", "$100,001 - $150,000","over $150,000", NA))
train$EducationLevel <- ordered(train$EducationLevel, 
                                levels=c("Current K-12", "High School Diploma", "Current Undergraduate","Associate's Degree", "Bachelor's Degree","Master's Degree", "Doctoral Degree", NA))
summary(train$EducationLevel)
summary(train$Income)

## new variable age
train$Age <- 2012-train$YOB
test$Age <- 2012-test$YOB


######################## some initial investigation ##########
plot(train$Gender,train$Party, main="Demogratic and Republican by gender")

counts <- table(train$Party, train$Gender)
barplot(counts, xlab = "Gender", ylab = "Number of People", main = "Demogratic and Republican by EducationLevel")

counts <- table(train$Party, train$EducationLevel)
barplot(counts, xlab = "Education level", ylab = "Number of People", main = "Demogratic and Republican by EducationLevel")

## Are you a feminist?
plot(train$Q109244,train$Party, main="Demogratic and Republican by feminist")

## Do you personally own a gun?
plot(train$Q115611,train$Party, main="Demogratic and Republican by gun")

## Do you pray or meditate on a regular basis?
plot(train$Q98197,train$Party, main="Demogratic and Republican by pray")

## Does life have a purpose?
plot(train$Q10699,train$Party, main="Demogratic and Republican by life purpose")


########################  fill all missing values with "unknown" in train dataset ##############
train2 <- train
for (k in 1:ncol(train2)){train2[[k]] <- as.character(train2[[k]]) }
train2 <- as.matrix(train2)
y <- which(is.na(train2)==TRUE) 
train2[y] <- "unknown"
train2 <- as.data.frame(train2)

head(train2)
str(train2)

myvars <- c("USER_ID", "Age")
train3 <- train[myvars]
train4 <- merge(train3,train2,by="USER_ID")

train4$Age.x <- as.numeric(train4$Age.x)
train5 <- train4[c(-1, -3,-110)]

colnames(train5)[1] <- "Age"


########################  fill all missing values with "unknown" in test dataset ##############
test2 <- test
for (k in 1:ncol(test2)){test2[[k]] <- as.character(test2[[k]]) }
test2 <- as.matrix(test2)
y <- which(is.na(test2)==TRUE) 
test2[y] <- "unknown"
test2 <- as.data.frame(test2)

head(test2)
str(test2)

myvars <- c("USER_ID", "Age")
test3 <- test[myvars]
test4 <- merge(test3,test2,by="USER_ID")

test4$Age.x <- as.numeric(test4$Age.x)
test5 <- test4[c(-3,-109)]

colnames(test5)[2] <- "Age"


#########################  age outlier #######################
## check outliers
summary(train5$Age)
summary(test$Age)


## remove this observation where Age > 100 and Age<18
train5 <- subset(train5, train5$Age>0 | is.na(train5$Age))
#trainAge <- subset(trainAge, trainAge$Age<=100 | is.na(trainAge$Age))
summary(train5$Age)

train5$Age[is.na(train5$Age)] <- median(train5$Age, na.rm=TRUE)
test5$Age[is.na(test5$Age)] <- median(test5$Age, na.rm=TRUE)

############################## Build a logistic regression model: trainAge #################
votng_log2 = glm(Party ~ Q115611 + Q109244 + Q98197 + Q101163 + + Q98869 + HouseholdStatus, 
                data=train5, family=binomial)
summary(votng_log2)

train_pred2 <- predict(votng_log2, newdata=train5, type="response")
table(train5$Party, train_pred2>0.5)
(2181+1332)/(2181+1332+769+1283) #0.63


votng_log = glm(Party ~ ., data=train5, family=binomial)
summary(votng_log)
train_pred <- predict(votng_log, newdata=train5, type="response")
table(train5$Party, train_pred>0.5)
(2049+1675)/(2049+1675+901+940) #0.67


## Compute the AUC with the following commands (if your model's predictions are called "predictLog"):
library(ROCR)
pred = prediction(train_pred, train5$Party)
as.numeric(performance(pred, "auc")@y.values) #0.728

pred2 = prediction(train_pred2, train5$Party)
as.numeric(performance(pred2, "auc")@y.values) #0.687


################################## classification tree: trainAge  ##################################
## build a classification tree to predict viting results
library(rpart)
library(rpart.plot)
votng_class <- rpart(Party ~ ., data=train5, method="class")
prp(votng_class) # There are 3 splits in total.
votng_class_pred <- predict(votng_class, type = "class")
#Q109244, Q115611, Q113181

table(train5$Party, votng_class_pred)
(2256+1186)/(2256+1186+694+1429) #0.62

votng_class2 <- rpart(Party ~ ., data=train, method="class")
prp(votng_class2) # There are 3 splits in total.


################################# Random Tree Model: TrainAge ############################################
library(randomForest)
voting_forest = randomForest(Party ~ ., data=train5, nodesize=25, ntree=200)
RandomForest_pred = predict(voting_forest, newdata=train5, type="class")
table(train5$Party, RandomForest_pred)
(2950+2614)/((2950+2614+1)) #0.9998

vu = varUsed(voting_forest, count=TRUE)
vusorted = sort(vu, decreasing = FALSE, index.return = TRUE)
dotchart(vusorted$x, names(voting_forest$forest$xlevels[vusorted$ix]))
varImpPlot(voting_forest)


###############################  prediction on test dataset #######################
test_pred <- predict(voting_forest, test5, type = "class")
submit <- data.frame(USER_ID = test5$USER_ID, Predictions = test_pred)
write.csv(submit, file = "~/Downloads/mit/Prediction5.csv", row.names = FALSE)

test_pred2 <- predict(votng_log2, test5, type = "response")
submit <- data.frame(USER_ID = test5$USER_ID, Predictions = test_pred2)
write.csv(submit, file = "~/Downloads/mit/Prediction2.csv", row.names = FALSE)


test_pred3 <- predict(votng_class, test5, type = "class")
submit <- data.frame(USER_ID = test5$USER_ID, Predictions = test_pred3)
write.csv(submit, file = "~/Downloads/mit/Prediction3.csv", row.names = FALSE)
