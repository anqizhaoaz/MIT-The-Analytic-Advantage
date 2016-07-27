
#### Week 5 Homework ####

#################  DETECTING VANDALISM ON WIKIPEDIA ################

#### Problem 1.1 - Bags of Words
wiki <- read.csv("~/Downloads/mit/wiki.csv", stringsAsFactors=FALSE)
str(wiki)

## Convert the "Vandal" column to a factor using the command wiki$Vandal = as.factor(wiki$Vandal).
wiki$Vandal = as.factor(wiki$Vandal)

## How many cases of vandalism were detected in the history of this page?
table(wiki$Vandal)

#### Problem 1.2 - Bags of Words
## 1) Create the corpus for the Added column, and call it "corpusAdded".
library("tm")
library("SnowballC")
corpusAdded = Corpus(VectorSource(wiki$Added))

## 2) Remove the English-language stopwords.
corpusAdded = tm_map(corpusAdded, removeWords, stopwords("english"))

## 3) Stem the words.
corpusAdded = tm_map(corpusAdded, stemDocument)

## 4) Build the DocumentTermMatrix, and call it dtmAdded.
dtmAdded = DocumentTermMatrix(corpusAdded)
dtmAdded

#### Problem 1.3 - Bags of Words
## Filter out sparse terms by keeping only terms that appear in 0.3% or more of the revisions, and call the new matrix sparseAdded. How many terms appear in sparseAdded?
findFreqTerms(dtmAdded, lowfreq=20)
sparse = removeSparseTerms(dtmAdded, 0.997) 
sparse


#### Problem 1.4 - Bags of Words
## Convert sparseAdded to a data frame called wordsAdded, and then prepend all the words with the letter A
wordsAdded = as.data.frame(as.matrix(sparse))
colnames(wordsAdded) = paste("A", colnames(wordsAdded))

## repeat all process for removed bag of words
corpusRemoved = Corpus(VectorSource(wiki$Removed))
corpusRemoved = tm_map(corpusRemoved, removeWords, stopwords("english"))
corpusRemoved = tm_map(corpusRemoved, stemDocument)
dtmRemoved = DocumentTermMatrix(corpusRemoved)
dtmRemoved
findFreqTerms(dtmRemoved, lowfreq=20)
sparseR = removeSparseTerms(dtmRemoved, 0.997) 
sparseR
wordsRemoved = as.data.frame(as.matrix(sparseR))
colnames(wordsRemoved) = paste("R", colnames(wordsRemoved))

#### Problem 1.5 - Bags of Words
wikiWords = cbind(wordsAdded, wordsRemoved)
wikiWords$Vandal = wiki$Vandal

## create train and test dataset
library(caTools)
set.seed(123)
split = sample.split(wikiWords$Vandal, SplitRatio=0.7)
train = subset(wikiWords, split==TRUE)
test = subset(wikiWords, split==FALSE)

## What is the accuracy on the test set of a baseline method that always predicts "not vandalism" (the most frequent outcome)?
table(test$Vandal)
619/(619+554) #0.5277067


#### Problem 1.6 - Bags of Words
## Build a CART model to predict Vandal, using all of the other variables as independent variables.
library(rpart)
library(rpart.plot)
wikiCART = rpart(Vandal ~ ., data=train, method="class")

wikiCART_pred = predict(wikiCART, newdata=test, type='"class')
table(test$Vandal, wikiCART_pred)
(619+15)/(619+15+539) #0.540

#### Problem 1.7 - Bags of Words
## Plot the CART tree. How many word stems does the CART model use?
prp(wikiCART)

#### Problem 1.8 - Bags of Words
## Given the performance of the CART model relative to the baseline, what is the best explanation of these results?
## Although it beats the baseline, bag of words is not very predictive for this problem. 
## There is no reason to think there was anything wrong with the split. CART did not overfit, which you can check by computing the accuracy of the model on the training set. 
## Over-sparsification is plausible but unlikely, since we selected a very high sparsity parameter. The only conclusion left is simply that bag of words didn't work very well in this case.


#### Problem 2.1 - Problem-specific Knowledge
## identifying a key class of words, and counting words.
# The grepl function returns TRUE if a string is found in another string, e.g.
grepl("cat","dogs and cats",fixed=TRUE) # TRUE
grepl("cat","dogs and rats",fixed=TRUE) # FALSE
wikiWords2 = wikiWords

# Make a new column in wikiWords2 that is 1 if "http" was in Added:
wikiWords2$HTTP = ifelse(grepl("http",wiki$Added,fixed=TRUE), 1, 0)
table(wikiWords2$HTTP)

#### Problem 2.2 - Problem-Specific Knowledge
wikiTrain2 = subset(wikiWords2, split==TRUE)
wikiTest2 = subset(wikiWords2, split==FALSE)

wikiCART2 = rpart(Vandal ~ ., data=wikiTrain2, method="class")
prp(wikiCART2)

wikiCART_pred2 = predict(wikiCART2, newdata=wikiTest2, type="class")
table(wikiTest2$Vandal, wikiCART_pred2)
(609+57)/(609+9+488+57) #0.573


#### Problem 2.3 - Problem-Specific Knowledge
## What is the average number of words added?
wikiWords2$NumWordsAdded = rowSums(as.matrix(dtmAdded))
wikiWords2$NumWordsRemoved = rowSums(as.matrix(dtmRemoved))
mean(wikiWords2$NumWordsAdded)


#### Problem 2.4 - Problem-Specific Knowledge
## What is the new accuracy of the CART model on the test set?
wikiTrain2 = subset(wikiWords2, split==TRUE)
wikiTest2 = subset(wikiWords2, split==FALSE)
wikiCART2 = rpart(Vandal ~ ., data=wikiTrain2, method="class")
prp(wikiCART2)

wikiCART_pred2 = predict(wikiCART2, newdata=wikiTest2, type="class")
table(wikiTest2$Vandal, wikiCART_pred2)
(514+248)/(514+104+297+248) #0.655

#### Problem 3.1 - Using Non-Textual Data
wikiWords3 = wikiWords2
wikiWords3$Minor = wiki$Minor
wikiWords3$Loggedin = wiki$Loggedin

wikiTrain3 = subset(wikiWords3, split==TRUE)
wikiTest3 = subset(wikiWords3, split==FALSE)
wikiCART3 = rpart(Vandal ~ ., data=wikiTrain3, method="class")
prp(wikiCART3)

wikiCART_pred3 = predict(wikiCART3, newdata=wikiTest3, type="class")
table(wikiTest3$Vandal, wikiCART_pred3)
(595+241)/(595+23+304+241) #0.7188


#### Problem 3.2 - Using Non-Textual Data
prp(wikiCART3)


###################  AUTOMATING REVIEWS IN MEDICINE  ################

#### PART 1 Loading the Data
clinical <- read.csv("~/Downloads/mit/clinical_trial.csv", stringsAsFactors=FALSE)
summary(clinical)
str(clinical)

## How many characters are there in the longest abstract? (Longest here is defined as the abstract with the largest number of characters.)
max(nchar(clinical$abstract))

## How many search results provided no abstract?
table(nchar(clinical$abstract)==0)

## Find the observation with the minimum number of characters in the title
clinical$title[which.min(nchar(clinical$title))]

#### PART 2 Preparing the Corpus
library("tm")
library("SnowballC")
corpusTitle = Corpus(VectorSource(clinical$title))
corpusAbstract = Corpus(VectorSource(clinical$abstract))

## Convert corpusTitle and corpusAbstract to lowercase
corpusTitle = tm_map(corpusTitle, tolower)
corpusAbstract = tm_map(corpusAbstract, tolower)
corpusTitle = tm_map(corpusTitle, PlainTextDocument)
corpusAbstract = tm_map(corpusAbstract, PlainTextDocument)

## Remove the punctuation in corpusTitle and corpusAbstract
corpusTitle = tm_map(corpusTitle, removePunctuation)
corpusAbstract = tm_map(corpusAbstract, removePunctuation)

## Remove the English language stop words from corpusTitle and corpusAbstract.
corpusTitle = tm_map(corpusTitle, removeWords, stopwords("english"))
corpusAbstract = tm_map(corpusAbstract, removeWords, stopwords("english"))

## Stem the words in corpusTitle and corpusAbstract
corpusTitle = tm_map(corpusTitle, stemDocument)
corpusAbstract = tm_map(corpusAbstract, stemDocument)

## Build a document term matrix called dtmTitle from corpusTitle and dtmAbstract from corpusAbstract.
dtmTitle = DocumentTermMatrix(corpusTitle)
dtmAbstract = DocumentTermMatrix(corpusAbstract)

## Limit dtmTitle and dtmAbstract to terms with sparseness of at most 95%
dtmTitle = removeSparseTerms(dtmTitle, 0.95) 
dtmAbstract = removeSparseTerms(dtmAbstract, 0.95) 

## How many terms remain in dtmTitle after removing sparse terms (aka how many columns does it have)?
dtmTitle #1860 X 31

## How many terms remain in dtmAbstract?
dtmAbstract #1860 X 335

##  Convert dtmTitle and dtmAbstract to data frames (keep the names dtmTitle and dtmAbstract).
dtmTitle = as.data.frame(as.matrix(dtmTitle))
dtmAbstract = as.data.frame(as.matrix(dtmAbstract))

## What is the most likely reason why dtmAbstract has so many more terms than dtmTitle?
# Because titles are so short, a word needs to be very common to appear in 5% of titles. Because abstracts have many more words, a word can be much less common and still appear in 5% of abstracts.

## What is the most frequent word stem across all the abstracts? Hint: you can use colSums() to compute the frequency of a word across all the abstracts.
which.max(colSums(dtmAbstract))

#### PART 3 Build a model
## Adding the letter T in front of all the title variable names and adding the letter A in front of all the abstract variable names. 
colnames(dtmTitle) = paste0("T", colnames(dtmTitle))
colnames(dtmAbstract) = paste0("A", colnames(dtmAbstract))

## combine dtmTitle and dtmAbstract into a single data frame
dtm = cbind(dtmTitle, dtmAbstract)

## add the dependent variable "trail"
dtm$trial = clinical$trial

## How many columns are in this combined data frame?
str(dtm)

## split it into a training and testing set
library(caTools)
set.seed(144)
spt = sample.split(dtm$trial, 0.7)
train = subset(dtm, spt==TRUE)
test = subset(dtm, spt==FALSE)

## What is the accuracy of the baseline model on the training set?
table(train$trial)
730/(730+572) #0.561

## build CART model
library(rpart)
library(rpart.plot)
trainCART = rpart(trial~., data=train, method="class")
prp(trainCART)

## Obtain the training set predictions for the model 
trainCART_pred = predict(trainCART)
which.max(trainCART_pred[,2])
summary(trainCART_pred) #0.872
table(train$trial, trainCART_pred[,2]>0.5)

## Without running the analysis, how do you expect the maximum predicted probability to differ in the testing set?
# Because the CART tree assigns the same predicted probability to each leaf node and there are a small number of leaf nodes compared to data points, 
# we expect exactly the same maximum predicted probability.

## What is the training set accuracy of the CART model?
(631+441)/(631+441+99+131) #0.8233

## What is the training set sensitivity of the CART model?
441/(441+131) #0.771

## What is the training set specificity of the CART model?
631/(631+99) #0.864

#### PART 4 Evaluating the model on the testing set
## predict on test data
testCART_pred = predict(trainCART, newdata=test)
table(test$trial, testCART_pred[,2]>0.5)

## What is the testing set accuracy, assuming a probability threshold of 0.5 for predicting that a result is a clinical trial?
(261+162)/(261+52+83+162) #0.758

## Using the ROCR package, what is the testing set AUC of the prediction model?
library(ROCR)
predROCR = prediction(testCART_pred[,2], test$trial)
perfROCR = performance(predROCR, "tpr", 'fpr') #true positive rate and false positive rate
plot(perfROCR, colorize=TRUE)
performance(predROCR, "auc")@y.values #0.837

#### PART 5: DECISION-MAKER TRADEOFFS


##################   SEPARATING SPAM FROM HAM (PART 1)  #####################

#### load data
emails <- read.csv("~/Downloads/mit/emails.csv")
# How many emails are in the dataset?
str(emails)
# How many of the emails are spam?
table(emails$spam)
# Which word appears at the beginning of every email in the dataset? Respond as a lower-case word with punctuation removed.
head(emails)
# How many characters are in the longest email in the dataset (where longest is measured in terms of the maximum number of characters)
max(nchar(as.character(emails$text)))
# Which row contains the shortest email in the dataset?
which.min(nchar(as.character(emails$text)))

#### Problem 2 - Preparing the Corpus
# 1) Build a new corpus variable called corpus.
library("tm")
library("SnowballC")
corpus = Corpus(VectorSource(emails$text))

# 2) Using tm_map, convert the text to lowercase.
corpus = tm_map(corpus, tolower) ##remove irregularities
corpus = tm_map(corpus, PlainTextDocument)

# 3) Using tm_map, remove all punctuation from the corpus.
corpus = tm_map(corpus, removePunctuation)

# 4) Using tm_map, remove all English stopwords from the corpus.
corpus= tm_map(corpus, removeWords, stopwords("english"))

# 5) Using tm_map, stem the words in the corpus.
corpus= tm_map(corpus, stemDocument)

# 6) Build a document term matrix from the corpus, called dtm.
freq = DocumentTermMatrix(corpus)
freq

## remove some terms not appear often (at least 5% of documents)
spdtm = removeSparseTerms(freq, 0.95) 
spdtm

## What is the word stem that shows up most frequently across all the emails in the dataset?
emailsSparse = as.data.frame(as.matrix(spdtm))
colnames(emailsSparse) = make.names(colnames(emailsSparse))
which.max(colSums(emailsSparse))

## Add a variable called "spam" to emailsSparse containing the email spam labels.
emailsSparse$spam = emails$spam

## How many word stems appear at least 5000 times in the ham emails in the dataset?
hamSparse = subset(emailsSparse, emailsSparse$spam == 0)
hamSparse = colSums(hamSparse)
sort(hamSparse)

## How many word stems appear at least 1000 times in the spam emails in the dataset?
spamSparse = subset(emailsSparse, emailsSparse$spam == TRUE)
spamSparse = colSums(spamSparse)
sort(spamSparse)
# Note that the variable "spam" is the dependent variable and is not the frequency of a word stem.


#### Problem 3 - Building machine learning models
## Convert the dependent variable to a factor
emailsSparse$spam = as.factor(emailsSparse$spam)
library(caTools)
set.seed(123)
spt = sample.split(emailsSparse$spam, 0.7)
train = subset(emailsSparse, spt==TRUE)
test =  subset(emailsSparse, spt==FALSE)

## build logistic regression
spamLog = glm(spam~., data=train, family=binomial)
summary(spamLog)
# For each model, obtain the predicted spam probabilities for the training set. 
predict_train_Log = predict(spamLog, type="response")
length(which(predict_train_Log < 0.00001))
length(which(predict_train_Log > 0.99999))
length(which(predict_train_Log > 0.00001 & predict_train_Log < 0.99999))
# What is the training set accuracy of spamLog, using a threshold of 0.5 for predictions?
table(train$spam, predict_train_Log>0.5)
(3052+954)/(3052+954+4+0) #0.999
# What is the training set AUC of spamLog? 0.9999
library(ROCR)
ROCRpred = prediction(predict_train_Log, train$spam)
as.numeric(performance(ROCRpred, "auc")@y.values)

## build CART model
library(rpart)
library(rpart.plot)
spamCART = rpart(spam~., data=train, method="class")

# For each model, obtain the predicted spam probabilities for the training set. 
predTrainCART= predict(spamCART)

# How many of the word stems "enron", "hou", "vinc", and "kaminski" appear in the CART tree? 
prp(spamCART)
# What is the training set accuracy of spamCART, using a threshold of 0.5 for predictions? 
table(train$spam, predTrainCART[,2] > 0.5)
(2885+894)/nrow(train) #0.942
# What is the training set AUC of spamCART? 0.9696
ROCRpred = prediction(predTrainCART[,2], train$spam)
as.numeric(performance(ROCRpred, "auc")@y.values) 

## build a random forest model
set.seed(123)
library(randomForest)
spamRF = randomForest(spam~., data=train)
# For each model, obtain the predicted spam probabilities for the training set. 
predict_train_RF= predict(spamRF, type="prob")
predict_train_RF
# What is the training set accuracy of spamRF, using a threshold of 0.5 for predictions? 
table(train$spam, predict_train_RF[,2] > 0.5)
(3013+914)/nrow(train) #0.9696
# What is the training set AUC of spamRF? 0.998
ROCRpred = prediction(predict_train_RF[,2], train$spam)
as.numeric(performance(ROCRpred, "auc")@y.values)

## Which model had the best training set performance, in terms of accuracy and AUC?

#### Problem 4 - Evaluating on the Test Set
# What is the testing set accuracy of spamLog, using a threshold of 0.5 for predictions?
predict_test_Log = predict(spamLog, newdata=test, type="response")
table(test$spam, predict_test_Log>0.5)
(1257+376)/nrow(test) #0.95
# What is the testing set AUC of spamLog? 0.9627517
ROCRpred = prediction(predict_test_Log, test$spam)
as.numeric(performance(ROCRpred, "auc")@y.values)

# What is the testing set accuracy of spamCART, using a threshold of 0.5 for predictions?
predict_test_CART = predict(spamCART, newdata=test)
table(test$spam, predict_test_CART[,2]>0.5)
(1228+386)/nrow(test) #0.939
# What is the testing set AUC of spamCART? 0.963
ROCRpred = prediction(predict_test_CART[,2], test$spam)
as.numeric(performance(ROCRpred, "auc")@y.values)

# What is the testing set accuracy of spamRF, using a threshold of 0.5 for predictions?
predict_test_RF = predict(spamRF, newdata=test, type='prob')
table(test$spam, predict_test_RF[,2]>0.5)
(1290+385)/nrow(test) #0.975
# What is the testing set AUC of spamRF? 0.9976
ROCRpred = prediction(predict_test_RF[,2], test$spam)
as.numeric(performance(ROCRpred, "auc")@y.values)

## Which model had the best testing set performance, in terms of accuracy and AUC?
## Which model demonstrated the greatest degree of overfitting?
#  Both CART and random forest had very similar accuracies on the training and testing sets. However, logistic regression obtained nearly perfect accuracy and AUC on the training set and had far-from-perfect performance on the testing set. This is an indicator of overfitting.



