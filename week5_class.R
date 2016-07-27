
####### Week 5 Text Analytics

## load data
tweets <- read.csv("~/Downloads/mit/tweets.csv", stringsAsFactors=FALSE)
str(tweets)
tweets$Negative = as.factor(tweets$Avg <= -1)
table(tweets$Negative)

## preprocessing
library("tm")
library("SnowballC")

## A corpus is a collection of documents. We'll need to convert our tweets to a corpus for pre-processing.
corpus = Corpus(VectorSource(tweets$Tweet))
corpus 
corpus[[1]]

corpus = tm_map(corpus, tolower) ##remove irregularities
corpus = tm_map(corpus, PlainTextDocument)

## Remove punctuation
corpus = tm_map(corpus, removePunctuation)
corpus[[1]]

## stopwords
stopwords("english")[1:10]
corpus = tm_map(corpus, removeWords, c("apple", stopwords("english")))
corpus[[1]]

# Stem document 
corpus = tm_map(corpus, stemDocument)
corpus[[1]]


## frequencies: the values in the matrix are the number of times that word apperar in the document
## DocumentTermMatrix that generates a matrix where the rows correspond to documents, in our case tweets,
## and the columns correspond to words in those tweets.
freq = DocumentTermMatrix(corpus)
freq #1181 tweets, 3289 words in our matrix
inspect(freq[1000:1005, 505:515])


## What are the most popular terms? and then we want to give an argument lowFreq, which
## is equal to the minimum number of times a term must appear to be displayed.
## 34 words apperar 56 times
findFreqTerms(freq, lowfreq=20)

## remove some terms not appear often
sparse = removeSparseTerms(freq, 0.995) 
#The sparsity threshold works as follows. If we say 0.98, this means to only keep terms that appear in 2% or more of the tweets.

tweetsSparse = as.data.frame(as.matrix(sparse))
colnames(tweetsSparse) = make.names(colnames(tweetsSparse))
tweetsSparse$Negative = tweets$Negative

## split into train and test dataset
library(caTools)
set.seed(123)
split = sample.split(tweetsSparse$Negative, SplitRatio=0.7)
train = subset(tweetsSparse, split==TRUE)
test = subset(tweetsSparse, split==FALSE)

## cart model to predict sentiment
library(rpart)
library(rpart.plot)
tweetCART = rpart(Negative ~ ., data=train, method="class")
prp(tweetCART)

pred_tweet_CART = predict(tweetCART, newdata=test, type="class")
table(test$Negative, pred_tweet_CART)
(294+18)/(294+6+37+18) #0.8789

## accuracy of baseline model
table(test$Negative)
300/355 #0.8451, CART model is better than baseline model.

## random forest model
library(randomForest)
set.seed(123)
tweetRF = randomForest(Negative ~ ., data=train)
pred_tweet_RF = predict(tweetRF, newdata=test)
table(test$Negative, pred_tweet_RF)
(293+21)/(293+7+34+21) #0.885
## So our random forest model has an accuracy of 0.885. This is a little better than our CART model,
## but due to the interpretability of our CART model, I'd probably prefer it over the random forest model.


## logistic model
tweetLog = glm(Negative ~ ., data=train, family = "binomial")
summary(tweetLog)
predictions = predict(tweetLog, newdata=test, type="response")

## confusion matrix (0.5)
table(test$Negative, predictions>0.5)
(253+32)/(253+47+23+32) #0.8028

## The accuracy is (254+37)/(254+46+18+37) = 0.8197183, which is worse than the baseline. If you were to compute the accuracy on the training set instead, you would see that the model does really well on the training set - this is an example of over-fitting. 
## The model fits the training set really well, but does not perform well on the test set. 
## A logistic regression model with a large number of variables is particularly at risk for overfitting.



######################## courtroom (recitation) #####################
#### load data
emails <- read.csv("~/Downloads/mit/energy_bids.csv", stringsAsFactors=FALSE)
str(emails)

emails$email[1] #first email
strwrap(emails$email[1]) #break down text into many lines

emails$responsive[1]
strwrap(emails$email[2]) #break down text into many lines
emails$responsive[2]

table(emails$responsive)

#### preprocessing
library(tm)
corpus = Corpus(VectorSource(emails$email))
strwrap(corpus[[1]])

corpus = tm_map(corpus, tolower)
corpus = tm_map(corpus, PlainTextDocument)
corpus = tm_map(corpus, removePunctuation)
corpus = tm_map(corpus, removeWords, stopwords("english"))
corpus = tm_map(corpus, stemDocument)
strwrap(corpus[[1]])

#### bag of words
dtm = DocumentTermMatrix(corpus)
dtm #855 * 22167
dtm = removeSparseTerms(dtm, 0.97) #we'll remove any term that doesn't appear in at least 3% of the documents. 
dtm #855*788

labeledTerms = as.data.frame(as.matrix(dtm))
labeledTerms$responsive = emails$responsive
str(labeledTerms)
# 788 variables are the frequencies of various words in the emails. the last one is the outcome variable - responsive
# 855 emails

#### building CART models
library(caTools)
set.seed(144)
spl = sample.split(labeledTerms$responsive, 0.7)
train = subset(labeledTerms, spl==TRUE)
test = subset(labeledTerms, spl==FALSE)
email_CART = rpart(responsive~., data=train, method="class")
prp(email_CART)

## prediction on test data
pred = predict(email_CART, newdata=test)
pred[1:10,]
pred.prob = pred[,2]
table(test$responsive, pred.prob>=0.5)
(195+25)/(195+20+25+17) #accuracy: 0.856

## accuracy of baseline model
table(test$responsive)
215/(215+42) #0.837, small improvement from CART model

#### ROC curve to understand the performance of our model at different cutoff
library(ROCR)
predROCR = prediction(pred.prob, test$responsive)
perfROCR = performance(predROCR, "tpr", 'fpr') #true positive rate and false positive rate
plot(perfROCR, colorize=TRUE)
## the best cutoff to select is entirely dependent on the costs assigned by the decision maker to false positives and true positives.
## in this part of the curve, where we have a true positive rate of around 70%,
## meaning that we're getting about 70% of all the responsive documents, and a false positive rate of about 20%, meaning that we're making mistakes
## and accidentally identifying as responsive 20% of the non-responsive documents.

## compute AUC value
performance(predROCR, "auc")@y.values #0.79
## We can see that we have an AUC in the test set of 79.4%, which means that our model can differentiate
## between a randomly selected responsive and non-responsive document about 80% of the time.
