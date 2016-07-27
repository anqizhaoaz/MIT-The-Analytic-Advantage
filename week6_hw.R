
###############   DOCUMENT CLUSTERING WITH DAILY KOS   ###############
## The file dailykos.csv contains data on 3,430 news articles or blogs that have been posted on Daily Kos. These articles were posted in 2004, leading up to the United States Presidential Election. The leading candidates were incumbent President George W. Bush (republican) and John Kerry (democratic). Foreign policy was a dominant topic of the election, specifically, the 2003 invasion of Iraq. 
## Each of the variables in the dataset is a word that has appeared in at least 50 different articles (1,545 words in total). 
dailykos <- read.csv("~/Downloads/mit/dailykos.csv")
str(dailykos)

#### Problem 1.1 - Hierarchical Clustering
distance = dist(dailykos, method="euclidean")
clusterKos = hclust(distance, method = "ward")

#### Problem 1.2 and 1.3 - Hierarchical Clustering
plot(clusterMovies)
rect.hclust(clusterMovies, k=7, border="red")

#### Problem 1.4 - Hierarchical Clustering
## In this problem, we are trying to cluster news articles or blog posts into groups. This can be used to show readers categories to choose from when trying to decide what to read.
# Thinking about the application, it is probably better to show the reader more categories than 2 or 3. These categories would probably be too broad to be useful. Seven or eight categories seems more reasonable.
## Use the cutree function to split your data into 7 clusters
clusterGroups = cutree(clusterKos, k=7)
table(dailykos,  clusterGroups, mean)

#### Problem 1.5 and 1.6 - Hierarchical Clustering
HierCluster1 = subset(dailykos, clusterGroups == 1)
tail(sort(colMeans(HierCluster1)))

HierCluster2 = subset(dailykos, clusterGroups == 2)
tail(sort(colMeans(HierCluster2)))

HierCluster3 = subset(dailykos, clusterGroups == 3)
tail(sort(colMeans(HierCluster3)))

HierCluster4 = subset(dailykos, clusterGroups == 4)
tail(sort(colMeans(HierCluster4)))

HierCluster5 = subset(dailykos, clusterGroups == 5)
tail(sort(colMeans(HierCluster5)))

HierCluster6 = subset(dailykos, clusterGroups == 6)
tail(sort(colMeans(HierCluster6)))

HierCluster7 = subset(dailykos, clusterGroups == 7)
tail(sort(colMeans(HierCluster7)))

#### Problem 2.1 - K-Means Clustering
set.seed(1000)
KMC = kmeans(dailykos, centers = 7, iter.max = 1000)
str(KMC)
table(KMC$cluster)

#### Problem 2.2 - K-Means Clustering
KmeansCluster1 = subset(dailykos, KMC$cluster == 1)
KmeansCluster2 = subset(dailykos, KMC$cluster == 2)
KmeansCluster3 = subset(dailykos, KMC$cluster == 3)
KmeansCluster4 = subset(dailykos, KMC$cluster == 4)
KmeansCluster5 = subset(dailykos, KMC$cluster == 5)
KmeansCluster6 = subset(dailykos, KMC$cluster == 6)
KmeansCluster7 = subset(dailykos, KMC$cluster == 7)

tail(sort(colMeans(KmeansCluster1)))
tail(sort(colMeans(KmeansCluster2)))
tail(sort(colMeans(KmeansCluster3)))
tail(sort(colMeans(KmeansCluster4)))
tail(sort(colMeans(KmeansCluster5)))
tail(sort(colMeans(KmeansCluster6)))
tail(sort(colMeans(KmeansCluster7)))

#### Problem 2.3 - K-Means Clustering
## Which Hierarchical Cluster best corresponds to K-Means Cluster 2?
tail(sort(colMeans(KmeansCluster2)))

#### Problem 2.4 - K-Means Clustering
## Which Hierarchical Cluster best corresponds to K-Means Cluster 3?
tail(sort(colMeans(KmeansCluster3)))

#### Problem 2.5 - K-Means Clustering
## Which Hierarchical Cluster best corresponds to K-Means Cluster 7?
tail(sort(colMeans(KmeansCluster7)))


#### Problem 2.6 - K-Means Clustering
## Which Hierarchical Cluster best corresponds to K-Means Cluster 6?
tail(sort(colMeans(KmeansCluster6)))



#########################  MARKET SEGMENTATION FOR AIRLINES ####################
## The file AirlinesCluster.csv contains information on 3,999 members of the frequent flyer program. 
## In this problem, we'll see how clustering can be used to find similar groups of customers who belong to an airline's frequent flyer program. 
## The airline is trying to learn more about its customers so that it can target different customer segments with different types of mileage offers. 

#### Problem 1.1 - Normalizing the Data
airline <- read.csv("~/Downloads/mit/AirlinesCluster.csv")
str(airline)
summary(airline)


#### Problem 1.2 - Normalizing the Data
## If we don't normalize the data, the variables that are on a larger scale will contribute much more to the distance calculation, and thus will dominate the clustering.


#### Problem 1.3 - Normalizing the Data
## Let's go ahead and normalize our data. You can normalize the variables in a data frame by using the preProcess function in the "caret" package. 
library(caret)
preproc = preProcess(airline)
airlinesNorm = predict(preproc, airline)
summary(airlinesNorm)


#### Problem 2.1 - Hierarchical Clustering
## Compute the distances between data points (using euclidean distance) and then run the Hierarchical clustering algorithm (using method="ward.D") on the normalized data.
distance = dist(airlinesNorm, method="euclidean")
cluster_airline = hclust(distance, method="ward.D")

plot(cluster_airline)
rect.hclust(clusterMovies, k=7, border="red")


#### Problem 2.2 - Hierarchical Clustering
## Suppose that after looking at the dendrogram and discussing with the marketing department, the airline decides to proceed with 5 clusters. 
## Divide the data points into 5 clusters by using the cutree function. How many data points are in Cluster 1?
clusterGroups = cutree(cluster_airline, k=5)
table(clusterGroups)


#### Problem 2.3 - Hierarchical Clustering
tapply(airline$Balance, clusterGroups, mean)
tapply(airline$QualMiles, clusterGroups, mean)
tapply(airline$BonusMiles, clusterGroups, mean)
tapply(airline$BonusTrans, clusterGroups, mean)
tapply(airline$FlightMiles, clusterGroups, mean)
tapply(airline$FlightTrans, clusterGroups, mean)
tapply(airline$DaysSinceEnroll, clusterGroups, mean)

#### Problem 2.4 - Hierarchical Clustering
## Compared to the other clusters, Cluster 2 has the largest average values in which variables (if any)? Select all that apply.

#### Problem 2.5 - Hierarchical Clustering
## Compared to the other clusters, Cluster 3 has the largest average values in which variables (if any)? Select all that apply.

#### Problem 2.6 - Hierarchical Clustering
## Compared to the other clusters, Cluster 4 has the largest average values in which variables (if any)? Select all that apply.

#### Problem 2.7 - Hierarchical Clustering
## Compared to the other clusters, Cluster 5 has the largest average values in which variables (if any)? Select all that apply.


#### Problem 3.1 - K-Means Clustering
## Now run the k-means clustering algorithm on the normalized data, again creating 5 clusters. Set the seed to 88 right before running the clustering algorithm, and set the argument iter.max to 1000.
set.seed(88)
KMA = kmeans(airlinesNorm, centers = 5, iter.max = 1000)
str(KMA)
table(KMA$cluster)

#### Problem 3.2 - K-Means Clustering
## Now, compare the cluster centroids to each other either by dividing the data points into groups and then using tapply, or by looking at the output of kmeansClust$centers, where "kmeansClust" is the name of the output of the kmeans function.
## Do you expect Cluster 1 of the K-Means clustering output to necessarily be similar to Cluster 1 of the Hierarchical clustering output?
tapply(airline$Balance, KMA$cluster, mean)
tapply(airline$QualMiles, KMA$cluster, mean)
tapply(airline$BonusMiles, KMA$cluster, mean)
tapply(airline$BonusTrans, KMA$cluster, mean)
tapply(airline$FlightMiles, KMA$cluster, mean)
tapply(airline$FlightTrans, KMA$cluster, mean)
tapply(airline$DaysSinceEnroll, KMA$cluster, mean)         



########################   PREDICTING STOCK RETURNS WITH CLUSTER-THEN-PREDICT   ###############
## In this assignment, we'll use cluster-then-predict to predict future stock prices using historical stock data.
## For this problem, we'll use StocksCluster.csv, which contains monthly stock returns from the NASDAQ stock exchange. 
stock <- read.csv("~/Downloads/mit/StocksCluster.csv")

#### Problem 1.1 - Exploring the Dataset
## How many observations are in the dataset? 
str(stock)

#### Problem 1.2 - Exploring the Dataset
## What proportion of the observations have positive returns in December?
table(stock$PositiveDec)
6324/(6324+5256)

#### Problem 1.3 - Exploring the Dataset
## What is the maximum correlation between any two return variables in the dataset?
stock_cor = subset(stock, select = -c(PositiveDec) )
cor(stock_cor)


#### Problem 1.4 - Exploring the Dataset
## Which month (from January through November) has the largest mean return across all observations in the dataset?

## Which month (from January through November) has the smallest mean return across all observations in the dataset?
summary(stock_cor)

#### Problem 2.1 - Initial Logistic Regression Model
## Run the following commands to split the data into a training set and testing set, putting 70% of the data in the training set and 30% of the data in the testing set:
library(caTools)
set.seed(144)
spl = sample.split(stock$PositiveDec, SplitRatio = 0.7)
stocksTrain = subset(stock, spl == TRUE)
stocksTest = subset(stock, spl == FALSE)

## use the stocksTrain data frame to train a logistic regression model (name it StocksModel) to predict PositiveDec using all the other variables as independent variables
stock_LM = glm(PositiveDec ~ ., data=stocksTrain, family = binomial)
summary(stock_LM)

## What is the overall accuracy on the training set, using a threshold of 0.5?
stock_LM_pred = predict(stock_LM, type = "response")
table(stocksTrain$PositiveDec, stock_LM_pred>0.5)
(990+3640)/nrow(stocksTrain) #0.5711818


#### Problem 2.2 - Initial Logistic Regression Model
## Now obtain test set predictions from StocksModel. What is the overall accuracy of the model on the test, again using a threshold of 0.5?
stock_LM_pred_test = predict(stock_LM, newdata=stocksTest, type = "response")
table(stocksTest$PositiveDec, stock_LM_pred_test>0.5)
(417+1553)/nrow(stocksTest)


#### Problem 2.3 - Initial Logistic Regression Model
## What is the accuracy on the test set of a baseline model that always predicts the most common outcome (PositiveDec = 1)?
table(stocksTest$PositiveDec)
1897/(1897+1577)


#### Problem 3.1 - Clustering Stocks
## Now, let's cluster the stocks. The first step in this process is to remove the dependent variable using the following commands:
limitedTrain = stocksTrain
limitedTrain$PositiveDec = NULL
limitedTest = stocksTest
limitedTest$PositiveDec = NULL

## In cluster-then-predict, our final goal is to predict the dependent variable, which is unknown to us at the time of prediction. Therefore, if we need to know the outcome value to perform the clustering, the methodology is no longer useful for prediction of an unknown outcome value.

#### Problem 3.2 - Clustering Stocks
## normalizes variables by subtracting by the mean and dividing by the standard deviation
library(caret)
preproc = preProcess(limitedTrain)
normTrain = predict(preproc, limitedTrain)
normTest = predict(preproc, limitedTest)

## What is the mean of the ReturnJan variable in normTrain?
summary(normTrain)

## What is the mean of the ReturnJan variable in normTest?
summary(normTest)


#### Problem 3.3 - Clustering Stocks
## Why is the mean ReturnJan variable much closer to 0 in normTrain than in normTest?


#### Problem 3.4 - Clustering Stocks
## Set the random seed to 144 (it is important to do this again, even though we did it earlier). 
## Run k-means clustering with 3 clusters on normTrain, storing the result in an object called km.
set.seed(144)
KM_normTrain = kmeans(normTrain, centers = 3)
str(KM_normTrain)
table(KM_normTrain$cluster)


#### Problem 3.5 - Clustering Stocks
## Recall from the recitation that we can use the flexclust package to obtain training set and testing set cluster assignments for our observations (note that the call to as.kcca may take a while to complete):
library(flexclust)
km.kcca = as.kcca(KM_normTrain, normTrain)
clusterTrain = predict(km.kcca)
clusterTest = predict(km.kcca, newdata=normTest)
table(clusterTest)

## How many test-set observations were assigned to Cluster 2?
table(clusterTest)


#### Problem 4.1 - Cluster-Specific Predictions
## Using the subset function, build data frames stocksTrain1, stocksTrain2, and stocksTrain3, containing the elements in the stocksTrain data frame assigned to clusters 1, 2, and 3, respectively (be careful to take subsets of stocksTrain, not of normTrain). Similarly build stocksTest1, stocksTest2, and stocksTest3 from the stocksTest data frame.
## Which training set data frame has the highest average value of the dependent variable?
stocksTrain1 = subset(stocksTrain, KM_normTrain$cluster == 1)
stocksTrain2 = subset(stocksTrain, KM_normTrain$cluster == 2)
stocksTrain3 = subset(stocksTrain, KM_normTrain$cluster == 3)

summary(stocksTrain1)
summary(stocksTrain2)
summary(stocksTrain3)

stocksTest1 = subset(stocksTest, clusterTest == 1)
stocksTest2 = subset(stocksTest, clusterTest == 2)
stocksTest3 = subset(stocksTest, clusterTest == 3)

#### Problem 4.2 - Cluster-Specific Predictions
## Build logistic regression models StocksModel1, StocksModel2, and StocksModel3, which predict PositiveDec using all the other variables as independent variables.
## Which variables have a positive sign for the coefficient in at least one of StocksModel1, StocksModel2, and StocksModel3 
## and a negative sign for the coefficient in at least one of StocksModel1, StocksModel2, and StocksModel3? Select all that apply
StocksModel1 = glm(PositiveDec ~ ., data=stocksTrain1, family = binomial)
StocksModel2 = glm(PositiveDec ~ ., data=stocksTrain2, family = binomial)
StocksModel3 = glm(PositiveDec ~ ., data=stocksTrain3, family = binomial)
summary(StocksModel1)
summary(StocksModel2)
summary(StocksModel3)


#### Problem 4.3 - Cluster-Specific Predictions
## Using StocksModel1, make test-set predictions called PredictTest1 on the data frame stocksTest1. Using StocksModel2, make test-set predictions called PredictTest2 on the data frame stocksTest2. Using StocksModel3, make test-set predictions called PredictTest3 on the data frame stocksTest3.
PredictTest1 = predict(StocksModel1, newdata=stocksTest1, type = "response")
PredictTest2 = predict(StocksModel2, newdata=stocksTest2, type = "response")
PredictTest3 = predict(StocksModel3, newdata=stocksTest3, type = "response")

## What is the overall accuracy of StocksModel1 on the test set stocksTest1, using a threshold of 0.5?
table(stocksTest1$PositiveDec, PredictTest1>0.5)
(30+774)/nrow(stocksTest1)

## What is the overall accuracy of StocksModel2 on the test set stocksTest2, using a threshold of 0.5?
table(stocksTest2$PositiveDec, PredictTest2>0.5)
(388+757)/nrow(stocksTest2)

## What is the overall accuracy of StocksModel3 on the test set stocksTest3, using a threshold of 0.5?
table(stocksTest3$PositiveDec, PredictTest3>0.5)
(49+13)/nrow(stocksTest3)


#### Problem 4.4 - Cluster-Specific Predictions
## To compute the overall test-set accuracy of the cluster-then-predict approach, 
## we can combine all the test-set predictions into a single vector and all the true outcomes into a single vector:
AllPredictions = c(PredictTest1, PredictTest2, PredictTest3)
AllOutcomes = c(stocksTest1$PositiveDec, stocksTest2$PositiveDec, stocksTest3$PositiveDec)

## What is the overall test-set accuracy of the cluster-then-predict approach, again using a threshold of 0.5?
table(AllOutcomes, AllPredictions>0.5)
(467+1544)/((467+1544+1110+353))


#### We see a modest improvement over the original logistic regression model. Since predicting stock returns is a notoriously hard problem, this is a good increase in accuracy. 
#### By investing in stocks for which we are more confident that they will have positive returns (by selecting the ones with higher predicted probabilities), this cluster-then-predict model can give us an edge over the original logistic regression model.

