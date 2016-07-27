

############  Week 7 Assignment  ##########

###############################   ELECTION FORECASTING REVISITED  #######################
#### In this homework problem, we'll revisit our logistic regression model from Unit 3, and learn how to plot the output on a map of the United States. Unlike what we did in the Crime lecture, 
#### this time we'll be plotting predictions rather than data!

## load the ggplot2, maps, and ggmap
library(ggplot2)
library(maps)
library(ggmap)

#### Problem 1.1 - Drawing a Map of the US
## load the US map and save it to the variable statesMap
statesMap = map_data("state")
str(statesMap)

## Sometimes a state may have multiple groups, for example, if it includes islands. How many different groups are there?
table(statesMap$group)


#### Problem 1.2 - Drawing a Map of the US
## draw a map of the United States
ggplot(statesMap, aes(x = long, y = lat, group = group)) + 
  geom_polygon(fill = "white", color = "black")


#### Problem 2.1 - Coloring the States by Predictions
## read election data
PollingImputed <- read.csv("~/Downloads/mit/PollingImputed.csv")
str(PollingImputed)

## split the data using the subset function into a training set called "Train" that has observations from 2004 and 2008, and a testing set called "Test" that has observations from 2012.
polling_train <- subset(PollingImputed, PollingImputed$Year<=2008)
polling_test <- subset(PollingImputed, PollingImputed$Year>=2012)
table(polling_train$Year)
table(polling_test$Year)

## create a logistic regression model
polling_train_log = glm(Republican ~ SurveyUSA + DiffCount, data=polling_train, family="binomial")
summary(polling_train_log)

## make predictions on the test set
polling_train_log_pred = predict(polling_train_log, newdata=polling_test, type="response")
table(polling_test$Republican, polling_train_log_pred>0.5)

TestPredictionBinary = as.numeric(polling_train_log_pred > 0.5)
table(TestPredictionBinary)

## put the predictions and state labels in a data.frame so that we can use ggplot
predictionDataFrame = data.frame(polling_train_log_pred, TestPredictionBinary, polling_test$State)

## For how many states is our binary prediction 1 (for 2012), corresponding to Republican?
table(predictionDataFrame$TestPredictionBinary)

## What is the average predicted probability of our model (on the Test set, for 2012)?
mean(polling_train_log_pred)


#### Problem 2.2 - Coloring the States by Predictions
## convert the Test.State variable to lowercase
predictionDataFrame$region = tolower(predictionDataFrame$polling_test.State)

## merge "predictionDataFrame" with the map data "statesMap"
predictionMap = merge(statesMap, predictionDataFrame, by = "region")

## make sure the observations are in order so that the map is drawn properly
predictionMap = predictionMap[order(predictionMap$order),]

## How many observations are there in predictionMap?
str(predictionMap)
str(statesMap)


#### Problem 2.3 - Coloring the States by Predictions
## When we merged the data in the previous problem, it caused the number of observations to change. Why? 
##  Because we only make predictions for 45 states, we no longer have observations for some of the states. These observations were removed in the merging process. 


#### Problem 2.4 - Coloring the States by Predictions
ggplot(predictionMap, aes(x = long, y = lat, group = group, fill = TestPredictionBinary)) + 
  geom_polygon(color = "black")


#### Problem 2.5 - Coloring the States by Predictions
ggplot(predictionMap, aes(x = long, y = lat, group = group, fill = TestPredictionBinary))+ 
  geom_polygon(color = "black") + 
  scale_fill_gradient(low = "blue", high = "red", guide = "legend", breaks= c(0,1), labels = c("Democrat", "Republican"), name = "Prediction 2012")


#### Problem 3.1 - Understanding the Predictions
## In the 2012 election, the state of Florida ended up being a very close race. It was ultimately won by the Democratic party. 
## Did we predict this state correctly or incorrectly?
predictionMap[predictionMap$polling_test.State=="Florida", ]
str(predictionMap)


#### Problem 3.2 - Understanding the Predictions
## What was our predicted probability for the state of Florida?
predFlorida = subset(predictionMap, predictionMap$polling_test.State=="Florida")
mean(predFlorida$polling_train_log_pred)


#### PROBLEM 4 - PARAMETER SETTINGS
ggplot(predictionMap, aes(x = long, y = lat, group = group, fill = TestPredictionBinary))+ 
  geom_polygon(color = "black", linetype=3, size=3, alpha=0.6) + 
  scale_fill_gradient(low = "blue", high = "red", guide = "legend", breaks= c(0,1), labels = c("Democrat", "Republican"), name = "Prediction 2012")



###################################  VISUALIZING NETWORK DATA  ############################
## For this assignment, we will visualize social networking data using anonymized data from Facebook; this data was originally curated in a recent paper about computing social circles in social networks. 
## In our visualizations, the vertices in our network will represent Facebook users and the edges will represent these users being Facebook friends with each other.
edges <- read.csv("~/Downloads/mit/edges.csv")
users <- read.csv("~/Downloads/mit/users.csv")

edges[order(edges$V1),]
table(edges$V1)
table(edges$V2)

#### Problem 1.1 - Summarizing the Data
## How many Facebook users are there in our dataset?
str(users)
str(edges)

## What is the average number of friends per user?
(length(edges$V1)+length(edges$V2))/length(users$id)

## From str(edges) or nrow(edges), we see that there are 146 pairs of users in our dataset who are Facebook friends. However, each pair (A, B) must be counted twice, 
## because B is a friend of A and A is a friend of B. To think of this in simpler terms, consider a network with just new people, A and B, and a single edge (A, B). Even though there are two vertices and one edge, each user has on average one friend.


#### Problem 1.2 - Summarizing the Data
## Out of all the students who listed a school, what was the most common locale?
table(users$locale, users$gender)


#### Problem 1.3 - Summarizing the Data
## Is it possible that either school A or B is an all-girls or all-boys school?
table(users$locale, users$)


#### Problem 2.1 - Creating a Network
## load the igraph package to visualize networks
library(igraph)
g = graph.data.frame(edges, FALSE, users)
# Our edges are undirected -- if A is a Facebook friend of B then B is a Facebook friend of A. Therefore, we set the directed parameter to FALSE.


#### Problem 2.2 - Creating a Network
plot(g, vertex.size=5, vertex.label=NA)
## How many connected components with at least 2 nodes are there in the graph?
# In addition to the large connected component, there is a 4-node component and two 2-node components.

## How many users are there with no friends in the network?
table(degree(g)==0)


#### Problem 2.3 - Creating a Network
## How many users are friends with 10 or more other Facebook users in this network?
table(degree(g)>=10)


#### Problem 2.4 - Creating a Network
## in a social network we might consider a user with a large number of friends to be an important user
## To visually draw attention to these nodes, we will change the size of the vertices so the vertices with high degrees are larger. 
## To do this, we will change the "size" attribute of the vertices of our graph to be an increasing function of their degrees:
V(g)$size = degree(g)/2+2
plot(g, vertex.label=NA)

## What is the largest size we assigned to any node in our graph?
## What is the smallest size we assigned to any node in our graph?
max(V(g)$size)
min(V(g)$size)


#### Problem 3.1 - Coloring Vertices
V(g)$color = "black"
V(g)$color[V(g)$gender == "A"] = "red"
V(g)$color[V(g)$gender == "B"] = "gray"
plot(g, vertex.label=NA)


#### Problem 3.2 - Coloring Vertices
## Now, color the vertices based on the school that each user in our network attended.
## Are the two users who attended both schools A and B Facebook friends with each other?
V(g)$color = "black"
V(g)$color[V(g)$school == "A"] = "red"
V(g)$color[V(g)$school == "AB"] = "blue"
plot(g, vertex.label=NA)


#### Problem 3.3 - Coloring Vertices
## Now, color the vertices based on the locale of the user.
## The large connected component is most associated with which locale?
## The 4-user connected component is most associated with which locale?
V(g)$color = "black"
V(g)$color[V(g)$locale == "A"] = "orange"
V(g)$color[V(g)$locale == "B"] = "purple"
plot(g, vertex.label=NA)


#### Problem 4 - Other Plotting Options
?igraph.plotting

## Which igraph plotting function would enable us to plot our graph in 3-D?
## What parameter to the plot() function would we use to change the edge width when plotting g?



########################## VISUALIZING TEXT DATA USING WORD CLOUDS #########################
## word clouds can be a visually appealing way to display the most frequent words in a body of text.
tweets <- read.csv("~/Downloads/mit/tweets.csv", stringsAsFactors=F)
str(tweets)

#### Problem 1.1 - Preparing the Data
# 1) Create a corpus using the Tweet variable
library("tm")
library("SnowballC")
corpus = Corpus(VectorSource(tweets$Tweet))
corpus = tm_map(corpus, tolower)
corpus = tm_map(corpus, PlainTextDocument)
corpus = tm_map(corpus, removePunctuation)
corpus = tm_map(corpus, removeWords, stopwords("english"))
frequencies = DocumentTermMatrix(corpus)
allTweets = as.data.frame(as.matrix(frequencies))
str(allTweets)

#### Problem 1.2 - Preparing the Data
## Although we typically stem words during the text preprocessing step, we did not do so here. What is the most compelling rationale for skipping this step when visualizing text data?


#### Problem 2.1 - Building a Word Cloud
## load the "wordcloud" package
## which we'll pass as the first argument to wordcloud()?
library(wordcloud)


#### Problem 2.2 - Building a Word Cloud
## Which function should we apply to allTweets to obtain the frequency of each word across all tweets?


#### Problem 2.3 - Building a Word Cloud
## What is the most common word across all the tweets
wordcloud(colnames(allTweets), colSums(allTweets))
wordcloud(colnames(allTweets), colSums(allTweets), scale=c(2, .25))


#### Problem 2.4 - Building a Word Cloud
## removing the most frequent word
corpus <- Corpus(VectorSource(tweets$Tweet))
corpus <- tm_map(corpus, tolower)
corpus <- tm_map(corpus, PlainTextDocument)
corpus <- tm_map(corpus, removePunctuation)
corpus <- tm_map(corpus, removeWords, c("apple", stopwords("english")))
frequencies <- DocumentTermMatrix(corpus)
allTweets <- as.data.frame(as.matrix(frequencies))
wordcloud(colnames(allTweets), colSums(allTweets), scale=c(2, 0.25))
## Create a word cloud with the updated corpus. What is the most common word in this new corpus


#### Problem 3.1 - Size and Color
## Word cloud with negative sentiment only (Avg value -1 or less)
negTweets <- subset(allTweets, tweets$Avg <= -1)
wordcloud(colnames(negTweets), colSums(negTweets))

# Which word clouds were created with parameter random.order set to FALSE?
wordcloud(colnames(allTweets),colSums(allTweets),scale=c(2,0.25),colors=brewer.pal(9, "YlOrRd")[c(-1,-2,-3,-4)],random.order=F)

## mimimum freq = 10
wordcloud(colnames(allTweets), colSums(allTweets), scale=c(2, 0.25),
          min.freq=10, random.order=FALSE)

wordcloud(colnames(allTweets), colSums(allTweets), scale=c(2, 0.25),
          rot.per=0.7, max.words=100)

## change color
wordcloud(colnames(allTweets), colSums(allTweets), scale=c(2, 0.25),
          min.freq=10, colors=brewer.pal(9,"Blues")[c(5,6,7,8,9)])

# Which word cloud was built with a non-default value for parameter rot.per?
wordcloud(colnames(allTweets),colSums(allTweets),scale=c(2,0.25),colors=brewer.pal(9, "YlOrRd"),rot.per=0.4)


library(RColorBrewer)
display.brewer.all()

# Which color palette would be most appropriate for use in a word cloud for which we want to use color
# to indicate word frequency?
display.brewer.pal(7, "Greys")

wordcloud(colnames(allTweets), colSums(allTweets), scale=c(2, 0.25),
          min.freq=10, colors=brewer.pal(9,"Blues"))
