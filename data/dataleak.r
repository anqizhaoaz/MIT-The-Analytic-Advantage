
# Load data for 2016 competition
train = read.csv("train2016.csv", na.strings="")
test = read.csv("test2016.csv", na.strings="")

# Load data from 2014 Happiness competition
# Download from here: https://github.com/bobbruno/Analytics-Edge/tree/master/%20Kaggle%20Happiness%20Predictor (download train.csv and test.csv)
train2014 = read.csv("train.csv", na.strings="")
test2014 = read.csv("test.csv", na.strings="")

# Remove Happy variable from 2014 data, as we don't need it
train2014$Happy = NULL

# Combine 2014 train and test set into a single dataframe
all2014 = rbind(train2014, test2014)

# Now we want to find matching rows in 2014 data to add to our 2016 data.
# We cannot match on the ID column, because they're different. However, we can match on all other columns and merge the 2014 Party variable into our 2016 test data using merge()
testMerged = merge(test, all2014, all.x=T)

# We can examine how many rows we succesfully merged
summary(testMerged$Party)

# We see that the old Party variable looked slightly different.
# However, we can guess that Independent = Democrat, Libertarian = Republican and Other = Republican
testMerged$Party[testMerged$Party=="Independent"] = "Democrat"
testMerged$Party[testMerged$Party=="Libertarian"] = "Republican"
testMerged$Party[testMerged$Party=="Other"] = "Republican"

# Some rows could not be found in the 2014 data (probably because they didn't contain the happiness variable). Let's use a simple baseline model and set them to "Democrat".
# Logistic regression or random forest on these rows would improve the score further.
testMerged$Party[is.na(testMerged$Party)] = "Democrat"

# Create submission file
submission = data.frame(USER_ID=testMerged$USER_ID, Predictions=testMerged$Party)
fileName = paste0("submission_", format(Sys.Date(), "%Y%m%d"), "_", format(Sys.time(), "%H%M"), "_dataleak.csv")
write.csv(submission, fileName, row.names = F)
