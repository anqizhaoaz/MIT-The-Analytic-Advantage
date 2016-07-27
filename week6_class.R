
##################################  MovieLens website ##################################

## load data
movies = read.table("~/Downloads/mit/movie.txt", header=FALSE, sep="|", quote="\"")
colnames(movies) = c("ID", "Title", "ReleaseDate", "VideoReleaseDate", "IMDB", "Unknown", "Action", "Adventure", "Animation", "Childrens", "Comedy", "Crime", "Documentary", "Drama", "Fantasy", "FilmNoir", "Horror", "Musical", "Mystery", "Romance", "SciFi", "Thriller", "War", "Western")
str(movies) #1682 obs and 24 variables

# Remove unnecessary variables
movies$ID = NULL
movies$ReleaseDate = NULL
movies$VideoReleaseDate = NULL
movies$IMDB = NULL

# Remove duplicates: unique returns a vector, data frame or array like x but with duplicate elements/rows removed.
movies = unique(movies)
str(movies) #1664 obs and 20 variables

# How many movies are classified as comedies?
table(movies$Comedy)

# How many movies are classified as westerns?
table(movies$Western)

# How many movies are classified as romance AND drama?
table(movies$Romance, movies$Drama)

distance = dist(movies[2:20], method="euclidean")
clusterMovies = hclust(distance, method = "ward")
plot(clusterMovies)

# Run the cutree function again to create the cluster groups,
clusterGroups = cutree(clusterMovies, k=10)
tapply(movies$Action, clusterGroups, mean)
tapply(movies$Romance, clusterGroups, mean)

subset(movies, Title=="Men in Black (1997)")
clusterGroups[257]

# Create a new data set with just the movies from cluster 2
cluster2 = subset(movies, clusterGroups==2)

# Look at the first 10 titles in this cluster:
cluster2$Title[1:10]

# k=2 Run the cutree function again to create the cluster groups,
clusterGroups = cutree(clusterMovies, k=2)
tapply(movies$Mystery, clusterGroups, mean)
cluster2 = subset(movies, clusterGroups==2)
summary(cluster2)



###################### Recitation: cluster images #####################
flower = read.csv("~/Downloads/mit/flower.csv", header=FALSE)
str(flower)

# the resolution of the image is 50 x 50
flowerMatrix = as.matrix(flower)
str(flowerMatrix)

flowerFactor = as.factor(flowerMatrix)
str(flowerFactor)

# calculate distance matrix, which computes the pairwise distances among the elements of the intensity vector
distance = dist(flowerVector, method = "euclidean")

# compute hierarchical clusters
clusterIntensity = hclust(distance, method="ward")
plot(clusterIntensity)

rect.hclust(clusterIntensity, k=3, border="red")
flowerClusters = cutree(clusterIntensity, k=3)
flowerClusters
tapply(flowerVector, flowerClusters, mean)

dim(flowerClusters) = c(50, 50)
image(flowerClusters, axes=FALSE)

# original image
image(flowerMatrix, axes=FALSE, col=grey(seq(0,1,length=256)))



####################  healthy data ######################
healthy = read.csv("~/Downloads/mit/healthy.csv", header=FALSE)
str(healthy)
healthyMatrix = as.matrix(healthy)
image(healthyMatrix, axes=FALSE, col=grey(seq(0,1,length=256)))

healthyVector = as.vector(healthyMatrix)
# distance = dist(healthyVector, method="euclidean")

# Run k-means
k=5
set.seed(1)
KMC = kmeans(healthyVector, centers = k, iter.max = 1000)
str(KMC)

# Extract clusters
healthyClusters = KMC$cluster
KMC$centers[2]

# Plot the image with the clusters
dim(healthyClusters) = c(nrow(healthyMatrix), ncol(healthyMatrix))

# segment images
image(healthyClusters, axes = FALSE, col=rainbow(k))

############################## tumor ##########################
# Apply to a test image

tumor = read.csv("~/Downloads/mit/tumor.csv", header=FALSE)
tumorMatrix = as.matrix(tumor)
tumorVector = as.vector(tumorMatrix)

# Apply clusters from before to new image, using the flexclust package
# install.packages("flexclust")
library(flexclust)

KMC.kcca = as.kcca(KMC, healthyVector)
tumorClusters = predict(KMC.kcca, newdata = tumorVector)

# Visualize the clusters
dim(tumorClusters) = c(nrow(tumorMatrix), ncol(tumorMatrix))
image(tumorClusters, axes = FALSE, col=rainbow(k))



