# EDAMI23l Task 3 : Clustering
# Author: Furkan Salman

# Problem: Given a dataset of red wine, cluster the wines into groups of wine quality.

# Solution: Use k-means and dbscan algorithms to cluster the wines.

# Load necessary dataset and libraries.
#install.packages("dbscan")
#install.packages("fpc")
#install.packages("cluster")
#install.packages("factoextra")
 
library(dbscan)
library(fpc)
library(cluster)
library(factoextra)

download.file('http://archive.ics.uci.edu/ml/machine-learning-databases/wine-quality/winequality-red.csv', 'wine_red.csv');
wineRed_ds = read.table("wine_red.csv", header = TRUE, sep=";", na.strings= "*")
wineRed_dsC <- wineRed_ds[, -12]

########### Data Inspecting and Preprocessing ###############

# Start by inspecting the dataset.
dim(wineRed_ds)
# Dimensions: 1599 x 12
summary(wineRed_ds)
str(wineRed_ds)
View(wineRed_ds)
# All of the attributes are numeric and target value is discrete.
# Since target value changes between 3 and 8, we can consider the number of clusters as 6.

# Check for missing values.
sum(is.na(wineRed_ds))
# There are no missing values.

# Plot the histogram for target value.
hist(wineRed_ds$quality, main = "Histogram of Quality", xlab = "Quality")
# Target value is dense around 5 and 6.

# Scale the dataset, to avoid biasing the clustering algorithm.
wineRedScaled = scale(wineRed_dsC,center = FALSE)
View(wineRedScaled)

############### K-Means #####################
# params = data,num of clusters, max iter, num of initial trial
wine.kmeans = kmeans(wineRedScaled, 6, iter.max = 20, nstart = 20)


#compare clusters with original class labels using confusion matrix
table(wineRed_ds$quality, wine.kmeans$cluster)

#   Confusion Matix
#      1   2   3   4   5   6
#  3   1   0   2   6   1   0
#  4   4   1   6  34   7   1
#  5 164  16 109 240 134  18
#  6  77   6 218 202 124  11
#  7  17   1 129  29  18   5
#  8   1   0  13   0   4   0

# Quality of the clustering

# Silhouette is a measurement
# that considers how closely related objects are within the cluster and how clusters are separated
# from each other. The silhouette value usually ranges from 0 to 1; a value closer to 1 suggests
# the data is better clustered.

km<-eclust(wineRedScaled, "kmeans", k=6, graph=TRUE)
str(km)
fviz_silhouette(km, palette="jco")

# Silhouette values:

#  cluster size ave.sil.width
#1       1   47          0.10
#2       2   24          0.53
#3       3  399          0.19
#4       4  403          0.14
#5       5  318          0.17
#6       6  408          0.21

# Calculate the average silhouette width for the clustering.
silhouette_score <- cluster.stats(dist(wineRedScaled), km$cluster)$avg.silwidth
print(silhouette_score)
# Silhouette score = 0.1820652


# Rand index
# The corrected Rand index provides a measure for assessing the similarity between
# two partitions, adjusted for chance. Its range is 0 (no agreement) to 1 (perfect agreement).

quality = as.numeric(wineRed_ds$quality)
clust_stats<-cluster.stats(d=dist(wineRedScaled), quality, km$cluster)
str(clust_stats)
clust_stats$corrected.rand
# rand = 0.02560952


# finding the optimal number of groups with the "elbow" method

# sum  of squares vector
wss <- vector(mode = "integer", length = 15)

#  1 to 15 clusters
for (i in 1:15) {
  kmeans.group <- kmeans(wineRedScaled, centers = i, nstart=20)
  # total within-cluster sum of squares
  wss[i] <- kmeans.group$tot.withinss
}

# total within-cluster sum of squares per number of groups
plot(1:15, wss, type = "b", 
     xlab = "number of groups", 
     ylab = "total within-cluster sum of squares")

# The optimal number of groups is 5, since the elbow is at 5.

# Try k-means with 5 clusters.
km<-eclust(wineRedScaled, "kmeans", k = 5, graph = TRUE)
str(km)
fviz_silhouette(km, palette="jco")

silhouette_score <- cluster.stats(dist(wineRedScaled), km$cluster)$avg.silwidth
print(silhouette_score)
# Silhouette score = 0.2917967

quality = as.numeric(wineRed_ds$quality)
clust_stats<-cluster.stats(d=dist(wineRedScaled), quality, km$cluster)
str(clust_stats)
clust_stats$corrected.rand
# Rand index = 0.03519709

# Calculation of accuracy
accuracyCalc <- function(confTbl, startCol)
{
  corr = 0;
  for(i in startCol:ncol(confTbl))
  {
    corr = corr + max(confTbl[,i])  
  }
  accuracy = corr/sum(confTbl)
  accuracy  
}
res = table(wineRed_ds$quality,km$cluster)
res
accuracyCalc(res, 1)
# Accuracy = 0.4953096

# In Conclusion originally we have 6 clusters, but clustering with 5 clusters gives us better results
# in terms of silhouette score, rand index and accuracy.

############### DBSCAN #####################

# Find the optimal value for eps.
dbscan::kNNdistplot(wineRedScaled, k = 5)
abline(h = 0.7, lty = "dashed")
# The optimal value for eps is 0.7.

# dbscan algorithm execution
wine.dbscan = dbscan(wineRedScaled, eps = 0.7, MinPts = 5)
table(wineRed_ds$quality, wine.dbscan$cluster)

#  Confusion Matix
#      0   1   2   3
#  3   2   8   0   0
#  4   5  48   0   0
#  5  62 608   5   6
#  6  29 605   0   4
#  7  10 189   0   0
#  8   0  18   0   0

# Quality of the clustering

# Calculate the silhouette score for the DBSCAN clustering.
silhouette_score <- cluster.stats(dist(wineRedScaled), wine.dbscan$cluster)$avg.silwidth
# Print the silhouette score
print(silhouette_score)
# Silhouette score = 0.3001556

# Calculate the Rand index for the DBSCAN clustering.
clust_stats <- cluster.stats(d = dist(wineRedScaled), quality, wine.dbscan$cluster)
print(clust_stats$corrected.rand)
# Rand Index = -0.00500849

# Calculation of accuracy
res = table(wineRed_ds$quality,wine.dbscan$cluster)
accuracyCalc(res, 1)
# Accuracy = 0.4258912

# plot the clusters
plot(wine.dbscan, wineRedScaled)

# From the confusion matrix, we can see that cluster 1 is dominating the whole dataset.
# In order to see if this is a result of eps, we can try different values for eps.

# dbscan with eps = 0.5
wine.dbscan = dbscan(wineRedScaled, eps = 0.5, MinPts = 5)
table(wineRed_ds$quality, wine.dbscan$cluster)

#         Confusion Matix
#      0   1   2   3   4   5   6   7
#  3   5   5   0   0   0   0   0   0
#  4  11  41   0   0   0   0   1   0
#  5 117 550   2   5   4   3   0   0
#  6  82 548   1   0   0   0   1   6
#  7  26 161   6   0   0   1   3   2
#  8   5  12   0   0   0   0   1   0

silhouette_score <- cluster.stats(dist(wineRedScaled), wine.dbscan$cluster)$avg.silwidth
print(silhouette_score)
# Silhouette score = -0.03017342
clust_stats <- cluster.stats(d = dist(wineRedScaled), quality, wine.dbscan$cluster)
print(clust_stats$corrected.rand)
# Rand Index = 0.01426606
res = table(wineRed_ds$quality,wine.dbscan$cluster)
print(accuracyCalc(res, 1))
# Accuracy = 0.4340213

plot(wine.dbscan, wineRedScaled)

############# CONCLUSION #####################

# From the confusion matrix,silhouette score and rand index we can see decreasing epsilon is not solving the problem.
# In conclusion DBSCAN is not a good algorithm for this dataset, because it is density based.
# KMeans is a better choice for this dataset but it is not doing a good job either.



