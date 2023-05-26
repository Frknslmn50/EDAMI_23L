# EDAMI23l Task 4 : Classification
# Author: Furkan Salman

# Problem: Given a dataset wines, classify the wines 
# into groups of wine qualities.

# Solution: Use decision tree and random forest algorithms to classify the wines.

# Load necessary libraries.
#install.packages("gmodels")
#install.packages("Hmisc")
#install.packages("caret")
#install.packages("rpart")
#install.packages("rpart.plot")
#install.packages("e1071")
#install.packages("C50")
#install.packages("randomForest")
#install.packages("datasets")


library(gmodels) #results analysis
library(Hmisc) #results analysis
library(caret)
library(rpart) # rpart() - decision tree classifier
library(rpart.plot) 
library(e1071)
library(C50) # C5 classifer
library(randomForest)
library(datasets)

# Load the dataset.
download.file('http://archive.ics.uci.edu/ml/machine-learning-databases/wine-quality/winequality-red.csv', 'wine_red.csv');
download.file('http://archive.ics.uci.edu/ml/machine-learning-databases/wine-quality/winequality-white.csv', 'wine_white.csv');
wineRed_ds = read.table("wine_red.csv", header = TRUE, sep=";", na.strings= "*")
wineWhite_ds = read.table("wine_white.csv", header = TRUE, sep=";", na.strings= "*")

########### Data Inspecting and Preprocessing ###############

# Combine the datasets.
dim(wineRed_ds)
# Dimensions: 1599 x 12
dim(wineWhite_ds)
# Dimensions: 4898 x 12
wine_ds = rbind(wineRed_ds, wineWhite_ds)
dim(wine_ds)
# Dimensions: 6497 x 12

summary(wine_ds)
str(wine_ds)
View(wine_ds)

# Check for missing values.
sum(is.na(wine_ds))
# There are no missing values.

# Plot the histogram for target value.
hist(wine_ds$quality, main = "Histogram of Quality", xlab = "Quality")
# Statistics of target value.
summary(wine_ds$quality)


# Convert quality to categorical variable as follows:
# 3,4,5-> low quality
# 6,7 -> medium quality
# 8,9 -> high quality
wine_ds$quality <- as.factor(ifelse(wine_ds$quality < 6, "low", ifelse(wine_ds$quality > 7, "high", "medium")))
summary(wine_ds$quality)
#  high    low medium
#   198   2384   3915
# As seen, the dataset is imbalanced.

# Split the dataset into training and test sets.
set.seed(123)
trainIndex <- createDataPartition(wine_ds$quality, p = 0.7, list = FALSE, times = 1)

wine_train <- wine_ds[trainIndex,]
wine_test  <- wine_ds[-trainIndex,]

# Check the distribution of target variable in training and test sets.
prop.table(table(wine_train$quality))
#      high        low     medium
# 0.03055617 0.36689382 0.60255001

prop.table(table(wine_test$quality))
#      high        low     medium
# 0.03028747 0.36704312 0.60266940

# As seen, the distribution of target variable is similar in training and test sets.

################################################################
# 1.  C5.0 classifier                                          #
################################################################

# Train the C5.0 classifier.
wine_C50 <- C5.0(wine_ds[,-12], wine_ds$quality)
summary(wine_C50)

# Quality of classification for training set.
wine_C50_pred_train <- predict(wine_C50, wine_train)
CrossTable(wine_train$quality, wine_C50_pred_train,
                    prop.chisq = FALSE,prop.c = FALSE, 
                    prop.r = FALSE,
                    dnn = c("predicted class", "actual class"))

confusionMatrix(wine_C50_pred_train, wine_train$quality, mode="everything")

# Quality of classification for test set.
wine_C50_pred_test <- predict(wine_C50, wine_test)
CrossTable(wine_test$quality, wine_C50_pred_test,
                    prop.chisq = FALSE,prop.c = FALSE, 
                    prop.r = FALSE,
                    dnn = c("predicted class", "actual class"))

confusionMatrix(wine_C50_pred_test, wine_test$quality, mode="everything")

# Scores for training set and test set are similar which means model is not overfitting.
# Overall accuracy is 0.83 but it is not a good measure for imbalanced datasets.
# Scores for medium and low quality are relatively good, we are able to classify them correctly most of the time.
# However, the score for high quality is very low. Recall is around 0.36 for high quality which means model
# usually misclassifies medium quality wines as high quality. Precision is above 0.90 for high quality which means
# if a wine is high quality, model is able to classify it correctly most of the time.
# We can say that model is not bad at classifying low and medium quality wines but it is not as good as
# we want at classifying high quality wines.
# Low quality wines are classified correctly most of the time but there are some false positives coming
# from medium quality wines. Medium quality wines are also classified correctly most of the time but there are
# some false positives coming from low quality wines.
# This is understandable because there are more medium quality wines in the dataset than low quality wines
# and the classes are construcrted from not so precise boundaries.

############ C5.0 with Ensembles ##############

# Train the C5.0 classifier with ensembles.
wine_C50_ens <- C5.0(wine_ds[,-12], wine_ds$quality, trials = 10)
summary(wine_C50_ens)

# Quality of classification for training set.
wine_C50_ens_pred_train <- predict(wine_C50_ens, wine_train)
CrossTable(wine_train$quality, wine_C50_ens_pred_train,
                    prop.chisq = FALSE,prop.c = FALSE, 
                    prop.r = FALSE,
                    dnn = c("predicted class", "actual class"))

confusionMatrix(wine_C50_ens_pred_train, wine_train$quality, mode="everything")

# Quality of classification for test set.
wine_C50_ens_pred_test <- predict(wine_C50_ens, wine_test)
CrossTable(wine_test$quality, wine_C50_ens_pred_test,
                    prop.chisq = FALSE,prop.c = FALSE, 
                    prop.r = FALSE,
                    dnn = c("predicted class", "actual class"))
                
confusionMatrix(wine_C50_ens_pred_test, wine_test$quality, mode="everything")

# Overall accuracy is 0.90 which is better than classical tree version
# but it is not a good measure for imbalanced datasets.
# Scores for medium and low quality are around 0.80 -  0.90 which is also better than
# classical tree version and satisfactory.
# When we look at the scores for high quality, we see that recall is around 0.48 for training set
# and 0.40 for test set. Precision is around 0.96 for both training and test sets.
# This means model is overfitting for high quality wines and it is not able to generalize well for high
# quality wines.

################################################################
# 2.  Recursive Partitioning Trees (rpart)                     #
################################################################

# WILL BE UPDATED

