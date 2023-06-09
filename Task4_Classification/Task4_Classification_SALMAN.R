# EDAMI23l Task 4 : Classification
# Author: Furkan Salman

# Problem: Given a dataset wines, classify the wines
# into groups of wine qualities.

# Solution:
# Use decision tree and random forest algorithms to classify the wines.
# Use confusion matrix and metrics such as precision, recall and F1 score
# to evaluate the performance of the models.
# Use cross validation to find the best parameters for the models.


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

# Train the rpart classifier.
wine_rpart <- rpart(quality ~ ., data = wine_train, method = "class")
print(wine_rpart)

summary(wine_rpart)
# plot rpart model
prp(wine_rpart,faclen = 0, cex = NULL, extra = 1,main="Wine Quality Classification")
# from the plot, we can see that this model is not able to classify high quality
# wines at all, it confuses all of them with medium.
# It is only able to classify as low quality or medium quality.

# Quality of classification for training set.
wine_rpart_pred_train <- predict(wine_rpart, wine_train, type = "class")
confusionMatrix(wine_rpart_pred_train, wine_train$quality, mode="everything")

# Quality of classification for test set.
wine_rpart_pred_test <- predict(wine_rpart, wine_test, type = "class")
confusionMatrix(wine_rpart_pred_test, wine_test$quality, mode="everything")
# Overall accuracy is 0.70 and there is no score for high quality wines.
# For medium and low the scores are worse than C5.0 classifier.

# Training with Loss matrix for rpart classifier.

#Prediction high low medium
#    high      0   0      0
#    low       0 398    196
#    medium   59 317    978
# Loss Matrix:
# 0 1 1
# 1 0 1
# 8 1 0
# Because we want to focus on mistakes of assigning
# high quality wines to medium quality.
lossM=matrix(c(0,1,1,1,0,1,8,1,0), byrow=TRUE, nrow=3)
lossM
wine_rpartLM <- rpart(quality ~ ., data = wine_train, method = "class", parms = list(loss=lossM))

# Quality of classification for training set.
wine_rpartLM_pred_train <- predict(wine_rpartLM, wine_train, type = "class")
confusionMatrix(wine_rpartLM_pred_train, wine_train$quality, mode="everything")

# Quality of classification for test set.
wine_rpartLM_pred_test <- predict(wine_rpartLM, wine_test, type = "class")
confusionMatrix(wine_rpartLM_pred_test, wine_test$quality, mode="everything")

# There is no significant difference between the scores of rpart and rpart with loss matrix.

################################################################
#  3. Random Forest                                            #
################################################################

# Hyperparameter tuning for random forest.

# Understanding the parameters:
# nodesize = minimal number of objects in a node
# mtry - the number of randomly selected attributes for searching the
# best test split in nodes
# ntree -  number of trees in a forest


# Looking for the best values of parameters by means of K-fold validation
# and grid search. 10-fold cross validation is used.
trControl <- trainControl(method = "cv", number = 10, search = "grid")

# we will be trying different values for mtry (1 to 10)
tuneGrid <- expand.grid(mtry = c(1:10))
tuneGrid

wine_Forests_mtry <- train(quality~.,  data = wine_train,
                            method = "rf",
                            metric = "Accuracy",
                            tuneGrid = tuneGrid,
                            trControl = trControl,
                            importance = TRUE,
                            nodesize = 10, 
                            ntree = 250)
print(wine_Forests_mtry)
#  mtry  Accuracy   Kappa
#   1    0.7806226  0.5385174
#   2    0.7861118  0.5526627
#   3    0.7885342  0.5589815
#   4    0.7852385  0.5535013
#   5    0.7907354  0.5653827
#   6    0.7874305  0.5591833
#   7    0.7854554  0.5541898
#   8    0.7889753  0.5616905
#   9    0.7843628  0.5527669
#  10    0.7852399  0.5553180

# mtry = 5 gives the best accuracy and kappa values.

# Let's try different values for ntree

# Create a vector to store accuracy values
accuracy_values <- vector("numeric", length = 7)

# Define the range of ntree values
ntree_values <- c(10, 50, 100, 150, 200, 250, 300)

# Perform cross-validation for each ntree value
for (i in 1:length(ntree_values)) {
  ntree <- ntree_values[i]
  
  # Perform cross-validation
  set.seed(123)  # Set a seed for reproducibility
  folds <- createFolds(wine_train$quality, k = 10, returnTrain = TRUE)
  accuracies <- vector("numeric", length = 10)
  
  for (j in 1:10) {
    train_index <- folds[[j]]
    train_data <- wine_train[train_index, ]
    test_data <- wine_train[-train_index, ]
    
    model <- randomForest(quality ~ ., data = train_data, ntree = ntree)
    predictions <- predict(model, newdata = test_data)
    accuracies[j] <- sum(predictions == test_data$quality) / length(predictions)
  }
  
  # Calculate average accuracy
  accuracy_values[i] <- mean(accuracies)
}

# Find the index of the maximum accuracy
best_index <- which.max(accuracy_values)
print(ntree_values[best_index])
print(accuracy_values)

# 150 trees gives the best accuracy.

# Let's build the final model with the best parameters.
wine_Forest <- randomForest(quality ~ ., data = wine_train, mtry = 5, ntree = 150, nodesize = 10)

# Quality of classification for training set.
wine_Forest_pred_train <- predict(wine_Forest, wine_train)
confusionMatrix(wine_Forest_pred_train, wine_train$quality, mode="everything")

# Quality of classification for test set.
wine_Forest_pred_test <- predict(wine_Forest, wine_test)
confusionMatrix(wine_Forest_pred_test, wine_test$quality, mode="everything")

# The accuracy of the model is 0.95 for training set and 0.78
# for test set. The model is overfitting. Sensitivity for high quality
# wines is 0.08 and specificity is 0.998. The model is not able to predict
# high quality wines. The model is not bad for medium and low quality wines.
# F1 scores are 0.73 for medium and 0.82 for low quality wines.
# Model is better than rpart classifier but worse than C5.0 classifier regarding
# the accuracy and F1 scores and specificity for high quality wines.

################################################################
#  Conclusion                                                  #
################################################################

# The best model is C5.0 classifier with boosting with the accuracy of 0.90 for test set.
# The model is able to predict high quality wines with the sensitivity of 0.40 and even though
# it is not very high, it is the best result among all the models. The specificity is 0.99.
# The model is able to predict low and medium quality wines with the sensitivity of 0.88 and 0.94
# respectively. F1 scores are 0.57 for high, 0.90 for medium and 0.93 for low quality wines.