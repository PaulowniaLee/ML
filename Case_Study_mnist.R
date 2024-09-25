#### Environment ####
library(tidyverse)
library(dslabs)
library(caret)
mnist <- read_mnist()

set.seed(1990)
index <- sample(nrow(mnist$train$images), 10000) 
x <- mnist$train$images[index,]
y <- factor(mnist$train$labels[index])

index <- sample(nrow(mnist$test$images), 1000) 
x_test <- mnist$test$images[index,]
y_test <- factor(mnist$test$labels[index])

remove(mnist, index)

#####

#### Preprocessing ####
nzv <- nearZeroVar(x)
# function recommending features to remove: 
# these features have nearly zero variability in our training set
col_index <- setdiff(1:ncol(x), nzv)
# these are indexes of columns to be kept 

colnames(x) <- 1:ncol(mnist$train$images)
colnames(x_test) <- colnames(x)
# add column names 

#####

#### KNN and RF ####

# knn 
# good practice: do a test run on a subset before running code taking hours
n <- 1000 # sample size
b <- 2 # 2-fold cross validation 
index <- sample(nrow(x), n) # decide on which rows we run
control <- trainControl(method = "cv", number = b, p = 0.9)
train_knn <- train(x[index, col_index], y[index], 
                   # use selected columns only 
                   method = "knn",
                   tuneGrid = data.frame(k = c(3,5,7)),
                   trControl = control)
# 可以不断更改sample size和fold数，来确定一个algorithm要跑多久

# In our example, we use 10-fold ultimately 
control <- trainControl(method = "cv", number = 10, p = .9) 
train_knn <- train(x[ ,col_index], y,
                   method = "knn",
                   tuneGrid = data.frame(k = c(3,5,7)),
                   trControl = control)
train_knn$bestTune

fit_knn <- knn3(x[, col_index], y, k = 5)
y_hat <- predict(fit_knn, x_test[, col_index], type = "class")
cm <- confusionMatrix(y_hat, factor(y_test))
cm$overall["Accuracy"]
cm$byClass[,1:2]
# Accuracy 0.951, pretty good 



# Random Forest 
library(randomForest)
control <- trainControl(method = "cv", number = 5) 
# fitting is costly, so only take 5-fold cv
grid <- data.frame(mtry = c(1,5,10,25,50,100))

train_rf <- train(x[,col_index], y,
                  method = "rf",
                  ntree = 150,
                  trControl = control,
                  tuneGrid = grid,
                  nSamp = 5000 
                  # smaller random sample when constructing each tree
                  )
train_rf$bestTune

fit_rf <- randomForest(x[, col_index], y,
                       minNode = 10)
y_hat <- predict(fit_rf, x_test[,col_index])
cm <- confusionMatrix(y_hat, factor(y_test))
cm$overall["Accuracy"]
# Accuracy 0.957, even better 

#####

#### Variable Importance ####
imp <- importance(fit_rf) 
# computes the importance of each feature 

# we can visualise it 
mat <- rep(0, ncol(x))
mat[col_index] <- imp
image(matrix(mat, 28, 28))
                                                                               method = "knn",
#####
                                                                               
#### Ensembles ####
# improve final result by combining results from different algorithms
p_rf <- predict(fit_rf, x_test[,col_index], type = "prob")
p_rf <- p_rf / rowSums(p_rf) # average of forest as output
p_knn <- predict(fit_knn, x_test[,col_index])
p <- (p_rf + p_knn) / 2 # average of two algorithms as prediction 
y_hat <- factor(apply(p, 1, which.max)-1)
# apply for rows: find maximum column of given row, 
# then colunum index minus 1 (first column is row index) get result
confusionMatrix(y_hat, y_test)$overall["Accuracy"]
# accuracy 0.96, better than both methods going solo

# Remark:
# Ensemble is one reason why we want to build multiple algorithms 
# for a single dataset 

#####

#### Ex ####
library(naivebayes)

models <- c("glm", "lda", "naive_bayes", 
            "knn", "gamLoess", "qda", "rf")
set.seed(1)
data("mnist_27")

# train
fits <- lapply(models, function(model){
  print(model)
  train(y ~ ., method = model, data = mnist_27$train)
})
names(fits) <- models



# predict 
y_hat <- sapply(fits, function(fit){
  predict(fit, mnist_27$test) 
})


acc <- map_chr(y_hat, function(hat){
  confusionMatrix(factor(hat, levels = levels(mnist_27$test$y)), 
                  mnist_27$test$y)$overall["Accuracy"]
})



# 3
# an elegant solution 
acc <- colMeans(y_hat == mnist_27$test$y)
mean(acc)
# a simple solution 
acc <- c(1:7)
for (i in c(1:7)){
  acc[i] <- confusionMatrix(factor(y_hat[,i], 
                            levels = levels(mnist_27$test$y)),
  mnist_27$test$y)$overall["Accuracy"]
} 
mean(acc)
# Remark: confusionMatrix 好像没法和sapply这种兼容



# 4 
vote <- c(1:nrow(y_hat))
for (i in c(1: nrow(y_hat))){
  vote[i] <- ifelse(sum(y_hat[i,] == "7") > 3, "7", "2")
}
# alternative 
votes <- rowMeans(y_hat == "7")
vote <- ifelse(votes > 0.5, "7", "2")
remove(votes)

mean(vote == mnist_27$test$y)



# 6
# we can use accuracy on train set to decide which algorithm to preserve 

train_acc <- c(
  fits$glm$results$Accuracy,
  fits$lda$results$Accuracy,
  fits$naive_bayes$results$Accuracy,
  fits$knn$results$Accuracy,
  fits$gamLoess$results$Accuracy,
  fits$qda$results$Accuracy,
  fits$rf$results$Accuracy
)
# correct way 
train_acc <- sapply(fits, function(fit){
  min(fit$results$Accuracy)
}) #只要中间的是个function就好了
mean(train_acc)



# 7 
selected <- names(which(train_acc >= 0.8))
votes <- rowMeans(subset(y_hat, select = selected) == "7")
vote <- ifelse(votes >= 0.5, "7", "2")
remove(votes)
mean(vote == mnist_27$test$y)
#alternative 
selected <- train_acc >= 0.8
votes <- rowMeans(y_hat[,selected] == "7")
vote <- ifelse(votes>=0.5, "7", "2")
mean(vote == mnist_27$test$y)


#####