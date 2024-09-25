#### Environment ####
library(tidyverse)
library(dslabs)
library(caret)

getwd()
setwd("/Users/mac/Documents/R_Notes/ML")

data("mnist_27")

#####

#### caret package ####
train_glm <- train(y ~ ., method = "glm", data = mnist_27$train)
train_knn <- train(y ~., method = "knn", data = mnist_27$train)
# train: train different algorithms, all similar syntax

y_hat_glm <- predict(train_glm, mnist_27$test, type = "raw")
y_hat_knn <- predict(train_knn, mnist_27$test, type = "raw")
# predict: use results of train directly, although they are different

confusionMatrix(y_hat_glm, mnist_27$test$y)$overall[["Accuracy"]]
confusionMatrix(y_hat_knn, mnist_27$test$y)$overall[["Accuracy"]]



# When an algorithm inlucdes a tuning parameter, train automatically 
# use cross validation to decide it from a few default values 
 # We can also customise 
set.seed(2008)
train_knn <- train(y ~ ., method = "knn",
                   data = mnist_27$train,
                   tuneGrid = data.frame(k = seq(9,71,2)))
# tuneGrid：手动确定potential k
ggplot(train_knn, highlight = T)
# ggplot中optimal parameter会被框出
train_knn$bestTune
train_knn$finalModel
# 读取best model 
# predict使用的也是best model

# We can also change how we do cross validation 
set.seed(2008)
control <- trainControl(method = "cv", number = 10, p = 0.9) 
# 10-fold cross validation
train_knn_cv <- train(y ~., method = "knn",
                      data = mnist_27$train,
                      tuneGrid = data.frame(k = seq(9, 71, 2)),
                      trControl = control)
ggplot(train_knn_cv, highlight = T)



# gamLoess method 
#grid <- expand.grid(span = seq(0.15,0.65, len = 10), degree = 1)
#train_loess <- train(y ~ .,
#                     method = "gamLoess",
#                     tuneGrid=grid,
#                     data = mnist_27$train) 
# kNN with a normal kernel 
# ggplot(train_loess, highlight = TRUE)

remove(train_knn, train_glm, train_knn_cv, control)
remove(y_hat_glm, y_hat_knn)
#####

#### Generative Models ####

# To get conditional probability Pr(Y = 1 | X = x)
# distribution of X can be helpful 
# models using this property are generative models

# If we don't consider distribution of X (say, we directly assume it)
# then these models are called discriminant models 



# Naive Bayes 
# estimate conditional distributions of the predictors, then Bayes' rule
# useful when predictors are few and categories are many 
# still, sometimes we just don't know the distribution 

# mathematically, Naive Bayes is very similar to logistic regression

# eg
data("heights")

y <- heights$height
set.seed(1995)
test_index <- createDataPartition(y, times = 1, p = 0.5, list = FALSE) 
train_set <- heights[-test_index,]
test_set <- heights[test_index,]
# heights given sex is approximately normal, so we know the distribution 
# just by mean and sa
params <- train_set %>%
  group_by(sex) %>%
  summarise(avg = mean(height), sd = sd(height))
params
# [2] is male, [1] is female
pi <- train_set %>%
  summarise(pi = mean(sex == "Female")) %>% #直接用mean接不住
  pull(pi)
# prevalence, Pr(Y = 1)
x <- test_set$height
# Y = 0 is male, Y = 1 is female
f0 <- dnorm(x, params$avg[2], params$sd[2])
f1 <- dnorm(x, params$avg[1], params$sd[1])

p_hat_bayes <- f1*pi / (f1*pi + f0*(1-pi))
# Pr(Y = 1 | X = x)

y_hat_bayes <- ifelse(p_hat_bayes > 0.5, "Female", "Male")
specificity(data = factor(y_hat_bayes), 
            reference = factor(test_set$sex))
sensitivity(data = factor(y_hat_bayes), 
            reference = factor(test_set$sex))
# we see our prediction has high specificity but low sensitivity 
# this is because our prevalence is much lower than population (0.5)
# we can correct it with Naive Bayes 
# i.e. change prevalence when we get p_bayes
p_hat_bayes_unbiased <- f1 * 0.5 / (f1 * 0.5 + f0 * (1 - 0.5)) 
y_hat_bayes_unbiased <- ifelse(p_hat_bayes_unbiased > 0.5, 
                               "Female", "Male")
sensitivity(data = factor(y_hat_bayes_unbiased), 
            factor(test_set$sex)) 
specificity(data = factor(y_hat_bayes_unbiased), 
            factor(test_set$sex)) 
qplot(x, p_hat_bayes_unbiased, geom = "line") + 
  geom_hline(yintercept = 0.5, lty = 2) + 
  geom_vline(xintercept = 67, lty = 2)
# the given cutoff of sex is abround 66-67, 
# which is about the middle of male and female avarage heights



# Quadratic discriminant analysis (QDA)
# a version of Naive Bayes, 
# assume conditional distribution of Xis multivariate normal
# 上面那个实际也是QDA

# 2 or 7 eg
data("mnist_27")

# 这次有两个predictor，还有correlation
params <- mnist_27$train %>%
  group_by(y) %>%
  summarise(avg_1 = mean(x_1), avg_2 = mean(x_2),
            sd_1 = sd(x_1), sd_2 = sd(x_2),
            r = cor(x_1, x_2))
params
# 不过实际上直接用train就好了
train_qda <- train(y ~., method = "qda", 
                   data = mnist_27$train)
y_hat <- predict(train_qda, mnist_27$test)
confusionMatrix(y_hat, mnist_27$test$y)$overall["Accuracy"]



# Linear discriminant analysis 
# QDA becomes infeasible if there are many parameters 
# say, 10, then 10 mean, 10 sd, 45 cor
# LDA provides a simplification: assume all corrlations are the same 
# (hence all sds are the same as well)

# 沿用2 or 7 example
train_lda <- train(y ~., method = "lda", data = mnist_27$train)
y_hat <- predict(train_lda, mnist_27$test)
confusionMatrix(y_hat, mnist_27$test$y)$overall["Accuracy"]

# Remark:
# QDA: boundary is quadratic 
# LDA: boundary is linear (a line)
# so QDA is more flexible, as LDA cannot capture nonlinear features

#####
  
#### Case study: more than 3 classes ####
# data generation 
{
if(!exists("mnist")) mnist <- read_mnist()
set.seed(3456)
index_127 <- sample(which(mnist$train$labels %in% c(1,2,7)), 2000) 
y <- mnist$train$labels[index_127]
x <- mnist$train$images[index_127,]
index_train <- createDataPartition(y, p=0.8, list = FALSE)
## get the quadrants
row_column <- expand.grid(row=1:28, col=1:28)
upper_left_ind <- which(row_column$col <= 14 & row_column$row <= 14) 
lower_right_ind <- which(row_column$col > 14 & row_column$row > 14) 
## binarize the values. Above 200 is ink, below is no ink
x <- x > 200
## proportion of pixels in lower right quadrant
x <- cbind(rowSums(x[ ,upper_left_ind])/rowSums(x),
           rowSums(x[ ,lower_right_ind])/rowSums(x)) 
##save data
train_set <- data.frame(y = factor(y[index_train]),
                        x_1 = x[index_train,1], x_2 = x[index_train,2])
test_set <- data.frame(y = factor(y[-index_train]),
                       x_1 = x[-index_train,1], x_2 = x[-index_train,2])
}
# 这次是1 or 2 or 7

# qda
train_qda <- train(y ~ ., method = "qda", data = train_set)
confusionMatrix(predict(train_qda, test_set), test_set$y)$table
confusionMatrix(predict(train_qda, test_set), 
                test_set$y)$overall["Accuracy"] # 0.749
# lda
train_lda <- train(y ~ ., method = "lda", data = train_set)
confusionMatrix(predict(train_lda, test_set), test_set$y)$table
confusionMatrix(predict(train_lda, test_set), 
                test_set$y)$overall["Accuracy"] # 0.629
# knn
train_knn <- train(y ~., method = "knn", data = train_set,
                   tuneGrid = data.frame(k = seq(15,51,2)))
confusionMatrix(predict(train_knn, test_set), test_set$y)$table
confusionMatrix(predict(train_knn, test_set), 
                test_set$y)$overall["Accuracy"] # 0.747

# Problems of generative model here is lack of fit for normal assumption
train_set %>% 
  mutate(y = factor(y)) %>% 
  ggplot(aes(x_1, x_2, fill = y, color=y)) + 
  geom_point(show.legend = FALSE) + 
  stat_ellipse(type="norm") # 这样画在椭圆内就是符合normal distribution
# 可见normal不能很好地概括distribution

#####

#### Classification and Regression Trees (CART) ####
# useful when there are many predictors (say 100+)
# idea:
# use data to create decision trees, with predictions at the ends 
# each predictor is a decision, used to partition outcome variable 



# Regression Trees
# (continuous outcome)

data("polls_2008")
qplot(day, margin, data = polls_2008)
# predict margin with day

library(rpart)
fit <- rpart(margin ~ ., data = polls_2008)

plot(fit, margin = 0.1)
text(fit, cex = 0.75)
# end with 8 partitions 
polls_2008 %>%
  mutate(y_hat = predict(fit)) %>% 
  ggplot() +
  geom_point(aes(day, margin)) + 
  geom_step(aes(day, y_hat), col="violet")
# our final fit

# Remark: How do we create partitions?
# 1. pick predictor and its value:
#    minimise the residual sum of square (RSS)
# 2. partition by predictor and value 
#    小于的在一边，大于等于另一边

# Remark: When does partition stops?
# 1. complexity parameter cp
# minimum for how much RSS must improve for another partition to be added
# 2. minsplit 
# minimum of observations in a partition before partitioning it further
# 3. minbucket 
# minimum number of observations in each node 

# use cross validation for fine tunning 
train_rpart <- train(margin ~ .,
                     method = "rpart",
                     tuneGrid = data.frame(cp = seq(0, 0.05, len = 25)), 
                     data = polls_2008)
ggplot(train_rpart)
plot(train_rpart$finalModel, margin = 0.1)
text(train_rpart$finalModel, cex = 0.75)
polls_2008 %>%
  mutate(y_hat = predict(train_rpart)) %>% 
  ggplot() +
  geom_point(aes(day, margin)) + 
  geom_step(aes(day, y_hat), col="purple")



# Classification (Decision) Trees
# (categorical outcome)
train_rpart <- train(y ~ .,
                     method = "rpart",
                     tuneGrid = data.frame(cp = seq(0.0, 0.1, len = 25)), 
                     data = mnist_27$train)
y_hat <- predict(train_rpart, mnist_27$test) 
confusionMatrix(y_hat, mnist_27$test$y)$overall["Accuracy"]
# better than linear regression, worse than kNN
# problem:
# not flexible, highly unstable to changes in training data 
# (hence rarely the best performing method)

#####

#### Random Forests ####
# average multiple decision trees
# random: bootstrap makes individual trees randomly different
# forest: combination of trees is the forest

# 1. Build B decision trees using training set
# 2. For every observation in the test set, form predictions using trees
# 3. For continuous, average predictions; 
#    For categories, majority vote 

# Create forest:
# 1. Bootstrap training set: sampling N observations with replacement 
# 2. randomly select features to be included for each tree 
#    (usually there are a large number of features)



library(randomForest)
fit <- randomForest(margin~., data = polls_2008)
plot(fit)
# we see error decreases quickly as we add about 30 trees
polls_2008 %>%
  mutate(y_hat = predict(fit, newdata = polls_2008)) %>% 
  ggplot() +
  geom_point(aes(day, margin)) +
  geom_line(aes(day, y_hat), col="violet")



# example for 2 or 7
train_rf <- randomForest(y ~ ., data=mnist_27$train)
confusionMatrix(predict(train_rf, mnist_27$test), 
                mnist_27$test$y)$overall["Accuracy"]
# we can also change parameters for randomeForest
nodesize <- seq(1, 51, 10)
acc <- sapply(nodesize, function(ns){
  train(y ~., method = "rf", data = mnist_27$train,
        tuneGrid = data.frame(mtry = 2),
        nodesize = ns)$results$Accuracy
})
qplot(nodesize, acc)
train_rf_2 <- randomForest(y ~., data = mnist_27$train,
                           nodesize = nodesize[which.max(acc)])
confusionMatrix(predict(train_rf_2, mnist_27$test),
                mnist_27$test$y)$overall["Accuracy"]
# we see the performance is better 



# Remark:
# Disadvantage of random forest is the loss of interpretability 
# Still, checking variable importance can help
# (i.e. how often one predictor is used in the individual trees)

#####
