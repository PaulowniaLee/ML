#### Environment ####
library(tidyverse)
library(dslabs)
library(caret)
library(gridExtra)

getwd()
setwd("/Users/mac/Documents/R_Notes/ML")

data("mnist_27")

#####

#### k-nearest neighbours ####
# first define distrance between all observations
# for (x1, x2), consider k-nearest poionts, take their average (0 and 1), 
# this is the estimate for conditional probability at (x1, x2)

knn_fit <- knn3(y ~ ., data = mnist_27$train,
                k = 5)
# . means we use all predictors available in the data set 
# k = 5: 5 nearest points 
y_hat_knn <- predict(knn_fit, mnist_27$test, type = "class") 
# type = "class"
# without specification, predict produces probability for each class
# with specification, predict directly gives factor results
confusionMatrix(y_hat_knn, mnist_27$test$y)$overall["Accuracy"]



# Over-training 
# worst at k = 1
# also reasons why performance in train-set is better than in test-set

knn_fit_1 <- knn3(y ~ ., data = mnist_27$train, k = 1)

p1 <- mnist_27$true_p %>% 
  mutate(knn = predict(knn_fit_1, newdata = .)[,2]) %>%
  ggplot() +
  geom_point(data = mnist_27$train, 
             aes(x_1, x_2, color= y), pch=21) +
  scale_fill_gradientn(colors=c("#F8766D", "white", "#00BFC4")) +
  stat_contour(aes(x_1, x_2, z = knn), 
               breaks=c(0.5), color="black") +
  ggtitle("Train set")

p2 <- mnist_27$true_p %>% 
  mutate(knn = predict(knn_fit_1, newdata = .)[,2]) %>%
  ggplot() +
  geom_point(data = mnist_27$test, 
             aes(x_1, x_2, color= y), 
             pch=21, show.legend = FALSE) +
  scale_fill_gradientn(colors=c("#F8766D", "white", "#00BFC4")) +
  stat_contour(aes(x_1, x_2, z = knn), 
               breaks=c(0.5), color="black") +
  ggtitle("Test set")
grid.arrange(p1, p2, nrow=1)
# 可以看到prediction基本是刚刚勾出了train set里2的轮廓
# 这在test set就不行了



# Over-smoothing 
# getting worse when k getting larger (less flexibility)
# e.g. k = 401: Similar to linear regression 



#####

#### Ex k-nearest ####

# 1
set.seed(1)
test_index <- createDataPartition(heights$sex, times = 1,
                                  p = 0.5, list = F)
test_set <- heights[test_index,]
train_set <- heights[-test_index,]

k <- seq(1, 101, 3)
Rikako <- function(n){
  fit <- knn3(sex ~., data = train_set, k = n)
  y_hat <- predict(fit, test_set, type = "class") %>%
    factor(levels = levels(test_set$sex))
  F_meas(data = y_hat, reference = factor(test_set$sex))
}
set.seed(1)
F_1 <- sapply(k, Rikako)

max(F_1)
k[which.max(F_1)]



# 2
data("tissue_gene_expression")

set.seed(1)
test_index <- createDataPartition(tissue_gene_expression$y, times = 1,
                                  p = 0.5, list = F)
test_set <- data.frame(x = tissue_gene_expression$x[test_index,],
                       y = tissue_gene_expression$y[test_index])
train_set <- data.frame(x = tissue_gene_expression$x[-test_index,],
                       y = tissue_gene_expression$y[-test_index])

k <- seq(1, 11, 2)
Mari <- function(k){
  fit <- knn3(y ~., data = train_set, k = k)
  y_hat <- predict(fit, test_set, type = "class") %>%
    factor(levels = levels(test_set$y))
  confusionMatrix(y_hat, test_set$y)$overall["Accuracy"]
}
set.seed(1)
accuracy <- sapply(k, Mari)

accuracy

#####

#### k-fold cross validation ####
# We first split the data into train set and test set 

# 0. fix the parameters
# 1. split the train set into k non-overlapping sets 
# 2. for the i-th split, there is a a validation set, and the remaining
# fit the model on the remaining, and get sample MSE on validation set
# 3. repeat the process, we get k sample MSEs
# 4. take the average of these sample MSEs, and consider it as true MSE

# Hence for this set of parameters, we get their MSE
# Find parameters minimise the MSE, and we are done 

#####

#### Example for Bootstrap ####
set.seed(1995)
n <- 10^6
income <- 10^(rnorm(n, log10(45000), log10(3)))
qplot(log10(income), bins = 30, color = I("black"))
# suppose this is our population 

# we get a N = 100 sample from the population 
# and we want to estimate the population median 
N <- 100
X <- sample(income, N) 
median(X)
# we find the estimate is quite different from true population median
m <- median(income) 
m

# Here comes bootstrap
# 1. treat out dataset X (size N=100) as if it is the population 
# 2. sample with replacement from dataset X, of the same sample size (N)
# 3. compute the summary statistic on these bootstrap samples 
# By theory: distribution of statistics obtained this way 
# approximate the distribution of the actual population 
B <- 10^4
M_star <- replicate(B, {
  X_star <- sample(X, N, replace = TRUE)
  median(X_star) 
})
quantile(M_star, c(0.025, 0.975))


# Takeaway:
# We can use similar ideas in cross validation:
# Instead of dividing the data into equal partitions (as in k-fold)
# We simply bootstrap many times

#####

#### Ex Bootstrap ####
data("mnist_27")

set.seed(1995)
indexes <- createResample(mnist_27$train$y, 10)

# 1
count <- as.data.frame(table(indexes[1]))
count[1:7,]

# 2
n <- seq(1,10,1)
p <- sapply(n, function(n){
  sum(indexes[[n]] == 3)
})
sum(p)

# 4
set.seed(1)
y <- rnorm(100,0,1)
quantile(y, probs = 0.75)


set.seed(1)
B <- 10^4
Q_star <- replicate(B, {
  y_star <- sample(y, 100, replace = TRUE)
  quantile(y_star, probs = 0.75) 
})
mean(Q_star)
sd(Q_star)
#####






