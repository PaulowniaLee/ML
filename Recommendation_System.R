#### Environment ####
library(tidyverse)
library(dslabs)
library(caret)

setwd("/Users/mac/Documents/R_Notes/ML")

data("movielens")
names(movielens)
movielens[1:3,]
#####

#### understand the challenge ####
movielens %>%
  summarise(n_users = n_distinct(userId),
            n_movies = n_distinct(movieId))
# note that not every user rated every movie 

hist(sort(table(movielens$movieId), decreasing = T),
     xlab = "n", ylab = "count",
     main = "Movies")
hist(sort(table(movielens$userId), decreasing = T),
     xlab = "n", ylab = "count",
     main = "Users")
# some movies get rated much more 
# some users are much more active



set.seed(755)
test_index <- createDataPartition(y = movielens$rating, times = 1, p = 0.2,
                                  list = FALSE) 
train_set <- movielens[-test_index,]
test_set <- movielens[test_index,]
# want to ensure that 
# movies and users appear in the test set also appears in the training set
test_set <- test_set %>% 
  semi_join(train_set, by = "movieId") %>% 
# returns all rows from test_set that has a match in train_set, by movieId
  semi_join(train_set, by = "userId")
# two lines in total:
# returns all rows from test_set that has a match in train_set,
# by both movieId and userId
# 即：如果某个entry的movieId或者userId在train set没出现，那么我们也将其排除
# test set （无法预测没有的数据）



# Loss function: RMSE
# by requirement 
RMSE <- function(true_ratings, predicted_ratings){
  sqrt(mean((true_ratings - predicted_ratings)^2))
}

#####

#### First Model ####

# start: all movies get same rating, regardless of users
# Y_u,i = mu + epsilon_u,i

mu_hat <- mean(train_set$rating)
naive_rme <- RMSE(test_set$rating, mu_hat)
naive_rme
# RMSE is 1.05
rmse_results <- tibble(method = "Just the average",
                       RMSE = naive_rme)



# Movie Effect 
# each movie has an effect b_i

# use least squares for estimation
# in our example, it is actually the average of Y_u,i - mu for each movie i
# (这么算比lm快)
movie_avg <- train_set %>%
  group_by(movieId) %>%
  summarise(b_i = mean(rating - mu_hat))

prediction <- mu_hat + test_set %>%
  left_join(movie_avg, by = "movieId") %>%
  pull(b_i)
# 这里先把movie_avg变成test_set的一个column (left_join, matching by movieId)，
# 然后把movie_avg拖出来加上mu_hat，保存单个column作为prediction
# 不过顺序和test_set一样，所以可以直接比较
RMSE(true_ratings = test_set$rating,
     predicted_ratings = prediction)
# RMSE is 0.989



# User Effect 
# each user has an effect b_u

# use least square for estimation 
# we again know the result: average of Y_u,i - mu - b_i for fixed user u
user_avg <- train_set %>%
  left_join(movie_avg, by = "movieId") %>%
  group_by(userId) %>%
  summarise(b_u = mean(rating - mu_hat - b_i))

prediction <- test_set %>%
  left_join(movie_avg, by = "movieId") %>%
  left_join(user_avg, by = "userId") %>%
  mutate(pred = mu_hat + b_i + b_u) %>%
  pull(pred)

rmse_results <- add_row(rmse_results,
                        method = "movie and user linear effect",
                        RMSE = RMSE(true_ratings = test_set$rating,
                                   predicted_ratings = prediction))
# RMSE is 0.905

#####

#### Regularization ####

# Our prediction did not improve much with two effect:
# reason: some low/high ratings rated by a few caused large mistakes 

# Regularization:
# Penalize large estimates formed using small sample sizes
# implementation: add a penalty after least squares, s.t. 
# 1) similar to least square when sample size is large
# 2) shrink estimated effect towards 0 when sample size is small

lambda <- 3
mu <- mean(train_set$rating)
movie_reg_avg <- train_set %>%
  group_by(movieId) %>%
  summarise(b_i = sum(rating - mu)/(n() + lambda),
            n_i = n())
# n() returns information about "current" group size
# only work inside contexts of tidyverse, like summarise()

tibble(original = movie_avg$b_i,
       regularlised = movie_reg_avg$b_i,
       n = movie_reg_avg$n_i) %>%
  ggplot(aes(original, regularlised, size = sqrt(n))) + 
  geom_point(shape = 1, alpha = 5)
# 可以看到regularlised model缩小了那些小样本造成的大估计
# 🐑 🥁 🍊 🍬 🐷

prediction <- test_set %>%
  left_join(movie_reg_avg, by = "movieId") %>%
  mutate(pred = mu + b_i) %>%
  pull(pred)

rmse_results <- add_row(rmse_results,
                        method = "Regularised movie effect",
                        RMSE = RMSE(true_ratings = test_set$rating,
                                    predicted_ratings = prediction))



# Choose the penalty terms 
# we use cross validation for lambda 
lambdas <- seq(0,10,0.25)

# to conduct cross validation, we have to partition train set again
# (we cannot use test set)
set.seed(2024)
test_index <- createDataPartition(y = train_set$rating, times = 1, p = 0.2,
                                  list = FALSE) 
training_set <- train_set[-test_index,]
testing_set <- train_set[test_index,]
testing_set <- testing_set %>% 
  semi_join(training_set, by = "movieId") %>% 
  semi_join(training_set, by = "userId")

# we first find best lambda for estimating movie effect 
mu <- mean(training_set$rating)
just_the_sum <- training_set %>%
  group_by(movieId) %>%
  summarize(s = sum(rating - mu), n_i = n()) 
# fitted model 

rmses <- sapply(lambdas, function(l){ 
  predicting <- testing_set %>%
  left_join(just_the_sum, by='movieId') %>% 
  mutate(b_i = s/(n_i+l)) %>%
  mutate(pred = mu + b_i) %>%
  pull(pred)
return(RMSE(predicted_ratings = predicting, 
            true_ratings =  testing_set$rating)) 
})
# predict and evaluate 

remove(training_set, testing_set)

qplot(lambdas, rmses) 
lambdas[which.min(rmses)]
# best lambda is 2.75

remove(lambda, lambdas)
remove(just_the_sum, movie_avg, movie_reg_avg, test_index,
       user_avg, mu, mu_hat, naive_rme, prediction, rmses)


# based on this, we improve our method to find lambda in two-effects model
# (same lambda for two penalties)
lambdas <- seq(0,10,0.25)

# get a partition 
set.seed(2024)
test_index <- createDataPartition(y = train_set$rating, times = 1, p = 0.2,
                                  list = FALSE) 
training_set <- train_set[-test_index,]
testing_set <- train_set[test_index,]
testing_set <- testing_set %>% 
  semi_join(training_set, by = "movieId") %>% 
  semi_join(training_set, by = "userId")

mu <- mean(training_set$rating)

rmses <- sapply(lambdas, function(l){
  # get fit 
  b_i <- training_set %>%
    group_by(movieId) %>%
    summarise(b_i = sum(rating - mu)/(n() + l))
  
  b_u <- training_set %>%
    left_join(b_i, by = "movieId") %>%
    group_by(userId) %>%
    summarise(b_u = sum(rating - b_i - mu)/(n() + l))
  
  # get prediction and evaluation 
  predicting <- testing_set %>%
    left_join(b_i, by = "movieId") %>%
    left_join(b_u, by = "userId") %>%
    mutate(pred = mu + b_i + b_u) %>%
    pull(pred)
  
  return(RMSE(predicted_ratings = predicting,
              true_ratings = testing_set$rating))
})

qplot(lambdas, rmses) 
lambdas[which.min(rmses)]
# best lambda is 3.5

remove(training_set, testing_set, mu, lambdas, rmses, test_index)



# Two-effects regularised model 
mu <- mean(train_set$rating)
l <- 3.5

# get fit 
b_i <- train_set %>%
  group_by(movieId) %>%
  summarise(b_i = sum(rating - mu)/(n() + l))

b_u <- train_set %>%
  left_join(b_i, by = "movieId") %>%
  group_by(userId) %>%
  summarise(b_u = sum(rating - b_i - mu)/(n() + l))

# get prediction 
prediction <- test_set %>%
  left_join(b_i, by = "movieId") %>%
  left_join(b_u, by = "userId") %>%
  mutate(pred = mu + b_i + b_u) %>%
  pull(pred)

rmse_results <- add_row(rmse_results,
                        method = "Regularised movie + user effect",
                        RMSE = RMSE(true_ratings = test_set$rating,
                                    predicted_ratings = prediction))
# resulting rmse is 0.881, much better
# Remark:
# we can perform k-fold cross validation as well, but that takes much longer

#####

#### Structure in the residual ####
# matrix factorisation:
# related to factor analysis, SVD, PCA

# in our model: Y_u,i = mu + b_i + b_u + epsilon_u,i
# but residual is not really independent 

# we illustrate with a small part of data 
train_small <- movielens %>%
  group_by(movieId) %>%
  filter(n() >= 50 | movieId == 3252) %>% 
  ungroup() %>% 
  group_by(userId) %>%
  filter(n() >= 50) %>% 
  ungroup()

y <- train_small %>%
  dplyr::select(userId, movieId, rating) %>% 
  # select is present in both MASS and dplyr, double-colons is used to 
  # specify which function we are calling (不然程序运行不了)
  pivot_wider(names_from = "movieId", values_from = "rating") %>% 
  as.matrix()

# add row names 
rownames(y)<- y[,1] 
y <- y[,-1]
# add column names
movie_titles <- movielens %>% 
  dplyr::select(movieId, title) %>% 
  distinct()
colnames(y) <- with(movie_titles, title[match(colnames(y), movieId)])

# consider the residuals 
y <- sweep(y, 2, colMeans(y, na.rm=TRUE)) #减去movie effect
y <- sweep(y, 1, rowMeans(y, na.rm=TRUE)) #减去user effect
# 🌹 关于sweep (documentation is trash)
{
# sweep: used when you operate a matrix by row or by column
  
# MARGIN: 2 means by column, 1 means by row
# FUN: function to be applied for each column / each row 
#      default function is "-", 即减法
# STATS: an input for FUN
}

# If our model has explained all, then these residuals should be independent
m_1 <- "Godfather, The"
m_2 <- "Godfather: Part II, The"
p1 <- qplot(y[ ,m_1], y[,m_2], xlab = m_1, ylab = m_2)
#
m_1 <- "Godfather, The"
m_3 <- "Goodfellas"
p2 <- qplot(y[ ,m_1], y[,m_3], xlab = m_1, ylab = m_3)
#
m_4 <- "You've Got Mail"
m_5 <- "Sleepless in Seattle"
p3 <- qplot(y[ ,m_4], y[,m_5], xlab = m_4, ylab = m_5)
#
gridExtra::grid.arrange(p1, p2 ,p3, ncol = 3)
# people like Godfather I also tend to like Godfather II
x <- y[, c(m_1, m_2, m_3, m_4, m_5)]
short_names <- c("Godfather", "Godfather2", "Goodfellas",
                 "You've Got", "Sleepless") 
colnames(x) <- short_names
cor(x, use="pairwise.complete")
# two groups of users here: 
# if you like godfather, you tend to like romantic movies less


#####

#### SVD Ex ####
# factor analysis 可以通过找SVD来实现 (不过SVD没有很好的interpretation)
# SVD和PCA本质是同一个东西
# It computes the orthogonal transform that de-correlates the variables and 
# keeps the ones with the largest variance.



# example: grade score for 100 students in 24 subjects
set.seed(1987)
n <- 100
k <- 8
Sigma <- 64  * matrix(c(1, .75, .5, .75, 1, .5, .5, .5, 1), 3, 3) 
m <- MASS::mvrnorm(n, rep(0, 3), Sigma)
m <- m[order(rowMeans(m), decreasing = TRUE),]
y <- m %x% matrix(rep(1, k), nrow = 1) + matrix(rnorm(matrix(n*k*3)), n, k*3)
colnames(y) <- c(paste(rep("Math",k), 1:k, sep="_"),
                 paste(rep("Science",k), 1:k, sep="_"),
                 paste(rep("Arts",k), 1:k, sep="_"))
# we will show that by just three relatively small pairs of vectors 
# we can explain much of the variability in this 100 x 24 dataset 


#1  visualisation of scores 
my_image <- function(x, zlim = range(x), ...){
  colors = rev(RColorBrewer::brewer.pal(9, "RdBu"))
  cols <- 1:ncol(x)
  rows <- 1:nrow(x)
  image(cols, rows, t(x[rev(rows),,drop=FALSE]), xaxt = "n", yaxt = "n",
        xlab="", ylab="",  col = colors, zlim = zlim, ...)
  abline(h=rows + 0.5, v = cols + 0.5)
  axis(side = 1, cols, colnames(x), las = 2)
}

my_image(y)


#2 check correlation between scores 
my_image(cor(y), zlim = c(-1,1))
range(cor(y))
axis(side = 2, 1:ncol(y), rev(colnames(y)), las = 2)


# 3 get SVD
s <- svd(y)
names(s) # returns U, V, and diagonal entries of D
# how svd works 
y_svd <- s$u %*% diag(s$d) %*% t(s$v)
# Y = UDV^t
max(abs(y - y_svd)) # these two are indeed the same 

# Y and YV has same total variability 
# (same is true for U^tV)
ss_y <- apply(y, 2, FUN = function(x){sum(x^2)})
# 求每一列的平方和 (最后是14个和)
# 所以是2 (which indicates columns)
ss_yv <- apply((y %*% s$v), 2, FUN = function(x){sum(x^2)})
max(abs(ss_y - ss_yv))
sum(ss_y)


# 4 
qplot(seq(1,ncol(y),1), ss_y)
qplot(seq(1,ncol(y),1), ss_yv)
# ss_y don't have a pattern 
# ss_yv is decreasing and close to 0 for the 4th column and beyond 


# 5
qplot(sqrt(ss_yv), s$d)
# note YV = UD
# so the sum of squares of columns of UD are the diagonal entries of D squared


# 6 
sum(ss_yv[1:3]) / sum(ss_yv)


# 7 compute UD with sweep (without diag)
identical(s$u %*% diag(s$d), 
          sweep(s$u, 2, s$d, FUN = "*"))


# 8 
# VY = UD
# variability of Y = variability of D = variability of UD
# (multiplying an orthogonal matrix, like V and U, doesn't change variability)
# we know D has diagonals listed in descending order 
# => variability of columns of D (and UD) is in decreasing order 
avg_score <- rowMeans(y)
ud <- sweep(s$u, 2, s$d, FUN = "*")
qplot(avg_score, ud[,1])
# here we consider first column of UD (most variability)
# we find a linear correlation with average score 


# 9 
my_image(s$v)
# The first column is very close to being a constant, 
# which implies that the first column of YV 
# is the sum of the rows of Y multiplied by some constant, 
# and is thus proportional to an average.



# 10 
# we can write entire SVD as 
# Y = U1d11V^t1 + U2d22V^t2 + .... + UpdppV^tp
plot(s$u[,1], ylim = c(-0.25, 0.25))
plot(s$v[,1], ylim = c(-0.25, 0.25))
with(s, my_image(
       (u[, 1, drop=FALSE]*d[1]) %*% t(v[, 1, drop=FALSE])
       ))
my_image(y)
# two images are close (while svd one reordered by variability)



# 11
# our first matrix factorisation Y ~ d11U1V^t1
s$d[1]^2/sum(s$d^2) # explains 72.6% total variability 
# interpretation: good students tend to be good in all subjects 

resid <- y - with(s,(u[,1, drop=FALSE]*d[1]) %*% t(v[,1, drop=FALSE])) 
# remove overall student effect
my_image(cor(resid), zlim = c(-1,1))
axis(side = 2, 1:ncol(y), rev(colnames(y)), las = 2)
# but within subject correlation is still high 
# there are also correlations between science and math 

plot(s$u[,2], ylim = c(-0.5, 0.5))
plot(s$v[,2], ylim = c(-0.5, 0.5))
with(s, my_image(
  (u[, 2, drop=FALSE]*d[2]) %*% t(v[, 2, drop=FALSE])
))
my_image(resid)
# again we find similarity between resid and second column 



# 12 
# add second column to our approximation 
# interpretation: student has different ability in math/science vs arts 
sum(s$d[1:2]^2)/sum(s$d^2) * 100 # explains 89.84% total variability 

# consider residual again 
resid <- y - with(s,sweep(u[,1:2], 2, d[1:2], FUN="*") %*% t(v[,1:2])) 
my_image(cor(resid), zlim = c(-1,1))
axis(side = 2, 1:ncol(y), rev(colnames(y)), las = 2)
# remaining structure is driven by differences between math and science 

plot(s$u[,3], ylim = c(-0.5, 0.5))
plot(s$v[,3], ylim = c(-0.5, 0.5))
with(s, my_image(
  (u[, 3, drop=FALSE]*d[3]) %*% t(v[, 3, drop=FALSE])
)) # 明显表现出规律
my_image(resid)
# again we find similarity between resid and third column 



# 13 
# we have three columns already 
sum(s$d[1:3]^2)/sum(s$d^2) * 100 
# explains 98.779%
resid <- y - with(s,sweep(u[,1:3], 2, d[1:3], FUN="*") %*% t(v[,1:3])) 
my_image(cor(resid), zlim = c(-1,1))
axis(side = 2, 1:ncol(y), rev(colnames(y)), las = 2)
# we no longer see structures
# residuals seems to be independent of each other, and we are done

with(s, my_image(
  (u[, 1:3, drop=FALSE]*d[1:3]) %*% t(v[, 1:3, drop=FALSE])
)) 
my_image(y)
# 明显看到三个组别
my_image(cor(y), zlim = c(-1,1))
my_image(cor(with(s,sweep(u[,1:3], 2, d[1:3], FUN="*") %*% t(v[,1:3]))), 
         zlim = c(-1,1))
# 可以看到y里面的correlation很好地被三个column概括了


# Final Remark
# components d_jjU_jV^t_j are equivalent to the jth principal component 
# 所以说PCA就是SVD，或者说SVD是实现PCA的一种numerical method 

#####
