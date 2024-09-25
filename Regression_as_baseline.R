#### Environment ####
library(tidyverse)
library(dslabs)
library(caret)
data("mnist_27")

setwd("/Users/mac/Documents/R_Notes/ML")
#####

#### Case Study: 2 or 7 ####

# Target: 
# Prediction: 2 or 7?
# Features:dark pixel proportion, upper left (X1) and lower right (X2)

mnist_27$train %>%
  ggplot(aes(x_1, x_2, color = y)) + 
  geom_point()
# pattern: 
# if X1 is large, probably 7
# for smaller X1 and middle values of X2, probably 2



# Model: Conditional Probability 
# p(x1, x2) = Pr(Y = 1 | X1 = x1, X2 = x2)
#           = b0 + b1x1 + b2x2

fit <- mnist_27$train %>%
  mutate(y = ifelse(y == 7, 1, 0)) %>% # dummy y
  lm(y ~ x_1 + x_2, data = .)

p_hat <- predict(fit, newdata = mnist_27$test) 
# estimate conditional probability in test data 
y_hat <- factor(ifelse(p_hat > 0.5, 7, 2))
# decision based on Bayes' rule 选概率大的那个情况作为prediction
confusionMatrix(y_hat, mnist_27$test$y)$overall[["Accuracy"]]
# accuracy is 0.75



# Limitation with OLS:
# cannot capture non-linear nature of the true conditional probability

#####

#### Ex ####
set.seed(1)
n <- 100
Sigma <- 9*matrix(c(1.0, 0.5, 0.5, 1.0), 2, 2)
dat <- MASS::mvrnorm(n = n, c(69, 69), Sigma) %>%
  data.frame() %>% 
  setNames(c("x", "y"))

# 1
Shiny <- function(data){
  test_index <- createDataPartition(
    data[,2], times = 1, p = 0.5, list = F)
  test <- data[test_index,]
  train <- data[-test_index,]
  
  fit <- lm(y ~ x, data = train)
  y_hat <- predict(fit, newdata = test)
  RMSE <- sqrt(mean((y_hat - test[,2])^2))
  # last line is automatically returned
}
set.seed(1)
RMSE <- replicate(100, Shiny(data = dat))
mean(RMSE)
sd(RMSE)



# 2
Yohane <- function(n){
  Sigma <- 9*matrix(c(1.0, 0.5, 0.5, 1.0), 2, 2)
  dat <- MASS::mvrnorm(n = n, c(69, 69), Sigma) %>%
    data.frame() %>% 
    setNames(c("x", "y"))
  RMSE <- replicate(100, Shiny(data = dat))
  result <- list(mean = mean(RMSE), 
                 sd = sd(RMSE))
}
n <- c(100, 500, 1000, 5000, 10000)
set.seed(1) 
# If you don't set seed, results would not be reproducible
# 下面是R default seed generation 说明
{
#Initially, there is no seed; 
# a new one is created from the current time and the process ID 
# when one is required. 
# Hence different sessions will give different simulation results, 
# However, the seed might be restored from a previous session 
# if a previously saved workspace is restored.
}
sapply(n, Yohane)
# Takeaway:
# More prediction, RMSE mean does not change much, but sd decreases
# (our prediction is more consistent)



# 4
set.seed(1)
n <- 100
Sigma <- 9*matrix(c(1.0, 0.95, 0.95, 1.0), 2, 2)
dat <- MASS::mvrnorm(n = 100, c(69, 69), Sigma) %>%
  data.frame() %>% setNames(c("x", "y"))
Shiny <- function(data){
  test_index <- createDataPartition(
    data[,2], times = 1, p = 0.5, list = F)
  test <- data[test_index,]
  train <- data[-test_index,]
  
  fit <- lm(y ~ x, data = train)
  y_hat <- predict(fit, newdata = test)
  RMSE <- sqrt(mean((y_hat - test[,2])^2))
  # last line is automatically returned
}
set.seed(1)
RMSE <- replicate(100, Shiny(data = dat))
mean(RMSE)
sd(RMSE)
# Takeaway:
# With higher correlation between y and x, our prediction is better
# (smallar RMSE)



# 6
set.seed(1)
Sigma <- matrix(c(1.0, 0.75, 0.75, 0.75, 
                  1.0, 0.25, 0.75, 0.25, 1.0), 3, 3)
dat <- MASS::mvrnorm(n = 100, c(0, 0, 0), Sigma) %>%
  data.frame() %>% setNames(c("y", "x_1", "x_2"))

Riko <- function(data){
  test_index <- createDataPartition(
    data[,1], times = 1, p = 0.5, list = F)
  test <- data[test_index,]
  train <- data[-test_index,]
  
  fit <- lm(y ~ x_1 + x_2, data = train)
  y_hat <- predict(fit, newdata = test)
  RMSE <- sqrt(mean((y_hat - test[,1])^2))
  # last line is automatically returned
}
set.seed(1)
RMSE <- replicate(1, Riko(data = dat))
mean(RMSE)

# Takeaway
# (simple econometrics) 
# extra predictors are useful when they are not highly correlated





