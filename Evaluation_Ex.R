#### Ex 1 ####
library(lubridate)
data("reported_heights")
dat <- mutate(reported_heights, date_time = ymd_hms(time_stamp)) %>%
  filter(date_time >= make_date(2016, 01, 25) &
           date_time < make_date(2016, 02, 1)) %>%
  mutate(type = ifelse(day(date_time) == 25 & 
                         hour(date_time) == 8 &
                         between(minute(date_time), 15, 30),
                       "inclass", "online")) %>%
  select(sex, type)
x <- dat$type
y <- factor(dat$sex, c("Female", "Male"))



# 1. 
sum(dat$type == "inclass" & dat$sex == "Female") / sum(dat$type == "inclass")
sum(dat$type == "online" & dat$sex == "Female") / sum(dat$type == "online")
# more elegant way
dat %>% 
  group_by(type) %>% 
  summarize(prop_female = mean(sex == "Female"))

# 2
y_hat <- ifelse(dat$type == "inclass", "Female", "Male") %>%
  factor(levels = c("Female", "Male"))
mean(y == y_hat)

# 3
table(y_hat, y)

# 4 5
sensitivity(data = y_hat, reference = y)
specificity(data = y_hat, reference = y)

# 6
mean(dat$sex == "Female")

#####

#### Ex 2 ####
data(iris)
iris <- iris[-which(iris$Species == "setosa"),]
y <- iris$Species

set.seed(76)
test_index <- createDataPartition(y, times = 1,
                                  p = 0.5, list = F)
test <- iris[test_index,]
train <- iris[-test_index,]



# 8 Search for best predictor 
{
# Sepal.Length
cutoff <- seq(from = min(train$Sepal.Length), 
              to = max(train$Sepal.Length),
              by = 0.1)
accuracy <- map_dbl(cutoff, function(x)
{
  y_hat <- ifelse(train$Sepal.Length > x, 
                  "virginica", "versicolor") %>%
    factor(levels = levels(train$Species))
  mean(y_hat == train$Species) 
})
max(accuracy) # 0.66
# Sepal.Width
cutoff <- seq(from = min(train$Sepal.Width), 
              to = max(train$Sepal.Width),
              by = 0.1)
accuracy <- map_dbl(cutoff, function(x)
{
  y_hat <- ifelse(train$Sepal.Width > x, 
                  "virginica", "versicolor") %>%
    factor(levels = levels(train$Species))
  mean(y_hat == train$Species) 
})
max(accuracy) # 0.62
# Petal.Length
cutoff <- seq(from = min(train$Petal.Length), 
              to = max(train$Petal.Length),
              by = 0.1)
accuracy <- map_dbl(cutoff, function(x)
{
  y_hat <- ifelse(train$Petal.Length > x, 
                  "virginica", "versicolor") %>%
    factor(levels = levels(train$Species))
  mean(y_hat == train$Species) 
})
max(accuracy) # 0.96
# Petal.Width
cutoff <- seq(from = min(train$Petal.Width), 
              to = max(train$Petal.Width),
              by = 0.1)
accuracy <- map_dbl(cutoff, function(x)
{
  y_hat <- ifelse(train$Petal.Width > x, 
                  "virginica", "versicolor") %>%
    factor(levels = levels(train$Species))
  mean(y_hat == train$Species) 
})
max(accuracy) # 0.96
}

# a more elegant example
foo <- function(x){ 
  rangedValues <- seq(range(x)[1], range(x)[2], by=0.1)
  sapply(rangedValues, function(i){
    y_hat <- ifelse(x>i, 'virginica', 'versicolor')
    mean(y_hat==train$Species)
  })
}
{
# lapply and sapply:
# apply a function over a list/vector 
# 前一个变量是list/vector，后一个是function
}
predictions <- apply(X = train[,-5], 
                     MARGIN = 2, 
                     FUN = foo)
{
# apply:
# apply a function to a part of a matrix/array
# X: an array, including a matrix (这里不用column5，因为那是outcome)
# MARGIN: which part of the matrix/array would be used
    # for matrix: 1 means over all rows, 2 means over all columns
# FUN: a function
}
sapply(predictions, max)



# 9 
# Best predictor is Petal.Width 
# Petal.Width
predictions <- foo(train[,4])
rangedValues <- seq(from = min(train[,4]), 
                    to = max(train[,4]), 
                    by = 0.1)
cutoffs <- rangedValues[which(predictions==max(predictions))]

y_hat <- ifelse(test[,4]>cutoffs[1], "virginica", "versicolor")
mean(y_hat == test$Species)



# 10 
# Use two predictors 
predictions <- foo(train[,3])
rangedValues <- seq(from = min(train[,3]), 
                    to = max(train[,3]), 
                    by = 0.1)
cutoff_1 <- rangedValues[which(predictions==max(predictions))]

predictions <- foo(train[,4])
rangedValues <- seq(from = min(train[,4]), 
                    to = max(train[,4]), 
                    by = 0.1)
cutoff_2 <- rangedValues[which(predictions==max(predictions))]

y_hat <- ifelse(test[,3]>cutoff_1[1] & test[,4]>cutoff_2[1], 
                "virginica", "versicolor")
mean(y_hat == test$Species)