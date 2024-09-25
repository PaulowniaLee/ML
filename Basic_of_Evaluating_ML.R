#### Environment ####
library(tidyverse)
library(caret)

library(dslabs)
data(heights)

getwd()
setwd("/Users/mac/Documents/R_Notes/ML")
#####

#### Setup ####
# Outcomes and Predictors
y <- heights$sex
x <- heights$height

# Training and Test sets
set.seed(2024)
test_index <- createDataPartition(y, times = 1,
                                  p = 0.5, list = F)
# times: how many random samples of indexes to return 
# p: what proportion of the data is represented by index
# list: whether to returen the indexes as a list
test_set <- heights[test_index,]
train_set <- heights[-test_index,]

#####

#### Algorithm 1: Guess ####
y_hat <- sample(c("Male", "Female"), 
                length(test_index),
                replace = T) %>%
# factors: encode a vector as a factor 
# levels: an optional vector of the unique values that x might take
  # levels(): access the levels attribute of a variable 
  # 这里用sex就是说转换后的factor只取male,female
# convert to factor, following convention of ML in R
  factor(levels = levels(test_set$sex)) 
# sample takes a sample of specified size
# 相当于是从Male和Female里按同等概率抽,即乱猜

mean(y_hat == test_set$sex) 
# known as overall accuracy: 
# proportion that is predicted right

#####

#### Algorithm 2: use mean ####
# predict Male if height is within two sd from the average male
y_hat <- ifelse(x > 62, "Male", "Female") %>%
  factor(levels = levels(test_set$sex))
# x is our training set
mean(y == y_hat) # 0.793, better than guessing

# We should optimise the cutoff using only the training set 
# So we try different cutoffs (sd) to pick the best one
cutoff <- seq(61,70)
accuracy <- map_dbl(cutoff, function(x)
{
  y_hat <- ifelse(train_set$height > x, 
                  "Male", "Female") %>%
    factor(levels = levels(test_set$sex))
  mean(y_hat == train_set$sex) #注意我们只用了train_set来训练
})
# map: apply a function to each element of a vector 
# dbl: return a double vector 

plot(accuracy)
max(accuracy) # best accuracy is 0.84, within the train_set
best_cutoff <- cutoff[which.max(accuracy)]
best_cutoff # best cutoff is 64

y_hat <- ifelse(test_set$height > best_cutoff,
                "Male", "Female") %>%
  factor(levels = levels(test_set$sex))
mean(y_hat == test_set$sex)
# the accuracy in test_set is 0.813

#####

#### Caveat of Overall Accuracy ####
# Confusion matrix: overall accuracy can be deceptive 
table(predicted = y_hat,
      actual = test_set$sex)

test_set %>%
  mutate(y_hat = y_hat) %>%
  group_by(sex) %>%
  summarise(accuracy = mean(y_hat == sex))
# Note: we predicted half of female as male
# But, the dataset has too many male, so overall accuracy is high 
mean(y == "Male") # 0.773 male

#####

#### Confusion Matrix, Sensitivity & Specificity ####
# Presumption: we have a binary outcome
# Y = 1 positive, Y = 0 negative 

# High sensitivity: Y = 1 => ^Y = 1
# High specificity: Y = 0 => ^Y = 0

# Confusion Matrix 
#                   Actually Positive(Y=1)     Actually Negative(Y=0)
#Predicted Positive       TP                    FP     
#Predicted Negative       FN                    TN

#Sensitivity TP/(TP + FN) (predicted positive in all actual positive)
#Specificity TN/(TN + FP) (predicted negative in all actual negative)

#Precision TP/(TP+FP) (predicted positive in all actual positive)
#Depends on dataset (prevalence). 
#With more Positive, precision would be higher just by guessing

cm <- confusionMatrix(data = y_hat, reference = test_set$sex)
# when there are only two factor levels, the first level would 
# be used as the positive result 
cm$overall["Accuracy"]
cm$byClass[c("Sensitivity", "Specificity", "Prevalence")]
# more statistic are included

#####

#### One-number Summary: F1 Score ####
# 公式见p509

# use F1 score to optimise our algorithm 
cutoff <- seq(61,70)
F_1 <- map_dbl(cutoff, function(x)
  {
  y_hat <- ifelse(train_set$height > x, "Male", "Female") %>%
    factor(levels = levels(test_set$sex))
  F_meas(data = y_hat, reference = factor(train_set$sex))
})
# F_meas computes F1 Score, beta default as 1 (equal weights)
# beta (which weights over sensitivity to specificity)
plot(F_1)
max(F_1) #0.6496
best_cutoff <-cutoff[which.max(F_1)]
best_cutoff # best cutoff is now 66

y_hat <- ifelse(test_set$height > best_cutoff, "Male", "Female") %>%
  factor(levels = levels(test_set$sex))
sensitivity(data = y_hat, reference = test_set$sex)
specificity(data = y_hat, reference = test_set$sex)
# We see now both sensitivity and specificity are high

#####

#### ROC Receiver Operating Characteristic ####

# Guess by prevalence
n <- length(test_index)
probs <- seq(0,1, length.out = 10) #length.out: desired length of seq
guessing <- map_df(probs, function(p) #df: returning a data frame
{ 
  y_hat <- 
    sample(c("Male", "Female"), n, 
           replace = T, prob = c(p, 1-p)) %>%
    factor(levels = c("Female", "Male"))
  list(method = "Guessing",
       FPR = 1 - specificity(y_hat, test_set$sex),
       TPR = sensitivity(y_hat, test_set$sex))
})
remove(p)
# list function is used to generate 
# a list object containing different types of data

# F1 Score method
cutoffs <- c(50, seq(60, 75), 80)
height_cutoff <- map_df(cutoffs, function(x)
{
  y_hat <- ifelse(test_set$height > x, "Male", "Female") %>% 
    factor(levels = c("Female", "Male"))
  list(method = "Height cutoff",
       FPR = 1-specificity(y_hat, test_set$sex),
       TPR = sensitivity(y_hat, test_set$sex))
})

# Plot Together 
bind_rows(guessing, height_cutoff) %>%
  ggplot(aes(FPR, TPR, color = method)) +
  geom_line() +
  geom_point() +
  xlab("1 - Specificity") +
  ylab("Sensitivity")
# We see cutoff method performs better for all cases

#####

#### Precision - Recall Plot ####
# Note: In ROC, both specificity and sensitivity 
# are unrelated to prevalence
    # Introduce precision - recall plot

# Low Prevalence (positive result is female)
guessing <- map_df(probs, function(p){
  y_hat <- sample(c("Male", "Female"), length(test_index), 
                  replace = T, prob=c(p, 1-p)) %>% 
    factor(levels = c("Female", "Male"))
  list(method = "Guess",
       recall = sensitivity(y_hat, test_set$sex),
       precision = precision(y_hat, test_set$sex))
})

height_cutoff <- map_df(cutoffs, function(x){
  y_hat <- ifelse(test_set$height > x, "Male", "Female") %>% 
    factor(levels = c("Female", "Male"))
  list(method = "Height cutoff",
       recall = sensitivity(y_hat, test_set$sex),
       precision = precision(y_hat, test_set$sex))
})

bind_rows(guessing, height_cutoff) %>%
  ggplot(aes(recall, precision, color = method)) +
  geom_line() +
  geom_point()


# High Prevalence (positive result is male)
guessing <- map_df(probs, function(p){
  y_hat <- sample(c("Male", "Female"), length(test_index), 
                  replace = T, 
                  prob=c(p, 1-p)) %>% 
    factor(levels = c("Male", "Female"))
  list(method = "Guess",
       recall = sensitivity(y_hat, 
                            relevel(test_set$sex, "Male", "Female")),
       precision = precision(y_hat, 
                             relevel(test_set$sex, "Male", "Female")))
})

height_cutoff <- map_df(cutoffs, function(x){
  y_hat <- ifelse(test_set$height > x, "Male", "Female") %>% 
    factor(levels = c("Male", "Female"))
  list(method = "Height cutoff",
       recall = sensitivity(y_hat, 
                            relevel(test_set$sex, "Male", "Female")),
       precision = precision(y_hat, 
                             relevel(test_set$sex, "Male", "Female")))
})

bind_rows(guessing, height_cutoff) %>%
  ggplot(aes(recall, precision, color = method)) +
  geom_line() +
  geom_point()

#####





