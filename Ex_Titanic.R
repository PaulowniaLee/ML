#### Environment ####
library(titanic)
library(caret)
library(tidyverse)
library(rpart)

setwd("/Users/mac/Documents/R_Notes/ML")

options(digits = 3)

# clean the data - `titanic_train` is loaded with the titanic package
titanic_clean <- titanic_train %>%
  mutate(Survived = factor(Survived),
         Embarked = factor(Embarked),
         Age = ifelse(is.na(Age), median(Age, na.rm = TRUE), Age), 
         # NA age to median age
         FamilySize = SibSp + Parch + 1) %>%    
         # count family members
  select(Survived, Sex, Pclass, Age, Fare, 
         SibSp, Parch, FamilySize, Embarked)
#####




#### Naive Method ####
# 1 Split 
set.seed(42)
test_index <- createDataPartition(titanic_clean$Survived,
                                   times = 1,
                                   p = 0.2, list = F)
test_set <- titanic_clean[test_index,]
train_set <- titanic_clean[-test_index,]
mean(train_set$Survived == "1")

# 2 Guess
set.seed(3)
y_hat <- sample(c(0,1), 
                length(test_index),
                replace = T) %>%
  factor(levels = levels(test_set$Survived)) 
mean(y_hat == test_set$Survived)

# 3
train_set %>%
  group_by(Sex) %>%
  summarise(rate = mean(Survived == "1")) 

# 4 predict by sex
y_hat <- ifelse(test_set$Sex == "female",
                "1", "0") %>%
  factor(levels = levels(test_set$Survived))
mean(y_hat == test_set$Survived)

# 5 
train_set %>%
  group_by(Pclass) %>%
  summarise(rate = mean(Survived == "1"))

# 6 predict by class
y_hat <- ifelse(test_set$Pclass == "1",
                "1", "0") %>%
  factor(levels = levels(test_set$Survived))
mean(y_hat == test_set$Survived)

# 7 
train_set %>%
  group_by(Pclass, Sex) %>%
  summarise(rate = mean(Survived == "1"))

# 8 predict by sex and class
y_hat <- ifelse(test_set$Sex == "female" & 
                  test_set$Pclass %in% c("1", "2"),
                "1", "0") %>%
  factor(levels = levels(test_set$Survived))
mean(y_hat == test_set$Survived)

# 9 
y_hat <- ifelse(test_set$Sex == "female",
                "1", "0") %>%
  factor(levels = levels(test_set$Survived))
F_meas(y_hat, factor(test_set$Survived))
sexonly <- confusionMatrix(y_hat, test_set$Survived)

y_hat <- ifelse(test_set$Pclass == "1",
                "1", "0") %>%
  factor(levels = levels(test_set$Survived))
F_meas(y_hat, factor(test_set$Survived))
classonly <- confusionMatrix(y_hat, test_set$Survived)

y_hat <- ifelse(test_set$Sex == "female" & 
                  test_set$Pclass %in% c("1", "2"),
                "1", "0") %>%
  factor(levels = levels(test_set$Survived))
F_meas(y_hat, factor(test_set$Survived))
sex_class <- confusionMatrix(y_hat, test_set$Survived)

#####

#### ML ####
# 7
library(gam)
set.seed(1)
fit <- train(Survived ~ Fare, data = train_set,
             method = "gamLoess")
y_hat <- predict(fit, test_set)
confusionMatrix(y_hat, test_set$Survived)$overall["Accuracy"]

# 8
set.seed(1)
fit <- train(Survived ~ Age, data = train_set,
             method = "glm")
y_hat <- predict(fit, test_set)
confusionMatrix(y_hat, test_set$Survived)$overall["Accuracy"]

set.seed(1)
fit <- train(Survived ~ Age + Sex + Pclass + Fare, 
             data = train_set,
             method = "glm")
y_hat <- predict(fit, test_set)
confusionMatrix(y_hat, test_set$Survived)$overall["Accuracy"]

set.seed(1)
fit <- train(Survived ~ ., 
             data = train_set,
             method = "glm")
y_hat <- predict(fit, test_set)
confusionMatrix(y_hat, test_set$Survived)$overall["Accuracy"]

# 9 
set.seed(6)
fit <- train(Survived ~., data = train_set,
             method = "knn",
             tuneGrid = data.frame(k = seq(3,51,2)))
fit$finalModel
plot(fit)
y_hat <- predict(fit, test_set)
confusionMatrix(y_hat, test_set$Survived)$overall["Accuracy"]

# 10 
set.seed(8)
fit <- train(Survived ~., data = train_set,
             method = "knn",
             tuneGrid = data.frame(k = seq(3,51,2)),
             trControl = trainControl(method = "cv",
                                      number = 10,
                                      p = 0.1))
fit$finalModel
y_hat <- predict(fit, test_set)
confusionMatrix(y_hat, test_set$Survived)$overall["Accuracy"]

# 11
set.seed(10)
fit <- train(Survived ~., data = train_set,
             method = "rpart",
             tuneGrid = data.frame(cp = seq(0,0.05,0.002)))
fit$bestTune
y_hat <- predict(fit, test_set)
confusionMatrix(y_hat, test_set$Survived)$overall["Accuracy"]

plot(fit$finalModel, margin = 0.1)
text(fit$finalModel, cex = 0.75)
# Note that going down to the left node means yes

# 12
set.seed(14)
fit <- train(Survived ~., data = train_set,
             method = "rf",
             ntree = 100,
             tuneGrid = data.frame(mtry = seq(1:7)))
# ntree: Number of trees 
# mtry: Number of variables to randomly sample as candidates at each split
fit$bestTune
y_hat <- predict(fit, test_set)
confusionMatrix(y_hat, test_set$Survived)$overall["Accuracy"]

#####