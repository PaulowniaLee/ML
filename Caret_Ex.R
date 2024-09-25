#### Environment ####
library(tidyverse)
library(dslabs)
library(caret)

getwd()
setwd("/Users/mac/Documents/R_Notes/ML")

#####


#### Ex 1 ####
set.seed(1996)
n <- 1000
p <- 10000
x <- matrix(rnorm(n*p), n, p)
colnames(x) <- paste("x", 1:ncol(x), sep = "_")
y <- rbinom(n, 1, 0.5) %>% factor()

x_subset <- x[ ,sample(p, 100)]

# 2
pvals <- rep(0, ncol(x))
for (i in 1:ncol(x)) {
  pvals[i] <- t.test(x[,i][y==0], x[,i][y==1], var.equal=TRUE)$p.value
}
sum(pvals <= 0.01)

# 3
set.seed(1)
x_subset <- x[,which(pvals <= 0.01)]
fit <- train(x_subset, y, method = "glm")
fit$results

#####

#### Ex 2 ####
library(rpart)
n <- 1000
sigma <- 0.25
set.seed(1)
x <- rnorm(n, 0, 1)
y <- 0.75 * x + rnorm(n, 0, sigma)
dat <- data.frame(x = x, y = y)

fit <- rpart(y ~., data = dat)
plot(fit, margin = 0.1)
text(fit, cex = 0.75)

library(randomForest)
fit <- randomForest(y ~ x, data = dat) 
plot(fit)

#####

#### Ex 3 ####
data("tissue_gene_expression")
library(rpart)

set.seed(1991)
fit <- with(tissue_gene_expression, 
            train(x, y, method = "rpart",
                  tuneGrid = data.frame(cp = seq(0, 0.1, 0.01))))
confusionMatrix(fit)    
ggplot(fit)

set.seed(1991)
fit <- with(tissue_gene_expression, 
            train(x, y, method = "rpart",
                  tuneGrid = data.frame(cp = seq(0, 0.1, 0.01))),
            control = rpart.control(minsplit = 0))
confusionMatrix(fit)

plot(fit$finalModel, margin = 0.1)
text(fit$finalModel, cex = 0.75)

# randomForest method 
set.seed(1991)
fit <- with(tissue_gene_expression,
            train(x, y, method = "rf",
                  nodesize = 1,
                  tuneGrid = data.frame(mtry = seq(50,200,25))))
fit$finalModel$mtry

#####


