#### Environment ####
library(tidyverse)
library(MASS)
library(dslabs)
library(caret)

setwd("/Users/mac/Documents/R_Notes/ML")
#####

#### Principal Component Analysis: Ideas ####

# useful for exploratory analysis 
# idea: reducing dimension while preserving important characteristics 



# generate bivariate normal data: example with twin heights
# 100 two-dimensional points, each point is a pair of twins 
# value: number of standard deviations each individual is from the mean
set.seed(1988)
n <- 100
Sigma <- matrix(c(9, 9*0.9, 9*0.92, 9*1), 2, 2)
x <- rbind(mvrnorm(n / 2, c(69, 69), Sigma),
           mvrnorm(n / 2, c(55, 55), Sigma))
plot(x[,1], x[,2])
# high correlation (between twins)
# two groups of data (adults and children)

# we want to reduce the dimension to one, 
# while still keeping important features 
# we start by preserving distances between any two points 

z <- cbind((x[,2] + x[,1]) / 2, x[,2] - x[,1]) # a transformation of x
plot(z[,1], z[,2])
# we see the avarage of two entries actually explains distance well
# -> we use avarage as one dimension proxy for two dimensional points
sd(dist(x) - dist(z[,1]) * sqrt(2) )
# sd is small: reduced version is not very much different from original 
cor(x[,1], x[,2])
# Remark: reason for success is high correlation between two values 
# which means actually one column would suffice 

# What we did here is actually first principal component of x

#####

#### Implement PCA ####

# Theory 
# 1. PCA: find a linear orthogonal transformation, such that the resulting 
#    matrix have columns listed with decreasing variablity 
#    即：first column has highest variability, second column the second, 
#    third column the third, ..., above k columns almost do not vary, 
#    thus not contributing much to distance 
# 2. We can approxmiate distance between points with first k columns
#    (即从n dimension 变成 k dimension)
# 3. This operation works for matrix of any dimension 
# 4, Useful when k < < n


pca <- prcomp(x) # implement PCA
pca$x # new matrix 
pca$rotation # rotation needed to transform x
# orthogonal rotation:
# transform x but preserving the distance between points 

# PCA is basically taking the average then divide root2
a <- sweep(x, 2, colMeans(x)) %*% pca$rotation
b <- pca$x 
max(abs(a-b))
# 这里的转化对于training model 很重要，因为我们必须用相同的方法处理
# train和test的feature

#####

#### Iris Example ####
x <- iris[,1:4] %>%
  as.matrix()
# 4 botanical measurements related to 3 flower species
cor(x)
# 3 measurements are highly related 
# => PCA should be a good approximation (in dimension 2)

pca <- prcomp(x)
summary(pca)
# 可见first two columns accounts for 97.8% of variability 
data.frame(pca$x[,1:2], Species = iris$Species) %>%
  ggplot(aes(PC1, PC2, fill = Species)) +
  geom_point(cex = 3, pch = 21) +
  coord_fixed(ratio = 1)
# PC1 and PC2 have distinguished species successfully 
d_approx <- dist(pca$x[,1:2])
d <- dist(x)
qplot(d, d_approx) + geom_abline(color = "violet")
# also distance is largely preserved after PCA

# We have visualized 4D data in 2D graph

#####

#### MNIST Example ####
if(!exists("mnist")) mnist <- read_mnist()
# want to reduce dimensions for pixel data 
# should be possible as proximate pixels are correlated 

pca <- prcomp(mnist$train$images)
# we can see PCA actually works 
pc <- 1:ncol(mnist$test$images)
qplot(pc, pca$sdev)
# sd drops quickly as ncol increases 



k <- 36 # use only first 36 dimensions
x_train <- pca$x[,1:k]
y <- factor(mnist$train$labels)
fit <- knn3(x_train, y)

col_means <- colMeans(mnist$test$images)
x_test <- sweep(mnist$test$images, 2, col_means) %*% pca$rotation
# treat test set features the same way as train set 
x_test <- x_test[,1:k]

y_hat <- predict(fit, x_test, type = "class")
confusionMatrix(y_hat, factor(mnist$test$labels))$overall["Accuracy"]
# accuracy 0.9748, very impressive as we only used 36 out of 784

#####



