#### Environment ####
library(tidyverse)
library(dslabs)
library(matrixStats)

setwd("/Users/mac/Documents/R_Notes/ML")

data("movielens")
data("tissue_gene_expression")

#####

#### Data Construction ####
top <- movielens %>%
  group_by(movieId) %>%
  summarize(n=n(), title = first(title)) %>% 
  top_n(50, n) %>%
  pull(movieId)

x <- movielens %>%
  filter(movieId %in% top) %>% group_by(userId) %>%
  filter(n() >= 25) %>%
  ungroup() %>%
  dplyr::select(title, userId, rating) %>% 
  pivot_wider(names_from = "userId", values_from = "rating")

row_names <- str_remove(x$title, ": Episode") %>% 
  str_trunc(20) 
x <- x[,-1] %>% as.matrix()
x <- sweep(x, 2, colMeans(x, na.rm = TRUE))
x <- sweep(x, 1, rowMeans(x, na.rm = TRUE))
rownames(x) <- row_names

#####

#### Clustering Algorithms ####
# all previous algorithms are supervised learning: 
#     use outcomes in a training set to supervise 
#     the creation of prediction algorithm

# unsupervised learning: 
#     unknown outcome, interested in discovering groups 
# predictors are used to define clusters (hence clustering algorithms)
# can be powerful as an exploratory tool 

# general idea 
# 1. define a distance between observations or groups of observations 
# (point distance and set distance)
# 2. decide how to join observations into clusters 

#####

#### Hierarchical Clustering ####

# idea: 
# 1. initially every observation is a group 
# 2. at each step, join pairs of closest groups 
# 3. repeat until there is only one group 

d <- dist(x) # define distance 
# dist: compute the distances between the rows of a data matrix 
# 所以这里是关于movie的区别
h <- hclust(d) # implement the algorithm 

plot(h, cex = 0.65, main = "", xlab = "") # show the hierarchy with graph 
# interpretation:
# distance: find the first location from top to bottom 
# that the movies split into two different groups. 
# The height of this location is the distance between the groups.

# to generate actual groups, we can 
# 1. set a minimum distance needed for observations to be in the same group 
# or 2. set the number of groups we want and then find the required minimum
groups <- cutree(h, k = 10)
# cutree: take output of hclust, perform either of above two 
# k: number of groups 
# h: set minimum distance 
groups[groups == 4]



# we can also explore clusters of raters 
h_2 <- dist(t(x)) %>% hclust()
# distance取transpose，就是column之间的distance，即rater差别
plot(h_2, cex = 0.65, main = "", xlab = "")
groups <- cutree(h_2, k = 5)
groups[groups == 2]


#####

#### k-means ####

# idea:
# 0. pre-define k, the number of clusters we want 
# 1. define k centres
# 2. each observation is assigned to the cluster with the closet centre 
# 3. redifine centres using observations in each cluster 
# repeat 2 and 3, until centres converge 

x_0 <- x
x_0[is.na(x_0)] <- 0 # base R kmeans does not handle NA
k <- kmeans(x_0, centers = 10)

groups <- k$cluster # 提取最终分类

# Note: first centre is chosen at random, so final results are random as well
# we can repeat the entire function and take the average 
# we can also assign the starting value 
k <- kmeans(x_0, centers = 10, nstart = 25)
# centers: either the number of centers, or a set of initial cluster centers
# nstart: if centers is a number, how many random starting sets to be chosen
# (即重复几次clustering)

#####

#### Heatmaps #### 
x <- sweep(tissue_gene_expression$x, 2, colMeans(tissue_gene_expression$x))
# 2 means by column, 每列的值都减去每一列的平均
h_1 <- hclust(dist(x))
h_2 <- hclust(dist(t(x))) 
# 找到列和行的cluster
image(x[h_1$order, h_2$order])
# 画heatmap

# heatmap function directly do this 
heatmap(x, col = RColorBrewer::brewer.pal(11, "Spectral"))



# However, these maps are too noisy 
# idea: remove features with no information
# that is, include only those with high variance 

ncol(x) # x有500列
sds <- colSds(x, na.rm = T)
o <- order(sds, decreasing = T)[1:25] #按sd大小排序
heatmap(x[,o], #只选sd最高的25列
        col = RColorBrewer::brewer.pal(11, "Spectral") #选配色方案
        )
# Final graph
# red, high value (distance); blue, low value (distance)
# blocks of similar color represent a cluster 

#####

#### Ex ####

# 1 remove row means, compute distance between observations
x <- sweep(tissue_gene_expression$x, 1, rowMeans(tissue_gene_expression$x))
d <- dist(t(x))
# columns now represent different observations

# 2. hierarchical clustering
h <- hclust(d) 
plot(h, cex = 0.25, main = "", xlab = colnames(x))
# impossible to read

# 3. k-means clustering
k <- kmeans(x, centers = 7, nstart = 25)
groups <- k$cluster

sum(is.na(
  match(names(groups), rownames(x))
  ))
# we match clusters identified by algorithms, with actual tissue types
# perfect match: we get the true clusters 

# 4. heatmap for 50 most variable genes 
ncol(x) # x有500列
sds <- rowSds(x, na.rm = T)
o <- order(sds, decreasing = T)[1:50] #按sd大小排序
heatmap(x[,o], #只选sd最高的25列
        col = RColorBrewer::brewer.pal(11, "RdBu") #选配色方案
)
# 不同observation之间确实区别很大









