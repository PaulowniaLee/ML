# ML 
Codes exploring machine learning methods with R. Based on Introduction to Data Science, a good guidebook for R. File marked with Ex are exercises, obtained from an Edx course developed around the book. <br/>

## Important ideas in machine learning ##
1. ML methods are built with data, and their main target is not to provide interpretation. 
2. Cross validatation: useful for fine tuing parameters of models. 
3. Train_set, test_set: the former is for model training, the latter is for model evaluation. Test_set should not be used in any stage of algorithm development. 
4. Let Y be predicting variable, X be predictor, then ultimate goal of ML is to find conditional probability Pr(Y = y | X = x). 
5. Evaluation: accuracy, F1_score, sensitivity & specificity. (eventually depending on training data quality and the goal of ML methods) 
6. Ensemble: make predictions with multiple methods, then take the (weighted) average. 
7. Regularisation: penalise large estimates formed using small samples. 

## Basic Algorithms ##
1. Linear regression (baseline, serious ML should perform better) 
2. general linear regression (traditional way to include non-linear stuff) 
3. k-nearst neighbours, gamLoess (highly sensitive to training data)(gamLoess is just kNN with a normal kernel) 
4. naive Bayes (estimate conditional distribution of predictor, then Bayes' rule)(similar to general linear regression) 
5. QDA, LDA (versions of naive Bayes assuming conditional distribution of predctor is multi normal)(applicable only when predictors are few) 
6. decision tree / regression tree (basic for random forest, useful when there are hundreds or thousands of predictors)(each intermediate node of the tree is a classification, leafs of tree are the final results). 
7. random forest (multiple trees) 

## Dimension reduction ##
- PCA 
- Factor analysis, SVD (essentially the same as PCA) <br/>
(useful for extracting explanatory variables from many predictors) 

## Unsupervised learning ##
All are above are about supervised learning. <br/>
Unsupervised learning are known as clustering methods, including: 
1. k-means 
2. hierarchical clustering
3. heat map (a visualised way of partition data) <br/>
(not about predicting, but about different ways to partition dataset into different groups)



