# ML
Codes exploring machine learning methods with R. Based on Introduction to Data Science, a good guidebook for R. File marked with Ex are exercises, obtained from an Edx course developed around the book. <br/>
<br/>
Important ideas in machine learning: <br/>
    0. ML methods are built with data, and their main target is not to provide interpretation. <br/>
    1. Cross validatation: useful for fine tuing parameters of models. <br/>
    2) Train_set, test_set: the former is for model training, the latter is for model evaluation. Test_set should not be used in any stage of algorithm development. <br/>
    3) Let Y be predicting variable, X be predictor, then ultimate goal of ML is to find conditional probability Pr(Y = y | X = x). <br/>
    4) Evaluation: accuracy, F1_score, sensitivity & specificity. (eventually depending on training data quality and the goal of ML methods) <br/>
    5) Ensemble: make predictions with multiple methods, then take the (weighted) average. <br/>
    6) Regularisation: penalise large estimates formed using small samples. <br/>
<br/>
Basic Algorithms: <br/>
    1) Linear regression (baseline, serious ML should perform better) <br/>
    2) general linear regression (traditional way to include non-linear stuff) <br/>
    3) k-nearst neighbours, gamLoess (highly sensitive to training data)(gamLoess is just kNN with a normal kernel) <br/>
    4) naive Bayes (estimate conditional distribution of predictor, then Bayes' rule)(similar to general linear regression) <br/>
    5) QDA, LDA (a version of naive Bayes assuming conditional distribution of predctor is multi normal)(applicable only when predictors are few) <br/>
    6) decision tree / regression tree (basic for random forest, useful when there are hundreds or thousands of predictors)(each intermediate node of the tree is a classification, leafs of tree are the final results). <br/>
    7) random forest (multiple trees) <br/>
<br/>
Reducing dimension: <br/>
PCA <br/>
Factor analysis, SVD (essentially the same as PCA) <br/>
(useful for extracting explanatory variables from many predictors) <br/>
<br/>
<br/>
All are above are about supervised learning. <br/>
Unsupervised learning are known as clustering methods, including: <br/>
    1) k-means <br/>
    2) hierarchical clustering <br/>
    3) heat map <br/>
(not about predicting, but about different ways to partition dataset into different groups)



