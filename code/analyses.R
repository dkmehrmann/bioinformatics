setwd("~/Homework/STAT_530/Project/")
load(file='./data/init_data.RData')


#### Naive Bayes ################################
library(klaR)

nbc <- NaiveBayes(X, as.factor(Y))
Yhat <- predict(nbc, X.test)

Y
as.numeric(Yhat$class)-1

#### Thresholded Naive Bayes ####################


#### LASSO ######################################


#### Others? ####################################
# SVM, trees, neural net? bootstrapping