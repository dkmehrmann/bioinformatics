setwd("~/Homework/STAT_530/Project/")
load(file='./data/init_data.RData')
set.seed(1738)

#### Naive Bayes ################################
library(e1071)

nbc <- naiveBayes(X.train, Y.train)
Yhat <- predict(nbc, X.test)

sum(Yhat != Y.test)/length(Y.test)
table(Yhat, Y.test)

#### LASSO ######################################
library(glmnet)
cvfit = cv.glmnet(X.train, Y.train, family = "binomial", type.measure = "auc")
plot(cvfit)

sum(coef(cvfit, s = "lambda.1se") != 0)

y.fit <- predict(cvfit, newx = X.train, s= "lambda.1se", type = "class")
sum(y.fit != Y.train)/length(Y.train)

y.pred <- predict(cvfit, newx = X.test, s= "lambda.1se", type = "class")
sum(y.pred != Y.test)/length(Y.test)

#### Random Forest ##############################
library(randomForest)
rf <- randomForest(X.train, Y.train, ntree=500, mtry=166)

y.fit.rf <- predict(rf, X.train)
sum(y.fit.rf != Y.train)/length(Y.train)

y.pred.rf <- predict(rf, X.test)
sum(y.pred.rf != Y.test)/length(Y.test)

rf$confusion
## Clearly, something is wrong