rm(list=ls())

#### Load GEOquery and set WD ###################
library(GEOquery)
library(caret)
setwd("~/Homework/STAT_530/Project/")
set.seed(1738)

#### Get the GSE and Metadata ###################
gse <- getGEO('GSE29210', destdir="./raw")
gse <- gse[[1]]
mtx <- exprs(gse)
p <- pData(gse)

#### Clean Data #################################
# dependent variable
Y <- p$characteristics_ch1.1

# Random training or test, 150 train, 49 test
train_rows <- sample(1:199, 150, replace=F)
train_ind <- 1:199 %in% train_rows

# make big DF
df <- data.frame(t(mtx), Y, train_ind)

# remove rows with all missing reads
df <- df[which(rowSums(is.na(df)) != 41000),]

# remove columns with misssing values
df <- df[,which(colSums(is.na(df)) == 0)]

train <- subset(df, train_ind==T)
test <- subset(df, train_ind==F)

Y.train <- train$Y
Y.test <- test$Y

X.train <- as.matrix(train[,-c(27689,27690)])
X.test <- as.matrix(test[,-c(27689,27690)])

dim(X.train)
dim(X.test)

#### LASSO ######################################
library(glmnet)
cvfit = cv.glmnet(X.train, Y.train, family = "binomial", type.measure = "auc")

sum(coef(cvfit, s = "lambda.1se") != 0)

y.fit <- predict(cvfit, newx = X.train, s= "lambda.1se", type = "class")
sum(y.fit == Y.train)/length(Y.train)

y.pred <- predict(cvfit, newx = X.test, s= "lambda.1se", type = "class")
sum(y.pred == Y.test)/length(Y.test)

confusionMatrix(y.pred, Y.test, positive = "er status: positive")

#### Random Forest ##############################
library(randomForest)
rf <- randomForest(X.train, Y.train, ntree=500)

y.fit.rf <- predict(rf, X.train)
sum(y.fit.rf == Y.train)/length(Y.train)

y.pred.rf <- predict(rf, X.test)
sum(y.pred.rf == Y.test)/length(Y.test)

# plot(rf$err.rate[,'OOB'], type='l')
# varImpPlot(rf, n.var=15)

rf$confusion
confusionMatrix(y.pred.rf, Y.test, positive = "er status: positive")
#### GBM ########################################
library(xgboost)
cv.res <- xgb.cv(data = as.matrix(df[,-c(27689,27690)]), 
                 label = as.numeric(df$Y)-1, nfold = 3, eta=0.05, max_depth=5,
                 nrounds = 25, objective = "binary:logistic")


#### Plotting ###################################
library(ggplot2)
cv_df <- data.frame(mean=c(cv.res$train.error.mean, cv.res$test.error.mean),
                    sd = c(cv.res$train.error.std, cv.res$test.error.std),
                    Dataset = c(rep('train', nrow(cv.res)), rep('test',nrow(cv.res))),
                    iter=rep(1:nrow(cv.res),2))
p1 <- ggplot(data = cv_df, aes(x=iter,y = mean, group=Dataset, color=Dataset))+ 
      geom_ribbon(aes(ymin = mean - sd, ymax = mean + sd, fill=Dataset), alpha = 0.3, colour=NA) + 
      geom_line(size = 1) + labs(x='Iteration', y="Error", title='Gradient Boosting CV Error')

p1


png('paper/xgb.png', width = 480, height = 300)
p1
dev.off()

png('paper/lasso.png', width = 480, height = 300)
plot(cvfit)
dev.off()

#### Citations ##################################
for (p in c("GEOquery", "glmnet", "xgboost", "randomForest", "ggplot2")){
  print(citation(package = p))
}

