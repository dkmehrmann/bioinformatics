setwd("~/Homework/STAT_530/Project/")
library(GEOquery)
#### Reproduce the Paper ########################
gse <- getGEO('GSE29210', destdir="./raw")
gse <- gse[[1]]
mtx <- exprs(gse)
p <- pData(gse)

#### Clean Data #################################
# dependent variable
Y <- p$characteristics_ch1.1

# Training or test
train_ind <- p$characteristics_ch1.2 == "set: training"

# make big DF
df <- data.frame(t(mtx), Y, train_ind)

# some of the necessary values are NA
# we take only the ones for which is is finite
df <- df[is.finite(df[,'A_32_P104334']),]
X <- df[,-c(41001,41002)]
Y <- df$Y

dim(X)
# and there are only 20 rows left...hmm

#### Normalize ##################################
# every value less than 0.1 is now 0.1
n1 <- function(dataframe){
  d <- dataframe
  d[d<0.1]=0.1
  return(d)
}

# divide each row by the median
n2 <- function(dataframe){
  d <- data.frame(t(apply(dataframe, 1, function(x) x-median(x))))
  return(d)
}

# divide each column by the median
n3 <- function(dataframe){
  d <- apply(dataframe, 2, function(x) x-median(x))
  return(d) 
}

# subtract from each column its mean and divide by sd
n4 <- function(dataframe){
  d <- apply(dataframe, 2, function(x) (x-mean(x))/sd(x))
  return(d)
}

# run them all
normalize <- function(dataframe){
  d <- n4(n3(n2(n1(dataframe))))
  return(d)
}

X <- normalize(X)
X <- data.frame(X)

#### Run their Classifier #######################

# A_32_P104334 = AW97281
# A_23_P372234 = CA12
# A_23_P75056 = GATA3

their_model <- function(AW, CA, GA){
  val = 0.2466*AW - 1.2934*CA - 2.2165*GA
  return(val < 1.8993)
}

get_prediction <- function(line){
  AW = line$A_32_P104334
  CA = line$A_23_P372234
  GA = line$A_23_P75056
  return(their_model(AW,CA,GA))
}

Yhat <- as.numeric(get_prediction(X))
Y <- as.numeric(Y) - 1

#### View the Predictions #######################
Yhat
Y

# accuracy
sum(Y == Yhat)/length(Y)

# probability of getting that or better by random guessing
1-pbinom(19,20,0.5)

