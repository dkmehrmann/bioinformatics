#### Install GEOquery ###########################

# source("http://www.bioconductor.org/biocLite.R")
# biocLite("GEOquery")

#### Load GEOquery and set WD ###################
library(GEOquery)
setwd("~/Homework/STAT_530/Project/")

#### Get the GSE and Metadata ###################
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

# remove rows with all missing reads
dim(df)
df <- df[which(rowSums(is.na(df)) != 41000),]
dim(df)

# remove columns with misssing values
df <- df[,which(colSums(is.na(df)) == 0)]
dim(df)
#### Split into Training and Test ###############

train <- subset(df, train_ind==T)
test <- subset(df, train_ind==F)

Y.train <- train$Y
Y.test <- test$Y

X.train <- train[,-c(27689,27690)]
X.test <- test[,-c(27689,27690)]

#### Normalization ##############################

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

## need to combine DFs to normalize. They didnt
## do this but predictions are terrible otherwise

X <- rbind(X.train, X.test)
X <- normalize(X)

# resplit the data
X.train <- X[1:176,]
X.test <- X[177:196,]

#### Save for Later and Clean Up ################
save(Y.train, Y.test, X.train, X.test, file="./data/init_data.RData")
rm(list=ls())
