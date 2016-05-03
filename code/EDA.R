setwd("~/Homework/STAT_530/Project/")
load(file='./data/init_data.RData')

# Size of the data
dim(X.train)
dim(X.test)

# Class balance
# The paper found 63.3% of the E176 group, and 60.9% of the E23 group to be ER+
table(Y.train)/length(Y.train)
table(Y.test)/length(Y.test)

summary(X.train[,3])
