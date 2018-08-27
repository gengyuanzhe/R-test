library(caret)
library(kernlab)
library(gmodels)
rm(list=ls())


dataset <- read.csv("data_min_divide.csv", sep = ',', stringsAsFactors = FALSE)
dataset <- dataset[,-1]
dataset <- data.matrix(dataset)
# dataset <- dataset[,c('group', 'DCA', 'GLCA', 'UCA', 'TLCA')]
# dataset[,2:ncol(dataset)] = log(dataset[,2:ncol(dataset)]+0.0000001, 10)
dataset[,2:ncol(dataset)] = scale(dataset[,2:ncol(dataset)], center = TRUE, scale = TRUE)
dataset <- as.data.frame(dataset)
dataset[["group"]] = factor(dataset[["group"]])

folds <- createFolds(dataset$group, k=5)


for (fold.num in 1: 5){
  training <- dataset[-folds[[fold.num]],]
  testing <- dataset[folds[[fold.num]],]
  m <- ksvm(group~., data = training, kernel="rbfdot", C = 1,kpar = list(sigma=0.05))
  tp <- predict(m, training[,-1])
  CrossTable(tp, training[,1])
  print(prop.table(table(tp == training[,1])))
  # p <- predict(m, as.matrix(testing[,-1]))
  # CrossTable(p,testing[,1])
  # print(prop.table(table(p == testing[,1])))
}
