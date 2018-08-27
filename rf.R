library(caret)
library(randomForest)

rm(list=ls())


dataset <- read.csv("data_min_divide_guanxinbing.csv", sep = ',', stringsAsFactors = FALSE)
dataset <- dataset[,-1]
dataset <- data.matrix(dataset)

dataset <- dataset[,c('group', 'DCA', 'GLCA', 'UCA', 'TLCA')]


dataset[,2:ncol(dataset)] = log(dataset[,2:ncol(dataset)]+0.0000001, 10)
dataset[,2:ncol(dataset)] = scale(dataset[,2:ncol(dataset)], center = TRUE, scale = TRUE)
dataset <- as.data.frame(dataset)
dataset[["group"]] = factor(dataset[["group"]])
fold <- createFolds(y = dataset$group,k = 5)


for (fold.num in 1:5){
  training <- dataset[-fold[[fold.num]],]
  testing <- dataset[fold[[fold.num]],]
  m <- randomForest(group ~ ., data=training, importance=TRUE, proximity=TRUE)
  p <- predict(m, as.matrix(testing[,-1]))
  
  library(gmodels)
  CrossTable(p, testing[,1])
  
  print(prop.table(table(p == testing[,1])))
}
