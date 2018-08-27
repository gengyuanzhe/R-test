#AdaBoost for DNN to imporve the performance of classification ability

#--intput: 
#    Data: train set
#    E: leaning algorithm
#    nc: times of training or number of weak classifiers
#--procedure:
#    1. D[i] = 1/m,   i=1:m, m is count of training samples     # distribution of training sample weight
#    2. for t=1,....nc do 
#    3.   h[t] = E(Data, D[t])                                  # classifier from Data and D
#    4.   e[t] = P(h[t](x) != f(x)), x~D[t]                     # error rate for Distribution D
#    5.   if e[t] > 0.5 then break
#    6.   a[t] = 0.5*ln((1-e[t])/e[t])                          # weight of classifier
#    7.   D[t+1] = D[t]/e[t],            when h[t](x) != f(x)   # update distribution for next training
#    8.   D[t+1] = D[t]/(1-e[t]),        when h[t](x) == f(x)
#    9. end for
#--output:
#    final classifier: H(x) = sign(sum(a[t]h[t](x)))    #for two-class classify

#usage
#input: trainData, nc
#--trainData: prepare the training data for regression.
#  respVar | var1| var2| var3|...
#    10    | 1.1 | 1.1 | 5.4 |...
#    12    | 1.2 | 1.7 | 2.4 |...
#  Note: First column is a response variable, the other columns are explanatory variables.
#--nc: times of training or number of weak classifiers

library(caret)
library(mxnet)
library(gmodels)
rm(list=ls())

#set.seed(0)
nc = 1

acc_summary = NULL
dataset <- read.csv("data_min_divide_guanxinbing.csv", sep = ',', stringsAsFactors = FALSE)
dataset <- dataset[,-1]
dataset[["group"]] = factor(dataset[["group"]])
folds <- createFolds(dataset$group, k=5)

dataset <- data.matrix(dataset)
dataset <- dataset[,c('group', 'DCA', 'GLCA', 'UCA', 'TLCA')]
dataset[,2:ncol(dataset)] = log(dataset[,2:ncol(dataset)]+0.0000001, 10)
dataset[,2:ncol(dataset)] = scale(dataset[,2:ncol(dataset)], center = TRUE, scale = TRUE)

for(fold.num in 1:5){
  #input
  {
    trainData <- dataset[-folds[[fold.num]],]
    #trainData <- rbind(trainData, trainData[trainData[,1]==1 ,], trainData[trainData[,1]==1,], trainData[trainData[,1]==1,], trainData[trainData[,1]==1,])
    train.cnt <- nrow(trainData)
    testData <- dataset[folds[[fold.num]],]
    #testData <- rbind(testData, testData[testData[,1]==1, ], testData[testData[,1]==1, ], testData[testData[,1]==1,], testData[testData[,1]==1,])
    
    test.x <- testData[, -1]
    test.y <- testData[, 1]
  }
  
  #Initial parameters
  {
    D = rep(1/train.cnt, train.cnt)  #distribution of sample
    classifier.weight = NULL         #weight of each classifier, which is 'a' in the algorithm above
    accum.preds <- NULL              #store the testData predict result of each classifier
  }
  
  #AdaBoost DNN classifier
  i = 1
  
  while (i <= nc){
    #Bootstrap resampling
    {
      sample.index <- sample(train.cnt, train.cnt, replace = T, prob = D)
      train.x <- trainData[sample.index, -1]
      train.y <- trainData[sample.index, 1]
    }
    {
      train.x <- trainData[,-1]
      train.y <- trainData[,1]
    }
    
    #dnn 
    {
      data <- mx.symbol.Variable("data")
      fc1 <- mx.symbol.FullyConnected(data, name="fc1", num_hidden=64)
      act1 <- mx.symbol.Activation(fc1, name="relu1", act_type="relu")
      # dr1 <- mx.symbol.Dropout(act1, name="dr1")
      fc2 <- mx.symbol.FullyConnected(act1, name="fc2", num_hidden=32)
      act2 <- mx.symbol.Activation(fc2, name="relu2", act_type="relu")
      # dr2 <- mx.symbol.Dropout(act2, name="dr2")
      fc3 <- mx.symbol.FullyConnected(act2, name="fc3", num_hidden=6)
      net <- mx.symbol.SoftmaxOutput(fc3, name="sm")
      
      #cpu or gpu
      device.cpu <- mx.cpu()
      n.gpu <- 1
      device.gpu <- lapply(0:(n.gpu-1), function(i) {
        mx.gpu(i)
      })
      
      
      mx.set.seed(0)
      tic <- proc.time()
      model <- mx.model.FeedForward.create(
        net, 
        X=train.x,
        y=train.y-1,
        ctx=device.cpu, 
        num.round= 200, 
        array.batch.size=20, 
        learning.rate=0.07,                 #0.001 ~ 0.1
        momentum=0.9,                      #0.6 ~ 0.9
        #wd=0.00001,                        #or omit
        eval.metric=mx.metric.accuracy,    #eval.metric=mx.metric.rmse when regression,
        initializer=mx.init.uniform(0.09), #or omit 
        array.layout = "rowmajor",
        epoch.end.callback=mx.callback.log.train.metric(60))  #or 100
      
      print(proc.time() - tic)
    }
    
    #adaboost
    {
      #train
      preds <- predict(model, trainData[,-1], array.layout = "rowmajor")
      train.preds <- max.col(t(preds))
      err <- train.preds != trainData[,1]
      err.rate <- sum(as.numeric(err))/length(err)
      cat("err.rate=", err.rate)
      if(err.rate > 0.5){
        next
      }
      if(err.rate == 0){
        err.rate <- 0.00001
      }
      
      classifier.weight <- c(classifier.weight, log((1-err.rate)/err.rate)/2)
      D[err] <- D[err]/err.rate/2
      D[!err] <- D[!err]/(1-err.rate)/2
      
      #test
      preds <- predict(model, test.x, array.layout = "rowmajor")
      test.preds <- max.col(t(preds))
      accum.preds <- rbind(accum.preds, test.preds)
      #accum.preds[i,] <- test.preds
    }
    
    
    #remain time
    {
      pie.data <- c(i/nc, 1-i/nc)
      names(pie.data) <- c("done", "remain")
      pie(pie.data)
    }
    
    i <- i+1
  }
  
  #vote to get final results
  {
    myfunc <- function(x, weigh, func){
      aggregate(weigh, by=list(x), FUN=func)
    }
    aggr.preds <- apply(X = accum.preds, MARGIN = 2, FUN = myfunc, weigh=classifier.weight, func=sum)
    
    myfunc <- function(x){
      return (x$Group.1[order(x$x, decreasing = TRUE)[1]])
    }
    final.preds <- sapply(aggr.preds, myfunc)
    
    CrossTable(final.preds, test.y)
    acc.rate <- sum(final.preds == test.y) * 100 / length(test.y) 
    cat("acc.rate:", acc.rate, "%\n")
  }
  
  acc_summary = cbind(acc_summary, acc.rate)
  
}


print(acc_summary)

