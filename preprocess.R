rm(list=ls())

dataset <- read.csv("data.csv", sep = ',', stringsAsFactors = FALSE)

for (j in 3:ncol(dataset)){
  minv <- min(dataset[dataset[,j] > 0, j])
  for (i in 1:nrow(dataset)){
    if(dataset[i,j] < 0){
      dataset[i,j] <- minv/2
    }
  }
}

write.csv(dataset, "data_min_half.csv")