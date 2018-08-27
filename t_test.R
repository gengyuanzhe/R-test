rm(list=ls())

dataset <- read.csv("data_min_divide_guanxinbing.csv", sep = ',', stringsAsFactors = FALSE)

group_ind1 <- dataset[,2]==0
group_ind2 <- dataset[,2]==1

p.t <- rep(NA, ncol(dataset)-2)
p.u <- rep(NA, ncol(dataset)-2)
for(j in 3:ncol(dataset)){
  x = dataset[group_ind1, j]
  y = dataset[group_ind2, j]
  psd.x = sqrt(sum((x - mean(x))^2)/(length(x)))  
  psd.y = sqrt(sum((y - mean(y))^2)/(length(y)))
  
  p.t[j-2] <- t.test(x, y)$p.value
  p.u[j-2] <- z.test(x, sigma.x=psd.x, y, sigma.y=psd.y)$p.value
}

fdr.t<-p.adjust(p.t,method="fdr",length(p.t))
fdr.u<-p.adjust(p.u,method="fdr",length(p.u)) 
write.csv(rbind(colnames(dataset)[c(-1,-2)], p.t, fdr.t, p.u, fdr.u), "guanxinbing_result2.csv")


# 对于每一列检验数据是否符合正态分布
# for (j in 3:ncol(dataset)){
# 
#   g1_res = shapiro.test(dataset[group_ind1, j])
#   print(colnames(dataset)[j])
#   print("group1:")
#   print(g1_res)
#   g2_res = shapiro.test(dataset[group_ind2, j])
#   print(colnames(dataset)[j])
#   print("group2:")
#   print(g2_res)
# }


# 
# # 方差齐性检验
# library(car)
# x <- c(134, 146, 104, 119, 124, 161, 107, 83, 113, 129, 97, 123,70, 118, 101, 85, 107, 132, 94)
# leveneTest(y = x, group=as.factor( cbind(t(rep(1,10)), t(rep(2,9)))

# T检验，要求两组样本符合正态分布，并且方差齐性
# for (j in 3:ncol(dataset)){
#   res <- t.test(dataset[group_ind1, j], dataset[group_ind2, j])
#   print(colnames(dataset)[j])
#   print(res$p.value)
# }
# 
# 
# # U检验，要求好像没那么多
# for (j in 3:ncol(dataset)){
#   res <- wilcox.test(dataset[group_ind1, j], dataset[group_ind2, j])
#   print(colnames(dataset)[j])
#   print(res$p.value)
# }