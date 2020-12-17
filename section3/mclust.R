#########################################
# file  : mclust.R
# auther: norih
# date  : Dec 16th, 2020
# clustering based on finite normal mixture 
# modeling by EM algorithm. 
# Used mclust package.
#########################################

library(mclust); library(car)
X <- Davis[-12, c("weight", "height")]
result <- Mclust(X)
print(summary(result, parameters = TRUE))
plot(result)

pi <- result$parameters$pro
Y <- Davis[,c("weight", "height")]
YY <- cdens(modelName = result$modelName, Y, parameters = result$parameters)
a <- -log(as.matrix(YY) %*% as.matrix(pi))
plot(a, ylab="anomaly score")
