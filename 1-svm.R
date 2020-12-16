#########################################
# file  : 1-svm.R
# auther: norih
# date  : Dec 17th, 2020
# 1 class svm (support vector machine)
#########################################

library(kernlab)

x <- rbind(matrix(rnorm(120),ncol=2), matrix(rnorm(120, 4),ncol=2))
x <- scale(x)
rbf <- rbfdot(sigma=0.5)
ocsvm <- ksvm(x, type="one-svc", kernel=rbf, nu=0.1)

colorcode <- rep(0, nrow(x))
colorcode[ocsvm@alphaindex] <- 1 #支持ベクトルの色を1にセット
plot(x, pch=21, bg=colorcode)
