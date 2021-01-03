#########################################
# file  : pca.R
# auther: norih
# date  : Dec 20th, 2020
# pca(principal component analysis) 
# reduces the dimensionality of the data 
# while retaining most of the variation in the data set.
#########################################

library(MASS)
library(ggplot2)
# Cars93 is dataset of automotive.
# N: 93 dataset 
# M: 27 features

str(Cars93)

cc <- c("Min.Price", "Price", "Max.Price", "MPG.city", "MPG.highway",
        "EngineSize", "Horsepower", "RPM", "Rev.per.mile", "Fuel.tank.capacity",
        "Length", "Wheelbase", "Width", "Turn.circle", "Weight")
mask <- is.element(colnames(Cars93), cc)
Xc <- t(scale(Cars93[,mask]))

colnames(Xc) <- t(Cars93[,"Make"])

# N: 93
# M: 15
# 散布行列(15x15)から固有値を解く
S <- Xc %*% t(Xc); evd <- eigen(S)
plot(evd$values, type="b", xlab="index", ylab="eigenvalue")

m <- 2
x2 <- t(evd$vectors[,1:m]) %*% Xc #eigen-vector1, 2ではられる空間へのXcの写像

#Xcの1つのデータに対する異常度
xtemp <- Xc[,1]; x2temp <- x2[,1]
a <- sum(xtemp * xtemp) - sum(x2temp * x2temp) 

#Xcすべてのデータに対する異常度
a1 <- colSums(Xc*Xc) - colSums(x2*x2)
idx <- order(a1, decreasing = T)[1:6]; print(a1[idx])
color <- rep(1,93); color[idx] = 2
ggplot(NULL, aes(x = x2[1,], y = x2[2,])) +
              geom_point(shape=1,size=10,color=color) +
              xlim(-8,8) + ylim(-8,8) + labs(title="Cars93 PCA distribution", x="first eigen", y="second eigen") +
              geom_text(aes(label=seq(1:93)))#,hjust=0, vjust=0)
    

# Method.2)グラム行列による固有値分解
G <- t(Xc) %*% Xc; evd.g <- eigen(G)
Lam.12 <- diag(evd.g$values[1:m]^{-1/2})
# Lam.12 %*% t(evd.g$vectorrs[,1:m]) %*% t(Xc) の１列目は、evd$vectors[,1]に等しい
xx2 <- Lam.12 %*% t(evd.g$vectors[,1:m]) %*% t(Xc) %*% Xc #xx2はx2に等しい
aa1 <- colSums(Xc*Xc) - colSums(xx2*xx2)
idx.g <- order(aa1, decreasing = T)[1:6]
print(a1[idx])
print(aa1[idx.g]) # result of method1 and method2 is the same
