#########################################
# file  : ridge.R
# auther: norih
# date  : Dec 22th, 2020
# ridge model is a linear model which a penalty is added to 
# because dependent values are normally has Multicollinearity
# so basic liner model cannot be solved. The determinant ~ zero. 
#########################################

library(MASS)
str(UScrime)
X <- UScrime[,-c(2,16)]
y <- UScrime[,16]
M <- ncol(X)
N <- length(y)
lambdas <- seq(0,5,length=50)
model <- lm.ridge(y ~.,cbind(X,y), lambda = lambdas)
bestIdx <- which.min(model$GCV)
coefs <- coef(model)[bestIdx,]
lam <- model$lambda[bestIdx]
ypred <- as.matrix(X) %*% as.matrix(coefs[2:15]) + coefs[1]
plot(y, ypred)

# 異常度の計算
sig2 <- (lam * sum(coefs[2:15]^2)) + sum((y - as.numeric(ypred))^2)/ N
X_ <- t(scale(X, scale = FALSE))
H <- t(X_) %*% solve(X_ %*% t(X_) + lam*diag(M), X_)
TrHN <- sum(diag(H)) / N
a <- (as.numeric(ypred) - y)^2/((1-TrHN)*sig2)
plot(a, xlab="index", ylab="anomaly score")
th <- sort(a)[N*(1-0.05)]
lines(0:50, rep(th,51), col="red", lty=2)
