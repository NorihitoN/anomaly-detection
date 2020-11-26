#########################################
# file  : MT.R
# auther: norih
# date  : Nov 26th, 2020
# Maharanobis-Taguchi System
#########################################
library(MASS)
library(tidyverse)
road
rownames(road)

ggplot(road, aes(y=rownames(road), x=deaths)) + 
  geom_bar(stat = "identity")

X <- road / road$drivers
X <- as.matrix(log(X[,-2] + 1))
mx <- colMeans(X)
Xc <- X - matrix(1, nrow(X), 1) %*% mx
Sx <- t(Xc) %*% Xc / nrow(X)
a <- rowSums((Xc %*% solve(Sx)) * Xc) / ncol(X)

# Davis %>% select(weight)
# 
# ggplot(Davis, aes(x=weight)) + geom_histogram()
# 
# mu <- mean(Davis$weight)
# s2 <- mean((Davis$weight - mu)^2)
# c(mu, s2)
# 
# 
# # 異常度の計算
# a <- (Davis$weight - mu)^2 / s2
# th <- qchisq(.99, 1)
# ggplot(NULL, aes(y=a, x = seq(1,length(a)))) + 
#       geom_point(alpha = 1/5) + 
#       geom_hline(yintercept = th, colour = "red", linetype = "dashed")
