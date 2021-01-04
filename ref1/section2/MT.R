#########################################
# file  : MT.R
# auther: norih
# date  : Nov 26th, 2020
# Maharanobis-Taguchi System
#########################################
library(MASS)
library(tidyverse)

p <- ggplot(road, aes(y=rownames(road), x=deaths)) + 
  geom_bar(stat = "identity")
print(p)

X <- road / road$drivers
X <- as.matrix(log(X[,-2] + 1))
mx <- colMeans(X)
Xc <- X - matrix(1, nrow(X), 1) %*% mx
Sx <- t(Xc) %*% Xc / nrow(X)
a <- rowSums((Xc %*% solve(Sx)) * Xc) / ncol(X)
p1 <- ggplot(NULL, aes(y=a, x = seq(1,length(a)))) +
      geom_point(alpha = 1/5) +
      geom_hline(yintercept = 1, colour = "red", linetype = "dashed")

xc_prime <- Xc["Calif",]
SN1 <- 10*log10(xc_prime^2/diag(Sx))

p2 <- ggplot(NULL, aes(x=reorder(names(SN1), SN1), y=SN1, fill = SN1)) + 
  geom_bar(stat = "identity")

#install.packages("gridExtra")
library(gridExtra)
g <- gridExtra::grid.arrange(p1, p2, nrow = 1)

ggsave("./ref1/section2/fig2.6.png", plot = g)
