#########################################
# file  : 2d-gaussian.R
# auther: norih
# date  : Nov 26th, 2020
# Hotelling's T-squared distribution
# Two Dimension
#########################################
library(car)
library(tidyverse)
data(Davis)

# select weight and height data
data <- Davis %>% 
        select(c(weight, height)) %>% 
        data.matrix()

p <- ggplot(NULL, aes(x=data[,1], y=data[,2])) + geom_point()
ggsave(file = "./ref1/section2/fig2.5.png", plot=p)

mx <- colMeans(data)
Xc <- data - matrix(1, nrow(data), 1 ) %*% mx
Sx <- t(Xc) %*% Xc / nrow(Xc)

a <- rowSums((Xc %*% solve(Sx)) * Xc) #異常度
th <- qchisq(.99, 1)
p <- ggplot(NULL, aes(y=a, x = seq(1,length(a)))) + 
      geom_point(alpha = 1/5) + 
      geom_hline(yintercept = th, colour = "red", linetype = "dashed")

ggsave("./ref1/section2/fig.2.6.png", plot=p)
