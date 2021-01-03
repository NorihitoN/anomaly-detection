#########################################
# file  : 1d-gaussian.R
# auther: norih
# date  : Nov 26th, 2020
# Hotelling's T-squared distribution
# One Dimension
#########################################
library(car)
library(tidyverse)
data(Davis)

Davis %>% select(weight)

ggplot(Davis, aes(x=weight)) + geom_histogram()

mu <- mean(Davis$weight)
s2 <- mean((Davis$weight - mu)^2)
c(mu, s2)


# 異常度の計算
a <- (Davis$weight - mu)^2 / s2
th <- qchisq(.99, 1)
p <- ggplot(NULL, aes(y=a, x = seq(1,length(a)))) + 
      geom_point(alpha = 1/5) + 
      geom_hline(yintercept = th, colour = "red", linetype = "dashed")

ggsave(file = "./ref1/section2/fig2.3.png", plot=p)
