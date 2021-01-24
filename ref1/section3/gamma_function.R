#########################################
# file  : gamma_function.R
# auther: norih
# date  : Nov 30th, 2020
# Fitting by Gamma Function
# One Dimension
#########################################

library(car)
library(tidyverse)
library(MASS)
data(Davis)

N <- length(Davis$weight)
mu <- mean(Davis$weight)
si <- sd(Davis$weight) * (N-1)/N #標準偏差 sdは不偏標準偏差

# モーメント法によるGamma関数のkmo, smoを求める。
kmo <- mu^2 / si^2
smo <- si^2 / mu

# 最尤推定によるGamma関数のk, s値
# MASSパッケージのfitdistr 
ml <- fitdistr(Davis$weight, "gamma")
kml <- ml$estimate["shape"] #shape
sml <- 1/ml$estimate["rate"] #scale

p1 <- ggplot(Davis, aes(x = weight, after_stat(density))) +
   geom_histogram(color="black", fill="white") +
   geom_line(aes(x=weight, y=dgamma(weight, shape=kmo, scale=smo)), size = .5) + 
   geom_line(aes(x=weight, y=dgamma(weight, shape=kml, scale=sml)), size = .5, linetype="dashed")

a <- Davis$weight / smo - (kmo - 1) * log(Davis$weight / smo)

th <- order(a, decreasing = T)[0.01*N]

p2 <- ggplot(NULL, aes(y=a, seq(1, length(a)))) + 
  geom_point(alpha=0.4) + 
  geom_hline(yintercept = a[th], linetype = "dashed")

#install.packages("gridExtra")
library(gridExtra)
g <- gridExtra::grid.arrange(p1, p2, nrow = 1)

ggsave("./ref1/section3/fig3.1_gamma.png", plot = g)
