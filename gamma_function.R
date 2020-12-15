#########################################
# file  : gamma_function.R
# auther: norih
# date  : Nov 30th, 2020
# Fitting by Gamma Function
# One Dimension
#########################################

library(car)
library(tidyverse)
data(Davis)

N <- length(Davis$weight)
mu <- mean(Davis$weight)
si <- sd(Davis$weight) * (N-1)/N #標準偏差 sdは不偏標準偏差
# モーメント法によるGamma関数のkmo, smoを求める。
kmo <- mu^2 / si^2
smo <- si^2 / mu

# 最尤推定によるGamma関数のk, s値
ml <- fitdistr(Davis$weight, "gamma")
kml <- ml$estimate["shape"]
sml <- 1/ml$estimate["rate"]

a <- Davis$weight / smo - (kmo - 1) * log(Davis$weight / smo)

th <- order(a, decreasing = T)[0.01*N]

ggplot(NULL, aes(y=a, seq(1, length(a)))) + 
  geom_point(alpha=0.4) + 
  geom_hline(yintercept = a[th], linetype = "dashed")

# ggplot(Davis, aes(x = weight, after_stat(density))) + 
#   geom_histogram() +
#   geom_line(aes(x=Davis$weight, y=dgamma(Davis$weight, kmo, smo)), color="red", size = 1) 
# 
# ggplot(NULL, aes(x=seq(40, 200), y=dgamma(seq(40, 200), kmo, smo))) + 
#   geom_point()
# 
# seq(40,200)
ggplot(NULL, aes(x = seq(0,20, by = 0.2), y = dgamma(seq(0,20, by = 0.2), kmo, smo))) + 
  geom_line()

