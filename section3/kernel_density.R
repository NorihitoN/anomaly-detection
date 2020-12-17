#########################################
# file  : kernel_density.R
# auther: norih
# date  : Dec 15th, 2020
# kernel density estimation (KDE) is a non-parametric way 
# to estimate the probability density function of a random variable.
# KDE is a fundamental data smoothing problem where inferences 
# about the population are made, based on a finite data sample.
#########################################

library(car)
library(tidyverse)
# 1-dimension : density() 
# 2-dimension : KernSmooth
library(KernSmooth)

x <- Davis %>% select(weight, height)

h <- c(dpik(x$weight), dpik(x$height))
est <- bkde2D(x, bandwidth=h, gridsize = c(10^3,10^3))
d <- list(x=est$x1, y=est$x2, z=est$fhat)

image(d, col=terrain.colors(7), xlim=c(35,110),ylim=c(145,200))
contour(d, add=T)