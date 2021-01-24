#########################################
# file  : GMM.R
# auther: norih
# date  : Dec 14th, 2020
# GMM is gaussian mixture model.
# Expectation-maximization algorithm is one of the algorithm
# which estimate maximum likelihood
#########################################
library(tidyverse)
# samples comes from mixture gaussian model
# signal: mu0 = 3, sig0 = 0.5, pi0 = 0.6
# noise: mu1 = 0, sig1 = 3, pi1 = 0.4
set.seed(50)
N <- 1000
mu0 = 3
sig0= 0.5
pi0 = 0.6
mu1 = 0
sig1 = 3
pi1 = 0.4

X <- sample(0:1, N, replace = TRUE, prob = c(pi0,pi1))
data <- rep(-99, N)
data[which(X == 0)] <- rnorm(length(which(X == 0)), mu0, sig0)
data[which(X == 1)] <- rnorm(length(which(X == 1)), mu1, sig1)

x0 <- seq(-5,10,.1)
y0 <- pi0 * dnorm(x0,mu0,sig0) + pi1 * dnorm(x0,mu1,sig1)
p <- ggplot(NULL, aes(x = data)) +
      geom_histogram(aes(y=..density..),      # Histogram with density instead of count on y-axis
                     binwidth=.5,
                     colour="black", alpha=.2, fill="#FF6666") +
      geom_line(aes(x=x0, y=y0), size = .5, linetype="dashed")
ggsave("./ref1/section3/fig3.2_GMM.png", plot = p)

# EM method to estimate (pi0, pi1), (mu0, sig0), (mu1, sig1)
# Initial condition
mu0 = 5
mu1 = -5
sig0 = 3
sig1 = 2
pi0 = 0.8
pi1 = 0.2


for(i in 1:20) {
  p0 <- pi0 * dnorm(data, mu0, sig0)
  p1 <- pi1 * dnorm(data, mu1, sig1)
  q0 <- p0/(p0+p1)
  q1 <- p1/(p0+p1)
  pi0 <- sum(q0) / N
  pi1 <- sum(q1) / N
  mu0 <- sum(q0 * data) / (N*pi0)
  mu1 <- sum(q1 * data) / (N*pi1)
  sig0 <- sqrt(sum(q0 * (data - mu0) * (data - mu0)) / (N*pi0))
  sig1 <- sqrt(sum(q1 * (data - mu1) * (data - mu1)) / (N*pi1))
}

paste(mu0, sig0, pi0, mu1, sig1, pi1, sep=",")

x1 <- seq(-5,10,.1)
y1 <- pi0 * dnorm(x1,mu0,sig0) + pi1 * dnorm(x1,mu1,sig1)

df <- data.frame(x = x0, original = y0, estimate = y1)
df %>% pivot_longer(cols = -x, names_to = "type", values_to = "value") %>% 
  ggplot()+geom_line(aes(x=x, y=value, color=type), size = .5)
ggsave("./ref1/section3/fig3.3_GMM.png")
