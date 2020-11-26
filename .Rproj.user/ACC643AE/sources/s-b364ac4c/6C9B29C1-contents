#########################################
# file  : T2_2d.R
# auther: norih
# date  : Nov 26th, 2020
# Hotelling's T-squared distribution
# Two Dimension
#########################################
data(Davis)

Davis

# select weight and height data
data <- Davis %>% select(c(weight, height)) %>% 
        data.matrix()
ggplot(NULL, aes(x=data[,1], y=data[,2])) + geom_point()

Xc <- data - matrix(1, nrow(data), 1 ) %*% mx
Sx <- t(Xc) %*% Xc / nrow(Xc)
Sx

Xc %*% solve(Sx)
a <- rowSums((Xc %*% solve(Sx)) * Xc) #異常度
th <- qchisq(.99, 1)
ggplot(NULL, aes(y=a, x = seq(1,length(a)))) + 
      geom_point(alpha = 1/5) + 
      geom_hline(yintercept = th, colour = "red", linetype = "dashed")
