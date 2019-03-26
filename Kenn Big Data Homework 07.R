### AUTHOR:     ERYL KENN VICTORINO
### PURPOSE:    HOMEWORK 7
###             from "Big Data Analysis" course
###             with Professor Min-Seong Kim
### MOD DATE:   3/26/2019

rm(list = ls())
setwd("C:/Users/Kennv/Desktop/GitHub/Big Data Analysis")

#install.packages("boot")
#install.packages("ISLR")
library(boot)
library(ISLR)

set.seed(1)

R <- 1000
Auto.boot <- Auto[ , c(1, 4, 5)]
n <- nrow(Auto.boot)
reg <- lm(mpg ~ horsepower + weight,
          data = Auto.boot)
x1 <- Auto.boot$horsepower
x2 <- Auto.boot$weight
b_0_hat <- reg$coefficients[1]
b_1_hat <- reg$coefficients[2]
b_2_hat <- reg$coefficients[3]
num <- sum(((x1 - mean(x1))^2) * (reg$residuals^2))/n
den <- sum((x1 - mean(x1))^2)/(n-1)
se <- sqrt(num)/den
se

betas <- rep(NA, R)
ts <- rep(NA, R)

for (r in 1:R){
  index <- sample(1:n, size = n, replace = T)
  X1 <- x1
  X2 <- x2
  u <- reg$residuals[index]
  Y <- b_0_hat + b_1_hat*X1 + b_2_hat*X2 + u
  
  regRes <- lm(Y ~ X1 + X2)
  betas[r] <- regRes$coefficients[2]
  
  numRes <- sum(((X1 - mean(X1))^2) * (regRes$residuals^2))/n
  denRes <- sum((X1 - mean(X1))^2)/(n-1)
  seRes <- sqrt(numRes)/denRes
  
  ts[r] <- sqrt(n)*(betas[r] - b_1_hat)/seRes
}

ts <- sort(ts)

crit <- c(ts[25], ts[975])
cat(paste("The 95% critical values are", round(crit[1], 3),
          "and", round(crit[2], 3), "."))
