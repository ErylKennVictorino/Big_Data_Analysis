### AUTHOR:     ERYL KENN VICTORINO
### PURPOSE:    HOMEWORK 3
###             from "Big Data Analysis" course
###             with Professor Min-Seong Kim
### MOD DATE:   3/26/2019

rm(list = ls())
setwd("C:/Users/Kennv/Desktop/GitHub/Big Data Analysis")


# Exercise 1
# Download "Auto.csv" from http://www-bcf.usc.edu/~gareth/ISL/data.html
# and estimate the regression of mpg on weight using the KNN method.
# Draw the regression line on the scatterplot and compare it with linear regression line.

auto = read.csv("Auto.csv", stringsAsFactors = F)

mpg <- auto$mpg
weight <- auto$weight
reg <- lm(mpg~weight)
plot(weight, mpg)
abline(reg)

knn <- function(x0, X, Y, K){
  x0 <- matrix(rep(x0, length(Y)), byrow = T)
  X <- matrix(X)
  dist <- rowSums((x0 - X)^2)
  rank <- order(dist)
  Y_K <- Y[rank][1:K]
  mean(Y_K)
}

x <- seq(from = min(weight), to = max(weight), length = 397)
nrow(x)
fhat <- matrix(rep(NA, 1985), 397, 5)
for (j in 1:5){
  K = 10*j - 9
  for (i in 1:397){
    fhat[i, j] <- knn(x[i], weight, mpg, K)
  }
}
nrow(fhat)
lines(x, fhat[, 1], col = "red", lwd = 2)
lines(x, fhat[, 2], col = "purple", lwd = 2)
lines(x, fhat[, 3], col = "black", lwd = 2, lty = 10)
lines(x, fhat[, 4], col = "blue", lwd = 2)
lines(x, fhat[, 5], col = "orange", lwd = 2)