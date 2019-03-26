### AUTHOR:     ERYL KENN VICTORINO
### PURPOSE:    HOMEWORK 6
###             from "Big Data Analysis" course
###             with Professor Min-Seong Kim
### MOD DATE:   3/26/2019

rm(list = ls())
setwd("C:/Users/Kennv/Desktop/GitHub/Big Data Analysis")

#install.packages("ISLR")
#install.packages("class")
#install.packages("boot")
library(ISLR)
library(class)
library(boot)

# Exercise 1
# Cross-validation can also be used to estimate the test error for a classification problem.
# Run a logit model with the Smarket data. The dependent variable is Direction
# glm.fit <- glm(Direction~Lag1+Lag2, family=binomial, data=Smarket)
# summary(glm.fit

set.seed(1)

cv.error <- rep(NA,4)
glm.fit2 <- glm(Direction ~ Lag1 + Lag2,
                family = binomial,
                data = Smarket)
cv.error[1] <- cv.glm(Smarket,glm.fit2,K=10)$delta[1]
glm.fit3 <- glm(Direction ~ Lag1 + Lag2 + Lag3,
                family = binomial,
                data = Smarket)
cv.error[2] <- cv.glm(Smarket,glm.fit3,K=10)$delta[1]
glm.fit4 <- glm(Direction ~ Lag1 + Lag2 + Lag3 + Lag4,
                family = binomial,
                data = Smarket)
cv.error[3] <- cv.glm(Smarket,glm.fit4,K=10)$delta[1]
glm.fit5 <- glm(Direction ~ Lag1 + Lag2 + Lag3 + Lag4 + Lag5,
                family = binomial,
                data = Smarket)
cv.error[4] <- cv.glm(Smarket,glm.fit5,K=10)$delta[1]
names(cv.error) <- c("glm.fit2", "glm.fit3", "glm.fit4", "glm.fit5")
cv.error
which.min(cv.error)

# Compare this model with the following models using K fold cross-validation with K=10.
# Direction~Lag1+Lag2+Lag3, Direction~Lag1+Lag2+Lag3+Lag4, Direction~Lag1+Lag2+Lag3+Lag4+Lag5

# Exercise 2.
# Consider KNN estimation to predict direction using Lag1 and Lag2. To choose the optimal number of neighbors,
# use the K=10, k fold cross validation. Use only 2004 and 2005 year data.

set.seed(1)

n <- dim(Smarket)[1]
x <- 1:n
opt_ks_first <- rep(NA, 10) # taking the first k with the minimum errors
opt_ks_mean <- rep(NA, 10) # taking the mean of the k's with the minimum errors
for (i in 1:10) {
  test <- sample(x, size = n/10, replace = F)
  train.data.x <- Smarket[-test, c("Lag1", "Lag2")]
  train.data.y <- Smarket$Direction[-test]
  test.data.x <- Smarket[test, c("Lag1", "Lag2")]
  test.data.y <- Smarket$Direction[test]
  x <- x[-test]
  err <- rep(NA, 100)
  for (j in 1:100){
    knn.pred <- knn(train.data.x,
                    test.data.x,
                    train.data.y,
                    k = j)
    err[j] <- mean(test.data.y != knn.pred)
  }
  opt_ks_first[i] <- which.min(err)
  opt_ks_mean[i] <- mean(which(err == min(err)))
}
opt_k_first = mean(opt_ks_first)
opt_k_mean = mean(opt_ks_mean)
opt_k_first
opt_k_mean