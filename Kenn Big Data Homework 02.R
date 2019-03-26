### AUTHOR:     ERYL KENN VICTORINO
### PURPOSE:    HOMEWORK 2
###             from "Big Data Analysis" course
###             with Professor Min-Seong Kim
### MOD DATE:   3/26/2019

rm(list = ls())
setwd("C:/Users/Kennv/Desktop/GitHub/Big Data Analysis")

#  Exercise 1
#  Simulate time series data and see that conventional tests may not work in the time series. 
#  e.g. GDP growth, stock price, a firm's revenue.... Marketing

reject <- 0

for (i in 1:1000) {
  Sales<- rep(NA,100)
  Online <- rep(NA,100)
  e <- rep(NA,100)
  
  Online[1] <- 2*rnorm(1)
  e[1] <- rnorm(1)
  b0 <- 1
  b1 <- 0
  Sales[1] <- b0 + b1*Online[1] + e[1]
  
  rho1 <- 0.7
  rho2 <- 0.7
  
  for (t in 2:100) {
    Online[t] <- rho1*Online[t - 1] + rnorm(1)
    e[t] <- rho2*e[t - 1] + rnorm(1)
    Sales[t] <- b0 + b1*Online[t] + e[t]
  }
  
  linear.fit <- lm(Sales ~ Online)
  summary(linear.fit) 
  ci <- confint(linear.fit)[2,]
  if (ci[1] > 0 | ci[2] < 0){
    reject <- reject + 1
  }
}
rejper <- reject*100/1000
rejper

# We can see that we reject H0: b1=0. So, we conclude that TV advertisement is associated with Sales.
# But, this is a mistake. The true b1=0. If our inference procedure is valid, when we test 100 times, 
# our mistake should be around 5 times.
# Using simulations, count how many times falsely reject H0, when we replicate the procedure above 1000 times.