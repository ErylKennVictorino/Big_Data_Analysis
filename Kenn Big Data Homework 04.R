### AUTHOR:     ERYL KENN VICTORINO
### PURPOSE:    HOMEWORK 4
###             from "Big Data Analysis" course
###             with Professor Min-Seong Kim
### MOD DATE:   3/26/2019

rm(list = ls())
setwd("C:/Users/Kennv/Desktop/GitHub/Big Data Analysis")

# Exercise 1
# create a categorical variable lowcharge which equals 1 if insurance$charges < 7000 and equals 0 otherwise
# run the logit regression of this on age, sex, bmi, smoker, region
# split the data by choosing 1000 observations for training and by using the other observations for testing
# assess the accuracy of this model

insurance = read.csv("insurance.csv", stringsAsFactors = F)
set.seed(80085)
insurance$lowcharge <- rep(0, dim(insurance)[1])
insurance$lowcharge[insurance$charges < 7000] <- 1
choice <- sample(nrow(insurance), 1000, replace = F)
train <- insurance[choice, ]
test <- insurance[-choice, ]
glm.fit2 <- glm(lowcharge ~ age + sex + bmi + smoker + region,
                family = binomial,
                data = train)
glm.probs <- predict(glm.fit2, test, type = "response")
glm.pred <- rep(0, dim(test)[1])
glm.pred[glm.probs > 0.5] = 1
table(glm.pred, test$lowcharge)
mean(glm.pred == test$lowcharge)

## It's pretty damn good