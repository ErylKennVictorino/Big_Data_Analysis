### AUTHOR:     ERYL KENN VICTORINO
### PURPOSE:    HOMEWORK 5
###             from "Big Data Analysis" course
###             with Professor Min-Seong Kim
### MOD DATE:   3/26/2019

rm(list = ls())
setwd("C:/Users/Kennv/Desktop/GitHub/Big Data Analysis")

#install.packages("ISLR")
#install.packages("MASS")
library(ISLR)
library(MASS)

# Test and Train data
test <- Caravan[1:1000,]
train <- Caravan[1000:5822,]
# LDA
lda.fit <- lda(Purchase ~ .,
               data = train)
plot(lda.fit)
lda.pred <- predict(lda.fit, test)
lda.class <- lda.pred$class
table(lda.class)
table(lda.class, test$Purchase)
mean(lda.class == test$Purchase)

# QDA
# qda.fit <- qda(Purchase ~ ., data=train)
# qda.fit
# qda.pred <- predict(qda.fit, test)
# qda.pred
# qda.class <- qda.pred$class
# table(qda.class)
# table(qda.class, test$Purchase)
# mean(qda.class == test$Purchase)
# This doesn't work because the QDA needs more parameters, so not enough variance in values
# "rank deficiency in group Yes" error

# Logit
glm.fit <- glm(Purchase ~ .,
               family = binomial,
               data = train)
glm.probs <- predict(glm.fit, test, type = "response")
glm.pred <- rep("No", dim(test)[1])
glm.pred[glm.probs > 0.5] <- "Yes"
table(glm.pred)
table(glm.pred, test$Purchase)
mean(glm.pred == test$Purchase)