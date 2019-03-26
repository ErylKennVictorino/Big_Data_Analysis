### AUTHOR:     ERYL KENN VICTORINO
### PURPOSE:    HOMEWORK 8
###             from "Big Data Analysis" course
###             with Professor Min-Seong Kim
### MOD DATE:   3/26/2019

rm(list = ls())
setwd("C:/Users/Kennv/Desktop/GitHub/Big Data Analysis")

#install.packages("ISLR")
#install.packages("leaps")
#install.packages("glmnet")
#install.packages("pls")
library(ISLR)
library(leaps)
library(glmnet)
library(pls)

College <- na.omit(College)

# Subset Selection

## Best subset selection
regfit.full <- regsubsets(Apps ~ .,
                          data = College,
                          nvmax = 17)
reg.summary.full <- summary(regfit.full)
plot(reg.summary.full$bic, xlab = "Number of Regressors", ylab = "BIC", type = "l", main = "Best Subset Selection")
a2 <- which.min(reg.summary.full$bic)
points(a2,reg.summary.full$bic[a2], col = "red", cex = 2, pch = 20)
plot(regfit.full, scale = "bic", main = "Best Subset Selection")
coef(regfit.full, 10)

## Forward Stepwise Subset Selection
regfit.fwd <- regsubsets(Apps ~ .,
                         data = College,
                         nvmax = 17,
                         method = "forward")
reg.summary.fwd <- summary(regfit.fwd)
reg.summary.fwd
plot(reg.summary.fwd$bic, xlab = "Number of Regressors", ylab = "BIC", type = "l", main = "Forward Stepwise Subset Selection")
a2 <- which.min(reg.summary.fwd$bic)
points(a2,reg.summary.fwd$bic[a2], col = "red", cex = 2, pch = 20)
plot(regfit.fwd, scale = "bic", main = "Forward Stepwise Subset Selection")
coef(regfit.fwd, 10)

## Backward Stepwise Subset Selection
regfit.bwd <- regsubsets(Apps ~ .,
                         data = College,
                         nvmax = 17,
                         method = "backward")
reg.summary.bwd <- summary(regfit.bwd)
reg.summary.bwd
plot(reg.summary.bwd$bic, xlab = "Number of Regressors", ylab = "BIC", type = "l", main = "Backward Stepwise Subset Selection")
a2 <- which.min(reg.summary.bwd$bic)
points(a2,reg.summary.bwd$bic[a2], col = "red", cex = 2, pch = 20)
plot(regfit.bwd, scale = "bic", main = "Backward Stepwise Subset Selection") 
coef(regfit.bwd, 10)

# Shrinkage

x.temp <- model.matrix(Apps ~ .,
                       data = College)
x <- x.temp[ , -1]
y <- College$Apps
set.seed(1)
train <- sample(1:nrow(x), round(nrow(x)/2))
y.train <- y[train]
x.train <- x[train,]
y.test <- y[-train]
x.test <- x[-train,]
grid <- 10^seq(10, -2, length = 100)

## Ridge Regression
ridge.mod <- glmnet(x.train, y.train, alpha = 0,lambda = grid)
cv.out <- cv.glmnet(x.train, y.train, alpha = 0)     
bestlamR <- cv.out$lambda.min
ridge.pred <- predict(ridge.mod, s = bestlamR, newx = x.test)
out <- glmnet(x, y, alpha = 0)
predict(out, type = "coefficients", s = bestlamR)[1:18, ]
ridge_score <- mean((ridge.pred - y.test)^2)

## Lasso Regression
lasso.mod <- glmnet(x.train, y.train, alpha = 1, lambda = grid)
cv.out <- cv.glmnet(x.train, y.train, alpha = 1)
bestlamL <- cv.out$lambda.min
lasso.pred <- predict(lasso.mod, s = bestlamL, newx = x.test)
out <- glmnet(x, y, alpha = 1, lambda = grid)
lasso.coef <- predict(out, type = "coefficients", s = bestlamL)[1:18, ]
lasso_score <- mean((lasso.pred - y.test)^2)

# Dimension Reduction

set.seed(1)
train <- sample(1:nrow(College), round(nrow(College)/2))
train.set <- College[train, ]
test.set <- College[-train, ]
x.test <- test.set[, -2]
y.test <- test.set[, 2]

## Principal Components Regression
set.seed(10)
pcr.fit <- pcr(Apps ~ .,
               data = College,
               scale = T,
               validation = "CV")
summary(pcr.fit)
validationplot(pcr.fit, val.type = "MSEP")

pcr.fit <- pcr(Apps ~ .,
               data = train.set,
               scale = T,
               ncomp = 17)
pcr.pred <- predict(pcr.fit, x.test, ncomp = 17)

pcr_score <- mean((pcr.pred - y.test)^2)

pcr.fit <- pcr(Apps ~ .,
               data = College,
               scale = T)
summary(pcr.fit)

## Partial Least Squares
set.seed(10)
pls.fit = plsr(Apps ~ .,
               data = College,
               scale = T,
               validation = "CV")
summary(pls.fit)                     
validationplot(pls.fit, val.type = "MSEP")

pls.fit <- plsr(Apps ~ .,
                data = train.set,
                scale = T,
                ncomp = 17)
pls.pred <- predict(pls.fit, x.test, ncomp = 17)

pls_score <- mean((pls.pred-y.test)^2)

pls.fit <- plsr(Apps ~ .,
                data = College,
                scale = T,
                ncomp = 12)
summary(pls.fit)

# Comparisons of the method performances

subset.fit <- lm(Apps ~ Private + Accept + Enroll + Top10perc + Top25perc + Outstate + Room.Board + PhD + Expend + Grad.Rate,
                 data = train.set)
subset.pred <- predict(subset.fit, x.test)
subset_score <- mean((subset.pred - y.test)^2)

subset_score
ridge_score
lasso_score
pcr_score
pls_score

print("Lasso works the best")