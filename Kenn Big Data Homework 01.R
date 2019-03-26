### AUTHOR:     ERYL KENN VICTORINO
### PURPOSE:    HOMEWORK 1
###             from "Big Data Analysis" course
###             with Professor Min-Seong Kim
### MOD DATE:   3/26/2019

rm(list = ls())
setwd("C:/Users/Kennv/Desktop/GitHub/Big Data Analysis")

#install.packages("MASS")
#install.packages("ISLR")
library(MASS)
library(ISLR)

# Exercise 1.
# Plot the fitted y with the prediction interval based on the quadratic regression model: lm.fit3
Boston$lstat2 <- (Boston$lstat)^2
lm.fit3 <- lm(formula = medv ~ lstat + lstat2,
              data = Boston)
x1 <- seq(min(Boston$lstat), max(Boston$lstat), length.out  = 30)
x2 <- x1^2
yhat <- predict(object = lm.fit3,
                newdata = data.frame(lstat = x1, lstat2 = x2),
                interval = "prediction")
plot(x = Boston$lstat, y = Boston$medv, pch = 20)
lines(x1, yhat[, 1], lwd = 3)
lines(x1, yhat[, 2], col = "red", lwd = 3, lty = 5)
lines(x1, yhat[, 3], col = "red", lwd = 3, lty = 5)

#  Exercise 2 
# (1) Download the housing dataset from https://www.kaggle.com/harlfoxem/housesalesprediction
#     and run a regression to predict housing prices. 

kchouse <- read.csv("kc_house_data.csv", stringsAsFactors = F)
str(kchouse)
head(kchouse)
summary(kchouse)

# (2) Build a model to predict the housing price given characteristics of a house in the dataset.
#     Consider the following predictors:
#    
#     season, sqft_living, yr_built, interaction of sqft_living yr_built, and waterfront
#     Create a variable "season" which equals 
#     "Winter" if a house was sold in Jan, Feb, Mar, Dec.
#     "Spring" if it was sold in Apr, May, Jun
#     "Summer" if it was sold in Jul, Aug
#     "Fall" if it was sold in Sep, Oct, Nov.
#     Do you find any seasonality in housing price?

kchouse <-transform(kchouse, Year = substr(date, 1, 4), Month = substr(date, 5, 6), Day = substr(date, 7, 8))
kchouse$cleanDate <- as.Date(paste0(kchouse$Year, '-', kchouse$Month, '-', kchouse$Day))
kchouse$season[kchouse$Month == '01' | kchouse$Month == '02' | kchouse$Month == '03' | kchouse$Month == '12'] <- 'Winter'
kchouse$season[kchouse$Month == '04' | kchouse$Month == '05' | kchouse$Month == '06'] <- 'Spring'
kchouse$season[kchouse$Month == '07' | kchouse$Month == '08'] <- 'Summer'
kchouse$season[kchouse$Month == '09' | kchouse$Month == '10' | kchouse$Month == '11'] <- 'Fall'
model = lm(price ~ season + sqft_living + yr_built + sqft_living*yr_built + waterfront,
           data = kchouse)
summary(model)

# since there is a significance difference between the means of the season coefficients, you can notice the seasonality

# (3) Do you find any nonlinearity or heteroskedasticity? 
#     What is the problem if the error term is heteroskedastic?
#     How can you address these problems (if you have here)?

par(mfrow=c(2,2))
plot(model)

# Looking at the Residuals vs Fitted and Scale Location graphs show that the variance of the residuals across the inviduals
# is not constant there is heteroskedasticity
# there is heteroskedasticity because the variance of the error terms are related to individuals
# using robust errors should help with these problems


# (4) Conduct an F test for the following hypotheses.
#     H0: there is no seasonality on the housing price. H1: H0 is not true.

unrestricted <- lm(price ~ season + sqft_living + yr_built + sqft_living*yr_built + waterfront,
                   data = kchouse)
restricted <- lm(price~sqft_living+yr_built+sqft_living*yr_built+waterfront,
                 data = kchouse)
unrestrictedssr <- sum((fitted(unrestricted) - kchouse$price)^2)
restrictedssr <- sum((fitted(restricted) - kchouse$price)^2)
f <- ((restrictedssr - unrestrictedssr)/3)/(unrestrictedssr/(21613-5-1))

# according to the F-Test, we reject the hypothesis

# (5) Predict the housing price when season = spring, sqft_living=2500, yr_built=2000, waterfront=0.
predict(object = model,
        newdata = data.frame(season = "Spring", sqft_living = 2500, yr_built = 2000, waterfront = 0),
        interval = "prediction")
predict(object = model,
        newdata = data.frame(season = "Spring", sqft_living = 2500, yr_built = 2000, waterfront = 0),
        interval = "confidence")
predict(object = model,
        newdata = data.frame(season = "Spring", sqft_living = 2500, yr_built = 2000, waterfront = 0))
