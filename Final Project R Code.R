
library(dplyr)
library(tidyr)
library(tidyverse)
library(olsrr)
library(broom)
library(glmnet)
library (car) # for VIF
library (ridge)

# Read dataset

nba = read.csv("Final Project Data.csv")
nba = na.omit(nba)
View(nba)

### Correlation ###

# Tells us which columns are numeric and which ones aren't
sapply(nba, is.numeric)
# Take away the columns that aren't numeric and other unnecessary columns (Rk, Team, Season, Playoffs, Championship, & Games) 
nba_numbers = nba[, sapply(nba, is.numeric)]
nba_numbers = nba_numbers[-c(1:3, 7:8)]

# Strongly correlated variables with "Wins"
correlation = cor(nba_numbers, use = "complete.obs", method = "pearson")
wins_corr = select(as.data.frame(correlation), "Wins")
wins_corr
# (1) PTS (2) All.NBA (3) Allstars (4) FG% (5) FG
# (1) ".721" (2) ".672" (3) ".642" (4) ".636" (5) ".569"

# Scatterplot of Wins vs. PPG
attach(nba_numbers)
plot(PTS, Wins, main = "Wins vs. Points per Game", xlab = "PPG", ylab = "Wins", pch=19)

# Scatterplot of Wins vs. Number of All-NBA Players per Team
attach(nba_numbers)
plot(All.NBA, Wins, main = "Wins vs. Number of All-NBA Players per Team", xlab = "Number of All-NBA Players per Team", ylab = "Wins", pch=19)

# Scatterplot of Wins vs. Number of Allstar Players per Team
attach(nba_numbers)
plot(Allstars, Wins, main = "Wins vs. Number of Allstar Players per Team", xlab = "Number of Allstar Players per Team", ylab = "Wins", pch=19)

# Scatterplot of Wins vs. FG%
attach(nba_numbers)
plot(FG., Wins, main = "Wins vs. Field Goal Percentage", xlab = "FG%", ylab = "Wins", pch=19)

# Scatterplot of Wins vs. FG
attach(nba_numbers)
plot(FG, Wins, main = "Wins vs. Field Goals Made", xlab = "FG", ylab = "Wins", pch=19)

# Ordinary Least Squares Estimation (OLS) (Multiple Regression of Wins on each predictor)
model = lm(Wins ~ ., data = nba_numbers)
summary(model)

# Best Subset Regression
model = lm(Wins ~ PTS + All.NBA + Allstars + FG. + FG + FTA + TOV + PF + X3P., data = nba_numbers)
ols_step_best_subset(model, details = TRUE)
k = ols_step_best_subset(model)
plot(k)

# Stepwise Forward Regression
ols_step_forward_p(model, details = TRUE)
k = ols_step_forward_p(model)
plot(k)
# compute the metrics with the 5 best variables chosen
stepwise_forward_regression_model = lm(Wins ~ PTS + All.NBA + DRB + STL + X3PA, data = nba_numbers)
k = ols_step_forward_p(stepwise_forward_regression_model)

# Stepwise AIC Forward Regression
ols_step_forward_aic(model, details = TRUE)
k = ols_step_forward_aic(model)
plot(k)

# Ridge Regression
inputData = data.frame(nba_numbers)
colnames(inputData)[1] <- "response"

XVars = inputData[, -1] # X variables
round(cor(XVars), 2)

set.seed(100) # set seed to replicate results
trainingIndex = sample(1:nrow(inputData), 0.8*nrow(inputData)) # indices for 80% training data
trainingData = inputData[trainingIndex, ] # training data
testData = inputData[-trainingIndex, ] # test data

lmMod = lm(response ~ ., trainingData)  # the linear reg model
summary (lmMod) # get summary
vif(lmMod) # get VIF- ni neg coefficients so no multicollinearity issues

predicted = predict (lmMod, testData)  # predict on test data
compare = cbind (actual=testData$response, predicted)  # combine actual and predicted

mean (apply(compare, 1, min)/apply(compare, 1, max)) #accuracy = 0.868174 (86.82%)


linRidgeMod = linearRidge(response ~ ., data = trainingData)
predicted <- predict(linRidgeMod, testData)  # predict on test data
compare <- cbind (actual=testData$response, predicted)  # combine
mean (apply(compare, 1, min)/apply(compare, 1, max)) #accuracy = 0.8773619 (87.74%)
# Can we get a better accuracy?

# Predicting With A Re-calibrated Linear Model
# Used same variables that had highest correlation with wins
newlmMod = lm(response ~ ., trainingData[, -c(2, 4:8, 10:17,19:20,22:23)]) #without everything except for the 5 variables from Stepwise Forward Regression
summary(newlmMod) # get summary
vif(newlmMod) # get VIF

predicted <- predict(newlmMod, testData) # predict on test data
compare <- cbind (actual=testData$response, predicted) # for comparison
mean (apply(compare, 1, min)/apply(compare, 1, max)) #accuracy = 0.8748442 (87.48%)

# Plot Ridge Regression Graph
y = nba_numbers$Wins
x = nba_numbers %>% select(c("PTS", "All.NBA", "DRB", "STL", "X3PA")) %>% data.matrix()
lambdas = 10^seq(3, -2, by = -.1)

fit = glmnet(x, y, alpha = 0, lambda = lambdas)
summary(fit)

# Plot 
cv_fit = cv.glmnet(x, y, alpha = 0, lambda = lambdas)
plot(cv_fit)

# Log-value of lambda
opt_lambda = cv_fit$lambda.min
opt_lambda

# Lasso Regression
fit.lasso=glmnet(x,y,alpha=1)
plot(fit.lasso,xvar="lambda",label=TRUE)
plot(fit.lasso,xvar="dev",label=TRUE)
cv.lasso=cv.glmnet(x,y)
plot(cv.lasso)
# Extract the coefficients
coef(cv.lasso)
# (Intercept) -289.6986158
# PTS            1.8075443
# All.NBA        3.1462831
# DRB            3.2191087
# STL            4.0135408
# X3PA          -0.2583616

# Stepwise Backward Regression
ols_step_backward_p(model, details = TRUE)
k = ols_step_backward_p(model)
plot(k)



