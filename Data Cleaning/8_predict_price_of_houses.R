library(nnet)
library(tidyverse)
library(caret)

# train <- read.csv("clean.csv")
# train <- train[,2:82]

train <- read.csv("clean1.csv")
train <- train[,2:82]

nzv_cols <- nearZeroVar(train)

# There are 21 near zero variance variables in the training dataset. 
# We have the index of these predictors. Therefore to check them we need to use [] 
# like train[6] or the easiest method is to use the names().

names(train[nzv_cols])
if(length(nzv_cols) > 0) 
  train_clean <- train[, -nzv_cols] # storing to a new data frame

nums <- sapply(train_clean, is.numeric)
train_no_factors <- train_clean[, nums]  #  drop all factors

# Outlier Analysis
plot(train_no_factors$SalePrice, train_no_factors$GrLivArea)
x <- train_no_factors %>% filter(train_no_factors$GrLivArea < 4000)
plot(x$SalePrice, x$GrLivArea, xlab = "Sale Price", ylab = "Gr Living Area") 

#Divide the data into training and test
training_set <- train_no_factors[1:1460,]


# Correlation plot of the variables
str(training_set)
corrplot::corrplot(cor(train_no_factors[1:1460,]), method = "circle", type = "lower", diag = FALSE, order = "FPC", tl.cex = 0.6, tl.col = "black") #  plot matrix and ordered by first principal component


# Build the model
fitnn = nnet(SalePrice ~ OverallQual + GrLivArea + GarageCars + GarageArea + X1stFlrSF + FullBath + X1stFlrSF
             + TotalBsmtSF + TotRmsAbvGrd + YearBuilt + MasVnrArea + YearRemodAdd + Fireplaces
             + LotFrontage + X2ndFlrSF + WoodDeckSF + BsmtFinSF1 + HalfBath + LotArea + BedroomAbvGr
             + BsmtUnfSF + BsmtFullBath,  
            training_set[,2:28], size = 3, skip = TRUE, linout = TRUE)



#Predict the data for test
P = predict(fitnn, train_no_factors[1461:2919, 2:27])
pred = data.frame(predicted = P, actual = train_no_factors[1461:2919, 28])
head(pred)

# plot prediction vs actual values
l = 1:350
plot(NA, xlim = c(1, length(pred[l, 1])), ylim = c(0, max(pred[, 2])), xlab = "Testing Cases", 
     ylab = "Price")
lines(pred[l, 1], col = "blue")
lines(pred[l, 2])
legend("topleft", cex = 0.75, lty = 1, c("Predicted Price", "True Price"), col = c("blue", "black"))

# predict for the actual test.csv
actual_P <- predict(fitnn, train_no_factors[1461:2919, 2:27])
actual_pred = data.frame(predicted = actual_P, actual = train_no_factors[1461:2919, 28])
head(actual_pred)
l1 = 1:50
plot(x = actual_pred[l1, 1], xlab = "Testing Cases", ylab = "Price")
lines(actual_pred[l1, 1], col = "blue")



#Calculate RMSE value
sqrt(sum((pred$predicted - pred$actual)^2, na.rm = TRUE) / nrow(pred))
