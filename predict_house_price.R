library(nnet)

train <- read.csv("cleaned_train.csv")
train <- train[,2:82]
test <- read.csv("test.csv")

nzv_cols <- nearZeroVar(train)

# There are 21 near zero variance variables in the training dataset. 
# We have the index of these predictors. Therefore to check them we need to use [] 
# like train[6] or the easiest method is to use the names().

names(train[nzv_cols])
if(length(nzv_cols) > 0) 
  train_clean <- train[, -nzv_cols] # storing to a new data frame

nums <- sapply(train_clean, is.numeric)
train_no_factors <- train_clean[, nums]  #  drop all factors

# Correlation plot of the variables
str(train_no_factors)
corrplot::corrplot(cor(train_no_factors), method = "circle", type = "lower", diag = FALSE, order = "FPC", tl.cex = 0.6, tl.col = "black") #  plot matrix and ordered by first principal component

#Divide the data into training and test
training_set <- train_no_factors[1:1100,]

# Build the model
fitnn = nnet(SalePrice ~ OverallQual + GrLivArea + GarageCars + GarageArea + X1stFlrSF + FullBath
             + TotalBsmtSF + TotRmsAbvGrd + YearBuilt + MasVnrArea + YearRemodAdd + Fireplaces, 
             training_set[,2:29], size = 3, skip = TRUE, linout = TRUE)

#Predict the data for test
P = predict(fitnn, train_no_factors[1101:1460, 2:28])
pred = data.frame(predicted = P, actual = train_no_factors[1101:1460, 29])
head(pred)

# plot prediction vs actual values
l = 1:350
plot(NA, xlim = c(1, length(pred[l, 1])), ylim = c(0, max(pred[, 2])), xlab = "Testing Cases", 
     ylab = "Price")
lines(pred[l, 1], col = "blue")
lines(pred[l, 2])
legend("topleft", cex = 0.75, lty = 1, c("Predicted Price", "True Price"), col = c("blue", "black"))



#Make same changes to test set
nzv_cols <- nearZeroVar(test)

names(test[nzv_cols])
if(length(nzv_cols) > 0) 
  test_clean <- test[, -nzv_cols] # storing to a new data frame

nums1 <- sapply(test_clean, is.numeric)
test_no_factors <- test_clean[, nums]  #  drop all factors



