#Cleaning atribute PoolQC
library(rpart)

summary(as.factor(df$PoolQC)) # PoolQC
table(df$PoolArea > 0, df$PoolQC, useNA = "ifany")
# There are 13 houses with swimming pools and only 10 have pool quality data available.

# Reassign 'None' for 'no pool' instead of NAs
df[df$PoolArea == 0,]$PoolQC <- rep('None', 2906)

# Now there are only three missing pool quality valueswhich can be predicted using rpart
# Here is a list of variables which may be likely predictors for the swimmingpol quality.
col.pred <- c("YearBuilt","YearRemodAdd", "PoolQC", "PoolArea","WoodDeckSF","OpenPorchSF","EnclosedPorch","X3SsnPorch"
              ,    "ScreenPorch","ExterQual","ExterCond", "YrSold","SaleType","SaleCondition")

# Predict using rpart
qlty.rpart <- rpart(as.factor(PoolQC) ~ .,
                    data = df[!is.na(df$PoolQC),col.pred], 
                    method = "class", 
                    na.action=na.omit)

df$PoolQC[is.na(df$PoolQC)] <- predict(qlty.rpart,                 
                                       df[is.na(df$PoolQC),col.pred], 
                                       type="class")