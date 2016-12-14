#basement prediction

table(is.na(df$BsmtExposure))
table(is.na(df$BsmtCond))
table(is.na(df$BsmtQual))
table(is.na(df$BsmtFinType2))
table(is.na(df$BsmtFinType1))
table(is.na(df$BsmtFinSF1) & is.na(df$BsmtFinSF2) & is.na(df$BsmtUnfSF))
table(is.na(df$BsmtFullBath) & is.na(df$BsmtHalfBath))
table(df$TotalBsmtSF == 0 & is.na(df$BsmtExposure))

col.bsmt <- c("TotalBsmtSF", "BsmtExposure", "BsmtCond", "BsmtQual","BsmtFinType1", "BsmtFinType2", 
              "BsmtFinSF1","BsmtFinSF2", "BsmtUnfSF")
# df[is.na(df$BsmtExposure) & is.na(df$BsmtCond),col.bsmt]

# There is one row with NA for all basement data and missing TotalBsmtSF which is assumed 0.
df$TotalBsmtSF[is.na(df$BsmtExposure) & is.na(df$TotalBsmtSF)] <- 0

col.bsmt <- c("BsmtExposure", "BsmtCond", "BsmtQual","BsmtFinType1", "BsmtFinType2")

# There are 79 rows with NA for basement data
df[df$TotalBsmtSF == 0 & is.na(df$BsmtExposure), col.bsmt] <- 
  apply(df[df$TotalBsmtSF == 0 & is.na(df$BsmtExposure), col.bsmt], 2, function(x) x <- rep("None", 79))

# Let us look at the rest of the missing basement data.
df[is.na(df$BsmtExposure)|is.na(df$BsmtCond)|is.na(df$BsmtQual)|is.na(df$BsmtFinType2), 
   c(col.bsmt, c("TotalBsmtSF","BsmtFinSF1","BsmtFinSF2", "BsmtUnfSF"))]

# Assume that all the basement info and the year the house was built can be a good predictor of any basement info.
col.pred <- c("BsmtExposure", "BsmtCond", "BsmtQual","BsmtFinType1", "BsmtFinType2","TotalBsmtSF","YearBuilt")

BsmtFinType2.rpart <- rpart(as.factor(BsmtFinType2) ~ .,
                            data = df[!is.na(df$BsmtFinType2),col.pred], 
                            method = "class", 
                            na.action=na.omit)

df$BsmtFinType2[is.na(df$BsmtFinType2)] <- as.character(predict(BsmtFinType2.rpart,                                               
                                                                df[is.na(df$BsmtFinType2),col.pred], 
                                                                type="class"))

BsmtQual.rpart <- rpart(as.factor(BsmtQual) ~ .,
                        data = df[!is.na(df$BsmtQual),col.pred], 
                        method = "class", 
                        na.action=na.omit)

df$BsmtQual[is.na(df$BsmtQual)] <- as.character(predict(BsmtQual.rpart,
                                                        df[is.na(df$BsmtQual),col.pred], 
                                                        type="class"))

BsmtCond.rpart <- rpart(as.factor(BsmtCond) ~ .,
                        data = df[!is.na(df$BsmtCond),col.pred], 
                        method = "class", 
                        na.action=na.omit)

df$BsmtCond[is.na(df$BsmtCond)] <- as.character(predict(BsmtCond.rpart, 
                                                        df[is.na(df$BsmtCond),col.pred], 
                                                        type="class"))

BsmtExposure.rpart <- rpart(as.factor(BsmtExposure) ~ .,
                            data = df[!is.na(df$BsmtExposure),col.pred], 
                            method = "class", 
                            na.action=na.omit)

df$BsmtExposure[is.na(df$BsmtExposure)] <- as.character(predict(BsmtExposure.rpart,                                               
                                                                df[is.na(df$BsmtExposure),col.pred], 
                                                                type="class"))

# Let us check there are any more NAs
df[is.na(df$BsmtFinSF1)|is.na(df$BsmtFinSF2)|is.na(df$BsmtUnfSF), c(col.pred, c("BsmtFinSF1", "BsmtFinSF2","BsmtUnfSF", "BsmtFullBath","BsmtHalfBath"))]

# There is no basement here as TotalBsmtSF = 0
df$BsmtFinSF1[is.na(df$BsmtFinSF1)|is.na(df$BsmtFinSF2)|is.na(df$BsmtUnfSF)] <- 0
df$BsmtFinSF2[is.na(df$BsmtFinSF1)|is.na(df$BsmtFinSF2)|is.na(df$BsmtUnfSF)] <- 0
df$BsmtUnfSF[is.na(df$BsmtFinSF1)|is.na(df$BsmtFinSF2)|is.na(df$BsmtUnfSF)] <- 0
df$BsmtFullBath[df$TotalBsmtSF == 0 & is.na(df$BsmtFullBath)] <- rep(0,2)
df$BsmtHalfBath[df$TotalBsmtSF == 0 & is.na(df$BsmtHalfBath)] <- rep(0,2)



