#Electrical
table(is.na(df$Electrical))

# Likely predictors
col.pred <- c("BldgType", "HouseStyle", "OverallQual", "OverallCond", "YearBuilt", "YearRemodAdd", "Electrical")

# Predict Electrical
elec.rpart <- rpart(as.factor(Electrical) ~ .,
                    data = df[!is.na(df$Electrical),col.pred],
                    method = "class",
                    na.action=na.omit)

df$Electrical[is.na(df$Electrical)] <- as.character(predict(elec.rpart, df[is.na(df$Electrical),col.pred], type = "class"))


###############################################################################################
#Kitchen Quality
table(is.na(df$KitchenQual))

# Likely predictors
col.pred <- c("BldgType", "HouseStyle", "OverallQual", "OverallCond", "YearBuilt", "YearRemodAdd", "KitchenQual")

# Predict KitchenQual
kit.rpart <- rpart(as.factor(KitchenQual) ~ .,
                   data = df[!is.na(df$KitchenQual),col.pred],
                   method = "class",
                   na.action=na.omit)

df$KitchenQual[is.na(df$KitchenQual)] <- as.character(predict(kit.rpart, df[is.na(df$KitchenQual),col.pred], type = "class"))


######################################################################################################
# Lot Frontage
library(ggplot2)
library(rpart)
table(is.na(df$LotFrontage))
# Likely predictors
col.pred <- c("MSSubClass", "MSZoning", "LotFrontage", "LotArea", "Street", "Alley", "LotShape", "LandContour", "LotConfig", "LandSlope", "BldgType", "HouseStyle", "YrSold", "SaleType", "SaleCondition")

# Predict LotFrontage
frntage.rpart <- rpart(LotFrontage ~ .,
                       data = df[!is.na(df$LotFrontage),col.pred],
                       method = "anova",
                       na.action=na.omit)

# Let us plot the existing and imputed values and check if imputed values follow the same patten
df.frontage <- as.data.frame(rbind(cbind(rep("Existing", nrow(df[!is.na(df$LotFrontage),])),df[!is.na(df$LotFrontage), "LotFrontage"]),
                                   cbind(rep("Imputed", nrow(df[is.na(df$LotFrontage),])),
                                         ceiling(predict(frntage.rpart, df[is.na(df$LotFrontage),col.pred])))))


ggplot(df.frontage, aes (x = as.numeric(as.character(V2)), colour = V1)) +
  geom_density()+
  xlab("Lot Frontage")+
  theme(legend.title=element_blank())

# Imputed value seem to be fine.

df$LotFrontage[is.na(df$LotFrontage)] <- ceiling(predict(frntage.rpart, df[is.na(df$LotFrontage),col.pred]))


###################################################################################################
# Sale Type
table(is.na(df$SaleType))
# Likely predictors
col.pred <- c("MSSubClass", "MSZoning", "LotFrontage", "LotArea", "Street", "Alley", "LotShape", "LandContour", "Utilities", "LotConfig", "LandSlope", "Neighborhood", "Condition1", "Condition2", "BldgType", "HouseStyle", "OverallQual", "OverallCond", "YearBuilt", "YearRemodAdd", "ExterQual", "ExterCond", "BsmtQual", "BsmtCond", "KitchenQual", "FireplaceQu", "GarageQual", "GarageCond", "PavedDrive", "WoodDeckSF", "OpenPorchSF", "EnclosedPorch", "X3SsnPorch", "ScreenPorch", "PoolQC", "YrSold", "SaleType", "SaleCondition")

# Predict SaleType
sale.rpart <- rpart(as.factor(SaleType) ~ .,
                    data = df[!is.na(df$SaleType),col.pred],
                    method = "class",
                    na.action=na.omit)

df$SaleType[is.na(df$SaleType)] <- as.character(predict(sale.rpart, df[is.na(df$SaleType),col.pred], type = "class"))


##########################################################################################################
# check if all the values are cleaned and imputed
sapply(df[,1:80], function(x) sum(is.na(x)))
