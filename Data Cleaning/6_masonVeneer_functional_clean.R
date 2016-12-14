#Mason Veneer
summary(as.factor(df$MasVnrType))
# There are 24 NA's and 1742 None MasVnrType. Let's check their corresponding area and see they are 0 or NAs
table(df$MasVnrArea[df$MasVnrType == "None"])

# While 1735 have their area correctly labeled, 7 of them don't seem to right.
# Since one square feet of MasVnrAreaseem to be very unlikely, let us change these 3 observations with MasVnrArea = 1 to MasVnrArea = 0

df$MasVnrArea <- ifelse(df$MasVnrArea == 1,0,df$MasVnrArea)

# Assign other 4 observations with areas > 0 but having Type as None to NA, which will be fixed later
df$MasVnrType[df$MasVnrArea > 0 & df$MasVnrType == "None" & !is.na(df$MasVnrType)] <- rep(NA, 4)

table(is.na(df$MasVnrType) & is.na(df$MasVnrArea))

# There are 23 NAs for both MasVnrType and MasVnrArea. Lets assign 0 to all MasVnrArea and None to all MasVnrType.
df$MasVnrArea[is.na(df$MasVnrArea)] <-rep(0, 23)
df$MasVnrType[is.na(df$MasVnrType) & df$MasVnrArea == 0] <- rep("None", 23)

# Let's check if all is well.
table(df$MasVnrType, df$MasVnrArea == 0, useNA = "ifany")

table(df$MasVnrType, df$MasVnrArea > 0, useNA = "ifany")
# There are 3 observations with MasVenType specified but has Area 0 and 5 observations with Area more than 0 but Type is missing.
# Let us assign observations with Area = 0  to Type = None 
df$MasVnrType[df$MasVnrType == "BrkFace" & df$MasVnrArea == 0] <- rep("None",2)
df$MasVnrType[df$MasVnrType == "Stone" & df$MasVnrArea == 0] <- rep("None",1)

# Predict MasVnrType 
Type.rpart <- rpart(as.factor(MasVnrType) ~ MasVnrArea,
                    data = df[!is.na(df$MasVnrType),c("MasVnrType","MasVnrArea")], 
                    method = "class", 
                    na.action=na.omit)

df$MasVnrType[is.na(df$MasVnrType)] <- as.character(predict(Type.rpart, 
                                                            df[is.na(df$MasVnrType),c("MasVnrType","MasVnrArea")],
                                                            type="class"))



###################################################################################################
# Functional
table(is.na(df$Functional))
# df[is.na(df$Functional),]
# There are two observations with missing Functional data. 

# Likely predictors
col.pred <- c("OverallQual", "OverallCond", "YearBuilt", "YearRemodAdd", "ExterQual", "ExterCond","BsmtQual", 
              "BsmtCond","GarageQual", "GarageCond","SaleType", "SaleCondition", "Functional")

func.rpart <- rpart(as.factor(Functional) ~ .,
                    data = df[!is.na(df$Functional),col.pred],
                    method = "class",
                    na.action=na.omit)

df$Functional[is.na(df$Functional)] <- as.character(predict(func.rpart, df[is.na(df$Functional),col.pred], type = "class"))


#############################################################################################################
#utilities
table(is.na(df$Utilities))
# df[is.na(df$Utilities),]

# Likely predictors
col.pred <- c("BldgType", "HouseStyle","OverallQual", "OverallCond", "YearBuilt", "YearRemodAdd", "ExterQual", "ExterCond","BsmtQual", 
              "BsmtCond","GarageQual", "GarageCond","SaleType", "SaleCondition", "Functional", "WoodDeckSF", "OpenPorchSF", "EnclosedPorch", "X3SsnPorch", "ScreenPorch", "PoolArea","Utilities")

util.rpart <- rpart(as.factor(Utilities) ~ .,
                    data = df[!is.na(df$Utilities),col.pred],
                    method = "class",
                    na.action=na.omit)

df$Utilities[is.na(df$Utilities)] <- as.character(predict(util.rpart, df[is.na(df$Utilities),col.pred], type = "class"))

###########################################################################################################
# Exterior
table(is.na(df$Exterior1st) & is.na(df$Exterior2nd))

# Likely predictors
col.pred <- c("BldgType", "HouseStyle", "OverallQual", "OverallCond", "YearBuilt", "YearRemodAdd", "RoofStyle", "RoofMatl", "Exterior1st", "Exterior2nd", "MasVnrType", "MasVnrArea", "ExterQual", "ExterCond")

# Predict Exterior1st
ext1.rpart <- rpart(as.factor(Exterior1st) ~ .,
                    data = df[!is.na(df$Exterior1st),col.pred],
                    method = "class",
                    na.action=na.omit)

df$Exterior1st[is.na(df$Exterior1st)] <- as.character(predict(ext1.rpart, df[is.na(df$Exterior1st),col.pred], type = "class"))

# Predict Exterior2nd
ext2.rpart <- rpart(as.factor(Exterior2nd) ~ .,
                    data = df[!is.na(df$Exterior2nd),col.pred],
                    method = "class",
                    na.action=na.omit)

df$Exterior2nd[is.na(df$Exterior2nd)] <- as.character(predict(ext2.rpart, df[is.na(df$Exterior2nd),col.pred], type = "class"))