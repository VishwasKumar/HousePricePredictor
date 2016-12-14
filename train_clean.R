######################################################################################################
### Data Cleaning 
######################################################################################################
library(Amelia)
library(mice)
library(ggplot2)
library(lattice)
library(rpart)
library(extracat)


train <- read.csv("train.csv", stringsAsFactors = F)
df <- data.frame(train)

str(df)

# Generate a map of the missing values using ggplot2
missmap(df[,1:80],
        main = "Missing values in Housing Prices Dataset",
        col=c('black', 'blue'),
        y.labels = NULL,
        y.at = NULL,
        y.cex=0.5, 
        x.cex=0.8)
# or use extracat
extracat::visna(train, sort = "b")

# Have a look at the missing values in the dataset
sapply(df[,1:80], function(x) sum(is.na(x)))


########################################################################################################
### PoolQc
########################################################################################################

# There are 7 houses with swimming pools and all have pool quality data available.

# Reassign 'None' for 'no pool' instead of NAs
df[df$PoolArea == 0,]$PoolQC <- rep('None', 1453)

########################################################################################################
### MiscFeature
########################################################################################################
# Here NA is again misleading and can be substituted by 'None' instead.
df$MiscFeature[is.na(df$MiscFeature)] <- rep('None', 1406)

########################################################################################################
### Alley
########################################################################################################

# Susbtituting NA with 'None'
df$Alley[is.na(df$Alley)] <- rep('None', 1369)

########################################################################################################
### Fence
########################################################################################################

# Again susbtitute NA with None
df$Fence[is.na(df$Fence)] <- rep('None', 1179)

########################################################################################################
### Fire Place quality
########################################################################################################

# Substitute NAs by None for 'No Fireplace'
df$FireplaceQu[is.na(df$FireplaceQu)] <- rep('None', 690)

########################################################################################################
### Garage
########################################################################################################

# There are 81 observations with GarageType = NA, 81 observations with GarageYrBlt, GarageFinish, GarageQual and GarageCond as NAs.

table(df$GarageArea == 0 & df$GarageCars==0 & is.na(df$GarageType))

# There are 81 houses with GarageArea = 0 and no Garage Type mentioned. All other garage relevant columns should have 'None'value.
col.Garage <- c("GarageType", "GarageYrBlt", "GarageFinish", "GarageQual","GarageCond")
df[df$GarageArea == 0 & df$GarageCars==0 & is.na(df$GarageType), col.Garage] <- apply(df[df$GarageArea == 0 & df$GarageCars==0 & is.na(df$GarageType), col.Garage], 2, function(x) x <- rep("None", 81))

# This leaves us with following observations with missing GarageYrBlt, GarageFinish, GarageQual and GarageCond

table(is.na(df$GarageYrBlt) & is.na(df$GarageFinish) & is.na(df$GarageQual) & is.na(df$GarageCond))

########################################################################################################
### Basement
########################################################################################################

col.bsmt <- c("TotalBsmtSF", "BsmtExposure", "BsmtCond", "BsmtQual","BsmtFinType1", "BsmtFinType2", 
              "BsmtFinSF1","BsmtFinSF2", "BsmtUnfSF")

df$TotalBsmtSF[is.na(df$BsmtExposure) & is.na(df$TotalBsmtSF)] <- 0

col.bsmt <- c("BsmtExposure", "BsmtCond", "BsmtQual","BsmtFinType1", "BsmtFinType2")

# There are 37 rows with NA for basement data
df[df$TotalBsmtSF == 0 & is.na(df$BsmtExposure), col.bsmt] <- 
  apply(df[df$TotalBsmtSF == 0 & is.na(df$BsmtExposure), col.bsmt], 2, function(x) x <- rep("None", 37))

# Let us look at the rest of the missing basement data.
df[is.na(df$BsmtExposure)|is.na(df$BsmtCond)|is.na(df$BsmtQual)|is.na(df$BsmtFinType2), 
   c(col.bsmt, c("TotalBsmtSF","BsmtFinSF1","BsmtFinSF2", "BsmtUnfSF"))]

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


########################################################################################################
### Mason Veneer
########################################################################################################

# While 859 have their area correctly labeled, 5 of them don't seem to right.
# Since one square feet of MasVnrAreaseem to be very unlikely, let us change these 3 observations with MasVnrArea = 1 to MasVnrArea = 0

df$MasVnrArea <- ifelse(df$MasVnrArea == 1,0,df$MasVnrArea)

# There are 8 NAs for both MasVnrType and MasVnrArea. Lets assign 0 to all MasVnrArea and None to all MasVnrType.
df$MasVnrArea[is.na(df$MasVnrArea)] <-rep(0, 8)
df$MasVnrType[is.na(df$MasVnrType) & df$MasVnrArea == 0] <- rep("None", 8)

# There are 3 observations with MasVenType specified but has Area 0 and 5 observations with Area more than 0 but Type is missing.
# Let us assign observations with Area = 0  to Type = None 

df$MasVnrType[df$MasVnrType == "BrkFace" & df$MasVnrArea == 0] <- rep("None",1)
df$MasVnrType[df$MasVnrType == "Stone" & df$MasVnrArea == 0] <- rep("None",1)

# Predict MasVnrType 
Type.rpart <- rpart(as.factor(MasVnrType) ~ MasVnrArea,
                    data = df[!is.na(df$MasVnrType),c("MasVnrType","MasVnrArea")], 
                    method = "class", 
                    na.action=na.omit)

df$MasVnrType[is.na(df$MasVnrType)] <- as.character(predict(Type.rpart, 
                                                            df[is.na(df$MasVnrType),c("MasVnrType","MasVnrArea")],
                                                            type="class"))


########################################################################################################
### Electrical
########################################################################################################


# Likely predictors
col.pred <- c("BldgType", "HouseStyle", "OverallQual", "OverallCond", "YearBuilt", "YearRemodAdd", "Electrical")

# Predict Electrical
elec.rpart <- rpart(as.factor(Electrical) ~ .,
                    data = df[!is.na(df$Electrical),col.pred],
                    method = "class",
                    na.action=na.omit)

df$Electrical[is.na(df$Electrical)] <- as.character(predict(elec.rpart, df[is.na(df$Electrical),col.pred], type = "class"))


########################################################################################################
### Lot Frontage
########################################################################################################

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

df$LotFrontage[is.na(df$LotFrontage)] <- ceiling(predict(frntage.rpart, df[is.na(df$LotFrontage),col.pred]))



##############################################
## Check all imputed values
#############################################
sapply(df[,1:80], function(x) sum(is.na(x)))



write.csv(df, file = "cleaned_train.csv")





























