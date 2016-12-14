#Garage
library(rpart)

table(is.na(df$GarageType))
table(is.na(df$GarageArea))
table(is.na(df$GarageCars))
table(is.na(df$GarageArea) & is.na(df$GarageCars))
table(is.na(df$GarageYrBlt))
table(is.na(df$GarageFinish))
table(is.na(df$GarageQual))
table(is.na(df$GarageCond))
table(is.na(df$GarageYrBlt) & is.na(df$GarageFinish) & is.na(df$GarageQual) & is.na(df$GarageCond))
# There are 157 observations with GarageType = NA, 1 observation with GarageArea and GarageCars as NA, 1
# 59 observations with GarageYrBlt, GarageFinish, GarageQual and GarageCond as NAs.
table(df$GarageArea == 0 & df$GarageCars==0 & is.na(df$GarageType))
# There are 157 houses with GarageArea = 0 and no Garage Type mentioned. All other garage relevant columns should have 'None'value.
col.Garage <- c("GarageType", "GarageYrBlt", "GarageFinish", "GarageQual","GarageCond")
df[df$GarageArea == 0 & df$GarageCars==0 & is.na(df$GarageType), col.Garage] <- apply(df[df$GarageArea == 0 & df$GarageCars==0 & is.na(df$GarageType), col.Garage], 2, function(x) x <- rep("None", 157))
# This leaves us with following observations with missing GarageYrBlt, GarageFinish, GarageQual and GarageCond
table(is.na(df$GarageYrBlt) & is.na(df$GarageFinish) & is.na(df$GarageQual) & is.na(df$GarageCond))
df[is.na(df$GarageYrBlt) & is.na(df$GarageFinish) & is.na(df$GarageQual) & is.na(df$GarageCond),c(col.Garage, c("GarageCars", "GarageArea"))]

# Predict GarageArea
col.pred <- c("GarageType", "GarageYrBlt", "GarageFinish", "GarageQual","GarageCond","YearBuilt", "GarageCars", "GarageArea")

area.rpart <- rpart(GarageArea ~ .,
                    data = df[!is.na(df$GarageArea),col.pred],
                    method = "anova",
                    na.action=na.omit)

df$GarageArea[is.na(df$GarageArea)] <- round(predict(area.rpart, df[is.na(df$GarageArea),col.pred]))

# Predict GarageCars
cars.rpart <- rpart(GarageCars ~ .,
                    data = df[!is.na(df$GarageCars),col.pred],
                    method = "anova",
                    na.action=na.omit)

df$GarageCars[is.na(df$GarageCars)] <- round(predict(cars.rpart, df[is.na(df$GarageCars),col.pred]))

# Predict GarageYrBlt
blt.rpart <- rpart(as.factor(GarageYrBlt) ~ .,
                   data = df[!is.na(df$GarageYrBlt),col.pred],
                   method = "class",
                   na.action=na.omit)

df$GarageYrBlt[is.na(df$GarageYrBlt)] <- as.numeric(as.character(predict(blt.rpart, df[is.na(df$GarageYrBlt),col.pred], type = "class")))

# Let's now look at the observations missing GarageFinish, GarageQual and GarageCond
df[is.na(df$GarageFinish) & is.na(df$GarageQual) & is.na(df$GarageCond),
   c(col.Garage, c("GarageCars", "GarageArea"))]

summary(as.factor(df$GarageFinish[df$GarageType == "Detchd" &  df$GarageYrBlt == 1950]))
summary(as.factor(df$GarageQual[df$GarageType == "Detchd" &  df$GarageYrBlt == 1950]))
summary(as.factor(df$GarageCond[df$GarageType == "Detchd" &  df$GarageYrBlt == 1950]))

# All of the garages made during this year were unfinised,  had GarageQual and GarageCond as TA
df$GarageFinish[df$GarageType == "Detchd" &  df$GarageYrBlt == 1950 & is.na(df$GarageFinish)] <- "Unf"
df$GarageQual[df$GarageType == "Detchd" &  df$GarageYrBlt == 1950 & is.na(df$GarageQual)] <- "TA"
df$GarageCond[df$GarageType == "Detchd" &  df$GarageYrBlt == 1950 & is.na(df$GarageCond)] <- "TA"