#Cleaing for misc, alley and fence attributes

#Misc
table(is.na(df$MiscFeature))
df$MiscFeature[is.na(df$MiscFeature)] <- rep('None', 2814)

#Alley
table(is.na(df$Alley))
df$Alley[is.na(df$Alley)] <- rep('None', 2721)

#Fence
table(is.na(df$Fence))
df$Fence[is.na(df$Fence)] <- rep('None', 2348)

#Fire Place
table(is.na(df$FireplaceQu))
# There are 1420 rows with NAs for  FireplaceQu
# Check number of fireplaces
table(as.factor(df$Fireplaces), useNA = "ifany")
# There are same amount of houses with no fireplaces. Do they match?
table(is.na(df$FireplaceQu), df$Fireplaces == 0)
# Substitute NAs by None for 'No Fireplace'
df$FireplaceQu[is.na(df$FireplaceQu)] <- rep('None', 1420)


#zoning
table(is.na(df$MSZoning))
col.pred <- c("Neighborhood", "Condition1", "Condition2", "MSZoning")
msz.rpart <- rpart(as.factor(MSZoning) ~ .,
                   data = df[!is.na(df$MSZoning),col.pred],
                   method = "class",
                   na.action=na.omit)

df$MSZoning[is.na(df$MSZoning)] <- as.character(predict(msz.rpart, df[is.na(df$MSZoning),col.pred], type = "class"))















