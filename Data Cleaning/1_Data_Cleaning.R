library(Amelia)
library(mice)
library(ggplot2)
library(lattice)
library(rpart)

#import the test and train datasets.
train <- read.csv(file = "train.csv", stringsAsFactors = F)
test <- read.csv(file = "test.csv", stringsAsFactors = F)

# replicate the missing sale price values and bind the training and test set. 
test$SalePrice <- rep(NA, 1459)
df <- rbind(train, test)
str(df)

# Generate a map of the missing values using ggplot2
missmap(df[,1:80],
        main = "Missing values in Housing Prices Dataset",
        col=c('black', 'blue'),
        y.labels = NULL,
        y.at = NULL,
        y.cex=0.5, 
        x.cex=0.8)

# Lets view the attributes with missing values
sapply(df[,1:80], function(x) sum(is.na(x)))


# Write to csv
#write.csv(df, file = "clean.csv")