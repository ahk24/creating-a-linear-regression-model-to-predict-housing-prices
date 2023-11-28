HP <- read.csv("train.csv")
HP
dim(HP)
HP[1:10]
summary(HP)
library(dplyr)

# seperating cat, num 
num_cols <- HP %>%
    select_if(is.numeric)
num_cols
cat_cols <- HP %>%
select_if(is.character)
cat_cols
summary(num_cols)

#filling the nulls of numerical variables 
null_perc <- colMeans(is.na(HP)) * 100
null_perc
HP
summary(HP)
colSums(is.na(HP))
num_cols
num_cols <- apply(num_cols, 2, function(x) {
  ifelse(is.na(x), mean(x, na.rm = TRUE), x)
})
colSums(is.na(num_cols))


# analyzing categorical variables
colSums(is.na(HP))

cat_cols[is.na(cat_cols)] <- 'NE'

colSums(is.na(cat_cols))

cat_cols

unique(cat_cols$CentralAir)

#transforming the categorical variables with natural order
cat_cols$GarageQual <- as.numeric(factor(cat_cols$GarageQual, levels = unique(cat_cols$GarageQual), ordered = TRUE))
cat_cols$GarageQual

cat_cols$Street <- as.numeric(factor(cat_cols$Street, levels = rev(unique(cat_cols$Street)), ordered = TRUE))
cat_cols$Street


cat_cols$Alley
cat_cols$Alley <- as.numeric(factor(cat_cols$Alley, levels = unique(cat_cols$Alley), ordered = TRUE))
cat_cols$Alley

cat_cols$LotShape
cat_cols$LotShape <- as.numeric(factor(cat_cols$LotShape, levels = rev(unique(cat_cols$LotShape)), ordered = TRUE))
cat_cols$LotShape


cat_cols$Utilities
cat_cols$Utilities <- as.numeric(factor(cat_cols$Utilities, levels = rev(unique(cat_cols$Utilities)), ordered = TRUE))
cat_cols$Utilities


cat_cols$Utilities
cat_cols$Utilities <- as.numeric(factor(cat_cols$Utilities, levels = rev(unique(cat_cols$Utilities)), ordered = TRUE))
cat_cols$Utilities

cat_cols$LandSlope
cat_cols$LandSlope <- as.numeric(factor(cat_cols$LandSlope, levels = rev(unique(cat_cols$LandSlope)), ordered = TRUE))
cat_cols$LandSlope


unique(cat_cols$ExterQual)
cat_cols$ExterQual <- as.numeric(factor(cat_cols$ExterQual, levels = rev(c("Ex", "Gd", "TA", "Fa")), ordered = TRUE))
cat_cols$ExterQual



unique(cat_cols$ExterCond)
cat_cols$ExterCond <- as.numeric(factor(cat_cols$ExterCond, levels = rev(c("Ex",  "Gd", "TA", "Fa", "Po")), ordered = TRUE))
cat_cols$ExterCond

unique(cat_cols$BsmtQual)
cat_cols$BsmtQual <- as.numeric(factor(cat_cols$BsmtQual, levels = rev(c("Ex",  "Gd", "TA", "Fa", "NE")), ordered = TRUE))
cat_cols$BsmtQual


unique(cat_cols$BsmtCond)
cat_cols$BsmtCond <- as.numeric(factor(cat_cols$BsmtCond, levels = rev(c("Gd", "TA", "Fa", "Po", "NE")), ordered = TRUE))
cat_cols$BsmtCond


unique(cat_cols$BsmtExposure)
cat_cols$BsmtExposure <- as.numeric(factor(cat_cols$BsmtExposure, levels = c("NE", "No",  "Mn", "Av", "Gd"), ordered = TRUE))
cat_cols$BsmtExposure



unique(cat_cols$BsmtFinType1)
cat_cols$BsmtFinType1 <- as.numeric(factor(cat_cols$BsmtFinType1, levels = rev(c("GLQ", "ALQ", "BLQ", "Rec", "LwQ", "Unf", "NE")), ordered = TRUE))
cat_cols$BsmtFinType1


unique(cat_cols$BsmtFinType2)
cat_cols$BsmtFinType2 <- as.numeric(factor(cat_cols$BsmtFinType2, levels = rev(c("GLQ", "ALQ", "BLQ", "Rec", "LwQ", "Unf", "NE")), ordered = TRUE))
cat_cols$BsmtFinType2


unique(cat_cols$HeatingQC)
cat_cols$HeatingQC <- as.numeric(factor(cat_cols$HeatingQC, levels = rev(unique(cat_cols$HeatingQC)), ordered = TRUE))
cat_cols$HeatingQC


unique(cat_cols$CentralAir)
cat_cols$CentralAir <- as.numeric(factor(cat_cols$CentralAir, levels = c("No", "Yes"), ordered = TRUE))
cat_cols$CentralAir


unique(cat_cols$KitchenQual)
cat_cols$KitchenQual <- as.numeric(factor(cat_cols$KitchenQual, levels = rev(c("Ex", "Gd", "TA", "Fa")), ordered = TRUE))
cat_cols$KitchenQual


unique(cat_cols$Functional)
cat_cols$Functional <- as.numeric(factor(cat_cols$Functional, levels = rev(c("Typ", "Min1" , "Min2", "Maj1",  "Mod",  "Maj2", "Sev" )), ordered = TRUE))
cat_cols$Functional


unique(cat_cols$FireplaceQu)
cat_cols$FireplaceQu <- as.numeric(factor(cat_cols$FireplaceQu, levels = rev(c("Ex","Gd", "TA", "Fa", "Po", "NE")), ordered = TRUE))
cat_cols$FireplaceQu


unique(cat_cols$GarageFinish)
cat_cols$GarageFinish <- as.numeric(factor(cat_cols$GarageFinish, levels = rev(c("Fin","RFn", "Unf", "NE" )), ordered = TRUE))
cat_cols$GarageFinish


unique(cat_cols$GarageCond)
cat_cols$GarageCond <- as.numeric(factor(cat_cols$GarageCond, levels = rev(c("Ex", "Gd", "TA" ,"Fa","Po", "NE")), ordered = TRUE))
cat_cols$GarageCond


unique(cat_cols$PavedDrive)
cat_cols$PavedDrive <- as.numeric(factor(cat_cols$PavedDrive, levels = rev(c("Y", "P", "N")), ordered = TRUE))
cat_cols$PavedDrive


unique(cat_cols$PoolQC)
cat_cols$PoolQC <- as.numeric(factor(cat_cols$PoolQC, levels = rev(c("Ex", "Gd", "Fa", "NE")), ordered = TRUE))
cat_cols$PoolQC

#making dummy variables for a few selected categorical features that have no apparent natural order
library(fastDummies)
cat_cols <- fastDummies::dummy_cols(cat_cols, select_columns = c("MSZoning", "Neighborhood", "BldgType", "HouseStyle", "SaleType", "SaleCondition"))

cat_cols <- cat_cols[, !(names(cat_cols) %in% c("MSZoning", "Neighborhood", "BldgType", "HouseStyle", "SaleType", "SaleCondition"))]

cat_cols
#eliminating the less important features for the sake of dimension reduction and more efficiency and accuracy in our model

cat_cols
cat_cols <- cat_cols[, !(names(cat_cols) %in% c("MiscFeature", "Fence", "GarageType", "Electrical", "Heating", "Foundation",
                                                "MasVnrType", "Exterior2nd", "Exterior1st", "RoofMatl", "RoofStyle", "Condition2",
                                                "Condition1", "LotConfig", "LandContour", "CentralAir"))]
cat_cols

PreProcessed_Unnormalized_HP <- cbind(num_cols, cat_cols)
dim(PreProcessed_Unnormalized_HP)

colSums(is.na(PreProcessed_Unnormalized_HP))






#normalizing the data


PreProcessed_HP <- as.data.frame(scale(PreProcessed_Unnormalized_HP))
summary(PreProcessed_HP)
write.csv(PreProcessed_HP, file = "PreProcessed_HousingPrice.csv", row.names = FALSE)
