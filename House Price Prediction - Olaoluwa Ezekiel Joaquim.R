#Installing Packages
install.packages(c("FSelector","mlbench","ggplot2","lars","RColorBrewer","reshape2",
                   "e1071","Amelia","RANN","arm","caret","ipred","knitr"))
library(odbc)
library(dplyr)
library(corrplot)
library(leaps)
library(FSelector)
library(mlbench)
library(ggplot2)
library(lars)
library(RColorBrewer)
library(reshape2)
library(ggplot2)
library(e1071)
library(dplyr)
library(Amelia)
library(RANN)
library(arm)
library(caret)
library(ipred)
library(corrplot)
library(knitr)

#connecting R to the database in order to import data 

con = dbConnect(odbc(),
                Driver = "SQL SERVER",
                Server = "DESKTOP-JGV2UT4",
                Database = "HomePrediction",
                Port = 1433)


#Introduction
#The primary requirement is a real-time effective model to predict final selling 
#price of houses

##Objective
#Initial focus of the project is to gain knowledge of the data and understand the 
#relation between each of the variables to the house's sale price.
#Later, further statistical analysis will be conducted to see how strongly
#these features effect the house prices the most.

#importing the train data into a dataframe
train = tbl(con,'train')
train  = collect(train)
View(train)

#importing the test data into a dataframe
test = tbl(con,'test')
test  = collect(test)
View(test)

#check for duplicate
sum(duplicated(train))
sum(duplicated(test))

#shape of the data
dim(train)



#list of all features and datatype
str(train)
dim(train)
str(train)


## Data Analysis

# Below is the summary of all missing value information.

# Train Set: Getting No of nos of null columns and if numerical, Yes or No

numCols_tr <- NULL
charCols_tr <- NULL
misc_tr <- NULL
for (i in 1:ncol(train)){
  if(is.numeric(train[,i])){
    numCols_tr <- c(numCols_tr, names(train)[i])
  }
  else if(is.character(train[,i])){
    charCols_tr <- c(charCols_tr,names(train)[i])
  }
  else {
    misc_tr <- c(misc_tr,names(train)[i])
  }
}
mis_vars <-data.frame(colSums(sapply(train, is.na)))
mis_vars$num_yn <- "Z"
for(i in 1:nrow(mis_vars)){
  if( rownames(mis_vars)[i] %in% numCols_tr){
    mis_vars[i,2] <- "Y"
  }else{
    mis_vars[i,2] <- "N"
  }
}
colnames(mis_vars) <- c('No_of_NAs', 'Numerical(y/n)')
kable(subset(mis_vars, No_of_NAs > 0 ))


# Test set: Getting No of nos of null columns and if numerical, Yes or No

numCols_te <- NULL
charCols_te <- NULL
misc_te <- NULL
for (i in 1:ncol(test)){
  if(is.numeric(test[,i])){
    numCols_te <- c(numCols_te,names(test)[i])
  }
  else if(is.character(test[,i])){
    charCols_te <- c(charCols_te,names(test)[i])
  }
  else {
    misc_te <- c(misc_te,names(test)[i])
  }
}
mis_vars_te <-data.frame(colSums(sapply(test, is.na)))
mis_vars_te$num_yn <- "Z"
for(i in 1:nrow(mis_vars_te)){
  if( rownames(mis_vars_te)[i] %in% numCols_te){
    mis_vars_te[i,2] <- "Y"
  }else{
    mis_vars_te[i,2] <- "N"
  }
}
colnames(mis_vars_te) <- c('No_of_NAs','Numerical(y/n)')
mis_vars_te <- mis_vars_te[order(mis_vars_te$`Numerical(y/n)`),]
kable(subset(mis_vars_te, No_of_NAs > 0 ))



### Data Cleaning

# summary of the data
summary(train)
str(train)


#converting to numerical data first

train$Id <- as.numeric(train$Id)
train$GarageYrBlt <- as.numeric(train$GarageYrBlt)
train$LotFrontage <- as.numeric(train$LotFrontage)
train$MasVnrArea <- as.numeric(train$MasVnrArea)
train$BsmtFinSF1 <- as.numeric(train$BsmtFinSF1)
train$BsmtFinSF2 <- as.numeric(train$BsmtFinSF2)
train$'_2ndFlrSF' <- as.numeric(train$'_2ndFlrSF')
train$LowQualFinSF  <- as.numeric(train$LowQualFinSF)
train$BsmtFullBath <- as.numeric(train$BsmtFullBath)
train$FullBath  <- as.numeric(train$FullBath)
train$HalfBath  <- as.numeric(train$HalfBath)
train$BedroomAbvGr  <- as.numeric(train$BedroomAbvGr)
train$BsmtHalfBath <- as.numeric(train$BsmtHalfBath)
train$KitchenAbvGr <- as.numeric(train$KitchenAbvGr)
train$Fireplaces <- as.numeric(train$Fireplaces)
train$GarageCars  <- as.numeric(train$GarageCars)
train$GarageArea  <- as.numeric(train$GarageArea)
train$WoodDeckSF   <- as.numeric(train$WoodDeckSF)
train$OpenPorchSF  <- as.numeric(train$OpenPorchSF)
train$EnclosedPorch <- as.numeric(train$EnclosedPorch)
train$'_3SsnPorch'  <- as.numeric(train$'_3SsnPorch')
train$ScreenPorch <- as.numeric(train$ScreenPorch)
train$PoolArea  <- as.numeric(train$PoolArea)
train$MiscVal  <- as.numeric(train$MiscVal)
train$ MoSold   <- as.numeric(train$ MoSold )

## Numeric Variables
#NAs in numeric variables: Since these variables have an impact on the outcome
#variables, they can not be ignored. Also, the number of missing values for each
#variable is significantly higher which might introduce a substantial amount of 
#bias or create reductions in efficiency. To avoid this, Imputation would be 
#performed  on these variables. 



# Replacing NA Values

# Numeric variables
# Train Set
bagImpute_tr <- predict(preProcess(train[,which(names(train) %in% 
                                                  c('GarageYrBlt', 'MasVnrArea', 'LotFrontage'))], 
                                   method = c("bagImpute")), 
                        train[,which(names(train) %in% 
                                       c('GarageYrBlt', 'MasVnrArea', 'LotFrontage'))])
train$GarageYrBlt <- round(bagImpute_tr$GarageYrBlt)
train$MasVnrArea <- bagImpute_tr$MasVnrArea
train$LotFrontage <- bagImpute_tr$LotFrontage

# Test Set
summary(test)

#converting to numeric first before converting
test$LotFrontage <- as.numeric(test$LotFrontage)
test$OverallCond <- as.numeric(test$OverallCond)
test$MasVnrArea <- as.numeric(test$MasVnrArea)
test$BsmtFinSF1 <- as.numeric(test$BsmtFinSF1)
test$BsmtFinSF2 <- as.numeric(test$BsmtFinSF2)
test$BsmtUnfSF <- as.numeric(test$BsmtUnfSF)
test$TotalBsmtSF <- as.numeric(test$TotalBsmtSF)
test$'_2ndFlrSF' <- as.numeric(test$'_2ndFlrSF')
test$LowQualFinSF  <- as.numeric(test$LowQualFinSF)
test$BsmtFullBath <- as.numeric(test$BsmtFullBath)
test$BsmtHalfBath <- as.numeric(test$BsmtHalfBath)
test$FullBath  <- as.numeric(test$FullBath)
test$HalfBath  <- as.numeric(test$HalfBath)
test$BedroomAbvGr  <- as.numeric(test$BedroomAbvGr)
test$KitchenAbvGr <- as.numeric(test$KitchenAbvGr)
test$Fireplaces <- as.numeric(test$Fireplaces)
test$GarageYrBlt <- as.numeric(test$GarageYrBlt)
test$GarageCars  <- as.numeric(test$GarageCars)
test$GarageArea  <- as.numeric(test$GarageArea)
test$WoodDeckSF   <- as.numeric(test$WoodDeckSF)
test$OpenPorchSF  <- as.numeric(test$OpenPorchSF)
test$EnclosedPorch <- as.numeric(test$EnclosedPorch)
test$'_3SsnPorch'  <- as.numeric(test$'_3SsnPorch')
test$ScreenPorch <- as.numeric(test$ScreenPorch)
test$PoolArea  <- as.numeric(test$PoolArea)
test$MiscVal  <- as.numeric(test$MiscVal)
test$MoSold   <- as.numeric(test$MoSold )


# Test Set using Bagimpute to fill in NAs

bagImpute_te <- predict(preProcess(test[,which(names(test) %in% 
                                                 c('GarageYrBlt', 'MasVnrArea', 'LotFrontage',
                                                   'BsmtFinSF1','BsmtFinSF2','BsmtUnfSF',
                                                   'TotalBsmtSF','BsmtFullBath','GarageCars',
                                                   'GarageArea', 'BsmtHalfBath'))], 
                                   method = c("bagImpute")), 
                        test[,which(names(test) %in% 
                                      c('GarageYrBlt', 'MasVnrArea', 'LotFrontage',
                                        'BsmtFinSF1','BsmtFinSF2','BsmtUnfSF',
                                        'TotalBsmtSF','BsmtFullBath','GarageCars',
                                        'GarageArea', 'BsmtHalfBath'))])

test$GarageYrBlt <- round(bagImpute_te$GarageYrBlt)
test$MasVnrArea <- bagImpute_te$MasVnrArea
test$LotFrontage <- bagImpute_te$LotFrontage
test$BsmtFinSF1 <- bagImpute_te$BsmtFinSF1
test$BsmtFinSF2 <- bagImpute_te$BsmtFinSF2
test$BsmtUnfSF <- bagImpute_te$BsmtUnfSF
test$TotalBsmtSF <- bagImpute_te$TotalBsmtSF
test$BsmtFullBath <- bagImpute_te$BsmtFullBath
test$BsmtHalfBath <- bagImpute_te$BsmtHalfBath
test$GarageCars <- round(bagImpute_te$GarageCars)
test$GarageArea <- bagImpute_te$GarageArea





#NA in character variables: All character variables contain the category of 
#a certain feature available in the house. As per the data description, 
#NA in such cases means absence of that feature. Hence, replacing NAs with
#proper descriptive words. All the charactor variables are converted to the
#factor variables


# Character Variables
# Train set
train$Alley <- ifelse(is.na(train$Alley),"No Alley", train$Alley)
train$MasVnrType <- ifelse(is.na(train$MasVnrType),"None", train$MasVnrType)
train$Electrical<- ifelse(is.na(train$Electrical),"None",train$Electrical)
train$BsmtQual <- ifelse(is.na(train$BsmtQual),"No Bsmt", train$BsmtQual)
train$BsmtCond <- ifelse(is.na(train$BsmtCond),"No Bsmt", train$BsmtCond)
train$BsmtExposure <- ifelse(is.na(train$BsmtExposure),"No Bsmt", train$BsmtExposure)
train$BsmtFinType1 <- ifelse(is.na(train$BsmtFinType1),"No Bsmt", train$BsmtFinType1)
train$BsmtFinType2 <- ifelse(is.na(train$BsmtFinType2),"No Bsmt", train$BsmtFinType2)
train$FireplaceQu <- ifelse(is.na(train$FireplaceQu),"No Fireplace", train$FireplaceQu)
train$GarageType <- ifelse(is.na(train$GarageType),"No Garage", train$GarageType)
train$GarageFinish <- ifelse(is.na(train$GarageFinish),"No Garage", train$GarageFinish)
train$GarageQual <- ifelse(is.na(train$GarageQual),"No Garage", train$GarageQual)
train$GarageCond <- ifelse(is.na(train$GarageCond),"No Garage", train$GarageCond)
train$PoolQC <- ifelse(is.na(train$PoolQC),"No Pool", train$PoolQC)
train$Fence <- ifelse(is.na(train$Fence),"No Fence", train$Fence)
train$MiscFeature <- ifelse(is.na(train$MiscFeature),"No Fence", train$MiscFeature)


# Test Set
test$Alley <- ifelse(is.na(test$Alley),"No Alley", test$Alley)
test$MasVnrType <- ifelse(is.na(test$MasVnrType),"None", test$MasVnrType)
test$BsmtQual <- ifelse(is.na(test$BsmtQual),"No Bsmt", test$BsmtQual)
test$BsmtCond <- ifelse(is.na(test$BsmtCond),"No Bsmt", test$BsmtCond)
test$BsmtExposure <- ifelse(is.na(test$BsmtExposure),"No Bsmt", test$BsmtExposure)
test$BsmtFinType1 <- ifelse(is.na(test$BsmtFinType1),"No Bsmt", test$BsmtFinType1)
test$BsmtFinType2 <- ifelse(is.na(test$BsmtFinType2),"No Bsmt", test$BsmtFinType2)
test$FireplaceQu <- ifelse(is.na(test$FireplaceQu),"No Fireplace", test$FireplaceQu)
test$GarageType <- ifelse(is.na(test$GarageType),"No Garage", test$GarageType)
test$GarageFinish <- ifelse(is.na(test$GarageFinish),"No Garage", test$GarageFinish)
test$GarageQual <- ifelse(is.na(test$GarageQual),"No Garage", test$GarageQual)
test$GarageCond <- ifelse(is.na(test$GarageCond),"No Garage", test$GarageCond)
test$PoolQC <- ifelse(is.na(test$PoolQC),"No Pool", test$PoolQC)
test$Fence <- ifelse(is.na(test$Fence),"No Fence", test$Fence)
test$MiscFeature <- ifelse(is.na(test$MiscFeature),"No Fence", test$MiscFeature)
test$MSZoning <- ifelse(is.na(test$MSZoning),"RM",test$MSZoning)
test$Utilities <- ifelse(is.na(test$Utilities),"AllPub",test$Utilities)
test$Exterior1st <- ifelse(is.na(test$Exterior1st),"WdShing",test$Exterior1st)
test$Exterior2nd <- ifelse(is.na(test$Exterior2nd),"Stone", test$Exterior2nd)
test$BsmtHalfBath <- ifelse(is.na(test$BsmtHalfBath),0,test$BsmtHalfBath)
test$KitchenQual <- ifelse(is.na(test$KitchenQual),"TA",test$KitchenQual)
test$Functional <- ifelse(is.na(test$Functional),"Typ",test$Functional)
test$SaleType <- ifelse(is.na(test$SaleType),"WD",test$SaleType)



View(train)


## Exploratory Data Analysis

char_var <- names(train)[which(sapply(train, is.character))]
for(name in char_var){
  train[[name]] <- factor(train[[name]])
}
char_var <- names(test)[which(sapply(test, is.character))]
for(name in char_var){
  test[[name]] <- factor(test[[name]])
} 

# Sale Price - Understanding the dependent variable

cat("Mean : " , mean(train$SalePrice))
cat("Median : " , median(train$SalePrice))
cat("Standard Deviation : " , sd(train$SalePrice))


# Mean > Median - Data is right skewed - Plotting to determine it

hist(train$SalePrice, xlim = c(1000,800000), main = "Sale Price Distribution", 
     xlab = 'Sale Price', freq = FALSE, col=brewer.pal(8,"Set3"),las = 3,breaks = 190)
lines(density(train$SalePrice))

# apply log to distribute it normally

hist(log(train$SalePrice),main = "Log of Sale Price Distribution",
     xlab = 'Sale Price',freq = FALSE,col=brewer.pal(8,"Set3"),las = 3,breaks = 190)
lines(density(log(train$SalePrice)))

# Finding Correlations
numeric_var <- names(train)[which(sapply(train, is.numeric))]
df_corr <- data.frame(cor(train[,(numeric_var)], method="pearson"))

#Correlation with Each variables and Sale Price:
df_sale_corr <- data.frame(abs(df_corr[]))
View(df_sale_corr)


## Top 10 correlated features

top10Corr <- df_sale_corr[2:11,]
colnames(top10Corr)<- c("Cors","Features")
row.names(top10Corr) <- NULL
kable(top10Corr[,c(2,1)])

## Exploring 5 features using Plot
options(scipen=5)
ggplot() +
  geom_point(aes(x = train$GrLivArea, y = train$SalePrice),colour = 'red') +
  geom_line(aes(x = train$GrLivArea, y = predict(lm(train$SalePrice~train$GrLivArea), 
                                                 newdata = train)),colour = 'black') +
  ggtitle('Variation of SalePrice - Living Area') +
  xlab('Living Area') +
  ylab('Sales Price')

# This plot clearly shows that the Living area above grade has a strong positive linear # relationship with the Sale price


ggplot() +
  geom_point(aes(x = train$GarageArea, y = train$SalePrice),colour = 'red') +
  geom_line(aes(x = train$GarageArea, y = predict(lm(train$SalePrice~train$GarageArea), 
                                                  newdata = train)),colour = 'black') +
  ggtitle('Variation of SalePrice - Garage Area') +
  xlab('Garage Area') +
  ylab('Sales Price')

# This plot clearly shows that the Garage Area has a strong positive linear relationship with the Sale price.But, this graph has lot of data points 
#concentrated at units '0' which results in an anomaly. There are considerable 5
#amount of houses with no basement at all. That resulted in this anomaly



ggplot() +
  geom_point(aes(x = train$TotalBsmtSF, y = train$SalePrice),colour = 'red') +
  geom_line(aes(x = train$TotalBsmtSF, y = predict(lm(train$SalePrice~train$TotalBsmtSF), 
                                                   newdata = train)),colour = 'black') +
  ggtitle('Variation of SalePrice - Total Basement') +
  xlab('Total Basement') +
  ylab('Sales Price')

#This plot clearly shows that the Total Basement Area has a strong positive linear relationship with the Sale price.But, this graph has lot of data points concentrated
# at units '0' which results in an anomaly. There are considerable amount of houses with no basement at all. That resulted in this anomaly


# Understanding OverallQual and GarageCars with SalesPrice

ggplot(train, aes(factor(OverallQual), SalePrice)) + geom_violin(scale = "width")

ggplot(train, aes(factor(GarageCars), SalePrice)) + geom_violin(scale = "width")

boxplot(train$SalePrice~train$GarageCars, data=train, notch=FALSE, 
        col=(c("gold","red")),
        main="Sales Price - Garage Cars", xlab="Garage Cars", ylab = "Sales Price")

boxplot(train$SalePrice~train$OverallQual, data=train, notch=FALSE, 
        col=(c("gold","red")), main="Sales Price - Overall Quality", 
        xlab="Overall Quality", ylab = "Sales Price")

# Correlation plot of all numeric Variables
correlations <- cor(train[, numeric_var], use = "everything")
corrplot(correlations)



## MODEL AND MODEL DEVELOPMENT

rmse <- function(y, yhat) {sqrt(mean((y - yhat)^2))}

#Creating a base Linear Model using all the predictors.
lm_all <- standardize(
  lm(
    SalePrice ~ MSSubClass +  MSZoning +  LotFrontage +  LotArea + Street +      
      Alley +        LotShape +    
      LandContour +  Utilities + LotConfig + LandSlope + Neighborhood + Condition1 + 
      Condition2 +   BldgType +    
      HouseStyle +   OverallQual + OverallCond + YearBuilt + YearRemodAdd + RoofStyle +   
      RoofMatl +     Exterior1st + 
      Exterior2nd +  MasVnrType + MasVnrArea +  ExterQual +  ExterCond + Foundation +  
      BsmtQual +     BsmtCond +    
      BsmtExposure + BsmtFinType1 + BsmtFinSF1 + BsmtFinType2 + BsmtFinSF2 + BsmtUnfSF +  
      TotalBsmtSF +  Heating +     
      HeatingQC + CentralAir +   Electrical + train$'_1stFlrSF' + train$'_2ndFlrSF' + LowQualFinSF 
    + GrLivArea +    BsmtFullBath +
      BsmtHalfBath + FullBath + HalfBath + BedroomAbvGr + KitchenAbvGr + KitchenQual
    +  TotRmsAbvGrd + Functional + Fireplaces +  FireplaceQu + GarageType +
      GarageYrBlt +  GarageFinish + GarageCars + GarageArea + GarageQual +  
      GarageCond +   PavedDrive +   WoodDeckSF +   OpenPorchSF +  
      EnclosedPorch +  train$'_3SsnPorch' +   ScreenPorch +  PoolArea +    
      PoolQC +  Fence + MiscFeature +  MiscVal +      MoSold +  YrSold +  
      SaleType +     SaleCondition
    , data = train
  )
)
summary(lm_all)
cat("RMSE of the baseline model with all predictors ", 
    rmse(train$SalePrice, predict(lm_all)))

# Checking Multi Collinearity - to eliminate highly correlated features - threshold value = 0.6

corr_num <- data.frame(cor(train[,(numeric_var)], method="pearson"))
mul_cor <- NULL 
for(i in 1:nrow(corr_num)){
  for(j in 1:i){
    df_temp <- NULL
    if(!is.na(corr_num[i,j])){
      if(corr_num[i,j] >= 0.6 && corr_num[i,j] != 1){
        df_temp$Colname1 <- names(corr_num)[i]
        df_temp$Colname2 <- names(corr_num)[j]
        df_temp$corr <- corr_num[i,j]
        mul_cor <- rbind(mul_cor,df_temp)
      }
    }
  }
}
mul_cor <- as.data.frame(mul_cor)
mul_cor <- subset(mul_cor, Colname1 != "SalePrice")
kable(mul_cor, row.names=FALSE)

#Base model served two purposes.
#1.This helps to compare the performance of base model with the future models 
# and see if the there is any improvement after selecting the best variables
#2.This also helped in checking col linearity between categorical variables
#Removing the predictor with NAs as coefficient because of multicolinearity. 

#Converting this feature to numeric 
train$TotalBsmtSF <- as.numeric(train$TotalBsmtSF)
test$TotalBsmtSF <- as.numeric(test$TotalBsmtSF)


lm_mod1 <- standardize(
  lm(
    SalePrice ~ MSSubClass +   MSZoning +     LotFrontage +  LotArea +      Street + 
      Alley +        LotShape +    
      LandContour +  Utilities +  LotConfig + LandSlope + Neighborhood +
      Condition1 +   Condition2 +   BldgType +    
      HouseStyle +   OverallQual +  OverallCond +  YearBuilt + YearRemodAdd +
      RoofStyle +    RoofMatl +     Exterior1st + 
      MasVnrType +   MasVnrArea +   ExterQual +    ExterCond +    Foundation + BsmtQual +    
      BsmtExposure  + BsmtFinSF1 +   BsmtFinType2 + BsmtFinSF2 +   BsmtUnfSF + Heating +     
      HeatingQC +    CentralAir +      train$'_1stFlrSF' + train$'_2ndFlrSF' +   
      LowQualFinSF  +    BsmtFullBath +
      BsmtHalfBath + FullBath +     HalfBath +     BedroomAbvGr + KitchenAbvGr
    + KitchenQual +  TotRmsAbvGrd + Functional +  
      Fireplaces +   FireplaceQu +  GarageType +   GarageYrBlt +  
      GarageCars +   GarageArea  +  
      PavedDrive +   WoodDeckSF +   OpenPorchSF +  EnclosedPorch +  train$'_3SsnPorch' + 
      ScreenPorch +  PoolArea +    
      PoolQC +       Fence +        MiscFeature +  MiscVal +      MoSold + 
      YrSold +       SaleType +     SaleCondition
    , data = train
  )
)
cat("RMSE of the model after removing multicollinear variables with all predictors ", 
    rmse(train$SalePrice, predict(lm_mod1)))

#Picking predictors basing on the Beta coeffiencients and P values.

lm_df <- as.data.frame(coef(summary(lm_mod1)))
lm_df$estimate_absolute_estimates <- abs(lm_df$Estimate)
kable(lm_df[order(-lm_df$estimate_absolute_estimates, -lm_df$`Pr(>|t|)`),][])


#New model after selecting the strong predictors picked from above, and strongly corelated variables. 

lm_mod3 <- lm(SalePrice ~ RoofMatl+Condition2+PoolQC+OverallQual+RoofStyle+
                OverallCond+YearBuilt+GarageArea+GrLivArea+TotalBsmtSF,data=train)
summary(lm_mod3)
cat("RMSE of the model with selected variables", 
    rmse(train$SalePrice, predict(lm_mod3))) 

#New model after selecting the strong predictors picked from above, and strongly corelated variables. 

lm_mod4 <- lm(SalePrice ~ RoofMatl+Condition2+OverallQual+
                OverallCond+YearBuilt+GarageArea+GrLivArea+TotalBsmtSF,data=train)
summary(lm_mod4)
cat("RMSE of the model with selected variables", 
    rmse(train$SalePrice, predict(lm_mod4))) 



# that above features are considered more often than other available variables.
# Exploring the residual plot of the final model
plot(lm_mod4,which = 1)

#Modifying the model further by
#1.	Converting Quality variable into factor variable to take into account the bin like effect on the SalePrice
#2.	Adding a interaction between Neighborhood ad Quality
train$OverallQual <- factor(train$OverallQual)
test$OverallQual <- factor(test$OverallQual)
lm_mod5 <- (lm(log(SalePrice) ~ OverallQual + TotalBsmtSF  + GrLivArea  + 
                 GarageCars + Neighborhood + OverallCond + LotArea + 
                 Neighborhood:OverallQual + YearBuilt 
               ,data=train))
summary(standardize(lm_mod5))
cat("RMSE of the final model", rmse(train$SalePrice, exp(predict(lm_mod5))))  


# Divide the Train Dataset to test and train sets, for predicting the RMSE and R2

train_part <- sample(nrow(train), 0.7*nrow(train), replace = F)
set.seed(123)
new_test <- train[-train_part, ]
new_prediction <- predict(lm_mod5, newdata= new_test)
cat("RMSE on Test model", rmse(new_test$SalePrice, exp(new_prediction)))  
R2 <- function(y, yhat, ybar, digits = 2) {
  1 - sum((y - yhat)^2)/sum((y - ybar)^2)
}
actualR2<-R2(y = new_test$SalePrice, yhat = exp(predict(lm_mod5,newdata = new_test)), 
             mean(new_test$SalePrice))
round(actualR2,2)

# Performing Cross Validation on Training Set
lm_model_cv <-train(log(SalePrice) ~ OverallQual + TotalBsmtSF + GrLivArea  + 
                      GarageCars + Neighborhood + OverallCond + LotArea  + YearBuilt,
                    data = train,
                    method= "lm",
                    trControl= trainControl(method="cv", number=10))
summary(lm_model_cv)
cat("RMSE: ", rmse(train$SalePrice, exp(predict(lm_model_cv, newdata= train))))
lm_model_cv$results$Rsquared



train$TotalBsmtSF <- as.numeric(train$TotalBsmtSF)
test$TotalBsmtSF <- as.numeric(test$TotalBsmtSF)


# Generating CSV file for the result
prediction_test <- predict(lm_mod5, newdata= test)
dt.result <- NULL
dt.result$Id <- test$Id
dt.result$SalePrice <- prediction_test
dt.result <- as.data.frame(dt.result, row.names = NULL)
write.csv(dt.result,"HousePricePredict.csv", row.names = FALSE)



