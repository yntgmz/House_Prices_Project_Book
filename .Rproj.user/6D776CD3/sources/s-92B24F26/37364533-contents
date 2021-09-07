#Libraries
library(bookdown)
library(kableExtra)
library(knitr)          #for tables
library(DataExplorer)   #for plot missing 
library(RColorBrewer)   #for color palattes
library(data.table)     #for custom function
library(ggplot2)        #for general plotting
library("GGally")       #for ggcor
library(Rcpp)           #missmap
library(Amelia)         #missmap
library(dplyr)          #mutate(), select()
library(ggridges)       #for ridge plot
library(stringr)        #for looking at columns containg "string"
library(imputeMissings) #impute missing values with median/mode or random forest
library(mice)           #impute missing values
library(forcats)        #for factor reorder
library(caret)          #for hot one encoding
library(plyr)           #to reorder factors 
library(vip)            #to show 
library(psych)          #corplot
require(Metrics)        #for rmse
library(DiagrammeR)     #for workflow diagram
library(gridExtra)      #for tables and graps
library(data.table)     #to create tables
library(RColorBrewer)   #For graph colors
library(naniar)         #plot missing values
library(car)            #for VIF
library(vip)            #for variable importance
library(gbm)            #for gbm model and plot
library(Metrics)        #for RMSE
library(Boruta)         #Boruta() for feature selection
library(gridExtra)
library(xgboost)
library(doParallel)
library(lattice)        #for plots
library(Matrix)
library(glmnet)         #fitting regularized models  
library(coefplot)       #for extracting coefficients

#Import Data
# Import Train and Tests Datasets
train_raw<-read.csv("train.csv",stringsAsFactors = TRUE) # 1460 rows,  81 columns
test_raw<-read.csv("test.csv", stringsAsFactors = TRUE)

#Merge Datasets for preprocessing steps. Separate into train and test sets later for modeling and prediction. 
test_raw$"SalePrice"<-0                   # Create "SalePrice" feature for test set
data_full<- rbind(train_raw, test_raw)    # Combine hdb_train and hdb_test

#Save ID column for submissiom
test_ID <-test_raw$Id

#Data Cleaning_Garage
garage_features <- names(data_full)[sapply(names(data_full), function(x) str_detect(x, "Garage"))]
#Get most commom values
kable(names(sapply(data_full[which( data_full$GarageCars == 1 & data_full$GarageType=="Detchd" ) ,garage_features], function(x) sort(table(x),  decreasing=TRUE)[1])), col.names = "Most Common Value per Feature") 
# Replace Values Manually
data_full[2127,'GarageQual']    = 'TA'
data_full[2127, 'GarageFinish'] = 'Unf'
data_full[2127, 'GarageCond']   = 'TA'
data_full$GarageType<-factor(data_full$GarageType, levels=c("None","2Types","Attchd",  "Basment","BuiltIn","CarPort","Detchd" ), ordered=FALSE)
#Update Garage type for row 2577
data_full[2577, 'GarageType'] = 'None'

#Data Cleaning_Garage year error
data_full$GarageYrBlt[data_full$GarageYrBlt==2207] <- 2007

#Data Cleaning_Garage year built
data_full$GarageYrBlt[is.na(data_full$GarageYrBlt)]<-data_full$YearBuilt[is.na(data_full$GarageYrBlt)]

#Data Cleaning_Garage year built
data_full$GarageYrBlt[is.na(data_full$GarageYrBlt)]<-data_full$YearBuilt[is.na(data_full$GarageYrBlt)]

#Data Cleaning_Basement
basement_features <- names(data_full)[sapply(names(data_full), function(x) str_detect(x, "Bsmt"))]
#View(data_full[is.na(data_full$BsmtCond), basement_features]) 
data_full[c(2041, 2186,2525),basement_features]
#names(which.max(table(data_full$BsmtCond)))
data_full[c(2041,2186, 2525),'BsmtCond']=names(which.max(table(data_full$BsmtCond)))

#Data Cleaning_Pool
pool_features <- names(data_full)[sapply(names(data_full), function(x) str_detect(x, "Pool"))]
data_full[c(2421,2504,2600),pool_features]
pool_na<-which(is.na(data_full$PoolQC) & data_full$PoolArea >0)
aggregate(data=data_full, PoolArea~PoolQC, mean, na.rm=TRUE)
data_full$PoolArea[which(is.na(data_full$PoolQC) & data_full$PoolArea >0)]
#Replace NA with most common values
data_full$PoolQC[data_full$Id == 2421] <- "Ex"
data_full$PoolQC[data_full$Id == 2504] <- "Ex"
data_full$PoolQC[data_full$Id == 2600] <- "Fa"

#Data Cleaning_Masonry 
masonry_features <- names(data_full)[sapply(names(data_full), function(x) str_detect(x, "Mas"))]
mas_na<-which(is.na(data_full$MasVnrType) & data_full$MasVnrArea >0)
data_full[2611,masonry_features]
data_full$MasVnrArea[which(is.na(data_full$MasVnrType) & data_full$MasVnrArea >0)]  #198
aggregate(data=data_full, MasVnrArea~MasVnrType, mean, na.rm=TRUE)
names(which.max(table(data_full$MasVnrType)))
summary(data_full$MasVnrType)  #most common is BrkFace
data_full$MasVnrType[data_full$Id == 2611] <- "BrkFace"

#Data Cleaning_few NA's
na_m <- c( "Utilities", "Functional", "Exterior1st", "Exterior2nd", "Electrical", "KitchenQual", "SaleType")
data_full[,na_m] <- apply(data_full[,na_m], 2,
                          function(x) {replace(x, is.na(x), names(which.max(table(x))))})

#Data Cleaning_None

na_none <- c("GarageFinish", "GarageQual", "GarageType", "GarageCond", "BsmtCond", "BsmtExposure", "BsmtQual", "BsmtFinType1", "BsmtFinType2", "FireplaceQu", "EnclosedPorch","PoolQC","MiscFeature","Alley", "Fence", "MasVnrType")

data_full[,na_none] <- apply(data_full[,na_none], 2,
                             function(x) {replace(x, is.na(x), "None")})

#Data Cleaning_Zero
na_z <- c("BsmtFinSF1","BsmtFinSF2", "BsmtUnfSF", "TotalBsmtSF", "BsmtFullBath", "BsmtHalfBath","GarageCars","GarageArea", "MasVnrArea")

data_full[,na_z] <- apply(data_full[,na_z], 2,
                          function(x) {replace(x, is.na(x), 0)})

#Impute LotFrontage (This takes a long time to run)
library(mice)
set.seed(123)
mice_rf_mod<- mice(data_full[, !names(data_full) %in% c('Id', 'SalePrice')], method ='rf', printFlag = FALSE)
mice_output <- complete(mice_rf_mod)
#Inpute LotFrontage
sum(is.na(data_full$LotFrontage))
data_full$LotFrontage[is.na(data_full$LotFrontage)] <- mice_output$LotFrontage[is.na(data_full$LotFrontage)]
sum(is.na(data_full$LotFrontage))
#Inpute MSZoning
set.seed(123)
sum(is.na(data_full$MSZoning))
data_full$MSZoning[is.na(data_full$MSZoning)] <- mice_output$MSZoning[is.na(data_full$MSZoning)]
sum(is.na(data_full$MSZoning))

# Comfirm all NA's are clared
sum(is.na(data_full))           


#Data Transformation

#Correct Data Types-Numeric
to_num <-c( "LotFrontage", "MasVnrArea", "BsmtFinSF1", "BsmtFinSF2", "BsmtUnfSF", "TotalBsmtSF", "GarageArea","X1stFlrSF", "X2ndFlrSF", "LowQualFinSF", "GrLivArea", "X3SsnPorch", "PoolArea", "WoodDeckSF", "SalePrice" ,"EnclosedPorch", "ScreenPorch", "LotArea",  "MiscVal", "OpenPorchSF")
data_full[,to_num] <- lapply(data_full[,to_num], as.numeric)
to_int<-c("BsmtFullBath","BsmtHalfBath","FullBath", "HalfBath", "BedroomAbvGr", "KitchenAbvGr", "TotRmsAbvGrd", "Fireplaces", "GarageCars", "MoSold", "YrSold","YearRemodAdd","GarageYrBlt","YearBuilt")
data_full[,to_int] <- lapply(data_full[,to_int], as.integer)

#Correct Data Types-Categorical.**

nom_to_cat <-c("MasVnrType","MSSubClass", "Neighborhood","CentralAir", "SaleCondition", "HouseStyle", "Street", "Alley", "LandContour", "Condition1", "Condition2", "BldgType", "RoofStyle", "RoofMatl", "Exterior1st", "Exterior2nd", "Foundation", "BsmtExposure", "Heating", "GarageType", "PavedDrive", "MiscFeature", "SaleType")
data_full[,nom_to_cat] <- lapply(data_full[,nom_to_cat], factor)

#Add levels to ordinal categorical
data_full$Utilities<-factor(data_full$Utilities,levels=c("ELO","NoSeWa","NoSewr","AllPub"), ordered=TRUE)

data_full$ExterQual<-factor(data_full$ExterQual, levels=c("Po","Fa","TA","Gd","Ex"), ordered=TRUE)

data_full$ExterCond<-factor(data_full$ExterCond, levels=c("Po","Fa","TA","Gd","Ex"), ordered=TRUE)

data_full$FireplaceQu<-factor(data_full$FireplaceQu, levels=c("None","Po","Fa","TA","Gd","Ex"), ordered=TRUE)

data_full$Functional<-factor(data_full$Functional,levels=c("Sal", "Sev", "Maj2", "Maj1","Mod","Min2","Min1","Typ"), ordered=TRUE)

data_full$PoolQC<-factor(data_full$PoolQC,levels=c("None","Fa","TA","Gd","Ex"), ordered=TRUE)

data_full$BsmtCond<-factor(data_full$BsmtCond, levels=c("None","Po","Fa","TA","Gd","Ex"), ordered=TRUE)

data_full$BsmtQual<-factor(data_full$BsmtQual,levels=c("None","Po","Fa","TA","Gd","Ex"), ordered=TRUE)

data_full$BsmtExposure<-factor(data_full$BsmtExposure, levels=c("None", "No", "Mn", "Av", "Gd"), ordered=TRUE)

data_full$BsmtFinType1<-factor(data_full$BsmtFinType1, levels=c("None","Unf","LwQ","Rec","BLQ","ALQ","GLQ"), ordered=TRUE)

data_full$BsmtFinType2<-factor(data_full$BsmtFinType2, levels=c("None","Unf","LwQ","Rec","BLQ","ALQ","GLQ"), ordered=TRUE)

data_full$HeatingQC<-factor(data_full$HeatingQC, levels=c("Po", "Fa", "TA", "Gd", "Ex"), ordered=TRUE)

data_full$KitchenQual<-factor(data_full$KitchenQual, levels=c("Po", "Fa", "TA", "Gd", "Ex"), ordered=TRUE)

data_full$GarageQual<-factor(data_full$GarageQual, levels=c("None","Po","Fa","TA","Gd","Ex"), ordered=TRUE)

data_full$GarageCond<-factor(data_full$GarageCond, levels=c("None","Po","Fa","TA","Gd","Ex"), ordered=TRUE)

data_full$Electrical<-factor(data_full$Electrical, levels=c("Mix","FuseP","FuseF","FuseA","SBrkr"), ordered=TRUE)

data_full$GarageFinish<-factor(data_full$GarageFinish, levels=c("None","Unf","RFn","Fin"), ordered=TRUE)

data_full$PavedDrive<-factor(data_full$PavedDrive, levels=c("N","P","Y"), ordered=TRUE)

data_full$Fence<-factor(data_full$Fence, levels=c("None","MnWw","GdWo", "MnPrv", "GdPrv"), ordered=TRUE)

data_full$OverallQual<-factor(data_full$OverallQual, levels=c("1", "2","3","4", "5", "6", "7", "8", "9", "10"), ordered=TRUE)

data_full$OverallCond<-factor(data_full$OverallCond, levels=c("1", "2","3","4", "5", "6", "7", "8", "9", "10"), ordered=TRUE)

data_full$GarageType<-factor(data_full$GarageType, levels=c("None","2Types","Attchd", "Basment","BuiltIn","CarPort","Detchd" ), ordered=FALSE)

data_full$LandSlope<-factor(data_full$LandSlope, levels=c("Gtl","Mod","Sev" ), ordered=TRUE)

data_full$LotShape<-factor(data_full$LotShape, levels=c("IR1","IR2", "IR3", "Reg" ), ordered=TRUE)

#Confirm chages
integer_var <- which(lapply(data_full,class) == "integer")     # Integer features
cat(length(integer_var),"Discrete Features:", names(integer_var))

numeric_var <- which(lapply(data_full, class) == "numeric")     # Target feature: numeric
cat(length(numeric_var),"Continous Feature:", names(numeric_var ))

factor_var <- which(lapply(data_full, class) == "factor")    # Categorical features
cat(length(factor_var), "Nominal Categorical Features:", names(factor_var))

factor_var2 <- which(sapply(data_full, is.ordered))  # Categorical features
cat(length(factor_var2),"Ordered Categorical Features:", names(factor_var2))

char_var <- which(lapply(data_full, class) == "character") # Zero character types 


#Attribute Construction from Year Features

data_2<-data_full  
data_2_train<-data_2[1:1460,]

data_2_log<-data_2_train
data_2_log$SalePrice<-log(data_2_log$SalePrice)


# Remodeled
data_2['Remod'] <- ifelse(data_2$YearBuilt==data_2$YearRemodAdd, 0, 1) #0=No Remodeling, 1=Remodeling
#New Build 
data_2['NewBuild'] <-  ifelse(data_2$YearBuilt == data_2$YrSold|data_2$YearBuilt+1 == data_2$YrSold, 1,0) 
# Age of property based on last year of dataset
data_2['BldgAge']<- max(data_2$YearBuilt) - data_2$YearBuilt

#Grouping Garage Cars
data_2$GarageCars[data_2$GarageCars==4]<-3
data_2$GarageCars[data_2$GarageCars==5]<-3
data_2$Fireplaces[data_2$Fireplaces==3]<-2
data_2$Fireplaces[data_2$Fireplaces==4]<-2
data_full$Electrical<-factor(data_full$Electrical, levels=c("Mix","FuseP","FuseF","FuseA","SBrkr"), ordered=TRUE)
data_2$Electrical<-recode_factor(data_2$Electrical, "Mix"=1, "FuseP"=2, "FuseF"=3, "FuseA"=3, "SBrkr"=4)
data_2$Functional<-recode_factor(data_2$Functional,  "Sal"=1 , "Sev"=1 , "Maj2"=2 ,"Maj1"=3 ,"Mod"=3,  "Min2"=4 ,"Min1"=4 ,"Typ"=5)

#Combining features
data_2 <- mutate(data_2, BSmtFinSFComb = BsmtFinSF1 + BsmtFinSF2,
                 TotalArea= GrLivArea + TotalBsmtSF,
                 TotalBaths = BsmtFullBath + (0.5*BsmtHalfBath) + FullBath+ (0.5*HalfBath),
                 PorchSF = OpenPorchSF + EnclosedPorch + X3SsnPorch + ScreenPorch)

#Bining 
data_2$Neigh_Cat<-recode_factor(data_2$Neighborhood, 'MeadowV' = 1, 'IDOTRR' = 1, 'BrDale' = 1, 'BrkSide' = 2, 'Edwards' = 2,'OldTown' = 2,'Sawyer' = 2, 'Blueste' = 2, 'SWISU' = 2,  'NPkVill' = 2, 'NAmes' = 2, 'Mitchel' = 2,'SawyerW' = 3,'NWAmes' = 3,  'Gilbert' = 3, 'Blmngtn' = 3, 'CollgCr' = 3, 'Crawfor' = 4, 'ClearCr' = 4,'Somerst' = 4, 'Veenker' = 4, 'Timber' = 4, 'StoneBr' = 5, 'NoRidge' = 5, 'NridgHt' = 5)

data_2$Neigh_Cat <- as.numeric(data_2$Neigh_Cat)

age<-data_2$BldgAge
AgeCat<- case_when(age<= 9 ~ 'New',
                   between(age, 10, 40) ~ 'Mid',
                   between(age, 41, 70) ~ 'Old',
                   age >= 71 ~ 'Antique')

data_2$AgeCat<-as.numeric(factor(AgeCat, levels=c('Antique','Old', 'Mid', 'New'), ordered=TRUE))
head(data_2[,c("BldgAge", "AgeCat","YearBuilt")])

data_2$LastSold <- 2010 - data_2$YrSold  

#Years Since Remodeled 
data_2['YearRemodAdd2'] <- ifelse(data_2$YearBuilt == data_2$YearRemodAdd, 0, data_2$YearRemodAdd) #fix the year remodeled column
head(data_2[,c("YearBuilt","YearRemodAdd", 'YearRemodAdd2','Remod' )])
data_2['TimeSinceRemod'] <- ifelse(data_2$Remod == 1, 2010 - data_2$YearRemodAdd2,0) 
data_2$RemodelFromCat<- case_when(data_2$TimeSinceRemod <= 0 & data_2$NewBuild == 1 ~ 'New',
                                  data_2$TimeSinceRemod <= 0 & data_2$YearRemodAdd2 == 0 ~ 'None',
                                  between(data_2$TimeSinceRemod, 0, 5) ~ 'Recent',
                                  between(data_2$TimeSinceRemod, 6, 10) ~ 'Mid',
                                  between(data_2$TimeSinceRemod, 11, 20) ~ 'Old',
                                  data_2$TimeSinceRemod >= 20 ~ 'Outdated')
data_2$RemodelFromCat<-factor(data_2$RemodelFromCat, levels=c('None','Outdated','Old', 'Mid','Recent', 'New'), ordered=TRUE)
data_2$RemodelFromCat<-as.integer(data_2$RemodelFromCat)

#Bining by Season
data_2$SeasonSale <- mapvalues(data_2$MoSold, from = c("1", "2", "3", "4", "5", "6", "7", "8", "9", "10", "11", "12"), to = c("LowSeason", "LowSeason", "MidSeason",'MidSeason', 'HighSeason','HighSeason','HighSeason','MidSeason', 'MidSeason','MidSeason','LowSeason','LowSeason'))
head(data_2[,c("MoSold","SeasonSale")])
data_2$SeasonSale<-as.numeric(factor(data_2$SeasonSale,levels=c('LowSeason','MidSeason', 'HighSeason'),ordered=TRUE))

#Update to numeric 
data_2$Utilities <-as.numeric(data_2$Utilities)
data_2$LandSlope<-as.numeric(data_2$LandSlope)
data_2$ExterQual <-as.numeric(data_2$ExterQual)
data_2$ExterCond <-as.numeric(data_2$ExterCond)
data_2$BsmtQual <-as.numeric(data_2$BsmtQual)
data_2$BsmtCond <-as.numeric(data_2$BsmtCond)
data_2$BsmtExposure <-as.numeric(data_2$BsmtExposure)
data_2$BsmtFinType1 <-as.numeric(data_2$BsmtFinType1)
data_2$BsmtFinType2 <-as.numeric(data_2$BsmtFinType2)
data_2$HeatingQC <-as.numeric(data_2$HeatingQC)
data_2$CentralAir <-as.numeric(data_2$CentralAir)
data_2$Electrical <-as.numeric(data_2$Electrical)
data_2$KitchenQual <-as.numeric(data_2$KitchenQual)
data_2$Functional <-as.numeric(data_2$Functional)
data_2$FireplaceQu <-as.numeric(data_2$FireplaceQu)
data_2$GarageFinish <-as.numeric(data_2$GarageFinish)
data_2$GarageQual <-as.numeric(data_2$GarageQual)
data_2$GarageCond <-as.numeric(data_2$GarageCond)
data_2$PavedDrive <-as.numeric(data_2$PavedDrive)
data_2$PoolQC <-as.numeric(data_2$PoolQC)
data_2$Fence <-as.numeric(data_2$Fence)
data_2$OverallQual<-as.numeric(data_2$OverallQual)
data_2$OverallCond <-as.numeric(data_2$OverallCond)

#Interactions
data_2 <- mutate(data_2,GarageScore = GarageQual * GarageCond,
                 OverallScore= OverallQual * OverallCond,
                 ExterScore = ExterQual * ExterCond,
                 KitchenScore = KitchenAbvGr * KitchenQual,
                 GarageGrade = GarageArea * GarageQual)

#Dimensionality and Numerosity Reduction
data_2 <- subset(data_2, select = -c(BsmtFinSF1,BsmtFinSF2,BsmtFullBath,GarageArea,GarageQual,
                                     BsmtHalfBath,FullBath,HalfBath,X1stFlrSF, X2ndFlrSF,
                                     OpenPorchSF,EnclosedPorch,X3SsnPorch,ScreenPorch,
                                     GarageQual,GarageCond,ExterCond,KitchenAbvGr,KitchenQual,
                                     BsmtFinType1,BsmtFinType2,BsmtCond,BsmtQual,
                                     Neighborhood,YearBuilt,YrSold, BldgAge,YearRemodAdd, MoSold, GarageYrBlt,YearRemodAdd2, Remod, TimeSinceRemod,PoolQC))

#Remove 4 outliers
highlight_df <- data_2_train %>% 
  filter(GrLivArea>=4000)
highlight_df$SalePrice
data_2 <- data_2[-c(524, 692, 1183, 1299),]  

#Export pre-processed data
#write.csv(data_2,"AmesDataClean.csv", row.names = FALSE)

#Modeling
#Import pre-processed data
ames_clean<-read.csv("AmesDataClean.csv",stringsAsFactors = TRUE) #2915 obs. of  67 variables:

#Convert Categorical Features to numeric with dummy codding using function dummyVars() from Caret package.
factor_var <- which(lapply(ames_clean, class) == "factor")
data_temp<-ames_clean
dummy<- dummyVars(" ~ MSSubClass + MSZoning +Street + Alley+ LotShape + LandContour+ LotConfig + Condition1 + Condition2 + BldgType + HouseStyle + RoofStyle + RoofMatl+ Heating + Exterior1st + Exterior2nd + MasVnrType + Foundation + GarageType + SaleType + SaleCondition + MiscFeature" , data = data_temp, fullRank = TRUE)
pred<- data.frame (predict(dummy, data_temp))
data_final<-cbind(ames_clean[,-factor_var], pred)

#Split into Train and Test set
train_1<-data_final[1:1456,-1] 
train_x<-select(train_1, -SalePrice)
train_y<-train_1$SalePrice
test_1<-data_final[1457:2915,-1] 
test_1<-select(test_1, -SalePrice)
train_1$SalePrice<- log(train_1$SalePrice)
test_ID<-data_final[1457:2915,1]

#Fit Linear Model
ml_model<- lm(SalePrice ~ ., train_1)
#identify the linearly dependent variables
ld_vars <- attributes(alias(ml_model)$Complete)$dimnames[[1]]
#remove the linearly dependent variables variables
train_ld<-select(train_1, -all_of(ld_vars))

#run model again
ml_model_new <-lm(SalePrice ~ ., train_ld)

#Use pre-process to finish the preprocessing steps 
#identify only the predictor variables
features <- setdiff(names(data_final), c("Sale_Price", "Id"))
#str(data_final) #183 variables
train_final<-data_final[1:1456,]
test_final<-data_final[1457:2915,]

# pre-process estimation based on training features
pre_process <- preProcess(
  x      = train_final[features],  #181 columns
  method = c("BoxCox", "center", "scale", "nzv"))

# apply to both training & test
train_x<- predict(pre_process, train_final[, features])
test_x<- predict(pre_process, test_final[, features])

#Fit Regularized models 
train_x2<-select(train_x, -SalePrice)
test_x2<-select(test_x, -SalePrice)
train_y<- log(train_final$SalePrice)

#Ridge 
set.seed(123)
glm_cv_ridge <- cv.glmnet(as.matrix(train_x2), train_y, alpha = 0)
rfeatures<-nrow(extract.coef(glm_cv_ridge, lambda = "lambda.min")) -1

#Lasso
glm_cv_lasso <- cv.glmnet(as.matrix(train_x2), train_y, alpha = 1)
lfeatures<-nrow(extract.coef(glm_cv_lasso, lambda = "lambda.min")) -1

#Elastic Net
set.seed(123)
glm_cv_net<- cv.glmnet(data.matrix(train_x2), train_y, alpha = 0.1)
efeatures<-nrow(extract.coef(glm_cv_net, lambda = "lambda.min")) -1

penalty_ridge <- glm_cv_ridge$lambda.min
penalty_lasso <- glm_cv_lasso$lambda.min
penalty_net <- glm_cv_net$lambda.min

#Training Models
set.seed(123)
glm_ridge_mod <- glmnet(x = as.matrix(train_x2), y = train_y, alpha = 0, lambda = penalty_ridge,standardize = FALSE)
glm_lasso_mod <- glmnet(x = as.matrix(train_x2), y = train_y, alpha = 1, lambda = penalty_lasso,standardize = FALSE)
glm_net_mod <- glmnet(x = as.matrix(train_x2), y = train_y, alpha = 0.1, lambda = penalty_net,standardize = FALSE)

y_pred_ridge <- as.numeric(predict(glm_ridge_mod, as.matrix(train_x2)))
y_pred_lasso <- as.numeric(predict(glm_lasso_mod, as.matrix(train_x2)))
y_pred_net<- as.numeric(predict(glm_net_mod,as.matrix(train_x2)))

#Ridge
ridge_SSE <- sum((y_pred_ridge - train_y)^2)
ridge_SST <- sum((train_y - mean(y_pred_ridge))^2)
ridge_R_squared <- 1 - ridge_SSE / ridge_SST
ridge_corr<-cor(y_pred_ridge, train_y)
ridge_RMSE<-rmse(train_y, y_pred_ridge)

#Lasso
lasso_SSE <- sum((y_pred_lasso - train_y)^2)
lasso_SST <- sum((train_y - mean(y_pred_lasso))^2)
lasso_R_squared <- 1 - lasso_SSE / lasso_SST
lasso_corr<-cor(y_pred_lasso, train_y)
lasso_RMSE<-rmse(train_y,y_pred_lasso)

# Elastic Net

net_SSE <- sum((y_pred_net - train_y)^2)
net_SST <- sum((train_y - mean(y_pred_net))^2)
net_R_squared <- 1 - net_SSE / net_SST
net_corr<-cor(y_pred_net, train_y)
net_RMSE<-rmse(train_y,y_pred_net)

#Summary Table
regression_models<- c("Ridge", "Lasso", "Elastic Net")
tunning_parameters<-c(penalty_ridge, penalty_lasso, penalty_net)
nfeatures<-c(rfeatures, lfeatures,efeatures )
R_Squared<-c(ridge_R_squared, lasso_R_squared, net_R_squared)
Cor<- c(ridge_corr, lasso_corr, net_corr)
RMSE<-c(ridge_RMSE, lasso_RMSE, net_RMSE)

lassoVarImp <- varImp(glm_lasso_mod,scale=F, lambda = penalty_lasso)
lassoImportance <- lassoVarImp$importance
varsSelected <- length(which(lassoImportance$Overall!=0))
varsNotSelected <- length(which(lassoImportance$Overall==0))

#XGB MODEL

#Boruta Selction

set.seed(123)        # to get same results
train_boruta <- Boruta(SalePrice~., data=train_1, doTrace=2, maxRuns=125)
# printing the results 


# Getting selected featUes and its stats.
trainb_fix <- TentativeRoughFix(train_boruta)
trainb_selfeat <- getSelectedAttributes(trainb_fix, withTentative = F)
trainb_selfeat_stats <- attStats(trainb_fix)
borutafeatures<-length(trainb_selfeat)

train_x_fs<-train_1[,trainb_selfeat] #1,460 rows, 75 columns
train_y_fs<-train_1$SalePrice
test_1_fs<-test_1[,trainb_selfeat] #  1,459  75 columns

# Put testing & training data into two seperates Dmatrixs objects
label_train_fs <- train_y_fs

dtrain_fs <- xgb.DMatrix(data = as.matrix(train_x_fs), label= label_train_fs )
dtest_fs <- xgb.DMatrix(data = as.matrix(test_1_fs))

#CV control
my_control_fs <-trainControl(method="cv", number=5)
# xgb grid was taking too long
xgb_grid_sf = expand.grid(
  nrounds = 1000,
  eta = c(0.1, 0.05, 0.01),
  max_depth = c(2, 3, 4, 5, 6),
  gamma = 0,
  colsample_bytree=1,
  min_child_weight=c(1, 2, 3, 4 ,5),
  subsample=1
)

#Parameter Tuning
default_param_sf<-list(
  booster = "gbtree",
  eta=0.05, #default = 0.3
  gamma=0,
  max_depth=6, #default=6
  min_child_weight=4, #default=1
  subsample=1,
  colsample_bytree=1
)

#Cross validation to determine the best number of rounds 
set.seed(123)

xgbcv_fs <- xgb.cv( params = default_param_sf, data =dtrain_fs, nrounds = 500, nfold = 5, showsd = T, stratified = T, print_every_n_fs = 40, early_stopping_rounds = 10, maximize = F)
cat("The XGB model includes",  borutafeatures,  "features, and  the optimal number of rounds:",  xgbcv_fs$best_iteration, "was selected based on  RMSE of 0.121172")
xgb_mod_fs <- xgb.train(data = dtrain_fs, params=default_param_sf, nrounds = xgbcv_fs$best_iteration, verbose = 2)

 

#Models Summary
#Calculate RMSE on training data
XGBpred_train <- predict(xgb_mod_fs, dtrain_fs)

XGB_SSE <- sum((XGBpred_train - train_y_fs)^2)
XGB_SST <- sum((train_y_fs - mean(XGBpred_train))^2)
XGB_R_squared <- 1 - XGB_SSE / XGB_SST
XGB_corr<-cor(XGBpred_train, train_y_fs)
XGB_RMSE<-rmse(train_y_fs,XGBpred_train)


regression_models<- c("Ridge", "Lasso", "Elastic Net", "XGBoost")
tunning_parameters<-c(penalty_ridge, penalty_lasso, penalty_net, " ")
nfeatures<-c(rfeatures, lfeatures, efeatures, borutafeatures)
R_Squared<-c(ridge_R_squared, lasso_R_squared, net_R_squared, XGB_R_squared)
Cor<- c(ridge_corr, lasso_corr, net_corr, XGB_corr)
RMSE<-c(ridge_RMSE, lasso_RMSE, net_RMSE, XGB_RMSE)
kable(data.frame(Models=regression_models, Parameters= tunning_parameters, Features=nfeatures,Corr=Cor,  Rsquared=R_Squared, RMSE = RMSE))

#Prediction
pred_ridge_test <-  as.double(predict(glm_ridge_mod, as.matrix(test_x2)))
pred_lasso_test <-  as.double(predict(glm_lasso_mod, as.matrix(test_x2)))
pred_net_test<-  as.double(predict(glm_net_mod,as.matrix(test_x2)))
pred_ridge_test_final<- exp(pred_ridge_test)
pred_lasso_test_final<- exp(pred_lasso_test)
pred_net_test_final<- exp(pred_net_test)
XGBpred_fs <- predict(xgb_mod_fs, dtest_fs)
predictions_XGB_fs <- exp(XGBpred_fs) #reverse the log to the real values

#Store predictions
Ridge_model_final_data2<- data.frame("ID"=test_final$Id,"SalePrice"=pred_ridge_test_final)
#write.csv(Ridge_model_final_data2, "Ridge_ModelFinal2", row.names=FALSE)

Lasso_model_final_data2<- data.frame("ID"=test_final$Id,"SalePrice"=pred_lasso_test_final)
#write.csv(Lasso_model_final_data2, "Lasso_ModelFinal2", row.names=FALSE)

ElasticNet_model_final_data2<- data.frame("ID"=test_final$Id,"SalePrice"=pred_net_test_final)
#write.csv(ElasticNet_model_final_data2, "ElasticNet_ModelFinal2", row.names=FALSE)

xgbmodel_pred_d2_fs<- data.frame("ID"=test_ID,"SalePrice"=predictions_XGB_fs)
#write.csv(xgbmodel_pred_d2_fs, "xgbModel_fs_outl", row.names=FALSE) 


#Validation
models<- c("Ridge", "Lasso", "Elastic Net", "XGBoost")
tunning_parameters<-c(penalty_ridge, penalty_lasso, penalty_net, " ")
nfeatures<-c(rfeatures, lfeatures, efeatures, borutafeatures)
R_Squared<-c(ridge_R_squared, lasso_R_squared, net_R_squared, XGB_R_squared)
Cor<- c(ridge_corr, lasso_corr, net_corr, XGB_corr)
RMSE<-c(ridge_RMSE, lasso_RMSE, net_RMSE, XGB_RMSE)
KaggleRMSE<-c(0.13163, 0.12998, 0.13061, 0.12758 )
kable(data.frame(Models=models, Features=nfeatures,Corr=Cor,  Rsquared=R_Squared, TrainingRMSE = RMSE, KaggleRMSE=KaggleRMSE))

#Stacked model
 
#write.csv(stacking , "stacked_fs_outl", row.names=FALSE) #KAGGLE RMSE: 0.12592

