data <- read_csv("MA867/Assignments/Individual assignment/data/house prices/train.csv")

#load all anticipated packages 
library(dplyr)
library(corrplot)
library(mice)
library(caret)
library(ggplot2)
library(MLmetrics)

#data exploration
str(data) #show the structure of (data types in) the dataframe
head(data, 4) #show the first 4 rows in the dataframe
tail(data,4) #show the last 4 rows in the  dataframe

summary(data) #show summary statistics of the  dataframe

names(data)#to get list of all the variables in the data set

hist(data$SalePrice) # show distribuion of sales price 
data$log_SalePrice<-log(data$SalePrice) #create new varaible that takes the natural log of sales price
hist(data$log_SalePrice) # show distribution of log of sales price--> histogram now follows normal distribution 




#create new df where we remove id column, want to keep original df untouched
train.data <- data[,]



#to determine which columns have missing data 
colSums(sapply(train.data, is.na))

na.cols<- which(colSums(is.na(train.data)) > 0)
sort(colSums(sapply(train.data[na.cols], is.na)), decreasing = TRUE)
# there are 34 columns with missing data 


#convert character varaibles to numeric

#street
train.data$StreetPave<-ifelse(train.data$Street=="Pave",1,0)



#Alley
train.data$alley<-ifelse(train.data$Alley=="NA" ,0,1)
train.data$alley[is.na(train.data$alley)]<-0


#central air
train.data$Central.Air<-ifelse(train.data$CentralAir=="Y",1,0)


#LotFrontage
Lotfrontage1<-table(train.data$LotFrontage)
Lotfrontage1
#Heating
Heating1<-table(train.data$Heating)
Heating1 #98% of results are gasA
train.data$GasA.Heating<-ifelse(train.data$Heating=="GasA",1,0)


#SaleType
SaleType1<-table(train.data$SaleType)
SaleType1 #95% of data is either a conventional warranty dead or a new purchase

train.data$Convential.Warranty<-ifelse(train.data$SaleType=="WD",1,0)
train.data$Newhouse<-ifelse(train.data$SaleType=="New",1,0)




#RoofStyle
Roofstyle1<-table(train.data$RoofStyle)
Roofstyle1 #Gable and Hip mak eup 98% 

train.data$GableRoof <- ifelse(train.data$RoofStyle=="Gable",1,0)
train.data$HipRoof <- ifelse(train.data$RoofStyle=="Hip",1,0)


#Roofmat

Roofmat1<-table(train.data$RoofMatl)
Roofmat1 #over 98% is standard shingles
train.data$Shingle.Roof <- ifelse(train.data$RoofMatl=="CompShg",1,0)


#Create new variable Remodled after built
remodled1<-table(train.data$YearRemodAdd)
remodled1

train.data$remodledafterbuilt<-ifelse(train.data$YearBuilt==train.data$YearRemodAdd,0,1)



#landcounter
train.data$flat.land<-ifelse(train.data$LandContour == "Lvl",1,0)

#foundation
foundation1<-table(train.data$Foundation)
foundation1

#create dummy variable for basement 

train.data$Basement<-ifelse(train.data$BsmtExposure=="NA" ,0,1)


train.data$Basement[is.na(train.data$Basement)]<-0

#Pool
train.data$Pool <-ifelse(train.data$PoolQC=="NA",0,1)
train.data$Pool[is.na(train.data$Pool)]<-0


#create dummy variable for garage 
train.data$Garage<-ifelse(train.data$GarageType=="NA" ,0,1)
train.data$Garage[is.na(train.data$Garage)]<-0



#create dummy for misc feature
train.data$Misc<-ifelse(train.data$MiscFeature=="NA" ,0,1)
train.data$Misc[is.na(train.data$Misc)]<-0


#create dummy for fence 
train.data$Fence.Type<-train.data$Fence
train.data$fence<-ifelse(train.data$Fence.Type=="NA" ,0,1)
train.data$fence[is.na(train.data$fence)]<-0


#convert factor scale to numeric scale

train.data$ExterQual <- as.numeric(factor(train.data$ExterQual, 
                                          levels = c( "Po", "Fa","TA","Gd","Ex"),
                                          labels = c(5,4,3,2,1) ,ordered = FALSE))


train.data$ExterCond <- as.numeric(factor(train.data$ExterCond, 
                                          levels = c( "Po", "Fa","TA","Gd","Ex"),
                                          labels = c(5,4,3,2,1) ,ordered = FALSE))

train.data$BsmtQual <- as.numeric(factor(train.data$BsmtQual, 
                                         levels = c( "Po", "Fa","TA","Gd","Ex"),
                                         labels = c(5,4,3,2,1) ,ordered = FALSE))


train.data$BsmtCond <- as.numeric(factor(train.data$BsmtCond, 
                                         levels = c( "Po", "Fa","TA","Gd","Ex"),
                                         labels = c(5,4,3,2,1) ,ordered = FALSE))


train.data$BsmtExposure <- as.numeric(factor(train.data$BsmtExposure, 
                                             levels = c( "No", "Mn","Av","Gd"),
                                             labels = c(4,3,2,1) ,ordered = FALSE))

train.data$HeatingQC <- as.numeric(factor(train.data$HeatingQC, 
                                          levels = c( "Po", "Fa","TA","Gd","Ex"),
                                          labels = c(5,4,3,2,1) ,ordered = FALSE))

train.data$KitchenQual <- as.numeric(factor(train.data$KitchenQual, 
                                            levels = c( "Po", "Fa","TA","Gd","Ex"),
                                            labels = c(5,4,3,2,1) ,ordered = FALSE))

train.data$FireplaceQu <- as.numeric(factor(train.data$FireplaceQu, 
                                            levels = c( "Po", "Fa","TA","Gd","Ex"),
                                            labels = c(5,4,3,2,1) ,ordered = FALSE))


train.data$GarageQual <- as.numeric(factor(train.data$GarageQual, 
                                           levels = c( "Po", "Fa","TA","Gd","Ex"),
                                           labels = c(5,4,3,2,1) ,ordered = FALSE))

train.data$GarageCond <- as.numeric(factor(train.data$GarageCond, 
                                           levels = c( "Po", "Fa","TA","Gd","Ex"),
                                           labels = c(5,4,3,2,1) ,ordered = FALSE))

train.data$PoolQC <- as.numeric(factor(train.data$PoolQC, 
                                       levels = c(  "Fa","TA","Gd","Ex"),
                                       labels = c(4,3,2,1) ,ordered = FALSE))





#seperate data into numeric data frame and character data frame


#reaplce missing values in character data subset 

data.character <- train.data[,sapply(train.data,is.character)]
data.character$Fence[is.na(data.character$Fence)]<-"NoFence"
data.character$GarageType[is.na(data.character$GarageType)]<-"NoGarage"


data.character[is.na(data.character)]<-"other"
md.pattern(data.character)

data.factor <- mutate_if(data.character, is.character, as.factor)
clean.data.factor<- data.factor
md.pattern(clean.data.factor)

#change all chracter variables to factors



data.factor <- mutate_if(data.character, is.character, as.factor)

data.numeric1 <- train.data[,sapply(train.data,is.numeric)]
data.numeric<-mutate_if(data.numeric1,is.double,as.numeric)


#assessing missing values in numeric df
md.pattern(data.numeric)

colSums(sapply(data.numeric, is.na))

na.cols.numeric<- which(colSums(is.na(data.numeric)) > 0)
sort(colSums(sapply(data.numeric[na.cols.numeric], is.na)), decreasing = TRUE)

#missing numerical values: Lotfrontage, MasnrArea, Bsmtqual, BsmtCond, Bsmtexposure, FireplaceQu, GarageQual, PoolQC, Garage YrBuilt

#garage year built
#for simplicity, for all missing values for garage year built we will use the year built of the house
grg <- which(is.na(data.numeric$GarageYrBlt))
data.numeric[grg, 'GarageYrBlt'] <- data.numeric[grg, 'YearBuilt']


#missing values appear in garage year built, lot frontage and masonary vaneer area is square feet
#all others are due to feature not existing 

data.numeric$BsmtQual[is.na(data.numeric$BsmtQual)]<-0
data.numeric$BsmtCond[is.na(data.numeric$BsmtCond)]<-0
data.numeric$BsmtExposure[is.na(data.numeric$BsmtExposure)]<-0
data.numeric$FireplaceQu[is.na(data.numeric$FireplaceQu)]<-0
data.numeric$PoolQC[is.na(data.numeric$PoolQC)]<-0
data.numeric$GarageQual[is.na(data.numeric$GarageQual)]<-0
data.numeric$GarageCond[is.na(data.numeric$GarageCond)]<-0
data.numeric$SBRKR.Electrical[is.na(data.numeric$SBRKR.Electrical)]<-0

md.pattern(data.numeric)



#assess correlation of numeric values 
data.cor <- cor(data.numeric, use= 'na.or.complete')
corrplot(data.cor,order = 'hclust', method = 'square')

#overall quality is most correlated with sales price 
plot(SalePrice ~ OverallQual, data=data) #plot of price vs overall quality 
hist(data$OverallQual) #histogram of overall quality

#rename 1stflrSF to FirstFlrSF
names(data.numeric)[names(data.numeric) == '1stFlrSF'] <- 'FirstFlrSF'

names(data.numeric)[names(data.numeric) == '2ndFlrSF'] <- 'SecondFlrSF'



#create df with clean numeric varaibles 
clean.data.numeric<-data.numeric[,]

md.pattern(clean.data.numeric)



#MasVnrArea
median(na.omit(data.numeric$MasVnrArea)) # is 0
clean.data.numeric$MasVnrArea[is.na(clean.data.numeric$MasVnrArea)]<-0

#LotFrontage
median(na.omit(data.numeric$LotFrontage)) #is 69
clean.data.numeric$LotFrontage[is.na(clean.data.numeric$LotFrontage)]<-69


#combine cleaned factor and numerical subsets
clean.train <-cbind(clean.data.numeric, clean.data.factor)

md.pattern(clean.train)




# dividie training dataset into training and testing subset, using a 70%/30% split
sample<-sample.int(n=nrow(clean.train), size=floor(0.7*nrow(clean.train)),replace=F)


data.train<-clean.train[sample,]
data.test<-clean.train[-sample,]




#Now clean Test dataset 

test <- read_csv("MA867/Assignments/Individual assignment/data/house prices/test.csv")
colSums(sapply(test, is.na))

na.cols2<- which(colSums(is.na(test)) > 0)
sort(colSums(sapply(test[na.cols2], is.na)), decreasing = TRUE)
# there are 33 columns with missing data 



#clean predict data set 
predict<-test[,] #create new dataset as to not alter original 


#convert character varaibles to numeric

#street
predict$StreetPave<-ifelse(predict$Street=="Pave",1,0)



#Alley
predict$alley<-ifelse(predict$Alley=="NA" ,0,1)
predicta$alley[is.na(predict$alley)]<-0


#central air
#clean predict data set 




#convert character varaibles to numeric

#street
predict$StreetPave<-ifelse(predict$Street=="Pave",1,0)



#Alley
predict$alley<-ifelse(predict$Alley=="NA" ,0,1)
predict$alley[is.na(predict$alley)]<-0


#central air
predict$Central.Air<-ifelse(predict$CentralAir=="Y",1,0)




#Heating
Heating2<-table(predict$Heating)
Heating2 #98% of results are gasA
predict$GasA.Heating<-ifelse(predict$Heating=="GasA",1,0)


#SaleType
SaleType2<-table(predict$SaleType)
SaleType2 #95% of data is either a conventional warranty dead or a new purchase

predict$Convential.Warranty<-ifelse(predict$SaleType=="WD",1,0)
predict$Newhouse<-ifelse(predict$SaleType=="New",1,0)


#RoofStyle
Roofstyle2<-table(predict$RoofStyle)
Roofstyle2 #Gable and Hip mak eup 98% 

predict$GableRoof <- ifelse(predict$RoofStyle=="Gable",1,0)
predict$HipRoof <- ifelse(predict$RoofStyle=="Hip",1,0)


#Roofmat

Roofmat2<-table(predict$RoofMatl)
Roofmat2 #over 98% is standard shingles
predict$Shingle.Roof <- ifelse(predict$RoofMatl=="CompShg",1,0)


#Remodled after built
remodled2<-table(predict$YearRemodAdd)
remodled2

predict$remodledafterbuilt<-ifelse(predict$YearBuilt==predict$YearRemodAdd,0,1)

#Functional has 2 missing values, reaplce those with typical
Functional2<- table(predict$Functional)
Functional2
predict$Functional[is.na(predict$Functional)]<-"Typ"

#landcounter
predict$flat.land<-ifelse(predict$LandContour == "Lvl",1,0)



#create dummy variable for basement 

predict$Basement<-ifelse(predict$BsmtExposure=="NA" ,0,1)


predict$Basement[is.na(predict$Basement)]<-0


#create dummy variable for garage 
predict$Garage<-ifelse(predict$GarageType=="NA" ,0,1)
predict$Garage[is.na(predict$Garage)]<-0



#create dummy for misc feature
predict$Misc<-ifelse(predict$MiscFeature=="NA" ,0,1)
predict$Misc[is.na(predict$Misc)]<-0


#create dummy for fence 
predict$Fence.Type<-predict$Fence
predict$fence<-ifelse(predict$Fence.Type=="NA" ,0,1)
predict$fence[is.na(predict$fence)]<-0


#convert factor scale to numeric scale

predict$ExterQual <- as.numeric(factor(predict$ExterQual, 
                                       levels = c( "Po", "Fa","TA","Gd","Ex"),
                                       labels = c(5,4,3,2,1) ,ordered = FALSE))


predict$ExterCond <- as.numeric(factor(predict$ExterCond, 
                                       levels = c( "Po", "Fa","TA","Gd","Ex"),
                                       labels = c(5,4,3,2,1) ,ordered = FALSE))

predict$BsmtQual <- as.numeric(factor(predict$BsmtQual, 
                                      levels = c( "Po", "Fa","TA","Gd","Ex"),
                                      labels = c(5,4,3,2,1) ,ordered = FALSE))

predict$BsmtCond <- as.numeric(factor(predict$BsmtCond, 
                                      levels = c( "Po", "Fa","TA","Gd","Ex"),
                                      labels = c(5,4,3,2,1) ,ordered = FALSE))


predict$BsmtExposure <- as.numeric(factor(predict$BsmtExposure, 
                                          levels = c( "No", "Mn","Av","Gd"),
                                          labels = c(4,3,2,1) ,ordered = FALSE))

predict$HeatingQC <- as.numeric(factor(predict$HeatingQC, 
                                       levels = c( "Po", "Fa","TA","Gd","Ex"),
                                       labels = c(5,4,3,2,1) ,ordered = FALSE))

predict$KitchenQual <- as.numeric(factor(predict$KitchenQual, 
                                         levels = c( "Po", "Fa","TA","Gd","Ex"),
                                         labels = c(5,4,3,2,1) ,ordered = FALSE))

predict$FireplaceQu <- as.numeric(factor(predict$FireplaceQu, 
                                         levels = c( "Po", "Fa","TA","Gd","Ex"),
                                         labels = c(5,4,3,2,1) ,ordered = FALSE))

predict$GarageQual <- as.numeric(factor(predict$GarageQual, 
                                        levels = c( "Po", "Fa","TA","Gd","Ex"),
                                        labels = c(5,4,3,2,1) ,ordered = FALSE))

predict$GarageCond <- as.numeric(factor(predict$GarageCond, 
                                        levels = c( "Po", "Fa","TA","Gd","Ex"),
                                        labels = c(5,4,3,2,1) ,ordered = FALSE))

predict$PoolQC <- as.numeric(factor(predict$PoolQC, 
                                    levels = c(  "Fa","TA","Gd","Ex"),
                                    labels = c(4,3,2,1) ,ordered = FALSE))






#seperate data into numeric data frame and character data frame


#reaplce all missing character values with other 


predict.character <- predict[,sapply(predict,is.character)]
predict.character[is.na(predict.character)]<-"other"
md.pattern(predict.character)

predict.factor <- mutate_if(predict.character, is.character, as.factor)
clean.predict.factor<- predict.factor
md.pattern(clean.predict.factor)

#change all chracter variables to factors


#assessing missing values in numeric df
predict.numeric <- predict[,sapply(predict,is.numeric)]

md.pattern(predict.numeric)

colSums(sapply(predict.numeric, is.na))

na.cols.numeric<- which(colSums(is.na(predict.numeric)) > 0)
sort(colSums(sapply(predict.numeric[na.cols.numeric], is.na)), decreasing = TRUE)

#missing numerical values: Lotfrontage, MasnrArea, Bsmtqual, BsmtCond, Bsmtexposure, FireplaceQu, GarageQual, PoolQC, Garage YrBuilt
#garage year built
#for simplicity, for all missing values for garage year built we will use the year built of the house
grg <- which(is.na(predict.numeric$GarageYrBlt))
predict.numeric[grg, 'GarageYrBlt'] <- predict.numeric[grg, 'YearBuilt']


#missing values appear in garage year built, lot frontage and masonary vaneer area is square feet 


predict.numeric$BsmtQual[is.na(predict.numeric$BsmtQual)]<-0
predict.numeric$BsmtFullBath[is.na(predict.numeric$BsmtFullBath)]<-0
predict.numeric$BsmtCond[is.na(predict.numeric$BsmtCond)]<-0
predict.numeric$BsmtExposure[is.na(predict.numeric$BsmtExposure)]<-0
predict.numeric$FireplaceQu[is.na(predict.numeric$FireplaceQu)]<-0
predict.numeric$PoolQC[is.na(predict.numeric$PoolQC)]<-0
predict.numeric$GarageQual[is.na(predict.numeric$GarageQual)]<-0
predict.numeric$GarageCond[is.na(predict.numeric$GarageCond)]<-0
predict.numeric$SBRKR.Electrical[is.na(predict.numeric$SBRKR.Electrical)]<-0
predict.numeric$Typical.Functional[is.na(predict.numeric$Typical.Functional)]<-0

md.pattern(predict.numeric)



#assess correlation of numeric values 
data.cor <- cor(predict.numeric, use= 'na.or.complete')
corrplot(data.cor,order = 'hclust', method = 'square')


hist(data$OverallQual) #histogram of overall quality

#rename 1stflrSF to FirstFlrSF
names(predict.numeric)[names(predict.numeric) == '1stFlrSF'] <- 'FirstFlrSF'

names(predict.numeric)[names(predict.numeric) == '2ndFlrSF'] <- 'SecondFlrSF'


#MasVnrArea

median(predict.numeric$MasVnrArea) #0

predict.numeric$MasVnrArea[is.na(predict.numeric$MasVnrArea)]<-0


predict.numeric[is.na(predict.numeric)]<-0



#create df with all relevant numeric varaibles 
clean.predict.numeric<-predict.numeric


clean.predict <-cbind(clean.predict.numeric, clean.predict.factor)




###
### Interactions 
###


predicted.prices.testing.log.i<-exp(predict(reg3, clean.predict))


predicted.prices<-cbind(predict$Id,predicted.prices.testing.log.i)




write.csv(predicted.prices, "C:\\Users\\mayad\\Documents\\Submission7.csv")


#




