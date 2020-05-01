reg10<- lm(log(SalePrice)~log(OverallQual)+ FullBath  + log(YearBuilt) + YearRemodAdd*remodledafterbuilt
           + TotalBsmtSF +  log(FirstFlrSF) + GarageCars +  GarageArea*GarageCars*GarageYrBlt  
           + log(GrLivArea) +GrLivArea*FirstFlrSF
           + log(TotRmsAbvGrd)     +ExterQual + Central.Air +BedroomAbvGr
           +KitchenQual  + HeatingQC + BsmtQual  + BsmtExposure *BsmtQual
           +GasA.Heating    +BldgType  +WoodDeckSF +Garage+ GarageFinish
           + BsmtFullBath   +SaleCondition  +GarageCond  +FireplaceQu 
           +log(LotArea) +LotArea*GrLivArea  +Neighborhood +Newhouse   +OpenPorchSF +MiscVal  +MSSubClass
           +Functional  +GarageQual  +OverallCond +YrSold +BsmtCond +LotConfig+SecondFlrSF 
           +LotArea*MasVnrArea+LotArea*FirstFlrSF*SecondFlrSF
           +MasVnrArea
           +Newhouse*LotArea
           ,data.train)


testing.prices.testinglog10<-predict(reg10, data.test) #predict the prices of the 439 houses left for testing the model

testing.prices.testing10<-exp(testing.prices.testinglog10)

percent.errors10 <- abs((data.test$SalePrice-testing.prices.testing10)/data.test$SalePrice)*100 #calculate absolute percentage errors
mean(percent.errors10) #display Mean Absolute Percentage Error (MAPE) 
#Re-run splitting data into train and test 5 times 
#8.467263,  9.003557,  8.530965, 8.76543,  8.158342


RMSLE(data.test$SalePrice,testing.prices.testing10) 
#Re-run splitting data into train and test 5 times 
#0.1213048,  0.1294528, 0.114788, 0.1219135, 0.1142115


predicted.prices.testing<-exp(predict(reg10, clean.predict))


predicted.prices<-cbind(predict$Id,predicted.prices.testing)




write.csv(predicted.prices, "C:\\Users\\mayad\\Documents\\FinalResults.csv")
