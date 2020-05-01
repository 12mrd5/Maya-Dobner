#Regression Models


#reular regression model, including most variables, aside from those identified as irrelevant 
reg1<- lm(SalePrice~OverallQual+ YearBuilt+ YearRemodAdd + TotalBsmtSF + FirstFlrSF +
            GarageCars+  GarageArea+ GrLivArea + TotRmsAbvGrd+ GarageYrBlt+ Central.Air+
            ExterQual + KitchenQual + HeatingQC + MasVnrArea+Neighborhood+BldgType+HouseStyle+
            +PavedDrive+ GarageType +SaleCondition+ FullBath,data.train)
        


testing.prices.testing1<-predict(reg1, data.test) #predict the prices of the 439 houses left for testing the model


percent.errors1 <- abs((data.test$SalePrice-testing.prices.testing1)/data.test$SalePrice)*100 #calculate absolute percentage errors
mean(percent.errors1) #display Mean Absolute Percentage Error (MAPE) #13.36909




#Now try log to ln
reg2<- lm(log(SalePrice)~OverallQual+ YearBuilt+ YearRemodAdd + TotalBsmtSF + FirstFlrSF +
            GarageCars+  GarageArea+ GrLivArea + TotRmsAbvGrd+ GarageYrBlt+ Central.Air+
            ExterQual + KitchenQual + HeatingQC + MasVnrArea+Neighborhood+BldgType+HouseStyle+
            +PavedDrive+ GarageType +SaleCondition+FullBath,data.train)


testing.prices.testinglog2<-predict(reg2, data.test) #predict the prices of the 439 houses left for testing the model
testing.prices.testing2<-exp(testing.prices.testinglog2)

percent.errors2 <- abs((data.test$SalePrice-testing.prices.testing2)/data.test$SalePrice)*100 #calculate absolute percentage errors
mean(percent.errors2) #display Mean Absolute Percentage Error (MAPE) #11.9676

RMSLE(data.test$SalePrice,testing.prices.testing2) #Display RMSLE #0.1598071
#improvement in MAPE due to Log to Linear

#Now try log to log with 2 independent varaibles that had highest correlation with SalesPrice  (OverallQual and total rooms above ground)
reg3<- lm(log(SalePrice)~log(OverallQual)+ YearBuilt+ YearRemodAdd + TotalBsmtSF + FirstFlrSF +
            GarageCars+  GarageArea+ GrLivArea + log(TotRmsAbvGrd)+ GarageYrBlt+ Central.Air+
            ExterQual + KitchenQual + HeatingQC + MasVnrArea+Neighborhood+BldgType+HouseStyle+
            +PavedDrive+ GarageType +SaleCondition+FullBath,data.train)
          
          
    
testing.prices.testinglog3<-predict(reg3, data.test) #predict the prices of the 439 houses left for testing the model
testing.prices.testing3<-exp(testing.prices.testinglog3)

percent.errors3 <- abs((data.test$SalePrice-testing.prices.testing3)/data.test$SalePrice)*100 #calculate absolute percentage errors
mean(percent.errors3) #display Mean Absolute Percentage Error (MAPE) #11.80717

RMSLE(data.test$SalePrice,testing.prices.testing3) #Display RMSLE #0.1576956
#improvement in MAPE and RMSLE


#add variables in question and created new variable remodled after built

reg4<- lm(log(SalePrice)~log(OverallQual)+ YearBuilt+ YearRemodAdd + TotalBsmtSF + FirstFlrSF +
            GarageCars+  GarageArea+ GrLivArea + log(TotRmsAbvGrd)+ GarageYrBlt+ Central.Air+
            ExterQual + KitchenQual + HeatingQC + MasVnrArea+Neighborhood+BldgType+HouseStyle+
            +PavedDrive+ GarageType +SaleCondition +FullBath
          +Newhouse+LotArea+MiscVal +remodledafterbuilt+ SecondFlrSF +Functional+
            MSZoning+ GarageArea +YrSold +LotConfig   +GasA.Heating +LotShape 
          +GableRoof+HipRoof,data.train)



testing.prices.testinglog4<-predict(reg4, data.test) #predict the prices of the 439 houses left for testing the model
testing.prices.testing4<-exp(testing.prices.testinglog4)

percent.errors4 <- abs((data.test$SalePrice-testing.prices.testing4)/data.test$SalePrice)*100 #calculate absolute percentage errors
mean(percent.errors4) #display Mean Absolute Percentage Error (MAPE) #11.48582

RMSLE(data.test$SalePrice,testing.prices.testing4) #Display RMSLE #0.157256
#improvement in MAPE and RMSLE

#add and remove variables 


reg5<- lm(log(SalePrice)~log(OverallQual)+ log(YearBuilt)+ YearRemodAdd + TotalBsmtSF + FirstFlrSF +
            GarageCars+  GarageArea+ GrLivArea + log(TotRmsAbvGrd)+ GarageYrBlt+ Central.Air+
            ExterQual + KitchenQual + HeatingQC + MasVnrArea+Neighborhood+BldgType+HouseStyle+
            +PavedDrive+ GarageType +SaleCondition +FullBath
            +Garage+ GarageFinish +WoodDeckSF
           +Newhouse+LotArea+MiscVal +remodledafterbuilt+ SecondFlrSF +Functional
            +MSZoning+ GarageArea +YrSold +LotConfig   +GasA.Heating +LotShape 
            +FireplaceQu +Foundation+flat.land +BsmtCond
          +GableRoof+HipRoof,data.train)

testing.prices.testinglog5<-predict(reg5, data.test) #predict the prices of the 439 houses left for testing the model
testing.prices.testing5<-exp(testing.prices.testinglog5)

percent.errors5 <- abs((data.test$SalePrice-testing.prices.testing5)/data.test$SalePrice)*100 #calculate absolute percentage errors
mean(percent.errors5) #display Mean Absolute Percentage Error (MAPE) #11.42913

RMSLE(data.test$SalePrice,testing.prices.testing6) #Display RMSLE #0.1327187
#improvement in MAPE and RMSLE


##interactions#
reg6<-lm(log(SalePrice)~log(OverallQual) +OverallQual*YearBuilt +OverallQual*LotArea
         + log(YearBuilt) + YearBuilt* YearRemodAdd+YearBuilt*YrSold + Newhouse*YrSold
         +log(LotArea) +LotArea*GrLivArea  +Neighborhood +BldgType +Newhouse*LotArea
         +Newhouse*YearBuilt + GrLivArea + TotRmsAbvGrd  +TotRmsAbvGrd*GrLivArea 
         +GrLivArea +GrLivArea*FirstFlrSF*SecondFlrSF  +BedroomAbvGr
         + TotalBsmtSF*GrLivArea +GrLivArea*FirstFlrSF*SecondFlrSF 
         +BsmtExposure *TotalBsmtSF   +BsmtFullBath/TotalBsmtSF
         +KitchenQual  +ExterQual  +LotConfig + Central.Air*OverallCond
         +GasA.Heating + HeatingQC  +Garage+ GarageFinish 
         +GarageArea * GarageQual +GarageCars +YrSold  
         +WoodDeckSF*LotArea  +SaleCondition  +SaleCondition*OverallCond
         +FireplaceQu+FireplaceQu*OverallCond  +Misc 
         +Functional   +OverallCond  +Foundation +flat.land
        +BsmtCond +LotConfig+SecondFlrSF 
         +LotArea*MasVnrArea+LotArea*FirstFlrSF*SecondFlrSF
         +MasVnrArea
         ,data.train)





testing.prices.testinglog6<-predict(reg6, data.test) #predict the prices of the 439 houses left for testing the model
testing.prices.testing6<-exp(testing.prices.testinglog6)

percent.errors6 <- abs((data.test$SalePrice-testing.prices.testing6)/data.test$SalePrice)*100 #calculate absolute percentage errors
mean(percent.errors6) #display Mean Absolute Percentage Error (MAPE) #9.063765, 8.91029

RMSLE(data.test$SalePrice,testing.prices.testing6) #Display RMSLE #0.1327187, 0.1336675
#improvement in MAPE and RMSLE


#more itneractions

reg7<-lm(log(SalePrice)~log(OverallQual) +OverallQual*YearBuilt +OverallQual*LotArea
            + log(YearBuilt) + YearBuilt* YearRemodAdd+YearBuilt*YrSold + Newhouse*YrSold
            +log(LotArea) +LotArea*GrLivArea  +Neighborhood  +Newhouse*LotArea
            +Newhouse*YearBuilt + GrLivArea + TotRmsAbvGrd  +TotRmsAbvGrd*GrLivArea 
            +GrLivArea +GrLivArea*FirstFlrSF*SecondFlrSF  +BedroomAbvGr
            + TotalBsmtSF*GrLivArea +GrLivArea*FirstFlrSF*SecondFlrSF 
            +FullBath+FullBath*HalfBath +(FullBath* HalfBath)/BedroomAbvGr
            +(FullBath* HalfBath)/GrLivArea+  log(FirstFlrSF) 
            +BsmtUnfSF/ (GrLivArea*TotalBsmtSF) + TotRmsAbvGrd*SecondFlrSF +log(GrLivArea)/log(LotArea)
            + BsmtQual  + BsmtExposure *BsmtQual +BsmtUnfSF/TotalBsmtSF
            +BsmtExposure *TotalBsmtSF   +BsmtFullBath/TotalBsmtSF
            +KitchenQual     +ExterQual  +LotConfig + Central.Air*OverallCond
            +GasA.Heating + HeatingQC +BldgType   + GarageCars *  GarageArea
            +Garage+ GarageFinish +GarageArea * GarageQual +GarageQual +LotArea *GarageCars  
            +WoodDeckSF*LotArea  +SaleCondition  +SaleCondition*OverallCond
            +FireplaceQu+FireplaceQu*OverallCond  +Misc 
            +Functional   +OverallCond   +OpenPorchSF*LotArea
            +LotArea*MasVnrArea+LotArea*GrLivArea*FirstFlrSF +flat.land +flat.land*LotArea
            +Foundation +flat.land*LotArea*MasVnrArea*OverallCond+YrSold +BsmtCond +LotConfig+SecondFlrSF 
         +LotArea*MasVnrArea+LotArea*FirstFlrSF*SecondFlrSF
         +MasVnrArea
         ,data.train)

            

testing.prices.testinglog7<-predict(reg7, data.test) #predict the prices of the 439 houses left for testing the model
testing.prices.testing7<-exp(testing.prices.testinglog7)

percent.errors7 <- abs((data.test$SalePrice-testing.prices.testing7)/data.test$SalePrice)*100 #calculate absolute percentage errors
mean(percent.errors7) #display Mean Absolute Percentage Error (MAPE) #9.539094, 8.713972

RMSLE(data.test$SalePrice,testing.prices.testing7) #Display RMSLE #0.1482687, 0.2978817
# MAPE and RMSLE increased
#too many interactions and variables


#remove some interactions
reg8<-lm(log(SalePrice)~log(OverallQual) +OverallQual*YearBuilt +OverallQual*LotArea
         + log(YearBuilt) + YearRemodAdd*remodledafterbuilt
         + TotalBsmtSF +  log(FirstFlrSF) + GarageCars +  GarageArea*GarageCars*GarageYrBlt 
         + log(GrLivArea) +GrLivArea*FirstFlrSF   + log(TotRmsAbvGrd)  + Central.Air 
         +ExterQual +BedroomAbvGr  +KitchenQual  + HeatingQC + BsmtQual 
         +GasA.Heating    +BldgType  +WoodDeckSF +Garage+ GarageFinish
         +(FullBath* HalfBath)/GrLivArea+  log(FirstFlrSF) 
         +BsmtUnfSF/ (GrLivArea*TotalBsmtSF) + TotRmsAbvGrd*SecondFlrSF +log(GrLivArea)/log(LotArea)
         + BsmtQual  + BsmtExposure *BsmtQual +BsmtUnfSF/TotalBsmtSF
         + BsmtFullBath   +SaleCondition   + FullBath   +GarageCond  +FireplaceQu 
         +log(LotArea) +LotArea*GrLivArea  +Neighborhood +Newhouse +MiscVal  +MSSubClass
         +Functional  +GarageQual  +OverallCond +YrSold +BsmtCond +LotConfig+SecondFlrSF 
         +LotArea*MasVnrArea+LotArea*FirstFlrSF*SecondFlrSF  +MasVnrArea
         ,data.train)


testing.prices.testinglog8<-predict(reg8, data.test) #predict the prices of the 439 houses left for testing the model
testing.prices.testing8<-exp(testing.prices.testinglog8)

percent.errors8 <- abs((data.test$SalePrice-testing.prices.testing8)/data.test$SalePrice)*100 #calculate absolute percentage errors
mean(percent.errors8) #display Mean Absolute Percentage Error (MAPE) #8.864269, 8.525043

RMSLE(data.test$SalePrice,testing.prices.testing8) #Display RMSLE #0.1304775,  0.1241769
#improvement in MAPE and RMSLE



#remove interactions and variables 
reg9<-lm(log(SalePrice)~log(OverallQual) + log(YearBuilt) + YearRemodAdd*remodledafterbuilt
         + TotalBsmtSF +  log(FirstFlrSF) + GarageCars +  GarageArea*GarageCars*GarageYrBlt 
       + log(GrLivArea) +GrLivArea*FirstFlrSF   + log(TotRmsAbvGrd)  + Central.Air 
          +ExterQual +BedroomAbvGr  +KitchenQual  + HeatingQC + BsmtQual 
       +GasA.Heating    +BldgType  +WoodDeckSF +Garage+ GarageFinish
       + BsmtFullBath   +SaleCondition   + FullBath   +GarageCond  +FireplaceQu 
       +log(LotArea) +LotArea*GrLivArea  +Neighborhood +Newhouse +MiscVal  +MSSubClass
       +Functional  +GarageQual  +OverallCond +YrSold +BsmtCond +LotConfig+SecondFlrSF 
       +LotArea*MasVnrArea+LotArea*FirstFlrSF*SecondFlrSF  +MasVnrArea
       ,data.train)


testing.prices.testinglog9<-predict(reg9, data.test) #predict the prices of the 439 houses left for testing the model
testing.prices.testing9<-exp(testing.prices.testinglog9)

percent.errors9 <- abs((data.test$SalePrice-testing.prices.testing9)/data.test$SalePrice)*100 #calculate absolute percentage errors
mean(percent.errors9) #display Mean Absolute Percentage Error (MAPE) #9.087089,  8.968252

RMSLE(data.test$SalePrice,testing.prices.testing9) #Display RMSLE #0.1309133,  0.1300327
# MAPE and RMSLE increased- too many new variables 


