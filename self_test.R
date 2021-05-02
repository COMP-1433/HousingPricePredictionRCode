# import data
train<- read.csv("train.csv")
test<- read.csv("test.csv") 

train<-subset(train,select = -c(Utilities))
test<-subset(test,select = -c(Utilities))

train
#typeof(train)
#typeof(test)



train_char <- train[,sapply(train,is.character)]
#train_char
#the character data in train 
train_int <- train[,sapply(train,is.integer)]
train_int
#train_int
#the integer data in train 
test_char <- test[,sapply(test,is.character)]
#test_char
test_int <- test[,sapply(test,is.integer)]
#test_int

# Qi Shihao
train[is.na(train)]<-0

train$MSZoning[train$MSZoning=="C (all)"]<-1
train$MSZoning[train$MSZoning=="FV"]<-2
train$MSZoning[train$MSZoning=="RH"]<-3
train$MSZoning[train$MSZoning=="RL"]<-4
train$MSZoning[train$MSZoning=="RM"]<-5
train$MSZoning=as.numeric(train$MSZoning)

train$Street[train$Street=="Grvl"]=1
train$Street[train$Street=="Pave"]=2
train$Street=as.numeric(train$Street)

train$Alley[train$Alley=="Grvl"]=1
train$Alley[train$Alley=="Pave"]=2
train$Alley=as.numeric(train$Alley)

train$LotShape[train$LotShape=="IR1"]=1
train$LotShape[train$LotShape=="IR2"]=2
train$LotShape[train$LotShape=="IR2"]=3
train$LotShape[train$LotShape=="Reg"]=4
train$LotShape=as.numeric(train$LotShape)

train$LandContour[train$LandContour=="Bnk"]=1
train$LandContour[train$LandContour=="HLS"]=2
train$LandContour[train$LandContour=="Low"]=3
train$LandContour[train$LandContour=="Lvl"]=4
train$LandContour=as.numeric(train$LandContour)

train$LotConfig[train$LotConfig=="Corner"]=1
train$LotConfig[train$LotConfig=="CulDSac"]=2
train$LotConfig[train$LotConfig=="FR2"]=3
train$LotConfig[train$LotConfig=="FR3"]=4
train$LotConfig[train$LotConfig=="Inside"]=5
train$LotConfig=as.numeric(train$LotConfig)

train$LandSlope[train$LandSlope=="Gtl"]=1
train$LandSlope[train$LandSlope=="Mod"]=2
train$LandSlope[train$LandSlope=="Sev"]=3

train$Neighborhood[train$Neighborhood=="Blmngtn"]=1
train$Neighborhood[train$Neighborhood=="Blueste"]=2
train$Neighborhood[train$Neighborhood=="BrDale"]=3
train$Neighborhood[train$Neighborhood=="BrkSide"]=4
train$Neighborhood[train$Neighborhood=="ClearCr"]=5
train$Neighborhood[train$Neighborhood=="CollgCr"]=6
train$Neighborhood[train$Neighborhood=="Crawfor"]=7
train$Neighborhood[train$Neighborhood=="Edwards"]=8
train$Neighborhood[train$Neighborhood=="Gilbert"]=9
train$Neighborhood[train$Neighborhood=="IDOTRR"]=10
train$Neighborhood[train$Neighborhood=="MeadowV"]=11
train$Neighborhood[train$Neighborhood=="Mitchel"]=12
train$Neighborhood[train$Neighborhood=="NAmes"]=13
train$Neighborhood[train$Neighborhood=="NoRidge"]=14
train$Neighborhood[train$Neighborhood=="NPkVill"]=15
train$Neighborhood[train$Neighborhood=="NridgHt"]=16
train$Neighborhood[train$Neighborhood=="NWAmes"]=17
train$Neighborhood[train$Neighborhood=="OldTown"]=18
train$Neighborhood[train$Neighborhood=="Sawyer"]=19
train$Neighborhood[train$Neighborhood=="Somerst"]=20
train$Neighborhood[train$Neighborhood=="StoneBr"]=21
train$Neighborhood[train$Neighborhood=="SWISU"]=22
train$Neighborhood[train$Neighborhood=="Timber"]=23
train$Neighborhood[train$Neighborhood=="Veenker"]=24
train$Neighborhood=as.numeric(train$Neighborhood)


#HE Zhejun 
train$MasVnrType[is.na(train$MasVnrType)]<-"0"
train$MasVnrType[train$MasVnrType=="BrkCmn"]<-"1"
train$MasVnrType[train$MasVnrType=="BrkFace"]<-"2"
train$MasVnrType[train$MasVnrType=="CBlock"]<-"3"
train$MasVnrType[train$MasVnrType=="Stone"]<-"4"
train$MasVnrType<-as.numeric(train$MasVnrType)

train$MasVnrArea[is.na(train$MasVnrArea)] <- median(na.omit(train$MasVnrArea))
#中位数

train$Foundation[train$Foundation=="BrkTil"]<-6
train$Foundation[train$Foundation=="CBlock"]<-5
train$Foundation[train$Foundation=="PConc"]<-4
train$Foundation[train$Foundation=="Slab"]<-3
train$Foundation[train$Foundation=="Stone"]<-2
train$Foundation[train$Foundation=="Wood"]<-1
train$Foundation<-as.numeric(train$Foundation)


train$ExterQual<-as.character(train$ExterQual)
train$ExterQual[train$ExterQual=="Ex"]<-"5"
train$ExterQual[train$ExterQual=="Gd"]<-"4"
train$ExterQual[train$ExterQual=="TA"]<-"3"
train$ExterQual[train$ExterQual=="Fa"]<-"2"
train$ExterQual[train$ExterQual=="Po"]<-"1"
train$ExterQual<-as.numeric(train$ExterQual)

train$ExterCond<-as.character(train$ExterCond)
train$ExterCond[train$ExterCond=="Ex"]<-"5"
train$ExterCond[train$ExterCond=="Gd"]<-"4"
train$ExterCond[train$ExterCond=="TA"]<-"3"
train$ExterCond[train$ExterCond=="Fa"]<-"2"
train$ExterCond[train$ExterCond=="Po"]<-"1"
train$ExterCond<-as.numeric(train$ExterCond)


train$BsmtQual <- as.character(train$BsmtQual)
train$BsmtQual[train$BsmtQual == "Ex"] <- "5"
train$BsmtQual[train$BsmtQual == "Gd"] <- "4"
train$BsmtQual[train$BsmtQual == "TA"] <- "3"
train$BsmtQual[train$BsmtQual == "Fa"] <- "2"
train$BsmtQual[train$BsmtQual == "Po"] <- "1"
train$BsmtQual[is.na(train$BsmtQual)] <- "0"
train$BsmtQual <- as.numeric(train$BsmtQual)


train$BsmtCond <- as.character(train$BsmtCond)
train$BsmtCond[train$BsmtCond == "Ex"] <- "5"
train$BsmtCond[train$BsmtCond == "Gd"] <- "4"
train$BsmtCond[train$BsmtCond == "TA"] <- "3"
train$BsmtCond[train$BsmtCond == "Fa"] <- "2"
train$BsmtCond[train$BsmtCond == "Po"] <- "1"
train$BsmtCond[is.na(train$BsmtCond)] <- "0"
train$BsmtCond <- as.numeric(train$BsmtCond)


train$BsmtExposure <- as.character(train$BsmtExposure)
train$BsmtExposure[train$BsmtExposure == "Gd"] <- "4"
train$BsmtExposure[train$BsmtExposure == "Av"] <- "3"
train$BsmtExposure[train$BsmtExposure == "Mn"] <- "2"
train$BsmtExposure[train$BsmtExposure == "No"] <- "1"
train$BsmtExposure[is.na(train$BsmtExposure)] <- "0"
train$BsmtExposure <- as.numeric(train$BsmtExposure)

train$BsmtFinType1 <- as.character(train$BsmtFinType1)
train$BsmtFinType1[train$BsmtFinType1 == "GLQ"] <- "6"
train$BsmtFinType1[train$BsmtFinType1 == "ALQ"] <- "5"
train$BsmtFinType1[train$BsmtFinType1 == "BLQ"] <- "4"
train$BsmtFinType1[train$BsmtFinType1 == "Rec"] <- "3"
train$BsmtFinType1[train$BsmtFinType1 == "LwQ"] <- "2"
train$BsmtFinType1[train$BsmtFinType1 == "Unf"] <- "1"
train$BsmtFinType1[is.na(train$BsmtFinType1)] <- "0"
train$BsmtFinType1 <- as.numeric(train$BsmtFinType1)


train$BsmtFinSF1[is.na(train$BsmtFinSF1)] <- median(na.omit(train$BsmtFinSF1))
# 中位数

train$BsmtFinType2 <- as.character(train$BsmtFinType2)
train$BsmtFinType2[train$BsmtFinType2 == "GLQ"] <- "6"
train$BsmtFinType2[train$BsmtFinType2 == "ALQ"] <- "5"
train$BsmtFinType2[train$BsmtFinType2 == "BLQ"] <- "4"
train$BsmtFinType2[train$BsmtFinType2 == "Rec"] <- "3"
train$BsmtFinType2[train$BsmtFinType2 == "LwQ"] <- "2"
train$BsmtFinType2[train$BsmtFinType2 == "Unf"] <- "1"
train$BsmtFinType2[is.na(train$BsmtFinType2)] <- "0"
train$BsmtFinType2 <- as.numeric(train$BsmtFinType2)

train$BsmtFinSF2[is.na(train$BsmtFinSF2)] <- 0

train$BsmtUnfSF[is.na(train$BsmtUnfSF)] <- 0

train_char <- train[,sapply(train,is.character)]
train_char 

