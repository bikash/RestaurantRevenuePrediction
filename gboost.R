#################################################################################################
#################################################################################################
## Author: Bikash Agrawal
## Date: 12th March Feb 2015
## Email: er.bikash21@gmail.com
## Description: Restaurant prediction
##          
## References: 
## [1] http://dnene.bitbucket.org/docs/mlclass-notes/lecture16.html
## [2] 
## 
#################################################################################################
#################################################################################################
#load dependencies

### setting path of repo folder.
getwd()
setwd("/Users/bikash/repos/kaggle/RestaurantRevenuePrediction/")
library(party)
library(xgboost)
require(methods)
#load data
train = read.csv("data/train.csv", header = TRUE, stringsAsFactors = FALSE)
test = read.csv("data/test.csv", header = TRUE, stringsAsFactors = FALSE)

# parse days open from start of competetion
competition_start <- strptime('23.03.2015', format='%d.%m.%Y')
train$days_open <- as.numeric(difftime(competition_start,
                                       strptime(train$Open.Date, format='%m/%d/%Y'), units='days'))
test$days_open <- as.numeric(difftime(competition_start,
                                      strptime(test$Open.Date, format='%m/%d/%Y'), units='days'))

train$weeks_open <- as.numeric(difftime(competition_start,
                                        strptime(train$Open.Date, format='%m/%d/%Y'), units='weeks'))
test$weeks_open <- as.numeric(difftime(competition_start,
                                       strptime(test$Open.Date, format='%m/%d/%Y'), units='weeks'))

train$years_open <- as.numeric(difftime(competition_start,
                                        strptime(train$Open.Date, format='%m/%d/%Y'), units='days')/365)
test$years_open <- as.numeric(difftime(competition_start,
                                       strptime(test$Open.Date, format='%m/%d/%Y'), units='days')/365)


## preprocessing
train$TypeNo[train$Type == "DT"] = 1
train$TypeNo[train$Type == "FC"] = 2
train$TypeNo[train$Type == "IL"] = 3
train$TypeNo[train$Type == "MB"] = 4

test$TypeNo[test$Type == "DT"] = 1
test$TypeNo[test$Type == "FC"] = 2
test$TypeNo[test$Type == "IL"] = 3
test$TypeNo[test$Type == "MB"] = 4

train$CityGp[train$City.Group == "Big Cities"] = 1
train$CityGp[train$City.Group == "Other"] = 2

test$CityGp[test$City.Group == "Big Cities"] = 1
test$CityGp[test$City.Group == "Other"] = 2

# remove unneeded columns
train$City <- NULL
train$Open.Date <- NULL
train$City.Group <- NULL
train$Type <- NULL
test$City <- NULL
test$Open.Date <- NULL
test$City.Group <- NULL
test$Type <- NULL

train = train[,-1]
test1 = test[,-1]
test = test1
y = train[,ncol(train)]
y = gsub('Class_','',y)
y = as.integer(y)-1 #xgboost take features in [0,numOfClass)

x = rbind(train[,-38],test)
x = as.matrix(x)
x = matrix(as.numeric(x),nrow(x),ncol(x))
trind = 1:length(y)
teind = (nrow(train)+1):nrow(x)

# Set necessary parameter
param <- list("objective" = "multi:softprob",
              "eval_metric" = "mlogloss",
              "num_class" = 2,
              "nthread" = 8,
              'max_depth' = 10,
              "min_child_weight" = 4,
              "gamma" = 1,
              "subsample" = .9,
              "colsample_bytree" = .8)

#shuffle
trind <- trind[sample(nrow(trind)),]

# Run Cross Valication
cv.nround = 200
bst.cv = xgb.cv(param=param, data = x[trind,], label = y, nfold = 3, nrounds=cv.nround)

#shuffle
trind <- trind[sample(nrow(trind)),]

# Train the model
nround = 200
bst = xgboost(param=param, data = x[trind,], label = y, nrounds=nround)

# Make prediction
pred = predict(bst,x[teind,])
pred = matrix(pred,1,length(pred)/1)
pred = t(pred)

# Output submission
pred = format(pred, digits=2,scientific=F) # shrink the size of submission
pred = data.frame(1:nrow(pred),pred)
names(pred) = c('Id', paste0('Prediction'))
write.csv(pred,file='output/xgboost_6.csv', quote=FALSE,row.names=FALSE)
