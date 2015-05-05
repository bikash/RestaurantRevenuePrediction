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
library(Matrix)
#load data
train = read.csv("data/train.csv", header = TRUE, stringsAsFactors = FALSE)
test = read.csv("data/test.csv", header = TRUE, stringsAsFactors = FALSE)

## see http://bit.ly/1CjehL9

one_hot <- function(df){
  n <- nrow(df)
  nlevels <- sapply(df, nlevels)
  i <- rep(seq_len(n), ncol(df))
  j <- unlist(lapply(df, as.integer)) +
    rep(cumsum(c(0, head(nlevels, -1))), each = n)
  x <- 1
  res <- sparseMatrix(i = i, j = j, x = x)
  
  return(res)
  
}

################################################################
################################################################
################################################################
################################################################
################################################################

cv.nround <- 100
nfold <- 4



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

test$revenue <- 1
myData <- rbind(train, test)
## determine the age of resturant
myData$is.New<-ifelse(round(myData$years_open,0)<=1,1,0)
myData$is.Mature<-ifelse(round(myData$years_open,0)>1 & myData$years_open<5,1,0)
myData$is.Old<-ifelse(round(myData$years_open,0)>=5,1,0)


## preprocessing
myData$Type1<-NULL
myData$Type <- as.character(myData$Type)
myData$Type1[myData$Type=="DT"] = 1
myData$Type1[myData$Type == "FC"] = 2
myData$Type1[myData$Type == "IL"] = 3
myData$Type1[myData$Type == "MB"] = 4
myData$Type1[myData$Type == train$Type[125]] = 1
# 
myData$CityGp[myData$City.Group == "Big Cities"] = 1
myData$CityGp[myData$City.Group == "Other"] = 2
# 

#if(invalid(myData$P2)) print("yes:")

# remove unneeded columns
myData$City <- NULL
myData$Open.Date <- NULL
myData$City.Group <- NULL
myData$Type <- NULL
#myData$is.New <- NULL
#myData$is.Old <- NULL
# remove outliers
myData <- myData[myData$revenue < 16000000,]

myData$revenue <- log(myData$revenue)
#important <- Boruta(revenue~., data=myData[1:137, ])
## no of taining set
n.train <- nrow(train)
print(n.train)
train_cols<-myData[,c(2:38,40:44)]
labels<-as.matrix(myData[,39])
mydata = myData[,-1]


y <- myData$revenue[1:135] ##train revenue

## (in order to do so, remove the revenue value from the train set)
x <- myData[,-39]


#remove the id value which has no meaning

x <- subset(x, select=-c(Id))

#Change the date into years and months (as factors!)
# date <- as.character(x$Open.Date)
# date2 <- strptime(date, format="%m/%d/%Y")
# year <- as.POSIXlt(date2)$year + 1900
# month <- as.POSIXlt(date2)$mon + 1
# x <- subset(x, select=-c(Open.Date))
# x$year <- year
# x$month <- month
# x$month <- as.factor(x$month)
# x$year <- as.factor(x$year)

## apply one-hot encodying

#x <- one_hot(x)

n <- nrow(x)
nlevels <- sapply(x, nlevels)
i <- rep(seq_len(n), ncol(x))
j <- unlist(lapply(x, as.integer)) +
  rep(cumsum(c(0, head(nlevels, -1))), each = n)
j[j == 0] <- 1
len <- length(j)
x1<-1
x = sparseMatrix(i = i, j = j, x =1)



trind = 1:length(y) ## rows from the training dataset in x
teind = (nrow(train)+1):nrow(x) ## rows from the test dataset in x


# Set necessary parameter
param <- list("objective" = "reg:linear",
              "max_depth"=6,
              "eta"=0.1,
              "subsample"=1,
              "gamma"=1,
              "min_child_weight"=1,
              "eval_metric" = "mlogloss",
              "silent"=1,
              "num_class" = 1,
              "nthread" = 6)

# Run Cross Valication
bst.cv = xgb.cv(param=param, data = x[trind,], label = y,
                nfold = nfold, nrounds=cv.nround)


#shuffle
#trind <- trind[sample(nrow(trind)),]

# Train the model
nround = 100
bst = xgboost(param=param, data = x[trind,], label = y, nrounds=nround)

# Make prediction
pred = predict(bst,x[teind,])
pred = matrix(pred,1,length(pred)/1)
pred = t(pred)

# Output submission
pred = format(pred, digits=5,scientific=F) # shrink the size of submission
pred = data.frame(1:nrow(pred),exp(pred))
names(pred) = c('Id', paste0('Prediction'))
write.csv(pred,file='output/xgboost_6.csv', quote=FALSE,row.names=FALSE)
