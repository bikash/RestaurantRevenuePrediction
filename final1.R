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
setwd("/Users/bikash/repos/RestaurantRevenuePrediction/datascience/")



library(caret)
library(data.table)
library(Boruta)
library(randomForest)

train = read.csv("data/train.csv", header = TRUE, stringsAsFactors = FALSE)
test = read.csv("data/test.csv", header = TRUE, stringsAsFactors = FALSE)
n.train <- nrow(train)



test$revenue <- 1
dat <- rbind(train, test)
rm(train, test)
#Convert date-time vvalue
# parse days open from start of competetion
competition_start <- strptime('01.01.2015', format='%d.%m.%Y')
dat$days_open <- as.numeric(difftime(competition_start,
                                     strptime(dat$Open.Date, format='%m/%d/%Y'), units='days'))
dat$weeks_open <- as.numeric(difftime(competition_start,
                                      strptime(dat$Open.Date, format='%m/%d/%Y'), units='weeks'))
dat$months_open <- as.numeric(difftime(competition_start,
                                       strptime(dat$Open.Date, format='%m/%d/%Y'), units='days')/30)
dat$years_open <- as.numeric(difftime(competition_start,
                                      strptime(dat$Open.Date, format='%m/%d/%Y'), units='days')/365)




#dat$Open.Date <- as.POSIXlt("01/01/2015", format="%m/%d/%Y") - as.POSIXlt(dat$Open.Date, format="%m/%d/%Y")
#dat$Open.Date <- as.numeric(dat$Open.Date / 1000) #Scale for factors

dat$Type <- as.factor(dat$Type)
dat$City.Group<-as.factor(dat$City.Group)

#View(dat)
#Log transform of P Variables and Revenue
dat[, paste("P", 1:37, sep="")] <- log(1 +dat[, paste("P", 1:37, sep="")])

dat$revenue <- log(dat$revenue)

cat("Using boruta for feature selection\n")
features <- names(dat)[1:length(dat)]
important <- Boruta(revenue~., data=dat[1:n.train, ])
important$finalDecision
print(important$finalDecision)
important_features <- features[important$finalDecision!="Rejected"]

dat$Open.Date <-NULL
dat$City <-NULL
dat$Type[dat$Type == "DT"] <- "IL"
dat$Type[dat$Type == "MB"] <- "FC"



RandomForestRegression_CV <- function(X_train,y,X_test=data.frame(),cv=5,ntree=50,nodesize=5,seed=123,metric="mae")
{
  score <- function(a,b,metric)
  {
    switch(metric,
           mae = sum(abs(a-b))/length(a),
           rmse = sqrt(sum((a-b)^2)/length(a)))
  }
  
  cat("Preparing Data\n")
  X_train$order <- seq(1, nrow(X_train))
  X_train$result <- as.numeric(y)
  
  set.seed(seed)
  X_train$randomCV <- floor(runif(nrow(X_train), 1, (cv+1)))
  
  cat(cv, "-fold Cross Validation\n", sep = "")
  for (i in 1:cv)
  {
    X_build <- subset(X_train, randomCV != i, select = -c(order, randomCV))
    X_val <- subset(X_train, randomCV == i) 
    
    model_rf <- randomForest(result ~., data = X_build, ntree = ntree, nodesize = nodesize)
    
    pred_rf <- predict(model_rf, X_val)
    X_val <- cbind(X_val, pred_rf)
    
    if (nrow(X_test) > 0)
    {
      pred_rf <- predict(model_rf, X_test)
    }
    
    cat("CV Fold-", i, " ", metric, ": ", score(X_val$result, X_val$pred_rf, metric), "\n", sep = "")
    
    if (i == 1)
    {
      output <- X_val
      if (nrow(X_test) > 0)
      {
        X_test <- cbind(X_test, pred_rf)
      }      
    }
    
    if (i > 1)
    {
      output <- rbind(output, X_val)
      if (nrow(X_test) > 0)
      {
        X_test$pred_rf <- (X_test$pred_rf * (i-1) + pred_rf)/i
      }            
    }
    
    gc()
  } 
  
  output <- output[order(output$order),]
  cat("\nRandomForest ", cv, "-Fold CV ", metric, ": ", score(output$result, output$pred_rf, metric), "\n", sep = "")
  
  output <- subset(output, select = c("order", "pred_rf"))
  return(list(output, X_test))  
}

# building model on log of revenue
result <- dat$revenue[1:n.train] 
# splitting into train and test
X_train <- dat[1:n.train,important_features[-c(1,2)]]
X_test <- dat[(n.train+1):nrow(dat),important_features[-c(1,2)]]
# 5-fold cross validation and scoring
model_rf_1 <- RandomForestRegression_CV(X_train,result,X_test,cv=5,ntree=100,nodesize=5,seed=235,metric="rmse")
model_rf_2 <- RandomForestRegression_CV(X_train,result,X_test,cv=5,ntree=100,nodesize=5,seed=357,metric="rmse")

## submission
test_rf_1 <- model_rf_1[[2]]
test_rf_2 <- model_rf_2[[2]]

test_id<-cbind(seq(0, length(test_rf_1$pred_rf) - 1, by=1))

pDat<-as.data.frame(cbind(seq(0, length(prediction) - 1, by=1), (0.5*exp(test_rf_1$pred_rf) + 0.5*exp(test_rf_2$pred_rf))))

md<-dat[-c(1:n.train),]
colnames(pDat)<-c("Id","Prediction")
predicteddat <- merge(md,pDat,by="Id")
DT <- data.table(predicteddat)
# cluster all records by Open.Date, City, Type and City.Group and assign for each record in cluster mean of cluster elements
DT<-DT[, Mean:=mean(Prediction), by=list(years_open,days_open,City.Group,Type )]

#make final submission
submit<-as.data.frame(cbind(DT$Id,DT$Mean))
colnames(submit)<-c("Id","Prediction")


#submit <- data.frame("Id" = test_id, "Prediction" = 0.5*exp(test_rf_1$pred_rf) + 0.5*exp(test_rf_2$pred_rf) )

write.csv(submit, "../output/submit.csv", row.names=F)