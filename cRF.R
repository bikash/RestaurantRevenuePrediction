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
setwd("/Users/bikash/repos/kaggle/RestaurantRevenuePrediction/")
#setwd("/home/ekstern/haisen/bikash/kaggle/RestaurantRevenuePrediction/")

library(party)
#library(Boruta)
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

test$revenue <- 1
myData <- rbind(train, test)
## determine the age of resturant
myData$is.New<-ifelse(round(myData$years_open,0)<=2,1,0)
myData$is.Old<-ifelse(round(myData$years_open,0)>=3,1,0)


## preprocessing
myData$TypeNo[train$Type == "DT"] = 1
myData$TypeNo[myData$Type == "FC"] = 2
myData$TypeNo[myData$Type == "IL"] = 3
myData$TypeNo[myData$Type == "MB"] = 4
# 
myData$CityGp[myData$City.Group == "Big Cities"] = 1
myData$CityGp[myData$City.Group == "Other"] = 2
# 
library(gtools)
if(invalid(myData$P2)) print("yes:")

# remove unneeded columns
myData$City <- NULL
myData$Open.Date <- NULL
myData$City.Group <- NULL
myData$Type <- NULL

myData$revenue <- log(myData$revenue)

data5<-myData[1:45]
library(Boruta)
important <- Boruta(revenue~., data=data5[1:137, ])

set.seed(415)

# remove outliers
train <- train[train$revenue < 16000000,]

rf = cforest(revenue ~., data = train[,-1], controls=cforest_unbiased(ntree=2000))

Prediction = predict(rf, test[,-1], OOB=TRUE, type = "response")

id<-test[,1]
submission<-cbind(id,Prediction)
colnames(submission)[2] <- "Prediction"

write.csv(submission, "output/conditional_forest_tree_1000.csv", row.names = FALSE, quote = FALSE)




n.train <- nrow(train)
test$revenue <- 1
#myData <- rbind(train, test)
#Log Transform P Variables and Revenue
#myData[, paste("P", 1:37, sep="")] <- log(1 +myData[, paste("P", 1:37, sep="")])


#train$revenue <- log(train$revenue)
#important <- Boruta(revenue~., data=myData[1:n.train, ])


# cities = as.factor(train$cities)
# ## create cities CSV
# out <- data.frame(id = test$Id, Prediction=cities)
# write.csv(out, "data/cities.csv", row.names = FALSE, quote = FALSE)
# 


set.seed(24501)
#Conditional Inference Tree
model <- ctree(revenue ~., data=train)
plot(fit, main="Conditional Inference Tree for Kyphosis")

rf = cforest(revenue ~., data = train[,-1], controls=cforest_unbiased(ntree=2000, mtry=3))
Prediction = predict(rf, test[,-1], OOB=TRUE, type = "response")

id<-test[,1]
submit<-as.data.frame(cbind(id, exp(Prediction)))
colnames(submit)<-c("Id","Prediction")
write.csv(submit,"submission_ctree.csv",row.names=FALSE,quote=FALSE)





## Extra trees
library(extraTrees)
mt_noexp<-model.matrix(as.formula(revenue~.), data=train[,-1])
fit<-extraTrees((mt_noexp, train$Id, mtry=3, nodesize=10, numTreads=4, ntree=2000, numRandomCuts=6, evenCuts=F)

write.csv(submission, "output/crfn_city_type_2000.csv", row.names = FALSE, quote = FALSE)

## GBM
library(gbm)
library(hydroGOF)
gbm_model <- gbm(revenue~.,        # dataset
                     var.monotone=NULL,
                     distribution="gaussian",
                     data= train,
                     n.trees=2000,                # number of trees
                     shrinkage=0.001,             # shrinkage or learning rate,0.001 to 0.1 usually work
                     interaction.depth=8,         # 1: additive model, 2: two-way interactions, etc.
                     bag.fraction = 0.6,          # subsampling fraction, 0.5 is probably best
                     train.fraction = 0.8,        # fraction of data for training, first train.fraction*N used for training
                     n.minobsinnode = 10,         # minimum total weight needed in each node
                     keep.data=FALSE,              # keep a copy of the dataset with the object
                     verbose=TRUE)                # print out progress
best_iter <- gbm.perf(gbm_model, method="test")
print(best_iter)
pred <- predict(gbm_model, test[,-1], 500, type="response")
print(pred)

out <- data.frame(Id = test$Id, Prediction=pred)
write.csv(out, file = "output/gbm_submission.csv", row.names = FALSE)

### 1790063.09235

score = rmse(train$revenue, pred)
print(score)
#########################################################################
#########################################################################
## Random Forest
#########################################################################
library(randomForest)
r = randomForest(revenue~., data = train[,-1], importance =TRUE, ntree=2000)
importance(r)
varImpPlot(r)
#prediction
Prediction <- predict(r, test, OOB=TRUE, type = "response")
#prop.table(table(test$target, Prediction),1)

#########################################################################
out <- data.frame(id = test$Id, Prediction=Prediction)
write.csv(out, file = "output/RandomForest.csv", row.names = FALSE)
#########################################################################
#########################################################################

#########################################################################
#########################################################################
## Decision Tree
#########################################################################

library(dplyr)
library(zoo)
library(rattle)
library(rpart.plot)
library(RColorBrewer)
library(lattice)
library(Amelia) ## Amelia is packages to display missing data using missmap function
library(rpart)


fit <- rpart(revenue ~., data=train, method="class", control=rpart.control(minsplit=2, cp=0))
plot(fit)
text(fit)
### Make a fancy plot for decision tree #################################

Prediction <- predict(fit, test, type = "class")
out <- data.frame(id = test$Id, Prediction = Prediction)
write.csv(out, file = "output/Decision_tree.csv", row.names = FALSE)
## Score 2621580.12472

#########################################################################
#########################################################################
## SVM
#########################################################################


