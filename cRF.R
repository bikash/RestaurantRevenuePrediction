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
library(e1071)
library(lubridate)
library(Boruta)
library(gtools)
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
set.seed(2234)


important <- Boruta(revenue~., data=mydata[1:135, ])

#Random Forest
set.seed(24501)
model <- cforest(revenue~., data=myData[1:135, c(important$finalDecision != "Rejected", TRUE)], controls=cforest_unbiased(ntree=1000))


rf = cforest(revenue ~., data = mydata[1:135,], controls=cforest_unbiased(ntree=1000))

#SVM
svm.model<- svm(x=as.matrix(train_cols),y=labels, cost=10,scale=TRUE,type="eps-regression")
#svm.model<-svm(revenue~., data=mydata[1:135, ], cost = 75, gamma=0.001, kernel = 'radial')

## GBM
library(gbm)
gbm.fit <- gbm(revenue~.,data=mydata[1:135,], cv.folds=10, n.trees=5000, distribution="gaussian", interaction.depth=3, bag.fraction=0.5, train.fraction=1.0, shrinkage=0.05, keep.data=TRUE)

## neural net
library(caret)
library(nnet)
train<-as.factor(train)
fit <- ctree(revenue ~ ., data=mydata[1:135,], controls = ctree_control(mincriterion = 0, minsplit = 8, minbucket = 6))
newNet<-nnet(revenue~ . ,data=mydata[1:135,], size=2, type = "class")
nn.pred <- predict(newNet, newdata=mydata[-c(1:135), ])
summary(nn.pred)
# pred<-factor(pred,levels=unlist(strsplit('A B C D E F G H I J K L M N O P',' ')))



#Make a Prediction
rf.pred = predict(rf, mydata[-c(1:135), ], OOB=TRUE, type = "response")
svm.pred <-  predict(svm.model, mydata[-c(1:135), ])
gbm.pred <-  predict(gbm.fit, mydata[-c(1:135), ])

cf.pred = predict(rf, mydata[-c(1:135), ], OOB=TRUE, type = "response")

## randomForest
library(randomForest)
fit_rf<- randomForest(revenue~.,data=mydata[1:135,],type="regression",prox=TRUE, ntree=1000)
rf1.pred<-predict(fit_rf, mydata[-c(1:135), ])

## combine model
pred = rowMeans(cbind(exp(rf.pred),exp(cf.pred)))



id<-test[,1]
submission<-cbind(id,pred)
colnames(submission)[2] <- "Prediction"

write.csv(submission, "output/conditional_forest_imp.csv", row.names = FALSE, quote = FALSE)





##importance variable plot
library(ggplot2)
library(randomForest)
rf <- randomForest(revenue~., data=mydata[1:135, ], importance=TRUE)
imp <- importance(rf, type=1)
featureImportance <- data.frame(Feature=row.names(imp), Importance=imp[,1])

p <- ggplot(featureImportance, aes(x=reorder(Feature, Importance), y=Importance)) +
  geom_bar(stat="identity", fill="#53cfff") +
  coord_flip() + 
  theme_light(base_size=20) +
  xlab("Importance") +
  ylab("") + 
  ggtitle("Random Forest Feature Importance\n") +
  theme(plot.title=element_text(size=18))

ggsave("2_feature_importance.png", p)

## t-SNE visualixation
library(Rtsne)
numeric_features <- train[,c(-1,-2,-3,-4,-5,-43)]
tsne <- Rtsne(as.matrix(numeric_features), check_duplicates = FALSE, pca = TRUE, 
              perplexity=30, theta=0.5, dims=2)

embedding <- as.data.frame(tsne$Y)
embedding$Revenue  <- train$revenue/1e6
embedding$Type     <- train$Type
embedding$CityType <- train[["City.Group"]]

p <- ggplot(embedding, aes(x=V1, y=V2, color=Revenue, shape=CityType)) +
  geom_point(size=4) +
  scale_colour_gradientn(colours=c("#3288bd","#66c2a5","#abdda4","#e6f598","#fee08b","#fdae61","#f46d43","#d53e4f"), name="Revenue ($M)") + 
  xlab("") + ylab("") +
  theme_light(base_size=20) +
  ggtitle("t-SNE Restaurant Visualization") + 
  theme(strip.background = element_blank(),
        strip.text.x     = element_blank(),
        axis.text.x      = element_blank(),
        axis.text.y      = element_blank(),
        axis.ticks       = element_blank(),
        axis.line        = element_blank(),
        panel.border     = element_blank())

ggsave("tsne.png", p, height=8, width=8, units="in")









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


