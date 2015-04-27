# Load in data

### setting path of repo folder.
getwd()
setwd("/Users/bikash/repos/kaggle/RestaurantRevenuePrediction/")
library(plyr)
library(party)
library(Boruta)
#load data
train = read.csv("data/train.csv", header = TRUE, stringsAsFactors = FALSE)
test = read.csv("data/test.csv", header = TRUE, stringsAsFactors = FALSE)


train <- data[with(train, order(revenue)),]
rows <- nrow(train)
for (n in c(1:rows)) {
  train$rev_cluster[n] <- LETTERS[ceiling(n/20)+1]
}
train$rev_cluster[1:2] <- 'A'
train$rev_cluster[c(rows-2,rows)] <- 'P'
test$rev_cluster <- 'Z'
test$revenue <- NaN
rev_cluster <- ddply(train,.(rev_cluster),summarize,max=max(revenue), min=min(revenue), count=length(revenue))
rev_cluster$pct<-rev_cluster$max/rev_cluster$min
combined <- rbind(test,train)

competition_start <- strptime('23.03.2015', format='%d.%m.%Y')
combined$days_open <- as.numeric(difftime(competition_start,
                                       strptime(combined$Open.Date, format='%m/%d/%Y'), units='days'))
combined$weeks_open <- as.numeric(difftime(competition_start,
                                        strptime(combined$Open.Date, format='%m/%d/%Y'), units='weeks'))
combined$years_open <- as.numeric(difftime(competition_start,
                                       strptime(combined$Open.Date, format='%m/%d/%Y'), units='days')/365)

combined$Open.Date <- NULL
combined$revenue <- NULL
combined$Id <- NULL
combined$rev_cluster <- as.factor(combined$rev_cluster)
ncol(combined)