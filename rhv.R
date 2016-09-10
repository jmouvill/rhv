library(magrittr)
library(ggplot2)
library(GGally)
library(dplyr)
library(plyr)
library(data.table)  
#source("/Users/jmvllt/Documents/workspace/R/Foot/FootFunctions.R")

people <- fread("/Users/Jean/Documents/workspace/rhv/people.csv")

train <- fread("/Users/Jean/Documents/workspace/rhv/act_train.csv")
train <- data.frame(train)
train$outcome <- factor(train$outcome)
test <- fread("/Users/Jean/Documents/workspace/rhv/act_test.csv")
test <- data.frame(test)
test %>% names()
test$outcome <- factor(test$outcome)

summ <- fread("/Users/Jean/Documents/workspace/rhv/sample_submission.csv")
summ %>% head()

train %>% head()
train %>% summary()

numberDistinctPeople <- nrow(distinct(train[,c("people_id","people_id")]))

##data cleaning for training set
t <- grepl("char_",names(train))
charFeatureTrain <- names(train)[t]
for(feature in charFeatureTrain){
  train[[feature]] <- substr(train[[feature]], 6, 100)
  train[[feature]] <- factor(ifelse(train[[feature]] == '', 0,train[[feature]]) )
}
train$activity_category <- factor(substr(train$activity_category, 6, 100))
train <- subset(train, select = -c(activity_id) )

##data cleaning for people set
people %>% head()
i <- 1
while (i < 10){
  feature <- paste('char_',i,sep='')
  people[[feature]] <- factor(substr(people[[feature]], 6, 100))
  i <- i +1
}
j <- 10
while(j<38){
  feature <- paste('char_',j,sep='')
  people[[feature]] <- factor(ifelse(people[[feature]] == TRUE, 1,0))
  j <- j+1
}
people$group_1 <- factor(substr(people$group_1, 7, 100))

##Join people and train set
finalTrain <- merge(x = train, y = people, by = "people_id", all.x = TRUE)
##distribution of char 38
ggplot(finalTrain, aes(x=char_38)) + geom_density(aes(color=outcome))


finalTrain %>% head
finalTrain$eventDate <- as.Date(finalTrain$date.x, format = "%Y-%m-%d")
trainOrdered <- finalTrain[with(finalTrain, order(people_id,eventDate )),]

#compute the previous average
trainOrdered %>% head
trainOrdered$numOutcome <- as.numeric(as.character(trainOrdered$outcome))
dt <- data.table(trainOrdered)
dt[,ave:=c(-1,cummean(numOutcome)),by=people_id]
trainOrdered <- data.frame(dt)
trainOrdered %>% subset(select = c(outcome, historicalOutcome)) %>% head(100)

##confused people 
confused <- trainOrdered[trainOrdered$historicalOutcome != 0 &trainOrdered$historicalOutcome != 1,] 
ggplot(confused, aes(x =historicalOutcome)) + geom_density(aes(color=outcome))

confused %>% subset(select = c(outcome,historicalOutcome,char_38)) %>% summary()









##################### futur plots
#### Matrix Scatter plot
selectFeatures <- c("outcome","char_38","prevOutcome")
filterDf <- subset(confused, select = selectFeatures)
ggpairs(filterDf, aes(colour = outcome, alpha=0.4))


##all plots 
i <- 1
while(i<(length(charFeatureTrain) +1 )){
  cat <- charFeatureTrain[i]
  p <- ggplot(train[train[[cat]]!=0,], aes_string(x=cat)) + 
    geom_bar(aes(color=outcome, fill = outcome),stat="count") + 
    facet_grid(outcome~.) + 
    labs(title = cat)
  print(p)
  i <- i +1 
}
