library(magrittr)
library(ggplot2)
library(GGally)
library(dplyr)
library(plyr)
library(data.table)  
source("/Users/jmvllt/Documents/workspace/R/Foot/FootFunctions.R")

people <- fread("/Users/jmvllt/Documents/workspace/redHatValue/people.csv")

train <- fread("/Users/jmvllt/Documents/workspace/redHatValue/act_train.csv")
train <- data.frame(train)
train$outcome <- factor(train$outcome)
test <- fread("/Users/jmvllt/Documents/workspace/redHatValue/act_test.csv")
test <- data.frame(test)
test$outcome <- factor(test$outcome)

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

##data cleaning for test set








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




selectFeatures <- c("outcome","char_1","char_2", "char_3")
filterDf <- subset(train, select = selectFeatures)
ggpairs(filterDf, aes(colour = outcome, alpha=0.4))



