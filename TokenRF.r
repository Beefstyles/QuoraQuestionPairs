library(dplyr)
library(data.table)
library(dtplyr)
library(topicmodels)
library(tidytext)
library(ggplot2)
library(randomForest)
library(tm)

train <- fread("train.csv")

QuestionCleanup <- function(x){
    x <- tolower(x)
    x <- gsub("<img src.*?>", "", x)
    x <- gsub("http\\S+", "", x)
    x <- gsub("\\[math\\]", "", x)    # text between [] refers to tags e.g. [math]
    x <- gsub("<.*?>", "", x)
    x <- gsub("\n", " ", x)                 # replace newline with a space
    x <- gsub("\\s+", " ", x)                # multiple spaces into one
    # using tm_map to remove stopwords
    docs <- Corpus(VectorSource(x))  #Creates a corpus for x
    docs <- tm_map(docs, removeWords, stopwords('en')) #removes the stop words here
    docs <- tm_map(docs, removePunctuation)    # dont remove punct so early in the analysis
    docs <- tm_map(docs, stripWhitespace)
    xxx <- sapply(docs, function(i) i)
    data_content <- data.frame(text = xxx, stringsAsFactors = FALSE)
    return(data_content$text)
}

trainClean = train
trainClean$question1 = tolower(trainClean$question1)

#Clean question 1 and remove extraneous parts
q1Docs = Corpus(VectorSource(trainClean$question1))
q1Docs <- tm_map(q1Docs, removeWords,stopwords('en'))
q1Docs <- tm_map(q1Docs,removePunctuation)
q1Docs <- tm_map(q1Docs,stripWhitespace)
trainClean$question1 = data.frame(text=sapply(q1Docs,as.character),stringsAsFactors = FALSE) #Adds corpus back to existing dataframe

#Clean question 2 and remove extraneous parts
q2Docs = Corpus(VectorSource(trainClean$question2))
q2Docs <- tm_map(q2Docs, removeWords,stopwords('en'))
q2Docs <- tm_map(q2Docs,removePunctuation)
q2Docs <- tm_map(q2Docs,stripWhitespace)
trainClean$question2 = data.frame(text=sapply(q2Docs,as.character),stringsAsFactors = FALSE)

#train$question1 <- QuestionCleanup(train$question1)
#train$question2 <- QuestionCleanup(train$question2)


