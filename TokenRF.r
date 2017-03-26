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
q1Docs <- Corpus(VectorSource(trainClean$question1))
q1Docs <- tm_map(q1Docs, removeWords,stopwords('en'))
q1Docs <- tm_map(q1Docs,removePunctuation)
q1Docs <- tm_map(q1Docs,stripWhitespace)
trainClean$question1 = data.frame(text=sapply(q1Docs,as.character),stringsAsFactors = FALSE) #Adds corpus back to existing dataframe

#Clean question 2 and remove extraneous parts
q2Docs <- Corpus(VectorSource(trainClean$question2))
q2Docs <- tm_map(q2Docs, removeWords,stopwords('en'))
q2Docs <- tm_map(q2Docs,removePunctuation)
q2Docs <- tm_map(q2Docs,stripWhitespace)
trainClean$question2 = data.frame(text=sapply(q2Docs,as.character),stringsAsFactors = FALSE)

tokens_q1 <- trainClean %>%
  unnest_tokens(word, question1, drop = FALSE, token = "regex", pattern = " ") %>%
  count(id, word) %>%
  ungroup()
  
tokens_q1 <- tokens_q1[trainClean, on = "id"]
colnames(tokens_q1)[1:3] <- c("id1","word1","n1")
tokens_q1 <- tokens_q1[,c("id1", "question1", "word1", "n1"),with = FALSE]

# calculate tf-idf weights
tf.idf1 <- tokens_q1 %>% bind_tf_idf(word1, question1, n1) %>%
                                select(id1, question1, word1, tf, idf, tf_idf)

###  for question2
tokens_q2 <- trainClean %>%
  unnest_tokens(word2, question2, drop = FALSE, token = "regex", pattern = " ") %>%
  count(id, word2) %>%
  ungroup()
tokens_q2 <- tokens_q2[trainClean, on = "id"]
colnames(tokens_q2)[1:3] <- c("id2", "word2", "n2")
tokens_q2 <- tokens_q2[,c("id2", "question2", "word2", "n2"),with=FALSE]
# calculate tf-idf weights
tf.idf2 <- tokens_q2 %>% bind_tf_idf(word2, question2, n2) %>%
                         select(id2, question2, word2, tf, idf, tf_idf)

func <- function(x){
  id.check <- x$id1[1] == tf.idf2$id2    # boolean vector to subset the question2 for same id 
  
  words1 <- x$word1
  words2 <- tf.idf2$word2[id.check]
  common <- intersect(words1, words2)    # list of common words in both
  uncommon.q1 <- setdiff(words1, words2) # words not present in question1
  uncommon.q2 <- setdiff(words2, words1) # words not present in question2
  len_common_words <- length(common)
  
  len_q1 <- nchar(x$question1[1])
  len_q2 <- nchar(tf.idf2$question2[id.check][1])
  diff_len <- abs(len_q1 - len_q2)       # difference in length of characters
  
  tfidf.wt1 <- x$tf_idf
  tfidf.wt2 <- tf.idf2$tf_idf[id.check]
  # calculate how similar both questions are based on tfidf weights
  # positive effect for the common words and negative exposure for the uncommon words
  w1_shared_wts <- tfidf.wt1[match(common, words1)]
  w1_unshared_wts <- tfidf.wt1[match(uncommon.q1, words1)]
  w2_shared_wts <- tfidf.wt2[match(common, words2)]
  w2_unshared_wts <- tfidf.wt2[match(uncommon.q2, words2)]
  ratio_commonality = (sum(c(w1_shared_wts,w2_shared_wts))-sum(c(w1_unshared_wts,w2_unshared_wts)))/(sum(tfidf.wt1, tfidf.wt2))
  return(list(len_common_words, ratio_commonality, diff_len))
}

trainTf = tf.idf1[ , c("len_common_words", "ratio_commonality", "diff_len") := func(.SD) , keyby = id1, .SDcols = c(colnames(tf.idf1))]
trainTf <- trainTf[, c("id1", "len_common_words", "ratio_commonality", "diff_len"), with = FALSE]]
colnames(trainTf)[1] = "id"
trainTf <- trainTf[!duplicated(ans$id),]
trainTf <- train[ans, on = "id"]
trainTf$is_duplicate <- factor(trainTf$is_duplicate)
trainTf$ratio_commonality[is.na(trainTf$ratio_commonality)] <- min(trainTf$ratio_commonality, na.rm = TRUE)



