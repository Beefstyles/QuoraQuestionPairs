library(ggplot2)
library(readr)

library(stringi)

library(syuzhet)
library(randomForest)

train <- read_csv("train.csv",n_max=100)
test <- read_csv("test.csv",n_max=100)

first <- stri_extract_first_words(train$question1)
second <- stri_extract_first_words(train$question2)
lastOne <- stri_extract_last_words(train$question1)
lastTwo <- stri_extract_last_words(train$question2)

train$first = ifelse(first == second, 1, 0)
train$second = ifelse(lastOne == lastTwo, 1, 0)

sentiment1 <- get_nrc_sentiment(train$question1)
sentiment2 <- get_nrc_sentiment(train$question2)

train = cbind(train, sentiment1, sentiment2)

tr = train[,6:28]

tr = na.omit(tr)

tr[,c(1:23)] = lapply(tr[,c(1:23)],as.factor)

rfModel <- randomForest(is_duplicate~.,data=tr)

pred <- predict(rfModel, newdata = tr,  type="prob")

#Round the results
pred = apply(pred, c(1,2), function(x) min(max(x, 1E-15), 1-1E-15))

logLoss = function(pred, actual){-1*mean(log(pred[model.matrix(~ actual + 0) - pred > 0]))}