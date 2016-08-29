# Student: Anthony J Buchanan, Mentor: Raghunandan Patthar
# File Description: Reads in data on Reddit comments and does sentiment analysis 

library(DBI)
setwd("D:/Users/Anthony/Documents/Springboard")

# read the comments data into the comments data frame
 devdb <- dbConnect(RSQLServer::SQLServer(), server="localhost", port=1433,
                  properties=list(user="rdata", password="password"))

 comments_raw <- dbGetQuery(devdb, "select top 30000 score_category, 
                           body from Comments")
  #comments_raw <- read.csv("comments_sent.csv", stringsAsFactors = FALSE)
  names(comments_raw)[1] <- "score_category" #rename because csv is off
  names(comments_raw)[2] <- "body"

# examine the structure of the comments data
str(comments_raw)

# convert score category to factor.
comments_raw$score_category <- factor(comments_raw$score_category)

# examine the type variable more carefully
str(comments_raw$score_category)
table(comments_raw$score_category)

# build a corpus using the text mining (tm) package
library(tm)
comments_corpus <- Corpus(VectorSource(comments_raw$body))

# examine the comments corpus
print(comments_corpus)
inspect(comments_corpus[1:3])

# clean up the corpus using tm_map()
corpus_clean <- tm_map(comments_corpus, content_transformer(tolower))
corpus_clean <- tm_map(corpus_clean, removeNumbers)
corpus_clean <- tm_map(corpus_clean, removeWords, stopwords())
corpus_clean <- tm_map(corpus_clean, removePunctuation)
corpus_clean <- tm_map(corpus_clean, stripWhitespace)
corpus_clean <- tm_map(corpus_clean, PlainTextDocument)

# examine the clean corpus
inspect(comments_corpus[1:3])
inspect(corpus_clean[1:3])

# create a document-term sparse matrix
comments_dtm <- DocumentTermMatrix(corpus_clean)
comments_dtm

# creating training and test datasets
comments_raw_train <- comments_raw[1:22500, ]
comments_raw_test  <- comments_raw[22501:30000, ]

comments_dtm_train <- comments_dtm[1:22500, ]
comments_dtm_test  <- comments_dtm[22501:30000, ]

comments_corpus_train <- corpus_clean[1:22500]
comments_corpus_test  <- corpus_clean[22501:30000]

# check that the proportion of score category is similar
prop.table(table(comments_raw_train$score_category))
prop.table(table(comments_raw_test$score_category))

# word cloud visualization
library(wordcloud)

wordcloud(comments_corpus_train, min.freq = 30, random.order = FALSE)

# subset the training data into positive, neutral and negative groups
positive <- subset(comments_raw_train, score_category == "Positive")
neutral  <- subset(comments_raw_train, score_category == "Neutral")
negative  <- subset(comments_raw_train, score_category == "Negative")

wordcloud(positive$body, max.words = 40, scale = c(3, 0.5))
wordcloud(neutral$body, max.words = 40, scale = c(3, 0.5))
wordcloud(negative$body, max.words = 40, scale = c(3, 0.5))

# indicator features for frequent words
findFreqTerms(comments_dtm_train, 5)
comments_dict <- findFreqTerms(comments_dtm_train, 5)
comments_train <- DocumentTermMatrix(comments_corpus_train, 
                                     list(dictionary = comments_dict))
comments_test  <- DocumentTermMatrix(comments_corpus_test, 
                                     list(dictionary = comments_dict))

# convert counts to a factor
convert_counts <- function(x) {
  x <- ifelse(x > 0, 1, 0)
  x <- factor(x, levels = c(0, 1), labels = c("No", "Yes"))
}

# apply() convert_counts() to columns of train/test data
comments_train <- apply(comments_train, MARGIN = 2, convert_counts)
comments_test  <- apply(comments_test, MARGIN = 2, convert_counts)

## Step 3: Training a model on the data ----
library(e1071)
comments_classifier <- naiveBayes(comments_train, 
                                  comments_raw_train$score_category)
comments_classifier

## Step 4: Evaluating model performance ----
comments_test_pred <- predict(comments_classifier, comments_test)

library(gmodels)
CrossTable(comments_test_pred, comments_raw_test$score_category,
           prop.chisq = FALSE, prop.t = FALSE, prop.r = FALSE,
           dnn = c('predicted', 'actual'))

## Step 5: Improving model performance ----
comments_classifier2 <- naiveBayes(comments_train, 
                        comments_raw_train$score_category, laplace = 1)
comments_test_pred2 <- predict(comments_classifier2, comments_test)
CrossTable(comments_test_pred2, comments_raw_test$score_category,
           prop.chisq = FALSE, prop.t = FALSE, prop.r = FALSE,
           dnn = c('predicted', 'actual'))
