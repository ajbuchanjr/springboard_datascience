#' 
#' score.sentiment() implements a very simple algorithm to estimate
#' sentiment, assigning a integer score by subtracting the number 
#' of occurrences of negative words from that of positive words.
#' 
#' @param sentences vector of text to score
#' @param pos.words vector of words of postive sentiment
#' @param neg.words vector of words of negative sentiment
#' @param .progress passed to <code>laply()</code> to control of progress bar.
#' @returnType data.frame
#' @return data.frame of text and corresponding sentiment scores

library(DBI)
setwd("D:/Users/Anthony/Documents/Springboard")
hu.liu.pos = scan('positivew.txt', what='character', comment.char=';')
hu.liu.neg = scan('negativew.txt', what='character', comment.char=';')

# create a postive and negative word list
pos.words = c(hu.liu.pos)
neg.words = c(hu.liu.neg)

# Database Stuff
devdb <- dbConnect(RSQLServer::SQLServer(), server="localhost", port=1433,
                   properties=list(user="rdata", password="password"))

if(!exists("comments"))
   {
     comments <- dbGetQuery(devdb, "select top 1000 body from Comments")
}
sentences <- comments[['body']]

scores.sentiment = function(sentences,pos.words,neg.words)
{
  
  require(plyr)
  require(stringr)
  
  scores = laply(sentences, function(sentence, pos.words, neg.words) {
  # clean up sentences with R's regex-driven global substitute, gsub():
  sentence = gsub('[[:punct:]]', '', sentence)
  sentence = gsub('[[:cntrl:]]', '', sentence)
  sentence = gsub('\\d+', '', sentence)
  # and convert to lower case:
  sentence = tolower(sentence)
  
  # split into words. str_split is in the stringr package
  word.list = strsplit(sentence, '\\s+')
  # sometimes a list() is one level of hierarchy too much
  words = unlist(word.list)
  
  # compare our words to the dictionaries of positive & negative terms
  pos.matches = match(words, pos.words)
  neg.matches = match(words, neg.words)
  
  # match() returns the position of the matched term or NA
  # we just want a TRUE/FALSE:
  pos.matches = !is.na(pos.matches)
  neg.matches = !is.na(neg.matches)
  
  # and conveniently enough, TRUE/FALSE will be treated as 1/0 by sum():
  score = sum(pos.matches) - sum(neg.matches)
  
  return(score)
  }, pos.words, neg.words)
  
  scores = data.frame(score=scores, text=sentences)
  return(scores)
}

reddit.sentiment <- scores.sentiment(sentences,pos.words,neg.words)
View(reddit.sentiment)
