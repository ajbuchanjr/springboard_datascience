# Author: Anthony J Buchanan
# File Description: Reddit scraper to pull in data from a given subreddit 
# as an input. Outputs a csv with the reddit comments for the last year .
library(jsonlite)
library(RCurl)
library(dplyr)
library(R.utils)
library(httr)
library(httpuv)
# setwd("D:/Users/Anthony/Documents/Springboard")
RedditToken <- function(appscript, secretvalue) {
  # Loads Reddit OAuth2 tokens.
  #
  # Args:
  #  appscript: The reddit application script code
  #  secret: The reddit secret api code
  #
  # Returns:
  #  Reddit authorization token
  
  # OAuth2 Settings
  reddit <- oauth_endpoint(
    authorize = "https://www.reddit.com/api/v1/authorize",
    access = "https://www.reddit.com/api/v1/access_token")
  app <- oauth_app("reddit", appscript, secretvalue)
  token <- oauth2.0_token(reddit, app, scope = c("read"), 
    use_basic_auth = TRUE, user_agent("rdatapull2 v0.2 by rdata"))
  return(token)
}
# Get the Reddit Token
my.token <- RedditToken("gOfXlYcHrvrWTg", "secretvalue")
LoadData <- function(url.val) {
  # Loads comment data from a given reddit url.
  #
  # Args:
  #  url: The url of the reddit comments to scrape
  #
  # Returns:
  #  A data fame with comments, including subreddit, created date
  #  author, score (total, up and down  votes), and comment text
  init.req <- GET(url.val, config(token = my.token), 
    user_agent("rdatapull1 v0.2 by rdata"))
  init.req <- content(init.req, as = "text")
  main.data <- tryCatch({
    fromJSON(init.req)
    if(!is.atomic(main.data[[2]]))
    {
      before.link <- main.data[[2]]$before
      after.link <- main.data[[2]]$after
      main <- main.data[[2]]$children$data
    }
  }, error = function(e) {
    cat("ERROR :",conditionMessage(e), "\n")
    Sys.sleep(600)
    })
  
  reduced.main <- tryCatch({ main[, c("subreddit","created","author","score",
                             "downs","ups","body")]
  }, error = function(e) {
    cat("ERROR :",conditionMessage(e), "\n")
    reduced.main <- data.frame(subreddit = character(0), created = numeric(0), 
     author = character(0), score = integer(0), downs = integer(0), 
     ups = integer(0), body = character(0), after.link = character(0))
  })
  if(!is.atomic(reduced.main) && exists(after.link))
  {  
    reduced.main$after.link <- after.link
  }
    return(reduced.main)
}
subreddit <- "worldnews"
url.start <- paste("https://oauth.reddit.com/r/",subreddit,
                   "/comments/.json?&t=year",sep="")
total.data <- LoadData(url.start)
if(!"after.link" %in% colnames(total.data))
{
  total.data[,"after.link"] <- NA
}
  while (!is.null(tail(total.data["after.link"],1))) {
    url.next <- paste(url.start,"&after=", tail(total.data["after.link"],1), sep="")
    total.data <- rbind_list(total.data, LoadData(url.next))
    print(nrow(total.data))
  }
write.csv(total.data, paste("RedditYear",subreddit,"Comments.csv",sep=""))
View(total.data)
