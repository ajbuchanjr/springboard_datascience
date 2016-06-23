# Author: Anthony J Buchanan
# File Description: Reddit scraper to pull in data from a given subreddit 
# as an input. Outputs a csv with the reddit comments for the last year .
library(jsonlite)
library(RCurl)
library(dplyr)
library(R.utils)
library(httr)
library(httpuv)
# setwd("C:/Users/abuchanan.NIMBUS")
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
    use_basic_auth = TRUE, user_agent("rdatapull v0.1 by rdata"))
  return(token)
}
LoadData <- function(url.val) {
  # Loads comment data from a given reddit url.
  #
  # Args:
  #  url: The url of the reddit comments to scrape
  #
  # Returns:
  #  A data fame with comments, including subreddit, created date
  #  author, score (total, up and down  votes), and comment text
  my.token <- RedditToken("gOfXlYcHrvrWTg", "secretvalue")
  init.req <- GET(url.val, config(token = my.token), 
    user_agent("rdatapull v0.1 by rdata"))
  init.req <- content(init.req, as = "text")
  main.data <- fromJSON(init.req)
  # Pause and try again on error
  # if (class(main.data) == "try-error") {
  #  Sys.sleep(10)
  #  main.data <- fromJSON(readLines(url, warn = FALSE))
  # }
    before.link <- main.data[[2]]$before
    after.link <- main.data[[2]]$after
    main <- main.data[[2]]$children$data
    reduced.main <- main[, c("subreddit","created","author","score",
                             "downs","ups","body")]
    reduced.main$after.link <- after.link
    return(reduced.main)
}
subreddit <- "worldnews"
url.start <- paste("https://oauth.reddit.com/r/",subreddit,
                   "/comments/.json?&t=year",sep="")
total.data <- LoadData(url.start)
while (!is.null(tail(total.data["after.link"],1))) {
    url.next <- paste(url.start,"&after=", tail(total.data["after.link"],1)
                       ,sep="")
    total.data <- rbind_list(total.data, LoadData(url.next))
    # write.table(total.data, paste("RedditYear", subreddit, "Comments.csv"), 
    # row.names=F,na="NA",append=T, quote= FALSE, sep=",", col.names=F)
  }
write.csv(total.data, paste("RedditYear",subreddit,"Comments.csv",sep=""))
View(total.data)
