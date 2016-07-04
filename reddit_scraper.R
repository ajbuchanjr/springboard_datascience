# Author: Anthony J Buchanan
# File Description: Reddit scraper to pull in data from a given subreddit 
# as an input. Outputs a csv with the reddit comments for the last year .
library(jsonlite)
library(RCurl)
library(dplyr)
library(R.utils)
library(httr)
library(httpuv)
library(DBI)
setwd("D:/Users/Anthony/Documents/Springboard")
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
    use_basic_auth = TRUE)
  return(token)
}
# Get the Reddit Token
my.token <- RedditToken("appid", "secret")
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
      user_agent("rpulldata v0.4 by /u/rdata"))
  init.req <- content(init.req, as = "text")
  main.data <- tryCatch({
    fromJSON(init.req)
  }, error = function(e) {
    cat("ERROR :",conditionMessage(e), "\n")
    # Sys.sleep(600)
    })
  if(!is.atomic(main.data[[2]]))
  {
    main <- main.data[[2]]$children$data
    main["before"]<- main.data[[2]]$before
    main["after"] <- main.data[[2]]$after
    main["inserteddate"] <- Sys.time()
  }
  
  reduced.main <- tryCatch({ main[, c("id","subreddit","created","author",
    "score","downs","ups","body","after","inserteddate")]
  }, error = function(e) {
    cat("ERROR :",conditionMessage(e), "\n")
    reduced.main <- data.frame(id = character(0), subreddit = character(0), 
    created = numeric(0), author = character(0), score = integer(0), 
    downs = integer(0), ups = integer(0), body = character(0), 
    after = character(0))
  })
    return(reduced.main)
}
WriteData <- function(dframe) {
  # Writes comment data from a dataframe.
  #
  # Args:
  #  dframe: The dataframe to checka and write
  #
  # Returns:
  #  Number of rows in a data fame with comments not in the database, 
  # including subreddit, created date, author, score (total, up and down votes),
  # and comment text
  idcheck <- dbGetQuery(devdb, paste("select id from Comments where id in (", 
    paste("'",dframe$id,collapse="',",sep=""), "')",sep=""))
  ret.data <- anti_join(dframe, idcheck, by="id")
  if (nrow(ret.data) > 0)
  {
    dbWriteTable(devdb, name="Comments", ret.data, append = T, 
                 overwrite = F)
  }
  return(nrow(ret.data))
}
subreddit <- "all"
url.start <- paste("https://oauth.reddit.com/r/",subreddit,
                     "/comments/.json?limit=100&t=year",sep="")
# Database Stuff
devdb <- dbConnect(RSQLServer::SQLServer(), server="localhost", port=1433,
                   properties=list(user="rdata", password="password"))
commentstart <- dbGetQuery(devdb, 
  "select top 1 id from Comments order by inserteddate DESC")
if(nrow(commentstart) > 0)
{
  url.next <- paste(url.start,"&after=", gsub("t1_","",commentstart[1,1]), 
    sep="")
} else {
  url.next <- url.start
}
total.data <- LoadData(url.next)
rows.written <- WriteData(total.data)
while (nrow(tail(total.data["after"],1)) > 0) {
    url.next <- paste(url.start,"&after=", gsub("t1_","",
    tail(total.data["after"],1)), sep="")
    total.data <- LoadData(url.next)
    rows.written <- WriteData(total.data)
    if (rows.written < 1)
    {
      total.data <- LoadData(url.start)
      rows.written <- WriteData(total.data)
    }
  }
# write.csv(total.data, paste("RedditYear",subreddit,"Comments.csv",sep=""))
dbDisconnect(devdb)
