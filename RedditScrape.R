library(jsonlite)
library(RCurl)
library(dplyr)
library(R.utils)

# setwd("C:/Users/abuchanan.NIMBUS")
LoadData <- function(url) {
  #Loads comment data, including votes, author for a given url
  main.data <- try(fromJSON(readLines(url, warn = FALSE)))
  # try again once
  if (class(main.data) == "try-error") {
    Sys.sleep(10)
    main.data <- fromJSON(readLines(url, warn = FALSE))
  }
    before.link <- main.data[[2]]$before
    after.link <- main.data[[2]]$after
    main <- main.data[[2]]$children$data
    reduced.main <- main[, c("subreddit","created","author","score",
                             "downs","ups","body")]
    reduced.main$after.link <- after.link
    return(reduced.main)
}
subreddit <- "worldnews"
url.start <- paste("http://www.reddit.com/r/",subreddit,
                   "/comments/.json?&t=year",sep="")
total.data <- LoadData(url.start)
while (!is.null(tail(total.data["after.link"],1))) {
    url.next <- paste(url.start,"&after=", tail(total.data["after.link"],1)
                       ,sep="")
    total.data <- rbind_list(total.data, LoadData(url.next))
    #write.table(total.data, paste("RedditYear", subreddit, "Comments.csv"), 
             #                     row.names=F,na="NA",append=T, quote= FALSE, sep=",", col.names=F)
  }
write.csv(total.data, paste("RedditYear",subreddit,"Comments.csv",sep=""))
# View(total.data)
