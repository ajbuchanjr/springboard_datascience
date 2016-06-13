library(dplyr)
library(tidyr)
# setwd("D:/Users/Anthony/Documents/Springboard")
tbl <- read.csv("titanic_original.csv", stringsAsFactors=F, na.strings=c("","NA"))
refinetbl <- replace_na(tbl, list(embarked = "S", age = mean(tbl$age, na.rm = T), boat = "None"))
# Missing cabin numberlikely means there was no cabin assigned as the passanger was likely crew or in steerage
refinetbl <- mutate(refinetbl, has_cabin_number = ifelse(is.na(cabin),0,1))
View(refinetbl)
write.csv(refinetbl, "titanic_clean.csv")