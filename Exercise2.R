# Author: Anthony J Buchanan
# File Description: Reads in data on the Titanic and replaces null data 
# with standard values
library(dplyr)
library(tidyr)
# setwd("D:/Users/Anthony/Documents/Springboard")
table.load <- read.csv("titanic_original.csv", stringsAsFactors=F, 
  na.strings=c("","NA"))
refined.table <- replace_na(table.load , list(embarked = "S", age = 
  mean(table.load$age, na.rm = T), boat = "None"))
# Missing cabin number likely means there was no cabin assigned as the 
# passenger was likely crew or in steerage
refined.table <- mutate(refined.table, has_cabin_number = 
  ifelse(is.na(cabin),0,1))
View(refined.table)
write.csv(refined.table, "titanic_clean.csv")
