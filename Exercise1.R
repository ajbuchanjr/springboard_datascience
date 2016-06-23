# Author: Anthony J Buchanan
# File Description: Standardizes list of products by name, code, number 
# and category, with address
library(dplyr)
library(tidyr)
setwd("D:/Users/Anthony/Documents/Springboard")
refine.table <- read.csv("refine_original.csv")
standard.names <- c("philips", "akzo", "van houten", "unilever")
refine.table.normalized <- gsub("^.+ps$", standard.names[1], gsub("^ak.+$", 
  standard.names[2],
  gsub("^van.+$", standard.names[3], gsub("^uni.+$", standard.names[4], 
  refine.table.normalized$company, ignore.case = T), ignore.case = T), 
  ignore.case = T), ignore.case = T)
refine.table$company <- refine.table.normalized
refine.table <- separate(refine.table, Product.code...number, 
  c("product_code","product_number"), sep="-")
refine.table <- mutate(refine.table, product_category = 
  ifelse(product_code =="p","Smartphone",ifelse(product_code =="v","TV", 
  ifelse(product_code=="x","Laptop", 
  ifelse(product_code=="q","Tablet", product_code)))))
refine.table <- mutate(refine.table, full_address = 
  paste(address, city, country, sep = ", "))
refine.table <- mutate(refine.table, company_philips = 
  ifelse(company =="philips",1,0), company_akzo = ifelse(company =="akzo",1,0), 
  company_van_houten = ifelse(company =="van houten",1,0), 
  company_unilever = ifelse(company =="unilever",1,0))
refine.table <- mutate(refine.table, product_smartphone = 
  ifelse(product_category =="Smartphone",1,0), product_tv = 
  ifelse(product_category =="TV",1,0), product_laptop = 
  ifelse(product_category =="Laptop",1,0), product_tablet = 
  ifelse(product_category =="Tablet",1,0))
View(refine.table)
write.csv(refine.table, "refine_clean.csv")
