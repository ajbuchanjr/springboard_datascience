library(dplyr)
library(tidyr)
# setwd("D:/Users/Anthony/Documents/Springboard")
refinetbl <- read.csv("refine_original.csv")
stdnames <- c("philips", "akzo", "van houten", "unilever")
refinetblnrm <- gsub("^.+ps$", stdnames[1], gsub("^ak.+$", stdnames[2], 
                gsub("^van.+$", stdnames[3], gsub("^uni.+$", stdnames[4], 
                refinetbl$company, ignore.case = T), ignore.case = T), ignore.case = T), ignore.case = T)
refinetbl$company <- refinetblnrm
refinetbl <- separate(refinetbl, Product.code...number, c("product_code","product_number"), sep="-")
refinetbl <- mutate(refinetbl, product_category = ifelse(product_code =="p","Smartphone",ifelse(product_code =="v","TV", ifelse(product_code=="x","Laptop", ifelse(product_code=="q","Tablet", product_code)))))
refinetbl <- mutate(refinetbl, full_address = paste(address, city, country, sep = ", "))
refinetbl <- mutate(refinetbl, company_philips = ifelse(company =="philips",1,0),company_akzo = ifelse(company =="akzo",1,0),company_van_houten = ifelse(company =="van houten",1,0), company_unilever = ifelse(company =="unilever",1,0))
refinetbl <- mutate(refinetbl, product_smartphone = ifelse(product_category =="Smartphone",1,0),product_tv = ifelse(product_category =="TV",1,0),product_laptop = ifelse(product_category =="Laptop",1,0), product_tablet = ifelse(product_category =="Tablet",1,0))
View(refinetbl)
write.csv(refinetbl, "refine_clean.csv")
