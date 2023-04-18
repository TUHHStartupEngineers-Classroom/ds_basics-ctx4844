library(tidyverse) 
library(rvest)     
library(xopen)     
library(jsonlite)  
library(glue)      
library(stringi)   

url_home          <- "https://www.rosebikes.de/fahrr%C3%A4der/mtb/trail-/-enduro"


html_home         <- read_html(url_home) # Read in the HTML for the entire web page


bike_model <- html_home %>% 
  
  html_nodes(css = ".catalog-category-bikes__title-text") %>% 
  html_text() %>%
  
  str_remove_all("\n") 

bike_model

# data scraping 

bike_price_in_euro <- html_home %>%
  
  html_nodes(css = ".catalog-category-bikes__price-title") %>%
  html_text() %>%
  
  str_remove_all("\\.") %>%
  stringr::str_replace_all(pattern = "\nab ", replacement = "") %>%
  stringr::str_replace_all(pattern = "\n", replacement = "") 

bike_price_in_euro

# merging the two tables into one

data_accu <- tibble(bike_model, bike_price_in_euro)

data_accu <- data_accu %>% mutate(bike_price_in_euro = as.character(gsub("???", ",",bike_price_in_euro)))
data_accu$bike_price_in_euro <- as.character(gsub(",","",data_accu$bike_price_in_euro))
data_accu$bike_price_in_euro <- as.character(gsub("ab","",data_accu$bike_price_in_euro))
#d<-da2
data_accu


write_rds(data_accu, "Data_Acquisition_2nd_part.rds")

write.csv(x=data_accu, file="Data_Acquisition_2nd_part.csv")
