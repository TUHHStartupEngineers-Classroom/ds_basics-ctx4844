library(jsonlite)
library(glue)
library(RSQLite)
library(httr)
library(xml2)
library(tidyverse)
library(tibble)  
library(readr)     
library(dplyr)    
library(magrittr) 
library(tidyr)    
library(stringr)
library(ggplot2)



resp <- GET('http://www.7timer.info/bin/api.pl?lon=53.55&lat=9.99&product=civillight&output=json')

air_data<- resp %>% 
  .$content %>% 
  rawToChar() %>% 
  fromJSON()


changed_data <- air_data[[3]]
changed_data
