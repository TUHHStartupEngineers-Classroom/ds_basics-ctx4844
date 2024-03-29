


library(tidyverse)
library(data.table)
library(vroom)
library(tictoc)



col_types_assignee <- list(
  id = col_character(),
  type = col_character(),
  organization = col_character()
)

assignee_tbl <- vroom(
  file       = "assignee.tsv", 
  delim      = "\t", 
  col_types  = col_types_assignee,
  na         = c("", "NA", "NULL")
)

# data table conversion :
assignee_data_frame <- as.data.table(assignee_tbl %>% rename(assignee_id = id))

assignee_data_frame %>% glimpse()

# Load patent assignee data:

col_types_patent_assignee <- list(
  patent_id = col_character(),
  assignee_id = col_character()
  
)

patent_assignee_tbl <- vroom(
  file       = "patent_assignee.tsv", 
  delim      = "\t", 
  col_types  = col_types_patent_assignee,
  na         = c("", "NA", "NULL")
)

# data table conversion:

patent_assignee_data_frame <- as.data.table(patent_assignee_tbl)

patent_assignee_data_frame %>% glimpse()

# merging data :

tic()
combined_data <- merge(x = assignee_data_frame, y = patent_assignee_data_frame, 
                       by    = "assignee_id", 
                       all.x = TRUE, 
                       all.y = FALSE)
toc()

combined_data %>% glimpse()

# Patent dominance:
Patent_Domination <- combined_data %>%
  
  filter(!is.na(type) & type == 2) %>%
  group_by(organization, type) %>%
  tally(sort = T) %>%
  ungroup() %>%
  arrange(desc(n))
#What US company has the most patents? 
Patent_Domination

Patent_Domination_top_10 <- head(Patent_Domination,10)



# Loading the reduced patent data:

col_types_patent <- list(
  id = col_character(),
  date = col_date("%Y-%m-%d"),
  num_claims = col_double()
  
)

patent_tbl <- vroom(
  file       = "patent.tsv", 
  delim      = "\t", 
  col_types  = col_types_patent,
  na         = c("", "NA", "NULL")
)

patent_tbl

# convertion into data frame:

patent_data_frame <- as.data.table(patent_tbl %>% rename(patent_id = id)) 


patent_data_frame %>% glimpse()

# Merging data:

tic()
combined_new_data <- merge(x = combined_data, y = patent_data_frame, 
                           by    = "patent_id", 
                           all.x = TRUE, 
                           all.y = FALSE)
toc()

combined_new_data %>% glimpse()

#Manipulation of data:

merged_data <- combined_new_data %>%
  
  select(organization, date, type) %>%
  mutate(year = year(date)) %>%
  filter(year == 2014)

merged_data %>% glimpse()

# Recent patent activity
# What US company had the most patents granted in August 2014? 
Aug_patents_2014 <- merged_data %>%
  
  filter(!is.na(type) & type == 2) %>%
  group_by(organization, type, year) %>%
  tally(sort = T) %>%
  ungroup() %>%
  arrange(desc(n))

Aug_patents_2014

Aug_patents_2014_top10 <- head(Aug_patents_2014,10)


# Loading uspc data:

col_types_uspc <- list(
  patent_id = col_character(),
  mainclass_id = col_character(),
  sequence = col_character()
)

uspc_tbl <- vroom(
  file       = "uspc.tsv", 
  delim      = "\t", 
  col_types  = col_types_uspc,
  na         = c("", "NA", "NULL")
)

# data table conversion :
uspc_data_frame <- as.data.table(uspc_tbl)

uspc_data_frame %>% glimpse()



tic()
combined_newest_data <- merge(x = combined_data, y = uspc_data_frame, 
                              by    = "patent_id", 
                              all.x = TRUE, 
                              all.y = FALSE)
toc()

combined_newest_data %>% glimpse()

# For the top 10 companies (worldwide) with the most patents, 
#what are the top 5 USPTO tech main classes?

top_10_uspto <- combined_newest_data %>%
  
  select(organization, type, mainclass_id, sequence) %>%
  filter(sequence == 0) %>%
  group_by( mainclass_id) %>%
  tally(sort = T) %>%
  ungroup() %>%
  arrange(desc(n))


top_10_uspto

top_10_uspto_top10 <- head(top_10_uspto,10)

data_wrangling_1 <- Aug_patents_2014_top10

write_rds(data_wrangling_1, "data_wrangling_1.rds")


data_wrangling_2 <- top_10_uspto_top10

write_rds(data_wrangling_2, "data_wrangling_2.rds")