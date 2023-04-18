# Loading libraries

library(tidyverse)
library(readxl)
library(lubridate)

# Importing files

bikes_tbl <- read_excel('bikes.xlsx')

orderlines_tbl <- read_excel('orderlines.xlsx')

bikeshops_tbl <- read_excel('bikeshops.xlsx')

# Joining data by means of Entity-relationship diagrams(ERD)

#left_join(orderlines_tbl, bikes_tbl, by = c("product.id" = "bike.id"))
joined_tbl <- orderlines_tbl %>% 
  left_join(bikes_tbl, by = c("product.id" = "bike.id")) %>%
  left_join(bikeshops_tbl, by = c("customer.id" = "bikeshop.id"))

# Data wrangling

wrangled_tbl <- joined_tbl %>% 
  
  # separating location column into state and city
  
  separate(col = location, into = c("city", "state"), sep = ", ") %>%
  
  # Adding a column that will calculate total price
  
  mutate(total.price = price * quantity)

# sales by location(state)

sales_by_location_tbl <- wrangled_tbl %>%
  
  # select columns
  
  select(state, total.price) %>%
  
  # grouping by state and summarizing sales
  
  group_by(state) %>%
  summarize(sales = sum(total.price)) %>%
  
  # adding a column that turns the numbers into a currency format
  
  mutate(sales.text = scales::dollar(sales, big.mark = ".", decimal.mark = ",", prefix = "", suffix = "£á"))

# visualization(sales by state)

sales_by_location_tbl %>%
  
  # Setup canvas with the columns state (x-axis) and sales (y-axis)
  ggplot(aes(x = state, y = sales)) + theme(axis.text.x = element_text(angle = 45, hjust = 1)) + 
  
  # Geometries
  geom_col(fill = "#2DC6D6") + # Use geom_col for a bar plot
  geom_label(aes(label = sales.text)) + # Adding labels to the bars
  geom_smooth(method = "lm", se = FALSE) + # Adding a trendline
  
  # Formatting
  # scale_y_continuous(labels = scales::dollar) + # Change the y-axis. 
  # Again, we have to adjust it for euro values
  scale_y_continuous(labels = scales::dollar_format(big.mark = ".", 
                                                    decimal.mark = ",", 
                                                    prefix = "", 
                                                    suffix = "£á")) +
  labs(
    title    = "Revenue by state",
    subtitle = "Upward Trend",
    x = "State", # Override defaults for x and y
    y = "Revenue"
  )

# sales by location(state) and year

sales_by_location_year_tbl <- wrangled_tbl %>%
  
  # selecting columns and adding a year column
  
  select(order.date, total.price, state) %>%
  mutate(year = year(order.date)) %>%
  
  # grouping by and summarizing sales according to year and location(state)
  
  group_by(year, state) %>%
  summarise(sales = sum(total.price)) %>%
  ungroup() %>%
  
  mutate(sales.text = scales::dollar(sales, big.mark = ".", 
                                     decimal.mark = ",", 
                                     prefix = "", 
                                     suffix = "£á"))  

sales_by_location_year_tbl    

# visualization( sales by year and state)

sales_by_location_year_tbl %>%
  
  # Set up x, y, fill
  ggplot(aes(x = year, y = sales, fill = state)) +
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
  
  # Geometries
  geom_col() + # Run up to here to get a stacked bar plot
  geom_smooth(method = "lm", se = FALSE) + # Adding a trendline
  
  # Facet
  facet_wrap(~ state) +
  
  # Formatting
  scale_y_continuous(labels = scales::dollar_format(big.mark = ".", 
                                                    decimal.mark = ",", 
                                                    prefix = "", 
                                                    suffix = "£á")) +
  labs(
    title = "Revenue by State and Year",
    subtitle = "All The Products are Showing Increment In Revenue Except Saxony-Anhalt
                             M.W.P. and Schleswig-Holstein",
    fill = "States" # Changes the legend name to state
  )

result_2a <- sales_by_location_tbl

write_rds(result_2a, "result2a.rds")

result_2b <- sales_by_location_year_tbl

write_rds(result_2b, "result2b.rds")




