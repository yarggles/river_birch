#load libraries
library(tidyverse)
library(lubridate)
library(scales)
library(kableExtra)

#import data and separate date into columns
sales <- read_csv("iowa_sales.csv") %>%
  mutate(year = year(date),
         month = month.name[month(date)],
         day = day(date)
         )

#filter to last 5 years, bottle size, and remove columns not needed
sales_recent <- sales %>%
  select(-vendor_number,
         -category) %>%
  filter(between(year, 2018, 2022), #only want complete years last 5
         between(bottle_volume_ml, 750, 1750)# only care about regular size bottles
         ) 


skimr::skim_without_charts(sales_recent)

#missing 3 vendor names, negative bottles sold means returned 

#cleaning 

filtered_sales <- sales_recent %>%
  filter(!is.na(vendor_name))
#removed 3 na values out of >8 million 

#standardizing vendor names
sales_clean <- filtered_sales %>%
  mutate(vendor_name = case_when(
   vendor_name == "AMERICAN HERITAGE DISTILLERS, LLC / CENTURY FARMS DISTILLERY" ~ "AMERICAN HERITAGE DISTILLERS, LLC",
   vendor_name == "BRECKENRIDGE DISTILLERY / DOUBLE DIAMOND DISTILLERY LLC" ~ "BRECKENRIDGE DISTILLERY",
   vendor_name == "CH DISTILLERY / 773 LLC" ~ "CH DISTILLERY",
   vendor_name == "CVI BRANDS / CALIFORNIA VINEYARDS INC" ~ "CVI BRANDS",
   vendor_name == "DUNKEL CORPORATION / IOWA DISTILLING" ~ "DUNKEL CORPORATION",
   vendor_name == "GEORGETOWN TRADING CO. LLC / JAMES PEPPER DISTILLING CO." ~ "GEORGETOWN TRADING CO. LLC",
   vendor_name == "KINGS COUNTRY DISTILLERY" ~ "KINGS COUNTY DISTILLERY",
   vendor_name == "LEVECKE CORPORATION JJB" ~ "LEVECKE CORPORATION",
   vendor_name == "MISSISSIPPI RIVER DISTIL" ~ "MISSISSIPPI RIVER DISTILLING COMPANY LLC",
   vendor_name == "PATERNO IMPORTS LTD / TERLATO WINES INTERNATIONAL" ~ "PATERNO IMPORTS LTD",
   vendor_name == "PRESTIGE WINE & SPIRITS GROUP / UNITED STATES DISTILLED PRODUCTS CO" ~ "PRESTIGE WINE & SPIRITS GROUP",
   vendor_name == "SAZERAC NORTH AMERICA" ~ "SAZERAC COMPANY  INC",
   vendor_name == "STOLLER WAREHOUSE" ~ "STOLLER IMPORTS INC",
   vendor_name == "STOLLER IMPORTS INC / MARSALLE COMPANY" ~ "STOLLER IMPORTS INC",
   vendor_name == "SUTTER HOME WINERY INC / TRINCHERO FAMILY ESTATES" ~ "SUTTER HOME WINERY INC",
   vendor_name == "TRAVERSE CITY WHISKEY CO / TCWC, LLC" ~ "TRAVERSE CITY WHISKEY CO",
   TRUE ~ vendor_name)
   )

#additional columns for analysis 
sales_metrics <- sales_clean %>% 
  mutate(week_day = wday(date, label = TRUE),
         volume_sold_l = (bottle_volume_ml * bottles_sold)/1000,
         vendor_profit = state_bottle_cost * bottles_sold,
         markup_percent = ((state_bottle_retail - state_bottle_cost)/state_bottle_cost)*100
         )%>%
  arrange(markup_percent)

#typical markup from vendor to customer

sales_metrics %>% 
  group_by(category_name) %>%
  ggplot(aes(category_name, markup_percent))+
    geom_boxplot()+
    labs(title = "State Mark-Up",
         x = NULL,
         y = "Percent")+
    scale_y_continuous(labels = label_number(scale_cut = cut_short_scale()))+
    coord_flip()
#seems they set a flat 50% markup across the board
# further profit based on vendors selling to the state- vendor_profit column 



#helper function for multiple plots
time_plot <- function(df,var){
  df %>% 
    group_by({{ var }}) %>% 
    summarise(sales = sum(vendor_profit)) %>% 
    ggplot(aes(({{ var }}), sales))+
    geom_col()+
    scale_y_continuous(labels = label_number(scale_cut = cut_short_scale()))
}

# sales by day of week
time_plot(sales_metrics, week_day)+
                  labs(title = "Daily Sales",
                       y = "Total Sales",
                       x = "Day")

#sales by month
time_plot(sales_metrics, month)+scale_x_discrete(limits = month.name)+
                   labs(title = "Monthly Sales",
                        y = "Total Sales",
                        x = "Month")

#sales by year
time_plot(sales_metrics, year)+
                  labs(title = "Yearly Sales",
                       y = "Total Sales",
                       x = "Year")

# top volume sales
# function to ease readability and speed 
volume_top_10 <- function(df,var){
  df %>% 
    group_by({{ var }}) %>% 
    summarise(volume = sum(volume_sold_l)) %>% 
    arrange(desc(volume)) %>% 
    slice(1:10) 
}

volume_graph <- function(df,var){
  df %>% 
    ggplot(aes(x=fct_reorder(({{var}}), volume),volume))+
    geom_col()+
    labs(title = "Total Volume Sold",
         y = "Volume (l)",
         x = NULL)+
    scale_y_continuous(labels = label_number(scale_cut = cut_short_scale()))+
    coord_flip()
}

category_volume_table <- volume_top_10(sales_metrics, category_name)
category_volume_plot <- volume_graph(category_volume_table, category_name)

vendor_volume_table <- volume_top_10(sales_metrics, vendor_name) 
vendor_volume_plot <- volume_graph(vendor_volume_table, vendor_name)

item_volume_table <- volume_top_10(sales_metrics, item_description)
item_volume_plot <- volume_graph(item_volume_table, item_description)

# top sales by profit
income_top_10 <- function(df,var){
  df %>% 
    group_by({{ var }}) %>% 
    summarise(sales = sum(vendor_profit)) %>% 
    arrange(desc(sales)) %>% 
    slice(1:10) 
}

sales_graph <- function(df,var){
  df %>% 
    ggplot(aes(x=fct_reorder(({{var}}), sales),sales))+
    geom_col()+
    labs(title = "Total Sales",
         y = "$",
         x = NULL)+
    scale_y_continuous(labels = label_number(scale_cut = cut_short_scale()))+
    coord_flip()
}
    
category_sales_table <- income_top_10(sales_metrics, category_name)
category_sales_plot <- sales_graph(category_sales_table, category_name)

vendor_sales_table <- income_top_10(sales_metrics, vendor_name)
vendor_sales_plot <- sales_graph(vendor_sales_table, vendor_name)

item_sales_table <- income_top_10(sales_metrics, item_description)
item_sales_plot <- sales_graph(item_sales_table, item_description)
























