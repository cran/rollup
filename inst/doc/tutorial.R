## ----include = FALSE----------------------------------------------------------
knitr::opts_chunk$set(
  collapse = TRUE,
  comment = "#>"
)

## ----setup--------------------------------------------------------------------
library(dplyr)
library(rollup)
data("web_service_data")
web_service_data %>% head

## ----warning=FALSE------------------------------------------------------------
library(tidyr)
# avg_pv_cnt group by (gender, age, (gender, age))
web_service_data %>% filter(date_id == '2024-06-30' & gender != "N") %>% 
  group_by(gender, age) %>% grouping_sets('gender', 'age', c('gender','age')) %>% 
  summarize(avg_pv_cnt = mean(page_view_cnt))

# avg_pv_cnt group by ((gender, age, product_view_cnt_cat), product_view_cnt_cat)
web_service_data %>% filter(date_id == '2024-06-30' & gender != "N") %>% 
  group_by(gender, age, product_view_cnt_cat) %>% grouping_sets('product_view_cnt_cat', c('product_view_cnt_cat', 'gender','age')) %>% 
  summarize(avg_pv_cnt = mean(page_view_cnt)) %>% pivot_wider(names_from = product_view_cnt_cat, values_from = avg_pv_cnt)

## ----warning=FALSE------------------------------------------------------------
# add sub-total rows to 2x2 cross table using with_cube()
web_service_data %>% filter(date_id == '2024-06-30' & gender != "N") %>% 
  group_by(gender, age) %>% with_cube() %>% 
  summarize(avg_pv_cnt = mean(page_view_cnt)) %>% pivot_wider(names_from = age, values_from = avg_pv_cnt)

# with_cube equals to grouping_sets with all possible combinations
web_service_data %>% filter(date_id == '2024-06-30' & gender != "N") %>%
  group_by(gender, age) %>% grouping_sets("gender","age",c("gender","age"), NA) %>%
  summarize(avg_pv_cnt = mean(page_view_cnt)) %>% pivot_wider(names_from = age, values_from = avg_pv_cnt)

## ----warning=FALSE------------------------------------------------------------
web_service_data %>% 
  group_by(date_id) %>% with_rollup() %>% 
  summarize(user_cnt = n_distinct(if_else(page_view_cnt > 0, id, NA)))

# with_rollup equals to grouping_sets with all possible combinations in descending order
web_service_data %>%
  group_by(date_id) %>% grouping_sets("date_id", NA) %>%
  summarize(user_cnt = n_distinct(if_else(page_view_cnt > 0, id, NA)))

