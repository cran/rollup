## ----include = FALSE----------------------------------------------------------
knitr::opts_chunk$set(
  collapse = TRUE,
  comment = "#>"
)
suppressPackageStartupMessages(library(dplyr))
suppressPackageStartupMessages(library(rollup))

## ----warning=FALSE, eval=FALSE------------------------------------------------
#  # From CRAN
#  install.packages("rollup")
#  
#  # From Github
#  library(devtools)
#  devtools::install_github("JuYoungAhn/rollup")

## ----warning=FALSE------------------------------------------------------------
mtcars %>% group_by(vs, am) %>% grouping_sets("vs","am",c("vs","am"), NA) %>% 
  summarize(n=n(), avg_mpg=mean(mpg))

mtcars %>% group_by(vs, am) %>% with_rollup() %>% 
  summarize(n=n(), avg_mpg=mean(mpg))

mtcars %>% group_by(vs, am) %>% with_cube() %>% 
  summarize(n=n(), avg_mpg=mean(mpg))

## ----setup--------------------------------------------------------------------
library(dplyr)
library(rollup)
data("web_service_data") # web_service_data of rollup package
web_service_data %>% head

## ----warning=FALSE------------------------------------------------------------
library(tidyr)
# compute average of `page_view_cnt` group by "gender", "age", and "gender & age", along with the overall average. NA in the output table represents overall aggregates.
web_service_data %>% filter(date_id == '2024-06-30' & gender != "N") %>% 
  group_by(gender, age) %>% grouping_sets('gender', 'age', c('gender','age'), NA) %>% 
  summarize(avg_pv_cnt = mean(page_view_cnt))

# compute average of `page_view_cnt` group by "gender & age & product_view_cnt_cat" along with the marginal average with regard to "product_view_cnt_cat".
web_service_data %>% filter(date_id == '2024-06-30' & gender != "N") %>% 
  group_by(gender, age, product_view_cnt_cat) %>% 
  grouping_sets('product_view_cnt_cat', c('product_view_cnt_cat', 'gender','age')) %>% 
  summarize(avg_pv_cnt = mean(page_view_cnt)) %>% 
  pivot_wider(names_from = product_view_cnt_cat, values_from = avg_pv_cnt)

## ----warning=FALSE------------------------------------------------------------
# This produces a table with average page view counts grouped by gender and age, including total aggregates across all combinations.
web_service_data %>% filter(date_id == '2024-06-30' & gender != "N") %>% 
  group_by(gender, age) %>% with_cube() %>% 
  summarize(avg_pv_cnt = mean(page_view_cnt)) %>% 
  pivot_wider(names_from = age, values_from = avg_pv_cnt)

## ----warning=FALSE------------------------------------------------------------
# The variables "age_big" and "age" have a hierarchy. 
web_service_data_processed <- web_service_data %>% mutate(
  age_big = case_when(
    age %in% c(10,20,30) ~ 'young',
    age %in% c(40,50,60) ~ 'old'  
  )
)

# If there are aggregates "age_big & age", marginal aggregates for "age" are not necessary.
# The following code computes aggregates for "age_big & age", "age_big", and entire data set.
web_service_data_processed %>% group_by(age_big, age) %>% 
  with_rollup() %>% summarize(
  user_cnt = n_distinct(id),
  avg_pv_cnt = mean(page_view_cnt)
)

